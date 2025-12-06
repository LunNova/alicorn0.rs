// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Expression evaluator with type-directed operative dispatch.
//!
//! This is the core of syntax → term conversion. Unlike naive pattern matching,
//! we use the type system to dispatch to operatives:
//!
//! 1. Look up head of expression
//! 2. Infer its type
//! 3. If operative type → call operative with remaining syntax
//! 4. If pi type → function application
//! 5. Otherwise → error
//!
//! See CLAUDE.md for architecture details.

pub mod operatives;

use alicorn_format::{Element, FormatList};
use alicorn_terms::{FlexValue, Inferrable, NativeOperative, TypingContext, as_operative_type, infer, is_pi_type};
use std::collections::HashMap;

/// Expression goal - infer type or check against expected type.
#[derive(Debug, Clone)]
pub enum Goal {
	/// Figure out the type
	Infer,
	/// Check against this expected type
	Check(FlexValue),
}

impl Goal {
	pub fn is_infer(&self) -> bool {
		matches!(self, Goal::Infer)
	}
}

/// Environment for expression evaluation.
/// Maps names to Inferrable terms (which may be operatives, functions, or values).
#[derive(Debug, Clone)]
pub struct Env {
	/// Name → Inferrable term bindings (for operatives, let-bound values, lambda params, etc.)
	bindings: HashMap<String, Inferrable>,
	/// Current lambda nesting depth (used for de Bruijn level assignment)
	pub depth: usize,
	/// Typing context for de Bruijn lookups
	pub typing_context: TypingContext,
}

impl Env {
	pub fn new() -> Self {
		Self {
			bindings: HashMap::new(),
			depth: 0,
			typing_context: TypingContext::new(),
		}
	}

	/// Create environment with base operatives
	pub fn with_base_operatives() -> Self {
		let mut env = Self::new();

		// Register core operatives
		env.bind("let", Inferrable::native_operative(NativeOperative::Let));
		env.bind("->", Inferrable::native_operative(NativeOperative::Arrow));
		env.bind("lambda", Inferrable::native_operative(NativeOperative::Lambda));
		env.bind("fn", Inferrable::native_operative(NativeOperative::AnnotatedLambda));
		env.bind("forall", Inferrable::native_operative(NativeOperative::Forall));
		env.bind(":", Inferrable::native_operative(NativeOperative::Annotate));

		// Register primitive types for annotations
		env.bind("Number", Inferrable::literal(FlexValue::HostNumberType));
		env.bind("String", Inferrable::literal(FlexValue::HostStringType));
		env.bind("Bool", Inferrable::literal(FlexValue::HostBoolType));

		env
	}

	/// Bind a name to a term (for operatives, let-bindings, lambda params, etc.)
	pub fn bind(&mut self, name: impl Into<String>, term: Inferrable) {
		self.bindings.insert(name.into(), term);
	}

	/// Look up a name - returns the bound term
	pub fn get(&self, name: &str) -> Option<Inferrable> {
		self.bindings.get(name).cloned()
	}

	/// Push a binding onto the typing context (for de Bruijn)
	pub fn push_type(&mut self, name: String, typ: FlexValue) {
		self.typing_context.push(name, typ);
	}
}

impl Default for Env {
	fn default() -> Self {
		Self::new()
	}
}

/// Errors that can occur during expression evaluation
#[derive(Debug, Clone)]
pub enum ExprError {
	UnboundVariable(String),
	InvalidSyntax(String),
	NotCallable(String),
	OperativeError(String),
	InferError(String),
}

impl std::fmt::Display for ExprError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprError::UnboundVariable(name) => write!(f, "Unbound variable: {}", name),
			ExprError::InvalidSyntax(msg) => write!(f, "Invalid syntax: {}", msg),
			ExprError::NotCallable(msg) => write!(f, "Not callable: {}", msg),
			ExprError::OperativeError(msg) => write!(f, "Operative error: {}", msg),
			ExprError::InferError(msg) => write!(f, "Type inference error: {}", msg),
		}
	}
}

impl std::error::Error for ExprError {}

impl From<alicorn_terms::InferError> for ExprError {
	fn from(e: alicorn_terms::InferError) -> Self {
		ExprError::InferError(e.to_string())
	}
}

pub type Result<T> = std::result::Result<T, ExprError>;

/// Convert a FormatList to an Inferrable term.
///
/// This is the main entry point. It dispatches based on syntax structure
/// and the types of sub-expressions.
pub fn expression(syntax: &FormatList, env: &mut Env, goal: Goal) -> Result<Inferrable> {
	// Empty list is unit
	if syntax.is_empty() {
		return Ok(Inferrable::unit());
	}

	// Single element: process as atom
	if syntax.len() == 1 {
		return expression_atom(&syntax[0], env, goal);
	}

	// Multiple elements: get head, check if operative, dispatch
	let head = &syntax[0];

	// Try to get the head as a term and infer its type
	if let Element::Symbol(name) = head {
		if let Some(head_term) = env.get(name.as_str()) {
			let head_type = infer(&head_term, &env.typing_context)?;

			// Check if it's an operative
			if let Some(operative) = as_operative_type(&head_type) {
				// Call the operative with the rest of the syntax
				return call_operative(operative, syntax.clone().slice(1..), env, goal);
			}

			// Check if it's a function (pi type)
			if is_pi_type(&head_type) {
				return expression_application(syntax, env, goal);
			}

			// Not callable
			return Err(ExprError::NotCallable(format!(
				"Head '{}' has type {:?}, which is not callable",
				name, head_type
			)));
		}
	}

	// Head is not a symbol or not found - try as application
	expression_application(syntax, env, goal)
}

/// Process a single atom (symbol, number, string, nested list).
fn expression_atom(elem: &Element, env: &mut Env, goal: Goal) -> Result<Inferrable> {
	match elem {
		Element::Number(n) => Ok(Inferrable::number(*n)),

		Element::String(s) => Ok(Inferrable::string(s.clone())),

		Element::Symbol(name) => {
			let name_str = name.as_str();

			// First try environment lookup
			if let Some(term) = env.get(name_str) {
				return Ok(term.clone());
			}

			// Then try typing context (de Bruijn)
			if let Some(index) = lookup_de_bruijn(name_str, env) {
				return Ok(Inferrable::bound_variable(index, name_str));
			}

			Err(ExprError::UnboundVariable(name_str.to_string()))
		}

		Element::List(inner) => expression(inner, env, goal),

		Element::Comment(_) => Ok(Inferrable::unit()),
	}
}

/// Look up a name in the typing context, returning de Bruijn index.
fn lookup_de_bruijn(_name: &str, _env: &Env) -> Option<usize> {
	// The typing context stores (name, type) pairs
	// We need to search for the name and return the index
	// For now, we don't have name tracking in TypingContext, so this returns None
	// TODO: Add name tracking to TypingContext
	None
}

/// Handle function application: (func arg1 arg2 ...)
fn expression_application(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	if syntax.is_empty() {
		return Ok(Inferrable::unit());
	}

	// Evaluate head
	let head_syntax = syntax.clone().slice(0..1);
	let mut result = expression(&head_syntax, env, Goal::Infer)?;

	// Apply to each argument
	for i in 1..syntax.len() {
		let arg_syntax = syntax.clone().slice(i..i + 1);
		let arg = expression(&arg_syntax, env, Goal::Infer)?;
		result = Inferrable::application(result, arg);
	}

	Ok(result)
}

/// Call a native operative with syntax.
fn call_operative(op: NativeOperative, syntax: FormatList, env: &mut Env, goal: Goal) -> Result<Inferrable> {
	use operatives::*;

	match op {
		NativeOperative::Let => let_operative(&syntax, env, goal),
		NativeOperative::Arrow => arrow_operative(&syntax, env, goal),
		NativeOperative::Lambda => lambda_operative(&syntax, env, goal),
		NativeOperative::Forall => forall_operative(&syntax, env, goal),
		NativeOperative::AnnotatedLambda => annotated_lambda_operative(&syntax, env, goal),
		NativeOperative::Annotate => annotate_operative(&syntax, env, goal),
	}
}

/// Process a block (list of statements) from parser output.
///
/// The parser returns a top-level list where each element is a statement.
/// For input "let x = 5\nlet y = 10", parser returns:
///   [[let, x, =, 5], [let, y, =, 10]]
///
/// This function iterates over each statement, calling expression() on it.
/// Returns the result of the last expression (REPL semantics).
/// Environment mutations (like `let` bindings) persist across statements.
pub fn block(syntax: &FormatList, env: &mut Env) -> Result<Inferrable> {
	let mut result = Inferrable::unit();

	for elem in syntax.iter() {
		match elem {
			Element::List(stmt) => {
				result = expression(stmt, env, Goal::Infer)?;
			}
			Element::Comment(_) => {
				// Skip comments at block level
			}
			_ => {
				// Single atom as a statement (e.g., just "42" on a line)
				result = expression_atom(elem, env, Goal::Infer)?;
			}
		}
	}

	Ok(result)
}

/// Parse input and run as a block with a fresh environment.
pub fn parse_and_run(input: &str) -> std::result::Result<Inferrable, Box<dyn std::error::Error>> {
	let syntax = alicorn_format::format(input)?;
	let mut env = Env::with_base_operatives();
	let term = block(&syntax, &mut env)?;
	Ok(term)
}

/// Parse input and run as a block, returning both result and environment.
pub fn parse_and_run_with_env(input: &str) -> std::result::Result<(Inferrable, Env), Box<dyn std::error::Error>> {
	let syntax = alicorn_format::format(input)?;
	let mut env = Env::with_base_operatives();
	let term = block(&syntax, &mut env)?;
	Ok((term, env))
}

/// Full pipeline: parse → expression → elaborate → evaluate → value
///
/// This is the complete end-to-end execution path:
/// 1. Parse source text to FormatList
/// 2. Process block through expression evaluator → Inferrable
/// 3. Elaborate Inferrable → Elaborated
/// 4. Evaluate Elaborated → FlexValue
pub fn run_file(input: &str) -> std::result::Result<FlexValue, Box<dyn std::error::Error>> {
	use alicorn_terms::{Env as EvalEnv, elaborate, evaluate};

	// Parse
	let syntax = alicorn_format::format(input)?;

	// Expression evaluation (operatives, etc.)
	let mut env = Env::with_base_operatives();
	let inferrable = block(&syntax, &mut env)?;

	// Elaborate
	let (elaborated, _typ) = elaborate(&inferrable, &env.typing_context)?;

	// Evaluate
	let value = evaluate(&elaborated, &EvalEnv::new());

	Ok(value)
}

/// Full pipeline with environment access
pub fn run_file_with_env(input: &str) -> std::result::Result<(FlexValue, Env), Box<dyn std::error::Error>> {
	use alicorn_terms::{Env as EvalEnv, elaborate, evaluate};

	let syntax = alicorn_format::format(input)?;
	let mut env = Env::with_base_operatives();
	let inferrable = block(&syntax, &mut env)?;
	let (elaborated, _typ) = elaborate(&inferrable, &env.typing_context)?;
	let value = evaluate(&elaborated, &EvalEnv::new());

	Ok((value, env))
}

#[cfg(test)]
mod tests {
	use super::*;
	use alicorn_terms::InferrableKind;

	#[test]
	fn block_number() {
		// Parser wraps in block: "42" → [[42]] but 42 is not a list, so it's [42]
		// Actually for single atom it might be different, let's check
		let syntax = alicorn_format::format("42").unwrap();
		let mut env = Env::with_base_operatives();
		let term = block(&syntax, &mut env).unwrap();

		assert!(matches!(term.kind, InferrableKind::Literal(FlexValue::HostNumber { value }) if value == 42.0));
	}

	#[test]
	fn block_string() {
		let syntax = alicorn_format::format("\"hello\"").unwrap();
		let mut env = Env::with_base_operatives();
		let term = block(&syntax, &mut env).unwrap();

		assert!(matches!(&term.kind, InferrableKind::Literal(FlexValue::HostString { value }) if value == "hello"));
	}

	#[test]
	fn block_lookup_operative() {
		let syntax = alicorn_format::format("let").unwrap();
		let mut env = Env::with_base_operatives();
		let term = block(&syntax, &mut env).unwrap();

		// Should get back the operative term
		assert!(matches!(term.kind, InferrableKind::Typed { .. }));
	}

	#[test]
	fn block_let_binding() {
		// Full pipeline: parse "let x = 5" → block → expression → operative
		let (result, env) = parse_and_run_with_env("let x = 5").unwrap();

		// let returns unit
		assert!(matches!(result.kind, InferrableKind::TupleCons { ref elements } if elements.is_empty()));

		// x should be bound in environment
		assert!(env.get("x").is_some(), "x should be bound after let");
	}

	#[test]
	fn block_multiple_statements() {
		// Multiple statements: bindings persist
		let (result, env) = parse_and_run_with_env("let x = 5\nlet y = 10").unwrap();

		// Last statement returns unit
		assert!(matches!(result.kind, InferrableKind::TupleCons { ref elements } if elements.is_empty()));

		// Both should be bound
		assert!(env.get("x").is_some(), "x should be bound");
		assert!(env.get("y").is_some(), "y should be bound");
	}

	// ============================================================
	// Full pipeline integration tests: source → value
	// ============================================================

	#[test]
	fn run_literal_number() {
		let result = run_file("42").unwrap();
		assert!(
			matches!(result, FlexValue::HostNumber { value } if value == 42.0),
			"Expected 42.0, got {:?}",
			result
		);
	}

	#[test]
	fn run_literal_string() {
		let result = run_file("\"hello\"").unwrap();
		assert!(
			matches!(&result, FlexValue::HostString { value } if value == "hello"),
			"Expected 'hello', got {:?}",
			result
		);
	}

	#[test]
	fn run_identity_lambda() {
		// (fn (x : Number) x) 42 → 42
		let result = run_file("(fn (x : Number) x) 42").unwrap();
		assert!(
			matches!(result, FlexValue::HostNumber { value } if value == 42.0),
			"Expected 42.0, got {:?}",
			result
		);
	}

	#[test]
	fn run_let_in_expression() {
		// let x = 5 in x → 5
		let result = run_file("let x = 5 in x").unwrap();
		assert!(
			matches!(result, FlexValue::HostNumber { value } if value == 5.0),
			"Expected 5.0, got {:?}",
			result
		);
	}

	#[test]
	fn run_nested_let() {
		// let x = 5 in (let y = 10 in x)  → 5
		let result = run_file("let x = 5 in (let y = 10 in x)").unwrap();
		assert!(
			matches!(result, FlexValue::HostNumber { value } if value == 5.0),
			"Expected 5.0, got {:?}",
			result
		);
	}

	#[test]
	fn run_k_combinator() {
		// ((fn (x : Number) (fn (y : Number) x)) 1) 2 → 1
		let result = run_file("((fn (x : Number) (fn (y : Number) x)) 1) 2").unwrap();
		assert!(
			matches!(result, FlexValue::HostNumber { value } if value == 1.0),
			"Expected 1.0, got {:?}",
			result
		);
	}
}
