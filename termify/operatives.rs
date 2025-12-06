// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Base operatives - syntax transformers for core language forms.
//!
//! Each operative receives:
//! - Raw syntax (FormatList) - the arguments to the operative
//! - Environment (mutable) - can add bindings
//! - Goal - infer or check mode
//!
//! And returns an Inferrable term.

use alicorn_format::{Element, FormatList};
use alicorn_terms::Inferrable;
use format_macro::format_matcher;

use crate::{Env, ExprError, Goal, Result, expression};

/// Helper to extract a symbol from an Element
fn expect_symbol(elem: &Element) -> Result<&str> {
	match elem {
		Element::Symbol(s) => Ok(s.as_str()),
		_ => Err(ExprError::InvalidSyntax(format!("Expected symbol, got {:?}", elem))),
	}
}

/// Helper to convert a single Element to a FormatList for recursive processing
fn elem_to_list(elem: &Element) -> FormatList {
	match elem {
		Element::List(l) => l.clone(),
		_ => {
			let mut list = FormatList::new();
			list.push_back(elem.clone());
			list
		}
	}
}

/// let operative: `let name = expr`
///
/// Binds `name` to `expr` in the environment and returns unit.
/// The binding persists for subsequent expressions.
///
/// Note: This is a "flat" let that modifies env, not `let x = e in body`.
/// For let-in, the syntax would be `let name = expr in body` but that's
/// typically handled at a higher level (block processing).
pub fn let_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			// Binding with body: name = expr in body...
			// This desugars to let-expression
			// NOTE: Must come before simple binding pattern!
			(~name~, =, ~expr~, in, ~body...~) => {
				let name_str = expect_symbol(name)?;
				let expr_list = elem_to_list(expr);
				let expr_term = expression(&expr_list, env, Goal::Infer)?;

				// Create a new environment with the binding for the body
				let mut body_env = env.clone();
				body_env.bind(name_str.to_string(), expr_term.clone());

				// Process body
				let body_term = expression(&body, &mut body_env, Goal::Infer)?;

				// Create let term
				return Ok(Inferrable::let_bind(name_str, expr_term, body_term));
			},

			// Simple binding: name = expr...
			// Captures entire tail after = (like Lua's listtail)
			(~name~, =, ~expr...~) => {
				let name_str = expect_symbol(name)?;
				let expr_term = expression(&expr, env, Goal::Infer)?;

				// Bind in environment
				env.bind(name_str.to_string(), expr_term);

				// Return unit
				return Ok(Inferrable::unit());
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("let expects: name = expr, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// Arrow operative: `param -> body` or `(param : type) -> body`
///
/// Creates a lambda abstraction.
pub fn arrow_operative(syntax: &FormatList, _env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// The arrow was already consumed, so syntax is: param, body...
	// But wait - the arrow is infix, so we get called differently.
	// Actually, looking at how Lua does it, the arrow is bound as an operator
	// and gets special handling in the expression parser.
	//
	// For now, let's handle: body (where param was before the arrow)
	// This needs rethinking for proper infix handling.

	format_matcher! {
		match syntax {
			// Simple lambda: body (param was before arrow, needs special handling)
			(~body...~) => {
				// This is a placeholder - proper infix handling needed
				return Err(ExprError::InvalidSyntax(
					"Arrow operator needs special infix handling".to_string()
				));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("Malformed arrow expression")
				));
			}
		}
	}
}

/// Lambda operative: `lambda (params...) body` or just for explicit lambda keyword
pub fn lambda_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			// lambda param body
			(~param~, ~body...~) => {
				let param_name = expect_symbol(param)?;

				// Create new env with param bound
				// Use Lua approach: store de Bruijn level (0-indexed) at bind time
				let mut body_env = env.clone();
				body_env.bind(param_name.to_string(), Inferrable::bound_variable(body_env.depth, param_name));
				body_env.depth += 1;

				let body_term = expression(&body, &mut body_env, Goal::Infer)?;

				return Ok(Inferrable::lambda(param_name, None, body_term));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("lambda expects: param body, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// Forall operative: `forall (param : type) result_type`
///
/// Creates a Pi type (dependent function type).
pub fn forall_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: (param : type) result...
	// First element should be a list with param : type
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax("forall expects: (param : type) result".to_string()));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	// Parse (param : type)
	let (param_name, param_type_term) = match first {
		Element::List(inner) => {
			// Expect: param : type
			if inner.len() < 3 {
				return Err(ExprError::InvalidSyntax(
					"forall param binding should be (param : type)".to_string(),
				));
			}
			let param = expect_symbol(&inner[0])?;
			// inner[1] should be ":"
			let type_syntax = inner.clone().slice(2..);
			let type_term = expression(&type_syntax, env, Goal::Infer)?;
			(param, type_term)
		}
		_ => return Err(ExprError::InvalidSyntax("forall expects (param : type)".to_string())),
	};

	// Extend env for result type
	// Use Lua approach: store de Bruijn level (0-indexed) at bind time
	let mut result_env = env.clone();
	result_env.bind(param_name.to_string(), Inferrable::bound_variable(result_env.depth, param_name));
	result_env.depth += 1;

	let result_term = expression(&rest, &mut result_env, Goal::Infer)?;

	Ok(Inferrable::pi(param_name, param_type_term, result_term))
}

/// Annotated lambda operative: `(param : type) -> body`
pub fn annotated_lambda_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: (param : type) body...
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax(
			"annotated lambda expects: (param : type) body".to_string(),
		));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	// Parse (param : type)
	let (param_name, param_type_term) = match first {
		Element::List(inner) => {
			if inner.len() < 3 {
				return Err(ExprError::InvalidSyntax("param binding should be (param : type)".to_string()));
			}
			let param = expect_symbol(&inner[0])?;
			let type_syntax = inner.clone().slice(2..);
			let type_term = expression(&type_syntax, env, Goal::Infer)?;
			(param, type_term)
		}
		_ => return Err(ExprError::InvalidSyntax("annotated lambda expects (param : type)".to_string())),
	};

	// Extend env for body
	// Use Lua approach: store de Bruijn level (0-indexed) at bind time
	let mut body_env = env.clone();
	body_env.bind(param_name.to_string(), Inferrable::bound_variable(body_env.depth, param_name));
	body_env.depth += 1;

	let body_term = expression(&rest, &mut body_env, Goal::Infer)?;

	Ok(Inferrable::lambda(param_name, Some(Box::new(param_type_term)), body_term))
}

/// Annotate operative: `expr : type`
pub fn annotate_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			// expr : type (but : already consumed as head)
			(~expr~, ~type_expr~) => {
				let expr_list = elem_to_list(expr);
				let type_list = elem_to_list(type_expr);

				let term = expression(&expr_list, env, Goal::Infer)?;
				let type_term = expression(&type_list, env, Goal::Infer)?;

				return Ok(Inferrable::annotated(term, type_term));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("annotation expects: expr type")
				));
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{Env, parse_and_run_with_env};

	#[test]
	fn let_operative_direct() {
		// Test the operative directly with the args it would receive.
		// When expression() dispatches to let_operative, it passes the REST
		// after consuming "let". So for "let x = 5", operative gets [x, =, 5].
		//
		// Parser returns a block, so we parse "x = 5" → [[x, =, 5]]
		// and extract the inner list.
		let syntax = alicorn_format::format("x = 5").unwrap();

		// Extract the inner list (first element of block)
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list in block, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = let_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "let_operative failed: {:?}", result);
		assert!(env.get("x").is_some(), "x should be bound after let");
	}

	#[test]
	fn let_through_block() {
		// Test through the full pipeline: parse → block → expression → operative
		let (result, env) = parse_and_run_with_env("let x = 5").unwrap();

		// let returns unit (empty tuple)
		assert!(
			matches!(&result.kind, alicorn_terms::InferrableKind::TupleCons { elements } if elements.is_empty()),
			"let should return unit, got {:?}",
			result
		);

		// x should be bound
		assert!(env.get("x").is_some(), "x should be bound after let");
	}

	#[test]
	fn let_in_expression() {
		// Test let with body: let x = 5 in x
		let syntax = alicorn_format::format("x = 5 in x").unwrap();
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = let_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "let-in failed: {:?}", result);
		// Result should be a let-binding term, not unit
		assert!(
			matches!(&result.as_ref().unwrap().kind, alicorn_terms::InferrableKind::Let { .. }),
			"let-in should produce Let term, got {:?}",
			result
		);
	}

	#[test]
	fn lambda_simple() {
		// lambda x body
		let syntax = alicorn_format::format("x x").unwrap();
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = lambda_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "lambda failed: {:?}", result);
		assert!(
			matches!(&result.as_ref().unwrap().kind, alicorn_terms::InferrableKind::Lambda { .. }),
			"should produce Lambda, got {:?}",
			result
		);
	}
}
