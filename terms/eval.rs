// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Toy evaluator - reduces typed terms to values.
//!
//! This is a minimal skeleton evaluator that can handle:
//! - Literals (already values)
//! - Lambda abstraction (creates closures)
//! - Application (beta reduction)
//! - Bound variables (lookup in environment)
//!
//! TODO: The real evaluator in Lua is ~7000 lines and handles:
//!   - Type checking / inference
//!   - Constraint solving for subtyping
//!   - Effects and programs
//!   - Records, enums, tuples
//!   - Operatives
//!   - etc.

use crate::typed::Term;
use crate::value::FlexValue;

// Runtime environment - a stack of values for bound variables.
// Index 0 in Term::BoundVariable refers to env[env.len() - 1] (most recent binding)
#[derive(Debug, Clone, Default)]
pub struct Env {
	bindings: Vec<FlexValue>,
}

impl Env {
	pub fn new() -> Self {
		Self { bindings: vec![] }
	}

	/// Push a value onto the environment (for entering a lambda body)
	pub fn push(&mut self, val: FlexValue) {
		self.bindings.push(val);
	}

	/// Pop a value from the environment (for leaving a lambda body)
	pub fn pop(&mut self) -> Option<FlexValue> {
		self.bindings.pop()
	}

	/// Look up a de Bruijn index. Index 0 = most recent binding.
	pub fn lookup(&self, index: usize) -> Option<&FlexValue> {
		if index < self.bindings.len() {
			Some(&self.bindings[self.bindings.len() - 1 - index])
		} else {
			None
		}
	}

	/// Extend environment with a new binding, returning new env
	pub fn extend(&self, val: FlexValue) -> Self {
		let mut new_env = self.clone();
		new_env.push(val);
		new_env
	}
}

/// Evaluate a typed term to a value.
///
/// This is a toy evaluator - just enough to run basic lambda calculus.
pub fn evaluate(term: &Term, env: &Env) -> FlexValue {
	match term {
		Term::Literal(val) => val.clone(),

		Term::BoundVariable { index, debug_name } => {
			env.lookup(*index).cloned().unwrap_or_else(|| {
				// Unbound variable - becomes a stuck value
				FlexValue::StuckValue(
					crate::value::StuckValue::Free {
						index: *index,
						debug_name: debug_name.clone(),
					},
					(),
				)
			})
		}

		Term::Lambda { param_name, body } => {
			// Create a closure capturing the current environment
			// We store the entire env as a tuple in capture
			FlexValue::Closure {
				param_name: param_name.clone(),
				body: Box::new(body.as_ref().clone()),
				capture: Box::new(FlexValue::TupleValue {
					elements: env.bindings.clone(),
				}),
			}
		}

		Term::Application { func, arg } => {
			let func_val = evaluate(func, env);
			let arg_val = evaluate(arg, env);

			apply(func_val, arg_val)
		}

		Term::TupleCons { elements } => {
			let vals: Vec<FlexValue> = elements.iter().map(|e| evaluate(e, env)).collect();
			FlexValue::TupleValue { elements: vals }
		}

		Term::Annotated { term, ty: _ } => {
			// Type annotation doesn't affect runtime - just evaluate the term
			evaluate(term, env)
		}
	}
}

/// Apply a function value to an argument value.
fn apply(func: FlexValue, arg: FlexValue) -> FlexValue {
	match func {
		FlexValue::Closure {
			param_name: _,
			body,
			capture,
		} => {
			// Beta reduction: restore captured env, add arg, evaluate body
			let mut env = Env::new();
			// Restore captured environment
			if let FlexValue::TupleValue { elements } = *capture {
				env.bindings = elements;
			}
			// Add the argument binding
			env.push(arg);
			evaluate(&body, &env)
		}

		// If func is stuck, the application is stuck too
		_ => FlexValue::StuckValue(
			crate::value::StuckValue::Application {
				func: Box::new(func),
				arg: Box::new(arg),
			},
			(),
		),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn eval_literal() {
		let term = Term::number(42.0);
		let result = evaluate(&term, &Env::new());
		assert!(matches!(result, FlexValue::HostNumber { value } if value == 42.0));
	}

	#[test]
	fn eval_identity_application() {
		// (λx. x) 42
		let id = Term::lambda("x", Term::var(0, "x"));
		let app = Term::app(id, Term::number(42.0));
		let result = evaluate(&app, &Env::new());
		assert!(matches!(result, FlexValue::HostNumber { value } if value == 42.0));
	}

	#[test]
	fn eval_k_combinator() {
		// ((λx. λy. x) 1) 2 = 1
		let k = Term::lambda("x", Term::lambda("y", Term::var(1, "x")));
		let k1 = Term::app(k, Term::number(1.0));
		let k1_2 = Term::app(k1, Term::number(2.0));
		let result = evaluate(&k1_2, &Env::new());
		assert!(matches!(result, FlexValue::HostNumber { value } if value == 1.0));
	}

	#[test]
	fn eval_tuple() {
		let tuple = Term::tuple(vec![Term::number(1.0), Term::number(2.0), Term::number(3.0)]);
		let result = evaluate(&tuple, &Env::new());
		match result {
			FlexValue::TupleValue { elements } => {
				assert_eq!(elements.len(), 3);
			}
			_ => panic!("Expected tuple"),
		}
	}

	#[test]
	fn unbound_var_becomes_stuck() {
		let term = Term::var(99, "unbound");
		let result = evaluate(&term, &Env::new());
		assert!(matches!(result, FlexValue::StuckValue(..)));
	}
}
