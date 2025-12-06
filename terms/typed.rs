// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Typed terms - the input to the evaluator.
//!
//! These are fully type-checked terms. Variables use de Bruijn indices.

use crate::value::{FlexValue, SpannedName};

// Typed terms - what the evaluator operates on.
// These have passed type checking already.
//
// Minimal skeleton for: let x = 5; x
// Which desugars to: (λx. x) 5
//
// TODO: Many more variants from Lua typed.lua:
//   - pi (dependent function type)
//   - tuple_cons, tuple_elim, tuple_type
//   - record_cons, record_elim, record_type
//   - enum variants
//   - host_intrinsic
//   - program/effect stuff
//   - etc.

#[derive(Debug, Clone)]
pub enum Term {
	/// A literal value (already evaluated)
	Literal(FlexValue),

	/// A bound variable, de Bruijn indexed.
	/// Index 0 = innermost binder, 1 = next out, etc.
	BoundVariable { index: usize, debug_name: SpannedName },

	/// Lambda abstraction
	Lambda { param_name: String, body: Box<Term> },

	/// Application of function to argument
	Application { func: Box<Term>, arg: Box<Term> },

	/// Tuple constructor
	TupleCons { elements: Vec<Term> },

	/// Type annotation (the term, checked against type)
	/// Useful for literals that need a specific type
	Annotated { term: Box<Term>, ty: Box<Term> },
}

impl Term {
	/// Create a literal number term
	pub fn number(n: f64) -> Self {
		Term::Literal(FlexValue::HostNumber { value: n })
	}

	/// Create a literal string term
	pub fn string(s: String) -> Self {
		Term::Literal(FlexValue::HostString { value: s })
	}

	/// Create a literal bool term
	pub fn bool(b: bool) -> Self {
		Term::Literal(FlexValue::HostBool { value: b })
	}

	/// Create a bound variable reference
	pub fn var(index: usize, name: impl Into<String>) -> Self {
		Term::BoundVariable {
			index,
			debug_name: name.into(),
		}
	}

	/// Create a lambda
	pub fn lambda(param: impl Into<String>, body: Term) -> Self {
		Term::Lambda {
			param_name: param.into(),
			body: Box::new(body),
		}
	}

	/// Create an application
	pub fn app(func: Term, arg: Term) -> Self {
		Term::Application {
			func: Box::new(func),
			arg: Box::new(arg),
		}
	}

	/// Create a tuple
	pub fn tuple(elements: Vec<Term>) -> Self {
		Term::TupleCons { elements }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn can_construct_identity() {
		// λx. x
		let id = Term::lambda("x", Term::var(0, "x"));
		assert!(matches!(id, Term::Lambda { .. }));
	}

	#[test]
	fn can_construct_application() {
		// (λx. x) 42
		let id = Term::lambda("x", Term::var(0, "x"));
		let app = Term::app(id, Term::number(42.0));
		assert!(matches!(app, Term::Application { .. }));
	}

	#[test]
	fn can_construct_nested_lambda() {
		// λx. λy. x  (K combinator)
		let k = Term::lambda("x", Term::lambda("y", Term::var(1, "x")));
		assert!(matches!(k, Term::Lambda { .. }));
	}
}
