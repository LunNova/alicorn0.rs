// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Elaborated terms - the internal representation after type checking.
//!
//! These are the "core" terms that the evaluator operates on.
//! Variables use de Bruijn indices. All types have been checked.
//!
//! Pipeline: FormatList → Inferrable → Elaborated → Value
//!           (syntax)     (surface)    (core)       (runtime)

use crate::value::{FlexValue, SpannedName};

/// Elaborated term - fully type-checked, ready for evaluation.
///
/// This is the internal "core calculus" representation.
/// Unlike Inferrable (which may have holes/unknowns), Elaborated
/// terms are complete and can be directly evaluated.
//
// TODO: Many more variants from Lua typed.lua:
//   - pi (dependent function type)
//   - tuple_elim, tuple_type
//   - record_cons, record_elim, record_type
//   - enum variants
//   - host_intrinsic
//   - program/effect stuff
//   - etc.
#[derive(Debug, Clone)]
pub enum Elaborated {
	/// A literal value (already evaluated)
	Literal(FlexValue),

	/// A bound variable, de Bruijn indexed.
	/// Index 0 = innermost binder, 1 = next out, etc.
	BoundVariable { index: usize, debug_name: SpannedName },

	/// Lambda abstraction
	Lambda { param_name: String, body: Box<Elaborated> },

	/// Application of function to argument
	Application { func: Box<Elaborated>, arg: Box<Elaborated> },

	/// Tuple constructor
	TupleCons { elements: Vec<Elaborated> },

	/// Type annotation (the term, checked against type)
	/// Useful for literals that need a specific type
	Annotated { term: Box<Elaborated>, ty: Box<Elaborated> },
}

impl Elaborated {
	/// Create a literal number term
	pub fn number(n: f64) -> Self {
		Elaborated::Literal(FlexValue::HostNumber { value: n })
	}

	/// Create a literal string term
	pub fn string(s: String) -> Self {
		Elaborated::Literal(FlexValue::HostString { value: s })
	}

	/// Create a literal bool term
	pub fn bool(b: bool) -> Self {
		Elaborated::Literal(FlexValue::HostBool { value: b })
	}

	/// Create a bound variable reference
	pub fn var(index: usize, name: impl Into<String>) -> Self {
		Elaborated::BoundVariable {
			index,
			debug_name: name.into(),
		}
	}

	/// Create a lambda
	pub fn lambda(param: impl Into<String>, body: Elaborated) -> Self {
		Elaborated::Lambda {
			param_name: param.into(),
			body: Box::new(body),
		}
	}

	/// Create an application
	pub fn app(func: Elaborated, arg: Elaborated) -> Self {
		Elaborated::Application {
			func: Box::new(func),
			arg: Box::new(arg),
		}
	}

	/// Create a tuple
	pub fn tuple(elements: Vec<Elaborated>) -> Self {
		Elaborated::TupleCons { elements }
	}

	/// Create unit (empty tuple)
	pub fn unit() -> Self {
		Elaborated::TupleCons { elements: vec![] }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn can_construct_identity() {
		// λx. x
		let id = Elaborated::lambda("x", Elaborated::var(0, "x"));
		assert!(matches!(id, Elaborated::Lambda { .. }));
	}

	#[test]
	fn can_construct_application() {
		// (λx. x) 42
		let id = Elaborated::lambda("x", Elaborated::var(0, "x"));
		let app = Elaborated::app(id, Elaborated::number(42.0));
		assert!(matches!(app, Elaborated::Application { .. }));
	}

	#[test]
	fn can_construct_nested_lambda() {
		// λx. λy. x  (K combinator)
		let k = Elaborated::lambda("x", Elaborated::lambda("y", Elaborated::var(1, "x")));
		assert!(matches!(k, Elaborated::Lambda { .. }));
	}
}
