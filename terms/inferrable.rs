// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Inferrable terms - pre-typechecking AST.
//!
//! These terms come from parsing/operatives and go into the type checker.
//! Unlike Lua, we don't have separate checkable/unanchored/anchored types.
//! Span is a field, and "check mode" is expressed via Annotated.

use crate::value::{FlexValue, NativeOperative, SpannedName};

/// Source span for error messages
#[derive(Debug, Clone, Default)]
pub struct Span {
	// TODO: actual span data (file, line, col, etc.)
	pub debug: String,
}

impl Span {
	pub fn new(debug: impl Into<String>) -> Self {
		Self { debug: debug.into() }
	}

	pub fn dummy() -> Self {
		Self { debug: "<dummy>".into() }
	}
}

/// Inferrable term - the output of operatives, input to type checker.
#[derive(Debug, Clone)]
pub struct Inferrable {
	pub span: Span,
	pub kind: InferrableKind,
}

impl Inferrable {
	pub fn new(span: Span, kind: InferrableKind) -> Self {
		Self { span, kind }
	}

	pub fn with_dummy_span(kind: InferrableKind) -> Self {
		Self { span: Span::dummy(), kind }
	}

	// Convenience constructors

	/// Create a pre-typed term (type already known)
	pub fn typed(typ: crate::typed::Term, term: crate::typed::Term) -> Self {
		Self::with_dummy_span(InferrableKind::Typed {
			typ: Box::new(typ),
			term: Box::new(term),
		})
	}

	pub fn bound_variable(index: usize, debug_name: impl Into<String>) -> Self {
		Self::with_dummy_span(InferrableKind::BoundVariable {
			index,
			debug_name: debug_name.into(),
		})
	}

	pub fn literal(value: FlexValue) -> Self {
		Self::with_dummy_span(InferrableKind::Literal(value))
	}

	pub fn number(n: f64) -> Self {
		Self::literal(FlexValue::HostNumber { value: n })
	}

	pub fn string(s: impl Into<String>) -> Self {
		Self::literal(FlexValue::HostString { value: s.into() })
	}

	pub fn bool(b: bool) -> Self {
		Self::literal(FlexValue::HostBool { value: b })
	}

	pub fn lambda(param_name: impl Into<String>, param_type: Option<Box<Inferrable>>, body: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::Lambda {
			param_name: param_name.into(),
			param_type,
			body: Box::new(body),
		})
	}

	pub fn application(func: Inferrable, arg: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::Application {
			func: Box::new(func),
			arg: Box::new(arg),
		})
	}

	pub fn annotated(term: Inferrable, expected_type: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::Annotated {
			term: Box::new(term),
			expected_type: Box::new(expected_type),
		})
	}

	pub fn let_bind(name: impl Into<String>, expr: Inferrable, body: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::Let {
			name: name.into(),
			debug_name: None,
			expr: Box::new(expr),
			body: Box::new(body),
		})
	}

	pub fn tuple(elements: Vec<Inferrable>) -> Self {
		Self::with_dummy_span(InferrableKind::TupleCons { elements })
	}

	pub fn unit() -> Self {
		Self::tuple(vec![])
	}

	pub fn pi(param_name: impl Into<String>, param_type: Inferrable, result_type: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::Pi {
			param_name: param_name.into(),
			param_type: Box::new(param_type),
			result_type: Box::new(result_type),
		})
	}

	/// Create a pre-typed operative term.
	/// The type is OperativeType { handler, unit_type }
	/// The value is OperativeCons { handler, unit }
	pub fn native_operative(op: NativeOperative) -> Self {
		use crate::typed::Term;

		let unit_type = FlexValue::TupleType {
			desc: Box::new(FlexValue::TupleValue { elements: vec![] }),
		};
		let unit_val = FlexValue::TupleValue { elements: vec![] };

		// The TYPE of the operative
		let op_type = FlexValue::OperativeType {
			handler: op,
			userdata_type: Box::new(unit_type.clone()),
		};

		// The VALUE of the operative
		let op_val = FlexValue::OperativeCons {
			handler: op,
			userdata: Box::new(unit_val),
		};

		Self::typed(Term::Literal(op_type), Term::Literal(op_val))
	}
}

/// The actual term variants
#[derive(Debug, Clone)]
pub enum InferrableKind {
	/// Pre-elaborated term - type already known, no inference needed.
	/// Used for built-in operatives and already-typechecked terms.
	Typed {
		typ: Box<crate::typed::Term>,
		term: Box<crate::typed::Term>,
	},

	/// Reference to a bound variable (de Bruijn index)
	BoundVariable { index: usize, debug_name: SpannedName },

	/// A literal value (already has a type)
	Literal(FlexValue),

	/// Lambda abstraction
	/// param_type is optional - if None, will be inferred (creates metavar)
	Lambda {
		param_name: String,
		param_type: Option<Box<Inferrable>>,
		body: Box<Inferrable>,
	},

	/// Function application
	Application { func: Box<Inferrable>, arg: Box<Inferrable> },

	/// Type annotation - "check mode"
	/// The term should have the expected_type
	Annotated {
		term: Box<Inferrable>,
		expected_type: Box<Inferrable>,
	},

	/// Let binding: let name = expr in body
	Let {
		name: String,
		debug_name: Option<SpannedName>,
		expr: Box<Inferrable>,
		body: Box<Inferrable>,
	},

	/// Tuple constructor
	TupleCons { elements: Vec<Inferrable> },

	/// Pi type (dependent function type)
	Pi {
		param_name: String,
		param_type: Box<Inferrable>,
		result_type: Box<Inferrable>,
	},

	/// Enum constructor: #Tag(args)
	EnumCons { constructor: String, arg: Box<Inferrable> },

	/// Record constructor: { field1 = val1, field2 = val2 }
	RecordCons { fields: Vec<(String, Inferrable)> },

	/// Operative constructor (first-class syntax transformer)
	OperativeCons {
		operative_type: Box<Inferrable>,
		userdata: Box<Inferrable>,
	},

	/// Host intrinsic reference
	HostIntrinsic { name: String, intrinsic_type: Box<Inferrable> },
	// TODO: More variants as needed:
	// - TupleElim, TupleType
	// - RecordElim, RecordType
	// - EnumCase, EnumType
	// - Program/Effect stuff
	// - etc.
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn can_construct_identity() {
		// λx. x
		let id = Inferrable::lambda("x", None, Inferrable::bound_variable(0, "x"));
		assert!(matches!(id.kind, InferrableKind::Lambda { .. }));
	}

	#[test]
	fn can_construct_let() {
		// let x = 5 in x
		let term = Inferrable::let_bind("x", Inferrable::number(5.0), Inferrable::bound_variable(0, "x"));
		assert!(matches!(term.kind, InferrableKind::Let { .. }));
	}

	#[test]
	fn can_construct_annotated() {
		// (42 : Number)
		let term = Inferrable::annotated(Inferrable::number(42.0), Inferrable::literal(FlexValue::HostNumberType));
		assert!(matches!(term.kind, InferrableKind::Annotated { .. }));
	}
}
