// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Inferrable terms - pre-typechecking AST.
//!
//! These terms come from parsing/operatives and go into the type checker.
//! Unlike Lua, we don't have separate checkable/unanchored/anchored types.
//! Span is a field, and "check mode" is expressed via Annotated.

use crate::value::{FlexValue, NativeOperative, SpannedName};

/// Named intrinsics - Rust implementations of host escapes.
///
/// These replace the Lua `intrinsic "lua code string"` with fixed named operations.
/// Each variant corresponds to a specific host value or operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Intrinsic {
	// Host type constants (lines 43-50 of prelude.alc)
	/// `terms.strict_value.host_bool_type` → HostBoolType value
	HostBoolType,
	/// `terms.strict_value.host_string_type` → HostStringType value
	HostStringType,
	/// `terms.host_syntax_type` → the type for syntax objects
	HostSyntaxType,
	/// `terms.host_environment_type` → the type for environments
	HostEnvironmentType,
	/// `terms.host_goal_type` → the type for expression goals
	HostGoalType,
	/// `terms.host_inferrable_term_type` → the type for inferrable terms
	HostInferrableTermType,
	/// `terms.host_checkable_term_type` → the type for checkable terms
	HostCheckableTermType,
	/// `terms.host_lua_error_type` → the type for errors
	HostErrorType,
	// TODO: Add more intrinsics as needed:
	// - gen_base_operator variants
	// - host function intrinsics
}

impl Intrinsic {
	/// Look up an intrinsic by name.
	///
	/// The name is used as a key to find the corresponding Rust implementation.
	pub fn from_name(name: &str) -> Option<Self> {
		match name.trim() {
			"host-bool-type" => Some(Intrinsic::HostBoolType),
			"host-string-type" => Some(Intrinsic::HostStringType),
			"host-syntax-type" => Some(Intrinsic::HostSyntaxType),
			"host-environment-type" => Some(Intrinsic::HostEnvironmentType),
			"host-goal-type" => Some(Intrinsic::HostGoalType),
			"host-inferrable-term-type" => Some(Intrinsic::HostInferrableTermType),
			"host-checkable-term-type" => Some(Intrinsic::HostCheckableTermType),
			"host-error-type" => Some(Intrinsic::HostErrorType),
			_ => None,
		}
	}

	/// Get the canonical name for this intrinsic (for debugging/display).
	pub fn name(&self) -> &'static str {
		match self {
			Intrinsic::HostBoolType => "host-bool-type",
			Intrinsic::HostStringType => "host-string-type",
			Intrinsic::HostSyntaxType => "host-syntax-type",
			Intrinsic::HostEnvironmentType => "host-environment-type",
			Intrinsic::HostGoalType => "host-goal-type",
			Intrinsic::HostInferrableTermType => "host-inferrable-term-type",
			Intrinsic::HostCheckableTermType => "host-checkable-term-type",
			Intrinsic::HostErrorType => "host-error-type",
		}
	}
}

/// Parameter visibility for lambdas and pi types.
///
/// - `Explicit`: Must be passed at call site (normal function args)
/// - `Implicit`: Can be inferred from context (type parameters in lambda_curry)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
	#[default]
	Explicit,
	Implicit,
}

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

	/// Create a pre-elaborated term (type already known)
	pub fn typed(typ: crate::elaborated::Elaborated, term: crate::elaborated::Elaborated) -> Self {
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
		Self::lambda_with_visibility(param_name, param_type, Visibility::Explicit, body)
	}

	pub fn lambda_implicit(param_name: impl Into<String>, param_type: Option<Box<Inferrable>>, body: Inferrable) -> Self {
		Self::lambda_with_visibility(param_name, param_type, Visibility::Implicit, body)
	}

	pub fn lambda_with_visibility(
		param_name: impl Into<String>,
		param_type: Option<Box<Inferrable>>,
		visibility: Visibility,
		body: Inferrable,
	) -> Self {
		Self::with_dummy_span(InferrableKind::Lambda {
			param_name: param_name.into(),
			param_type,
			visibility,
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

	/// Create a wrapped type: wrapped(T)
	pub fn wrapped_type(inner_type: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::HostWrappedType {
			inner_type: Box::new(inner_type),
		})
	}

	/// Create a wrap term: wrap T x
	pub fn host_wrap(wrap_type: Inferrable, content: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::HostWrap {
			wrap_type: Box::new(wrap_type),
			content: Box::new(content),
		})
	}

	/// Create an unwrap term: unwrap T x
	pub fn host_unwrap(unwrap_type: Inferrable, container: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::HostUnwrap {
			unwrap_type: Box::new(unwrap_type),
			container: Box::new(container),
		})
	}

	/// Create a host intrinsic reference
	pub fn host_intrinsic(intrinsic: Intrinsic, intrinsic_type: Inferrable) -> Self {
		Self::with_dummy_span(InferrableKind::HostIntrinsic {
			intrinsic,
			intrinsic_type: Box::new(intrinsic_type),
		})
	}

	/// Create a pre-elaborated operative term.
	/// The type is OperativeType { handler, unit_type }
	/// The value is OperativeCons { handler, unit }
	pub fn native_operative(op: NativeOperative) -> Self {
		use crate::elaborated::Elaborated;

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

		Self::typed(Elaborated::Literal(op_type), Elaborated::Literal(op_val))
	}
}

/// The actual term variants
#[derive(Debug, Clone)]
pub enum InferrableKind {
	/// Pre-elaborated term - type already known, no inference needed.
	/// Used for built-in operatives and already-typechecked terms.
	Typed {
		typ: Box<crate::elaborated::Elaborated>,
		term: Box<crate::elaborated::Elaborated>,
	},

	/// Reference to a bound variable (de Bruijn index)
	BoundVariable { index: usize, debug_name: SpannedName },

	/// A literal value (already has a type)
	Literal(FlexValue),

	/// Lambda abstraction
	/// param_type is optional - if None, will be inferred (creates metavar)
	/// visibility controls whether param must be passed explicitly at call site
	Lambda {
		param_name: String,
		param_type: Option<Box<Inferrable>>,
		visibility: Visibility,
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

	/// Host intrinsic reference - named native operation
	HostIntrinsic {
		intrinsic: Intrinsic,
		intrinsic_type: Box<Inferrable>,
	},

	/// wrapped(T) - the type of wrapped values of type T
	HostWrappedType { inner_type: Box<Inferrable> },

	/// wrap T x - wrap a value of type T
	HostWrap {
		wrap_type: Box<Inferrable>,
		content: Box<Inferrable>,
	},

	/// unwrap T x - unwrap a wrapped value
	HostUnwrap {
		unwrap_type: Box<Inferrable>,
		container: Box<Inferrable>,
	},
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
