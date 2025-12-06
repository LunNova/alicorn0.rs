// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Runtime values with flex/strict distinction via pattern-wishcast.

use pattern_wishcast::pattern_wishcast;

// Forward declaration - actual type lives in elaborated.rs
// We use Box<crate::elaborated::Elaborated> in Closure
pub type SpannedName = String; // TODO: span + name

/// Native operatives - Rust implementations of syntax transformers.
/// These are the built-in operatives like `let`, `lambda`, `forall`, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NativeOperative {
	/// let name = expr (binds in environment, returns unit)
	Let,
	/// param -> body (lambda with inferred param type)
	Lambda,
	/// lambda_single (param : type) body - single-param lambda with explicit type
	LambdaSingle,
	/// forall (param : type) -> result_type (pi type)
	Forall,
	/// The arrow operator for lambda/pi sugar
	Arrow,
	/// type annotation: expr : type
	Annotate,
	/// type_(level, depth) - universe constructor (literal args only, Lua bootstrap jank)
	Type_,
	/// lambda_curry ((param : type)) body - lambda with implicit type parameter
	LambdaCurry,
	/// lambda_implicit (param : type) body - lambda with implicit param (single parens)
	LambdaImplicit,
	/// wrap T x - wrap a value of type T
	Wrap,
	/// unwrap T x - unwrap a wrapped value
	Unwrap,
	/// wrapped(T) - the type of wrapped values of type T
	Wrapped,
	// TODO: More operatives as needed
	// Mk, Switch, Enum, Intrinsic, etc.
}

// FIXME: pattern-wishcast macro doesn't support doc comments inside, unfuck it later
//
// StuckValue: Stuck evaluation states - computations that can't proceed.
//   Only appear in FlexValue, never in StrictValue.
//   - Free: de Bruijn index that escaped its binder
//   - Metavariable: awaiting unification
//   - Application: stuck function or argument
//
// Value: Runtime values, parameterized by pattern for strict/flex distinction.
//   - FlexValue: may contain stuck states anywhere in the tree
//   - StrictValue: guaranteed fully evaluated, no stuck states
//
//   Variant groups:
//   - Universe/Type formers: Star
//   - Function types: Pi, Closure
//   - Tuples: TupleValue, TupleType
//   - Primitive values: HostNumber, HostString, HostBool
//   - Host types: HostNumberType, HostStringType, HostBoolType
//
// StrictValue: Fully evaluated value - no stuck computations anywhere.
// FlexValue: Partially evaluated value - may contain stuck computations.
//
// TODO: Many more variants needed from Lua:
//   - Records, enums
//   - Ranges (lower_bounds, upper_bounds, relation)
//   - Effect rows
//   - param_info, result_info for Pi
//   - etc.
//
// Operatives:
//   - OperativeType: The TYPE of an operative (handler + userdata_type)
//   - OperativeCons: The VALUE of an operative (handler + userdata)

pattern_wishcast! {
	enum StuckValue = {
		Free { index: usize, debug_name: SpannedName },
		Metavariable { id: usize },
		Application { func: Box<FlexValue>, arg: Box<FlexValue> },
	};

	enum Value is <P: PatternFields> = StuckValue | {
		Star { level: u8, depth: u8 },

		Pi {
			param_type: Box<Self>,
			result_type: Box<Self>,
		},
		Closure {
			param_name: String,
			body: Box<crate::elaborated::Elaborated>,
			capture: Box<Self>,
		},

		TupleValue { elements: Vec<Self> },
		TupleType { desc: Box<Self> },

		HostNumber { value: f64 },
		HostString { value: String },
		HostBool { value: bool },

		HostNumberType,
		HostStringType,
		HostBoolType,
		HostTypeType,

		HostWrappedType { type_val: Box<Self> },
		HostWrappedValue { type_val: Box<Self>, content: Box<Self> },

		OperativeType {
			handler: NativeOperative,
			userdata_type: Box<Self>,
		},
		OperativeCons {
			handler: NativeOperative,
			userdata: Box<Self>,
		},
	};

	type StrictValue = Value is
		Star { .. } |
		Pi { .. } |
		Closure { .. } |
		TupleValue { .. } |
		TupleType { .. } |
		HostNumber { .. } |
		HostString { .. } |
		HostBool { .. } |
		HostNumberType |
		HostStringType |
		HostBoolType |
		HostTypeType |
		HostWrappedType { .. } |
		HostWrappedValue { .. } |
		OperativeType { .. } |
		OperativeCons { .. };

	type FlexValue = Value is _;

	#[derive(SubtypingRelation(upcast=to_flex, downcast=try_to_strict))]
	impl StrictValue : FlexValue;
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn strict_to_flex_upcast() {
		let strict = StrictValue::HostNumber { value: 42.0 };
		let flex: FlexValue = strict.to_flex();
		assert!(matches!(flex, FlexValue::HostNumber { value } if value == 42.0));
	}

	#[test]
	fn flex_to_strict_downcast_success() {
		let flex = FlexValue::HostNumber { value: 42.0 };
		let strict = flex.try_to_strict();
		assert!(strict.is_ok());
	}

	#[test]
	fn flex_to_strict_downcast_fail_on_stuck() {
		let flex = FlexValue::StuckValue(StuckValue::Metavariable { id: 0 }, ());
		let strict = flex.try_to_strict();
		assert!(strict.is_err());
	}

	#[test]
	fn nested_tuple_preserves_strictness() {
		let inner = StrictValue::HostNumber { value: 1.0 };
		let tuple = StrictValue::TupleValue { elements: vec![inner] };
		let flex = tuple.to_flex();
		assert!(flex.try_to_strict().is_ok());
	}

	#[test]
	fn nested_stuck_prevents_downcast() {
		let stuck = FlexValue::StuckValue(StuckValue::Metavariable { id: 0 }, ());
		let tuple = FlexValue::TupleValue { elements: vec![stuck] };
		assert!(tuple.try_to_strict().is_err());
	}
}
