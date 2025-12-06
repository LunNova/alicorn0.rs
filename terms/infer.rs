// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Minimal type inference for the expression evaluator.
//!
//! This is a skeleton that handles just enough to:
//! - Extract types from pre-typed terms (Typed variant)
//! - Look up bound variable types
//! - Synthesize types for literals
//!
//! Full type inference with constraint solving comes later.

use crate::eval::{Env, evaluate};
use crate::inferrable::{Inferrable, InferrableKind};
use crate::value::FlexValue;

/// Typing context - maps de Bruijn indices to their types.
#[derive(Debug, Clone, Default)]
pub struct TypingContext {
	/// Stack of (name, type) pairs. Index 0 = most recent binding.
	entries: Vec<(String, FlexValue)>,
}

impl TypingContext {
	pub fn new() -> Self {
		Self { entries: vec![] }
	}

	/// Push a new binding onto the context
	pub fn push(&mut self, name: String, typ: FlexValue) {
		self.entries.push((name, typ));
	}

	/// Pop a binding from the context
	pub fn pop(&mut self) -> Option<(String, FlexValue)> {
		self.entries.pop()
	}

	/// Extend context with a new binding, returning new context
	pub fn extend(&self, name: String, typ: FlexValue) -> Self {
		let mut new_ctx = self.clone();
		new_ctx.push(name, typ);
		new_ctx
	}

	/// Look up a de Bruijn index. Index 0 = most recent binding.
	pub fn lookup(&self, index: usize) -> Option<&FlexValue> {
		if index < self.entries.len() {
			Some(&self.entries[self.entries.len() - 1 - index].1)
		} else {
			None
		}
	}

	pub fn len(&self) -> usize {
		self.entries.len()
	}

	pub fn is_empty(&self) -> bool {
		self.entries.is_empty()
	}
}

/// Errors that can occur during type inference
#[derive(Debug, Clone)]
pub enum InferError {
	UnboundVariable { index: usize, debug_name: String },
	CannotInfer(String),
	NotYetImplemented(String),
}

impl std::fmt::Display for InferError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			InferError::UnboundVariable { index, debug_name } => {
				write!(f, "Unbound variable: {} (index {})", debug_name, index)
			}
			InferError::CannotInfer(msg) => write!(f, "Cannot infer type: {}", msg),
			InferError::NotYetImplemented(msg) => write!(f, "Not yet implemented: {}", msg),
		}
	}
}

impl std::error::Error for InferError {}

pub type InferResult<T> = Result<T, InferError>;

/// Infer the type of an inferrable term.
///
/// This is a minimal skeleton that handles:
/// - Typed: extract the embedded type
/// - BoundVariable: look up in context
/// - Literal: synthesize type from value
///
/// Other cases return NotYetImplemented for now.
pub fn infer(term: &Inferrable, ctx: &TypingContext) -> InferResult<FlexValue> {
	match &term.kind {
		// Pre-typed term: just evaluate the embedded type
		InferrableKind::Typed { typ, term: _ } => {
			// The type is a Term (typed term), evaluate it to get a value
			let env = Env::new();
			Ok(evaluate(typ, &env))
		}

		// Bound variable: convert level to index and look up in typing context
		InferrableKind::BoundVariable { index: level, debug_name } => {
			// Convert 0-indexed level to 0-indexed de Bruijn index
			// Check bounds to avoid underflow
			if *level >= ctx.len() {
				return Err(InferError::UnboundVariable {
					index: *level,
					debug_name: debug_name.clone(),
				});
			}
			let actual_index = ctx.len() - level - 1;
			ctx.lookup(actual_index).cloned().ok_or_else(|| InferError::UnboundVariable {
				index: actual_index,
				debug_name: debug_name.clone(),
			})
		}

		// Literal: synthesize type from value
		InferrableKind::Literal(value) => Ok(type_of_value(value)),

		// Lambda without annotation: would need metavariable
		InferrableKind::Lambda { param_type: None, .. } => Err(InferError::NotYetImplemented(
			"Lambda without type annotation requires metavariables".to_string(),
		)),

		// Lambda with annotation: infer param type, extend context, infer body
		InferrableKind::Lambda {
			param_name,
			param_type: Some(param_ty),
			body,
		} => {
			let param_type_val = infer(param_ty, ctx)?;
			let body_ctx = ctx.extend(param_name.clone(), param_type_val.clone());
			let body_type = infer(body, &body_ctx)?;

			Ok(FlexValue::Pi {
				param_type: Box::new(param_type_val),
				result_type: Box::new(body_type),
			})
		}

		// Application: infer func type, check it's pi, return result type
		InferrableKind::Application { func, arg: _ } => {
			let func_type = infer(func, ctx)?;
			match func_type {
				FlexValue::Pi { result_type, .. } => Ok(*result_type),
				_ => Err(InferError::CannotInfer("Application of non-function".to_string())),
			}
		}

		// Annotated: the annotation IS the type
		InferrableKind::Annotated { expected_type, .. } => infer(expected_type, ctx),

		// Let: infer body type (after binding expr)
		InferrableKind::Let { name, expr, body, .. } => {
			let expr_type = infer(expr, ctx)?;
			let body_ctx = ctx.extend(name.clone(), expr_type);
			infer(body, &body_ctx)
		}

		// Tuple: infer element types, construct tuple type
		InferrableKind::TupleCons { elements } => {
			let element_types: Result<Vec<_>, _> = elements.iter().map(|e| infer(e, ctx)).collect();
			Ok(FlexValue::TupleType {
				desc: Box::new(FlexValue::TupleValue { elements: element_types? }),
			})
		}

		// Pi type: it's a type, its type is Star
		InferrableKind::Pi { .. } => {
			// Pi types live in Star
			Ok(FlexValue::Star { level: 0, depth: 1 })
		}

		// Other cases not yet implemented
		_ => Err(InferError::NotYetImplemented(format!(
			"Inference for {:?}",
			std::mem::discriminant(&term.kind)
		))),
	}
}

/// Synthesize the type of a value.
///
/// This is simple: numbers have NumberType, strings have StringType, etc.
pub fn type_of_value(value: &FlexValue) -> FlexValue {
	match value {
		FlexValue::HostNumber { .. } => FlexValue::HostNumberType,
		FlexValue::HostString { .. } => FlexValue::HostStringType,
		FlexValue::HostBool { .. } => FlexValue::HostBoolType,

		// Type values are types of types (Star)
		FlexValue::HostNumberType | FlexValue::HostStringType | FlexValue::HostBoolType => FlexValue::Star { level: 0, depth: 1 },

		// Operative type: its type is... also Star (it's a type)
		FlexValue::OperativeType { .. } => FlexValue::Star { level: 0, depth: 1 },

		// Operative value: its type is the operative type
		FlexValue::OperativeCons { handler, userdata } => FlexValue::OperativeType {
			handler: *handler,
			userdata_type: Box::new(type_of_value(userdata)),
		},

		// Tuple value: type is TupleType of element types
		FlexValue::TupleValue { elements } => {
			let element_types: Vec<FlexValue> = elements.iter().map(type_of_value).collect();
			FlexValue::TupleType {
				desc: Box::new(FlexValue::TupleValue { elements: element_types }),
			}
		}

		// Closure: its type is Pi
		FlexValue::Closure { .. } => {
			// Would need to inspect the closure to get its type
			// For now, return a placeholder
			FlexValue::Star { level: 0, depth: 1 }
		}

		// Pi type: its type is Star
		FlexValue::Pi { .. } => FlexValue::Star { level: 0, depth: 1 },

		// Star: type of Star(n) is Star(n+1)
		FlexValue::Star { level, depth } => FlexValue::Star {
			level: level + 1,
			depth: *depth,
		},

		// Tuple type: its type is Star
		FlexValue::TupleType { .. } => FlexValue::Star { level: 0, depth: 1 },

		// Stuck values: type is unknown/stuck
		FlexValue::StuckValue(_, _) => {
			// Return a placeholder for stuck values
			FlexValue::Star { level: 0, depth: 1 }
		}
	}
}

/// Check if a type is an operative type, and if so, extract the handler.
pub fn as_operative_type(typ: &FlexValue) -> Option<crate::value::NativeOperative> {
	match typ {
		FlexValue::OperativeType { handler, .. } => Some(*handler),
		_ => None,
	}
}

/// Check if a type is a Pi type (function type).
pub fn is_pi_type(typ: &FlexValue) -> bool {
	matches!(typ, FlexValue::Pi { .. })
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::value::NativeOperative;

	#[test]
	fn infer_literal_number() {
		let term = Inferrable::number(42.0);
		let ctx = TypingContext::new();
		let typ = infer(&term, &ctx).unwrap();
		assert!(matches!(typ, FlexValue::HostNumberType));
	}

	#[test]
	fn infer_typed_operative() {
		let term = Inferrable::native_operative(NativeOperative::Let);
		let ctx = TypingContext::new();
		let typ = infer(&term, &ctx).unwrap();
		assert!(matches!(
			typ,
			FlexValue::OperativeType {
				handler: NativeOperative::Let,
				..
			}
		));
	}

	#[test]
	fn infer_bound_variable() {
		let term = Inferrable::bound_variable(0, "x");
		let ctx = TypingContext::new().extend("x".to_string(), FlexValue::HostNumberType);
		let typ = infer(&term, &ctx).unwrap();
		assert!(matches!(typ, FlexValue::HostNumberType));
	}

	#[test]
	fn infer_unbound_variable_fails() {
		let term = Inferrable::bound_variable(99, "unbound");
		let ctx = TypingContext::new();
		let result = infer(&term, &ctx);
		assert!(result.is_err());
	}
}
