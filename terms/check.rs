// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Elaboration: Inferrable → Elaborated
//!
//! This module converts surface terms (Inferrable) to core terms (Elaborated).
//! It performs type checking and fills in any holes.
//!
//! This is a minimal skeleton - real elaboration would include:
//! - Constraint generation and solving
//! - Metavariable unification
//! - Subtyping checks
//! - etc.

use crate::elaborated::Elaborated;
use crate::eval::{Env as EvalEnv, evaluate};
use crate::infer::TypingContext;
use crate::inferrable::{Inferrable, InferrableKind};
use crate::value::FlexValue;

/// Errors during elaboration
#[derive(Debug, Clone)]
pub enum CheckError {
	/// Variable not in scope
	UnboundVariable { index: usize, name: String },
	/// Type mismatch
	TypeMismatch { expected: FlexValue, got: FlexValue },
	/// Feature not yet implemented
	NotImplemented(String),
}

impl std::fmt::Display for CheckError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CheckError::UnboundVariable { index, name } => {
				write!(f, "Unbound variable: {} (index {})", name, index)
			}
			CheckError::TypeMismatch { expected, got } => {
				write!(f, "Type mismatch: expected {:?}, got {:?}", expected, got)
			}
			CheckError::NotImplemented(msg) => {
				write!(f, "Not yet implemented: {}", msg)
			}
		}
	}
}

impl std::error::Error for CheckError {}

pub type CheckResult<T> = Result<T, CheckError>;

/// Elaborate an Inferrable term to an Elaborated term.
///
/// Returns both the elaborated term and its inferred type.
///
/// This is a simple skeleton that handles:
/// - Typed: already elaborated, just extract
/// - Literal: wrap value
/// - BoundVariable: lookup type in context
/// - Lambda: elaborate body with extended context
/// - Application: elaborate func and arg
/// - Let: desugar to application of lambda
/// - TupleCons: elaborate elements
pub fn elaborate(term: &Inferrable, ctx: &TypingContext) -> CheckResult<(Elaborated, FlexValue)> {
	match &term.kind {
		// Already elaborated - just extract the embedded term and type
		InferrableKind::Typed { typ, term } => {
			let type_val = evaluate(typ, &EvalEnv::new());
			Ok((term.as_ref().clone(), type_val))
		}

		// Literal value - type is synthesized from the value
		InferrableKind::Literal(val) => {
			let typ = crate::infer::type_of_value(val);
			Ok((Elaborated::Literal(val.clone()), typ))
		}

		// Bound variable - convert level to de Bruijn index and lookup type
		InferrableKind::BoundVariable { index: level, debug_name } => {
			// Convert 0-indexed level to 0-indexed de Bruijn index
			// Level = position from bottom of context (fixed at bind time)
			// Index = position from top of context (what evaluator expects)
			// Check bounds to avoid underflow
			if *level >= ctx.len() {
				return Err(CheckError::UnboundVariable {
					index: *level,
					name: debug_name.clone(),
				});
			}
			let actual_index = ctx.len() - level - 1;
			let typ = ctx.lookup(actual_index).cloned().ok_or_else(|| CheckError::UnboundVariable {
				index: actual_index,
				name: debug_name.clone(),
			})?;
			Ok((
				Elaborated::BoundVariable {
					index: actual_index,
					debug_name: debug_name.clone(),
				},
				typ,
			))
		}

		// Lambda with type annotation
		// TODO: Use visibility for implicit arg handling when constraint solver is done
		InferrableKind::Lambda {
			param_name,
			param_type: Some(param_type_inferrable),
			visibility: _,
			body,
		} => {
			// Elaborate the parameter type
			let (param_type_elab, _) = elaborate(param_type_inferrable, ctx)?;
			let param_type_val = evaluate(&param_type_elab, &EvalEnv::new());

			// Elaborate body with extended context
			let body_ctx = ctx.extend(param_name.clone(), param_type_val.clone());
			let (body_elab, body_type) = elaborate(body, &body_ctx)?;

			let result_type = FlexValue::Pi {
				param_type: Box::new(param_type_val),
				result_type: Box::new(body_type),
			};

			Ok((
				Elaborated::Lambda {
					param_name: param_name.clone(),
					body: Box::new(body_elab),
				},
				result_type,
			))
		}

		// Lambda without annotation - requires type inference with metavariables
		InferrableKind::Lambda { param_type: None, .. } => Err(CheckError::NotImplemented(
			"Lambda without type annotation - use `lambda_single (x : Type) body` syntax".to_string(),
		)),

		// Application
		InferrableKind::Application { func, arg } => {
			let (func_elab, func_type) = elaborate(func, ctx)?;
			let (arg_elab, _arg_type) = elaborate(arg, ctx)?;

			// Extract result type from Pi
			let result_type = match func_type {
				FlexValue::Pi { result_type, .. } => *result_type,
				_ => {
					// For now, just return a placeholder
					// Real impl would error or handle stuck types
					FlexValue::Star { level: 0, depth: 1 }
				}
			};

			Ok((
				Elaborated::Application {
					func: Box::new(func_elab),
					arg: Box::new(arg_elab),
				},
				result_type,
			))
		}

		// Type annotation
		InferrableKind::Annotated { term, expected_type } => {
			let (type_elab, _) = elaborate(expected_type, ctx)?;
			let expected_type_val = evaluate(&type_elab, &EvalEnv::new());

			let (term_elab, _inferred_type) = elaborate(term, ctx)?;

			// TODO: Check that inferred_type <: expected_type_val
			// For now, just trust the annotation

			Ok((term_elab, expected_type_val))
		}

		// Let binding: desugar to (λname. body) expr
		InferrableKind::Let { name, expr, body, .. } => {
			let (expr_elab, expr_type) = elaborate(expr, ctx)?;

			// Elaborate body with binding
			let body_ctx = ctx.extend(name.clone(), expr_type);
			let (body_elab, body_type) = elaborate(body, &body_ctx)?;

			// Desugar: let x = e in b  →  (λx. b) e
			let lambda = Elaborated::Lambda {
				param_name: name.clone(),
				body: Box::new(body_elab),
			};

			Ok((
				Elaborated::Application {
					func: Box::new(lambda),
					arg: Box::new(expr_elab),
				},
				body_type,
			))
		}

		// Tuple constructor
		InferrableKind::TupleCons { elements } => {
			let mut elab_elements = Vec::with_capacity(elements.len());
			let mut type_elements = Vec::with_capacity(elements.len());

			for elem in elements {
				let (elem_elab, elem_type) = elaborate(elem, ctx)?;
				elab_elements.push(elem_elab);
				type_elements.push(elem_type);
			}

			let tuple_type = FlexValue::TupleType {
				desc: Box::new(FlexValue::TupleValue { elements: type_elements }),
			};

			Ok((Elaborated::TupleCons { elements: elab_elements }, tuple_type))
		}

		// Pi type
		InferrableKind::Pi {
			param_name,
			param_type,
			result_type,
		} => {
			let (param_type_elab, _) = elaborate(param_type, ctx)?;
			let param_type_val = evaluate(&param_type_elab, &EvalEnv::new());

			let result_ctx = ctx.extend(param_name.clone(), param_type_val.clone());
			let (result_type_elab, _) = elaborate(result_type, &result_ctx)?;
			let result_type_val = evaluate(&result_type_elab, &EvalEnv::new());

			let pi_val = FlexValue::Pi {
				param_type: Box::new(param_type_val),
				result_type: Box::new(result_type_val),
			};

			// Pi type elaborates to a literal of that type
			// The type of a type is Star
			Ok((Elaborated::Literal(pi_val.clone()), FlexValue::Star { level: 0, depth: 1 }))
		}

		// wrapped(T) - type constructor for wrapped types
		InferrableKind::HostWrappedType { inner_type } => {
			let (inner_elab, _) = elaborate(inner_type, ctx)?;

			// The type of wrapped(T) is host-type
			Ok((
				Elaborated::HostWrappedType {
					type_term: Box::new(inner_elab),
				},
				FlexValue::HostTypeType,
			))
		}

		// wrap T x - wrap a value
		InferrableKind::HostWrap { wrap_type, content } => {
			let (type_elab, _) = elaborate(wrap_type, ctx)?;
			let type_val = evaluate(&type_elab, &EvalEnv::new());
			let (content_elab, _content_type) = elaborate(content, ctx)?;

			// The result type is wrapped(T)
			let result_type = FlexValue::HostWrappedType {
				type_val: Box::new(type_val),
			};

			Ok((
				Elaborated::HostWrap {
					type_term: Box::new(type_elab),
					content: Box::new(content_elab),
				},
				result_type,
			))
		}

		// unwrap T x - unwrap a value
		InferrableKind::HostUnwrap { unwrap_type, container } => {
			let (type_elab, _) = elaborate(unwrap_type, ctx)?;
			let type_val = evaluate(&type_elab, &EvalEnv::new());
			let (container_elab, _container_type) = elaborate(container, ctx)?;

			// The result type is T (the unwrapped type)
			Ok((
				Elaborated::HostUnwrap {
					type_term: Box::new(type_elab),
					container: Box::new(container_elab),
				},
				type_val,
			))
		}

		// Host intrinsic - evaluate the intrinsic to its constant value
		InferrableKind::HostIntrinsic { intrinsic, intrinsic_type } => {
			use crate::inferrable::Intrinsic;

			// Elaborate the type annotation
			let (type_elab, _) = elaborate(intrinsic_type, ctx)?;
			let result_type = evaluate(&type_elab, &EvalEnv::new());

			// Get the constant value for this intrinsic
			let value = match intrinsic {
				Intrinsic::HostBoolType => FlexValue::HostBoolType,
				Intrinsic::HostStringType => FlexValue::HostStringType,
				Intrinsic::HostSyntaxType => FlexValue::HostSyntaxType,
				Intrinsic::HostEnvironmentType => FlexValue::HostEnvironmentType,
				Intrinsic::HostGoalType => FlexValue::HostGoalType,
				Intrinsic::HostInferrableTermType => FlexValue::HostInferrableTermType,
				Intrinsic::HostCheckableTermType => FlexValue::HostCheckableTermType,
				Intrinsic::HostErrorType => FlexValue::HostErrorType,
			};

			Ok((Elaborated::Literal(value), result_type))
		}

		// Not yet implemented
		_ => Err(CheckError::NotImplemented(format!(
			"Elaboration for {:?}",
			std::mem::discriminant(&term.kind)
		))),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn elaborate_literal() {
		let term = Inferrable::number(42.0);
		let ctx = TypingContext::new();
		let (elab, typ) = elaborate(&term, &ctx).unwrap();

		assert!(matches!(elab, Elaborated::Literal(FlexValue::HostNumber { value }) if value == 42.0));
		assert!(matches!(typ, FlexValue::HostNumberType));
	}

	#[test]
	fn elaborate_typed() {
		use crate::value::NativeOperative;

		let term = Inferrable::native_operative(NativeOperative::Let);
		let ctx = TypingContext::new();
		let (_, typ) = elaborate(&term, &ctx).unwrap();

		assert!(matches!(typ, FlexValue::OperativeType { .. }));
	}

	#[test]
	fn elaborate_let_desugars() {
		// let x = 5 in x  →  (λx. x) 5
		let term = Inferrable::let_bind("x", Inferrable::number(5.0), Inferrable::bound_variable(0, "x"));
		let ctx = TypingContext::new();
		let (elab, _) = elaborate(&term, &ctx).unwrap();

		// Should be Application { Lambda, 5 }
		assert!(matches!(elab, Elaborated::Application { .. }));
	}
}
