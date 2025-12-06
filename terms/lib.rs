// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Term types for Alicorn.
//!
//! Term hierarchy:
//! - `inferrable`: Pre-typechecking terms (surface syntax from operatives)
//! - `elaborated`: Fully elaborated terms (core calculus, evaluator input)
//! - `value`: Runtime values with flex/strict distinction
//!
//! Pipeline: FormatList → Inferrable → Elaborated → Value
//!
//! See CLAUDE.md for architecture details.

pub mod check;
pub mod elaborated;
pub mod eval;
pub mod infer;
pub mod inferrable;
pub mod intrinsic_compat;
pub mod value;

// Re-exports for convenience
pub use check::{CheckError, elaborate};
pub use elaborated::Elaborated;
pub use eval::{Env, evaluate};
pub use infer::{InferError, TypingContext, as_operative_type, infer, is_pi_type, type_of_value};
pub use inferrable::{Inferrable, InferrableKind, Intrinsic, Span, Visibility};
pub use value::{FlexValue, NativeOperative, SpannedName, StrictValue, StuckValue};
