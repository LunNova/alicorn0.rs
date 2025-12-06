// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Term types for Alicorn.
//!
//! Term hierarchy (simplified from Lua):
//! - `inferrable`: Pre-typechecking terms (from operatives)
//! - `typed`: Fully elaborated terms (evaluator input)
//! - `value`: Runtime values with flex/strict distinction
//!
//! See CLAUDE.md for architecture details.

pub mod eval;
pub mod infer;
pub mod inferrable;
pub mod typed;
pub mod value;

// Re-exports for convenience
pub use eval::{Env, evaluate};
pub use infer::{InferError, TypingContext, as_operative_type, infer, is_pi_type, type_of_value};
pub use inferrable::{Inferrable, InferrableKind, Span};
pub use typed::Term; // TODO: rename to Typed
pub use value::{FlexValue, NativeOperative, SpannedName, StrictValue, StuckValue};
