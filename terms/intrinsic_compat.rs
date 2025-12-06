// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Compatibility mapping from Lua intrinsic strings to Rust intrinsics.
//!
//! This allows running the original Lua prelude.alc without modification.
//! The operative tries the clean name first, then falls back to this mapping.

use crate::inferrable::Intrinsic;

/// Try to map a Lua intrinsic string to an Intrinsic.
///
/// These are the exact strings from the original Lua prelude.alc.
pub fn from_lua_string(s: &str) -> Option<Intrinsic> {
	let s = s.trim();

	match s {
		// Host type constants (prelude lines 43-50)
		"return terms.strict_value.host_bool_type" => Some(Intrinsic::HostBoolType),
		"return terms.strict_value.host_string_type" => Some(Intrinsic::HostStringType),
		"return terms.host_syntax_type" => Some(Intrinsic::HostSyntaxType),
		"return terms.host_environment_type" => Some(Intrinsic::HostEnvironmentType),
		"return terms.host_goal_type" => Some(Intrinsic::HostGoalType),
		"return terms.host_inferrable_term_type" => Some(Intrinsic::HostInferrableTermType),
		"return terms.host_checkable_term_type" => Some(Intrinsic::HostCheckableTermType),
		"return terms.host_lua_error_type" => Some(Intrinsic::HostErrorType),

		// TODO: Add more as we encounter them in the prelude
		// "return terms.strict_value.host_number_type" => Some(Intrinsic::HostNumberType),
		// "return function(a, b) return a .. b end" => Some(Intrinsic::StringConcat),
		// etc.
		_ => None,
	}
}
