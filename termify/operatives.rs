// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Base operatives - syntax transformers for core language forms.
//!
//! Each operative receives:
//! - Raw syntax (FormatList) - the arguments to the operative
//! - Environment (mutable) - can add bindings
//! - Goal - infer or check mode
//!
//! And returns an Inferrable term.

use alicorn_format::{Element, FormatList};
use alicorn_terms::{Elaborated, FlexValue, Inferrable, Visibility};
use format_macro::format_matcher;

use crate::{Env, ExprError, Goal, Result, expression};

/// Helper to extract a symbol from an Element
fn expect_symbol(elem: &Element) -> Result<&str> {
	match elem {
		Element::Symbol(s) => Ok(s.as_str()),
		Element::List(inner) => {
			// Give a helpful hint if it looks like a typed binding
			let preview: String = inner
				.iter()
				.take(5)
				.map(|e| match e {
					Element::Symbol(s) => s.to_string(),
					Element::Number(n) => n.to_string(),
					Element::List(_) => "(..)".to_string(),
					_ => "..".to_string(),
				})
				.collect::<Vec<_>>()
				.join(" ");
			Err(ExprError::InvalidSyntax(format!(
				"expected a name, got a list: ({}) - maybe wrong operative? \
				 (lambda takes untyped param, lambda_single takes (name : type))",
				preview
			)))
		}
		_ => Err(ExprError::InvalidSyntax(format!("expected a symbol/name, got {:?}", elem))),
	}
}

/// Helper to extract a number literal from an Element
fn expect_number(elem: &Element) -> Result<f64> {
	match elem {
		Element::Number(n) => Ok(*n),
		_ => Err(ExprError::InvalidSyntax(format!("Expected number literal, got {:?}", elem))),
	}
}

/// Helper to convert a single Element to a FormatList for recursive processing
fn elem_to_list(elem: &Element) -> FormatList {
	match elem {
		Element::List(l) => l.clone(),
		_ => {
			let mut list = FormatList::new();
			list.push_back(elem.clone());
			list
		}
	}
}

/// Result of parsing an ascribed segment like `(x : T)` or `(x : A, y : B)`
#[derive(Debug)]
pub struct AscribedSegment {
	/// The parameter names (single for `(x : T)`, multiple for `(x : A, y : B)`)
	pub names: Vec<String>,
	/// The type term - for single param, it's the param type
	/// For multi-param, it's a tuple type
	pub type_term: Inferrable,
	/// Whether this was a single-param segment (affects Pi vs Tuple representation)
	pub single: bool,
}

/// Parse an ascribed segment: `(name : type)` or `(x : A, y : B, ...)`
///
/// This handles telescoping - each name is bound before parsing subsequent types,
/// so in `(A : Type, x : A)`, the type `A` in `x : A` can reference the first param.
///
/// For single param: `(name : type)` - just binds name and returns type
/// For multi-param: `(x : T1, y : T2)` - binds each name, builds tuple type
///
/// Returns the segment info and the updated environment with all names bound.
pub fn parse_ascribed_segment(syntax: &FormatList, env: &mut Env) -> Result<AscribedSegment> {
	// Check if syntax looks like (name : type) - single param
	// vs (name1 : type1, name2 : type2, ...) - multi-param (has comma-separated segments)

	// The parser wraps comma-separated elements, so:
	// - `(x : T)` parses as [x, :, T]
	// - `(x : A, y : B)` parses as [[x, :, A], [y, :, B]] (each segment wrapped)

	if syntax.is_empty() {
		// Empty segment: () -> return empty/unit type
		return Ok(AscribedSegment {
			names: vec![],
			type_term: Inferrable::tuple(vec![]),
			single: false,
		});
	}

	// Check if first element is a list (multi-param) or symbol (single-param)
	let first = &syntax[0];

	match first {
		Element::List(_) => {
			// Multi-param: each element is a segment like [name, :, type]
			parse_multi_param_segment(syntax, env)
		}
		Element::Symbol(_) => {
			// Single-param: [name, :, type...]
			parse_single_param_segment(syntax, env)
		}
		_ => Err(ExprError::InvalidSyntax(format!(
			"ascribed segment should start with name or (name : type), got {:?}",
			first
		))),
	}
}

/// Parse a single-param segment: `(name : type)`
fn parse_single_param_segment(syntax: &FormatList, env: &mut Env) -> Result<AscribedSegment> {
	// Expect: name : type...
	if syntax.len() < 3 {
		return Err(ExprError::InvalidSyntax("single param segment should be (name : type)".to_string()));
	}

	let name = expect_symbol(&syntax[0])?;
	// syntax[1] should be ":"
	let type_syntax = syntax.clone().slice(2..);
	let type_term = expression(&type_syntax, env, Goal::Infer)?;

	// Bind the name in env for subsequent use
	env.bind(name.to_string(), Inferrable::bound_variable(env.depth, name));
	env.depth += 1;

	Ok(AscribedSegment {
		names: vec![name.to_string()],
		type_term,
		single: true,
	})
}

/// Parse a multi-param segment: `(x : A, y : B, ...)`
fn parse_multi_param_segment(syntax: &FormatList, env: &mut Env) -> Result<AscribedSegment> {
	let mut names = Vec::new();
	let mut type_terms = Vec::new();

	// Each element should be a list like [name, :, type]
	for elem in syntax.iter() {
		let segment = match elem {
			Element::List(inner) => inner,
			_ => {
				return Err(ExprError::InvalidSyntax(format!(
					"multi-param segment element should be (name : type), got {:?}",
					elem
				)));
			}
		};

		if segment.len() < 3 {
			return Err(ExprError::InvalidSyntax("each param should be (name : type)".to_string()));
		}

		let name = expect_symbol(&segment[0])?;
		// segment[1] should be ":"
		let type_syntax = segment.clone().slice(2..);

		// Parse type with current env (can reference previously bound names!)
		let type_term = expression(&type_syntax, env, Goal::Infer)?;

		// Bind this name for subsequent params
		env.bind(name.to_string(), Inferrable::bound_variable(env.depth, name));
		env.depth += 1;

		names.push(name.to_string());
		type_terms.push(type_term);
	}

	// Build a tuple type from all the element types
	// For proper dependent tuples, we'd need TupleDescCons/Empty
	// For now, use simple tuple type
	let tuple_type = Inferrable::tuple(type_terms);

	Ok(AscribedSegment {
		names,
		type_term: tuple_type,
		single: false,
	})
}

/// Parse a possibly double-parens wrapped ascribed segment.
///
/// Handles:
/// - `((x : T))` - double parens (implicit param) -> inner segment
/// - `(x : T)` - single parens (explicit param)
/// - `(x : A, y : B)` - multi-param
/// - `()` - empty params
///
/// Returns the segment and whether it was double-parens (implicit).
pub fn parse_maybe_implicit_segment(elem: &Element, env: &mut Env) -> Result<(AscribedSegment, bool)> {
	let outer = match elem {
		Element::List(l) => l,
		_ => {
			return Err(ExprError::InvalidSyntax(
				"param segment should be (name : type) or ((name : type))".to_string(),
			));
		}
	};

	// Check for double parens: ((x : T)) - single element that's a list
	if outer.len() == 1 {
		if let Element::List(inner) = &outer[0] {
			// Double parens - implicit param
			let segment = parse_ascribed_segment(inner, env)?;
			return Ok((segment, true));
		}
	}

	// Single parens - explicit param(s)
	let segment = parse_ascribed_segment(outer, env)?;
	Ok((segment, false))
}

/// let operative: `let name = expr`
///
/// Binds `name` to `expr` in the environment and returns unit.
/// The binding persists for subsequent expressions.
///
/// Note: This is a "flat" let that modifies env, not `let x = e in body`.
/// For let-in, the syntax would be `let name = expr in body` but that's
/// typically handled at a higher level (block processing).
pub fn let_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			// Binding with body: name = expr in body...
			// This desugars to let-expression
			// NOTE: Must come before simple binding pattern!
			(~name~, =, ~expr~, in, ~body...~) => {
				let name_str = expect_symbol(name)?;
				let expr_list = elem_to_list(expr);
				let expr_term = expression(&expr_list, env, Goal::Infer)?;

				// Create a new environment with the binding for the body
				let mut body_env = env.clone();
				body_env.bind(name_str.to_string(), expr_term.clone());

				// Process body
				let body_term = expression(&body, &mut body_env, Goal::Infer)?;

				// Create let term
				return Ok(Inferrable::let_bind(name_str, expr_term, body_term));
			},

			// Simple binding: name = expr...
			// Captures entire tail after = (like Lua's listtail)
			(~name~, =, ~expr...~) => {
				let name_str = expect_symbol(name)?;
				let expr_term = expression(&expr, env, Goal::Infer)?;

				// Bind in environment
				env.bind(name_str.to_string(), expr_term);

				// Return unit
				return Ok(Inferrable::unit());
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("let expects: name = expr, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// Arrow operative: `param -> body` or `(param : type) -> body`
///
/// Creates a lambda abstraction.
pub fn arrow_operative(syntax: &FormatList, _env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// The arrow was already consumed, so syntax is: param, body...
	// But wait - the arrow is infix, so we get called differently.
	// Actually, looking at how Lua does it, the arrow is bound as an operator
	// and gets special handling in the expression parser.
	//
	// For now, let's handle: body (where param was before the arrow)
	// This needs rethinking for proper infix handling.

	format_matcher! {
		match syntax {
			// Simple lambda: body (param was before arrow, needs special handling)
			(~body...~) => {
				// This is a placeholder - proper infix handling needed
				return Err(ExprError::InvalidSyntax(
					"Arrow operator needs special infix handling".to_string()
				));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("Malformed arrow expression")
				));
			}
		}
	}
}

/// Lambda operative: `lambda param body` or `lambda (param : type) body`
///
/// Smart lambda that detects whether param is typed or untyped:
/// - `lambda x body` → untyped param (type will be inferred)
/// - `lambda (x : T) body` → typed param (like lambda_single)
pub fn lambda_operative(syntax: &FormatList, env: &mut Env, goal: Goal) -> Result<Inferrable> {
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax("lambda expects: param body".to_string()));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	match first {
		// Typed param: (x : T) → use ascribed segment parser
		Element::List(inner) => {
			// Check if it looks like a typed binding (has : somewhere)
			let has_colon = inner.iter().any(|e| matches!(e, Element::Symbol(s) if s == ":"));

			if has_colon {
				// Typed param - use ascribed segment parser
				let mut body_env = env.clone();
				let segment = parse_ascribed_segment(inner, &mut body_env)?;

				if !segment.single || segment.names.len() != 1 {
					return Err(ExprError::InvalidSyntax("lambda typed param should be (name : type)".to_string()));
				}

				let param_name = &segment.names[0];
				let body_term = expression(&rest, &mut body_env, goal)?;

				return Ok(Inferrable::lambda(param_name, Some(Box::new(segment.type_term)), body_term));
			} else {
				// List but no colon - could be multi-param tuple syntax (not yet supported)
				return Err(ExprError::InvalidSyntax(
					"lambda with list param but no ':' - multi-param tuple syntax not yet supported".to_string(),
				));
			}
		}

		// Untyped param: just a symbol
		Element::Symbol(param_name) => {
			let param_name = param_name.as_str();

			let mut body_env = env.clone();
			body_env.bind(param_name.to_string(), Inferrable::bound_variable(body_env.depth, param_name));
			body_env.depth += 1;

			let body_term = expression(&rest, &mut body_env, goal)?;

			return Ok(Inferrable::lambda(param_name, None, body_term));
		}

		_ => {
			return Err(ExprError::InvalidSyntax(format!(
				"lambda param must be a name or (name : type), got {:?}",
				first
			)));
		}
	}
}

/// Forall operative: `forall (params) -> result` or `forall ((param : type)) -> result`
///
/// Creates a Pi type (dependent function type).
///
/// Syntax variants:
/// - `forall ((x : T)) -> R` - single implicit param (double parens)
/// - `forall (x : T) -> R` - single explicit param
/// - `forall (x : T, y : U) -> R` - multiple params
/// - `forall () -> R` - no params
pub fn forall_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: (params) -> result or ((param : type)) -> result
	if syntax.len() < 3 {
		return Err(ExprError::InvalidSyntax("forall expects: (param : type) -> result".to_string()));
	}

	let first = &syntax[0];

	// Find the arrow position
	let arrow_pos = syntax.iter().position(|e| matches!(e, Element::Symbol(s) if s == "->"));
	let arrow_pos = match arrow_pos {
		Some(pos) => pos,
		None => {
			return Err(ExprError::InvalidSyntax("forall expects '->' before result type".to_string()));
		}
	};

	// Result type is everything after the arrow
	let result_syntax = syntax.clone().slice(arrow_pos + 1..);

	// Parse the param(s) using ascribed segment parser
	// This binds the param names in a cloned env for the result type
	let mut result_env = env.clone();
	let (param_segment, _implicit) = parse_maybe_implicit_segment(first, &mut result_env)?;

	// Parse the result type (also an ascribed segment for named results like `(rel : U)`)
	// Result syntax is [Element::List([rel, :, target])], so we need to unwrap the outer element
	let result_segment = if result_syntax.len() == 1 {
		if let Element::List(inner) = &result_syntax[0] {
			parse_ascribed_segment(inner, &mut result_env)?
		} else {
			// Bare symbol result type - not an ascribed segment
			// Just evaluate it as an expression
			let type_term = expression(&result_syntax, &mut result_env, Goal::Infer)?;
			AscribedSegment {
				names: vec![],
				type_term,
				single: true,
			}
		}
	} else {
		// Multiple elements after arrow - evaluate as expression
		let type_term = expression(&result_syntax, &mut result_env, Goal::Infer)?;
		AscribedSegment {
			names: vec![],
			type_term,
			single: true,
		}
	};

	// Build the Pi type
	// For single param: Pi { param_name, param_type, result_type }
	// For multi-param: need to build nested Pis or tuple-based Pi
	if param_segment.names.is_empty() {
		// forall () -> R - no params, just the result type
		return Ok(result_segment.type_term);
	}

	if param_segment.single {
		// Single param - simple Pi
		let param_name = &param_segment.names[0];
		Ok(Inferrable::pi(param_name, param_segment.type_term, result_segment.type_term))
	} else {
		// Multi-param - build nested Pis from right to left
		// forall (x : A, y : B) -> R  becomes  Pi x:A. Pi y:B. R
		// We need to iterate in reverse and build up the result type

		// But wait - parse_multi_param_segment returns a tuple type, not individual types
		// We need the individual types. Let me fix this by storing them separately.

		// For now, build a single Pi with tuple param type
		// This is a simplification - proper telescopes need nested Pis
		let combined_name = param_segment.names.join("_");
		Ok(Inferrable::pi(&combined_name, param_segment.type_term, result_segment.type_term))
	}
}

/// lambda_single operative: `lambda_single (param : type) body`
///
/// Single-param lambda with explicit type annotation and explicit visibility.
pub fn lambda_single_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: (param : type) body...
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax("lambda_single expects: (param : type) body".to_string()));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	// Parse (param : type) using ascribed segment parser
	let inner = match first {
		Element::List(l) => l,
		_ => return Err(ExprError::InvalidSyntax("lambda_single expects (param : type)".to_string())),
	};

	// parse_ascribed_segment binds the param in env
	let mut body_env = env.clone();
	let segment = parse_ascribed_segment(inner, &mut body_env)?;

	if !segment.single || segment.names.len() != 1 {
		return Err(ExprError::InvalidSyntax(
			"lambda_single expects exactly one param: (param : type)".to_string(),
		));
	}

	let param_name = &segment.names[0];
	let body_term = expression(&rest, &mut body_env, Goal::Infer)?;

	Ok(Inferrable::lambda(param_name, Some(Box::new(segment.type_term)), body_term))
}

/// Annotate operative: `expr : type`
pub fn annotate_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			// expr : type (but : already consumed as head)
			(~expr~, ~type_expr~) => {
				let expr_list = elem_to_list(expr);
				let type_list = elem_to_list(type_expr);

				let term = expression(&expr_list, env, Goal::Infer)?;
				let type_term = expression(&type_list, env, Goal::Infer)?;

				return Ok(Inferrable::annotated(term, type_term));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("annotation expects: expr type")
				));
			}
		}
	}
}

/// type_ operative: `type_(level, depth)` - universe constructor
///
/// Constructs a universe type `star(level, depth)`.
/// The type of `type_(L, D)` is `star(L+1, D+1)`.
///
/// NOTE: This is Lua bootstrap jank - only accepts literal number arguments.
/// A proper implementation would be a function that accepts any expression
/// evaluating to a number. But this sidesteps needing full expression evaluation
/// and type checking to work first.
#[allow(non_snake_case)]
pub fn type__operative(syntax: &FormatList, _env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			(~level_elem~, ~depth_elem~) => {
				let level_f64 = expect_number(level_elem)?;
				let depth_f64 = expect_number(depth_elem)?;

				// Validate they're non-negative integers
				if level_f64 < 0.0 || level_f64.fract() != 0.0 {
					return Err(ExprError::InvalidSyntax(
						format!("type_ level must be a non-negative integer, got {}", level_f64)
					));
				}
				if depth_f64 < 0.0 || depth_f64.fract() != 0.0 {
					return Err(ExprError::InvalidSyntax(
						format!("type_ depth must be a non-negative integer, got {}", depth_f64)
					));
				}

				let level = level_f64 as u8;
				let depth = depth_f64 as u8;

				// The VALUE is star(level, depth)
				let star_value = Elaborated::Literal(FlexValue::Star { level, depth });

				// The TYPE is star(level+1, depth+1)
				let star_type = Elaborated::Literal(FlexValue::Star {
					level: level + 1,
					depth: depth + 1,
				});

				return Ok(Inferrable::typed(star_type, star_value));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("type_ expects: level depth (two number literals), got {} elements", syntax.len())
				));
			}
		}
	}
}

/// lambda_curry operative: `lambda_curry ((param : type)) body`
///
/// Creates a lambda with an implicit type parameter. The double parens indicate
/// an explicit type parameter that can be inferred at call sites.
///
/// Syntax: `lambda_curry ((T : Type)) body`
/// - First element is `((T : Type))` - a list containing a list with param binding
/// - Rest is the body expression
///
/// The resulting lambda has Visibility::Implicit, meaning the type argument
/// can be inferred rather than explicitly passed.
pub fn lambda_curry_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: ((param : type)) body...
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax("lambda_curry expects: ((param : type)) body".to_string()));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	// Parse ((param : type)) using maybe_implicit which handles double parens
	let mut body_env = env.clone();
	let (segment, is_implicit) = parse_maybe_implicit_segment(first, &mut body_env)?;

	if !is_implicit {
		return Err(ExprError::InvalidSyntax(
			"lambda_curry expects double parens ((param : type)), got single parens".to_string(),
		));
	}

	if segment.names.is_empty() {
		return Err(ExprError::InvalidSyntax("lambda_curry expects at least one param".to_string()));
	}

	let body_term = expression(&rest, &mut body_env, Goal::Infer)?;

	// For single param, create simple implicit lambda
	// For multi-param, would need to build nested lambdas (not yet needed)
	if segment.single && segment.names.len() == 1 {
		let param_name = &segment.names[0];
		Ok(Inferrable::lambda_with_visibility(
			param_name,
			Some(Box::new(segment.type_term)),
			Visibility::Implicit,
			body_term,
		))
	} else {
		// Multi-param implicit lambda - build nested lambdas
		// For now, combine names as single tuple param
		let combined_name = segment.names.join("_");
		Ok(Inferrable::lambda_with_visibility(
			&combined_name,
			Some(Box::new(segment.type_term)),
			Visibility::Implicit,
			body_term,
		))
	}
}

/// lambda_implicit operative: `lambda_implicit (param : type) body`
///
/// Creates a lambda with an implicit parameter. Unlike lambda_curry which uses
/// double parens `((param : type))`, lambda_implicit uses single parens.
///
/// Syntax: `lambda_implicit (T : Type) body`
/// - First element is `(T : Type)` - a list with param binding
/// - Rest is the body expression
///
/// The resulting lambda has Visibility::Implicit, meaning the type argument
/// can be inferred rather than explicitly passed.
pub fn lambda_implicit_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	// Expect: (param : type) body...
	if syntax.is_empty() {
		return Err(ExprError::InvalidSyntax("lambda_implicit expects: (param : type) body".to_string()));
	}

	let first = &syntax[0];
	let rest = syntax.clone().slice(1..);

	// Parse (param : type) using ascribed segment parser
	let inner = match first {
		Element::List(l) => l,
		_ => {
			return Err(ExprError::InvalidSyntax(
				"lambda_implicit expects (param : type) as first argument".to_string(),
			));
		}
	};

	// parse_ascribed_segment binds the param in env
	let mut body_env = env.clone();
	let segment = parse_ascribed_segment(inner, &mut body_env)?;

	if !segment.single || segment.names.len() != 1 {
		return Err(ExprError::InvalidSyntax(
			"lambda_implicit expects exactly one param: (param : type)".to_string(),
		));
	}

	let param_name = &segment.names[0];
	let body_term = expression(&rest, &mut body_env, Goal::Infer)?;

	// Create lambda with IMPLICIT visibility
	Ok(Inferrable::lambda_with_visibility(
		param_name,
		Some(Box::new(segment.type_term)),
		Visibility::Implicit,
		body_term,
	))
}

/// wrap operative: `wrap T x`
///
/// Wraps a value x of type T, producing a value of type wrapped(T).
/// This is used for the modal type system - creating phase-distinguished values.
pub fn wrap_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			(~type_elem~, ~content_elem~) => {
				let type_list = elem_to_list(type_elem);
				let content_list = elem_to_list(content_elem);

				let type_term = expression(&type_list, env, Goal::Infer)?;
				let content_term = expression(&content_list, env, Goal::Infer)?;

				return Ok(Inferrable::host_wrap(type_term, content_term));
			},

			// Also handle single argument case: wrap(T, x) as a list
			(~args~) => {
				let args_list = elem_to_list(args);
				if args_list.len() >= 2 {
					// Recursively call with the two args
					return wrap_operative(&args_list, env, _goal);
				}
				return Err(ExprError::InvalidSyntax(
					"wrap expects two arguments: type and value".to_string()
				));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("wrap expects: T x, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// unwrap operative: `unwrap T x`
///
/// Unwraps a value of type wrapped(T), producing a value of type T.
pub fn unwrap_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			(~type_elem~, ~container_elem~) => {
				let type_list = elem_to_list(type_elem);
				let container_list = elem_to_list(container_elem);

				let type_term = expression(&type_list, env, Goal::Infer)?;
				let container_term = expression(&container_list, env, Goal::Infer)?;

				return Ok(Inferrable::host_unwrap(type_term, container_term));
			},

			// Handle single argument case: unwrap(T, x) as a list
			(~args~) => {
				let args_list = elem_to_list(args);
				if args_list.len() >= 2 {
					return unwrap_operative(&args_list, env, _goal);
				}
				return Err(ExprError::InvalidSyntax(
					"unwrap expects two arguments: type and wrapped value".to_string()
				));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("unwrap expects: T x, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// wrapped operative: `wrapped(T)` or `wrapped T`
///
/// Returns the type of wrapped values of type T.
pub fn wrapped_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	format_matcher! {
		match syntax {
			(~type_elem~) => {
				let type_list = elem_to_list(type_elem);
				let type_term = expression(&type_list, env, Goal::Infer)?;

				return Ok(Inferrable::wrapped_type(type_term));
			},

			_ => {
				return Err(ExprError::InvalidSyntax(
					format!("wrapped expects: T, got {} elements", syntax.len())
				));
			}
		}
	}
}

/// intrinsic operative: `intrinsic "name" : type`
///
/// Host escape hatch - looks up a named native intrinsic and returns it
/// with the specified type annotation.
///
/// Example: `intrinsic "host-bool-type" : wrapped(host-type)`
pub fn intrinsic_operative(syntax: &FormatList, env: &mut Env, _goal: Goal) -> Result<Inferrable> {
	use alicorn_terms::Intrinsic;

	// Expect: "name" : type...
	// Or: "name" ":" type... (if : is a separate symbol)
	if syntax.len() < 3 {
		return Err(ExprError::InvalidSyntax("intrinsic expects: \"name\" : type".to_string()));
	}

	// First element should be the intrinsic name (string literal)
	let name = match &syntax[0] {
		Element::String(s) => s.clone(),
		other => {
			return Err(ExprError::InvalidSyntax(format!(
				"intrinsic name must be a string literal, got {:?}",
				other
			)));
		}
	};

	// Second element should be ":"
	match &syntax[1] {
		Element::Symbol(s) if s == ":" => {}
		other => {
			return Err(ExprError::InvalidSyntax(format!(
				"intrinsic expects ':' after name, got {:?}",
				other
			)));
		}
	}

	// Rest is the type expression
	let type_syntax = syntax.clone().slice(2..);
	let type_term = expression(&type_syntax, env, Goal::Infer)?;

	// Look up the intrinsic by name, with fallback to Lua compat strings
	let intrinsic = Intrinsic::from_name(&name)
		.or_else(|| alicorn_terms::intrinsic_compat::from_lua_string(&name))
		.ok_or_else(|| ExprError::InvalidSyntax(format!("unknown intrinsic: \"{}\"", name)))?;

	Ok(Inferrable::host_intrinsic(intrinsic, type_term))
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{Env, parse_and_run_with_env};

	#[test]
	fn let_operative_direct() {
		// Test the operative directly with the args it would receive.
		// When expression() dispatches to let_operative, it passes the REST
		// after consuming "let". So for "let x = 5", operative gets [x, =, 5].
		//
		// Parser returns a block, so we parse "x = 5" → [[x, =, 5]]
		// and extract the inner list.
		let syntax = alicorn_format::format("x = 5").unwrap();

		// Extract the inner list (first element of block)
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list in block, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = let_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "let_operative failed: {:?}", result);
		assert!(env.get("x").is_some(), "x should be bound after let");
	}

	#[test]
	fn let_through_block() {
		// Test through the full pipeline: parse → block → expression → operative
		let (result, env) = parse_and_run_with_env("let x = 5").unwrap();

		// let returns unit (empty tuple)
		assert!(
			matches!(&result.kind, alicorn_terms::InferrableKind::TupleCons { elements } if elements.is_empty()),
			"let should return unit, got {:?}",
			result
		);

		// x should be bound
		assert!(env.get("x").is_some(), "x should be bound after let");
	}

	#[test]
	fn let_in_expression() {
		// Test let with body: let x = 5 in x
		let syntax = alicorn_format::format("x = 5 in x").unwrap();
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = let_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "let-in failed: {:?}", result);
		// Result should be a let-binding term, not unit
		assert!(
			matches!(&result.as_ref().unwrap().kind, alicorn_terms::InferrableKind::Let { .. }),
			"let-in should produce Let term, got {:?}",
			result
		);
	}

	#[test]
	fn lambda_simple() {
		// lambda x body
		let syntax = alicorn_format::format("x x").unwrap();
		let args = match &syntax[0] {
			Element::List(l) => l.clone(),
			other => panic!("Expected list, got {:?}", other),
		};

		let mut env = Env::with_base_operatives();
		let result = lambda_operative(&args, &mut env, Goal::Infer);

		assert!(result.is_ok(), "lambda failed: {:?}", result);
		assert!(
			matches!(&result.as_ref().unwrap().kind, alicorn_terms::InferrableKind::Lambda { .. }),
			"should produce Lambda, got {:?}",
			result
		);
	}
}
