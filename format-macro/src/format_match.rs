use proc_macro2::{Ident, Span, TokenTree};
use quote::{quote, quote_spanned};

#[derive(Debug)]
struct ParseError {
	span: Span,
	message: String,
}

impl ParseError {
	fn new(span: Span, message: impl Into<String>) -> Self {
		Self {
			span,
			message: message.into(),
		}
	}

	fn to_compile_error(&self) -> proc_macro2::TokenStream {
		let msg = &self.message;
		quote_spanned! { self.span =>
			compile_error!(#msg)
		}
	}
}

#[derive(Debug, Clone)]
enum Pattern {
	Symbol(String),
	Number(f64),
	String(String),
	Placeholder(String, bool),
	List(Vec<Pattern>),
}

impl Pattern {
	/// Check if two patterns are structurally equivalent (ignoring placeholder names).
	/// Used to detect duplicate/unreachable patterns.
	fn is_equivalent_to(&self, other: &Pattern) -> bool {
		match (self, other) {
			(Pattern::Symbol(a), Pattern::Symbol(b)) => a == b,
			(Pattern::Number(a), Pattern::Number(b)) => a == b,
			(Pattern::String(a), Pattern::String(b)) => a == b,
			(Pattern::Placeholder(_, rep_a), Pattern::Placeholder(_, rep_b)) => rep_a == rep_b,
			(Pattern::List(a), Pattern::List(b)) => a.len() == b.len() && a.iter().zip(b.iter()).all(|(p1, p2)| p1.is_equivalent_to(p2)),
			_ => false,
		}
	}
}

struct MatchArm {
	pattern: Pattern,
	pattern_span: Span,
	body: proc_macro2::TokenStream,
}

fn parse_pattern(tokens: &mut std::iter::Peekable<proc_macro2::token_stream::IntoIter>) -> Result<Pattern, String> {
	if let Some(token) = tokens.next() {
		match token {
			TokenTree::Group(group) => {
				if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
					let mut patterns = Vec::new();
					let mut inner = group.stream().into_iter().peekable();

					while inner.peek().is_some() {
						let pat = parse_pattern(&mut inner)?;
						patterns.push(pat);

						if let Some(TokenTree::Punct(p)) = inner.peek() {
							if p.as_char() == ',' {
								inner.next();
							}
						}
					}

					return Ok(Pattern::List(patterns));
				}
			}
			TokenTree::Punct(p) if p.as_char() == '~' => {
				let name = if let Some(TokenTree::Ident(ident)) = tokens.next() {
					ident.to_string()
				} else {
					return Err("Expected identifier after ~".to_string());
				};

				let mut is_repetition = false;
				if let Some(TokenTree::Punct(p)) = tokens.peek() {
					if p.as_char() == '.' {
						let saved_pos = tokens.clone();
						if let Some(TokenTree::Punct(p1)) = tokens.next() {
							if p1.as_char() == '.' {
								if let Some(TokenTree::Punct(p2)) = tokens.next() {
									if p2.as_char() == '.' {
										if let Some(TokenTree::Punct(p3)) = tokens.next() {
											if p3.as_char() == '.' {
												is_repetition = true;
											} else {
												*tokens = saved_pos;
											}
										} else {
											*tokens = saved_pos;
										}
									} else {
										*tokens = saved_pos;
									}
								} else {
									*tokens = saved_pos;
								}
							} else {
								*tokens = saved_pos;
							}
						}
					}
				}

				if let Some(TokenTree::Punct(p)) = tokens.next() {
					if p.as_char() != '~' {
						return Err(format!("Expected closing ~ after placeholder name, got {p}"));
					}
				} else {
					return Err("Expected closing ~ after placeholder name".to_string());
				}

				return Ok(Pattern::Placeholder(name, is_repetition));
			}
			TokenTree::Ident(ident) => {
				let s = ident.to_string();

				// foo(...) syntax is parsed as a list with foo as first element
				if let Some(TokenTree::Group(group)) = tokens.peek() {
					if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
						let Some(TokenTree::Group(group)) = tokens.next() else {
							unreachable!()
						};

						let mut patterns = vec![Pattern::Symbol(s)];
						let mut inner = group.stream().into_iter().peekable();

						while inner.peek().is_some() {
							let pat = parse_pattern(&mut inner)?;
							patterns.push(pat);

							if let Some(TokenTree::Punct(p)) = inner.peek() {
								if p.as_char() == ',' {
									inner.next();
								}
							}
						}

						return Ok(Pattern::List(patterns));
					}
				}

				return Ok(Pattern::Symbol(s));
			}
			TokenTree::Literal(lit) => {
				let s = lit.to_string();

				if let Ok(n) = s.parse::<f64>() {
					return Ok(Pattern::Number(n));
				}

				let s = s.trim_matches('"').to_string();
				return Ok(Pattern::String(s));
			}
			TokenTree::Punct(p) => {
				let ch = p.as_char();
				if ch == ',' || ch == ';' {
					return Ok(Pattern::Symbol(ch.to_string()));
				}

				// Collect joint punctuation into multi-char operators
				let mut op = String::new();
				op.push(ch);

				if p.spacing() == proc_macro2::Spacing::Joint {
					while let Some(TokenTree::Punct(next_p)) = tokens.peek() {
						let next_ch = next_p.as_char();
						if next_ch == ',' || next_ch == ';' {
							break;
						}
						op.push(next_ch);
						let spacing = next_p.spacing();
						tokens.next();
						if spacing == proc_macro2::Spacing::Alone {
							break;
						}
					}
				}

				return Ok(Pattern::Symbol(op));
			}
		}
	}

	Err("Expected pattern".to_string())
}

fn generate_match_code(
	scrutinee: &Ident,
	pattern: &Pattern,
	bindings: &mut Vec<(String, proc_macro2::TokenStream)>,
	conditions: &mut Vec<proc_macro2::TokenStream>,
	idx: &mut usize,
) {
	match pattern {
		Pattern::Symbol(s) => {
			let i = *idx;
			conditions.push(quote! {
				#scrutinee.get(#i).and_then(|e| if let ::alicorn_format::Element::Symbol(sym) = e {
					if sym.as_str() == #s { Some(()) } else { None }
				} else { None }).is_some()
			});
			*idx += 1;
		}
		Pattern::Number(n) => {
			let i = *idx;
			conditions.push(quote! {
				#scrutinee.get(#i).and_then(|e| if let ::alicorn_format::Element::Number(num) = e {
					if *num == #n { Some(()) } else { None }
				} else { None }).is_some()
			});
			*idx += 1;
		}
		Pattern::String(s) => {
			let i = *idx;
			conditions.push(quote! {
				#scrutinee.get(#i).and_then(|e| if let ::alicorn_format::Element::String(str) = e {
					if str == #s { Some(()) } else { None }
				} else { None }).is_some()
			});
			*idx += 1;
		}
		Pattern::Placeholder(name, is_rep) => {
			let binding_name = Ident::new(name, Span::call_site());

			if *is_rep {
				// Vector::skip() is O(log n) with structural sharing
				let i = *idx;
				bindings.push((name.clone(), quote! { let #binding_name = #scrutinee.skip(#i); }));
			} else {
				let i = *idx;
				bindings.push((name.clone(), quote! { let #binding_name = &#scrutinee[#i]; }));
				*idx += 1;
			}
		}
		Pattern::List(patterns) => {
			let i = *idx;
			let temp_name = Ident::new(&format!("__list_{i}"), Span::call_site());

			conditions.push(quote! {
				#scrutinee.get(#i).and_then(|e| if let ::alicorn_format::Element::List(_) = e {
					Some(())
				} else { None }).is_some()
			});

			// Keep as &Vector for consistent skip/take
			bindings.push((
				format!("__list_{i}"),
				quote! {
					let #temp_name = if let ::alicorn_format::Element::List(inner) = &#scrutinee[#i] {
						inner
					} else {
						unreachable!()
					};
				},
			));

			*idx += 1;

			let mut inner_idx = 0;
			for pat in patterns {
				generate_match_code(&temp_name, pat, bindings, conditions, &mut inner_idx);
			}
		}
	}
}

/// Analyze a pattern list to find repetition information.
/// Returns (rep_position, patterns_after_rep) where:
/// - rep_position: The index of the repetition placeholder, if any
/// - patterns_after_rep: Count of patterns that come after the repetition
fn analyze_repetition(patterns: &[Pattern]) -> (Option<usize>, usize) {
	let rep_position = patterns.iter().position(|p| matches!(p, Pattern::Placeholder(_, true)));

	let mut patterns_after_rep: usize = 0;
	for pat in patterns.iter().rev() {
		if matches!(pat, Pattern::Placeholder(_, true)) {
			break;
		}
		patterns_after_rep += 1;
	}

	(rep_position, patterns_after_rep)
}

/// Minimum elements to match: patterns before rep + patterns after rep.
/// Repetition can match zero elements.
fn calculate_min_len(patterns: &[Pattern]) -> usize {
	let (rep_position, patterns_after_rep) = analyze_repetition(patterns);
	if let Some(pos) = rep_position {
		pos + patterns_after_rep
	} else {
		patterns.len()
	}
}

/// Tilde at top level indicates a pattern, not an expression.
fn contains_tilde(stream: &proc_macro2::TokenStream) -> bool {
	stream
		.clone()
		.into_iter()
		.any(|t| matches!(t, TokenTree::Punct(p) if p.as_char() == '~'))
}

/// Parse one arm body, consuming tokens until comma or pattern marker.
fn parse_arm_body(tokens: &proc_macro2::TokenStream) -> Result<(proc_macro2::TokenStream, proc_macro2::TokenStream), ParseError> {
	let mut iter = tokens.clone().into_iter().peekable();

	if let Some(TokenTree::Group(_)) = iter.peek() {
		let group = iter.next().unwrap();
		let rest: proc_macro2::TokenStream = iter.collect();

		if let Some(first_token) = rest.clone().into_iter().next() {
			match first_token {
				TokenTree::Punct(p) if p.as_char() == ',' => {}
				_ => {
					return Err(ParseError::new(
						group.span(),
						"Missing comma after match arm body. Each match arm must end with a comma (,)",
					));
				}
			}
		}

		return Ok((proc_macro2::TokenStream::from(group), rest));
	}

	let mut body_tokens = Vec::new();

	while let Some(token) = iter.peek() {
		match token {
			TokenTree::Punct(p) if p.as_char() == ',' => break,
			TokenTree::Punct(p) if p.as_char() == '~' => break,
			// Paren with tilde is likely next pattern, not function args
			TokenTree::Group(g) if g.delimiter() == proc_macro2::Delimiter::Parenthesis => {
				if contains_tilde(&g.stream()) {
					break;
				}
				body_tokens.push(iter.next().unwrap());
			}
			_ => {
				body_tokens.push(iter.next().unwrap());
			}
		}
	}

	let body: proc_macro2::TokenStream = body_tokens.clone().into_iter().collect();
	let rest: proc_macro2::TokenStream = iter.collect();

	if body.is_empty() {
		let span = rest.clone().into_iter().next().map_or_else(Span::call_site, |t| t.span());
		return Err(ParseError::new(span, "Expected expression in match arm body"));
	}

	// After body: comma, end of stream, or error
	if let Some(first_token) = rest.clone().into_iter().next() {
		match first_token {
			TokenTree::Punct(p) if p.as_char() == ',' => {}
			_ => {
				let span = body_tokens.last().map_or_else(Span::call_site, TokenTree::span);
				return Err(ParseError::new(
					span,
					"Missing comma after match arm body. Each match arm must end with a comma (,)",
				));
			}
		}
	}

	Ok((body, rest))
}

#[cfg(test)]
mod tests {
	use super::*;
	use quote::quote;

	// Tests for analyze_repetition

	#[test]
	fn test_analyze_repetition_no_repetition() {
		// Pattern: (a, b, c) - no repetition
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, None);
		assert_eq!(after, 3); // All 3 patterns counted (no repetition to stop at)
	}

	#[test]
	fn test_analyze_repetition_at_end() {
		// Pattern: (a, b, ~rest...~) - repetition at end
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Placeholder("rest".to_string(), true),
		];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, Some(2));
		assert_eq!(after, 0); // No patterns after repetition
	}

	#[test]
	fn test_analyze_repetition_in_middle() {
		// Pattern: (a, ~rest...~, b, c) - repetition in middle
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Placeholder("rest".to_string(), true),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, Some(1));
		assert_eq!(after, 2); // 2 patterns after repetition (b, c)
	}

	#[test]
	fn test_analyze_repetition_at_start() {
		// Pattern: (~rest...~, a, b, c) - repetition at start
		let patterns = vec![
			Pattern::Placeholder("rest".to_string(), true),
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, Some(0));
		assert_eq!(after, 3); // 3 patterns after repetition
	}

	#[test]
	fn test_analyze_repetition_single_rep() {
		// Pattern: (~all...~) - just a repetition
		let patterns = vec![Pattern::Placeholder("all".to_string(), true)];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, Some(0));
		assert_eq!(after, 0);
	}

	#[test]
	fn test_analyze_repetition_with_placeholders() {
		// Pattern: (~head~, ~rest...~, ~tail~) - non-rep placeholder, rep, non-rep placeholder
		let patterns = vec![
			Pattern::Placeholder("head".to_string(), false),
			Pattern::Placeholder("rest".to_string(), true),
			Pattern::Placeholder("tail".to_string(), false),
		];
		let (rep_pos, after) = analyze_repetition(&patterns);
		assert_eq!(rep_pos, Some(1));
		assert_eq!(after, 1); // 1 pattern after repetition
	}

	// Tests for calculate_min_len

	#[test]
	fn test_min_len_no_repetition() {
		// Pattern: (a, b, c) - needs exactly 3 elements
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		assert_eq!(calculate_min_len(&patterns), 3);
	}

	#[test]
	fn test_min_len_repetition_at_end() {
		// Pattern: (a, b, ~rest...~) - needs at least 2 elements (a, b)
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Placeholder("rest".to_string(), true),
		];
		assert_eq!(calculate_min_len(&patterns), 2);
	}

	#[test]
	fn test_min_len_repetition_in_middle() {
		// Pattern: (a, ~rest...~, b, c) - needs at least 3 elements (a, b, c)
		let patterns = vec![
			Pattern::Symbol("a".to_string()),
			Pattern::Placeholder("rest".to_string(), true),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		assert_eq!(calculate_min_len(&patterns), 3); // 1 before + 2 after
	}

	#[test]
	fn test_min_len_repetition_at_start() {
		// Pattern: (~rest...~, a, b, c) - needs at least 3 elements (a, b, c)
		let patterns = vec![
			Pattern::Placeholder("rest".to_string(), true),
			Pattern::Symbol("a".to_string()),
			Pattern::Symbol("b".to_string()),
			Pattern::Symbol("c".to_string()),
		];
		assert_eq!(calculate_min_len(&patterns), 3); // 0 before + 3 after
	}

	#[test]
	fn test_min_len_only_repetition() {
		// Pattern: (~all...~) - needs 0 elements (can match empty)
		let patterns = vec![Pattern::Placeholder("all".to_string(), true)];
		assert_eq!(calculate_min_len(&patterns), 0);
	}

	// Tests for parse_arm_body

	#[test]
	fn test_parse_arm_body_function_call() {
		let input = quote! { test(a, b, c), };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "test (a , b , c)");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_group() {
		let input = quote! { { foo + bar }, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "{ foo + bar }");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_missing_comma_before_tilde_stops() {
		// This should error: we stop at ~ but there's no comma before it
		let input = quote! { x + 1 ~pattern~ };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}

	#[test]
	fn test_parse_arm_body_no_comma() {
		let input = quote! { x + 1 };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "x + 1");
		assert_eq!(rest.to_string(), "");
	}

	#[test]
	fn test_parse_arm_body_simple_identifier() {
		let input = quote! { None, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "None");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_literal_number() {
		let input = quote! { 42, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "42");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_literal_bool() {
		let input = quote! { true, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "true");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_literal_string() {
		let input = quote! { "hello", };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "\"hello\"");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_binary_operation() {
		let input = quote! { x + y, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "x + y");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_complex_binary() {
		let input = quote! { a * b + c, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "a * b + c");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_field_access() {
		let input = quote! { x.field, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "x . field");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_method_call() {
		let input = quote! { obj.method(), };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "obj . method ()");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_method_chain() {
		let input = quote! { x.foo().bar().baz(), };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "x . foo () . bar () . baz ()");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_if_expression() {
		let input = quote! { if c { a } else { b }, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "if c { a } else { b }");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_match_expression() {
		let input = quote! { match x { _ => 1 }, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "match x { _ => 1 }");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_tuple_literal() {
		let input = quote! { (1, 2), };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "(1 , 2)");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_array_literal() {
		let input = quote! { [1, 2, 3], };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "[1 , 2 , 3]");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_struct_literal() {
		let input = quote! { Foo { x: 1 }, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "Foo { x : 1 }");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_block_with_statements() {
		let input = quote! { { let x = 1; x + 1 }, };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "{ let x = 1 ; x + 1 }");
		assert_eq!(rest.to_string(), ",");
	}

	// Tests for strict comma requirement enforcement

	#[test]
	fn test_parse_arm_body_missing_comma_before_tilde() {
		let input = quote! { None ~x~ };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}

	#[test]
	fn test_parse_arm_body_missing_comma_after_block() {
		let input = quote! { { x } (bar) };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}

	#[test]
	fn test_parse_arm_body_with_comma_before_tilde() {
		let input = quote! { None, ~x~ };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "None");
		assert_eq!(rest.to_string(), ", ~ x ~");
	}

	#[test]
	fn test_parse_arm_body_end_of_input_valid() {
		let input = quote! { Some(x) };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "Some (x)");
		assert_eq!(rest.to_string(), "");
	}

	// Tests for expression parsing with tilde detection

	#[test]
	fn test_parse_arm_body_stops_at_paren_with_tilde_requires_comma() {
		// Should stop before paren group containing tilde, then error (missing comma)
		let input = quote! { None (~x~) };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}

	#[test]
	fn test_parse_arm_body_function_call_without_tilde() {
		// Should consume paren group without tilde (function call)
		let input = quote! { Some(x, y), };
		let (body, rest) = parse_arm_body(&input).unwrap();

		assert_eq!(body.to_string(), "Some (x , y)");
		assert_eq!(rest.to_string(), ",");
	}

	#[test]
	fn test_parse_arm_body_nested_calls_stops_at_tilde_pattern() {
		// foo().bar() should be consumed, then stop at (~x~), then error
		let input = quote! { foo().bar() (~x~) };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}

	#[test]
	fn test_parse_arm_body_complex_expr_with_pattern_after() {
		// Complex expression followed by pattern - should error (missing comma)
		let input = quote! { x + y * z (bar, ~name~) };
		let result = parse_arm_body(&input);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.message.contains("Missing comma"));
	}
}

pub fn format_matcher_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	let mut tokens = input.into_iter().peekable();

	let scrutinee = if let Some(TokenTree::Ident(ident)) = tokens.next() {
		if ident.to_string() != "match" {
			return quote! { compile_error!("Expected 'match'") };
		}
		if let Some(TokenTree::Ident(scrutinee_ident)) = tokens.next() {
			scrutinee_ident
		} else {
			return quote! { compile_error!("Expected scrutinee identifier") };
		}
	} else {
		return quote! { compile_error!("Expected 'match'") };
	};

	let arms_group = if let Some(TokenTree::Group(group)) = tokens.next() {
		if group.delimiter() != proc_macro2::Delimiter::Brace {
			return quote! { compile_error!("Expected match body in braces") };
		}
		group
	} else {
		return quote! { compile_error!("Expected match body") };
	};

	let mut arms = Vec::new();
	let mut arm_iter = arms_group.stream().into_iter().peekable();

	while arm_iter.peek().is_some() {
		let mut pattern_tokens = Vec::new();

		loop {
			match arm_iter.peek() {
				Some(TokenTree::Punct(p)) if p.as_char() == '=' => {
					let saved = arm_iter.clone();
					arm_iter.next();
					if let Some(TokenTree::Punct(p2)) = arm_iter.peek() {
						if p2.as_char() == '>' {
							arm_iter.next();
							break;
						}
					}
					arm_iter = saved;
					pattern_tokens.push(arm_iter.next().unwrap());
				}
				Some(_) => {
					pattern_tokens.push(arm_iter.next().unwrap());
				}
				None => {
					return quote! { compile_error!("Expected => in match arm") };
				}
			}
		}

		let remaining: proc_macro2::TokenStream = arm_iter.collect();
		let (body, rest) = match parse_arm_body(&remaining) {
			Ok(result) => result,
			Err(e) => return e.to_compile_error(),
		};

		arm_iter = rest.into_iter().peekable();

		if let Some(TokenTree::Punct(p)) = arm_iter.peek() {
			if p.as_char() == ',' {
				arm_iter.next();
			}
		}

		let pattern_stream: proc_macro2::TokenStream = pattern_tokens.clone().into_iter().collect();
		let mut pattern_iter = pattern_stream.into_iter().peekable();

		let pattern_span = if let Some(first_token) = pattern_tokens.first() {
			if let Some(last_token) = pattern_tokens.last() {
				first_token.span().join(last_token.span()).unwrap_or_else(|| first_token.span())
			} else {
				first_token.span()
			}
		} else {
			Span::call_site()
		};

		let pattern = if let Some(TokenTree::Ident(ident)) = pattern_iter.peek() {
			if ident.to_string() == "_" {
				pattern_iter.next();
				Pattern::Symbol("_".to_string())
			} else {
				match parse_pattern(&mut pattern_iter) {
					Ok(p) => p,
					Err(e) => return quote! { compile_error!(#e) },
				}
			}
		} else {
			match parse_pattern(&mut pattern_iter) {
				Ok(p) => p,
				Err(e) => return quote! { compile_error!(#e) },
			}
		};

		arms.push(MatchArm {
			pattern,
			pattern_span,
			body,
		});
	}

	for (i, arm) in arms.iter().enumerate() {
		if matches!(&arm.pattern, Pattern::Symbol(s) if s == "_") {
			continue;
		}

		for prev_arm in &arms[..i] {
			if matches!(&prev_arm.pattern, Pattern::Symbol(s) if s == "_") {
				continue;
			}

			if arm.pattern.is_equivalent_to(&prev_arm.pattern) {
				let primary_error = quote_spanned! { arm.pattern_span =>
					compile_error!("unreachable pattern");
				};
				let note = quote_spanned! { prev_arm.pattern_span =>
					compile_error!("note: earlier pattern here");
				};
				return primary_error.into_iter().chain(note).collect();
			}
		}
	}

	let mut if_chains = Vec::new();

	for arm in &arms {
		if matches!(&arm.pattern, Pattern::Symbol(s) if s == "_") {
			let body = &arm.body;
			if_chains.push(quote! {
				{
					#body
				}
			});
			break;
		}

		let mut bindings = Vec::new();
		let mut conditions = Vec::new();
		let mut idx = 0;
		let list_name = Ident::new("__input_list", Span::call_site());

		let (_has_repetition, _patterns_after_rep, min_len) = if let Pattern::List(patterns) = &arm.pattern {
			let (rep_position, patterns_after_rep) = analyze_repetition(patterns);
			let has_repetition = rep_position.is_some();

			for (i, pat) in patterns.iter().enumerate() {
				if let Pattern::Placeholder(name, true) = pat {
					let binding_name = Ident::new(name, Span::call_site());
					let start_idx = idx;

					bindings.push((
						name.clone(),
						quote! {
							let #binding_name = if #list_name.len() >= #patterns_after_rep + #start_idx {
								#list_name.skip(#start_idx).take(#list_name.len() - #patterns_after_rep - #start_idx)
							} else {
								::imbl::Vector::new()
							};
						},
					));

					// Patterns after repetition are indexed from the end
					idx = 0;
				} else if has_repetition && i > rep_position.unwrap() {
					let offset_from_end = patterns.len() - i - 1;

					match pat {
						Pattern::Symbol(s) => {
							conditions.push(quote! {
								#list_name.len() > #offset_from_end &&
								#list_name.get(#list_name.len() - 1 - #offset_from_end).and_then(|e| if let ::alicorn_format::Element::Symbol(sym) = e {
									if sym.as_str() == #s { Some(()) } else { None }
								} else { None }).is_some()
							});
						}
						Pattern::Placeholder(name, false) => {
							let binding_name = Ident::new(name, Span::call_site());
							bindings.push((
								name.clone(),
								quote! { let #binding_name = &#list_name[#list_name.len() - 1 - #offset_from_end]; },
							));
						}
						_ => {
							return quote! { compile_error!("Complex patterns after repetition not yet supported") };
						}
					}
				} else {
					generate_match_code(&list_name, pat, &mut bindings, &mut conditions, &mut idx);
				}
			}

			let min_len = calculate_min_len(patterns);
			(has_repetition, patterns_after_rep, min_len)
		} else {
			return quote! { compile_error!("Top-level pattern must be a list (use parentheses)") };
		};

		let body = &arm.body;
		let binding_code: Vec<_> = bindings.iter().map(|(_, code)| code).collect();
		let length_check = quote! { #list_name.len() >= #min_len };

		if conditions.is_empty() {
			if_chains.push(quote! {
				if {
					let #list_name = &#scrutinee;
					#length_check
				} {
					let #list_name = &#scrutinee;
					#(#binding_code)*
					#body
				}
			});
		} else {
			if_chains.push(quote! {
				if {
					let #list_name = &#scrutinee;
					#length_check && #(#conditions)&&*
				} {
					let #list_name = &#scrutinee;
					#(#binding_code)*
					#body
				}
			});
		}
	}

	let match_expr = if if_chains.len() == 1 {
		if_chains.into_iter().next().unwrap()
	} else {
		let first = if_chains.first().unwrap();
		let rest = &if_chains[1..];
		quote! {
			#first #( else #rest )*
		}
	};

	quote! {
		{
			let #scrutinee: &::alicorn_format::FormatList = &#scrutinee;
			#match_expr
		}
	}
}
