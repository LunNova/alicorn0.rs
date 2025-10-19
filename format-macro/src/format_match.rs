use proc_macro2::{Ident, Span, TokenTree};
use quote::quote;
use syn::Expr;

#[derive(Debug, Clone)]
enum Pattern {
	Symbol(String),
	Number(f64),
	String(String),
	Placeholder(String, bool), // (name, is_repetition)
	List(Vec<Pattern>),
}

struct MatchArm {
	pattern: Pattern,
	body: Expr,
}

fn parse_pattern(tokens: &mut std::iter::Peekable<proc_macro2::token_stream::IntoIter>) -> Result<Pattern, String> {
	if let Some(token) = tokens.next() {
		match token {
			TokenTree::Group(group) => {
				if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
					// Parse list pattern
					let mut patterns = Vec::new();
					let mut inner = group.stream().into_iter().peekable();

					while inner.peek().is_some() {
						let pat = parse_pattern(&mut inner)?;
						patterns.push(pat);

						// Skip comma if present
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
				// Parse ~name~ or ~name...~ placeholder
				let name = if let Some(TokenTree::Ident(ident)) = tokens.next() {
					ident.to_string()
				} else {
					return Err("Expected identifier after ~".to_string());
				};

				// Check for ... repetition marker
				let mut is_repetition = false;
				if let Some(TokenTree::Punct(p)) = tokens.peek() {
					if p.as_char() == '.' {
						// Try to consume ...
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

				// Consume closing ~
				if let Some(TokenTree::Punct(p)) = tokens.next() {
					if p.as_char() != '~' {
						return Err(format!("Expected closing ~ after placeholder name, got {}", p));
					}
				} else {
					return Err("Expected closing ~ after placeholder name".to_string());
				}

				return Ok(Pattern::Placeholder(name, is_repetition));
			}
			TokenTree::Ident(ident) => {
				let s = ident.to_string();

				// Check if followed by a parenthesis group (function call syntax)
				if let Some(TokenTree::Group(group)) = tokens.peek() {
					if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
						let group = if let Some(TokenTree::Group(g)) = tokens.next() {
							g
						} else {
							unreachable!()
						};

						// Parse as list with symbol as first element
						let mut patterns = vec![Pattern::Symbol(s)];
						let mut inner = group.stream().into_iter().peekable();

						while inner.peek().is_some() {
							let pat = parse_pattern(&mut inner)?;
							patterns.push(pat);

							// Skip comma if present
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

				// Try to parse as number
				if let Ok(n) = s.parse::<f64>() {
					return Ok(Pattern::Number(n));
				}

				// Otherwise treat as string (remove quotes if present)
				let s = s.trim_matches('"').to_string();
				return Ok(Pattern::String(s));
			}
			TokenTree::Punct(p) => {
				// Collect joint punctuation into multi-char operators (but not commas/semicolons)
				let ch = p.as_char();
				if ch == ',' || ch == ';' {
					return Ok(Pattern::Symbol(ch.to_string()));
				}

				let mut op = String::new();
				op.push(ch);

				// If this punct has Joint spacing, collect following Joint puncts
				if p.spacing() == proc_macro2::Spacing::Joint {
					while let Some(TokenTree::Punct(next_p)) = tokens.peek() {
						let next_ch = next_p.as_char();
						// Don't consume commas or semicolons
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
				// Check if this is the last pattern - repetition can only be at the end
				// For now, we'll just match from current index to end
				let i = *idx;
				bindings.push((name.clone(), quote! { let #binding_name = &#scrutinee[#i..]; }));
				// Don't increment idx - caller should check and stop iterating
			} else {
				// Single element
				let i = *idx;
				bindings.push((name.clone(), quote! { let #binding_name = &#scrutinee[#i]; }));
				*idx += 1;
			}
		}
		Pattern::List(patterns) => {
			let i = *idx;
			let temp_name = Ident::new(&format!("__list_{}", i), Span::call_site());

			// Check that this element is a list
			conditions.push(quote! {
				#scrutinee.get(#i).and_then(|e| if let ::alicorn_format::Element::List(_) = e {
					Some(())
				} else { None }).is_some()
			});

			// Extract the inner list
			bindings.push((
				format!("__list_{}", i),
				quote! {
					let #temp_name = if let ::alicorn_format::Element::List(inner) = &#scrutinee[#i] {
						inner.as_slice()
					} else {
						unreachable!()
					};
				},
			));

			*idx += 1;

			// Recursively match inner patterns
			let mut inner_idx = 0;
			for pat in patterns {
				generate_match_code(&temp_name, pat, bindings, conditions, &mut inner_idx);
			}
		}
	}
}

pub fn format_matcher_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	let mut tokens = input.into_iter().peekable();

	// Parse "match"
	let scrutinee = if let Some(TokenTree::Ident(ident)) = tokens.next() {
		if ident.to_string() != "match" {
			return quote! { compile_error!("Expected 'match'") };
		}
		// Parse scrutinee
		if let Some(TokenTree::Ident(scrutinee_ident)) = tokens.next() {
			scrutinee_ident
		} else {
			return quote! { compile_error!("Expected scrutinee identifier") };
		}
	} else {
		return quote! { compile_error!("Expected 'match'") };
	};

	// Parse match body (must be a braced group)
	let arms_group = if let Some(TokenTree::Group(group)) = tokens.next() {
		if group.delimiter() != proc_macro2::Delimiter::Brace {
			return quote! { compile_error!("Expected match body in braces") };
		}
		group
	} else {
		return quote! { compile_error!("Expected match body") };
	};

	// Parse arms - use syn's ParseStream for proper expression parsing
	let arms_stream = arms_group.stream();
	let arms = match syn::parse::Parser::parse2(
		|input: syn::parse::ParseStream| {
			let mut arms = Vec::new();

			while !input.is_empty() {
				// Collect pattern tokens until we hit =>
				// We can't use syn here because the pattern contains non-Rust syntax (~x~)
				let mut pattern_tokens = Vec::new();

				// Manually collect tokens until =>
				let fork = input.fork();
				let remaining: proc_macro2::TokenStream = fork.parse()?;
				let mut iter = remaining.into_iter().peekable();

				while let Some(tok) = iter.peek() {
					// Check for =>
					if let proc_macro2::TokenTree::Punct(p) = tok {
						if p.as_char() == '=' {
							let saved = iter.clone();
							iter.next();
							if let Some(proc_macro2::TokenTree::Punct(p2)) = iter.peek() {
								if p2.as_char() == '>' {
									// Found =>, stop collecting pattern
									// Advance input past what we collected
									for _ in &pattern_tokens {
										let _: proc_macro2::TokenTree = input.parse()?;
									}
									break;
								}
							}
							iter = saved;
						}
					}
					pattern_tokens.push(iter.next().unwrap());
				}

				// Consume =>
				input.parse::<syn::Token![=>]>()?;

				// Parse body as expression - syn knows when it ends!
				let body: Expr = match input.parse() {
					Ok(expr) => expr,
					Err(e) => {
						eprintln!("ERROR parsing body after pattern: {:?}", pattern_tokens);
						eprintln!("Remaining input: {}", input);
						return Err(e);
					}
				};

				// Optional trailing comma
				if input.peek(syn::Token![,]) {
					input.parse::<syn::Token![,]>()?;
				}

				// Parse the pattern from collected tokens
				let pattern_stream: proc_macro2::TokenStream = pattern_tokens.into_iter().collect();
				let mut pattern_iter = pattern_stream.into_iter().peekable();

				let pattern = if let Some(TokenTree::Ident(ident)) = pattern_iter.peek() {
					if ident.to_string() == "_" {
						pattern_iter.next(); // consume _
						Pattern::Symbol("_".to_string())
					} else {
						parse_pattern(&mut pattern_iter).map_err(|e| syn::Error::new(input.span(), e))?
					}
				} else {
					parse_pattern(&mut pattern_iter).map_err(|e| syn::Error::new(input.span(), e))?
				};

				arms.push(MatchArm { pattern, body });
			}

			Ok(arms)
		},
		arms_stream,
	) {
		Ok(arms) => arms,
		Err(e) => return e.to_compile_error(),
	};

	// Generate match code
	let mut if_chains = Vec::new();

	for arm in &arms {
		// Check if this is wildcard
		if matches!(&arm.pattern, Pattern::Symbol(s) if s == "_") {
			let body = &arm.body;
			if_chains.push(quote! {
				{
					#body
				}
			});
			break; // Wildcard must be last
		}

		// Generate matching conditions and bindings for this arm
		let mut bindings = Vec::new();
		let mut conditions = Vec::new();
		let mut idx = 0;

		// First, check that scrutinee is a list and extract it
		let list_name = Ident::new("__input_list", Span::call_site());

		// The top-level pattern should be a List (from the parentheses in source)
		let (_has_repetition, _patterns_after_rep, min_len) = if let Pattern::List(patterns) = &arm.pattern {
			// Count how many non-repetition patterns we have after any repetition
			let mut has_repetition = false;
			let mut patterns_after_rep: usize = 0;
			for pat in patterns.iter().rev() {
				if matches!(pat, Pattern::Placeholder(_, true)) {
					has_repetition = true;
					break;
				}
				if has_repetition {
					patterns_after_rep += 1;
				}
			}

			// Generate match code for the inner patterns
			for (i, pat) in patterns.iter().enumerate() {
				if let Pattern::Placeholder(name, true) = pat {
					// This is a repetition
					// It matches from current idx to (length - patterns_after_rep - 1)
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

					// Move idx forward to skip the repetition part
					// The next pattern will start at len - patterns_after_rep
					idx = 0; // We'll calculate it specially for patterns after repetition
				} else if has_repetition && i > patterns.iter().position(|p| matches!(p, Pattern::Placeholder(_, true))).unwrap() {
					// Pattern after repetition - index from the end
					let offset_from_end = patterns.len() - i - 1;

					// Generate special code for patterns that come after repetition
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

			// Calculate minimum required length (non-repetition patterns before any repetition)
			let min_len = if has_repetition {
				patterns.iter().position(|p| matches!(p, Pattern::Placeholder(_, true))).unwrap() + patterns_after_rep
			} else {
				patterns.len()
			};

			(has_repetition, patterns_after_rep, min_len)
		} else {
			return quote! { compile_error!("Top-level pattern must be a list (use parentheses)") };
		};

		let body = &arm.body;
		let binding_code: Vec<_> = bindings.iter().map(|(_, code)| code).collect();

		// Always add length check
		let length_check = quote! { #list_name.len() >= #min_len };

		if conditions.is_empty() {
			// No explicit conditions, just length check and bindings
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

	// Chain all the if statements together
	let result = if if_chains.len() == 1 {
		if_chains.into_iter().next().unwrap()
	} else {
		let first = if_chains.first().unwrap();
		let rest = &if_chains[1..];
		quote! {
			#first #( else #rest )*
		}
	};

	result
}
