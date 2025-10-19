use proc_macro2::TokenTree;
use quote::quote;

fn token_stream_to_alicorn_string(input: proc_macro2::TokenStream) -> String {
	let mut result = String::new();
	let mut tokens = input.into_iter().peekable();

	while let Some(token) = tokens.next() {
		match token {
			TokenTree::Group(group) => {
				let (open, close) = match group.delimiter() {
					proc_macro2::Delimiter::Parenthesis => ("(", ")"),
					proc_macro2::Delimiter::Brace => ("{", "}"),
					proc_macro2::Delimiter::Bracket => ("[", "]"),
					proc_macro2::Delimiter::None => ("", ""),
				};
				result.push_str(open);
				result.push_str(&token_stream_to_alicorn_string(group.stream()));
				result.push_str(close);
			}
			TokenTree::Ident(ident) => {
				if !result.is_empty() && !result.ends_with(|c: char| c.is_whitespace() || "([{,;".contains(c)) {
					result.push(' ');
				}
				result.push_str(&ident.to_string());
			}
			TokenTree::Punct(punct) => {
				let ch = punct.as_char();
				match ch {
					',' => result.push(','),
					';' => result.push(';'),
					_ => {
						// Collect joint punctuation into multi-char operators
						let mut op = String::new();
						op.push(ch);

						// If this punct has Joint spacing, collect following Joint puncts
						if punct.spacing() == proc_macro2::Spacing::Joint {
							while let Some(TokenTree::Punct(p)) = tokens.peek() {
								op.push(p.as_char());
								let spacing = p.spacing();
								tokens.next();
								if spacing == proc_macro2::Spacing::Alone {
									break;
								}
							}
						}

						// Add space before operator if needed
						if !result.is_empty() && !result.ends_with(|c: char| c.is_whitespace() || "([{,;".contains(c)) {
							result.push(' ');
						}
						result.push_str(&op);
					}
				}
			}
			TokenTree::Literal(lit) => {
				if !result.is_empty() && !result.ends_with(|c: char| c.is_whitespace() || "([{,;".contains(c)) {
					result.push(' ');
				}
				result.push_str(&lit.to_string());
			}
		}
	}

	result
}

fn generate_element_code(element: &alicorn_format::Element) -> proc_macro2::TokenStream {
	match element {
		alicorn_format::Element::Symbol(s) => {
			let s_str = s.as_str();
			quote! { ::alicorn_format::Element::Symbol(::ustr::Ustr::from(#s_str)) }
		}
		alicorn_format::Element::Number(n) => {
			quote! { ::alicorn_format::Element::Number(#n) }
		}
		alicorn_format::Element::String(s) => {
			quote! { ::alicorn_format::Element::String(#s.to_string()) }
		}
		alicorn_format::Element::Comment(c) => {
			quote! { ::alicorn_format::Element::Comment(#c.to_string()) }
		}
		alicorn_format::Element::List(list) => {
			let elements = list.iter().map(generate_element_code);
			quote! { ::alicorn_format::Element::List(::imbl::vector![#(#elements),*]) }
		}
	}
}

pub fn listify(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	// Convert TokenStream to Alicorn format string
	let alicorn_string = token_stream_to_alicorn_string(input);

	// Parse it at compile time using the existing format() function
	let format_list = match alicorn_format::format(&alicorn_string) {
		Ok(list) => list,
		Err(e) => {
			let error_msg = format!("listify error: {}\nInput string: {}", e, alicorn_string);
			return quote! {
				compile_error!(#error_msg)
			}
			.into();
		}
	};

	// Generate code to construct the FormatList at runtime
	let elements = format_list.iter().map(generate_element_code);

	quote! {
		::imbl::vector![#(#elements),*]
	}
}
