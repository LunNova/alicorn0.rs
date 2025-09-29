use super::{BraceType, Element, FormatList};
use crate::{
	Indent,
	lexer::{self, Item},
};
use std::sync::LazyLock;
use ustr::Ustr;

static COMMA: LazyLock<Ustr> = LazyLock::new(|| Ustr::from(","));
static SEMICOLON: LazyLock<Ustr> = LazyLock::new(|| Ustr::from(";"));
static SQUARE_LIST: LazyLock<Ustr> = LazyLock::new(|| Ustr::from("square-list"));
static CURLY_LIST: LazyLock<Ustr> = LazyLock::new(|| Ustr::from("curly-list"));
static SQUARE_LIST_COMMA: LazyLock<Ustr> = LazyLock::new(|| Ustr::from("square-list,"));
static CURLY_LIST_COMMA: LazyLock<Ustr> = LazyLock::new(|| Ustr::from("curly-list,"));

#[derive(Debug, PartialEq, Clone, Copy)]
enum ListType {
	Braced(BraceType),
	BracedFromFunctionCall(BraceType),
	Naked,
}

pub fn listify(lines: Vec<lexer::LexedLine>) -> Result<FormatList, ListifyError> {
	if lines.is_empty() {
		return Ok(vec![]);
	}

	let mut line_iter = lines.into_iter().peekable();
	// Stack of indent level at start + type of list
	// when empty we're assumed to be in a naked list that started at indent -1
	let mut list_types: Vec<(Indent, ListType)> = vec![
		// Everything in the doc is subordinate to a naked list that can't be exited
		(-1, ListType::Naked),
	];
	// Starts with partial list for the secret -1 indent list we're pretending exists to simplify the logic :3
	let mut partials: Vec<Vec<Element>> = vec![vec![]];
	let mut requires_continuation = false;
	let mut can_chain = false;
	loop {
		let (list_indent, list_type) = *list_types.last().unwrap();

		let mut finished = false;
		let (line_indent, line) = match line_iter.next() {
			Some((line_indent, line)) => (line_indent, line),
			_ => {
				// We are done with the file but need to wind down the list stack to indent zero!
				finished = true;
				(0, vec![])
			}
		};

		let min_valid_indent: Indent = list_types
			.iter()
			.filter(|(_, lt)| *lt != ListType::Naked)
			.map(|(indent, _)| *indent)
			.last()
			.unwrap_or(-1);

		if line_indent < min_valid_indent {
			dbg!(line_indent, min_valid_indent, list_types);
			return Err(ListifyError::InvalidIndent);
		}

		let mut line_iter = line.into_iter().peekable();

		// Check for backslash at the start of the line
		let starts_with_backslash = matches!(line_iter.peek(), Some(Item::Backslash));
		if starts_with_backslash {
			line_iter.next(); // Consume the backslash
		}

		// FIXME: let's handle commas and semicolons by storing them inside the partial list
		// and then at the point where that list gets popped and used we apply them
		// This lets us enforce not mixing comma/semicolon and handle special comma rules more easily

		// Handle indentation and list context based on the list type
		match list_type {
			ListType::Braced(brace_type) | ListType::BracedFromFunctionCall(brace_type) => {
				// A line at the same indent as a braced list's start must start by closing that list
				if line_indent == list_indent && !matches!(line_iter.peek(), Some(Item::CloseBrace(b)) if *b == brace_type) {
					dbg!(line_indent, list_indent, list_type);
					return Err(ListifyError::InvalidIndent);
				}

				// When in a braced list and line starts with backslash, start a new naked list context at this depth
				if starts_with_backslash {
					list_types.push((line_indent, ListType::Naked));
					partials.push(vec![]);
				}
			}
			ListType::Naked => {
				if line_indent > list_indent {
					if line_indent > list_indent + 1 {
						return Err(ListifyError::InvalidIndent);
					}
					// Starting a new list at a deeper indent which is added to prev list,
					// unless there was a continuation marker
					if !requires_continuation && !starts_with_backslash {
						// Create a new nested list
						list_types.push((line_indent, ListType::Naked));
						partials.push(vec![]);
					}
					// If requires_continuation or starts_with_backslash, we continue in the same list
				} else if line_indent == list_indent {
					// We're at the same indent level, which means we're continuing in the same list

					// If the last line ended with backslash but we're not indented further, that's an error
					if requires_continuation && !starts_with_backslash {
						return Err(ListifyError::InvalidContinuation);
					}

					// If this line starts with backslash and we're at the same level,
					// we're continuing the parent list
					if starts_with_backslash {
						// Close the current list
						let completed_list = partials.pop().unwrap();
						let parent_list = partials.last_mut().unwrap();

						if completed_list.len() == 1 {
							// Lonely token rule: If a list has only one element, don't wrap it
							parent_list.push(completed_list.into_iter().next().unwrap());
						} else if !completed_list.is_empty() {
							// Otherwise wrap in a list
							parent_list.push(Element::List(completed_list));
						}

						// Pop the list type as we've completed it
						list_types.pop();
					} else {
						// We're at the same level as a previous naked list, so close it and start a new one
						let completed_list = partials.pop().unwrap();
						let parent_list = partials.last_mut().unwrap();

						if completed_list.len() == 1 {
							// Lonely token rule
							parent_list.push(completed_list.into_iter().next().unwrap());
						} else if !completed_list.is_empty() {
							parent_list.push(Element::List(completed_list));
						}

						// Pop and push to maintain the same indent level but start a new list
						list_types.pop();
						list_types.push((line_indent, ListType::Naked));
						partials.push(vec![]);
					}
				} else {
					// We've decreased indent, so close all lists down to this indent level
					while list_types.last().unwrap().0 >= line_indent {
						let completed_list = partials.pop().unwrap();
						let parent_list = partials.last_mut().unwrap();

						let relevant_items = completed_list.iter().filter(|it| !matches!(it, Element::Comment(_))).count();

						if relevant_items == 1 {
							// Lonely token rule
							parent_list.extend(completed_list);
						} else if !completed_list.is_empty() {
							parent_list.push(Element::List(completed_list));
						}

						list_types.pop();
					}

					// If we've gone back to a previous level and this line starts with backslash,
					// we're continuing that list
					if starts_with_backslash {
						// Continue in the current list
					} else {
						// Start a new list at this level
						list_types.push((line_indent, ListType::Naked));
						partials.push(vec![]);
					}
				}
			}
		}

		// We ran out of lines and should have cleaned up the context
		if finished {
			// Close all remaining lists
			while partials.len() > 1 {
				let completed_list = partials.pop().unwrap();
				let parent_list = partials.last_mut().unwrap();
				let relevant_items = completed_list.iter().filter(|it| !matches!(it, Element::Comment(_))).count();

				if relevant_items == 1 {
					// Lonely token rule
					parent_list.extend(completed_list);
				} else if !completed_list.is_empty() {
					parent_list.push(Element::List(completed_list));
				}

				list_types.pop();
			}
			break;
		}

		// Process entries in this line
		let mut current_list = partials.last_mut().unwrap();
		let mut temp_elements = vec![];

		// Reset continuation flag
		requires_continuation = false;

		while let Some(item) = line_iter.next() {
			match item {
				Item::Symbol(symbol) => {
					assert!(!symbol.is_empty());
					can_chain = false; // Clear chaining flag when processing symbols
					// Check if next item is an open brace without whitespace (function call syntax)
					if let Some(Item::OpenBrace(brace_type)) = line_iter.peek() {
						// FIXME: Handle function call-style syntax by acting like
						// Sym, OpenBrace, ...,
						// ->
						// OpenBrace, Sym, Comma, ...

						// Add all accumulated elements to the current list
						current_list.append(&mut temp_elements);

						// Start a new braced list from function call
						list_types.push((line_indent, ListType::BracedFromFunctionCall(*brace_type)));
						partials.push(vec![]);
						current_list = partials.last_mut().unwrap();
						match brace_type {
							BraceType::Paren => {}
							BraceType::Square => {
								temp_elements.push(Element::Symbol(*SQUARE_LIST_COMMA));
							}
							BraceType::Curly => {
								temp_elements.push(Element::Symbol(*CURLY_LIST_COMMA));
							}
						}

						temp_elements.push(Element::Symbol(symbol));
						temp_elements.push(Element::Symbol(*COMMA));
						line_iter.next();
					} else {
						temp_elements.push(Element::Symbol(symbol));
					}
				}
				Item::OpenBrace(brace_type) => {
					// Check if we can chain (last element was a function call result)
					if can_chain && !current_list.is_empty() {
						// Pop the last element (the function to call)
						let function_element = current_list.pop().unwrap();

						// Add all accumulated elements to the current list
						current_list.append(&mut temp_elements);

						// Start a new function call braced list
						list_types.push((line_indent, ListType::BracedFromFunctionCall(brace_type)));
						partials.push(vec![]);
						current_list = partials.last_mut().unwrap();

						// Add the function and comma for function call syntax
						match brace_type {
							BraceType::Paren => {}
							BraceType::Square => {
								temp_elements.push(Element::Symbol(*SQUARE_LIST_COMMA));
							}
							BraceType::Curly => {
								temp_elements.push(Element::Symbol(*CURLY_LIST_COMMA));
							}
						}

						temp_elements.push(function_element);
						temp_elements.push(Element::Symbol(*COMMA));

						can_chain = false;
					} else {
						// Add all accumulated elements to the current list
						current_list.append(&mut temp_elements);

						// Start a new regular braced list
						list_types.push((line_indent, ListType::Braced(brace_type)));
						partials.push(vec![]);
						current_list = partials.last_mut().unwrap();
						match brace_type {
							BraceType::Paren => {}
							BraceType::Square => {
								temp_elements.push(Element::Symbol(*SQUARE_LIST_COMMA));
							}
							BraceType::Curly => {
								temp_elements.push(Element::Symbol(*CURLY_LIST_COMMA));
							}
						}

						can_chain = false;
					}
				}
				Item::CloseBrace(brace_type) => {
					// First add any temporary elements
					current_list.append(&mut temp_elements);

					// Ensure we're closing the right type of brace
					if let Some((_, list_type)) = list_types.last() {
						let (expected_brace, is_function_call) = match list_type {
							ListType::Braced(b) => (*b, false),
							ListType::BracedFromFunctionCall(b) => (*b, true),
							_ => return Err(ListifyError::UnexpectedCloseBrace),
						};

						if expected_brace != brace_type {
							return Err(ListifyError::MismatchedBraces);
						}

						// Close the braced list
						let mut completed_list = partials.pop().unwrap();
						let has_commas = completed_list.iter().any(|x| matches!(x, Element::Symbol(s) if *s == *COMMA));
						let has_semicolons = completed_list.iter().any(|x| matches!(x, Element::Symbol(s) if *s == *SEMICOLON));
						let has_special_split = completed_list
							.first()
							.map(|x| if let Element::Symbol(sym) = x { sym.ends_with(',') } else { false })
							.unwrap_or(false);
						if has_commas || has_semicolons {
							if has_commas && has_semicolons {
								return Err(ListifyError::MixedListDelimeter);
							}
							// TODO: create new empty list
							// split list into chunks delimited by matches!(x, Element::Symbol(s) if s == ",")
							// For each chunk:
							//   If size == 1 add the 1 elem in it directly to empty list
							//   If size > 1 wrap all in an Element::List and add to list
							// Create a new empty list to hold the processed chunks
							let mut processed_list = Vec::new();

							if has_special_split {
								if let Element::Symbol(special_split) = completed_list.remove(0) {
									processed_list.push(Element::Symbol(special_split.trim_end_matches(',').to_string().into()));
								} else {
									panic!("Should be unreachable");
								}
							}

							// Keep track of current chunk elements
							let mut current_chunk = Vec::new();

							// Process the list items
							for item in completed_list {
								if let Element::Symbol(ref s) = item {
									if *s == *COMMA || *s == *SEMICOLON {
										// Process the current chunk
										if current_chunk.len() == 1 && *s == *COMMA {
											// For single items, add directly without wrapping
											processed_list.push(current_chunk.pop().unwrap());
										} else if !current_chunk.is_empty() {
											// For multiple items, wrap in a list
											processed_list.push(Element::List(current_chunk));
											current_chunk = Vec::new();
										}
										continue;
									}
								}
								// Add this item to the current chunk
								current_chunk.push(item);
							}

							// Process the final chunk
							if has_commas {
								if current_chunk.len() == 1 {
									processed_list.push(current_chunk.pop().unwrap());
								} else if !current_chunk.is_empty() {
									processed_list.push(Element::List(current_chunk));
								}
							} else if has_semicolons {
								// For semicolons, tail elements go directly to top level
								processed_list.extend(current_chunk);
							}
							completed_list = processed_list;
						} else if has_special_split {
							if let Element::Symbol(special_split) = &completed_list[0] {
								let res = Element::Symbol(special_split.trim_end_matches(',').to_string().into());
								completed_list[0] = res
							} else {
								panic!("Should be unreachable");
							}
						}
						let parent_list = partials.last_mut().unwrap();
						parent_list.push(Element::List(completed_list));

						// Set chaining flag if this was a function call, otherwise clear it
						can_chain = is_function_call;

						list_types.pop();
						current_list = partials.last_mut().unwrap();
					} else {
						return Err(ListifyError::UnexpectedCloseBrace);
					}
				}
				Item::Comma => {
					// Only valid in braced lists
					if let Some((_, list_type)) = list_types.last() {
						match list_type {
							ListType::Braced(_) | ListType::BracedFromFunctionCall(_) => temp_elements.push(Element::Symbol(*COMMA)),
							_ => return Err(ListifyError::CommaInNakedList),
						}
					} else {
						return Err(ListifyError::CommaInNakedList);
					}
				}
				Item::Semicolon => {
					// In naked lists, semicolons split the list but don't appear in output
					if let Some((_, ListType::Naked)) = list_types.last() {
						// Add accumulated elements as a sublist
						if !temp_elements.is_empty() {
							current_list.push(Element::List(temp_elements.clone()));
							temp_elements.clear();
						}
					} else {
						temp_elements.push(Element::Symbol(*SEMICOLON))
					}
				}
				Item::Backslash => {
					// Check if this is at the end of the line
					if line_iter.peek().is_none() {
						requires_continuation = true;
					} else {
						// Can't backslash mid-line
						return Err(ListifyError::InvalidContinuation);
					}
				}
				Item::Whitespace => {
					// Whitespace is only relevant for peeking for fn call special case
					// Also clears chaining flag
					can_chain = false;
				}
				Item::String(sym) => {
					can_chain = false;
					temp_elements.push(Element::String(sym));
				}
				Item::Comment(com) => {
					can_chain = false;
					temp_elements.push(Element::Comment(com));
				}
				Item::Number(n) => {
					can_chain = false;
					temp_elements.push(Element::Number(n));
				}
			}
		}

		// Add any remaining temporary elements to the current list
		if !temp_elements.is_empty() {
			if temp_elements.len() == 1 && requires_continuation {
				// If we have a single element and continuation, don't wrap
				current_list.push(temp_elements.pop().unwrap());
			} else if !requires_continuation {
				// If no continuation and multiple elements, add them all
				current_list.append(&mut temp_elements);
			} else {
				// With continuation and multiple elements, add as a list
				current_list.push(Element::List(temp_elements.clone()));
				temp_elements.clear();
			}
		}
	}

	assert!(partials.len() == 1);
	assert!(list_types.len() == 1);

	Ok(partials.pop().unwrap())
}

#[derive(Debug, PartialEq, Clone)]
pub enum ListifyError {
	InvalidIndent,
	InvalidContinuation,
	MismatchedBraces,
	UnexpectedCloseBrace,
	UnexpectedItem,
	CommaInNakedList,
	EmptyLexedLine,
	MixedListDelimeter,
}

impl std::error::Error for ListifyError {}

impl std::fmt::Display for ListifyError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ListifyError::InvalidIndent => write!(f, "Invalid indentation"),
			ListifyError::InvalidContinuation => write!(f, "Invalid continuation"),
			ListifyError::MismatchedBraces => write!(f, "Mismatched braces"),
			ListifyError::UnexpectedCloseBrace => write!(f, "Unexpected closing brace"),
			ListifyError::UnexpectedItem => write!(f, "Unexpected item"),
			ListifyError::CommaInNakedList => write!(f, "Comma in naked list"),
			ListifyError::EmptyLexedLine => write!(f, "Empty lexed line"),
			ListifyError::MixedListDelimeter => write!(f, "Mixed list delimiters"),
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::lexer::{Item, lex};
	use pretty_assertions::assert_eq;

	// test cases for listification stage of format
	// Note: lexing has already been performed and multiline comments and strings
	// have been condensed into single String/Comment Items
	// so eg a file with 3 lines in it
	// #### hello
	// 		a
	// 	b
	// has been parsed into (0, Item::Comment(" hello\na\nb"))
	// Helper macro for creating Element variants
	macro_rules! exp {
			// Handle nested lists with [] syntax
			([ $($inner:tt),* ]) => {
					Element::List(vec![ $( exp!($inner) ),* ])
			};
			// Handle string literals with s prefix
			(s $s:expr) => {
					Element::String($s.to_string())
			};
			// Handle number literals with n prefix
			(n $n:expr) => {
					Element::Number($n as f64)
			};
			// Handle comment literals with c prefix
			(c $s:expr) => {
					Element::Comment($s.to_string())
			};
			// Default to Symbol for other literals
			($s:expr) => {
					Element::Symbol($s.into())
			};
	}

	macro_rules! symbol {
		($s:expr) => {
			Element::Symbol(Ustr::from($s))
		};
	}
	macro_rules! isymbol {
		($s:expr) => {
			Item::Symbol(Ustr::from($s))
		};
	}

	// Used to specify expected result in format tests
	macro_rules! format_list {
			([ $($elem:tt),* ]) => {
					vec![Element::List(vec![ $( exp!($elem) ),* ])]
			};
			([ $($elem1:tt),* ], [ $($elem2:tt),* ] $(, [ $($rest:tt),* ])* ) => {
					{
							let mut result = vec![
									vec![ $( exp!($elem1) ),* ],
									vec![ $( exp!($elem2) ),* ]
							];
							$(
									result.push(vec![ $( exp!($rest) ),* ]);
							)*
							result
					}
			};
	}

	#[test]
	fn test_c_style_fn_syntax() {
		let input = "some_complicated_type(x, y)";

		let lexed = lex(input).unwrap();
		let result = listify(lexed).unwrap();

		let expected = format_list!(["some_complicated_type", "x", "y"]);
		assert_eq!(result, expected);
	}

	#[test]
	fn test_c_style_fn_syntax_with_op() {
		let input = "some_complicated_type(x, y, z + x)";

		let lexed = lex(input).unwrap();
		let result = listify(lexed).unwrap();

		let expected = format_list!(["some_complicated_type", "x", "y", ["z", "+", "x"]]);
		assert_eq!(result, expected);
	}

	#[test]
	fn test_lambda_expression_explicitlists() {
		let input = r#"
lambda ((x : i32) (y : i32) (z : some_complicated_type(x, y)))
	body body body
	body body
	result
"#;

		let lexed = lex(input).unwrap();
		let result = listify(lexed).unwrap();

		let expected = format_list!([
			"lambda",
			[
				["x", ":", "i32"],
				["y", ":", "i32"],
				["z", ":", ["some_complicated_type", "x", "y"]]
			],
			["body", "body", "body"],
			["body", "body"],
			"result"
		]);

		assert_eq!(result, expected);
	}

	#[test]
	fn test_lambda_expression_no_semicolon() {
		let input = r#"lambda
	a
		x : i32
		y : i32
		z : some_complicated_type(x, y)
	body body body
	body body
	result"#;

		let lexed = lex(input).unwrap();
		let result = listify(lexed).unwrap();

		let expected = format_list!([
			"lambda",
			[
				"a",
				["x", ":", "i32"],
				["y", ":", "i32"],
				["z", ":", ["some_complicated_type", "x", "y"]]
			],
			["body", "body", "body"],
			["body", "body"],
			"result"
		]);

		assert_eq!(result, expected);
	}

	#[test]
	fn test_lambda_expression() {
		let input = r#"lambda
	;
		x : i32
		y : i32
		z : some_complicated_type(x, y)
	body body body
	body body
	result"#;

		let lexed = lex(input).unwrap();
		let result = listify(lexed).unwrap();

		let expected = format_list!([
			"lambda",
			[
				["x", ":", "i32"],
				["y", ":", "i32"],
				["z", ":", ["some_complicated_type", "x", "y"]]
			],
			["body", "body", "body"],
			["body", "body"],
			"result"
		]);

		assert_eq!(result, expected);
	}

	#[test]
	fn basic() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("let"),
				Item::Whitespace,
				isymbol!("my_var"),
				Item::Whitespace,
				isymbol!("="),
				Item::Whitespace,
				Item::String("this is a string".to_string()),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("my_var"),
				symbol!("="),
				Element::String("this is a string".to_string())
			])]
		)
	}

	#[test]
	fn basic_multiple() {
		let lexed = vec![
			(
				0,
				vec![
					isymbol!("let"),
					Item::Whitespace,
					isymbol!("my_var"),
					Item::Whitespace,
					isymbol!("="),
					Item::Whitespace,
					Item::String("this is a string".to_string()),
				],
			),
			(
				0,
				vec![
					isymbol!("let"),
					Item::Whitespace,
					isymbol!("my_var2"),
					Item::Whitespace,
					isymbol!("="),
					Item::Whitespace,
					Item::String("this is a string2".to_string()),
				],
			),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![
				Element::List(vec![
					symbol!("let"),
					symbol!("my_var"),
					symbol!("="),
					Element::String("this is a string".to_string())
				]),
				Element::List(vec![
					symbol!("let"),
					symbol!("my_var2"),
					symbol!("="),
					Element::String("this is a string2".to_string())
				])
			]
		)
	}

	#[test]
	fn parenlist_after_symbol_whitespace() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("print"),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("+"),
				Item::Whitespace,
				isymbol!("b"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("print"),
				Element::List(vec![symbol!("a"), symbol!("+"), symbol!("b"),])
			])]
		)
	}

	#[test]
	fn parenlist_after_symbol_no_whitespace() {
		// Tests the function-call like syntax
		let lexed = lex(r#"
print("fmt", a + b)
"#)
		.unwrap();
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("print"),
				Element::String("fmt".to_string()),
				Element::List(vec![symbol!("a"), symbol!("+"), symbol!("b"),])
			])]
		)
	}

	#[test]
	fn chained_function_calls() {
		// Tests chained function calls like foo(a)(b)
		let lexed = lex(r#"foo(a)(b)"#).unwrap();
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![Element::List(vec![symbol!("foo"), symbol!("a")]), symbol!("b")])]
		)
	}

	#[test]
	fn chained_function_calls_complex() {
		// Tests more complex chained calls
		let lexed = lex(r#"host-tuple-of(accept-handler-desc)(accept-handler)"#).unwrap();
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![symbol!("host-tuple-of"), symbol!("accept-handler-desc")]),
				symbol!("accept-handler")
			])]
		)
	}

	#[test]
	fn triple_chained_function_calls() {
		// Tests triple chained calls foo(a)(b)(c)
		let lexed = lex(r#"foo(a)(b)(c)"#).unwrap();
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![Element::List(vec![symbol!("foo"), symbol!("a")]), symbol!("b")]),
				symbol!("c")
			])]
		)
	}

	#[test]
	fn chained_function_calls_with_multiple_args() {
		// Tests chained calls with multiple arguments
		let lexed = lex(r#"make-adder(5, x)(3, y)"#).unwrap();
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![symbol!("make-adder"), Element::Number(5.0), symbol!("x")]),
				Element::Number(3.0),
				symbol!("y")
			])]
		)
	}

	// Series of tests for "lonely token" rule
	// Normally when we indent that means we're creating nested lists in the parent indent level
	// but if a line has only a single meaningful token in it it's inlined into the parent list
	// print a
	// print
	// 	a
	// both parse the same due to the lonely token rule
	// if we didn't have this rule, the second one would parse to print (a) with a in a nested list
	#[test]
	fn indent_lonely_one() {
		let lexed = vec![
			(0, vec![isymbol!("let"), Item::Whitespace, isymbol!("my_var")]),
			(1, vec![isymbol!("=")]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![symbol!("let"), symbol!("my_var"), symbol!("="),])]
		)
	}

	#[test]
	fn indent_lonely() {
		let lexed = vec![
			(0, vec![isymbol!("let"), Item::Whitespace, isymbol!("my_var")]),
			(1, vec![isymbol!("=")]),
			(1, vec![Item::String("this is a string".to_string())]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("my_var"),
				symbol!("="),
				Element::String("this is a string".to_string())
			])]
		)
	}

	#[test]
	fn indent_lonely_2() {
		let lexed = vec![
			(0, vec![isymbol!("let"), Item::Whitespace]),
			(1, vec![isymbol!("my_var")]),
			(1, vec![isymbol!("=")]),
			(1, vec![Item::String("this is a string".to_string())]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("my_var"),
				symbol!("="),
				Element::String("this is a string".to_string())
			])]
		)
	}

	#[test]
	fn indent_nested() {
		let lexed = vec![
			(0, vec![isymbol!("print"), Item::Whitespace]),
			(1, vec![isymbol!("a"), isymbol!("+"), isymbol!("b")]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("print"),
				Element::List(vec![symbol!("a"), symbol!("+"), symbol!("b"),])
			])]
		)
	}

	#[test]
	fn curly_braces() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("let"),
				Item::Whitespace,
				isymbol!("config"),
				Item::Whitespace,
				isymbol!("="),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Curly),
				isymbol!("name"),
				Item::Whitespace,
				isymbol!(":"),
				Item::Whitespace,
				Item::String("test".to_string()),
				Item::CloseBrace(BraceType::Curly),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("config"),
				symbol!("="),
				Element::List(vec![
					symbol!("curly-list"),
					symbol!("name"),
					symbol!(":"),
					Element::String("test".to_string()),
				])
			])]
		)
	}

	#[test]
	fn square_brackets() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("let"),
				Item::Whitespace,
				isymbol!("items"),
				Item::Whitespace,
				isymbol!("="),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Square),
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("b"),
				Item::Whitespace,
				isymbol!("c"),
				Item::CloseBrace(BraceType::Square),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("items"),
				symbol!("="),
				Element::List(vec![symbol!("square-list"), symbol!("a"), symbol!("b"), symbol!("c"),])
			])]
		)
	}

	#[test]
	fn nested_brace_types() {
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				Item::OpenBrace(BraceType::Square),
				isymbol!("a"),
				Item::CloseBrace(BraceType::Square),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Curly),
				isymbol!("b"),
				Item::CloseBrace(BraceType::Curly),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![symbol!("square-list"), symbol!("a"),]),
				Element::List(vec![symbol!("curly-list"), symbol!("b"),]),
			])]
		)
	}

	#[test]
	fn comment_only_line() {
		let lexed = vec![
			(
				0,
				vec![isymbol!("let"), Item::Whitespace, isymbol!("x"), Item::Whitespace, isymbol!("=")],
			),
			(1, vec![Item::Comment(" This is a comment".to_string())]),
			(1, vec![isymbol!("5")]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("x"),
				symbol!("="),
				Element::Comment(" This is a comment".to_string()),
				symbol!("5"),
			])]
		)
	}

	#[test]
	fn comment_only_line_lonely() {
		let lexed = vec![
			(
				0,
				vec![isymbol!("let"), Item::Whitespace, isymbol!("x"), Item::Whitespace, isymbol!("=")],
			),
			(1, vec![Item::Comment(" This is a comment".to_string())]),
			(1, vec![isymbol!("5"), Item::Comment(" This is also a comment".to_string())]),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("let"),
				symbol!("x"),
				symbol!("="),
				Element::Comment(" This is a comment".to_string()),
				symbol!("5"),
				Element::Comment(" This is also a comment".to_string()),
			])]
		)
	}

	#[test]
	fn comma_naked_list() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("b"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("c"),
				Item::Comma,
			],
		)];
		let listified = listify(lexed);
		assert!(listified.is_err());
	}
	#[test]
	fn semicolon_naked_list() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("b"),
				Item::Semicolon,
				Item::Whitespace,
				isymbol!("c"),
				Item::Semicolon,
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![symbol!("a"), symbol!("b"),]),
				Element::List(vec![symbol!("c")]),
			])]
		)
	}

	#[test]
	fn comma_separated_parenlist() {
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("b"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("c"),
				Item::Whitespace,
				isymbol!("d"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("e"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				Element::List(vec![symbol!("a"), symbol!("b"),]),
				Element::List(vec![symbol!("c"), symbol!("d"),]),
				symbol!("e"),
			])]
		)
	}

	#[test]
	fn multiple_explicit_lists_comma_and_semicolon() {
		let lexed = vec![
			(
				0,
				vec![
					Item::OpenBrace(BraceType::Paren),
					isymbol!("a"),
					Item::Whitespace,
					isymbol!("b"),
					Item::Comma,
					Item::Whitespace,
					isymbol!("c"),
					Item::Whitespace,
					isymbol!("d"),
					Item::Comma,
					Item::Whitespace,
					isymbol!("e"),
					Item::CloseBrace(BraceType::Paren),
				],
			),
			(
				0,
				vec![
					Item::OpenBrace(BraceType::Square),
					isymbol!("a"),
					Item::Whitespace,
					isymbol!("b"),
					Item::Semicolon,
					Item::Whitespace,
					isymbol!("c"),
					Item::Whitespace,
					isymbol!("d"),
					Item::Semicolon,
					Item::Whitespace,
					isymbol!("e"),
					Item::CloseBrace(BraceType::Square),
				],
			),
		];
		let listified = listify(lexed).unwrap();
		dbg!(&listified);
		assert_eq!(
			listified,
			vec![
				Element::List(vec![
					Element::List(vec![symbol!("a"), symbol!("b"),]),
					Element::List(vec![symbol!("c"), symbol!("d"),]),
					symbol!("e"),
				]),
				Element::List(vec![
					symbol!("square-list"),
					Element::List(vec![symbol!("a"), symbol!("b"),]),
					Element::List(vec![symbol!("c"), symbol!("d"),]),
					Element::List(vec![symbol!("e")]),
				])
			]
		)
	}

	#[test]
	fn comma_separated_parenlist_single_values() {
		// Single values in comma-separated lists should not be wrapped
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("b"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("c"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(listified, vec![Element::List(vec![symbol!("a"), symbol!("b"), symbol!("c"),])])
	}

	#[test]
	fn square_comma_separated_list() {
		// Based on the Lua tests for "[1 , 2, 1 + 2]"
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Square),
				isymbol!("1"),
				Item::Whitespace,
				Item::Comma,
				Item::Whitespace,
				isymbol!("2"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("1"),
				Item::Whitespace,
				isymbol!("+"),
				Item::Whitespace,
				isymbol!("2"),
				Item::CloseBrace(BraceType::Square),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("square-list"),
				symbol!("1"),
				symbol!("2"),
				Element::List(vec![symbol!("1"), symbol!("+"), symbol!("2"),]),
			])]
		)
	}

	#[test]
	fn semicolon_separated_parenlist() {
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Semicolon,
				Item::Whitespace,
				isymbol!("b"),
				Item::Semicolon,
				Item::Whitespace,
				isymbol!("c"),
				Item::Whitespace,
				isymbol!("d"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![Element::List(vec![
				Element::List(vec![symbol!("a")]),
				Element::List(vec![symbol!("b")]),
				symbol!("c"),
				symbol!("d"),
			])])]
		)
	}

	#[test]
	fn multiple_indentation_levels() {
		let lexed = vec![
			(0, vec![isymbol!("root")]),
			(1, vec![isymbol!("level1"), Item::Whitespace, isymbol!("item1")]),
			(2, vec![isymbol!("level2"), Item::Whitespace, isymbol!("item2")]),
			(3, vec![isymbol!("level3")]),
			(1, vec![isymbol!("back_to_level1")]),
		];
		let lexed_2 = lex(r#"
root
	level1 item1
		level2 item2
			level3
	back_to_level1
"#)
		.unwrap();
		assert_eq!(lexed, lexed_2);
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("root"),
				Element::List(vec![
					symbol!("level1"),
					symbol!("item1"),
					Element::List(vec![symbol!("level2"), symbol!("item2"), symbol!("level3"),]),
				]),
				symbol!("back_to_level1"),
			])]
		)
	}

	#[test]
	fn multiple_indentation_levels_end_0() {
		let lexed = vec![
			(0, vec![isymbol!("root")]),
			(1, vec![isymbol!("level1"), Item::Whitespace, isymbol!("item1")]),
			(2, vec![isymbol!("level2"), Item::Whitespace, isymbol!("item2")]),
			(3, vec![isymbol!("level3")]),
			(0, vec![isymbol!("back_to_level0")]),
		];
		let lexed_2 = lex(r#"
root
	level1 item1
		level2 item2
			level3
back_to_level0
"#)
		.unwrap();
		assert_eq!(lexed, lexed_2);
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![
				Element::List(vec![
					symbol!("root"),
					Element::List(vec![
						symbol!("level1"),
						symbol!("item1"),
						Element::List(vec![symbol!("level2"), symbol!("item2"), symbol!("level3"),]),
					]),
				]),
				symbol!("back_to_level0"),
			]
		)
	}

	#[test]
	fn mixed_indentation_and_parentheses() {
		let lexed = vec![
			(0, vec![isymbol!("function")]),
			(
				1,
				vec![
					isymbol!("args"),
					Item::Whitespace,
					Item::OpenBrace(BraceType::Paren),
					isymbol!("a"),
					Item::Comma,
					Item::Whitespace,
					isymbol!("b"),
					Item::CloseBrace(BraceType::Paren),
				],
			),
			(1, vec![isymbol!("body")]),
			(2, vec![isymbol!("statement1")]),
			(
				2,
				vec![
					isymbol!("statement2"),
					Item::Whitespace,
					Item::OpenBrace(BraceType::Paren),
					isymbol!("nested"),
					Item::CloseBrace(BraceType::Paren),
				],
			),
		];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("function"),
				Element::List(vec![symbol!("args"), Element::List(vec![symbol!("a"), symbol!("b"),]),]),
				Element::List(vec![
					symbol!("body"),
					symbol!("statement1"),
					Element::List(vec![symbol!("statement2"), Element::List(vec![symbol!("nested"),]),]),
				]),
			])]
		)
	}

	#[ignore = "mixfix NYI"]
	#[test]
	fn mixfix_list_syntax() {
		let lexed = vec![(0, vec![isymbol!("-["), isymbol!("expr"), isymbol!("]->")])];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![Element::List(vec![symbol!("-[_]->"), symbol!("expr"),])])]
		)
	}

	// FIXME: this is definitely not right go reread format.lua until it makes sense
	#[ignore = "mixfix NYI, expected value in assert wrong"]
	#[test]
	fn complex_brace_punctuation_syntax() {
		let lexed = vec![(
			0,
			vec![
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("-["),
				isymbol!("b"),
				isymbol!("]"),
				isymbol!("/["),
				isymbol!("c"),
				isymbol!("]->"),
				Item::Whitespace,
				isymbol!("d"),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("a"),
				Element::List(vec![
					symbol!("-[_]/[_]->"),
					Element::List(vec![symbol!("b")]),
					Element::List(vec![symbol!("c")]),
				]),
				symbol!("d"),
			])]
		)
	}

	#[test]
	fn continuation_not_wrapped() {
		let input = r#"
a \
	b c
	d
"#;
		let res = listify(lex(input).unwrap()).unwrap();
		assert_eq!(res, format_list!(["a", "b", "c", "d"]))
	}

	#[test]
	fn continuation_not_wrapped_next() {
		let input = r#"
a
	\ b c
	d
"#;
		let res = listify(lex(input).unwrap()).unwrap();
		assert_eq!(res, format_list!(["a", "b", "c", "d"]))
	}

	#[test]
	fn continuation_not_wrapped_multiple_start() {
		let input = r#"
a a \
	b c
	d
"#;
		let res = listify(lex(input).unwrap()).unwrap();
		assert_eq!(res, format_list!(["a", "a", "b", "c", "d"]))
	}

	#[test]
	fn continuation_not_wrapped_next_multiple_start() {
		let input = r#"
a a
	\ b c
	d
"#;
		let res = listify(lex(input).unwrap()).unwrap();
		assert_eq!(res, format_list!(["a", "a", "b", "c", "d"]))
	}

	#[test]
	fn continuation_like_lonely() {
		let input = r#"
a \
	b
	c
"#;
		let res = listify(lex(input).unwrap()).unwrap();
		assert_eq!(res, format_list!(["a", "b", "c"]))
	}

	#[test]
	fn lonely_mixed() {
		// Skipping a level (0 -> 2)
		let lexed: Vec<(i16, Vec<Item>)> = vec![
			(0, vec![isymbol!("a")]),
			(0, vec![isymbol!("b"), isymbol!("c")]),
			(0, vec![isymbol!("d")]),
		];
		let result = listify(lexed).unwrap();
		assert_eq!(
			result,
			vec![symbol!("a"), Element::List(vec![symbol!("b"), symbol!("c"),]), symbol!("d"),]
		);
	}

	#[test]
	fn lonely_mixed_explicit() {
		// Skipping a level (0 -> 2)
		let lexed: Vec<(i16, Vec<Item>)> = vec![
			(0, vec![isymbol!("a")]),
			(
				0,
				vec![
					Item::OpenBrace(BraceType::Paren),
					isymbol!("b"),
					isymbol!("c"),
					Item::CloseBrace(BraceType::Paren),
				],
			),
			(
				0,
				vec![Item::OpenBrace(BraceType::Paren), isymbol!("d"), Item::CloseBrace(BraceType::Paren)],
			),
		];
		let result = listify(lexed).unwrap();
		assert_eq!(
			result,
			vec![
				symbol!("a"),
				Element::List(vec![symbol!("b"), symbol!("c"),]),
				Element::List(vec![symbol!("d"),]),
			]
		);
	}

	#[test]
	fn indentation_error() {
		// Skipping a level (0 -> 2)
		let lexed = vec![(0, vec![isymbol!("root")]), (2, vec![isymbol!("too_indented")])];
		let result = listify(lexed);
		assert!(result.is_err());
	}

	#[test]
	fn indentation_skip_with_semicolon() {
		// Skipping a level (0 -> 2) by using a line with only semicolon at indent 1
		let lexed = vec![
			(0, vec![isymbol!("root")]),
			(1, vec![Item::Semicolon]),
			(2, vec![isymbol!("extra_indented")]),
		];
		let result = listify(lexed).unwrap();
		assert_eq!(result, format_list!(["root", "extra_indented"]));
	}

	#[test]
	fn mismatched_parentheses() {
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				isymbol!("unclosed"),
				// Missing closing parenthesis
			],
		)];
		let result = listify(lexed);
		assert!(result.is_err());
	}

	#[test]
	fn braces_with_closing_type_mismatch() {
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Curly),
				isymbol!("mismatch"),
				Item::CloseBrace(BraceType::Square), // Wrong closing type
			],
		)];
		let result = listify(lexed);
		assert!(result.is_err());
	}

	#[test]
	fn combined_comma_and_semicolon() {
		// Based on the Lua test "let (letn't, =, let)"
		let lexed = vec![(
			0,
			vec![
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("b"),
				Item::Semicolon,
				Item::Whitespace,
				isymbol!("c"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("d"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed);
		assert_eq!(listified, Err(ListifyError::MixedListDelimeter));
	}

	#[test]
	fn simple_fn_def() {
		// Based on the Lua test "def f (a : int, b : int) (a + b)"
		let lexed = vec![(
			0,
			vec![
				isymbol!("def"),
				Item::Whitespace,
				isymbol!("f"),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Whitespace,
				isymbol!(":"),
				Item::Whitespace,
				isymbol!("int"),
				Item::Comma,
				Item::Whitespace,
				isymbol!("b"),
				Item::Whitespace,
				isymbol!(":"),
				Item::Whitespace,
				isymbol!("int"),
				Item::CloseBrace(BraceType::Paren),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Paren),
				isymbol!("a"),
				Item::Whitespace,
				isymbol!("+"),
				Item::Whitespace,
				isymbol!("b"),
				Item::CloseBrace(BraceType::Paren),
			],
		)];
		let listified = listify(lexed).unwrap();
		assert_eq!(
			listified,
			vec![Element::List(vec![
				symbol!("def"),
				symbol!("f"),
				Element::List(vec![
					Element::List(vec![symbol!("a"), symbol!(":"), symbol!("int"),]),
					Element::List(vec![symbol!("b"), symbol!(":"), symbol!("int"),]),
				]),
				Element::List(vec![symbol!("a"), symbol!("+"), symbol!("b"),]),
			])]
		)
	}
}
