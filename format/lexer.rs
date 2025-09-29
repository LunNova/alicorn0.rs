use crate::Indent;
use core::f64;
use std::{iter::Peekable, str::Lines};
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BraceType {
	Paren,
	Square,
	Curly,
	// Angle,
}

#[derive(Debug, PartialEq)]
pub enum Item {
	OpenBrace(BraceType),
	CloseBrace(BraceType),
	/// Either a comment until the end of the line after #
	/// or a multiline comment that starts with ###
	/// and is indented and ends when indentation returns to same level
	Comment(String),
	/// String literal in double quotes
	/// Multiline string literal in triple double quotes
	/// Same indent behavior
	/// Multiline string literals have leading tabs stripped
	/// and are only
	String(String),
	Number(f64),
	Symbol(Ustr),
	Comma,
	Semicolon,
	Backslash,
	Whitespace,
}

struct Anchor {
	// 0-indexed into original string
	char: usize,
	// 1-indexed as displayed in the editor
	line: usize,
	column: usize,
}

type Span = std::ops::Range<Anchor>;

struct Spanned<const SPANNED: bool, T> {
	token: T,
	span: Span,
}
pub type LexedLine = (Indent, Vec<Item>);

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Copy, Clone)]
enum MultilineType {
	Comment,
	String,
}

enum LexError {}

fn peek_multiline_remainder(buf: &mut String, source: &mut Peekable<Lines>, initial_indent: Indent) {
	while let Some(next) = source.peek() {
		if next.trim().is_empty() {
			source.next();
			if !buf.is_empty() {
				buf.push('\n');
			}
			continue;
		}
		let indent = next.chars().take_while(|&c| c == '\t').count() as Indent;
		if indent > initial_indent {
			let next = source.next().unwrap();
			if !buf.is_empty() {
				buf.push('\n');
			}
			buf.push_str(next[(initial_indent + 1) as usize..].trim_end());
		} else {
			break;
		}
	}
	while buf.ends_with('\n') {
		buf.pop();
	}
}

pub fn lex(input: &str) -> Result<Vec<LexedLine>, ()> {
	let mut lexed: Vec<LexedLine> = vec![];
	let mut lines = input.lines().peekable();
	let mut _line_number = 1;

	// TODO: make lines Peek<> so multiline comments/strings can get extra lines until the comment ends
	// and the main loop still works

	while let Some(line) = lines.next() {
		let mut tokens: Vec<Item> = vec![];

		if line.trim().is_empty() {
			continue;
		}

		// Count indentation (number of tabs at the beginning)
		let indent = line.chars().take_while(|&c| c == '\t').count() as Indent;
		let original_line = &line[indent as usize..];
		let line = original_line.trim_end();

		let mut chars = line.chars().peekable();
		let mut pos = 0;

		fn can_start_symbol(chr: char) -> bool {
			if chr.is_whitespace() || chr.is_control() {
				return false;
			}
			match chr {
				'#' | '"' | '\'' | '\\' | ',' | ';' | '[' | '(' | '{' | ']' | ')' | '}' => false,
				_ => true,
			}
		}

		fn can_continue_symbol(chr: char) -> bool {
			if chr.is_whitespace() || chr.is_control() {
				return false;
			}
			match chr {
				'#' | '"' | '\\' | ',' | ';' | '[' | '(' | '{' | ']' | ')' | '}' => false,
				_ => true,
			}
		}

		while let Some(c) = chars.next() {
			pos += 1;

			match c {
				// Multiline comment
				'#' if line[pos - 1..].starts_with("####") => {
					// Skip the next # characters since we've already processed the first one
					chars.next();
					chars.next();
					chars.next();
					pos += 3;

					// According to notion remainder is valid for multiline comments
					let mut multi = (&line[pos..]).to_string();
					peek_multiline_remainder(&mut multi, &mut lines, indent);
					tokens.push(Item::Comment(multi));
					break;
				}
				// Single line comment
				'#' => {
					tokens.push(Item::Comment(original_line[pos..].to_string()));
					break;
				}

				// Handle string literals
				'"' if line[pos - 1..].starts_with(r#""""""#) => {
					// Check for quadruple quotes (multiline string)
					// Skip the next three quotes since we've already processed the first one
					chars.next();
					chars.next();
					chars.next();
					pos += 3;

					// Start of multiline string
					// Capture rest of line then
					// TODO: consume more lines from iterator here and add to the string then add the string to tokens
					// use peek so we don't consume if indent isn't enough
					// error if don't get any lines of content in the multiline

					let mut multi = (&line[pos..].trim_end()).to_string();
					// TODO: Determine if content after multiline start should be allowed.
					// Lua allows penta quotes ("""""") which includes an extra quote in the string,
					// but unclear if this is correct behavior or a bug in the Lua parser.
					if !multi.is_empty() {
						return Err(());
					}
					peek_multiline_remainder(&mut multi, &mut lines, indent);
					tokens.push(Item::String(multi));
					break;
				}
				'"' => {
					// Single line string
					let mut string_content = String::new();
					let mut escaped = false;

					let mut ended = false;
					while let Some(str_char) = chars.next() {
						pos += 1;

						if escaped {
							match str_char {
								'n' => string_content.push('\n'),
								't' => string_content.push('\t'),
								'r' => string_content.push('\r'),
								'\\' => string_content.push('\\'),
								'"' => string_content.push('"'),
								_ => return Err(()), // Invalid escape sequence
							}
							escaped = false;
						} else if str_char == '\\' {
							escaped = true;
						} else if str_char == '"' {
							ended = true;
							break;
						} else {
							string_content.push(str_char);
						}
					}

					if !ended {
						return Err(());
					}

					tokens.push(Item::String(string_content));
				}

				// Braces
				'(' => tokens.push(Item::OpenBrace(BraceType::Paren)),
				')' => tokens.push(Item::CloseBrace(BraceType::Paren)),
				'[' => tokens.push(Item::OpenBrace(BraceType::Square)),
				']' => tokens.push(Item::CloseBrace(BraceType::Square)),
				'{' => tokens.push(Item::OpenBrace(BraceType::Curly)),
				'}' => tokens.push(Item::CloseBrace(BraceType::Curly)),

				// Separators
				',' => tokens.push(Item::Comma),
				';' => tokens.push(Item::Semicolon),
				'\\' => tokens.push(Item::Backslash),

				// Whitespace
				' ' | '\t' => {
					tokens.push(Item::Whitespace);
					// Consume additional whitespace
					while let Some(&next_char) = chars.peek() {
						if next_char == ' ' || next_char == '\t' {
							chars.next();
							pos += 1;
						} else {
							break;
						}
					}
				}

				// Handle symbols
				_ if can_start_symbol(c) => {
					let mut symbol = c.to_string();
					while let Some(&next_char) = chars.peek() {
						if can_continue_symbol(next_char) {
							symbol.push(chars.next().unwrap());
							pos += 1;
						} else {
							break;
						}
					}
					// If this was a numer make an Item::Number instead
					// if this was not valid as a number or a symbol that's a parse error
					// TODO: check this logic is right
					if let Some(special_num) = match symbol.as_str() {
						"inf" | "+inf" => Some(f64::INFINITY),
						"-inf" => Some(f64::NEG_INFINITY),
						"nan" => Some(f64::NAN),
						_ => None,
					} {
						tokens.push(Item::Number(special_num));
					} else if let Ok(num) = symbol.parse::<f64>() {
						// TODO: if this was a numer make an Item::Number instead
						// if this was not valid as a number or a symbol that's a parse error
						tokens.push(Item::Number(num));
					} else {
						tokens.push(Item::Symbol(Ustr::from(&symbol)));
					}
				}

				// Other characters (could be errors or special tokens depending on requirements)
				chr => todo!("Unknown char {chr}"),
			}
		}

		// Add to lexed result if we have tokens
		if !tokens.is_empty() {
			lexed.push((indent, tokens));
		}

		_line_number += 1;
	}

	Ok(lexed)
}

#[cfg(test)]
mod tests {
	use super::*;
	use pretty_assertions::assert_eq;

	// Helper macro for creating Symbol items in tests
	macro_rules! symbol {
		($s:expr) => {
			Item::Symbol($s.into())
		};
	}

	#[test]
	fn basic_tokens() {
		let input = "symbol 123 (test) [array] {object} <tag>  , # comment";
		let result = lex(input).unwrap();

		// Expected: one line with indent 0, containing the tokens
		assert_eq!(result.len(), 1);
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);

		// Check tokens
		assert_eq!(
			tokens,
			&vec![
				symbol!("symbol"),
				Item::Whitespace,
				Item::Number(123.0),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Paren),
				symbol!("test"),
				Item::CloseBrace(BraceType::Paren),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Square),
				symbol!("array"),
				Item::CloseBrace(BraceType::Square),
				Item::Whitespace,
				Item::OpenBrace(BraceType::Curly),
				symbol!("object"),
				Item::CloseBrace(BraceType::Curly),
				Item::Whitespace,
				symbol!("<tag>"),
				Item::Whitespace,
				Item::Comma,
				Item::Whitespace,
				Item::Comment(" comment".to_string()),
			]
		);
	}

	#[test]
	fn indentation() {
		let input = "level0\n\tlevel1\n\t\tlevel2\nlevel0_again";
		let result = lex(input).unwrap();

		assert_eq!(
			result,
			vec![
				(0, vec![symbol!("level0")]),
				(1, vec![symbol!("level1")]),
				(2, vec![symbol!("level2")]),
				(0, vec![symbol!("level0_again")])
			]
		);
	}

	#[test]
	fn single_line_comment() {
		let input = "before # this is a comment\nafter";
		let result = lex(input).unwrap();

		assert_eq!(
			result,
			vec![
				(
					0,
					vec![symbol!("before"), Item::Whitespace, Item::Comment(" this is a comment".to_string())]
				),
				(0, vec![symbol!("after")])
			]
		);
	}

	#[test]
	fn multiline_comment() {
		let input = "before #### multiline comment starts\n\tindented comment line\n\tmore comment\nnot comment";
		let result = lex(input).unwrap();

		// Expected: two lines (multiline comment should act as a single line at lexing stage)
		assert_eq!(result.len(), 2);

		// Line 1: "before" and a multiline comment token
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("before"));
		assert_eq!(tokens[1], Item::Whitespace);
		assert_eq!(
			tokens[2],
			Item::Comment(" multiline comment starts\nindented comment line\nmore comment".to_string())
		);

		// Line 2: "not comment"
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("not"));
		assert_eq!(tokens[1], Item::Whitespace);
		assert_eq!(tokens[2], symbol!("comment"));
	}

	#[test]
	fn multiline_comment_gap_no_trailing_newline() {
		// This test specifically catches the bug where multiline comments
		// incorrectly include a trailing newline if there was an empty line
		// immediately after the multi line comment
		let input = "#### multiline comment\n\tcomment content\n\tlast line\n\nafter comment";
		let result = lex(input).unwrap();

		// Expected: two lines
		assert_eq!(result.len(), 2);

		// Line 1: multiline comment token
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(tokens.len(), 1);

		// The comment should NOT have a trailing newline
		assert_eq!(
			tokens[0],
			Item::Comment(" multiline comment\ncomment content\nlast line".to_string())
		);

		// Line 2: "after comment"
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("after"));
		assert_eq!(tokens[1], Item::Whitespace);
		assert_eq!(tokens[2], symbol!("comment"));
	}

	#[test]
	fn multiline_comment_continues_after_empty_line() {
		// Test that multiline comments correctly continue after empty lines
		// when indentation returns to comment level
		let input = "#### multiline comment\n\tcomment content\n\n\tmore comment after gap\n\tlast line\nafter comment";
		let result = lex(input).unwrap();

		// Expected: two lines
		assert_eq!(result.len(), 2);

		// Line 1: multiline comment token that includes content after the gap
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(tokens.len(), 1);
		assert_eq!(
			tokens[0],
			Item::Comment(" multiline comment\ncomment content\n\nmore comment after gap\nlast line".to_string())
		);

		// Line 2: "after comment"
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("after"));
		assert_eq!(tokens[1], Item::Whitespace);
		assert_eq!(tokens[2], symbol!("comment"));
	}

	#[test]
	fn single_line_string() {
		let input = r#"let my_var = "this is a string""#;
		let result = lex(input).unwrap();

		// Expected: one line with a symbol, whitespace, symbol, whitespace, and string
		assert_eq!(result.len(), 1);
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);

		assert_eq!(
			tokens,
			&vec![
				symbol!("let"),
				Item::Whitespace,
				symbol!("my_var"),
				Item::Whitespace,
				symbol!("="),
				Item::Whitespace,
				Item::String("this is a string".to_string()),
			]
		);
	}

	#[test]
	fn multiline_string_invalid_after_start() {
		let input = r#"before """" multiline string starts
	indented string line
	more string
not string"#;
		eprintln!("{input}");
		let result = lex(input);
		assert!(result.is_err());
	}

	#[test]
	fn multiline_string() {
		let input = r#"before """"
	indented string line
	more string
not string"#;
		eprintln!("{input}");
		let result = lex(input).unwrap();

		// Line 1: "before" and a multiline string token
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(
			tokens,
			&vec![
				symbol!("before"),
				Item::Whitespace,
				Item::String("indented string line\nmore string".to_string())
			]
		);

		// Line 2: "not string"
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("not"));
		assert_eq!(tokens[1], Item::Whitespace);
		assert_eq!(tokens[2], symbol!("string"));

		// Expected: two lines (multiline string should act as a single line at lexing stage)
		assert_eq!(result.len(), 2);
	}

	#[test]
	fn string_tab_stripping() {
		let input = r#"
""""
	second line
		third line (more indented)
	fourth line"#;
		let result = lex(input).unwrap();

		// Expected: one token in one line, with the correct indentation preserved
		assert_eq!(result.len(), 1);
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);

		// Check the string content
		if let Item::String(content) = &tokens[0] {
			// Verify the content of the string with correct tab stripping
			assert_eq!(content, "second line\n\tthird line (more indented)\nfourth line");
		} else {
			panic!("Expected a String token");
		}
	}

	#[test]
	#[ignore = "Penta quotes not yet supported - unclear if Lua behavior is correct"]
	fn penta_quote_multiline_string() {
		let input = r#"before """""
	indented string line
	more string
not string"#;
		eprintln!("{input}");
		let result = lex(input).unwrap();

		// Line 1: "before" and a multiline string token
		// Line 2: "not string" (outside the multiline string)
		assert_eq!(result.len(), 2);

		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(tokens.len(), 3); // "before", whitespace, string

		// Check that we have the expected tokens
		assert_eq!(tokens[0], symbol!("before"));
		assert_eq!(tokens[1], Item::Whitespace);

		// Check the string content - should have leading quote from penta quotes
		if let Item::String(content) = &tokens[2] {
			// Penta quotes should include a leading quote character
			assert_eq!(content, "\"indented string line\nmore string");
		} else {
			panic!("Expected a String token, got {:?}", tokens[2]);
		}

		// Second line should be outside the multiline string
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("not"));
		assert_eq!(tokens[2], symbol!("string"));
	}

	#[test]
	fn complex_numbers() {
		let input = "123 45.67 -89.01 1.23e4 5.6e-7";
		let result = lex(input).unwrap();

		// Expected: one line with numbers and whitespace
		assert_eq!(result.len(), 1);
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);

		// Check tokens
		assert_eq!(
			tokens,
			&vec![
				Item::Number(123.0),
				Item::Whitespace,
				Item::Number(45.67),
				Item::Whitespace,
				Item::Number(-89.01),
				Item::Whitespace,
				Item::Number(1.23e4),
				Item::Whitespace,
				Item::Number(5.6e-7),
			]
		);
	}

	#[test]
	fn escaped_characters_in_strings() {
		let input = r#"simple = "line\nwith\tescape\r\n\"chars\"\\""#;
		let result = lex(input).unwrap();

		// Expected: one line with tokens for a string with escape sequences
		assert_eq!(result.len(), 1);
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);

		// The string should have properly handled escape sequences
		if let Item::String(content) = &tokens[4] {
			assert_eq!(content, "line\nwith\tescape\r\n\"chars\"\\");
		} else {
			panic!("Expected a String token");
		}
	}

	#[test]
	fn empty_lines_and_whitespace_only_lines() {
		let input = "first\n\n  \t  \nfourth";
		let result = lex(input).unwrap();

		// Expected: only two lines with actual content (empty lines should be skipped)
		assert_eq!(result.len(), 2);

		// Line 1: "first"
		let (indent, tokens) = &result[0];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("first"));

		// Line 2: "fourth"
		let (indent, tokens) = &result[1];
		assert_eq!(*indent, 0);
		assert_eq!(tokens[0], symbol!("fourth"));
	}

	#[test]
	fn nested_indentation_with_different_tokens() {
		let input = r#"level0
	if condition
		print "hello"
		# comment
next_level0"#;
		let result = lex(input).unwrap();

		assert_eq!(
			result,
			vec![
				(0, vec![symbol!("level0")]),
				(1, vec![symbol!("if"), Item::Whitespace, symbol!("condition")]),
				(2, vec![symbol!("print"), Item::Whitespace, Item::String("hello".to_string())]),
				(2, vec![Item::Comment(" comment".to_string())]),
				(0, vec![symbol!("next_level0")]),
			]
		);
	}

	#[test]
	fn multiple_multiline_constructs() {
		let input = "
first #### comment one
\tindent one
second \"\"\"\"
\tsecond
\tindent string
third";
		let result = lex(input).unwrap();

		assert_eq!(
			result,
			vec![
				(
					0,
					vec![
						symbol!("first"),
						Item::Whitespace,
						Item::Comment(" comment one\nindent one".to_string())
					]
				),
				(
					0,
					vec![
						symbol!("second"),
						Item::Whitespace,
						Item::String("second\nindent string".to_string())
					]
				),
				(0, vec![symbol!("third")]),
			]
		);
	}

	// 	#[test]
	// 	fn continuations() {
	// 		let input = r#"
	// # line continuation can also begin at the start of the next line
	// ::typed-integers:: 0:u8 1:i8 2:i16 3:u16
	// 	\ 4:u32 5:i32 6:u64 7:i64

	// # which comes in handy when we want to continue the parent list
	// 	jim kirk
	// 	commander spock
	// 	hikari sulu
	// 	\ and many more
	// "#
	// 		.trim_start();
	// 		let res = lex(input).unwrap();
	// 		use super::Item::{Backslash, Comment, Symbol, Whitespace};
	// 		// use super::MultilineType::*;
	// 		assert_eq!(
	// 			res,
	// 			vec![
	// 				(0, vec![Comment(" line continuation can also begin at the start of the next line")]),
	// 				(
	// 					0,
	// 					vec![
	// 						Symbol("::typed-integers::"),
	// 						Whitespace,
	// 						Symbol("0:u8"),
	// 						Whitespace,
	// 						Symbol("1:i8"),
	// 						Whitespace,
	// 						Symbol("2:i16"),
	// 						Whitespace,
	// 						Symbol("3:u16")
	// 					]
	// 				),
	// 				(
	// 					1,
	// 					vec![
	// 						Backslash,
	// 						Whitespace,
	// 						Symbol("4:u32"),
	// 						Whitespace,
	// 						Symbol("5:i32"),
	// 						Whitespace,
	// 						Symbol("6:u64"),
	// 						Whitespace,
	// 						Symbol("7:i64")
	// 					]
	// 				),
	// 				(0, vec![Comment(" which comes in handy when we want to continue the parent list")]),
	// 				(1, vec![Symbol("jim"), Whitespace, Symbol("kirk")]),
	// 				(1, [Symbol("commander"), Whitespace, Symbol("spock")]),
	// 				(1, [Symbol("hikari"), Whitespace, Symbol("sulu")]),
	// 				(
	// 					1,
	// 					vec![
	// 						Backslash,
	// 						Whitespace,
	// 						Symbol("and"),
	// 						Whitespace,
	// 						Symbol("many"),
	// 						Whitespace,
	// 						Symbol("more")
	// 					]
	// 				)
	// 			]
	// 		);
	// 	}

	#[test]
	fn error_unclosed_string() {
		let input = "good\nunclosed \"string\nmore";
		let result = lex(input);

		// Expected: error due to unclosed string
		assert!(result.is_err());
	}
}
