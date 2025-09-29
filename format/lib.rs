#![allow(dead_code)]
use imbl::Vector;
use std::fmt;
use ustr::Ustr;

/// lexer is responsible for turning an alicorn file into
/// a vec of lexed lines. a lexed line is a (u8, Vec<Token>) where the u8 is the indent level
/// The lexer skips empty lines and has already handled multiline comments and strings
/// by merging them into a single token
mod lexer;
mod listify;

use lexer::BraceType;

#[derive(Debug, Clone, PartialEq)]
pub enum Element {
	Symbol(Ustr),
	Number(f64),
	List(FormatList),
	Comment(String),
	String(String),
}

impl fmt::Display for BraceType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			BraceType::Paren => write!(f, ""), // No marker for parens
			BraceType::Square => write!(f, "square-list"),
			BraceType::Curly => write!(f, "curly-list"),
		}
	}
}

pub type Indent = i16;
pub type FormatList = Vec<Element>;

#[derive(Debug, Clone, PartialEq)]
pub enum ImmElement {
	Symbol(Ustr),
	Number(f64),
	List(ImmFormatList),
	Comment(String),
	String(String),
}

pub type ImmFormatList = Vector<ImmElement>;

pub use listify::ListifyError;

/// Parse Alicorn format text into nested lists.
///
/// This is the main entry point for converting Alicorn's indentation-sensitive
/// format into a nested list structure (similar to S-expressions).
///
/// The process is: input text → lexing → listification → nested lists
pub fn format(input: &str) -> Result<FormatList, Box<dyn std::error::Error>> {
	let lexed = lexer::lex(input).map_err(|_| "Lexing failed")?;
	let formatted = listify::listify(lexed)?;
	Ok(formatted)
}

/// Convert a Vec-based Element tree to an imbl-based ImmElement tree
pub fn immutify(elements: Vec<Element>) -> ImmFormatList {
	elements.into_iter().map(immutify_element).collect()
}

fn immutify_element(element: Element) -> ImmElement {
	match element {
		Element::Symbol(s) => ImmElement::Symbol(s),
		Element::Number(n) => ImmElement::Number(n),
		Element::List(list) => ImmElement::List(immutify(list)),
		Element::Comment(c) => ImmElement::Comment(c),
		Element::String(s) => ImmElement::String(s),
	}
}
