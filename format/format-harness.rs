use argh::FromArgs;
use std::fs;
use std::io::{self, Read};

use alicorn_format::{Element, FormatList, format};

#[derive(FromArgs)]
/// Test harness for running input strings through the Rust format implementation
struct Args {
	#[argh(option, short = 'f')]
	/// read input from file
	file: Option<String>,

	#[argh(option, short = 'i')]
	/// parse the given string
	input: Option<String>,

	#[argh(switch, short = 'r')]
	/// output raw nested list structure with metadata
	raw: bool,

	#[argh(switch, short = 'c')]
	/// remove comments from output
	remove_comments: bool,
}

fn format_lua_style(list: &FormatList, remove_comments: bool) -> String {
	let mut result = String::new();
	if list.is_empty() {
		result.push_str("{ }");
		return result;
	}
	result.push_str("{ ");

	let mut first = true;
	for element in list {
		if let Some(formatted) = format_element(element, remove_comments) {
			if !first {
				result.push_str(", ");
			}
			result.push_str(&formatted);
			first = false;
		}
	}

	result.push_str(" }");
	result
}

fn format_element(element: &Element, remove_comments: bool) -> Option<String> {
	match element {
		Element::Symbol(s) => Some(format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\""))),
		Element::Number(n) => {
			if n.fract() == 0.0 && *n >= i64::MIN as f64 && *n <= i64::MAX as f64 {
				Some(format!("{}", *n as i64))
			} else {
				Some(format!("{}", n))
			}
		}
		Element::List(list) => Some(format_lua_style(list, remove_comments)),
		Element::Comment(c) => {
			if remove_comments {
				None
			} else {
				let escaped = c
					.replace("\\", "\\\\")
					.replace("\"", "\\\"")
					.replace("\n", "\\n")
					.replace("\t", "\\t");
				Some(format!("\"{}\"", escaped))
			}
		}
		Element::String(s) => {
			let escaped = s
				.replace("\\", "\\\\")
				.replace("\"", "\\\"")
				.replace("\n", "\\n")
				.replace("\t", "\\t");
			Some(format!("{{ elements = {{ \"{}\" }}, kind = \"string\" }}", escaped))
		}
	}
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let args: Args = argh::from_env();

	let input = if let Some(filename) = args.file {
		fs::read_to_string(&filename).map_err(|e| format!("Could not read file '{}': {}", filename, e))?
	} else if let Some(input_string) = args.input {
		input_string
	} else {
		// Read from stdin
		let mut buffer = String::new();
		io::stdin().read_to_string(&mut buffer)?;
		buffer
	};

	// Format the input
	let result = format(&input)?;

	// Output the result
	if args.raw {
		println!("{:#?}", result);
	} else {
		let formatted = format_lua_style(&result, args.remove_comments);
		println!("{}", formatted);
	}

	Ok(())
}
