// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Fundament Software SPC <https://fundament.software>

//! Alicorn runner - execute .alc files

use std::env;
use std::fs;
use std::process::ExitCode;

fn main() -> ExitCode {
	let args: Vec<String> = env::args().collect();

	if args.len() < 2 {
		eprintln!("Usage: {} <file.alc>", args[0]);
		eprintln!("       {} -e '<expression>'", args[0]);
		return ExitCode::from(1);
	}

	let input = if args[1] == "-e" {
		if args.len() < 3 {
			eprintln!("Error: -e requires an expression");
			return ExitCode::from(1);
		}
		args[2].clone()
	} else {
		match fs::read_to_string(&args[1]) {
			Ok(contents) => contents,
			Err(e) => {
				eprintln!("Error reading {}: {}", args[1], e);
				return ExitCode::from(1);
			}
		}
	};

	match alicorn_termify::run_file(&input) {
		Ok(value) => {
			println!("{:?}", value);
			ExitCode::SUCCESS
		}
		Err(e) => {
			eprintln!("Error: {}", e);
			ExitCode::from(1)
		}
	}
}
