use alicorn_format::Element;
use format_macro::{format_matcher, listify};
use imbl::Vector;

fn main() {
	// Test basic function call syntax
	let result = listify!(foo(bar, baz));
	println!("Result: {:#?}", result);

	// Test with operators
	let result2: Vector<Element> = listify!(a + b * c);
	println!("\nResult2: {:#?}", result2);

	// Test nested
	let result3 = listify!(print("hello", a + b));
	println!("\nResult3: {:#?}", result3);

	// Test square brackets
	let result4 = listify!([1, 2, 3]);
	println!("\nResult4: {:#?}", result4);

	// Test format_matcher with guillemets
	let input = listify!(lambda(x, y, body));

	// input is a Vector with one List element, extract it
	let input_list = if let Some(alicorn_format::Element::List(list)) = input.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let result = format_matcher! {
		match input_list {
			lambda(~name~, ~args...~, ~body~) => {
				println!("Matched lambda!");
				println!("name: {:?}", name);
				println!("args: {:?}", args);
				println!("body: {:?}", body);
				true
			},
			_ => {
				println!("No match");
				false
			}
		}
	};

	println!("Matcher result: {}", result);
}

#[test]
fn test_let_pattern() {
	let let_expr = listify!(x = 42 in x + 1);
	let let_list = if let Some(alicorn_format::Element::List(list)) = let_expr.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match let_list {
			(~name~, =, ~expr~, in, ~body~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				assert!(matches!(expr, alicorn_format::Element::Number(42.0)));
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match let pattern");
}

#[test]
fn test_lambda_simple() {
	let lam = listify!(f -> x + 1);
	let lam_list = if let Some(alicorn_format::Element::List(list)) = lam.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match lam_list {
			(~name~, ->, ~body...~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				println!("Body: {:?}", body);
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match simple lambda");
}

#[test]
fn test_lambda_typed() {
	let lam = listify!(f : Int -> x + 1);
	let lam_list = if let Some(alicorn_format::Element::List(list)) = lam.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match lam_list {
			(~name~, :, ~annotation~, ->, ~body~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				assert!(matches!(annotation, alicorn_format::Element::Symbol(_)));
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match typed lambda");
}

#[test]
fn test_lambda_bounded() {
	let lam = listify!(f < T : Trait -> x + 1);
	let lam_list = if let Some(alicorn_format::Element::List(list)) = lam.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match lam_list {
			(~name~, <, ~bound~, :, ~annotation~, ->, ~body~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				assert!(matches!(bound, alicorn_format::Element::Symbol(_)));
				assert!(matches!(annotation, alicorn_format::Element::Symbol(_)));
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match bounded typed lambda");
}

#[test]
fn test_forall_simple() {
	let forall = listify!(T : Type -> T);
	let forall_list = if let Some(alicorn_format::Element::List(list)) = forall.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match forall_list {
			(~name~, :, ~annotation~, ->, ~result~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				assert!(matches!(annotation, alicorn_format::Element::Symbol(_)));
				assert!(matches!(result, alicorn_format::Element::Symbol(_)));
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match forall pattern");
}

#[test]
fn test_forall_bounded() {
	let forall = listify!(T < U : Trait -> T);
	let forall_list = if let Some(alicorn_format::Element::List(list)) = forall.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	let matched = format_matcher! {
		match forall_list {
			(~name~, <, ~bound~, :, ~annotation~, ->, ~result~) => {
				assert!(matches!(name, alicorn_format::Element::Symbol(_)));
				assert!(matches!(bound, alicorn_format::Element::Symbol(_)));
				assert!(matches!(annotation, alicorn_format::Element::Symbol(_)));
				assert!(matches!(result, alicorn_format::Element::Symbol(_)));
				true
			},
			_ => false
		}
	};
	assert!(matched, "Should match bounded forall pattern");
}
