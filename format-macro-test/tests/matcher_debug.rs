use alicorn_format::Element;
use format_macro::format_matcher;
use std::collections::HashMap;

type Env = HashMap<String, Element>;

fn eval(_expr: &Element, _env: &Env) -> Element {
	Element::Number(0.0)
}

fn try_apply_lambda(lam_list: &imbl::Vector<Element>, arg: &Element, env: &Env) -> Option<Element> {
	format_matcher! {
		match lam_list {
			(~param~, ->, ~body~) => {
				// Apply lambda
				let mut new_env = env.clone();
				if let Element::Symbol(p) = param {
					new_env.insert(p.as_str().to_string(), arg.clone());
				}
				Some(eval(body, &new_env))
			},
			_ => None
		}
	}
}

#[test]
fn try_apply() {
	use imbl::vector;

	// Manually construct: (x -> body)
	let lam_list = vector![
		Element::Symbol(ustr::Ustr::from("x")),
		Element::Symbol(ustr::Ustr::from("->")),
		Element::Symbol(ustr::Ustr::from("body")),
	];

	let arg = Element::Number(42.0);
	let env = std::collections::HashMap::new();

	println!("lam_list: {:?}", lam_list);
	let result = try_apply_lambda(&lam_list, &arg, &env);
	println!("result: {:?}", result);
	assert!(result.is_some());
}
