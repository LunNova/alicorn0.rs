use alicorn_format::Element;
use format_macro::{format_matcher, listify};
use imbl::{Vector, vector};
use std::collections::HashMap;

type Env = HashMap<String, Element>;

fn env_to_element(env: &Env) -> Element {
	let pairs: Vector<Element> = env
		.iter()
		.map(|(k, v)| Element::List(vector![Element::Symbol(k.as_str().into()), v.clone()]))
		.collect();
	Element::List(pairs)
}

fn element_to_env(elem: &Element) -> Env {
	let mut env = HashMap::new();
	if let Element::List(pairs) = elem {
		for pair in pairs.iter() {
			if let Element::List(kv) = pair {
				if kv.len() == 2 {
					if let (Element::Symbol(k), v) = (&kv[0], &kv[1]) {
						env.insert(k.to_string(), v.clone());
					}
				}
			}
		}
	}
	env
}

fn make_closure(param: &str, body: Vector<Element>, env: &Env) -> Element {
	Element::List(vector![
		Element::Symbol("#".into()),
		Element::Symbol("closure".into()),
		Element::Symbol(param.into()),
		Element::List(body),
		env_to_element(env)
	])
}

fn apply_closure(closure: &Vector<Element>, arg: &Element) -> Option<Element> {
	format_matcher! {
		match closure {
			(#closure, ~param~, ~body~, ~env~) => {
				if let Element::Symbol(p) = param {
					let mut new_env = element_to_env(env);
					new_env.insert(p.to_string(), arg.clone());
					eprintln!("Applying closure with env: {:?}", new_env);
					if let Element::List(body_list) = body {
						let result = eval(&Element::List(body_list.clone()), &new_env);
						eprintln!("Body eval result: {:?}", result);
						Some(result)
					} else {
						None
					}
				} else {
					None
				}
			}
			_ => None
		}
	}
}

fn eval(expr: &Element, env: &Env) -> Element {
	match expr {
		Element::Number(n) => Element::Number(*n),
		Element::Symbol(s) => {
			// Look up variable in environment
			env.get(s.as_str()).cloned().unwrap_or_else(|| expr.clone())
		}
		Element::List(list) => {
			// Check for special forms
			format_matcher! {
				match list {
					// Let binding: x = expr in body
					(~name~, =, ~expr~, in, ~body...~) => {
						let val = eval(expr, env);
						let mut new_env = env.clone();
						if let Element::Symbol(s) = name {
							new_env.insert(s.as_str().to_string(), val);
						}
						eval(&Element::List(body.clone()), &new_env)
					},
					// Lambda: x -> body (create closure)
					(~param~, ->, ~body...~) => {
						eprintln!("MATCHED LAMBDA PATTERN!");
						if let Element::Symbol(p) = param {
							let closure = make_closure(p.as_str(), body.clone(), env);
							eprintln!("Created closure: {:?}", closure);
							closure
						} else {
							Element::List(list.clone())
						}
					},
					// Function application or operators
					(~op~, ~left~, ~right~) => {
						if let Element::Symbol(s) = op {
							match s.as_str() {
								"+" => {
									let l = eval(left, env);
									let r = eval(right, env);
									if let (Element::Number(ln), Element::Number(rn)) = (l, r) {
										Element::Number(ln + rn)
									} else {
										Element::List(list.clone())
									}
								}
								_ => Element::List(list.clone())
							}
						} else {
							Element::List(list.clone())
						}
					},
					// Two-element function application: (func arg)
					(~func~, ~arg~) => {
						eprintln!("MATCHED 2-ELEMENT APPLICATION!");
						let f = eval(func, env);
						eprintln!("Evaluated func to: {:?}", f);
						let a = eval(arg, env);
						eprintln!("Evaluated arg to: {:?}", a);
						if let Element::List(closure_list) = &f {
							eprintln!("It's a list, trying to apply as closure");
							if let Some(result) = apply_closure(closure_list, &a) {
								eprintln!("Application succeeded! Result: {:?}", result);
								return result;
							} else {
								eprintln!("apply_closure returned None");
							}
						}
						Element::List(list.clone())
					},
					_ => { Element::List(list.clone()) }
				}
			}
		}
		_ => expr.clone(),
	}
}

#[test]
fn test_listify_arrow() {
	// First, let's see what listify! actually produces for ->
	let lam = listify!(x -> + x 1);

	// Check the structure
	assert_eq!(lam.len(), 1, "Should have one top-level element");

	let list = if let Some(Element::List(list)) = lam.get(0) {
		list
	} else {
		panic!("Expected a list");
	};

	// Print what we got
	println!("Lambda structure:");
	for (i, elem) in list.iter().enumerate() {
		println!("  [{}]: {:?}", i, elem);
	}

	// Assert on the actual structure
	assert_eq!(list.len(), 5, "Should have 5 elements: x, ->, +, x, 1");
	assert!(matches!(list[0], Element::Symbol(_)), "First should be symbol 'x'");
	assert!(matches!(list[1], Element::Symbol(_)), "Second should be symbol '->'");
}

#[test]
fn test_combined_evaluator() {
	// Test: make a lambda x -> (+ x 1) and apply it to 4
	let lam = listify!(x -> + x 1);
	let lam_elem = lam.get(0).unwrap();

	// Build application: (lam, 4)
	let app = Element::List(vector![lam_elem.clone(), Element::Number(4.0)]);

	let env = HashMap::new();
	let result = eval(&app, &env);

	assert_eq!(result, Element::Number(5.0), "Lambda application should compute (+ 4 1) = 5");

	// Test: let x = 4 in (+ x 1)
	let let_expr = listify!(x = 4 in + x 1);
	let let_elem = let_expr.get(0).unwrap();
	let result = eval(let_elem, &env);
	assert_eq!(result, Element::Number(5.0), "Let binding should compute (+ 4 1) = 5");
}
