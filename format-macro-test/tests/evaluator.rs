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
					if let Element::List(body_list) = body {
						// If the body is a single expression, evaluate it directly
						// Otherwise evaluate as a sequence
						let result = if body_list.len() == 1 {
							eval(&body_list[0], &new_env)
						} else {
							eval(&Element::List(body_list.clone()), &new_env)
						};
						Some(result)
					} else {
						None
					}
				} else {
					None
				}
			},
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
						if let Element::Symbol(p) = param {
							make_closure(p.as_str(), body.clone(), env)
						} else {
							Element::List(list.clone())
						}
					},
					// Varargs function application: (func args...)
					(~func~, ~args...~) => {
						let f = eval(func, env);

						// Evaluate all args
						let evaluated_args: Vector<Element> = args.iter()
							.map(|a| eval(a, env))
							.collect();

						// Check for builtin operators
						if let Element::Symbol(s) = &f {
							match s.as_str() {
								"+" => {
									if evaluated_args.len() == 2 {
										if let (Element::Number(l), Element::Number(r)) = (&evaluated_args[0], &evaluated_args[1]) {
											return Element::Number(l + r);
										}
									}
									return Element::List(list.clone());
								}
								_ => {}
							}
						}

						// Try applying as closure (use first arg)
						if let Element::List(closure_list) = &f {
							if !evaluated_args.is_empty() {
								if let Some(result) = apply_closure(closure_list, &evaluated_args[0]) {
									return result;
								}
							}
						}
						Element::List(list.clone())
					},
					_ => Element::List(list.clone())
				}
			}
		}
		_ => expr.clone(),
	}
}

#[test]
fn test_listify_arrow() {
	let lam = listify!(x -> + x 1);
	println!("{lam:#?}");

	let expected = vector![Element::List(vector![
		Element::Symbol("x".into()),
		Element::Symbol("->".into()),
		Element::Symbol("+".into()),
		Element::Symbol("x".into()),
		Element::Number(1.0),
	])];

	assert_eq!(lam, expected);
}

#[test]
fn test_lambda_application() {
	// Test: make a lambda x -> (+ x 1) and apply it to 4
	let lam = listify!(x -> + x 1);
	let lam_elem = lam.get(0).unwrap();

	// Build application: (lam, 4)
	let app = Element::List(vector![lam_elem.clone(), Element::Number(4.0)]);

	let env = HashMap::new();
	let result = eval(&app, &env);

	assert_eq!(result, Element::Number(5.0), "Lambda application should compute (+ 4 1) = 5");
}

#[test]
fn test_let_binding() {
	// Test: let x = 4 in (+ x 1)
	let let_expr = listify!(x = 4 in + x 1);
	let let_elem = let_expr.get(0).unwrap();

	let env = HashMap::new();
	let result = eval(let_elem, &env);

	assert_eq!(result, Element::Number(5.0), "Let binding should compute (+ 4 1) = 5");
}
