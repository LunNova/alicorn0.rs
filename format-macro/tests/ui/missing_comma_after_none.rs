use format_macro::format_matcher;
use alicorn_format::{Element, FormatList};

fn main() {
    let list: FormatList = vec![
        Element::Symbol("foo".into()),
        Element::Number(1.0)
    ].into_iter().collect();

    format_matcher! {
        match list {
            (foo, ~x~) => None
            (bar, ~y~) => Some(y)
        }
    }
}
