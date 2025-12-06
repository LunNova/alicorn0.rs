use format_macro::format_matcher;
use alicorn_format::{Element, FormatList};

fn main() {
    let list: FormatList = vec![Element::Number(1.0)].into_iter().collect();

    format_matcher! {
        match list {
            (~x~) => None,
            (~y~) => Some(y),
            _ => None
        }
    };
}
