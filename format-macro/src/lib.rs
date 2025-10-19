use proc_macro::TokenStream;

mod format_match;
mod listify;

#[proc_macro]
pub fn listify(input: TokenStream) -> TokenStream {
	listify::listify(input.into()).into()
}

#[proc_macro]
pub fn format_matcher(input: TokenStream) -> TokenStream {
	format_match::format_matcher_impl(input.into()).into()
}
