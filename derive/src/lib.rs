mod arguments;
mod directives;
mod utils;
mod value;

#[proc_macro_derive(BooleanDirective, attributes(smear))]
pub fn boolean_directive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  directives::boolean::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_derive(BooleanArgument, attributes(smear))]
pub fn boolean_argument(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  arguments::boolean::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_derive(ObjectValue, attributes(smear))]
pub fn object_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  value::object::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}
