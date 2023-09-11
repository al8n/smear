mod arguments;
mod definitions;
mod directives;
mod utils;
mod value;

// ================================= Directive Macros ================================
#[proc_macro_derive(Directive, attributes(smear))]
pub fn directive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  directives::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

// ================================= Argument Macros =================================
#[proc_macro_derive(Argument, attributes(smear))]
pub fn argument(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  arguments::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

// ================================== Value Macros ==================================

#[proc_macro_derive(ObjectValue, attributes(smear))]
pub fn object_value(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  value::object::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

// ================================= Definition Macros ===============================

#[proc_macro_derive(ObjectDefinition, attributes(smear))]
pub fn object_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  definitions::object::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_derive(EnumDefinition, attributes(smear))]
pub fn enum_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  definitions::enum_::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_derive(InterfaceDefinition, attributes(smear))]
pub fn interface_definition(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  definitions::interface::derive(input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}
