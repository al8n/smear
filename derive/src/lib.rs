mod definitions;
mod directive;
mod utils;
mod value;

// ================================= Directive Macros ================================
#[proc_macro_derive(Directive, attributes(smear))]
pub fn directive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as syn::DeriveInput);

  directive::derive(input)
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

#[cfg(feature = "indexmap")]
#[proc_macro_attribute]
pub fn indexmap(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::map::map(smear_types::value::MapKind::IndexMap, args, input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_attribute]
pub fn hashmap(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::map::map(smear_types::value::MapKind::HashMap, args, input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_attribute]
pub fn btreemap(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::map::map(smear_types::value::MapKind::BTreeMap, args, input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[cfg(feature = "indexmap")]
#[proc_macro_attribute]
pub fn indexset(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::set::set(smear_types::value::SetKind::IndexSet, args, input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_attribute]
pub fn hashset(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::set::set(smear_types::value::SetKind::HashSet, args, input)
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

#[proc_macro_attribute]
pub fn btreeset(
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  value::set::set(smear_types::value::SetKind::BTreeSet, args, input)
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
