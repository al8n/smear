use darling::FromDeriveInput;
use quote::quote;

use syn::{DeriveInput, Ident};

pub fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  Ok(quote! {})
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named))]
struct Map {
  ident: Ident,
  key: String,
  value: String,
}
