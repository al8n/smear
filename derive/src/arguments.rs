
mod field;
pub(crate) use field::ArgumentField;
mod struct_;
pub(crate) use struct_::Argument;

use darling::FromDeriveInput;

pub(crate) fn derive(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let argument  = Argument::from_derive_input(&input)?;
  todo!()
}

