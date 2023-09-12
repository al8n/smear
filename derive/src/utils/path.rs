use darling::FromMeta;
use quote::{quote, ToTokens};
use syn::Path;

#[derive(FromMeta, Default, Clone)]
pub(crate) enum PathAttribute {
  #[default]
  None,
  Str(String),
  Path(Path),
}

impl PathAttribute {
  pub(crate) fn is_none(&self) -> bool {
    matches!(self, Self::None)
  }

  pub(crate) fn path(&self) -> syn::Result<Option<Path>> {
    match self {
      Self::None => Ok(None),
      Self::Str(s) => syn::parse_str::<Path>(s).map(Some),
      Self::Path(p) => Ok(Some(p.clone())),
    }
  }

  pub(crate) fn to_token_stream(
    &self,
    args: impl ToTokens,
  ) -> syn::Result<proc_macro2::TokenStream> {
    match self {
      Self::None => Ok(quote! {}),
      Self::Str(s) => syn::parse_str::<Path>(s).map(|p| quote! { #p(#args) }),
      Self::Path(p) => Ok(quote! { #p(#args) }),
    }
  }

  pub(crate) fn to_token_stream_with_default_and_custom(
    &self,
    default: impl FnOnce() -> syn::Result<proc_macro2::TokenStream>,
    custom: impl FnOnce(&Path) -> syn::Result<proc_macro2::TokenStream>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    match self {
      Self::None => default(),
      Self::Str(s) => custom(&syn::parse_str::<Path>(s)?),
      Self::Path(p) => custom(p),
    }
  }

  pub(crate) fn to_token_stream_with_custom(
    &self,
    custom: impl FnOnce(&Path) -> syn::Result<proc_macro2::TokenStream>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    match self {
      Self::None => Ok(quote! {}),
      Self::Str(s) => custom(&syn::parse_str::<Path>(s)?),
      Self::Path(p) => custom(p),
    }
  }

  pub(crate) fn to_token_stream_with_default(
    &self,
    args: impl ToTokens,
    default: impl FnOnce() -> syn::Result<proc_macro2::TokenStream>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    match self {
      Self::None => default(),
      Self::Str(s) => syn::parse_str::<Path>(s).map(|p| quote! { #p(#args) }),
      Self::Path(p) => Ok(quote! { #p(#args) }),
    }
  }
}
