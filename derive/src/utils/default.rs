use darling::FromMeta;
use quote::{quote, ToTokens};
use syn::{Expr, Path};

#[derive(FromMeta)]
pub(crate) enum DefaultAttribute {
  None,
  Expr(Expr),
  Path(Path),
}

impl ToTokens for DefaultAttribute {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match self {
      Self::None => tokens.extend(quote! { ::code::default::Default::default() }),
      Self::Expr(expr) => tokens.extend(quote! { { #expr } }),
      Self::Path(p) => tokens.extend(quote! { #p() }),
    }
  }
}
