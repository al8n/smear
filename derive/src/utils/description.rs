use quote::{quote, ToTokens};

#[derive(Default)]
pub struct Description(Option<String>);

impl darling::FromMeta for Description {
  fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
    if let syn::Expr::Lit(lit) = expr {
      if let syn::Lit::Str(s) = &lit.lit {
        Ok(Self(Some(s.value())))
      } else {
        Err(darling::Error::unexpected_lit_type(&lit.lit))
      }
    } else {
      Err(darling::Error::unexpected_expr_type(expr))
    }
  }
}

impl Description {
  pub fn to_tokens(&self) -> proc_macro2::TokenStream {
    match &self.0 {
      Some(d) => quote!(::core::option::Option::Some(#d)),
      None => quote!(::core::option::Option::None),
    }
  }
}

impl ToTokens for Description {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    tokens.extend(self.to_tokens())
  }
}
