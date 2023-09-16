use darling::{FromMeta, ast::NestedMeta};
use quote::{quote, ToTokens};
use syn::{Expr, Path};

#[derive(Clone)]
enum Value {
  Path(Path),
  Expr(Expr),
}

impl ToTokens for DefaultAttribute {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match &self.0 {
      None => tokens.extend(quote! { ::code::default::Default::default() }),
      Some(Value::Expr(expr)) => tokens.extend(quote! { { #expr } }),
      Some(Value::Path(p)) => tokens.extend(quote! { #p() }),
    }
  }
}

#[derive(Default, Clone)]
pub(crate) struct DefaultAttribute(Option<Value>);

impl FromMeta for DefaultAttribute {
  fn from_word() -> darling::Result<Self> {
    Ok(Self(None))
  }

  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    match items.len() {
      0 => Err(darling::Error::too_few_items(1)),
      1 => {
        match &items[0] {
          NestedMeta::Lit(lit) => {
            if let syn::Lit::Str(s) = &lit {
              Ok(Self(Some(Value::Path(syn::parse_str(&s.value())?))))
            } else {
              Err(darling::Error::unexpected_lit_type(lit))
            }
          },
          NestedMeta::Meta(meta) => Self::from_meta(meta),
        }
      },
      _ => Err(darling::Error::too_many_items(1)),
    }
  }

  fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
    match expr {
      Expr::Lit(lit) => {
        if let syn::Lit::Str(s) = &lit.lit {
          Ok(Self(Some(Value::Path(syn::parse_str(&s.value())?))))
        } else {
          Err(darling::Error::unexpected_lit_type(&lit.lit))
        }
      }
      Expr::Path(path) => Ok(Self(Some(Value::Path(path.path.clone())))),
      expr => Ok(Self(Some(Value::Expr(expr.clone())))),
    }
  }
}
