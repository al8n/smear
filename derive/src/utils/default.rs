use darling::{ast::NestedMeta, Error, FromMeta};
use quote::{quote, ToTokens};
use syn::{Expr, Lit, Path};

#[derive(Clone)]
enum Value {
  Path(Path),
  Expr(Expr),
  Lit(Lit),
}

impl ToTokens for DefaultAttribute {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match &self.0 {
      None => tokens.extend(quote! { ::core::default::Default::default() }),
      Some(Value::Expr(expr)) => tokens.extend(quote! { { #expr } }),
      Some(Value::Path(p)) => tokens.extend(quote! { #p() }),
      Some(Value::Lit(l)) => tokens.extend(quote! { #l }),
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
      0 => Err(Error::too_few_items(1)),
      1 => match &items[0] {
        NestedMeta::Lit(lit) => Self::from_value(lit),
        NestedMeta::Meta(meta) => Self::from_meta(meta),
      },
      _ => Err(Error::too_many_items(1)),
    }
  }

  fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
    match expr {
      Expr::Lit(lit) => Self::from_value(&lit.lit),
      Expr::Path(path) => Ok(Self(Some(Value::Path(path.path.clone())))),
      expr => Ok(Self(Some(Value::Expr(expr.clone())))),
    }
  }

  fn from_value(value: &Lit) -> darling::Result<Self> {
    Ok(Self(Some(Value::Lit(value.clone()))))
  }
}
