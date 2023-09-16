use darling::{ast::NestedMeta, FromMeta};
use syn::{Expr, Path};

#[derive(Default, Clone)]
pub(crate) struct PathAttribute(Option<Path>);

impl PathAttribute {
  pub fn path(&self) -> Option<Path> {
    self.0.clone()
  }
}

impl FromMeta for PathAttribute {
  fn from_word() -> darling::Result<Self> {
    Ok(Self(None))
  }

  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    match items.len() {
      0 => Err(darling::Error::too_few_items(1)),
      1 => match &items[0] {
        NestedMeta::Lit(lit) => {
          if let syn::Lit::Str(s) = &lit {
            Ok(Self(Some(syn::parse_str(&s.value())?)))
          } else {
            Err(darling::Error::unexpected_lit_type(lit))
          }
        }
        NestedMeta::Meta(meta) => Self::from_meta(meta),
      },
      _ => Err(darling::Error::too_many_items(1)),
    }
  }

  fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
    match expr {
      Expr::Lit(lit) => {
        if let syn::Lit::Str(s) = &lit.lit {
          Ok(Self(Some(syn::parse_str(&s.value())?)))
        } else {
          Err(darling::Error::unexpected_lit_type(&lit.lit))
        }
      }
      Expr::Path(path) => Ok(Self(Some(path.path.clone()))),
      expr => Err(darling::Error::unexpected_expr_type(expr)),
    }
  }
}
