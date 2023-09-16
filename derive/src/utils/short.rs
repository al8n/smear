use darling::{ast::NestedMeta, Error, FromMeta};
use syn::{Expr, Lit};

#[derive(Debug, Default, Clone)]
pub struct Short(pub(crate) Option<Option<char>>);

impl FromMeta for Short {
  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    match items.len() {
      0 => Err(Error::too_few_items(1)),
      1 => match &items[0] {
        NestedMeta::Lit(syn::Lit::Char(s)) => Ok(Self(Some(Some(s.value())))),
        NestedMeta::Lit(lit) => Err(Error::unexpected_lit_type(lit)),
        NestedMeta::Meta(meta) => Self::from_meta(meta),
      },
      _ => Err(Error::too_many_items(1)),
    }
  }

  fn from_word() -> darling::Result<Self> {
    Ok(Self(Some(None)))
  }

  fn from_expr(expr: &Expr) -> darling::Result<Self> {
    match expr {
      Expr::Lit(lit) => match &lit.lit {
        Lit::Char(ch) => Ok(Self(Some(Some(ch.value())))),
        lit => Err(Error::unexpected_lit_type(lit)),
      },
      expr => Err(Error::unexpected_expr_type(expr)),
    }
  }

  fn from_char(value: char) -> darling::Result<Self> {
    Ok(Self(Some(Some(value))))
  }
}

impl Short {
  pub fn validate(&self, name: &str) -> syn::Result<()> {
    if let Some(Some(short_char)) = self.0 {
      validate_short(short_char)?;
    } else if let Some(None) = self.0 {
      if !name.chars().next().unwrap().is_ascii_alphabetic() {
        return Err(syn::Error::new_spanned(name, "The `short` is not set and the first character of the name is not an ASCII alphabetic character. Please set `short` manually."));
      }
    }
    Ok(())
  }
}

fn validate_short(short: char) -> darling::Result<()> {
  if !short.is_ascii_alphabetic() {
    return Err(Error::custom(format!(
      "The value of `short` must be an ASCII alphabetic character, but got '{short}'."
    )));
  }
  Ok(())
}
