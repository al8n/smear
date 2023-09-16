use darling::{ast::NestedMeta, util::path_to_string, Error, FromMeta};
use syn::Expr;

use crate::utils::is_valid_char;

#[derive(Debug, Default, Clone)]
pub struct Long(pub(crate) Option<String>);

impl FromMeta for Long {
  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    match items.len() {
      0 => Err(Error::too_few_items(1)),
      1 => match &items[0] {
        NestedMeta::Lit(syn::Lit::Str(s)) => Ok(Self(Some(s.value()))),
        NestedMeta::Lit(lit) => Err(Error::unexpected_lit_type(lit)),
        NestedMeta::Meta(meta) => Self::from_meta(meta),
      },
      _ => Err(Error::too_many_items(1)),
    }
  }

  fn from_word() -> darling::Result<Self> {
    Ok(Self(None))
  }

  fn from_expr(expr: &Expr) -> darling::Result<Self> {
    match expr {
      Expr::Lit(lit) => match &lit.lit {
        syn::Lit::Str(s) => Ok(Self(Some(s.value()))),
        lit => Err(Error::unexpected_lit_type(lit)),
      },
      Expr::Path(p) => Ok(Self(Some(path_to_string(&p.path)))),
      expr => Err(Error::unexpected_expr_type(expr)),
    }
  }
}

impl Long {
  pub fn validate(&self, default: &str) -> syn::Result<()> {
    if self.0.is_some() {
      Ok(())
    } else {
      validate_long(default)?;
      Ok(())
    }
  }
}

pub(crate) fn validate_long(long: &str) -> darling::Result<()> {
  if long.is_empty() {
    return Err(darling::Error::custom("The `long` cannot be empty."));
  }

  if long.len() == 1 {
    return Err(darling::Error::custom("The length of `long` must be larger than 1. If you want single character name, please set `short` instead."));
  }

  let ch = long.chars().next().unwrap();
  if ch.is_ascii_digit() {
    return Err(darling::Error::custom(format!(
      "The first character of the `long` must be an ASCII alphabetic character or '_': got '{}'.",
      ch
    )));
  }

  for (idx, ch) in long.char_indices() {
    if !is_valid_char(ch) {
      return Err(darling::Error::custom(format!(
        "The value of `long` contains an invalid character: '{}({}:{})'.",
        long, ch, idx
      )));
    }
  }

  Ok(())
}
