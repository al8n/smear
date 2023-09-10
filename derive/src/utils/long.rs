use darling::FromMeta;
use syn::Expr;

use crate::utils::is_valid_char;

#[derive(Debug, Default, Clone)]
pub struct Long(pub(crate) Option<String>);

impl FromMeta for Long {
  fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
    match item {
      syn::Meta::Path(_) => Ok(Self(None)),
      syn::Meta::List(_) => Err(
        darling::Error::custom("expected a value for this attribute: `long` or `long = \"...\"`")
          .with_span(item),
      ),
      syn::Meta::NameValue(item) => {
        if let Expr::Lit(lit) = &item.value {
          if let syn::Lit::Str(s) = &lit.lit {
            let s = s.value();
            validate_long(&s).map_err(|e| e.with_span(&lit.lit))?;
            Ok(Long(Some(s)))
          } else {
            Err(darling::Error::unexpected_lit_type(&lit.lit).with_span(lit))
          }
        } else {
          Err(darling::Error::unexpected_expr_type(&item.value).with_span(&item))
        }
      }
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
