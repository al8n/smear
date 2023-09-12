use darling::FromMeta;
use syn::Expr;

#[derive(Debug, Default, Clone)]
pub struct Short(pub(crate) Option<Option<char>>);

impl FromMeta for Short {
  fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
    match item {
      syn::Meta::Path(_) => Ok(Self(Some(None))),
      syn::Meta::List(l) => Err(
        darling::Error::custom("expected a value for this attribute: `short` or `short = '...'`")
          .with_span(l),
      ),
      syn::Meta::NameValue(nv) => {
        if let Expr::Lit(lit) = &nv.value {
          if let syn::Lit::Char(char_lit) = &lit.lit {
            let char = char_lit.value();
            validate_short(char).map_err(|e| e.with_span(char_lit))?;
            Ok(Short(Some(Some(char))))
          } else {
            Err(darling::Error::unexpected_lit_type(&lit.lit).with_span(lit))
          }
        } else {
          Err(darling::Error::unexpected_expr_type(&nv.value).with_span(&nv))
        }
      }
    }
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
    return Err(darling::Error::custom(format!(
      "The value of `short` must be an ASCII alphabetic character, but got '{short}'."
    )));
  }
  Ok(())
}
