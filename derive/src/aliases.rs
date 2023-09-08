use darling::{ast::NestedMeta, Error, FromMeta};
use indexmap::IndexSet;
use quote::ToTokens;

use crate::utils::is_valid_char;

#[derive(Debug, Default)]
pub struct Aliases {
  pub names: Vec<String>,
}

impl FromMeta for Aliases {
  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    if items.is_empty() {
      return Err(
        darling::Error::custom("Expected at least one alias, got `aliases()`")
          .with_span(&proc_macro2::Span::call_site()),
      );
    }

    let mut names = IndexSet::with_capacity(items.len());
    let mut errors = Vec::new();
    for n in items {
      match n {
        NestedMeta::Meta(meta) => {
          let p = meta.require_path_only()?;
          let alias = p.to_token_stream().to_string();
          match validate(&alias).map_err(|e| e.with_span(p)) {
            Ok(_) => {
              names.insert(alias);
            }
            Err(e) => errors.push(e),
          }
        }
        NestedMeta::Lit(lit) => match lit {
          syn::Lit::Str(litstr) => {
            let alias = litstr.value();
            match validate(&alias) {
              Ok(_) => {
                names.insert(alias);
              }
              Err(e) => errors.push(e.with_span(litstr)),
            }
          }
          syn::Lit::Char(ch) => {
            if !ch.value().is_alphabetic() {
              errors.push(
                Error::custom(format!(
                  "A single-character `alias` must be an alphabeta: got '{}'.",
                  ch.value()
                ))
                .with_span(ch),
              );
            } else {
              names.insert(ch.value().to_string());
            }
          }
          lit => errors.push(darling::Error::unexpected_lit_type(lit).with_span(lit)),
        },
      }
    }
    if errors.is_empty() {
      Ok(Self {
        names: names.into_iter().collect(),
      })
    } else {
      Err(Error::multiple(errors))
    }
  }
}

fn validate(alias: &str) -> darling::Result<()> {
  if alias.is_empty() {
    return Err(Error::custom("The `alias` cannot be empty."));
  }

  if alias.len() == 1 {
    let ch = alias.chars().next().unwrap();
    if !ch.is_ascii_alphabetic() {
      return Err(Error::custom(format!(
        "A single-character `alias` must be an alphabeta: got '{}'.",
        ch
      )));
    }
  } else {
    if alias.chars().next().unwrap().is_ascii_digit() {
      return Err(Error::custom(
        "The first character of an `alias` attribute cannot be a number.",
      ));
    }

    for (idx, ch) in alias.char_indices() {
      if !is_valid_char(ch) {
        return Err(Error::custom(format!(
          "The value in `alias` contains an invalid character: '{}({}:{})'.",
          alias, ch, idx
        )));
      }
    }
  }
  Ok(())
}
