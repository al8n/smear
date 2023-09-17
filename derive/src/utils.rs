mod aliases;
mod attributes;
mod default;
mod deprecated;
mod description;
mod directive;
mod long;
mod name;
mod optional;
mod path;
mod short;
mod ty;

pub(crate) use aliases::*;
pub(crate) use attributes::*;
pub(crate) use default::*;
pub(crate) use deprecated::*;
pub(crate) use description::*;
pub(crate) use directive::*;
pub(crate) use long::*;
pub(crate) use name::*;
pub(crate) use optional::*;
pub(crate) use path::*;
pub(crate) use short::*;
pub(crate) use ty::*;

// Utility function to check if a character is valid
pub(crate) fn is_valid_char(ch: char) -> bool {
  ch.is_ascii_alphabetic() || ch == '_' || ch.is_ascii_digit()
}

pub(crate) struct DisplayPath<'a>(pub &'a syn::Path);

impl<'a> core::fmt::Display for DisplayPath<'a> {
  fn fmt(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
    for (i, segment) in self.0.segments.iter().enumerate() {
      if i > 0 || self.0.leading_colon.is_some() {
        formatter.write_str("::")?;
      }
      write!(formatter, "{}", segment.ident)?;
    }
    Ok(())
  }
}

use quote::format_ident;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SafeIdent {
  safe: Option<syn::Ident>,
  ident: syn::Ident,
}

impl SafeIdent {
  pub fn with_safe(ident: syn::Ident, safe: syn::Ident) -> Self {
    Self {
      safe: Some(safe),
      ident,
    }
  }

  pub fn safe(&self) -> &syn::Ident {
    self.safe.as_ref().unwrap_or(&self.ident)
  }

  pub fn ident(&self) -> &syn::Ident {
    &self.ident
  }
}

impl quote::ToTokens for SafeIdent {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    self.safe().to_tokens(tokens)
  }
}

impl From<syn::Ident> for SafeIdent {
  fn from(ident: syn::Ident) -> Self {
    Self::from(&ident)
  }
}

impl From<&syn::Ident> for SafeIdent {
  fn from(ident: &syn::Ident) -> Self {
    Self::from(ident.to_string())
  }
}

impl From<String> for SafeIdent {
  fn from(ident: String) -> Self {
    Self::from(ident.as_str())
  }
}

impl From<&str> for SafeIdent {
  fn from(name: &str) -> Self {
    let name = name.trim().replace('-', "_");
    if RUST_KEYWORDS.contains(&name.as_str()) {
      return Self {
        safe: Some(format_ident!("{}", format!("{}_", name))),
        ident: format_ident!("{}", name),
      };
    }

    Self {
      safe: None,
      ident: format_ident!("{}", name),
    }
  }
}

impl core::fmt::Display for SafeIdent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.ident.fmt(f)
  }
}

impl core::fmt::Debug for SafeIdent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.ident())?;
    if let Some(safe) = self.safe.as_ref() {
      write!(f, " ({safe})")?;
    }
    Ok(())
  }
}

/// Reference: https://doc.rust-lang.org/reference/keywords.html
const RUST_KEYWORDS: &[&str] = &[
  // Keywords currently in use
  "as",
  "break",
  "const",
  "continue",
  "crate",
  "dyn",
  "else",
  "enum",
  "extern",
  "false",
  "fn",
  "for",
  "if",
  "impl",
  "in",
  "let",
  "loop",
  "match",
  "mod",
  "move",
  "mut",
  "pub",
  "ref",
  "return",
  "self",
  "Self",
  "static",
  "struct",
  "super",
  "trait",
  "true",
  "type",
  "unsafe",
  "use",
  "where",
  "while",
  // Keywords reserved for future use
  "abstract",
  "async",
  "await",
  "become",
  "box",
  "do",
  "final",
  "macro",
  "macro_rules",
  "override",
  "priv",
  "try",
  "typeof",
  "unsized",
  "virtual",
  "yield",
];
