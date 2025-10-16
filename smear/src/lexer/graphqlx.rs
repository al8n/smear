use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::utils::human_display::DisplayHuman;

/// Errors for GraphQLx lexers
pub mod error;

/// Abstract syntax tree (AST) lexers for GraphQLx
pub mod ast;

/// Concrete syntax tree (CST) lexers for GraphQLx
pub mod lossless;

mod handlers;

// #[cfg(test)]
// mod tests;

/// A GraphQLx integer literal, which can be in decimal, hexadecimal, binary, or octal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitInt<S> {
  /// A decimal integer literal.
  Decimal(S),
  /// A hexadecimal integer literal.
  Hex(S),
  /// A binary integer literal.
  Binary(S),
  /// An octal integer literal.
  Octal(S),
}

impl<S> AsRef<S> for LitInt<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitInt<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitInt<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: DisplayHuman> DisplayHuman for LitInt<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitInt<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
      Self::Binary(s) => *s,
      Self::Octal(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
      Self::Binary(s) => s,
      Self::Octal(s) => s,
    }
  }
}

/// A GraphQLx float literal, which can be in decimal or hexadecimal format.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, TryUnwrap, Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitFloat<S> {
  /// A decimal float literal.
  Decimal(S),
  /// A hexadecimal float literal.
  Hex(S),
}

impl<S> AsRef<S> for LitFloat<S> {
  #[inline(always)]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitFloat<&str> {
  #[inline(always)]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitFloat<&[u8]> {
  #[inline(always)]
  fn as_ref(&self) -> &[u8] {
    self.source_ref()
  }
}

impl<S: DisplayHuman> DisplayHuman for LitFloat<S> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S: core::fmt::Display> core::fmt::Display for LitFloat<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> LitFloat<S> {
  /// Returns the underlying source.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Decimal(s) => *s,
      Self::Hex(s) => *s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
    }
  }
}
