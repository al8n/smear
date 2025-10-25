use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::utils::human_display::DisplayHuman;

/// Errors for GraphQLx lexers
pub mod error;

/// Syntactic tokens for GraphQLx - fast lexing that skips trivia.
///
/// This module provides [`SyntacticToken`](syntactic::SyntacticToken), which is optimized for
/// high-performance parsing by automatically filtering out whitespace, comments, and commas.
///
/// **Use this for**: GraphQLx servers, query execution, schema compilation, and any
/// performance-critical parsing where you don't need to preserve formatting.
///
/// **Key benefits**:
/// - Minimal memory footprint
/// - Maximum parsing speed
/// - Zero-copy token references
/// - Supports GraphQLx extensions (generics, imports, type paths, etc.)
///
/// See [`SyntacticToken`](syntactic::SyntacticToken) for detailed documentation.
pub mod syntactic {
  pub use super::ast::*;
}

/// Lossless tokens for GraphQLx - complete source preservation.
///
/// This module provides [`LosslessToken`](lossless::LosslessToken), which preserves all source
/// information including whitespace, comments, and formatting. Essential for developer tools
/// that need to maintain or manipulate source code without losing information.
///
/// **Use this for**: Code formatters, linters, IDEs, syntax highlighters, documentation tools,
/// and any application that needs perfect source reconstruction.
///
/// **Key benefits**:
/// - Complete source fidelity
/// - Access to all comments and formatting
/// - Build Concrete Syntax Trees (CST)
/// - Supports GraphQLx extensions (generics, imports, type paths, etc.)
///
/// See [`LosslessToken`](lossless::LosslessToken) for detailed documentation.
pub mod lossless;

mod ast;
mod handlers;

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
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitInt<&str> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitInt<&[u8]> {
  #[cfg_attr(not(tarpaulin), inline(always))]
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
  #[cfg_attr(not(tarpaulin), inline(always))]
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
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &S {
    self.source_ref()
  }
}

impl AsRef<str> for LitFloat<&str> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &str {
    self.source_ref()
  }
}

impl AsRef<[u8]> for LitFloat<&[u8]> {
  #[cfg_attr(not(tarpaulin), inline(always))]
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
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Decimal(s) => s,
      Self::Hex(s) => s,
    }
  }
}
