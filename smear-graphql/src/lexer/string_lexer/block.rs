use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::human_display::DisplayHuman;

mod str;
mod u8_slice;

use super::LitPlainStr;

variant_type!(
  /// A block string representation in GraphQL containing one or more escaped triple quotes,
  /// carriage returns, leading/trailing blank lines, or common indentation.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  pub struct LitComplexBlockStr {
    /// The number of escaped triple quotes in the string.
    num_escaped_triple_quotes: usize,
    /// Whether there are any carriage return (`\r`) or carriage return + line feed (`\r\n`)
    /// line terminators in the string.
    has_cr_terminators: bool,
    /// The number of leading blank lines (spaces/tabs only) to trim.
    leading_blank_lines: usize,
    /// The number of trailing blank lines (spaces/tabs only) to trim.
    trailing_blank_lines: usize,
    /// The common indentation (spaces/tabs) across all non-blank lines
    /// *after the first* that should be stripped.
    common_indent: usize,
    /// The total number of lines in the block string (including blank lines).
    total_lines: usize,
  }
);

/// A block string representation in GraphQL.
#[derive(
  Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBlockStr<S> {
  /// A clean block string, no escaped triple quotes, no CR/CRLF,
  /// no leading/trailing blank lines, and no common indent.
  Plain(LitPlainStr<S>),

  /// A block string required some processing to unescape or normalize.
  /// This includes handling escaped triple quotes, line endings, and indentation.
  Complex(LitComplexBlockStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitBlockStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Plain(s) => DisplayHuman::fmt(s.source_ref(), f),
      Self::Complex(c) => DisplayHuman::fmt(c.source_ref(), f),
    }
  }
}

impl<'a> LitBlockStr<&'a str> {
  /// Returns the str representation of the block string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Plain(s) => s.as_str(),
      Self::Complex(c) => c.as_str(),
    }
  }
}

impl<'a> LitBlockStr<&'a [u8]> {
  /// Returns the byte slice representation of the block string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Plain(s) => s.as_bytes(),
      Self::Complex(c) => c.as_bytes(),
    }
  }
}

impl<S> LitBlockStr<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Plain(s) => s.source(),
      Self::Complex(c) => c.source(),
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Plain(s) => s.source_ref(),
      Self::Complex(c) => c.source_ref(),
    }
  }
}

impl_common_traits!(LitBlockStr::<&'a str>::as_str);
impl_common_traits!(LitBlockStr::<&'a [u8]>::as_bytes);
impl_common_traits!(LitComplexBlockStr::<&'a str>::as_str);
impl_common_traits!(LitComplexBlockStr::<&'a [u8]>::as_bytes);
