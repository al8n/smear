use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::utils::human_display::DisplayHuman;

mod str;

/// An inline string representation in GraphQL.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InlineString<S> {
  /// A clean string without any escaped characters or escaped unicode.
  Clean(S),
  /// Only simple escapes: \" \\ \/ \b \f \n \r \t (no `\uXXXX`).
  SimpleEscape(S),
  /// Only unicode escapes: one or more `\uXXXX`, and no simple ones.
  UnicodeEscape(S),
  /// Both simple and unicode escapes are present.
  MixedEscape(S),
}

impl<S: DisplayHuman> core::fmt::Display for InlineString<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Clean(s)
      | Self::SimpleEscape(s)
      | Self::UnicodeEscape(s)
      | Self::MixedEscape(s) => DisplayHuman::fmt(s, f),
    }
  }
} 

impl<'a> InlineString<&'a str> {
  /// Returns the str representation of the inline string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Clean(s) | Self::SimpleEscape(s) | Self::UnicodeEscape(s) | Self::MixedEscape(s) => s,
    }
  }
}

impl<'a> InlineString<&'a [u8]> {
  /// Returns the byte slice representation of the inline string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Clean(s) | Self::SimpleEscape(s) | Self::UnicodeEscape(s) | Self::MixedEscape(s) => s,
    }
  }
}

impl<S> InlineString<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(self) -> S where S: Copy {
    match self {
      Self::Clean(s) | Self::SimpleEscape(s) | Self::UnicodeEscape(s) | Self::MixedEscape(s) => s,
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Clean(s) | Self::SimpleEscape(s) | Self::UnicodeEscape(s) | Self::MixedEscape(s) => s,
    }
  }
}

impl_common_traits!(InlineString::<&'a str>::as_str);
impl_common_traits!(InlineString::<&'a [u8]>::as_bytes);
