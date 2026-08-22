use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::utils::{
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
};

use super::LitPlainStr;
use std::{borrow::Cow, string::String};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use self::simd::skip_inline_str_simd;
pub(crate) use self::{
  str::{StringToken, lex_inline_str_from_str},
  u8_slice::{StringToken as BytesStringToken, lex_inline_str_from_bytes},
};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod simd;
mod str;
mod u8_slice;

variant_type!(
  /// A complex inline string representation in GraphQL containing one or more escapes.
  /// This includes simple escapes, unicode escapes, or both.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  pub struct LitComplexInlineStr {
    /// The capacity required to store the normalized string.
    required_capacity: usize,
  }
);

impl<S> DisplayCompact for LitComplexInlineStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> DisplayPretty for LitComplexInlineStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

/// An inline string representation in GraphQL.
#[derive(
  Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, From, IsVariant, Unwrap, TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitInlineStr<S> {
  /// A clean string without any escaped characters or escaped unicode.
  Plain(LitPlainStr<S>),
  /// A complex string containing escaped characters.
  ///
  /// This includes escapes like:
  /// 1. `\"`, `\\`, `\n`, etc,
  /// 2. fixed-width unicode escapes like `\u1234`
  /// 3. variable-width unicode escapes like `\u{1F600}`
  Complex(LitComplexInlineStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitInlineStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Plain(s) => DisplayHuman::fmt(s.source_ref(), f),
      Self::Complex(s) => DisplayHuman::fmt(s.source_ref(), f),
    }
  }
}

impl<'a> LitInlineStr<&'a str> {
  /// Returns the str representation of the inline string.
  #[inline(always)]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Plain(s) => s.as_str(),
      Self::Complex(s) => s.as_str(),
    }
  }
}

impl<'a> LitInlineStr<&'a [u8]> {
  /// Returns the byte slice representation of the inline string.
  #[inline(always)]
  pub const fn as_bytes(&self) -> &'a [u8] {
    match self {
      Self::Plain(s) => s.as_bytes(),
      Self::Complex(s) => s.as_bytes(),
    }
  }
}

impl<'a> TryFrom<LitInlineStr<&'a [u8]>> for LitInlineStr<&'a str> {
  type Error = core::str::Utf8Error;

  #[inline]
  fn try_from(value: LitInlineStr<&'a [u8]>) -> Result<Self, Self::Error> {
    match value {
      LitInlineStr::Plain(s) => s.try_into().map(Self::Plain),
      LitInlineStr::Complex(s) => s.try_into().map(Self::Complex),
    }
  }
}

/// Answers the literal's **value** — draft §2.9.1's `StringValue`, delimiters gone and every
/// escape applied — borrowing it whenever the spelling already *is* the value.
///
/// # Value, not spelling
///
/// This type has a second `&str` door that answers the other question, and the two are not
/// interchangeable: [`as_str`](LitInlineStr::as_str), [`Deref`](core::ops::Deref),
/// [`AsRef`], [`Borrow`](core::borrow::Borrow) and `From<LitInlineStr<&str>> for &str` all
/// hand back the **source spelling**, `"` delimiters and backslashes included, because keeping the
/// source is what makes lexing allocate nothing. This conversion is the cooked reading, and
/// [`Cow`] is its return type precisely so that the [`Plain`](LitInlineStr::Plain) half — a
/// literal with no escape in it — still costs nothing but a reslice.
///
/// The two variants therefore answer the same question, which is what makes this door meaningful:
/// they used to disagree. `Plain` returned the source *with* its quotes, so `Cow::from` of the
/// literal `""` was the two-character string `""`, while `Complex` returned a cooked value with
/// the delimiters already off.
impl<'a> From<LitInlineStr<&'a str>> for Cow<'a, str> {
  #[inline]
  fn from(value: LitInlineStr<&'a str>) -> Self {
    match value {
      LitInlineStr::Plain(s) => Cow::Borrowed(inline_body(s.as_str())),
      LitInlineStr::Complex(s) => {
        let mut builder = String::with_capacity(s.required_capacity());
        normalize_str_to_string(inline_body(s.as_str()), &mut builder);
        Cow::Owned(builder)
      }
    }
  }
}

/// Strips an inline literal's `"` delimiters.
///
/// Total on a slice that does not carry them. Both carriers are `pub(crate)`-constructed and every
/// construction in this crate spans a whole `"…"` token, so the delimiters are the lexer's
/// guarantee — and a conversion on an already-lexed literal is not the place to re-check it with a
/// panicking index.
#[inline]
fn inline_body(raw: &str) -> &str {
  raw
    .strip_prefix('"')
    .and_then(|rest| rest.strip_suffix('"'))
    .unwrap_or(raw)
}

impl<S> LitInlineStr<Option<S>> {
  /// a
  #[inline(always)]
  pub fn transpose(self) -> Option<LitInlineStr<S>> {
    match self {
      LitInlineStr::Plain(s) => s.transpose().map(LitInlineStr::Plain),
      LitInlineStr::Complex(s) => s.transpose().map(LitInlineStr::Complex),
    }
  }
}

impl<S> LitInlineStr<S> {
  /// Returns the underlying source.
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Plain(s) => s.source(),
      Self::Complex(s) => s.source(),
    }
  }

  /// Map
  #[inline(always)]
  pub fn map<O, F>(self, f: F) -> LitInlineStr<O>
  where
    F: FnOnce(S) -> O,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.map(f)),
      Self::Complex(s) => LitInlineStr::Complex(s.map(f)),
    }
  }

  /// Returns the reference to the underlying source.
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Plain(s) => s.source_ref(),
      Self::Complex(s) => s.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn to_equivalent<T>(&self) -> LitInlineStr<T>
  where
    S: tokora::utils::ToEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.to_equivalent()),
      Self::Complex(s) => LitInlineStr::Complex(s.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn into_equivalent<T>(self) -> LitInlineStr<T>
  where
    S: tokora::utils::IntoEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.into_equivalent()),
      Self::Complex(s) => LitInlineStr::Complex(s.into_equivalent()),
    }
  }
}

impl_common_traits!(LitInlineStr::<&'a str>::as_str);
impl_common_traits!(LitInlineStr::<&'a [u8]>::as_bytes);
impl_common_traits!(LitComplexInlineStr::<&'a str>::as_str);
impl_common_traits!(LitComplexInlineStr::<&'a [u8]>::as_bytes);

/// Applies draft §2.9.1's escapes to an inline literal's body.
///
/// # Every escape the lexer accepts, and no panic for the ones it does not
///
/// `\"` `\\` `\/` `\b` `\f` `\n` `\r` `\t`, the fixed-width `\uXXXX` — including a surrogate
/// **pair**, which is two escapes spelling one character — and the braced `\u{X…}` of one to six
/// digits. The braced form is the one this used to be missing: `read_hex4` read the `{` as a hex
/// digit and panicked on it, so `Cow::from` of `"\u{1F600}"` aborted on a literal
/// `handle_braced_escape_unicode` in this very module accepts. A conversion on an
/// already-lexed literal is not allowed to be a panic site, so nothing here can panic: a sequence
/// the escape grammar does not cover is copied through as it was spelled, for the same reason
/// `inline_body` hands back a slice that carries no delimiters rather than panicking on one.
#[inline]
fn normalize_str_to_string(src: &str, output: &mut String) {
  let mut rest = src;
  while let Some(at) = rest.find('\\') {
    output.push_str(&rest[..at]);
    let after = &rest[at + 1..];
    match read_escape(after) {
      Some((ch, consumed)) => {
        output.push(ch);
        rest = &after[consumed..];
      }
      None => {
        output.push('\\');
        rest = after;
      }
    }
  }
  output.push_str(rest);
}

/// Reads one escape body — everything after the backslash — as its character and byte span.
#[inline]
fn read_escape(after: &str) -> Option<(char, usize)> {
  let ch = after.chars().next()?;
  let simple = match ch {
    '"' => '"',
    '\\' => '\\',
    '/' => '/',
    'b' => '\u{8}',
    'f' => '\u{c}',
    'n' => '\n',
    'r' => '\r',
    't' => '\t',
    'u' => return read_unicode_escape(&after[1..]).map(|(ch, span)| (ch, span + 1)),
    _ => return None,
  };
  Some((simple, ch.len_utf8()))
}

/// Reads a `\u` escape body — everything after the `u` — in either of draft §2.9.1's spellings.
#[inline]
fn read_unicode_escape(after: &str) -> Option<(char, usize)> {
  if let Some(rest) = after.strip_prefix('{') {
    let close = rest.find('}')?;
    let scalar = u32::from_str_radix(rest.get(..close)?, 16).ok()?;
    // `+ 2` for the braces the offset above does not span.
    return char::from_u32(scalar).map(|ch| (ch, close + 2));
  }

  let leading = hex4(after)?;
  // A leading surrogate is a character only with its trailing half, which the grammar spells as a
  // second `\u` escape immediately after this one. Anything else is not a character at all.
  if (0xD800..0xDC00).contains(&leading) {
    let tail = after.get(4..)?.strip_prefix("\\u")?;
    let trailing = hex4(tail)?;
    if !(0xDC00..0xE000).contains(&trailing) {
      return None;
    }
    let combined = 0x1_0000 + ((leading - 0xD800) << 10) + (trailing - 0xDC00);
    return char::from_u32(combined).map(|ch| (ch, 10));
  }

  char::from_u32(leading).map(|ch| (ch, 4))
}

/// Reads exactly four hex digits.
///
/// `get` rather than an index: it answers `None` on a slice shorter than four bytes *and* on one
/// whose fourth byte is inside a character, which is what lets the caller index at `4` afterwards.
#[inline]
fn hex4(digits: &str) -> Option<u32> {
  u32::from_str_radix(digits.get(..4)?, 16).ok()
}

#[inline(always)]
const fn utf8_len_for_scalar(cp: u32) -> usize {
  match cp {
    0x0000..=0x007F => 1,
    0x0080..=0x07FF => 2,
    0x0800..=0xFFFF => 3, // (surrogates are rejected elsewhere)
    _ => 4,               // 0x10000..=0x10_FFFF
  }
}
