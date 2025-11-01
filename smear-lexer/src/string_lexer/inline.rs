use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{
  human_display::DisplayHuman,
  sdl_display::{DisplayCompact, DisplayPretty},
};

use super::LitPlainStr;
use std::{borrow::Cow, string::String};

pub(crate) use self::{
  str::{StringToken, lex_inline_str_from_str},
  u8_slice::{StringToken as BytesStringToken, lex_inline_str_from_bytes},
};

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
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_str(&self) -> &'a str {
    match self {
      Self::Plain(s) => s.as_str(),
      Self::Complex(s) => s.as_str(),
    }
  }
}

impl<'a> LitInlineStr<&'a [u8]> {
  /// Returns the byte slice representation of the inline string.
  #[cfg_attr(not(tarpaulin), inline(always))]
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

impl<'a> From<LitInlineStr<&'a str>> for Cow<'a, str> {
  #[inline]
  fn from(value: LitInlineStr<&'a str>) -> Self {
    match value {
      LitInlineStr::Plain(s) => Cow::Borrowed(s.as_str()),
      LitInlineStr::Complex(s) => {
        let mut builder = String::with_capacity(s.required_capacity());
        let raw = s.as_str();
        normalize_str_to_string(&raw[1..raw.len() - 1], &mut builder);
        Cow::Owned(builder)
      }
    }
  }
}

impl<S> LitInlineStr<S> {
  /// Returns the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Plain(s) => s.source(),
      Self::Complex(s) => s.source(),
    }
  }

  /// Returns the reference to the underlying source.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Plain(s) => s.source_ref(),
      Self::Complex(s) => s.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn to_equivalent<T>(&self) -> LitInlineStr<T>
  where
    S: logosky::utils::ToEquivalent<T>,
  {
    match self {
      Self::Plain(s) => LitInlineStr::Plain(s.to_equivalent()),
      Self::Complex(s) => LitInlineStr::Complex(s.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_equivalent<T>(self) -> LitInlineStr<T>
  where
    S: logosky::utils::IntoEquivalent<T>,
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

#[inline]
fn normalize_str_to_string(src: &str, output: &mut String) {
  #[inline]
  fn read_hex4(it: &mut core::str::Chars<'_>) -> u32 {
    (0..4)
      .map(|_| {
        it.next()
          .expect("\\u escape truncated")
          .to_digit(16)
          .expect("invalid hex digit in \\u escape")
      })
      .fold(0u32, |acc, d| (acc << 4) | d)
  }

  let mut chars = src.chars();

  while let Some(c) = chars.next() {
    match c {
      '\\' => {
        let esc = chars.next().expect("backslash at end");
        match esc {
          '"' | '\\' | '/' => output.push(esc),
          'b' => output.push('\x08'),
          'f' => output.push('\x0C'),
          'n' => output.push('\n'),
          'r' => output.push('\r'),
          't' => output.push('\t'),
          'u' => {
            let u = read_hex4(&mut chars);

            // If high surrogate, require a following \uXXXX low surrogate.
            if (0xD800..=0xDBFF).contains(&u) {
              let slash = chars
                .next()
                .expect("high surrogate not followed by low surrogate (missing \\)");
              let u_ch = chars
                .next()
                .expect("high surrogate not followed by low surrogate (missing u)");
              if slash != '\\' || u_ch != 'u' {
                panic!("high surrogate not followed by \\u");
              }
              let lo = read_hex4(&mut chars);
              if !(0xDC00..=0xDFFF).contains(&lo) {
                panic!("invalid low surrogate");
              }

              let combined = 0x10000 + (((u - 0xD800) << 10) | (lo - 0xDC00));
              let ch = core::char::from_u32(combined).expect("invalid combined code point");
              output.push(ch);
            } else if (0xDC00..=0xDFFF).contains(&u) {
              // Lone low surrogate is invalid in JSON.
              panic!("lone low surrogate");
            } else {
              let ch = core::char::from_u32(u).expect("invalid code point");
              output.push(ch);
            }
          }
          _ => unreachable!(),
        }
      }
      other => output.push(other),
    }
  }
}

#[cfg_attr(not(tarpaulin), inline(always))]
const fn utf8_len_for_scalar(cp: u32) -> usize {
  match cp {
    0x0000..=0x007F => 1,
    0x0080..=0x07FF => 2,
    0x0800..=0xFFFF => 3, // (surrogates are rejected elsewhere)
    _ => 4,               // 0x10000..=0x10_FFFF
  }
}
