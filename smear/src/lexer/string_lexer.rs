use derive_more::{Deref, DerefMut, From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  logos::Lexer,
  utils::{human_display::DisplayHuman, sdl_display::DisplaySDL},
};

pub use block::{LitBlockStr, LitComplexBlockStr};
pub use inline::{LitComplexInlineStr, LitInlineStr};

use crate::error::{StringError, StringErrors};



variant_type!(
  /// A plain string without any escapes.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  #[repr(transparent)]
  pub struct LitPlainStr {}
);

impl<S> DisplaySDL for LitPlainStr<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

/// A GraphQL string literal, either inline or block.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitStr<S> {
  /// An inline string literal.
  Inline(LitInlineStr<S>),
  /// A block string literal.
  Block(LitBlockStr<S>),
}

impl<S: DisplayHuman> core::fmt::Display for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Inline(s) => write!(f, "{s}"),
      Self::Block(s) => write!(f, "{s}"),
    }
  }
}

impl<S: DisplayHuman> DisplayHuman for LitStr<S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<S> LitStr<S> {
  /// Returns the underlying source
  #[inline(always)]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    match self {
      Self::Inline(s) => s.source(),
      Self::Block(s) => s.source(),
    }
  }

  /// Returns the reference to the underlying source
  #[inline(always)]
  pub const fn source_ref(&self) -> &S {
    match self {
      Self::Inline(s) => s.source_ref(),
      Self::Block(s) => s.source_ref(),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn to_equivalent<T>(&self) -> LitStr<T>
  where
    S: logosky::utils::ToEquivalent<T>,
  {
    match self {
      Self::Inline(s) => LitStr::Inline(s.to_equivalent()),
      Self::Block(s) => LitStr::Block(s.to_equivalent()),
    }
  }

  /// Converts this to an equivalent type.
  #[inline(always)]
  pub fn into_equivalent<T>(self) -> LitStr<T>
  where
    S: logosky::utils::IntoEquivalent<T>,
  {
    match self {
      Self::Inline(s) => LitStr::Inline(s.into_equivalent()),
      Self::Block(s) => LitStr::Block(s.into_equivalent()),
    }
  }
}

#[derive(Deref, DerefMut)]
#[repr(transparent)]
pub(super) struct SealedWrapper<L: ?Sized>(L);

impl<T> SealedWrapper<T> {
  #[inline(always)]
  pub const fn from_mut(t: &mut T) -> &mut Self {
    // Safety: This is safe because SealedWrapper is repr(transparent) over T
    unsafe { &mut *(t as *mut T as *mut Self) }
  }
}

mod block;
mod inline;

impl<'de: 'a, 'a> TryFrom<&'de str> for LitStr<&'a str> {
  type Error = StringErrors<char>;

  #[inline]
  fn try_from(value: &'de str) -> Result<Self, Self::Error> {
    if value.starts_with("\"\"\"") {
      let mut lexer = Lexer::<block::BlockStringToken>::new(value);
      lexer.bump(3);
      block::lex_block_str_from_str(SealedWrapper::from_mut(&mut lexer)).map(Self::Block)
    } else if value.starts_with('"') {
      let mut lexer = Lexer::<inline::StringToken>::new(value);
      lexer.bump(1);
      inline::lex_inline_str_from_str(SealedWrapper::from_mut(&mut lexer)).map(Self::Inline)
    } else {
      Err(StringError::unopened_string(value.chars().next(), 0).into())
    }
  }
}

impl<'de: 'a, 'a> TryFrom<&'de [u8]> for LitStr<&'a [u8]> {
  type Error = StringErrors<u8>;

  #[inline]
  fn try_from(value: &'de [u8]) -> Result<Self, Self::Error> {
    if value.starts_with(b"\"\"\"") {
      let mut lexer = Lexer::<block::BytesBlockStringToken>::new(value);
      lexer.bump(3);
      block::lex_block_str_from_bytes(SealedWrapper::from_mut(&mut lexer)).map(Self::Block)
    } else if value.starts_with(b"\"") {
      let mut lexer = Lexer::<inline::BytesStringToken>::new(value);
      lexer.bump(1);
      inline::lex_inline_str_from_bytes(SealedWrapper::from_mut(&mut lexer)).map(Self::Inline)
    } else {
      Err(StringError::unopened_string(value.first().copied(), 0).into())
    }
  }
}
