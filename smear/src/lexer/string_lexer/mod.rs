use derive_more::{Deref, DerefMut, From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  logos::Lexer,
  utils::{
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

use crate::lexer::error::{StringError, StringErrors};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use block::skip_block_str_from_bytes;
pub use block::{LitBlockStr, LitComplexBlockStr};
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) use inline::skip_inline_str_simd;
pub use inline::{LitComplexInlineStr, LitInlineStr};

macro_rules! variant_type {
  (
    $(#[$meta:meta])*
    $vis:vis struct $name:ident {
      $(
        $(#[$field_meta:meta])*
        $field:ident: $ty:ty $(,)?
      )*
    }
  ) => {
    $(#[$meta])*
    $vis struct $name<S> {
      source: S,
      $($field: $ty),*
    }

    impl<'a> TryFrom<$name<&'a [u8]>> for $name<&'a str> {
      type Error = core::str::Utf8Error;

      #[inline]
      fn try_from(value: $name<&'a [u8]>) -> Result<Self, Self::Error> {
        core::str::from_utf8(value.source())
          .map(|s| {
            Self::new(s, $(value.$field),*)
          })
      }
    }

    impl<S> $name<Option<S>> {
      /// transpose
      pub fn transpose(self) -> Option<$name<S>> {
        match self.source {
          Some(source) => Some($name {
            source,
            $($field: self.$field),*
          }),
          None => None,
        }
      }
    }

    impl<S> $name<S> {
      #[inline(always)]
      #[allow(clippy::too_many_arguments)]
      pub(crate) const fn new(source: S, $($field: $ty),*) -> Self {
        Self { source, $($field),* }
      }

      $(
        $( #[$field_meta] )*
        #[inline(always)]
        pub const fn $field(&self) -> $ty {
          self.$field
        }
      )*

      /// Returns the source of the simple escape string.
      #[inline(always)]
      pub const fn source_ref(&self) -> &S {
        &self.source
      }

      /// Returns the underlying source.
      #[inline(always)]
      pub const fn source(&self) -> S where S: Copy {
        self.source
      }

      /// Map
      #[inline(always)]
      pub fn map<O, F>(self, f: F) -> $name<O>
      where
        F: FnOnce(S) -> O,
      {
        $name {
          source: f(self.source),
          $($field: self.$field),*
        }
      }

      /// Converts this to an equivalent type.
      #[inline(always)]
      pub fn to_equivalent<T>(&self) -> $name<T>
      where
        S: tokora::utils::ToEquivalent<T>,
      {
        $name::new(self.source.to_equivalent(), $(self.$field),*)
      }

      /// Converts this to an equivalent type.
      #[inline(always)]
      pub fn into_equivalent<T>(self) -> $name<T>
      where
        S: tokora::utils::IntoEquivalent<T>,
      {
        $name::new(self.source.into_equivalent(), $(self.$field),*)
      }
    }

    impl<S: tokora::utils::human_display::DisplayHuman> core::fmt::Display for $name<S> {
      #[inline]
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        tokora::utils::human_display::DisplayHuman::fmt(&self.source, f)
      }
    }

    impl<'a> $name<&'a str> {
      /// Returns the str representation.
      #[inline(always)]
      pub const fn as_str(&self) -> &'a str {
        self.source
      }
    }

    impl<'a> $name<&'a [u8]> {
      /// Returns the byte slice representation.
      #[inline(always)]
      pub const fn as_bytes(&self) -> &'a [u8] {
        self.source
      }
    }
  }
}

macro_rules! impl_common_traits {
  ($name:ident::<&$lt:lifetime $ty:ty>::$fn:ident) => {
    impl PartialEq<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn eq(&self, other: &$ty) -> bool {
        self.$fn().eq(other)
      }
    }

    impl PartialEq<$name<&'_ $ty>> for $ty {
      #[inline(always)]
      fn eq(&self, other: &$name<&'_ $ty>) -> bool {
        other.eq(self)
      }
    }

    impl PartialOrd<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn partial_cmp(&self, other: &$ty) -> Option<core::cmp::Ordering> {
        self.$fn().partial_cmp(other)
      }
    }

    impl PartialOrd<$name<&'_ $ty>> for $ty {
      #[inline(always)]
      fn partial_cmp(&self, other: &$name<&'_ $ty>) -> Option<core::cmp::Ordering> {
        other.partial_cmp(self).map(core::cmp::Ordering::reverse)
      }
    }

    impl core::borrow::Borrow<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn borrow(&self) -> &$ty {
        self
      }
    }

    impl AsRef<$ty> for $name<&'_ $ty> {
      #[inline(always)]
      fn as_ref(&self) -> &$ty {
        core::borrow::Borrow::borrow(self)
      }
    }

    impl core::ops::Deref for $name<&'_ $ty> {
      type Target = $ty;

      #[inline(always)]
      fn deref(&self) -> &Self::Target {
        self.$fn()
      }
    }

    impl<$lt> From<$name<&$lt $ty>> for &$lt $ty {
      #[inline(always)]
      fn from(s: $name<&$lt $ty>) -> Self {
        s.$fn()
      }
    }
  };
}

mod block;
mod inline;

variant_type!(
  /// A plain string without any escapes.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  #[repr(transparent)]
  pub struct LitPlainStr {}
);

impl<S> DisplayCompact for LitPlainStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    self.source_ref().fmt(f)
  }
}

impl<S> DisplayPretty for LitPlainStr<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
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
    S: tokora::utils::ToEquivalent<T>,
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
    S: tokora::utils::IntoEquivalent<T>,
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

/// Panic message for the (statically unreachable) case where a string the SIMD
/// fast path routed to error delegation turns out to lex cleanly.
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
const VALID_STRING_UNREACHABLE: &str =
  "SIMD string fast path emits every valid string literal; only errors reach delegation";

/// Delegate a malformed string literal — whose opener the SIMD fast path could
/// not resolve — straight to the `string_lexer` sub-lexer, i.e. the very same
/// `lex_*` code the full grammar reaches through its `#[token("\"")]` /
/// `#[token("\"\"\"")]` arms, but *without* paying to build the whole grammar.
///
/// Implemented for the two Logos scan primitives (`str`, `[u8]`). Both dialects'
/// SIMD lexers call this on the string-error arms of their dispatch loop; it is
/// the string twin of `simd::delegate_to_logos`.
///
/// The seek combines the position-0 `TryFrom` idiom above (construct a carrier,
/// `bump` past the opener) with `delegate_to_logos`'s mid-stream positioning
/// (start from the whole source and advance to `token_start`): the carrier is
/// built over the entire source, then bumped to `token_start + opener_len` so
/// the sub-lexer's base offset lands exactly where the full grammar's did. The
/// returned end offset is `carrier.span().end` after lexing — equal to the end
/// of the error-token span the full grammar reports — so the caller can build a
/// byte-for-byte-identical error span of `token_start..end`.
///
/// Only ever invoked on the error path: the SIMD fast path emits every *valid*
/// string itself, so the delegated `lex_*` here always returns `Err` (hence the
/// [`VALID_STRING_UNREACHABLE`] `expect_err`).
#[cfg(any(feature = "graphql", feature = "graphqlx"))]
pub(crate) trait DelegateStringError {
  /// The `StringErrors` character type this primitive lexes into: `char` for
  /// `str`, `u8` for `[u8]` — matching the SIMD lexer's own `Slice::Char`.
  type Char;

  /// Lex the malformed string literal opening at `token_start`, returning its
  /// collected errors and the absolute byte offset where lexing stopped.
  ///
  /// `block` selects the block (`"""`, 3-byte opener) carrier over the inline
  /// (`"`, 1-byte opener) one. See the trait docs for the seek/span contract.
  fn delegate_string_error(
    &self,
    token_start: usize,
    block: bool,
  ) -> (StringErrors<Self::Char>, usize);
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl DelegateStringError for str {
  type Char = char;

  #[inline]
  fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<char>, usize) {
    if block {
      let mut lexer = Lexer::<block::BlockStringToken>::new(self);
      lexer.bump(token_start + 3);
      let errs = block::lex_block_str_from_str(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    } else {
      let mut lexer = Lexer::<inline::StringToken>::new(self);
      lexer.bump(token_start + 1);
      let errs = inline::lex_inline_str_from_str(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    }
  }
}

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
impl DelegateStringError for [u8] {
  type Char = u8;

  #[inline]
  fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
    if block {
      let mut lexer = Lexer::<block::BytesBlockStringToken>::new(self);
      lexer.bump(token_start + 3);
      let errs = block::lex_block_str_from_bytes(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    } else {
      let mut lexer = Lexer::<inline::BytesStringToken>::new(self);
      lexer.bump(token_start + 1);
      let errs = inline::lex_inline_str_from_bytes(SealedWrapper::from_mut(&mut lexer))
        .expect_err(VALID_STRING_UNREACHABLE);
      (errs, lexer.span().end)
    }
  }
}

#[cfg(all(feature = "bytes", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use bytes::Bytes;

  impl DelegateStringError for Bytes {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      self.as_ref().delegate_string_error(token_start, block)
    }
  }
};

#[cfg(all(feature = "hipstr", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use hipstr::{HipByt, HipStr};

  impl DelegateStringError for HipByt<'_> {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      self.as_ref().delegate_string_error(token_start, block)
    }
  }

  impl DelegateStringError for HipStr<'_> {
    type Char = char;

    #[inline]
    fn delegate_string_error(
      &self,
      token_start: usize,
      block: bool,
    ) -> (StringErrors<char>, usize) {
      let s: &str = self.as_ref();
      s.delegate_string_error(token_start, block)
    }
  }
};

#[cfg(all(feature = "bstr", any(feature = "graphql", feature = "graphqlx")))]
const _: () = {
  use bstr::BStr;

  impl DelegateStringError for BStr {
    type Char = u8;

    #[inline]
    fn delegate_string_error(&self, token_start: usize, block: bool) -> (StringErrors<u8>, usize) {
      let s: &[u8] = self.as_ref();
      s.delegate_string_error(token_start, block)
    }
  }
};
