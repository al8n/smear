use derive_more::{Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{logos, utils::human_display::DisplayHuman};

pub use block::*;
pub use inline::*;

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

    impl<S> $name<S> {
      #[inline(always)]
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
      pub const fn source(self) -> S where S: Copy {
        self.source
      }
    }

    impl<S: logosky::utils::human_display::DisplayHuman> core::fmt::Display for $name<S> {
      #[inline]
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        logosky::utils::human_display::DisplayHuman::fmt(&self.source, f)
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
    impl<$lt> PartialEq<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn eq(&self, other: &$ty) -> bool {
        self.$fn().eq(other)
      }
    }

    impl<$lt> PartialEq<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn eq(&self, other: &$name<&$lt $ty>) -> bool {
        other.eq(self)
      }
    }

    impl<$lt> PartialOrd<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn partial_cmp(&self, other: &$ty) -> Option<core::cmp::Ordering> {
        self.$fn().partial_cmp(other)
      }
    }

    impl<$lt> PartialOrd<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn partial_cmp(&self, other: &$name<&$lt $ty>) -> Option<core::cmp::Ordering> {
        other.partial_cmp(self).map(core::cmp::Ordering::reverse)
      }
    }

    impl<$lt> core::borrow::Borrow<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn borrow(&self) -> &$ty {
        self
      }
    }

    impl<$lt> AsRef<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn as_ref(&self) -> &$ty {
        core::borrow::Borrow::borrow(self)
      }
    }

    impl<$lt> core::ops::Deref for $name<&$lt $ty> {
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

variant_type!(
  /// A plain string without any escapes.
  #[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
  #[repr(transparent)]
  pub struct LitPlainStr {}
);

mod block;
mod inline;

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
}

#[derive(From, Into, Deref, DerefMut)]
pub(super) struct SealedLexer<'a, 'l, T: logos::Logos<'l>>(&'a mut logos::Lexer<'l, T>);
