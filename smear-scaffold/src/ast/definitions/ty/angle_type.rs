use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{MapType, SetType};

/// The angle type is a sum type that can represent either a set type or a map type.
/// This is useful for SDLs who want to support both set and map types.
#[derive(Debug, Clone, Copy, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum AngleType<Key, Value> {
  /// The set type.
  Set(SetType<Key>),
  /// The map type.
  Map(MapType<Key, Value>),
}

impl<Key, Value> AsSpan<Span> for AngleType<Key, Value> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value> IntoSpan<Span> for AngleType<Key, Value> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Set(ty) => ty.into_span(),
      Self::Map(ty) => ty.into_span(),
    }
  }
}

impl<Key, Value> AngleType<Key, Value> {
  /// Creates a new angle type from a set type.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Set(ty) => ty.span(),
      Self::Map(ty) => ty.span(),
    }
  }
}
