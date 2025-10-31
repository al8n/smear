use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{AsSpan, IntoSpan, Span};

use super::{MapType, SetType};

/// CST representation of an angle type (either Set or Map).
///
/// This is a sum type that can represent either `<Type>` (set) or `<Key => Value>` (map).
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum AngleType<Key, Value, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  /// The set type `<Type>`
  Set(SetType<Key, S, TriviaContainer>),
  /// The map type `<Key => Value>`
  Map(MapType<Key, Value, S, TriviaContainer>),
}

impl<Key, Value, S, TriviaContainer> AsSpan<Span> for AngleType<Key, Value, S, TriviaContainer> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, S, TriviaContainer> IntoSpan<Span> for AngleType<Key, Value, S, TriviaContainer> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Set(ty) => ty.into_span(),
      Self::Map(ty) => ty.into_span(),
    }
  }
}

impl<Key, Value, S, TriviaContainer> AngleType<Key, Value, S, TriviaContainer> {
  /// Returns the span covering the entire angle type.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Set(ty) => ty.span(),
      Self::Map(ty) => ty.span(),
    }
  }
}
