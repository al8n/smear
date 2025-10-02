use derive_more::{From, IsVariant, Unwrap, TryUnwrap};

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoSpan, Span},
};

use crate::punctuator::{Bang, FatArrow, LAngle, RAngle};

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

  /// Creates a parser for map types using the provided element type parser.
  ///
  /// This parser handles the complete map type syntax including brackets,
  /// element type parsing, and optional bang modifier. The element type
  /// parsing is delegated to the provided parser for flexibility.
  ///
  /// ## Parameters
  /// - `parser`: Parser for the element type within the map
  ///
  /// ## Grammar Handled
  /// ```text
  /// MapType : < Key!? => Value !? > !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// <String => Int>        # Nullable map of nullable strings
  /// <String => Int!>!      # Non-null map of non-null strings
  /// <<User>>               # Nested map type
  /// <ID!>                  # Nullable map of non-null IDs
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, KP, VP>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    FatArrow: Parseable<'a, I, T, Error> + 'a,
    KP: Parser<'a, I, Key, E> + Clone,
    VP: Parser<'a, I, Value, E> + Clone,
  {
    LAngle::parser()
      .ignore_then(key_parser.clone())
      .then(
        RAngle::parser()
          .ignored()
          .map(|_| None)
          .or(
            FatArrow::parser()
              .ignore_then(
                value_parser
                  .then_ignore(RAngle::parser())
                  .map(Some)
              )
          )
      )
      .then(Bang::parser().or_not())
      .map_with(|((k, v), bang), exa| match v {
        None => Self::Set(SetType::new(exa.span(), k, bang.is_some())),
        Some(v) => Self::Map(MapType::new(exa.span(), k, v, bang.is_some())),
      })
  }
}

impl<'a, Key, Value, I, T, Error> Parseable<'a, I, T, Error> for AngleType<Key, Value>
where
  Key: Parseable<'a, I, T, Error>,
  Value: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Bang: Parseable<'a, I, T, Error> + 'a,
  FatArrow: Parseable<'a, I, T, Error> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser_with(Key::parser(), Value::parser())
  }
}
