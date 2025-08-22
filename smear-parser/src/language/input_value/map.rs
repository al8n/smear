use chumsky::{container::Container, extra::ParserExtra, prelude::*};

use super::super::{
  super::{char::Char, source::Source, spanned::Spanned, convert::*, language::ignored::ignored},
  punct::{LAngle, RAngle, Colon},
};

use core::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct MapEntry<Key, Value, Span> {
  span: Span,
  key: Key,
  colon: Colon<Span>,
  value: Value,
}

impl<Key, Value, Span> AsRef<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span> IntoSpanned<Span> for MapEntry<Key, Value, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Key, Value, Span> IntoComponents for MapEntry<Key, Value, Span> {
  type Components = (Span, Key, Colon<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.key, self.colon, self.value)
  }
}

impl<Key, Value, Span> MapEntry<Key, Value, Span> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }
  pub const fn key(&self) -> &Key {
    &self.key
  }
  pub const fn value(&self) -> &Value {
    &self.value
  }
  /// key ':' value  (only **trailing** ignored so repeats stop cleanly at '>')
  pub fn parser_with<'src, I, E, KP, VP>(
    key_parser: KP,
    value_parser: VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    KP: Parser<'src, I, Key, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
  {
    key_parser
      .then(Colon::parser().padded_by(ignored()))
      .then(value_parser)
      .map_with(|((key, colon), value), sp| Self {
        key,
        colon,
        value,
        span: Spanned::from_map_extra(sp),
      })
  }
}

#[derive(Debug, Clone)]
pub struct Map<Key, Value, Span, C = Vec<MapEntry<Key, Value, Span>>> {
  span: Span,
  l_angle: LAngle<Span>,
  fields: C,
  r_angle: RAngle<Span>,
  _key: PhantomData<Key>,
  _value: PhantomData<Value>,
}

impl<Key, Value, Span, C> AsRef<Span> for Map<Key, Value, Span, C> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Key, Value, Span, C> IntoSpanned<Span> for Map<Key, Value, Span, C> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Key, Value, Span, C> IntoComponents for Map<Key, Value, Span, C> {
  type Components = (Span, LAngle<Span>, C, RAngle<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_angle, self.fields, self.r_angle)
  }
}

impl<Key, Value, Span, C> Map<Key, Value, Span, C> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn l_angle(&self) -> &LAngle<Span> {
    &self.l_angle
  }
  pub const fn r_angle(&self) -> &RAngle<Span> {
    &self.r_angle
  }
  pub const fn fields(&self) -> &C {
    &self.fields
  }

  pub fn parser_with<'src, I, E, KP, VP>(key_parser: KP, value_parser: VP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    KP: Parser<'src, I, Key, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
    C: Container<MapEntry<Key, Value, Span>>,
  {
    LAngle::<Span>::parser()
      .then_ignore(ignored())
      .then(choice((
        // Empty sentinel: "<:>"
        Colon::<Span>::parser()
          .padded_by(ignored())
          .then(RAngle::<Span>::parser())
          .map(|_| C::default()),
        // Non-empty: one-or-more entries; commas live in `ws`
        MapEntry::<Key, Value, Span>::parser_with(key_parser, value_parser).padded_by(ignored()).repeated().at_least(1).collect::<C>(),
      )))
      .then_ignore(ignored())
      .then(RAngle::parser())
      .map_with(|((l_angle, fields), r_angle), sp| Self {
        span: Spanned::from_map_extra(sp),
        l_angle,
        r_angle,
        fields,
        _key: PhantomData,
        _value: PhantomData,
      })
  }
}

