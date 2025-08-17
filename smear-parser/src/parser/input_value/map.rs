use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{LAngle, RAngle},
  SmearChar, Spanned,
};

#[derive(Debug, Clone)]
pub struct MapEntry<T, Src, Span> {
  span: Spanned<Src, Span>,
  colon: Spanned<Src, Span>,
  key: T,
  value: T,
}

impl<T, Src, Span> MapEntry<T, Src, Span> {
  /// Returns the span of the entry.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }

  /// Returns the key of the entry.
  #[inline]
  pub const fn key(&self) -> &T {
    &self.key
  }

  /// Returns the value of the entry.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }

  /// Returns a parser for the field.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    value
      .clone()
      .then(
        just(I::Token::COLON)
          .map_with(|_, span| Spanned::from(span))
          .padded_by(super::ignored::padded()),
      )
      .then(value) // <-- use injected value parser
      .map_with(|((key, colon), value), sp| Self {
        key,
        colon,
        value,
        span: Spanned::from(sp),
      })
      .padded_by(super::ignored::padded())
  }
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object T](https://spec.graphql.org/draft/#sec-Input-Object-T)
#[derive(Debug, Clone)]
pub struct Map<T, Src, Span> {
  /// The original span of the object value
  span: Spanned<Src, Span>,
  /// The left `{` token.
  l_angle: LAngle<Spanned<Src, Span>>,
  /// The right `}` token.
  r_angle: RAngle<Spanned<Src, Span>>,
  /// The content between the brackets.
  fields: Vec<MapEntry<T, Src, Span>>,
}

impl<T, Src, Span> Map<T, Src, Span> {
  /// Returns the span of the object value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left angle of the object value.
  #[inline]
  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }

  /// Returns the right angle of the object value.
  #[inline]
  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }

  /// Returns the fields of the object value.
  #[inline]
  pub const fn fields(&self) -> &[MapEntry<T, Src, Span>] {
    self.fields.as_slice()
  }

  /// Returns a parser for the object value.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    just(I::Token::LESS_THAN)
      .map_with(|_, span| LAngle::new(Spanned::from(span)))
      .then(
        MapEntry::<T, Src, Span>::parser_with(value.clone())
          .separated_by(just(I::Token::COMMA).padded_by(super::ignored::padded()))
          .allow_trailing() // `{ a: 1, }` allowed
          .collect()
          .padded_by(super::ignored::padded()),
      )
      .then(just(I::Token::GREATER_THAN).map_with(|_, span| RAngle::new(Spanned::from(span))))
      .map_with(|((l_angle, fields), r_angle), span| Self {
        span: Spanned::from(span),
        l_angle,
        r_angle,
        fields,
      })
  }
}
