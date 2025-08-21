use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{Char, Spanned};

use super::Int;

/// The sign of the float value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign<Src, Span> {
  /// The positive sign `+`
  Positive(Spanned<Src, Span>),
  /// The negative sign `-`
  Negative(Spanned<Src, Span>),
}

/// Represents the exponent part of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Exponent<Src, Span> {
  /// The span of the whole exponent part.
  span: Spanned<Src, Span>,
  /// The span of the exponent char, e.g. `e` or `E`
  e: Spanned<Src, Span>,
  /// The span of the sign char, e.g. `+` or `-`
  sign: Option<Sign<Src, Span>>,
  /// The span of the digits part, e.g. `123`
  digits: Spanned<Src, Span>,
}

impl<Src, Span> Exponent<Src, Span> {
  /// Returns the span of the whole exponent part.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the exponent char, e.g. `e` or `E`
  #[inline]
  pub const fn e(&self) -> &Spanned<Src, Span> {
    &self.e
  }

  /// Returns the span of the sign char, e.g. `+` or `-`
  #[inline]
  pub const fn sign(&self) -> Option<&Sign<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }
}

/// Represents the fractional of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Fractional<Src, Span> {
  /// The span of the whole fractional part.
  span: Spanned<Src, Span>,
  /// The span of the dot character, e.g. `.`
  dot: Spanned<Src, Span>,
  /// The span of the digits part, e.g. `123`
  digits: Spanned<Src, Span>,
}

impl<Src, Span> Fractional<Src, Span> {
  /// Returns the span of the whole fractional part.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the dot character, e.g. `.`
  #[inline]
  pub const fn dot(&self) -> &Spanned<Src, Span> {
    &self.dot
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }
}

/// Represents a raw float value parsed from input.
///
/// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
#[derive(Debug, Clone, Copy)]
pub struct FloatValue<Src, Span> {
  /// The span of the float value
  span: Spanned<Src, Span>,
  /// The integer section of the float value
  int: IntValue<Src, Span>,
  /// The fractional section of the float value
  fractional: Option<Fractional<Src, Span>>,
  /// The exponent section of the float value
  exponent: Option<Exponent<Src, Span>>,
}

impl<Src, Span> FloatValue<Src, Span> {
  /// Returns the span of the float value
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the integer section of the float value.
  pub const fn int(&self) -> &IntValue<Src, Span> {
    &self.int
  }

  /// Returns the fractional section of the float value.
  pub const fn fractional(&self) -> Option<&Fractional<Src, Span>> {
    self.fractional.as_ref()
  }

  /// Returns the exponent section of the float value.
  pub const fn exponent(&self) -> Option<&Exponent<Src, Span>> {
    self.exponent.as_ref()
  }

  /// Returns a parser of the float value
  ///
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let exp = || {
      just(I::Token::e)
        .or(just(I::Token::E))
        .map_with(|_, span| Spanned::from(span))
        .then(
          just(I::Token::PLUS)
            .map_with(|_, span| Sign::Positive(Spanned::from(span)))
            .or(just(I::Token::MINUS).map_with(|_, span| Sign::Negative(Spanned::from(span))))
            .or_not(),
        )
        .then(text::digits::<I, E>(10).map_with(|_, span| Spanned::from(span)))
        .map_with(|((e, sign), digits), span| Exponent {
          span: Spanned::from(span),
          e,
          sign,
          digits,
        })
    };

    let frac = || {
      just(I::Token::DOT)
        .map_with(|_, span| Spanned::from(span))
        .then(text::digits::<I, E>(10).map_with(|_, span| Spanned::from(span)))
        .map_with(|(dot, digits), span| Fractional {
          span: Spanned::from(span),
          dot,
          digits,
        })
    };

    Int::<Src, Span>::parser()
      .then(frac().then(exp().or_not()))
      .map_with(|(int, (frac, exp)), span| Float {
        span: Spanned::from(span),
        int,
        fractional: Some(frac),
        exponent: exp,
      })
      .or(
        Int::<Src, Span>::parser()
          .then(exp())
          .map_with(|(int, exp), span| Float {
            span: Spanned::from(span),
            int,
            fractional: None,
            exponent: Some(exp),
          }),
      )
      .padded_by(super::ignored::ignored())
  }
}
