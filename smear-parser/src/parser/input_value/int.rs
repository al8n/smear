use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{SmearChar, Spanned};

/// Represents a raw integer value parsed from input.
///
/// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
#[derive(Debug, Clone, Copy)]
pub struct Int<Src, Span> {
  /// The original raw string representation of the integer value.
  span: Spanned<Src, Span>,
  /// Returns the sign of the value.
  sign: Option<Spanned<Src, Span>>,
  /// The integer part of the value.
  digits: Spanned<Src, Span>,
}

impl<Src, Span> Int<Src, Span> {
  /// Returns the span of the integer value.
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the sign
  pub const fn sign(&self) -> Option<&Spanned<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the span of the digits part.
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }

  /// Returns a parser for the integer value.
  ///
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value).
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let sign = just(I::Token::MINUS)
      .map_with(|_, span| Spanned::from(span))
      .or_not();
    let val = text::int(10).map_with(|_, sp| Spanned::from(sp));

    sign
      .then(val)
      .map_with(|(sign, digits), sp| Self {
        span: Spanned::from(sp),
        sign,
        digits,
      })
      .padded_by(super::ignored::padded())
  }
}
