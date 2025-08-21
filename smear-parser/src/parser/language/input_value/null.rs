use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{Char, Spanned};

/// Represents a null value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct NullValue<Src, Span> {
  /// The original span of the null value
  span: Spanned<Src, Span>,
}

impl<Src, Span> NullValue<Src, Span> {
  /// Returns the span of the null value
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a parser of null value.
  ///
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    just([I::Token::n, I::Token::u, I::Token::l, I::Token::l])
      .map_with(|_, span| Null {
        span: Spanned::from(span),
      })
      .padded_by(super::ignored::ignored())
  }
}
