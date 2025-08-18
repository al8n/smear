use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{SmearChar, Spanned};

/// Represents a boolean value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct Boolean<Src, Span> {
  /// The original span of the boolean value
  span: Spanned<Src, Span>,
  /// The value of the boolean
  value: bool,
}

impl<Src, Span> Boolean<Src, Span> {
  /// Returns the value
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a parser for the boolean value.
  ///
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    just([I::Token::t, I::Token::r, I::Token::u, I::Token::e])
      .to(true)
      .or(
        just([
          I::Token::f,
          I::Token::a,
          I::Token::l,
          I::Token::s,
          I::Token::e,
        ])
        .to(false),
      )
      .map_with(|data, span| Boolean {
        span: Spanned::from(span),
        value: data,
      })
      .padded_by(super::ignored::ignored())
  }
}
