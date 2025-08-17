use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBracket, RBracket},
  SmearChar, Spanned,
};

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone)]
pub struct List<Value, Src, Span> {
  /// The original span of the list value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_bracket: LBracket<Spanned<Src, Span>>,
  /// The right `]` token.
  r_bracket: RBracket<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Vec<Value>,
}

impl<Value, Src, Span> List<Value, Src, Span> {
  /// Returns the span of the list value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the list value.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Spanned<Src, Span>> {
    &self.l_bracket
  }

  /// Returns the right bracket of the list value.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Spanned<Src, Span>> {
    &self.r_bracket
  }

  /// Returns the values of the list.
  #[inline]
  pub const fn values(&self) -> &[Value] {
    self.values.as_slice()
  }

  /// Returns a parser for the list value.
  /// Returns a parser for the list value.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    Src: 'src,
    Span: 'src,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone + 'src,
  {
    let empty = just(I::Token::BRACKET_OPEN)
      .padded_by(super::ignored::padded())
      .map_with(|_, sp| LBracket::new(Spanned::from(sp)))
      .then(
        just(I::Token::BRACKET_CLOSE)
          .padded_by(super::ignored::padded())
          .map_with(|_, sp| RBracket::new(Spanned::from(sp))),
      )
      .map_with(|(l_bracket, r_bracket), sp| Self {
        span: Spanned::from(sp),
        l_bracket,
        r_bracket,
        values: Vec::new(),
      });
    let parser = just(I::Token::BRACKET_OPEN)
      .padded_by(super::ignored::padded())
      .map_with(|_, sp| LBracket::new(Spanned::from(sp)))
      .then(
        value
          .padded_by(super::ignored::padded())
          .separated_by(just(I::Token::COMMA).padded_by(super::ignored::padded()))
          .allow_trailing()
          .collect()
          .padded_by(super::ignored::padded()),
      )
      .then(
        just(I::Token::BRACKET_CLOSE)
          .padded_by(super::ignored::padded())
          .map_with(|_, sp| RBracket::new(Spanned::from(sp))),
      )
      .map_with(|((l_bracket, values), r_bracket), sp| Self {
        span: Spanned::from(sp),
        l_bracket,
        r_bracket,
        values,
      });
    empty.or(parser)
  }
}
