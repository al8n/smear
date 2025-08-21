use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use core::marker::PhantomData;
use std::vec::Vec;

use super::super::{
  char::Char,
  language::{
    ignored::ignored,
    punct::{LParen, RParen},
  },
  spanned::Spanned,
};

/// The arguments definition.
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition)
#[derive(Debug, Clone)]
pub struct ArgumentsDefinition<
  InputValueDefinition,
  Src,
  Span,
  Container = Vec<InputValueDefinition>,
> {
  span: Spanned<Src, Span>,
  values: Container,
  l_paren: LParen<Src, Span>,
  r_paren: RParen<Src, Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Src, Span, Container>
  ArgumentsDefinition<InputValueDefinition, Src, Span, Container>
{
  /// The span of the arguments definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The values of the arguments definition.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  /// The left parenthesis of the arguments definition.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Src, Span> {
    &self.l_paren
  }

  /// The right parenthesis of the arguments definition.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Src, Span> {
    &self.r_paren
  }

  /// Returns a parser for the arguments definition.
  #[inline]
  pub fn parser_with<'src, I, E, P>(
    input_value_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    let ws = ignored();
    let open = LParen::parser();
    let close = RParen::parser();

    // Each item is padded by Ignored so commas/newlines/etc work naturally
    let item = input_value_definition_parser.padded_by(ws.clone());

    open
      // allow Ignored right after '(' (e.g., newlines/commas)
      .then_ignore(ws.clone())
      // one-or-more items, collected into `Container`
      .then(item.repeated().at_least(1).collect())
      // optional Ignored before ')'
      .then_ignore(ws.clone())
      .then(close)
      .map_with(|((l_paren, values), r_paren), sp| Self {
        span: Spanned::from(sp),
        values,
        l_paren,
        r_paren,
        _input_value_definition: PhantomData,
      })
  }
}
