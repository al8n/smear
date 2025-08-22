use chumsky::{extra::ParserExtra, prelude::*};

use core::marker::PhantomData;
use std::vec::Vec;

use super::super::{
  char::Char,
  language::{
    ignored::ignored,
    punct::{LParen, RParen},
  },
  source::Source,
  spanned::Spanned,
  convert::*,
};

/// The arguments definition.
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition)
#[derive(Debug, Clone)]
pub struct ArgumentsDefinition<
  InputValueDefinition,
  Span,
  Container = Vec<InputValueDefinition>,
> {
  span: Span,
  l_paren: LParen<Span>,
  values: Container,
  r_paren: RParen<Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Span, Container> AsRef<Span>
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<InputValueDefinition, Span, Container> IntoSpanned<Span>
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<InputValueDefinition, Span, Container> IntoComponents
  for ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  type Components = (Span, LParen<Span>, Container, RParen<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_paren, self.values, self.r_paren)
  }
}

impl<InputValueDefinition, Span, Container>
  ArgumentsDefinition<InputValueDefinition, Span, Container>
{
  /// The span of the arguments definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The values of the arguments definition.
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  /// The left parenthesis of the arguments definition.
  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  /// The right parenthesis of the arguments definition.
  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
    &self.r_paren
  }

  /// Returns a parser for the arguments definition.
  #[inline]
  pub fn parser_with<'src, I, E, P>(
    input_value_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    LParen::parser()
      // allow Ignored right after '(' (e.g., newlines/commas)
      .then_ignore(ignored())
      // one-or-more items, collected into `Container`
      .then(input_value_definition_parser.padded_by(ignored()).repeated().at_least(1).collect())
      // optional Ignored before ')'
      .then_ignore(ignored())
      .then(RParen::parser())
      .map_with(|((l_paren, values), r_paren), sp| Self {
        span: Spanned::from_map_extra(sp),
        values,
        l_paren,
        r_paren,
        _input_value_definition: PhantomData,
      })
  }
}
