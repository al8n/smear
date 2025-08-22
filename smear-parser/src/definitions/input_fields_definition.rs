use chumsky::{extra::ParserExtra, prelude::*};

use core::marker::PhantomData;
use std::vec::Vec;

use super::super::{
  language::{
    ignored::ignored,
    punct::{LBrace, RBrace},
  },
  source::{Char, Slice, Source},
};

/// The arguments definition.
///
/// Spec: [InputFieldsDefinition](https://spec.graphql.org/draft/#InputFieldsDefinition)
#[derive(Debug, Clone)]
pub struct InputFieldsDefinition<InputValueDefinition, Span, Container = Vec<InputValueDefinition>>
{
  span: Span,
  values: Container,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Span, Container>
  InputFieldsDefinition<InputValueDefinition, Span, Container>
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

  /// The left brace of the input fields definition.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// The right brace of the input fields definition.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Returns a parser for the input fields definition.
  #[inline]
  pub fn parser_with<'src, I, E, P>(
    input_value_definition_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, InputValueDefinition, E> + Clone,
    Container: chumsky::container::Container<InputValueDefinition>,
  {
    LBrace::parser()
      // allow Ignored right after '{' (e.g., newlines/commas)
      .then_ignore(ignored())
      // one-or-more items, collected into `Container`
      .then(input_value_definition_parser.padded_by(ignored()).repeated().at_least(1).collect())
      // optional Ignored before '}'
      .then_ignore(ignored())
      .then(RBrace::parser())
      .map_with(|((l_brace, values), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        values,
        l_brace,
        r_brace,
        _input_value_definition: PhantomData,
      })
  }
}
