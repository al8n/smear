use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::parser::{
  language::{
    ignored::ignored,
    punct::{LBrace, RBrace},
  },
  Char, Spanned,
};

/// The arguments definition.
///
/// Spec: [InputFieldsDefinition](https://spec.graphql.org/draft/#InputFieldsDefinition)
#[derive(Debug, Clone)]
pub struct InputFieldsDefinition<
  InputValueDefinition,
  Src,
  Span,
  Container = Vec<InputValueDefinition>,
> {
  span: Spanned<Src, Span>,
  values: Container,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  _input_value_definition: PhantomData<InputValueDefinition>,
}

impl<InputValueDefinition, Src, Span, Container>
  InputFieldsDefinition<InputValueDefinition, Src, Span, Container>
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

  /// The left brace of the input fields definition.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }

  /// The right brace of the input fields definition.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }

  /// Returns a parser for the input fields definition.
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
    let open = LBrace::parser();
    let close = RBrace::parser();

    // Each item is padded by Ignored so commas/newlines/etc work naturally
    let item = input_value_definition_parser.padded_by(ws.clone());

    open
      // allow Ignored right after '{' (e.g., newlines/commas)
      .then_ignore(ws.clone())
      // one-or-more items, collected into `Container`
      .then(item.repeated().at_least(1).collect())
      // optional Ignored before '}'
      .then_ignore(ws.clone())
      .then(close)
      .map_with(|((l_brace, values), r_brace), sp| Self {
        span: Spanned::from(sp),
        values,
        l_brace,
        r_brace,
        _input_value_definition: PhantomData,
      })
  }
}
