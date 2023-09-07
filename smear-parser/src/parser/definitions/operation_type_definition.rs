use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::{
    ignored::ignored,
    punct::{Colon, LBrace, RBrace},
  },
  Name, SmearChar, Spanned,
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct RootOperationTypeDefinition<OperationType, Src, Span> {
  span: Spanned<Src, Span>,
  operation_type: OperationType,
  colon: Colon<Src, Span>,
  name: Name<Src, Span>,
}

impl<OperationType, Src, Span> RootOperationTypeDefinition<OperationType, Src, Span> {
  /// Returns the span of the operation type definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the operation type.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the colon span.
  #[inline]
  pub const fn colon(&self) -> &Colon<Src, Span> {
    &self.colon
  }

  /// Returns the name of the operation type definition.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Consumes the operation type definition.
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    OperationType,
    Colon<Src, Span>,
    Name<Src, Span>,
  ) {
    (self.span, self.operation_type, self.colon, self.name)
  }

  /// Returns a parser for the operation type definition.
  pub fn parser_with<'src, I, E, P>(
    operation_type_parser: impl FnOnce() -> P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, OperationType, E> + Clone,
  {
    operation_type_parser()
      .then(Colon::parser().padded_by(ignored()))
      .then(Name::parser())
      .map_with(|((operation_type, colon), name), sp| Self {
        span: Spanned::from(sp),
        operation_type,
        colon,
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct RootOperationTypesDefinition<
  OperationTypeDefinition,
  Src,
  Span,
  Container = Vec<OperationTypeDefinition>,
> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  operation_type_definitions: Container,
  r_brace: RBrace<Src, Span>,
  _m: PhantomData<OperationTypeDefinition>,
}

impl<OperationTypeDefinition, Src, Span, Container>
  RootOperationTypesDefinition<OperationTypeDefinition, Src, Span, Container>
{
  /// Returns the span of the operation types definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left brace span.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }

  /// Returns the operation type definitions.
  #[inline]
  pub const fn operation_type_definitions(&self) -> &Container {
    &self.operation_type_definitions
  }

  /// Returns the right brace span.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }

  /// Consumes the operation types definition.
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    LBrace<Src, Span>,
    Container,
    RBrace<Src, Span>,
  ) {
    (
      self.span,
      self.l_brace,
      self.operation_type_definitions,
      self.r_brace,
    )
  }

  /// Returns a parser for the operation types definition.
  pub fn parser_with<'src, I, E, P>(
    operation_type_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, OperationTypeDefinition, E> + Clone,
    Container: chumsky::container::Container<OperationTypeDefinition>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        operation_type_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RBrace::parser())
      .map_with(
        |((l_brace, operation_type_definitions), r_brace), sp| Self {
          span: Spanned::from(sp),
          l_brace,
          operation_type_definitions,
          r_brace,
          _m: PhantomData,
        },
      )
  }
}
