use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::{
    ignored,
    punct::{Colon, LBrace, RBrace},
    Name,
  },
  source::{Char, Slice, Source},
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct RootOperationTypeDefinition<OperationType, Span> {
  span: Span,
  operation_type: OperationType,
  colon: Colon<Span>,
  name: Name<Span>,
}

impl<OperationType, Span> RootOperationTypeDefinition<OperationType, Span> {
  /// Returns the span of the operation type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the operation type.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the colon span.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns the name of the operation type definition.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Consumes the operation type definition.
  pub fn into_components(self) -> (Span, OperationType, Colon<Span>, Name<Span>) {
    (self.span, self.operation_type, self.colon, self.name)
  }

  /// Returns a parser for the operation type definition.
  pub fn parser_with<'src, I, E, P>(
    operation_type_parser: impl FnOnce() -> P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    P: Parser<'src, I, OperationType, E> + Clone,
  {
    operation_type_parser()
      .then(Colon::parser().padded_by(ignored()))
      .then(Name::parser())
      .map_with(|((operation_type, colon), name), sp| Self {
        span: Span::from_map_extra(sp),
        operation_type,
        colon,
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct RootOperationTypesDefinition<
  OperationTypeDefinition,
  Span,
  Container = Vec<OperationTypeDefinition>,
> {
  span: Span,
  l_brace: LBrace<Span>,
  operation_type_definitions: Container,
  r_brace: RBrace<Span>,
  _m: PhantomData<OperationTypeDefinition>,
}

impl<OperationTypeDefinition, Span, Container>
  RootOperationTypesDefinition<OperationTypeDefinition, Span, Container>
{
  /// Returns the span of the operation types definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the left brace span.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns the operation type definitions.
  #[inline]
  pub const fn operation_type_definitions(&self) -> &Container {
    &self.operation_type_definitions
  }

  /// Returns the right brace span.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Consumes the operation types definition.
  pub fn into_components(self) -> (Span, LBrace<Span>, Container, RBrace<Span>) {
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
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
          span: Span::from_map_extra(sp),
          l_brace,
          operation_type_definitions,
          r_brace,
          _m: PhantomData,
        },
      )
  }
}
