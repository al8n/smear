use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  lang::{ignored, keywords, StringValue},
  source::{Char, Slice, Source},
};

#[derive(Debug, Clone)]
pub struct SchemaDefinition<Directives, RootOperationTypesDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  schema: keywords::Schema<Span>,
  directives: Option<Directives>,
  operation_type_definitions: RootOperationTypesDefinition,
}

impl<Directives, RootOperationTypesDefinition, Span>
  SchemaDefinition<Directives, RootOperationTypesDefinition, Span>
{
  /// Returns the span of the schema definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the description of the schema definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns the schema keyword.
  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Span> {
    &self.schema
  }

  /// Returns the directives of the schema definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the operation type definitions of the schema definition.
  #[inline]
  pub const fn root_operation_type_definitions(&self) -> &RootOperationTypesDefinition {
    &self.operation_type_definitions
  }

  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    keywords::Schema<Span>,
    Option<Directives>,
    RootOperationTypesDefinition,
  ) {
    (
      self.span,
      self.description,
      self.schema,
      self.directives,
      self.operation_type_definitions,
    )
  }

  pub fn parser_with<'src, I, E, DP, RP>(
    directives_parser: DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Schema::parser().then_ignore(ignored()))
      .then(directives_parser.then_ignore(ignored()).or_not())
      .then(root_operation_types_definition_parser)
      .map_with(
        |(((description, schema), directives), operation_type_definitions), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          schema,
          directives,
          operation_type_definitions,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum SchemaExtensionContent<Directives, RootOperationTypesDefinition> {
  Directives(Directives),
  Operations {
    directives: Option<Directives>,
    definitions: RootOperationTypesDefinition,
  },
}

impl<Directives, RootOperationTypesDefinition>
  SchemaExtensionContent<Directives, RootOperationTypesDefinition>
{
  pub fn parser_with<'src, I, E, DP, RP>(
    directives_parser: impl Fn() -> DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,

    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .then_ignore(ignored())
        .or_not()
        .then(root_operation_types_definition_parser)
        .map(|(directives, definitions)| Self::Operations {
          directives,
          definitions,
        }),
      directives_parser().map(Self::Directives),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct SchemaExtension<Directives, RootOperationTypesDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  schema: keywords::Schema<Span>,
  content: SchemaExtensionContent<Directives, RootOperationTypesDefinition>,
}

impl<Directives, RootOperationTypesDefinition, Span>
  SchemaExtension<Directives, RootOperationTypesDefinition, Span>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Span> {
    &self.schema
  }

  #[inline]
  pub const fn content(&self) -> &SchemaExtensionContent<Directives, RootOperationTypesDefinition> {
    &self.content
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Schema<Span>,
    SchemaExtensionContent<Directives, RootOperationTypesDefinition>,
  ) {
    (self.span, self.extend, self.schema, self.content)
  }

  pub fn parser_with<'src, I, E, DP, RP>(
    directives_parser: impl Fn() -> DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    keywords::Extend::<Span>::parser()
      .then_ignore(ignored())
      .then(keywords::Schema::<Span>::parser().then_ignore(ignored()))
      .then(SchemaExtensionContent::<
        Directives,
        RootOperationTypesDefinition,
      >::parser_with(
        directives_parser,
        root_operation_types_definition_parser,
      ))
      .map_with(|((extend_keyword, schema_keyword), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend: extend_keyword,
        schema: schema_keyword,
        content,
      })
      .padded_by(ignored())
  }
}
