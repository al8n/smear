use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords,
  language::{ignored::ignored, input_value::String},
  SmearChar, Spanned,
};

#[derive(Debug, Clone)]
pub struct SchemaDefinition<Directives, RootOperationTypesDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<String<Src, Span>>,
  schema: keywords::Schema<Src, Span>,
  directives: Option<Directives>,
  operation_type_definitions: RootOperationTypesDefinition,
}

impl<Directives, RootOperationTypesDefinition, Src, Span>
  SchemaDefinition<Directives, RootOperationTypesDefinition, Src, Span>
{
  /// Returns the span of the schema definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the description of the schema definition.
  #[inline]
  pub const fn description(&self) -> Option<&String<Src, Span>> {
    self.description.as_ref()
  }

  /// Returns the schema keyword.
  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Src, Span> {
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
    Spanned<Src, Span>,
    Option<String<Src, Span>>,
    keywords::Schema<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    String::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Schema::parser().then_ignore(ignored()))
      .then(directives_parser.then_ignore(ignored()).or_not())
      .then(root_operation_types_definition_parser)
      .map_with(
        |(((description, schema), directives), operation_type_definitions), sp| Self {
          span: Spanned::from(sp),
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
    I: StrInput<'src>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
pub struct SchemaExtension<Directives, RootOperationTypesDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  schema: keywords::Schema<Src, Span>,
  content: SchemaExtensionContent<Directives, RootOperationTypesDefinition>,
}

impl<Directives, RootOperationTypesDefinition, Src, Span>
  SchemaExtension<Directives, RootOperationTypesDefinition, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }

  #[inline]
  pub const fn schema_keyword(&self) -> &keywords::Schema<Src, Span> {
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
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Schema<Src, Span>,
    SchemaExtensionContent<Directives, RootOperationTypesDefinition>,
  ) {
    (self.span, self.extend, self.schema, self.content)
  }

  pub fn parser_with<'src, I, E, DP, RP>(
    directives_parser: impl Fn() -> DP,
    root_operation_types_definition_parser: RP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
    RP: Parser<'src, I, RootOperationTypesDefinition, E> + Clone,
  {
    keywords::Extend::<Src, Span>::parser()
      .then_ignore(ignored())
      .then(keywords::Schema::<Src, Span>::parser().then_ignore(ignored()))
      .then(SchemaExtensionContent::<
        Directives,
        RootOperationTypesDefinition,
      >::parser_with(
        directives_parser,
        root_operation_types_definition_parser,
      ))
      .map_with(|((extend_keyword, schema_keyword), content), sp| Self {
        span: Spanned::from(sp),
        extend: extend_keyword,
        schema: schema_keyword,
        content,
      })
      .padded_by(ignored())
  }
}
