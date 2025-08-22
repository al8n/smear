use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  char::Char,
  keywords,
  language::{ignored::ignored, input_value::StringValue},
  name::Name,
  source::Source,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct InputObjectDefinition<FieldsDefinition, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  input: keywords::Input<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
  fields: Option<FieldsDefinition>,
}

impl<FieldsDefinition, Directives, Span>
  InputObjectDefinition<FieldsDefinition, Directives, Span>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    &self.input
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn fields(&self) -> Option<&FieldsDefinition> {
    self.fields.as_ref()
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    keywords::Input<Span>,
    Name<Span>,
    Option<Directives>,
    Option<FieldsDefinition>,
  ) {
    (
      self.span,
      self.description,
      self.input,
      self.name,
      self.directives,
      self.fields,
    )
  }

  #[inline]
  pub fn parser_with<'src, I, E, IFDP, DP>(
    input_fields_definition_parser: impl FnOnce() -> IFDP,
    directives_parser: impl FnOnce() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    IFDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Input::parser().then_ignore(ignored()))
      .then(Name::parser())
      .then(directives_parser().padded_by(ignored()).or_not())
      .then(
        input_fields_definition_parser()
          .padded_by(ignored())
          .or_not(),
      )
      .map_with(
        |((((description, input), name), directives), fields), sp| Self {
          span: Spanned::from_map_extra(sp),
          description,
          name,
          input,
          directives,
          fields,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub enum InputObjectExtensionContent<Directives, FieldsDefinition> {
  Directives(Directives),
  Fields {
    directives: Option<Directives>,
    fields: FieldsDefinition,
  },
}

impl<Directives, FieldsDefinition> InputObjectExtensionContent<Directives, FieldsDefinition> {
  pub fn parser_with<'src, I, E, DP, IFDP>(
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> IFDP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,

    DP: Parser<'src, I, Directives, E> + Clone,
    IFDP: Parser<'src, I, FieldsDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .then_ignore(ignored())
        .or_not()
        .then(fields_definition_parser())
        .map(|(directives, fields)| Self::Fields { directives, fields }),
      directives_parser().map(Self::Directives),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct InputObjectExtension<Directives, FieldsDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  input: keywords::Input<Span>,
  name: Name<Span>,
  content: InputObjectExtensionContent<Directives, FieldsDefinition>,
}

impl<Directives, FieldsDefinition, Span>
  InputObjectExtension<Directives, FieldsDefinition, Span>
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
  pub const fn input_keyword(&self) -> &keywords::Input<Span> {
    &self.input
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn content(&self) -> &InputObjectExtensionContent<Directives, FieldsDefinition> {
    &self.content
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Input<Span>,
    Name<Span>,
    InputObjectExtensionContent<Directives, FieldsDefinition>,
  ) {
    (self.span, self.extend, self.input, self.name, self.content)
  }

  #[inline]
  pub fn parser_with<'src, I, E, IFDP, DP>(
    input_fields_definition_parser: impl Fn() -> IFDP,
    directives_parser: impl Fn() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    IFDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Input::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(InputObjectExtensionContent::parser_with(
        directives_parser,
        input_fields_definition_parser,
      ))
      .map_with(|(((extend, input), name), content), sp| Self {
        span: Spanned::from_map_extra(sp),
        extend,
        input,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
