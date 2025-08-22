use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  keywords,
  language::{ignored::ignored, input_value::StringValue},
  name::Name,
  source::{Char, Slice, Source},
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  ty: keywords::Type<Span>,
  name: Name<Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
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
  pub const fn type_keyword(&self) -> &keywords::Type<Span> {
    &self.ty
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces> {
    self.implements.as_ref()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn fields_definition(&self) -> Option<&FieldsDefinition> {
    self.fields_definition.as_ref()
  }

  /// Consumes the components of this definition.
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    keywords::Type<Span>,
    Name<Span>,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  ) {
    (
      self.span,
      self.description,
      self.ty,
      self.name,
      self.implements,
      self.directives,
      self.fields_definition,
    )
  }

  pub fn parser_with<'src, I, E, FDP, DP, IP>(
    fields_definition_parser: impl FnOnce() -> FDP,
    directives_parser: impl Fn() -> DP,
    implement_interfaces_parser: impl Fn() -> IP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Type::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then(implement_interfaces_parser().padded_by(ignored()).or_not())
      .then(directives_parser().padded_by(ignored()).or_not())
      .then(fields_definition_parser().padded_by(ignored()).or_not())
      .map_with(
        |(((((description, ty), name), implements), directives), fields), sp| Self {
          span: Spanned::from_map_extra(sp),
          description,
          name,
          directives,
          fields_definition: fields,
          ty,
          implements,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub enum ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
  Directives {
    implements: Option<ImplementInterfaces>,
    directives: Directives,
  },
  Fields {
    implements: Option<ImplementInterfaces>,
    directives: Option<Directives>,
    fields: FieldsDefinition,
  },
  Implements(ImplementInterfaces),
}

impl<ImplementInterfaces, Directives, FieldsDefinition>
  ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>
{
  pub fn parser_with<'src, I, E, IP, FDP, DP>(
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FDP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,

    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
  {
    choice((
      implement_interfaces_parser()
        .then_ignore(ignored())
        .or_not()
        .then(directives_parser().then_ignore(ignored()).or_not())
        .then(fields_definition_parser())
        .map(|((implements, directives), fields)| Self::Fields {
          implements,
          directives,
          fields,
        }),
      implement_interfaces_parser()
        .then_ignore(ignored())
        .or_not()
        .then(directives_parser())
        .map(|(implements, directives)| Self::Directives {
          implements,
          directives,
        }),
      implement_interfaces_parser().map(Self::Implements),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  interface: keywords::Type<Span>,
  name: Name<Span>,
  content: ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn extend(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  #[inline]
  pub const fn interface(&self) -> &keywords::Type<Span> {
    &self.interface
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.content
  }

  /// Consumes the components of this extension.
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Type<Span>,
    Name<Span>,
    ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
  ) {
    (
      self.span,
      self.extend,
      self.interface,
      self.name,
      self.content,
    )
  }

  pub fn parser_with<'src, I, E, FDP, DP, IP>(
    implement_interfaces_parser: impl Fn() -> IP,
    directives_parser: impl Fn() -> DP,
    fields_definition_parser: impl Fn() -> FDP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Type::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(ObjectExtensionContent::<
        ImplementInterfaces,
        Directives,
        FieldsDefinition,
      >::parser_with(
        implement_interfaces_parser,
        directives_parser,
        fields_definition_parser,
      ))
      .map_with(|(((extend, interface), name), content), sp| Self {
        span: Spanned::from_map_extra(sp),
        extend,
        interface,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
