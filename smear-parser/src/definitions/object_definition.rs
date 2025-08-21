use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::super::{
  char::Char,
  keywords,
  language::{ignored::ignored, input_value::StringValue},
  name::Name,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  ty: keywords::Type<Src, Span>,
  name: Name<Src, Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
  ObjectDefinition<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn type_keyword(&self) -> &keywords::Type<Src, Span> {
    &self.ty
  }

  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
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
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    keywords::Type<Src, Span>,
    Name<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
          span: Spanned::from(sp),
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
    I: StrInput<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
pub struct ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  interface: keywords::Type<Src, Span>,
  name: Name<Src, Span>,
  content: ObjectExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
  ObjectExtension<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn extend(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }

  #[inline]
  pub const fn interface(&self) -> &keywords::Type<Src, Span> {
    &self.interface
  }

  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
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
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Type<Src, Span>,
    Name<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
        span: Spanned::from(sp),
        extend,
        interface,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
