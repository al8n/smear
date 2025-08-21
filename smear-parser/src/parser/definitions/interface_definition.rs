use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords,
  language::{ignored::ignored, input_value::StringValue, punct::Ampersand},
  Char, Name, Spanned,
};

#[derive(Debug, Clone)]
pub struct ImplementInterface<Src, Span> {
  span: Spanned<Src, Span>,
  amp: Option<Ampersand<Src, Span>>,
  name: Name<Src, Span>,
}

impl<Src, Span> ImplementInterface<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn ampersand(&self) -> Option<&Ampersand<Src, Span>> {
    self.amp.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Ampersand::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Spanned::from(sp),
        amp,
        name,
      })
  }

  /// Subsequent members: `& Name`  (amp is **required**)
  pub fn parser_with_amp<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Ampersand::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Spanned::from(sp),
        amp: Some(amp),
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct ImplementInterfaces<Src, Span, Container = Vec<ImplementInterface<Src, Span>>> {
  span: Spanned<Src, Span>,
  implements: keywords::Implements<Src, Span>,
  interfaces: Container,
}

impl<Src, Span, Container> ImplementInterfaces<Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn implements(&self) -> &keywords::Implements<Src, Span> {
    &self.implements
  }

  #[inline]
  pub const fn interfaces(&self) -> &Container {
    &self.interfaces
  }

  #[inline]
  pub fn into_interfaces(self) -> Container {
    self.interfaces
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    keywords::Implements<Src, Span>,
    Container,
  ) {
    (self.span, self.implements, self.interfaces)
  }

  /// Parses a list of implemented interfaces.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    Container: chumsky::container::Container<ImplementInterface<Src, Span>>,
  {
    let ws = ignored();

    keywords::Implements::<Src, Span>::parser()
      .then_ignore(ws.clone())
      .then(
        ImplementInterface::parser()
          .then_ignore(ws.clone())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .map_with(|(implements, interfaces), sp| Self {
        span: Spanned::from(sp),
        implements,
        interfaces,
      })
  }
}

#[derive(Debug, Clone)]
pub struct InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  interface: keywords::Interface<Src, Span>,
  name: Name<Src, Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
  InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
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
  pub const fn interface(&self) -> &keywords::Interface<Src, Span> {
    &self.interface
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
    keywords::Interface<Src, Span>,
    Name<Src, Span>,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  ) {
    (
      self.span,
      self.description,
      self.interface,
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
    String::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Interface::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then(implement_interfaces_parser().padded_by(ignored()).or_not())
      .then(directives_parser().padded_by(ignored()).or_not())
      .then(fields_definition_parser().padded_by(ignored()).or_not())
      .map_with(
        |(((((description, interface), name), implements), directives), fields), sp| Self {
          span: Spanned::from(sp),
          description,
          name,
          directives,
          fields_definition: fields,
          interface,
          implements,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub enum InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
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
  InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>
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
pub struct InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  interface: keywords::Interface<Src, Span>,
  name: Name<Src, Span>,
  content: InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
  InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Src, Span>
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
  pub const fn interface(&self) -> &keywords::Interface<Src, Span> {
    &self.interface
  }

  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  #[inline]
  pub const fn content(
    &self,
  ) -> &InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition> {
    &self.content
  }

  /// Consumes the components of this extension.
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Interface<Src, Span>,
    Name<Src, Span>,
    InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
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
      .then(keywords::Interface::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(InterfaceExtensionContent::<
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
