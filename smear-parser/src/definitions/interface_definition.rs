use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{ignored, keywords, punct::Ampersand, Name, StringValue},
  source::{Char, Slice, Source},
};

#[derive(Debug, Clone)]
pub struct LeadingImplementInterface<Span> {
  span: Span,
  amp: Option<Ampersand<Span>>,
  name: Name<Span>,
}

impl<Span> AsRef<Span> for LeadingImplementInterface<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for LeadingImplementInterface<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for LeadingImplementInterface<Span> {
  type Components = (Span, Option<Ampersand<Span>>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.amp, self.name)
  }
}

impl<Span> LeadingImplementInterface<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn ampersand(&self) -> Option<&Ampersand<Span>> {
    self.amp.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Ampersand::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Span::from_map_extra(sp),
        amp,
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct ImplementInterface<Span> {
  span: Span,
  amp: Ampersand<Span>,
  name: Name<Span>,
}

impl<Span> AsRef<Span> for ImplementInterface<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for ImplementInterface<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for ImplementInterface<Span> {
  type Components = (Span, Ampersand<Span>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.amp, self.name)
  }
}

impl<Span> ImplementInterface<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn ampersand(&self) -> &Ampersand<Span> {
    &self.amp
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Subsequent members: `& Name`  (amp is **required**)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Ampersand::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(amp, name), sp| Self {
        span: Span::from_map_extra(sp),
        amp,
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct ImplementInterfaces<Span, Container = Vec<ImplementInterface<Span>>> {
  span: Span,
  implements: keywords::Implements<Span>,
  leading: LeadingImplementInterface<Span>,
  remaining: Container,
}

impl<Span, Container> AsRef<Span> for ImplementInterfaces<Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span, Container> IntoSpan<Span> for ImplementInterfaces<Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span, Container> IntoComponents for ImplementInterfaces<Span, Container> {
  type Components = (
    Span,
    keywords::Implements<Span>,
    LeadingImplementInterface<Span>,
    Container,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.implements, self.leading, self.remaining)
  }
}

impl<Span, Container> ImplementInterfaces<Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn implements(&self) -> &keywords::Implements<Span> {
    &self.implements
  }

  #[inline]
  pub const fn leading(&self) -> &LeadingImplementInterface<Span> {
    &self.leading
  }

  #[inline]
  pub const fn remaining(&self) -> &Container {
    &self.remaining
  }

  /// Parses a list of implemented interfaces.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Container: chumsky::container::Container<ImplementInterface<Span>>,
  {
    keywords::Implements::<Span>::parser()
      .then_ignore(ignored())
      .then(LeadingImplementInterface::parser())
      .then(
        ImplementInterface::parser()
          .padded_by(ignored())
          .repeated()
          .collect(),
      )
      .map_with(|((implements, leading), remaining), sp| Self {
        span: Span::from_map_extra(sp),
        implements,
        leading,
        remaining,
      })
  }
}

#[derive(Debug, Clone)]
pub struct InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  interface: keywords::Interface<Span>,
  name: Name<Span>,
  implements: Option<ImplementInterfaces>,
  directives: Option<Directives>,
  fields_definition: Option<FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> AsRef<Span>
  for InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoSpan<Span>
  for InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoComponents
  for InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Interface<Span>,
    Name<Span>,
    Option<ImplementInterfaces>,
    Option<Directives>,
    Option<FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
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
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  InterfaceDefinition<ImplementInterfaces, Directives, FieldsDefinition, Span>
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
  pub const fn interface(&self) -> &keywords::Interface<Span> {
    &self.interface
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
    keywords::Interface<Span>,
    Name<Span>,
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
    fields_definition_parser: FDP,
    directives_parser: DP,
    implement_interfaces_parser: IP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    FDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    IP: Parser<'src, I, ImplementInterfaces, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(keywords::Interface::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then(implement_interfaces_parser.padded_by(ignored()).or_not())
      .then(directives_parser.padded_by(ignored()).or_not())
      .then(fields_definition_parser.padded_by(ignored()).or_not())
      .map_with(
        |(((((description, interface), name), implements), directives), fields), sp| Self {
          span: Span::from_map_extra(sp),
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
pub struct InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  interface: keywords::Interface<Span>,
  name: Name<Span>,
  content: InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> AsRef<Span>
  for InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoSpan<Span>
  for InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span> IntoComponents
  for InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Interface<Span>,
    Name<Span>,
    InterfaceExtensionContent<ImplementInterfaces, Directives, FieldsDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.extend,
      self.interface,
      self.name,
      self.content,
    )
  }
}

impl<ImplementInterfaces, Directives, FieldsDefinition, Span>
  InterfaceExtension<ImplementInterfaces, Directives, FieldsDefinition, Span>
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
  pub const fn interface(&self) -> &keywords::Interface<Span> {
    &self.interface
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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
    Span,
    keywords::Extend<Span>,
    keywords::Interface<Span>,
    Name<Span>,
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

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
        span: Span::from_map_extra(sp),
        extend,
        interface,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
