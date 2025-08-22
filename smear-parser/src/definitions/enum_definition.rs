use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  convert::*,
  keywords,
  language::{
    ignored::ignored,
    input_value::{EnumValue, StringValue},
    punct::{LBrace, RBrace},
  },
  name::Name,
  source::{Char, Slice, Source},
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct EnumValueDefinition<Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  enum_value: EnumValue<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> AsRef<Span> for EnumValueDefinition<Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, Span> IntoSpan<Span> for EnumValueDefinition<Directives, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Directives, Span> IntoComponents for EnumValueDefinition<Directives, Span> {
  type Components = (
    Span,
    Option<StringValue<Span>>,
    EnumValue<Span>,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.enum_value,
      self.directives,
    )
  }
}

impl<Directives, Span> EnumValueDefinition<Directives, Span> {
  /// The span of the enum value definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The description of the enum value definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// The enum value of the enum value definition.
  #[inline]
  pub const fn enum_value(&self) -> &EnumValue<Span> {
    &self.enum_value
  }

  /// The directives of the enum value definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a parser for the input value definition.
  #[inline]
  pub fn parser_with<'src, I, E, DP>(directives_parser: DP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(EnumValue::<Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .map_with(|((description, enum_value), directives), sp| Self {
        span: Span::from_map_extra(sp),
        description,
        enum_value,
        directives,
      })
  }
}

#[derive(Debug, Clone)]
pub struct EnumValuesDefinition<EnumValueDefinition, Span, Container = Vec<EnumValueDefinition>> {
  span: Span,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  enum_values: Container,
  _m: PhantomData<EnumValueDefinition>,
}

impl<EnumValueDefinition, Span, Container> AsRef<Span>
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<EnumValueDefinition, Span, Container> IntoSpan<Span>
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<EnumValueDefinition, Span, Container> IntoComponents
  for EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.enum_values, self.r_brace)
  }
}

impl<EnumValueDefinition, Span, Container>
  EnumValuesDefinition<EnumValueDefinition, Span, Container>
{
  /// The span of the enum values definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The left brace of the enum values definition.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// The right brace of the enum values definition.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// The enum values of the enum values definition.
  #[inline]
  pub const fn enum_values(&self) -> &Container {
    &self.enum_values
  }

  /// Returns a parser for the enum definition.
  #[inline]
  pub fn parser_with<'src, I, E, P>(enum_value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, EnumValueDefinition, E> + Clone,
    Container: chumsky::container::Container<EnumValueDefinition>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        enum_value_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(ignored())
      .then(RBrace::parser())
      .map_with(|((l_brace, enum_values), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        r_brace,
        enum_values,
        _m: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct EnumDefinition<Directives, EnumValuesDefinition, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  keyword: keywords::Enum<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
  enum_values: Option<EnumValuesDefinition>,
}

impl<Directives, EnumValuesDefinition, Span> AsRef<Span>
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoSpan<Span>
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Directives, EnumValuesDefinition, Span> IntoComponents
  for EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Enum<Span>,
    Name<Span>,
    Option<Directives>,
    Option<EnumValuesDefinition>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.keyword,
      self.name,
      self.directives,
      self.enum_values,
    )
  }
}

impl<Directives, EnumValuesDefinition, Span>
  EnumDefinition<Directives, EnumValuesDefinition, Span>
{
  /// The span of the enum definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The description of the enum definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// The span of the name of the enum definition
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// The enum keyword of the enum definition.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    &self.keyword
  }

  /// The enum values of the enum definition.
  #[inline]
  pub const fn enum_values(&self) -> Option<&EnumValuesDefinition> {
    self.enum_values.as_ref()
  }

  /// The directives of the enum definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Consumes the enum definition, returning its components
  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    keywords::Enum<Span>,
    Name<Span>,
    Option<EnumValuesDefinition>,
    Option<Directives>,
  ) {
    (
      self.span,
      self.description,
      self.keyword,
      self.name,
      self.enum_values,
      self.directives,
    )
  }

  /// Returns a parser for the enum definition.
  #[inline]
  pub fn parser_with<'src, I, E, P, DP>(
    enum_values_definition: P,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    P: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = ignored();

    // Head: Description? enum Name Directives[Const]? EnumValuesDefinition?
    StringValue::parser()
      .then_ignore(ws.clone())
      .or_not()
      .then(keywords::Enum::parser())
      .then_ignore(ws.clone())
      .then(Name::<Span>::parser())
      .then_ignore(ws.clone())
      .then(directives_parser.or_not())
      .then_ignore(ws.clone())
      .then(enum_values_definition.or_not())
      .map_with(
        |((((description, keyword), name), directives), enum_values), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          keyword,
          name,
          directives,
          enum_values,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub enum EnumExtensionContent<Directives, EnumValuesDefinition> {
  Values {
    directives: Option<Directives>,
    values: EnumValuesDefinition,
  },
  Directives(Directives),
}

impl<Directives, EnumValuesDefinition> EnumExtensionContent<Directives, EnumValuesDefinition> {
  pub fn parser_with<'src, I, E, DP, EVP>(
    directives_parser: impl Fn() -> DP,
    enum_values_parser: impl Fn() -> EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,

    DP: Parser<'src, I, Directives, E> + Clone,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
  {
    choice((
      directives_parser()
        .then_ignore(ignored())
        .or_not()
        .then(enum_values_parser())
        .map(|(directives, values)| Self::Values { directives, values }),
      directives_parser().map(Self::Directives),
    ))
  }
}

#[derive(Debug, Clone)]
pub struct EnumExtension<Directives, EnumValuesDefinition, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  keyword: keywords::Enum<Span>,
  name: Name<Span>,
  content: EnumExtensionContent<Directives, EnumValuesDefinition>,
}

impl<Directives, EnumValuesDefinition, Span> EnumExtension<Directives, EnumValuesDefinition, Span> {
  /// The span of the enum definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The span of the name of the enum definition
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// The extend keyword of the enum extension
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// The enum keyword of the enum definition.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Span> {
    &self.keyword
  }

  /// The enum values of the enum definition.
  #[inline]
  pub const fn content(&self) -> &EnumExtensionContent<Directives, EnumValuesDefinition> {
    &self.content
  }

  /// Consumes the enum definition, returning its components
  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Enum<Span>,
    Name<Span>,
    EnumExtensionContent<Directives, EnumValuesDefinition>,
  ) {
    (
      self.span,
      self.extend,
      self.keyword,
      self.name,
      self.content,
    )
  }

  /// Returns a parser for the enum definition.
  #[inline]
  pub fn parser_with<'src, I, E, DP, EVP>(
    directives_parser: impl Fn() -> DP,
    enum_values_definition: impl Fn() -> EVP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Enum::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser().then_ignore(ignored()))
      .then(EnumExtensionContent::parser_with(
        directives_parser,
        enum_values_definition,
      ))
      .map_with(|(((extend, keyword), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        keyword,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
