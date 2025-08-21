use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::super::{
  char::Char,
  keywords,
  language::{
    ignored::ignored,
    input_value::{EnumValue, StringValue},
    punct::{LBrace, RBrace},
  },
  name::Name,
  spanned::Spanned,
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct EnumValueDefinition<Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  enum_value: EnumValue<Src, Span>,
  directives: Option<Directives>,
}

impl<Directives, Src, Span> EnumValueDefinition<Directives, Src, Span> {
  /// The span of the enum value definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The description of the enum value definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  /// The enum value of the enum value definition.
  #[inline]
  pub const fn enum_value(&self) -> &EnumValue<Src, Span> {
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(EnumValue::<Src, Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .map_with(|((description, enum_value), directives), sp| Self {
        span: Spanned::from(sp),
        description,
        enum_value,
        directives,
      })
  }
}

#[derive(Debug, Clone)]
pub struct EnumValuesDefinition<
  EnumValueDefinition,
  Src,
  Span,
  Container = Vec<EnumValueDefinition>,
> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  enum_values: Container,
  _m: PhantomData<EnumValueDefinition>,
}

impl<EnumValueDefinition, Src, Span, Container>
  EnumValuesDefinition<EnumValueDefinition, Src, Span, Container>
{
  /// The span of the enum values definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The left brace of the enum values definition.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }

  /// The right brace of the enum values definition.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }

  /// The enum values of the enum values definition.
  #[inline]
  pub const fn enum_values(&self) -> &Container {
    &self.enum_values
  }

  /// Consumes the enum values definition, returning its components
  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    LBrace<Src, Span>,
    RBrace<Src, Span>,
    Container,
  ) {
    (self.span, self.l_brace, self.r_brace, self.enum_values)
  }

  /// Returns a parser for the enum definition.
  #[inline]
  pub fn parser_with<'src, I, E, P>(enum_value_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
        span: Spanned::from(sp),
        l_brace,
        r_brace,
        enum_values,
        _m: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct EnumDefinition<Directives, EnumValuesDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  keyword: keywords::Enum<Src, Span>,
  name: Name<Src, Span>,
  enum_values: Option<EnumValuesDefinition>,
  directives: Option<Directives>,
}

impl<Directives, EnumValuesDefinition, Src, Span>
  EnumDefinition<Directives, EnumValuesDefinition, Src, Span>
{
  /// The span of the enum definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The description of the enum definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  /// The span of the name of the enum definition
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// The enum keyword of the enum definition.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Src, Span> {
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
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    keywords::Enum<Src, Span>,
    Name<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
      .then(Name::<Src, Span>::parser())
      .then_ignore(ws.clone())
      .then(directives_parser.or_not())
      .then_ignore(ws.clone())
      .then(enum_values_definition.or_not())
      .map_with(
        |((((description, keyword), name), directives), enum_values), sp| Self {
          span: Spanned::from(sp),
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
    I: StrInput<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
pub struct EnumExtension<Directives, EnumValuesDefinition, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  keyword: keywords::Enum<Src, Span>,
  name: Name<Src, Span>,
  content: EnumExtensionContent<Directives, EnumValuesDefinition>,
}

impl<Directives, EnumValuesDefinition, Src, Span>
  EnumExtension<Directives, EnumValuesDefinition, Src, Span>
{
  /// The span of the enum definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The span of the name of the enum definition
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// The extend keyword of the enum extension
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }

  /// The enum keyword of the enum definition.
  #[inline]
  pub const fn enum_keyword(&self) -> &keywords::Enum<Src, Span> {
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
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Enum<Src, Span>,
    Name<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    EVP: Parser<'src, I, EnumValuesDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Enum::parser())
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser().then_ignore(ignored()))
      .then(EnumExtensionContent::parser_with(
        directives_parser,
        enum_values_definition,
      ))
      .map_with(|(((extend, keyword), name), content), sp| Self {
        span: Spanned::from(sp),
        extend,
        keyword,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
