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
pub struct ScalarDefinition<Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  scalar: keywords::Scalar<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> ScalarDefinition<Directives, Span> {
  /// The span of the scalar definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The description of the scalar definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// The keyword of the scalar definition.
  #[inline]
  pub const fn scalar_keyword(&self) -> &keywords::Scalar<Span> {
    &self.scalar
  }

  /// The name of the scalar definition.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// The directives of the scalar definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Consumes the scalar definition, returning its components.
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    keywords::Scalar<Span>,
    Name<Span>,
    Option<Directives>,
  ) {
    (
      self.span,
      self.description,
      self.scalar,
      self.name,
      self.directives,
    )
  }

  /// Returns a parser for the input value definition.
  #[inline]
  pub fn parser_with<'src, I, E, DP>(directives_parser: DP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Scalar::<Span>::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .map_with(|(((description, scalar), name), directives), sp| Self {
        span: Spanned::from_map_extra(sp),
        description,
        scalar,
        name,
        directives,
      })
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub struct ScalarExtension<Directives, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  scalar: keywords::Scalar<Span>,
  name: Name<Span>,
  directives: Directives,
}

impl<Directives, Span> ScalarExtension<Directives, Span> {
  /// The span of the scalar definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The extend keyword of the scalar extension.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// The scalar keyword of the scalar extension.
  #[inline]
  pub const fn scalar_keyword(&self) -> &keywords::Scalar<Span> {
    &self.scalar
  }

  /// The name of the scalar extension.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// The directives of the scalar extension.
  #[inline]
  pub const fn directives(&self) -> &Directives {
    &self.directives
  }

  /// Consumes the scalar extension, returning its components.
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Scalar<Span>,
    Name<Span>,
    Directives,
  ) {
    (
      self.span,
      self.extend,
      self.scalar,
      self.name,
      self.directives,
    )
  }

  /// Returns a parser for the input value definition.
  #[inline]
  pub fn parser_with<'src, I, E, DP>(
    directives_parser: impl FnOnce() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then(keywords::Scalar::<Span>::parser())
      .padded_by(ignored())
      .then(Name::<Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser())
      .map_with(|(((extend, scalar), name), directives), sp| Self {
        span: Spanned::from_map_extra(sp),
        extend,
        scalar,
        name,
        directives,
      })
      .padded_by(ignored())
  }
}
