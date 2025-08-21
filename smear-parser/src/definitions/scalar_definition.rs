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
pub struct ScalarDefinition<Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  scalar: keywords::Scalar<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
}

impl<Directives, Src, Span> ScalarDefinition<Directives, Src, Span> {
  /// The span of the scalar definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The description of the scalar definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  /// The keyword of the scalar definition.
  #[inline]
  pub const fn scalar_keyword(&self) -> &keywords::Scalar<Src, Span> {
    &self.scalar
  }

  /// The name of the scalar definition.
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
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
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    keywords::Scalar<Src, Span>,
    Name<Src, Span>,
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
      .then(keywords::Scalar::<Src, Span>::parser())
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .map_with(|(((description, scalar), name), directives), sp| Self {
        span: Spanned::from(sp),
        description,
        scalar,
        name,
        directives,
      })
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub struct ScalarExtension<Directives, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  scalar: keywords::Scalar<Src, Span>,
  name: Name<Src, Span>,
  directives: Directives,
}

impl<Directives, Src, Span> ScalarExtension<Directives, Src, Span> {
  /// The span of the scalar definition.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The extend keyword of the scalar extension.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }

  /// The scalar keyword of the scalar extension.
  #[inline]
  pub const fn scalar_keyword(&self) -> &keywords::Scalar<Src, Span> {
    &self.scalar
  }

  /// The name of the scalar extension.
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
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
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Scalar<Src, Span>,
    Name<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    keywords::Extend::parser()
      .then(keywords::Scalar::<Src, Span>::parser())
      .padded_by(ignored())
      .then(Name::<Src, Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser())
      .map_with(|(((extend, scalar), name), directives), sp| Self {
        span: Spanned::from(sp),
        extend,
        scalar,
        name,
        directives,
      })
      .padded_by(ignored())
  }
}
