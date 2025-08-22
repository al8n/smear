use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    name::Name,
    source::{Char, Slice, Source},
  },
  ignored::ignored,
  punct::Colon,
};

#[derive(Debug, Clone)]
pub struct Alias<Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
}

impl<Span> AsRef<Span> for Alias<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Alias<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for Alias<Span> {
  type Components = (Span, Name<Span>, Colon<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon)
  }
}

impl<Span> Alias<Span> {
  /// Returns the span of the alias.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name of the alias.
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns the colon of the alias.
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns a parser of the alias.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Name::<Span>::parser()
      .then_ignore(ignored())
      .then(Colon::parser())
      .map_with(|(name, colon), sp| Self {
        span: Span::from_map_extra(sp),
        name,
        colon,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Field<Args, Directives, SelectionSet, Span> {
  span: Span,
  alias: Option<Alias<Span>>,
  name: Name<Span>,
  arguments: Option<Args>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Args, Directives, SelectionSet, Span> AsRef<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Args, Directives, SelectionSet, Span> IntoSpan<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Args, Directives, SelectionSet, Span> IntoComponents
  for Field<Args, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<Alias<Span>>,
    Name<Span>,
    Option<Args>,
    Option<Directives>,
    Option<SelectionSet>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.alias,
      self.name,
      self.arguments,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Args, Directives, SelectionSet, Span> Field<Args, Directives, SelectionSet, Span> {
  /// Returns the span of the field
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the alias of the field
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Span>> {
    self.alias.as_ref()
  }

  /// Returns the span of the field name
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns the arguments of the field
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Returns the directives of the field
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the selection set of the field
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }

  /// Returns a parser of the field
  pub fn parser_with<'src, I, E, AP, DP, SP>(
    args: AP,
    directives: DP,
    selection_set: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    AP: Parser<'src, I, Args, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    Alias::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored())
      .then(args.or_not())
      .then_ignore(ignored())
      .then(directives.or_not())
      .then_ignore(ignored())
      .then(selection_set.or_not())
      .map_with(
        |((((alias, name), arguments), directives), selection_set), sp| Self {
          span: Span::from_map_extra(sp),
          alias,
          name,
          arguments,
          directives,
          selection_set,
        },
      )
  }
}
