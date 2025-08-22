use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    language::ignored::ignored,
    name::Name,
    source::{Char, Slice, Source},
  },
  punct::At,
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Directive<Args, Span> {
  span: Span,
  at: At<Span>,
  name: Name<Span>,
  arguments: Option<Args>,
}

impl<Args, Span> AsRef<Span> for Directive<Args, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Args, Span> IntoSpan<Span> for Directive<Args, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Args, Span> IntoComponents for Directive<Args, Span> {
  type Components = (Span, At<Span>, Name<Span>, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.at, self.name, self.arguments)
  }
}

impl<Args, Span> Directive<Args, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    &self.at
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  #[inline]
  pub fn into_arguments(self) -> Option<Args> {
    self.arguments
  }
}

impl<Args, Span> Directive<Args, Span> {
  pub fn parser_with<'src, I, E, P>(args_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Args, E> + Clone,
  {
    At::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored())
      .then(args_parser.or_not())
      .map_with(|((at, name), arguments), sp| Self {
        span: Span::from_map_extra(sp),
        at,
        name,
        arguments,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Directives<Directive, Span, Container = Vec<Directive>> {
  span: Span,
  directives: Container,
  _directive: PhantomData<Directive>,
}

impl<Directive, Span, Container> AsRef<Span> for Directives<Directive, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directive, Span, Container> IntoSpan<Span> for Directives<Directive, Span, Container> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Directive, Span, Container> IntoComponents for Directives<Directive, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<Directive, Span, Container> Directives<Directive, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
  /// `Directive+` (the nonterminal `Directives` in the spec is one-or-more).
  pub fn parser_with<'src, I, E, P>(directive: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Directive, E> + Clone,
    Container: chumsky::container::Container<Directive>,
  {
    directive
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|directives, sp| Self {
        span: Span::from_map_extra(sp),
        directives,
        _directive: PhantomData,
      })
  }
}
