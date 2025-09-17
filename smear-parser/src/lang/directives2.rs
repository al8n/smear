use chumsky::{extra::ParserExtra, IterParser, Parser};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};

use super::{super::source::*, punctuator::At};

use core::marker::PhantomData;
use std::vec::Vec;

/// Represents a single directive in a GraphQL-style syntax.
///
/// A directive consists of an `@` symbol followed by a name and optional arguments.
/// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
#[derive(Debug, Clone, Copy)]
pub struct Directive<Name, Args> {
  span: Span,
  at: At,
  name: Name,
  arguments: Option<Args>,
}

impl<Name, Args> AsRef<Span> for Directive<Name, Args> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Args> IntoSpan<Span> for Directive<Name, Args> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Args> IntoComponents for Directive<Name, Args> {
  type Components = (Span, At, Name, Option<Args>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.at, self.name, self.arguments)
  }
}

impl<Name, Args> Directive<Name, Args> {
  /// Returns a reference to the span covering the entire directive.
  ///
  /// The span includes the `@` symbol, name, and arguments (if present).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `@` symbol that starts this directive.
  #[inline]
  pub const fn at(&self) -> &At {
    &self.at
  }

  /// Returns a reference to the directive's name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directive's arguments, if present.
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Consumes the directive and returns its arguments, if present.
  #[inline]
  pub fn into_arguments(self) -> Option<Args> {
    self.arguments
  }
}

impl<'a, Name, Args, I, T, Error> Parseable<'a, I, T, Error> for Directive<Name, Args>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Args: Parseable<'a, I, T, Error>,
  At: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  Error: 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a
  {
    At::parser()
      .then(Name::parser())
      .then(Args::parser().or_not())
      .map_with(|((at, name), arguments), exa| Self {
        span: exa.span(),
        at,
        name,
        arguments,
      })
  }
}

/// Represents a collection of one or more directives.
///
/// In GraphQL and similar languages, multiple directives can be applied to a single element.
/// This structure represents such a collection, maintaining the source span information
/// and providing access to all individual directives.
///
/// Spec: [Directives](https://spec.graphql.org/draft/#Directives)
#[derive(Debug, Clone, Copy)]
pub struct Directives<Directive, Container = Vec<Directive>> {
  span: Span,
  directives: Container,
  _directive: PhantomData<Directive>,
}

impl<Directive, Container> AsRef<Span> for Directives<Directive, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directive, Container> IntoSpan<Span> for Directives<Directive, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directive, Container> IntoComponents for Directives<Directive, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.directives)
  }
}

impl<Directive, Container> Directives<Directive, Container> {
  /// Returns a reference to the span covering all directives in the collection.
  ///
  /// The span encompasses from the start of the first directive to the end of the last directive.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all directives.
  #[inline]
  pub const fn directives(&self) -> &Container {
    &self.directives
  }

  /// Consumes the directives collection and returns the underlying container.
  #[inline]
  pub fn into_directives(self) -> Container {
    self.directives
  }
}

impl<'a, Directive, Container, I, T, Error> Parseable<'a, I, T, Error> for Directives<Directive, Container>
where
  T: Token<'a>,
  I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
  Directive: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<Directive>,
  Error: 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a
  {
    Directive::parser()
      .repeated()
      .at_least(1)
      .collect()
      .map_with(|directives, exa| Self {
        span: exa.span(),
        directives,
        _directive: PhantomData,
      })
  }
}
