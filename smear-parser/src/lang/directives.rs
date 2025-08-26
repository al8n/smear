use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    source::{Char, Slice, Source},
  },
  ignored,
  punct::At,
  Name,
};

use core::marker::PhantomData;
use std::vec::Vec;

/// Represents a single directive in a GraphQL-style syntax.
///
/// A directive consists of an `@` symbol followed by a name and optional arguments.
/// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
#[derive(Debug, Clone, Copy)]
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
  fn into_span(self) -> Span {
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
  /// Returns a reference to the span covering the entire directive.
  ///
  /// The span includes the `@` symbol, name, and arguments (if present).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `@` symbol that starts this directive.
  #[inline]
  pub const fn at(&self) -> &At<Span> {
    &self.at
  }

  /// Returns a reference to the directive's name.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
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

  /// Creates a parser that can parse a single directive with custom argument parsing.
  ///
  /// The parser expects the following sequence:
  /// 1. An `@` symbol
  /// 2. Optional ignored content (whitespace, comments)
  /// 3. A directive name
  /// 4. Optional ignored content
  /// 5. Optional arguments (parsed by the provided `args_parser`)
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the directive.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(args_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Args, E> + Clone,
  {
    At::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .then(ignored().ignore_then(args_parser).or_not())
      .map_with(|((at, name), arguments), sp| Self {
        span: Span::from_map_extra(sp),
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
  fn into_span(self) -> Span {
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

  /// Creates a parser that can parse one or more directives.
  ///
  /// This parser implements the `Directive+` production rule, meaning it requires
  /// at least one directive but can parse multiple consecutive directives.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the directives.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(directive: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Directive, E> + Clone,
    Container: chumsky::container::Container<Directive>,
  {
    directive
      .padded_by(ignored())
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
