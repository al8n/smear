use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::PathSeparator;

/// A GraphQLx path, which is a sequence of identifiers separated by `::`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path<Ident, Container = Vec<Ident>> {
  span: Span,
  fqp: bool,
  segments: Container,
  _s: PhantomData<Ident>,
}

impl<Ident, Container> AsSpan<Span> for Path<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for Path<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for Path<Ident, Container> {
  type Components = (Span, bool, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.fqp, self.segments)
  }
}

impl<Ident, Container> Path<Ident, Container> {
  /// Creates a new path from the given segments.
  #[inline]
  const fn new(span: Span, fqp: bool, segments: Container) -> Self {
    Self {
      span,
      fqp,
      segments,
      _s: PhantomData,
    }
  }

  /// Returns whether the path is fully qualified (starts with `::`).
  #[inline]
  pub const fn is_fully_qualified(&self) -> bool {
    self.fqp
  }

  /// Returns the segments of the path.
  #[inline]
  pub const fn segments(&self) -> &Container {
    &self.segments
  }

  /// Returns the slice of the segments of the path.
  #[inline]
  pub fn as_slice(&self) -> &[Ident]
  where
    Container: AsRef<[Ident]>,
  {
    self.segments.as_ref()
  }

  /// Returns the span of the path.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Creates a parser for the path.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP>(
    ident_parser: IP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error>,
    IP: Parser<'a, I, Ident, E> + Clone,
    Container: chumsky::container::Container<Ident>,
  {
    PathSeparator::parser()
      .or_not()
      .then(
        ident_parser
          .separated_by(PathSeparator::parser())
          .at_least(1)
          .collect(),
      )
      .map_with(|(sep, segments), exa| Self::new(exa.span(), sep.is_some(), segments))
  }
}

impl<'a, Ident, Container, I, T, Error> Parseable<'a, I, T, Error> for Path<Ident, Container>
where
  Container: chumsky::container::Container<Ident>,
  Ident: Parseable<'a, I, T, Error>,
  PathSeparator: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    T: Token<'a>,
  {
    Self::parser_with(Ident::parser())
  }
}
