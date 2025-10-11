use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, cmp::Equivalent},
};

use crate::punctuator::PathSeparator;

/// A GraphQLx path, which is a sequence of identifiers separated by `::`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path<Ident, Container = Vec<Ident>> {
  span: Span,
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
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.segments)
  }
}

impl<Ident, Container> Path<Ident, Container> {
  /// Creates a new path from the given segments.
  #[inline]
  const fn new(span: Span, segments: Container) -> Self {
    Self {
      span,
      segments,
      _s: PhantomData,
    }
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
    ident_parser
      .separated_by(PathSeparator::parser())
      .at_least(1)
      .collect()
      .map_with(|segments, exa| Self::new(exa.span(), segments))
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

impl<Ident, Container> Equivalent<Path<Ident, Container>> for str
where
  str: Equivalent<Ident>,
  Container: AsRef<[Ident]>,
{
  #[inline]
  fn equivalent(&self, other: &Path<Ident, Container>) -> bool {
    let mut parts = self.split("::");
    let mut segs = other.as_slice().iter();

    loop {
      match (parts.next(), segs.next()) {
        (None, None) => return true, // same length & all matched
        (Some(p), Some(ident)) => {
          // reject empty segments like "a::::b" or "::a" (after stripping FQP)
          if p.is_empty() || !p.equivalent(ident) {
            return false;
          }
        }
        // different lengths
        _ => return false,
      }
    }
  }
}
