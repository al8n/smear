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
  fqdp: bool, // fully qualified path
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
    (self.span, self.fqdp, self.segments)
  }
}

impl<Ident, Container> Path<Ident, Container> {
  /// Creates a new path from the given segments.
  #[inline]
  const fn new(span: Span, segments: Container, fqdp: bool) -> Self {
    Self {
      span,
      segments,
      fqdp,
      _s: PhantomData,
    }
  }

  /// Returns the segments of the path.
  #[inline]
  pub const fn segments(&self) -> &Container {
    &self.segments
  }

  /// Returns `true` if the path is fully qualified (i.e., starts with `::`).
  #[inline]
  pub const fn is_fully_qualified(&self) -> bool {
    self.fqdp
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
      .map_with(|(leading, segments), exa| Self::new(exa.span(), segments, leading.is_some()))
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

impl<Ident, Container> PartialEq<Path<Ident, Container>> for str
where
  str: Equivalent<Ident>,
  Container: AsRef<[Ident]>,
{
  #[inline]
  fn eq(&self, other: &Path<Ident, Container>) -> bool {
    <Self as Equivalent<Path<Ident, Container>>>::equivalent(self, other)
  }
}

impl<Ident, Container> PartialEq<str> for Path<Ident, Container>
where
  str: Equivalent<Ident>,
  Container: AsRef<[Ident]>,
{
  #[inline]
  fn eq(&self, other: &str) -> bool {
    <str as Equivalent<Path<Ident, Container>>>::equivalent(other, self)
  }
}

impl<Ident, Container> Equivalent<Path<Ident, Container>> for str
where
  str: Equivalent<Ident>,
  Container: AsRef<[Ident]>,
{
  #[inline]
  fn equivalent(&self, other: &Path<Ident, Container>) -> bool {
    // 1) leading `::` must match the path's FQP flag
    let self_fqp = self.starts_with("::");
    if self_fqp != other.is_fully_qualified() {
      return false;
    }

    // 2) strip the leading `::` (if any)
    let body = if self_fqp { &self[2..] } else { self };

    // 3) iterate segments without allocating
    let mut parts = body.split("::");
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

impl<Ident, Container> core::fmt::Display for Path<Ident, Container>
where
  Ident: core::fmt::Display,
  Container: AsRef<[Ident]>,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    if self.is_fully_qualified() {
      f.write_str("::")?;
    }
    let mut first = true;
    for segment in self.as_slice() {
      if !first {
        f.write_str("::")?;
      }
      first = false;
      core::fmt::Display::fmt(segment, f)?;
    }
    Ok(())
  }
}
