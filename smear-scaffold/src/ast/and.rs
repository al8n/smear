use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

/// A simple pair structure representing two sequentially parsed elements.
/// This is useful for parsers that need to return two related values together.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct And<A, B> {
  span: Span,
  first: A,
  second: B,
}

impl<A, B> AsSpan<Span> for And<A, B> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<A, B> IntoSpan<Span> for And<A, B> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<A, B> IntoComponents for And<A, B> {
  type Components = (Span, A, B);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.first, self.second)
  }
}

impl<A, B> And<A, B> {
  /// Creates a new `And` with the given elements.
  #[inline]
  const fn new(span: Span, first: A, second: B) -> Self {
    Self {
      span,
      first,
      second,
    }
  }

  /// Returns the span of the pair.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the first element of the pair.
  #[inline]
  pub const fn first(&self) -> &A {
    &self.first
  }

  /// Returns the second element of the pair.
  #[inline]
  pub const fn second(&self) -> &B {
    &self.second
  }

  /// Creates a parser for a sequential pair of elements.
  ///
  /// The resulting parser will parse the first element using `first_parser`
  /// and the second element using `second_parser`.
  pub fn parser_with<'a, I, T, Error, E, FP, SP>(
    first_parser: FP,
    second_parser: SP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    FP: Parser<'a, I, A, E> + Clone + 'a,
    SP: Parser<'a, I, B, E> + Clone + 'a,
  {
    first_parser
      .then(second_parser)
      .map_with(|(first, second), exa| Self::new(exa.span(), first, second))
  }
}

impl<'a, A, B, I, T, Error> Parseable<'a, I, T, Error> for And<A, B>
where
  A: Parseable<'a, I, T, Error>,
  B: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(A::parser(), B::parser())
  }
}
