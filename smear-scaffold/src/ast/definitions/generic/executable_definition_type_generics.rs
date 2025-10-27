use core::marker::PhantomData;

use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{
    IterParser, Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra,
  },
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::punctuator::{LAngle, RAngle};

use std::vec::Vec;

/// A definition type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionTypeGenerics<Ident, Container = Vec<Ident>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
}

impl<Ident, Container> AsSpan<Span> for ExecutableDefinitionTypeGenerics<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExecutableDefinitionTypeGenerics<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExecutableDefinitionTypeGenerics<Ident, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Container> ExecutableDefinitionTypeGenerics<Ident, Container> {
  /// Creates a new `ExecutableDefinitionTypeGenerics` with the given parameters.
  #[inline]
  const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _ident: PhantomData,
    }
  }

  /// Returns the span of the type generics.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parameters of the type generics.
  #[inline]
  pub const fn params(&self) -> &Container {
    &self.params
  }

  /// Returns the mutable parameters of the type generics.
  #[inline]
  pub fn params_slice(&self) -> &[Ident]
  where
    Container: AsRef<[Ident]>,
  {
    self.params.as_ref()
  }

  /// Returns a parser for the definition type generics.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP>(
    ident_parser: IP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Container: ChumskyContainer<Ident>,
  {
    LAngle::parser()
      .ignore_then(ident_parser.repeated().at_least(1).collect())
      .then_ignore(RAngle::parser())
      .map_with(|params, exa| Self::new(exa.span(), params))
  }
}

impl<'a, Ident, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinitionTypeGenerics<Ident, Container>
where
  Ident: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: ChumskyContainer<Ident> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Ident::parser())
  }
}
