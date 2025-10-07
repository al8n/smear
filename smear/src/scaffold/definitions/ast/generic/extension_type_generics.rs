use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{IterParser, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{LAngle, RAngle};

/// A extension type parameter with an optional default type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeParam<Ident> {
  span: Span,
  ident: Ident,
}

impl<Ident> AsSpan<Span> for ExtensionTypeParam<Ident> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident> IntoSpan<Span> for ExtensionTypeParam<Ident> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident> IntoComponents for ExtensionTypeParam<Ident> {
  type Components = (Span, Ident);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident)
  }
}

impl<Ident> ExtensionTypeParam<Ident> {
  /// Creates a new `ExtensionTypeParam` with the given identifier and optional default type.
  #[inline]
  const fn new(span: Span, ident: Ident) -> Self {
    Self { span, ident }
  }

  /// Returns the span of the type parameter.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the identifier of the type parameter.
  #[inline]
  pub const fn ident(&self) -> &Ident {
    &self.ident
  }

  /// Returns a parser for the extension type parameter.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP>(
    ident_parser: IP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone,
  {
    ident_parser.map_with(|ident, exa| Self::new(exa.span(), ident))
  }
}

impl<'a, Ident, I, T, Error> Parseable<'a, I, T, Error> for ExtensionTypeParam<Ident>
where
  Ident: Parseable<'a, I, T, Error> + 'a,
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

/// A extension type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U = String> # A type generics with two type parameters: `T` and `U` where `U` has a default type of `String`
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeGenerics<Ident, Container = Vec<ExtensionTypeParam<Ident>>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
}

impl<Ident, Container> AsSpan<Span> for ExtensionTypeGenerics<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ExtensionTypeGenerics<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for ExtensionTypeGenerics<Ident, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Container> ExtensionTypeGenerics<Ident, Container> {
  /// Creates a new `ExtensionTypeGenerics` with the given parameters.
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
  pub fn params_slice(&mut self) -> &[ExtensionTypeParam<Ident>]
  where
    Container: AsRef<[ExtensionTypeParam<Ident>]>,
  {
    self.params.as_ref()
  }

  /// Returns a parser for the extension type generics.
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
    Container: ChumskyContainer<ExtensionTypeParam<Ident>>,
  {
    LAngle::parser()
      .ignore_then(
        ExtensionTypeParam::parser_with(ident_parser)
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(RAngle::parser())
      .map_with(|params, exa| Self::new(exa.span(), params))
  }
}

impl<'a, Ident, Container, I, T, Error> Parseable<'a, I, T, Error>
  for ExtensionTypeGenerics<Ident, Container>
where
  Ident: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: ChumskyContainer<ExtensionTypeParam<Ident>> + 'a,
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
