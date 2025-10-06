use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{IterParser, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{Equal, LAngle, RAngle};

/// A declare type parameter with an optional default type.
///
/// ```graphqlx
/// T = String # A type parameter `T` with a default type of `String`
/// T # A type parameter `T` without a default type
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DeclareTypeParam<Ident, Type> {
  span: Span,
  ident: Ident,
  default: Option<Type>,
}

impl<Ident, Type> AsSpan<Span> for DeclareTypeParam<Ident, Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type> IntoSpan<Span> for DeclareTypeParam<Ident, Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type> IntoComponents for DeclareTypeParam<Ident, Type> {
  type Components = (Span, Ident, Option<Type>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ident, self.default)
  }
}

impl<Ident, Type> DeclareTypeParam<Ident, Type> {
  /// Creates a new `DeclareTypeParam` with the given identifier and optional default type.
  #[inline]
  const fn new(span: Span, ident: Ident, default: Option<Type>) -> Self {
    Self {
      span,
      ident,
      default,
    }
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

  /// Returns the optional default type of the type parameter.
  #[inline]
  pub const fn default(&self) -> Option<&Type> {
    self.default.as_ref()
  }

  /// Returns a parser for the declare type parameter.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    Equal: Parseable<'a, I, T, Error> + 'a,
  {
    ident_parser
      .then(Equal::parser().ignore_then(type_parser).or_not())
      .map_with(|(ident, default), exa| Self::new(exa.span(), ident, default))
  }
}

impl<'a, Ident, Type, I, T, Error> Parseable<'a, I, T, Error> for DeclareTypeParam<Ident, Type>
where
  Equal: Parseable<'a, I, T, Error> + 'a,
  Ident: Parseable<'a, I, T, Error> + 'a,
  Type: Parseable<'a, I, T, Error> + 'a,
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
    Self::parser_with(Ident::parser(), Type::parser())
  }
}

/// A declare type generics with a list of type parameters.
///
/// ```graphqlx
/// <T, U = String> # A type generics with two type parameters: `T` and `U` where `U` has a default type of `String`
/// <T, U> # A type generics with two type parameters: `T` and `U`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclareTypeGenerics<Ident, Type, Container = Vec<DeclareTypeParam<Ident, Type>>> {
  span: Span,
  params: Container,
  _ident: PhantomData<Ident>,
  _type: PhantomData<Type>,
}

impl<Ident, Type, Container> AsSpan<Span> for DeclareTypeGenerics<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for DeclareTypeGenerics<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for DeclareTypeGenerics<Ident, Type, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Ident, Type, Container> DeclareTypeGenerics<Ident, Type, Container> {
  /// Creates a new `DeclareTypeGenerics` with the given parameters.
  #[inline]
  const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _ident: PhantomData,
      _type: PhantomData,
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
  pub fn params_slice(&mut self) -> &[DeclareTypeParam<Ident, Type>]
  where
    Container: AsRef<[DeclareTypeParam<Ident, Type>]>,
  {
    self.params.as_ref()
  }

  /// Returns a parser for the declare type generics.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    Equal: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Container: ChumskyContainer<DeclareTypeParam<Ident, Type>>,
  {
    LAngle::parser()
      .ignore_then(
        DeclareTypeParam::parser_with(ident_parser, type_parser)
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(RAngle::parser())
      .map_with(|params, exa| Self::new(exa.span(), params))
  }
}

impl<'a, Ident, Type, Container, I, T, Error> Parseable<'a, I, T, Error>
  for DeclareTypeGenerics<Ident, Type, Container>
where
  Equal: Parseable<'a, I, T, Error> + 'a,
  Ident: Parseable<'a, I, T, Error> + 'a,
  Type: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: ChumskyContainer<DeclareTypeParam<Ident, Type>> + 'a,
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
    Self::parser_with(Ident::parser(), Type::parser())
  }
}
