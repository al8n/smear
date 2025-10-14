use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{LAngle, RAngle};

/// A type generics.
///
/// ## Example
///
/// The `User<ID, Username>` where `ID` and `Username` are type generic params, and `<ID, Username>` are the type generics.
///
/// ```graphqlx
/// type User<I, U = String, V = Int> {
///   id: I!,
///   name: U!,
///   age: V,
/// }
///
/// type Comment {
///   user: User<ID, Username>!,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenerics<Type, Container = Vec<Type>> {
  span: Span,
  params: Container,
  _type: PhantomData<Type>,
}

impl<Type, Container> AsSpan<Span> for TypeGenerics<Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type, Container> IntoSpan<Span> for TypeGenerics<Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type, Container> IntoComponents for TypeGenerics<Type, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

impl<Type, Container> TypeGenerics<Type, Container> {
  /// Creates a new `TypeGenerics` with the given parameters.
  #[inline]
  const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
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
  pub fn params_slice(&self) -> &[Type]
  where
    Container: AsRef<[Type]>,
  {
    self.params().as_ref()
  }

  /// Returns a parser for the type generics.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, TP>(type_parser: TP) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Container: chumsky::container::Container<Type>,
  {
    LAngle::parser()
      .ignore_then(type_parser.repeated().at_least(1).collect())
      .then_ignore(RAngle::parser())
      .map_with(|params, exa| Self::new(exa.span(), params))
  }
}

impl<'a, Type, Container, I, T, Error> Parseable<'a, I, T, Error> for TypeGenerics<Type, Container>
where
  Type: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Container: chumsky::container::Container<Type> + 'a,
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
    Self::parser_with(Type::parser())
  }
}
