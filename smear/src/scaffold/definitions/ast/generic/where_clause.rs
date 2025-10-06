use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{IterParser, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::{
  keywords::Where,
  punctuator::{Ampersand, Colon, LAngle, PathSeparator, RAngle},
};

use super::{super::Path, TypeGenerics};

/// A GraphQLx predicate type path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PredicateTypePath<
  Ident,
  Type,
  PathSegmentContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
> {
  span: Span,
  path: Path<Ident, PathSegmentContainer>,
  generics: Option<TypeGenerics<Type, TypeContainer>>,
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> AsSpan<Span>
  for PredicateTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoSpan<Span>
  for PredicateTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer> IntoComponents
  for PredicateTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  type Components = (
    Span,
    Path<Ident, PathSegmentContainer>,
    Option<TypeGenerics<Type, TypeContainer>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.path, self.generics)
  }
}

impl<Ident, Type, PathSegmentContainer, TypeContainer>
  PredicateTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
{
  /// Creates a new path from the given segments.
  #[inline]
  const fn new(
    span: Span,
    path: Path<Ident, PathSegmentContainer>,
    generics: Option<TypeGenerics<Type, TypeContainer>>,
  ) -> Self {
    Self {
      span,
      path,
      generics,
    }
  }

  /// Returns the path.
  #[inline]
  pub const fn path(&self) -> &Path<Ident, PathSegmentContainer> {
    &self.path
  }

  /// Returns the type generics.
  #[inline]
  pub const fn type_generics(&self) -> Option<&TypeGenerics<Type, TypeContainer>> {
    self.generics.as_ref()
  }

  /// Returns the span of the path.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Creates a parser for the path.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error>,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    PathSegmentContainer: ChumskyContainer<Ident>,
    TypeContainer: ChumskyContainer<Type>,
  {
    Path::parser_with(ident_parser)
      .then(TypeGenerics::parser_with(type_parser).or_not())
      .map_with(|(path, generics), exa| Self::new(exa.span(), path, generics))
  }
}

impl<'a, Ident, Type, PathSegmentContainer, TypeContainer, I, T, Error> Parseable<'a, I, T, Error>
  for PredicateTypePath<Ident, Type, PathSegmentContainer, TypeContainer>
where
  PathSegmentContainer: ChumskyContainer<Ident>,
  TypeContainer: ChumskyContainer<Type>,
  Ident: Parseable<'a, I, T, Error>,
  PathSeparator: Parseable<'a, I, T, Error>,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Type: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Ident::parser(), Type::parser())
  }
}

/// A where predicate, which constrains a type to implement certain interfaces.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<Ident, Type, Container = Vec<PredicateTypePath<Ident, Type>>> {
  span: Span,
  bounded_type: PredicateTypePath<Ident, Type>,
  bounds: Container,
}

impl<Ident, Type, Container> AsSpan<Span> for WherePredicate<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for WherePredicate<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for WherePredicate<Ident, Type, Container> {
  type Components = (Span, PredicateTypePath<Ident, Type>, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.bounded_type, self.bounds)
  }
}

impl<Ident, Type, Container> WherePredicate<Ident, Type, Container> {
  /// Creates a new `WherePredicate` with the given bounded type and bounds.
  #[inline]
  const fn new(
    span: Span,
    bounded_type: PredicateTypePath<Ident, Type>,
    bounds: Container,
  ) -> Self {
    Self {
      span,
      bounded_type,
      bounds,
    }
  }

  /// Returns the span of the where predicate.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the bounded type.
  #[inline]
  pub const fn bounded_type(&self) -> &PredicateTypePath<Ident, Type> {
    &self.bounded_type
  }

  /// Returns the bounds.
  #[inline]
  pub const fn bounds(&self) -> &Container {
    &self.bounds
  }

  /// Returns the bounds as a slice.
  #[inline]
  pub fn bounds_slice(&self) -> &[PredicateTypePath<Ident, Type>]
  where
    Container: AsRef<[PredicateTypePath<Ident, Type>]>,
  {
    self.bounds.as_ref()
  }

  /// Returns a parser for the where predicate.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    Container: ChumskyContainer<PredicateTypePath<Ident, Type>> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    Ampersand: Parseable<'a, I, T, Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
  {
    PredicateTypePath::parser_with(ident_parser.clone(), type_parser.clone())
      .then_ignore(Colon::parser())
      .then(
        PredicateTypePath::parser_with(ident_parser, type_parser)
          .separated_by(Ampersand::parser())
          .allow_leading()
          .at_least(1)
          .collect(),
      )
      .map_with(|(bounded_type, bounds), exa| Self::new(exa.span(), bounded_type, bounds))
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<Ident, Type, Container = Vec<WherePredicate<Ident, Type>>> {
  span: Span,
  predicates: Container,
  _m: PhantomData<Ident>,
  _n: PhantomData<Type>,
}

impl<Ident, Type, Container> AsSpan<Span> for WhereClause<Ident, Type, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Container> IntoSpan<Span> for WhereClause<Ident, Type, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Container> IntoComponents for WhereClause<Ident, Type, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.predicates)
  }
}

impl<Ident, Type, Container> WhereClause<Ident, Type, Container> {
  /// Creates a new `WhereClause` with the given predicates.
  #[inline]
  const fn new(span: Span, predicates: Container) -> Self {
    Self {
      span,
      predicates,
      _m: PhantomData,
      _n: PhantomData,
    }
  }

  /// Returns the span of the where clause.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the predicates.
  #[inline]
  pub const fn predicates(&self) -> &Container {
    &self.predicates
  }

  /// Returns the predicates as a slice.
  #[inline]
  pub fn predicates_slice(&self) -> &[WherePredicate<Ident, Type>]
  where
    Container: AsRef<[WherePredicate<Ident, Type>]>,
  {
    self.predicates.as_ref()
  }

  /// Returns a parser for the where clause.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP>(
    ident_parser: IP,
    type_parser: TP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    Ident: 'a,
    Type: 'a,
    Container: ChumskyContainer<WherePredicate<Ident, Type>> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    Ampersand: Parseable<'a, I, T, Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Where: Parseable<'a, I, T, Error> + 'a,
  {
    Where::parser()
      .ignore_then(
        WherePredicate::parser_with(ident_parser, type_parser)
          .repeated()
          .collect(),
      )
      .map_with(|predicates, exa| Self::new(exa.span(), predicates))
  }
}

impl<'a, Ident, Type, Container, I, T, Error> Parseable<'a, I, T, Error>
  for WhereClause<Ident, Type, Container>
where
  Container: ChumskyContainer<WherePredicate<Ident, Type>>,
  WherePredicate<Ident, Type>: Parseable<'a, I, T, Error>,
  Where: Parseable<'a, I, T, Error>,
  Ident: Parseable<'a, I, T, Error>,
  Type: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error> + 'a,
  Ampersand: Parseable<'a, I, T, Error> + 'a,
  PathSeparator: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Where: Parseable<'a, I, T, Error> + 'a,
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
    Self::parser_with(Ident::parser(), Type::parser())
  }
}
