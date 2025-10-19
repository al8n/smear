use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{IterParser, Parser, container::Container as ChumskyContainer, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use smear_lexer::{
  keywords::Where,
  punctuator::{Ampersand, Colon, LAngle, PathSeparator, RAngle},
};

use super::TypePath;

use std::vec::Vec;

/// A where predicate, which constrains a type to implement certain interfaces.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<
  Ident,
  Type,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  Container = Vec<TypePath<Ident, Type>>,
> {
  span: Span,
  bounded_type: TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
  bounds: Container,
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> AsSpan<Span>
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> IntoSpan<Span>
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container> IntoComponents
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  type Components = (
    Span,
    TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
    Container,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.bounded_type, self.bounds)
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
  WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
{
  /// Creates a new `WherePredicate` with the given bounded type and bounds.
  #[inline]
  const fn new(
    span: Span,
    bounded_type: TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>,
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
  pub const fn bounded_type(&self) -> &TypePath<Ident, Type, PathSegmentsContainer, TypeContainer> {
    &self.bounded_type
  }

  /// Returns the bounds.
  #[inline]
  pub const fn bounds(&self) -> &Container {
    &self.bounds
  }

  /// Returns the bounds as a slice.
  #[inline]
  pub fn bounds_slice(&self) -> &[TypePath<Ident, Type>]
  where
    Container: AsRef<[TypePath<Ident, Type>]>,
  {
    self.bounds().as_ref()
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
    Container: ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>> + 'a,
    PathSegmentsContainer: ChumskyContainer<Ident> + 'a,
    TypeContainer: ChumskyContainer<Type> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    Ampersand: Parseable<'a, I, T, Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
  {
    TypePath::parser_with(ident_parser.clone(), type_parser.clone())
      .then_ignore(Colon::parser())
      .then(
        TypePath::parser_with(ident_parser, type_parser)
          .separated_by(Ampersand::parser())
          .allow_leading()
          .at_least(1)
          .collect(),
      )
      .map_with(|(bounded_type, bounds), exa| Self::new(exa.span(), bounded_type, bounds))
  }
}

impl<'a, Ident, Type, PathSegmentsContainer, TypeContainer, Container, I, T, Error>
  Parseable<'a, I, T, Error>
  for WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, Container>
where
  Container: ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  PathSegmentsContainer: ChumskyContainer<Ident>,
  TypeContainer: ChumskyContainer<Type>,
  TypePath<Ident, Type>: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error> + 'a,
  Ampersand: Parseable<'a, I, T, Error> + 'a,
  PathSeparator: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
  Ident: Parseable<'a, I, T, Error>,
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

/// A where clause, which contains a list of where predicates.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<
  Ident,
  Type,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  TypePathsContainer = Vec<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  Container = Vec<WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer>>,
> {
  span: Span,
  predicates: Container,
  _m: PhantomData<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container> AsSpan<Span>
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoSpan<Span>
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoComponents
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.predicates)
  }
}

impl<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
{
  /// Creates a new `WhereClause` with the given predicates.
  #[inline]
  const fn new(span: Span, predicates: Container) -> Self {
    Self {
      span,
      predicates,
      _m: PhantomData,
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
  pub fn predicates_slice(
    &self,
  ) -> &[WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>]
  where
    Container: AsRef<
      [WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>],
    >,
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
    Container: ChumskyContainer<
        WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
      > + 'a,
    TypePathsContainer:
      ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>> + 'a,
    PathSegmentsContainer: ChumskyContainer<Ident> + 'a,
    TypeContainer: ChumskyContainer<Type> + 'a,
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
          .at_least(1)
          .collect(),
      )
      .map_with(|predicates, exa| Self::new(exa.span(), predicates))
  }
}

impl<
  'a,
  Ident,
  Type,
  PathSegmentsContainer,
  TypeContainer,
  TypePathsContainer,
  Container,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
where
  Container: ChumskyContainer<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
  TypePathsContainer: ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  PathSegmentsContainer: ChumskyContainer<Ident>,
  TypeContainer: ChumskyContainer<Type>,
  WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer>: Parseable<'a, I, T, Error>,
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

/// A constrained `Target`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constrained<
  Ident,
  Type,
  Target,
  PathSegmentsContainer = Vec<Ident>,
  TypeContainer = Vec<Type>,
  TypePathsContainer = Vec<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  Container = Vec<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
> {
  span: Span,
  where_clause: Option<
    WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
  >,
  target: Target,
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  AsSpan<Span>
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoSpan<Span>
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  IntoComponents
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  type Components = (
    Span,
    Option<
      WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
    >,
    Target,
  );

  fn into_components(self) -> Self::Components {
    (self.span, self.where_clause, self.target)
  }
}

impl<Ident, Type, Target, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>
  Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
{
  /// Creates a new `Constrained` with the given target and optional where clause.
  #[inline]
  const fn new(
    span: Span,
    where_clause: Option<
      WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
    >,
    target: Target,
  ) -> Self {
    Self {
      span,
      where_clause,
      target,
    }
  }

  /// Returns the span of the constrained type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional where clause.
  #[inline]
  pub const fn where_clause(
    &self,
  ) -> Option<
    &WhereClause<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer, Container>,
  > {
    self.where_clause.as_ref()
  }

  /// Returns the target.
  #[inline]
  pub const fn target(&self) -> &Target {
    &self.target
  }

  /// Creates a parser for the constrained type.
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, IP, TP, P>(
    ident_parser: IP,
    type_parser: TP,
    target_parser: P,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    IP: Parser<'a, I, Ident, E> + Clone + 'a,
    TP: Parser<'a, I, Type, E> + Clone + 'a,
    P: Parser<'a, I, Target, E> + Clone + 'a,
    Ident: 'a,
    Type: 'a,
    Container: ChumskyContainer<
        WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
      > + 'a,
    TypePathsContainer:
      ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>> + 'a,
    PathSegmentsContainer: ChumskyContainer<Ident> + 'a,
    TypeContainer: ChumskyContainer<Type> + 'a,
    Colon: Parseable<'a, I, T, Error> + 'a,
    Ampersand: Parseable<'a, I, T, Error> + 'a,
    PathSeparator: Parseable<'a, I, T, Error> + 'a,
    LAngle: Parseable<'a, I, T, Error> + 'a,
    RAngle: Parseable<'a, I, T, Error> + 'a,
    Where: Parseable<'a, I, T, Error> + 'a,
  {
    WhereClause::parser_with(ident_parser, type_parser)
      .or_not()
      .then(target_parser)
      .map_with(|(where_clause, target), exa| Self::new(exa.span(), where_clause, target))
  }
}

impl<
  'a,
  Ident,
  Type,
  Target,
  PathSegmentsContainer,
  TypeContainer,
  TypePathsContainer,
  Container,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for Constrained<
    Ident,
    Type,
    Target,
    PathSegmentsContainer,
    TypeContainer,
    TypePathsContainer,
    Container,
  >
where
  Ident: Parseable<'a, I, T, Error>,
  Type: Parseable<'a, I, T, Error>,
  Target: Parseable<'a, I, T, Error>,
  PathSegmentsContainer: ChumskyContainer<Ident>,
  TypeContainer: ChumskyContainer<Type>,
  TypePathsContainer: ChumskyContainer<TypePath<Ident, Type, PathSegmentsContainer, TypeContainer>>,
  Container: ChumskyContainer<
    WherePredicate<Ident, Type, PathSegmentsContainer, TypeContainer, TypePathsContainer>,
  >,
  WherePredicate<Ident, Type>: Parseable<'a, I, T, Error>,
  Where: Parseable<'a, I, T, Error>,
  Colon: Parseable<'a, I, T, Error> + 'a,
  Ampersand: Parseable<'a, I, T, Error> + 'a,
  PathSeparator: Parseable<'a, I, T, Error> + 'a,
  LAngle: Parseable<'a, I, T, Error> + 'a,
  RAngle: Parseable<'a, I, T, Error> + 'a,
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
    Self::parser_with(Ident::parser(), Type::parser(), Target::parser())
  }
}
