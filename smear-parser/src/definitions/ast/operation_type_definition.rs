use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use core::marker::PhantomData;
use std::vec::Vec;

use crate::lang::punctuator::{Colon, LBrace, RBrace};

/// Represents a single root operation type definition that maps an operation type to a GraphQL Object type.
///
/// Root operation type definitions specify which Object type serves as the entry point for each
/// kind of GraphQL operation. They form the foundation of GraphQL schema execution by defining
/// the root types that clients can access for queries, mutations, and subscriptions.
///
/// ## Examples
///
/// ```text
/// query: Query
/// mutation: Mutation
/// subscription: Subscription
///
/// # Custom root type names
/// query: QueryRoot
/// mutation: MutationRoot
/// subscription: RealtimeSubscription
/// ```
///
/// ## Grammar
/// ```text
/// RootOperationTypeDefinition : OperationType : NamedType
/// ```
///
/// Spec: [Root Operation Types Definition](https://spec.graphql.org/draft/#sec-Root-Operation-Types)
#[derive(Debug, Clone, Copy)]
pub struct RootOperationTypeDefinition<Name, OperationType> {
  span: Span,
  operation_type: OperationType,
  name: Name,
}

impl<Name, OperationType> AsSpan<Span> for RootOperationTypeDefinition<Name, OperationType> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, OperationType> IntoSpan<Span> for RootOperationTypeDefinition<Name, OperationType> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, OperationType> IntoComponents for RootOperationTypeDefinition<Name, OperationType> {
  type Components = (Span, OperationType, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.operation_type, self.name)
  }
}

impl<Name, OperationType> RootOperationTypeDefinition<Name, OperationType> {
  /// Returns a reference to the span covering the entire root operation type definition.
  ///
  /// The span includes the operation type keyword, colon separator, and the target type name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the operation type (query, mutation, or subscription).
  ///
  /// This specifies which kind of GraphQL operation this definition applies to.
  /// Each operation type can only be defined once per schema.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns a reference to the name of the Object type that serves as the root.
  ///
  /// This must be the name of an Object type defined elsewhere in the schema.
  /// The referenced type becomes the entry point for operations of this type.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for root operation type definitions.
  ///
  /// This parser handles the syntax `OperationType : Name` where the operation type
  /// is parsed by the provided parser and the name must be a valid GraphQL name.
  pub fn parser_with<'src, I, T, Error, E, NP, P>(
    name_parser: NP,
    operation_type_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Colon: Parseable<'src, I, T, Error>,
    NP: Parser<'src, I, Name, E> + Clone,
    P: Parser<'src, I, OperationType, E> + Clone,
  {
    operation_type_parser
      .then_ignore(Colon::parser())
      .then(name_parser)
      .map_with(|(operation_type, name), exa| Self {
        span: exa.span(),
        operation_type,
        name,
      })
  }
}

impl<'a, Name, OperationType, I, T, Error> Parseable<'a, I, T, Error>
  for RootOperationTypeDefinition<Name, OperationType>
where
  Colon: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  OperationType: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), OperationType::parser())
  }
}

/// Represents a collection of root operation type definitions enclosed in braces.
///
/// This structure defines the complete set of root operation types for a GraphQL schema,
/// specifying which Object types serve as entry points for different kinds of operations.
/// Every schema must have at least a query root type, while mutation and subscription
/// root types are optional.
///
/// ## Examples
///
/// ```text
/// # Minimal schema with only query
/// {
///   query: Query
/// }
///
/// # Complete schema with all operation types
/// {
///   query: Query
///   mutation: Mutation
///   subscription: Subscription
/// }
///
/// # Custom root type names
/// {
///   query: ApiQuery
///   mutation: ApiMutation
///   subscription: RealtimeEvents
/// }
/// ```
///
/// ## Type Parameters
/// - `OperationTypeDefinition`: The type of the individual root operation type definitions.
/// - `Span`: The type representing the span of the entire root operation types definition.
/// - `Container`: The type of the container holding the operation type definitions (default is `Vec<OperationTypeDefinition>`).
///
/// ## Grammar
/// ```text
/// RootOperationTypesDefinition : { RootOperationTypeDefinition+ }
/// ```
#[derive(Debug, Clone, Copy)]
pub struct RootOperationTypesDefinition<
  RootOperationTypeDefinition,
  Container = Vec<RootOperationTypeDefinition>,
> {
  span: Span,
  root_operation_types: Container,
  _m: PhantomData<RootOperationTypeDefinition>,
}

impl<RootOperationTypeDefinition, Container> AsSpan<Span>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<RootOperationTypeDefinition, Container> IntoSpan<Span>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<RootOperationTypeDefinition, Container> IntoComponents
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.root_operation_types)
  }
}

impl<RootOperationTypeDefinition, Container>
  RootOperationTypesDefinition<RootOperationTypeDefinition, Container>
{
  /// Returns a reference to the span covering the entire root operation types definition.
  ///
  /// The span includes the opening brace, all operation type definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all root operation types definition.
  ///
  /// This collection must contain at least one definition (the query root type) and
  /// may contain up to three definitions (query, mutation, subscription).
  #[inline]
  pub const fn root_operation_type_definitions(&self) -> &Container {
    &self.root_operation_types
  }

  /// Consumes and returns the root operation type definitions.
  #[inline]
  pub fn into_root_operation_type_definitions(self) -> Container {
    self.root_operation_types
  }

  /// Creates a parser for root operation types definitions.
  ///
  /// This parser handles the braced block syntax containing one or more root operation
  /// type definitions. The parser ensures at least one definition is present and
  /// properly handles whitespace and comments within the block.
  pub fn parser_with<'src, I, T, Error, E, P>(
    root_operation_type_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    LBrace: Parseable<'src, I, T, Error>,
    RBrace: Parseable<'src, I, T, Error>,
    P: Parser<'src, I, RootOperationTypeDefinition, E> + Clone,
    Container: chumsky::container::Container<RootOperationTypeDefinition>,
  {
    LBrace::parser()
      .ignore_then(root_operation_type_parser.repeated().at_least(1).collect())
      .then_ignore(RBrace::parser())
      .map_with(|root_operation_types, exa| Self {
        span: exa.span(),
        root_operation_types,
        _m: PhantomData,
      })
  }
}

impl<'a, RootOperationTypeDefinition, Container, I, T, Error> Parseable<'a, I, T, Error>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Container>
where
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  RootOperationTypeDefinition: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<RootOperationTypeDefinition>,
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
    Self::parser_with(RootOperationTypeDefinition::parser())
  }
}
