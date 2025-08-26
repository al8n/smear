use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored,
    punct::{Colon, LBrace, RBrace},
  },
  source::{Char, Slice, Source},
};

use core::marker::PhantomData;
use std::vec::Vec;

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
pub struct RootOperationTypeDefinition<Name, OperationType, Span> {
  span: Span,
  operation_type: OperationType,
  colon: Colon<Span>,
  name: Name,
}

impl<Name, OperationType, Span> AsRef<Span>
  for RootOperationTypeDefinition<Name, OperationType, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, OperationType, Span> IntoSpan<Span>
  for RootOperationTypeDefinition<Name, OperationType, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, OperationType, Span> IntoComponents
  for RootOperationTypeDefinition<Name, OperationType, Span>
{
  type Components = (Span, OperationType, Colon<Span>, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.operation_type, self.colon, self.name)
  }
}

impl<Name, OperationType, Span> RootOperationTypeDefinition<Name, OperationType, Span> {
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

  /// Returns a reference to the colon separator token.
  ///
  /// The colon separates the operation type from the target Object type name
  /// in the syntax `operationType : TypeName`.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
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
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the root operation type.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP, P>(
    name_parser: NP,
    operation_type_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    NP: Parser<'src, I, Name, E> + Clone,
    P: Parser<'src, I, OperationType, E> + Clone,
  {
    operation_type_parser
      .then(Colon::parser().padded_by(ignored()))
      .then(name_parser)
      .map_with(|((operation_type, colon), name), sp| Self {
        span: Span::from_map_extra(sp),
        operation_type,
        colon,
        name,
      })
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
  Span,
  Container = Vec<RootOperationTypeDefinition>,
> {
  span: Span,
  l_brace: LBrace<Span>,
  root_operation_types: Container,
  r_brace: RBrace<Span>,
  _m: PhantomData<RootOperationTypeDefinition>,
}

impl<RootOperationTypeDefinition, Span, Container> AsRef<Span>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Span, Container>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<RootOperationTypeDefinition, Span, Container> IntoSpan<Span>
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<RootOperationTypeDefinition, Span, Container> IntoComponents
  for RootOperationTypesDefinition<RootOperationTypeDefinition, Span, Container>
{
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.l_brace,
      self.root_operation_types,
      self.r_brace,
    )
  }
}

impl<RootOperationTypeDefinition, Span, Container>
  RootOperationTypesDefinition<RootOperationTypeDefinition, Span, Container>
{
  /// Returns a reference to the span covering the entire root operation types definition.
  ///
  /// The span includes the opening brace, all operation type definitions, and the closing brace.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the opening left brace token.
  ///
  /// This marks the beginning of the root operation types definition block.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
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

  /// Returns a reference to the closing right brace token.
  ///
  /// This marks the end of the root operation types definition block.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Creates a parser for root operation types definitions.
  ///
  /// This parser handles the braced block syntax containing one or more root operation
  /// type definitions. The parser ensures at least one definition is present and
  /// properly handles whitespace and comments within the block.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the root operation types definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(
    operation_type_parser: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, RootOperationTypeDefinition, E> + Clone,
    Container: chumsky::container::Container<RootOperationTypeDefinition>,
  {
    LBrace::parser()
      .then(
        operation_type_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then(RBrace::parser())
      .map_with(|((l_brace, root_operation_types), r_brace), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        root_operation_types,
        r_brace,
        _m: PhantomData,
      })
  }
}
