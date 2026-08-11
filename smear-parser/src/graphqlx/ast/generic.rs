//! GraphQLx bindings for the shared generic-definition carriers.

use tokora::SimpleSpan;

use super::{DefaultVec, Name, Path, Type};

/// A declared GraphQLx generic type parameter with an optional default type.
pub type DefinitionTypeParam<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::generic::DefinitionTypeParam<Name<S, Span>, Ty, Span>;

/// An angle-delimited list of declared GraphQLx generic type parameters.
pub type DefinitionTypeGenerics<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<DefinitionTypeParam<S, Span, Ty>>,
> = crate::generic::DefinitionTypeGenerics<DefinitionTypeParam<S, Span, Ty>, Span, Container>;

/// A GraphQLx generic argument in a type-system extension.
pub type ExtensionTypeParam<S, Span = SimpleSpan> =
  crate::generic::ExtensionTypeParam<Name<S, Span>, Span>;

/// An angle-delimited list of GraphQLx generic arguments on a type-system extension.
pub type ExtensionTypeGenerics<
  S,
  Span = SimpleSpan,
  Container = DefaultVec<ExtensionTypeParam<S, Span>>,
> = crate::generic::ExtensionTypeGenerics<ExtensionTypeParam<S, Span>, Span, Container>;

/// An angle-delimited list of GraphQLx generic names in executable syntax.
pub type ExecutableDefinitionTypeGenerics<
  S,
  Span = SimpleSpan,
  Container = DefaultVec<Name<S, Span>>,
> = crate::generic::ExecutableDefinitionTypeGenerics<Name<S, Span>, Span, Container>;

/// A GraphQLx definition name with optional declared generic parameters.
pub type DefinitionName<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<DefinitionTypeParam<S, Span, Ty>>,
> = crate::generic::DefinitionName<
  Name<S, Span>,
  DefinitionTypeGenerics<S, Span, Ty, Container>,
  Span,
>;

/// A GraphQLx extension path with optional generic arguments.
pub type ExtensionName<
  S,
  Span = SimpleSpan,
  PathContainer = DefaultVec<Name<S, Span>>,
  Container = DefaultVec<ExtensionTypeParam<S, Span>>,
> = crate::generic::ExtensionName<
  Path<S, Span, PathContainer>,
  ExtensionTypeGenerics<S, Span, Container>,
  Span,
>;

/// A GraphQLx executable definition name with optional generic names.
pub type ExecutableDefinitionName<S, Span = SimpleSpan, Container = DefaultVec<Name<S, Span>>> =
  crate::generic::ExecutableDefinitionName<
    Name<S, Span>,
    ExecutableDefinitionTypeGenerics<S, Span, Container>,
    Span,
  >;

/// The leading generic declaration and name of a GraphQLx executable definition.
pub type ExecutableDefinitionHeader<ImplementationGenerics, Name, Span = SimpleSpan> =
  crate::generic::ExecutableDefinitionHeader<ImplementationGenerics, Name, Span>;

/// A namespaced GraphQLx path with optional recursive generic type arguments.
pub type TypePath<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = DefaultVec<Ty>,
> = crate::generic::TypePath<
  Path<S, Span, PathContainer>,
  crate::ty::TypeGenerics<Ty, Span, TypeContainer>,
  Span,
>;

/// A GraphQLx fragment-reference path with optional type arguments.
pub type FragmentTypePath<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = DefaultVec<Ty>,
> = TypePath<S, Span, Ty, PathContainer, TypeContainer>;

/// A GraphQLx `where` predicate over a bounded type path.
pub type WherePredicate<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = DefaultVec<Ty>,
  BoundContainer = DefaultVec<TypePath<S, Span, Ty, PathContainer, TypeContainer>>,
> = crate::generic::WherePredicate<
  TypePath<S, Span, Ty, PathContainer, TypeContainer>,
  Span,
  BoundContainer,
>;

/// A GraphQLx `where` clause containing one or more predicates.
pub type WhereClause<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = DefaultVec<Ty>,
  BoundContainer = DefaultVec<TypePath<S, Span, Ty, PathContainer, TypeContainer>>,
  PredicateContainer = DefaultVec<
    WherePredicate<S, Span, Ty, PathContainer, TypeContainer, BoundContainer>,
  >,
> = crate::generic::WhereClause<
  WherePredicate<S, Span, Ty, PathContainer, TypeContainer, BoundContainer>,
  Span,
  PredicateContainer,
>;

/// A GraphQLx target carrying an optional `where` clause.
pub type Constrained<Target, Clause, Span = SimpleSpan> =
  crate::generic::Constrained<Target, Clause, Span>;
