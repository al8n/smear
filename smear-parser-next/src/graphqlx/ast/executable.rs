//! GraphQLx executable-definition AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/default.rs`, `default/operation_definition.rs`, and
//! `default/fragment_definition.rs`): variable definitions, operation definitions
//! (named + shorthand), fragment definitions, and the executable document, keyed by
//! the source slice `S`. Three GraphQLx twists over the GraphQL shapes:
//!
//! - an operation's name slot is a [`DefinitionName`] (`GetData<T, U = String>` —
//!   name plus optional generic *parameters*), and a fragment carries generics
//!   twice: impl generics on the keyword (`fragment<T>`) [`And`] an
//!   [`ExecutableDefinitionName`] (`ItemFragment<T>`);
//! - both definitions may constrain their selection set with a where clause, so
//!   the selection-set slot is a [`ConstrainedSelectionSet`];
//! - executable definitions may carry a leading description, and an executable
//!   document interleaves [`ImportDefinition`]s with the described definitions
//!   ([`ImportOrExecutableDefinition`]).

use smear_scaffold::ast as scaffold;

use super::{
  DefaultInputValue, DefinitionName, Directives, ExecutableDefinitionName,
  ExecutableDefinitionTypeGenerics, ImportDefinition, Name, SelectionSet, StringValue, Type,
  TypeCondition, VariableValue,
};

pub use scaffold::{And, OperationType};

/// A node paired with an optional description (a leading string literal).
pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

/// Variable definition in an operation
/// (`Description? Variable ':' Type DefaultValue? Directives?`).
pub type VariableDefinition<S> =
  scaffold::VariableDefinition<VariableValue<S>, Type<S>, DefaultInputValue<S>, Directives<S>>;

/// Variable definition with an optional leading description.
pub type DescribedVariableDefinition<S> = Described<VariableDefinition<S>, S>;

/// List of variable definitions for an operation (`( VariableDefinition+ )`).
pub type VariablesDefinition<S> = scaffold::VariablesDefinition<DescribedVariableDefinition<S>>;

/// A selection set optionally constrained by a leading where clause
/// (`WhereClause? SelectionSet`).
pub type ConstrainedSelectionSet<S> =
  scaffold::generic::Constrained<Name<S>, Type<S>, SelectionSet<S>>;

/// Named operation definition (query, mutation, or subscription with metadata,
/// a generic-parameter-carrying name, and an optionally constrained selection set).
pub type NamedOperationDefinition<S> = scaffold::NamedOperationDefinition<
  DefinitionName<S>,
  OperationType,
  VariablesDefinition<S>,
  Directives<S>,
  ConstrainedSelectionSet<S>,
>;

/// Operation definition (named, or the query-shorthand selection set).
pub type OperationDefinition<S> =
  scaffold::OperationDefinition<NamedOperationDefinition<S>, SelectionSet<S>>;

/// The name slot of a fragment definition: the optional impl generics on the
/// `fragment` keyword paired with the fragment's own generic-carrying name
/// (`fragment<T> ItemFragment<T>`).
pub type FragmentDefinitionName<S> =
  And<Option<ExecutableDefinitionTypeGenerics<S>>, ExecutableDefinitionName<S>>;

/// Fragment definition in an executable document
/// (`fragment ImplGenerics? Name Generics? TypeCondition Directives? WhereClause?
/// SelectionSet`).
pub type FragmentDefinition<S> = scaffold::FragmentDefinition<
  FragmentDefinitionName<S>,
  TypeCondition<S>,
  Directives<S>,
  ConstrainedSelectionSet<S>,
>;

/// Executable definition (operation or fragment).
pub type ExecutableDefinition<S> =
  scaffold::ExecutableDefinition<OperationDefinition<S>, FragmentDefinition<S>>;

/// Executable definition with an optional leading description.
pub type DescribedExecutableDefinition<S> = Described<ExecutableDefinition<S>, S>;

/// An import definition or a described executable definition — the item an
/// executable document collects.
pub type ImportOrExecutableDefinition<S> =
  scaffold::ImportOrExecutableDefinition<ImportDefinition<S>, DescribedExecutableDefinition<S>>;

/// Executable document (imports, operations, and fragments).
pub type ExecutableDocument<S> = scaffold::Document<ImportOrExecutableDefinition<S>>;
