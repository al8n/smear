//! GraphQL executable-definition AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate (`graphql/ast/default.rs`):
//! variable definitions, operation definitions (named + shorthand), fragment
//! definitions, and the executable document, keyed by the source slice `S`. Variable
//! definitions carry an optional [`Described`] wrapper exactly as the frozen crate
//! does, so `VariablesDefinition` threads [`DescribedVariableDefinition`].

use smear_scaffold::ast as scaffold;

use super::{
  DefaultInputValue, Directives, FragmentName, Name, SelectionSet, StringValue, Type,
  TypeCondition, VariableValue,
};

pub use scaffold::OperationType;

/// A node paired with an optional description (a leading string literal).
pub type Described<T, S> = scaffold::Described<T, StringValue<S>>;

/// Variable definition in an operation
/// (`Description? Variable ':' Type DefaultValue? Directives?`).
pub type VariableDefinition<S, Ty = Type<Name<S>>> =
  scaffold::VariableDefinition<VariableValue<S>, Ty, DefaultInputValue<S>, Directives<S>>;

/// Variable definition with an optional leading description.
pub type DescribedVariableDefinition<S, Ty = Type<Name<S>>> =
  Described<VariableDefinition<S, Ty>, S>;

/// List of variable definitions for an operation (`( VariableDefinition+ )`).
pub type VariablesDefinition<S, Ty = Type<Name<S>>> =
  scaffold::VariablesDefinition<DescribedVariableDefinition<S, Ty>>;

/// Fragment definition in an executable document
/// (`fragment FragmentName TypeCondition Directives? SelectionSet`).
pub type FragmentDefinition<S> =
  scaffold::FragmentDefinition<FragmentName<S>, TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// Named operation definition (query, mutation, or subscription with metadata).
pub type NamedOperationDefinition<S, Ty = Type<Name<S>>> = scaffold::NamedOperationDefinition<
  Name<S>,
  OperationType,
  VariablesDefinition<S, Ty>,
  Directives<S>,
  SelectionSet<S>,
>;

/// Operation definition (named, or the query-shorthand selection set).
pub type OperationDefinition<S, Ty = Type<Name<S>>> =
  scaffold::OperationDefinition<NamedOperationDefinition<S, Ty>, SelectionSet<S>>;

/// Executable definition (operation or fragment).
pub type ExecutableDefinition<S, Ty = Type<Name<S>>> =
  scaffold::ExecutableDefinition<OperationDefinition<S, Ty>, FragmentDefinition<S>>;

/// Executable document (operations and fragments).
pub type ExecutableDocument<S, Ty = Type<Name<S>>> =
  scaffold::Document<ExecutableDefinition<S, Ty>>;
