//! GraphQLx import definitions for modular schema composition.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/import.rs`): the import specifiers, list, clause, and definition,
//! aliased onto the [`smear_scaffold`](crate::scaffold) import vocabulary. The
//! `import` system lets a schema be split across files and composed together.

use smear_scaffold::ast::{self as scaffold, Path};

use crate::{ident::Ident, value::InlineStringValue};

/// A named import specifier, optionally with an alias.
///
/// Represents importing a specific type by name, with the ability to rename it
/// using the `as` keyword.
///
/// ## Grammar
///
/// ```text
/// NamedSpecifier : Name (as Path)?
/// ```
pub type NamedSpecifier<S> = scaffold::NamedSpecifier<Ident<S>, Path<Ident<S>>>;

/// A wildcard import specifier, optionally with an alias.
///
/// Represents importing all exported types from a module using `*`.
///
/// ## Grammar
///
/// ```text
/// WildcardSpecifier : * (as Path)?
/// ```
pub type WildcardSpecifier<S> = scaffold::WildcardSpecifier<Ident<S>>;

/// An import member, which can be either a named or wildcard specifier.
///
/// ## Grammar
///
/// ```text
/// ImportMember :
///   - NamedSpecifier
///   - WildcardSpecifier
/// ```
pub type ImportMember<S> = scaffold::ImportMember<Ident<S>>;

/// An import list containing multiple import members enclosed in braces.
///
/// ## Grammar
///
/// ```text
/// ImportList : { ImportMember+ }
/// ```
pub type ImportList<S> = scaffold::ImportList<Ident<S>>;

/// An import clause specifying what to import — either a list of specific imports
/// or a wildcard import.
pub type ImportClause<S> = scaffold::ImportClause<Ident<S>>;

/// A complete GraphQLx import definition — the import clause and the file path.
///
/// ## Grammar
///
/// ```text
/// ImportDefinition : import ImportClause from StringValue
///
/// ImportClause :
///   - ImportList
///   - WildcardSpecifier
/// ```
pub type ImportDefinition<S> = scaffold::ImportDefinition<Ident<S>, InlineStringValue<S>>;
