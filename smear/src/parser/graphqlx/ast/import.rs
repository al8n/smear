//! GraphQLx import definitions for modular schema composition.
//!
//! This module provides types for GraphQLx's import system, which allows
//! schemas to be split across multiple files and composed together.

use crate::{
  parser::{ident::Ident, value::InlineStringValue},
  scaffold::{self, Path},
};

/// A named import specifier, optionally with an alias.
///
/// Represents importing a specific type by name, with the ability to
/// rename it using the `as` keyword.
///
/// ## Grammar
///
/// ```text
/// NamedSpecifier : Name (as Path)?
/// ```
pub type NamedSpecifier<S, Container = Vec<Ident<S>>> =
  scaffold::NamedSpecifier<Ident<S>, Path<Ident<S>, Container>>;

/// A wildcard import specifier, optionally with an alias.
///
/// Represents importing all exported types from a module using `*`.
///
/// ## Grammar
///
/// ```text
/// WildcardSpecifier : * (as Path)?
/// ```
pub type WildcardSpecifier<S, Container = Vec<Ident<S>>> =
  scaffold::WildcardSpecifier<Ident<S>, Container>;

/// An import member, which can be either a named or wildcard specifier.
///
/// ## Grammar
///
/// ```text
/// ImportMember :
///   - NamedSpecifier
///   - WildcardSpecifier
/// ```
pub type ImportMember<S, Container = Vec<Ident<S>>> =
  scaffold::ImportMember<Ident<S>, Path<Ident<S>, Container>>;

/// An import list containing multiple import members enclosed in braces.
///
/// ## Grammar
///
/// ```text
/// ImportList : { ImportMember+ }
/// ```
pub type ImportList<
  S,
  PathContainer = Vec<Ident<S>>,
  MemberContainer = Vec<ImportMember<Ident<S>, Vec<Ident<S>>>>,
> = scaffold::ImportList<
  Ident<S>,
  Path<Ident<S>, PathContainer>,
  ImportMember<Ident<S>, MemberContainer>,
>;

/// A complete GraphQLx import definition.
///
/// Represents the full import statement including the import clause
/// and the file path.
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
pub type ImportDefinition<
  S,
  PathContainer = Vec<Ident<S>>,
  MemberContainer = Vec<ImportMember<Ident<S>, Vec<Ident<S>>>>,
> = scaffold::ImportDefinition<
  Ident<S>,
  InlineStringValue<S>,
  PathContainer,
  ImportMember<Ident<S>, MemberContainer>,
>;
