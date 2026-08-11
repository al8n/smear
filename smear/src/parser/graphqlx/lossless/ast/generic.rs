//! Typed wrappers over the nine generic-parameter, definition-name and `where`-clause node kinds.
//!
//! # Nothing here has a GraphQL counterpart
//!
//! GraphQL has no generic parameters, no `where` clause and no definition-name node, so there is
//! no wrapper to port and nothing in `graphql/lossless/ast/` to compare a getter against.
//!
//! # The three generic lists are three node kinds, and the difference is their members
//!
//! | wrapper | member | reached from |
//! |---|---|---|
//! | [`DefinitionTypeGenerics`] | [`DefinitionTypeParam`] — a node | a definition's name |
//! | [`ExtensionTypeGenerics`] | a bare `Name` token | an extension's target |
//! | [`ExecutableDefinitionTypeGenerics`] | a bare `Name` token | a fragment's header and its name |
//!
//! Only the first has a member node, because only its member can carry something: a definition
//! *declares* parameters and may default them, an extension *applies* arguments and may not.
//! `<T>` is the same bytes in all three positions, so the node kind is the only thing that tells a
//! consumer which side of that it is looking at — which is why the lower two are separate wrappers
//! over one production rather than one wrapper reached twice.
//!
//! # A definition's name is a node, and that is what removes every positional getter
//!
//! [`DefinitionName`], [`ExtensionName`] and [`ExecutableDefinitionName`] each wrap the name and
//! whatever generics follow it. GraphQL's twin has no such node, so `scalar S` puts the keyword
//! and the name under one parent as two `Name` tokens and its wrapper needs `tok_nth 1` to reach
//! the second. **Here the keyword is the definition's only direct `Name` token and the name is a
//! child node**, so no wrapper in this dialect uses `tok_nth` at all — see
//! [`definition`](super::definition)'s module docs for the full statement.

use crate::{
  ast_node,
  parser::graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::ty::{DefinitionTypePath, ListType, MapType, Path, SetType, TypePath},
  },
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One declared generic parameter, `Name (= Type)?`.
  ///
  /// **The default is a type reference, not a
  /// [`DefaultValue`](super::value::DefaultValue).** The `=` here introduces a type, so the four
  /// getters below are the type kinds; reusing the value node would put a value where a type
  /// belongs, and the two spell the same token.
  DefinitionTypeParam => K::DefinitionTypeParam {
    /// The parameter's name.
    name: tok K::Name,
    /// The parameter's default, when it is a path type.
    definition_type_path: opt DefinitionTypePath,
    /// The parameter's default, when it is a list type.
    list_type: opt ListType,
    /// The parameter's default, when it is a set type.
    set_type: opt SetType,
    /// The parameter's default, when it is a map type.
    map_type: opt MapType,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// The generic parameters a definition declares, `< DefinitionTypeParam+ >`.
  ///
  /// The one list of the three whose members are nodes — see this module's docs.
  DefinitionTypeGenerics => K::DefinitionTypeGenerics {
    /// Every parameter declared, in order.
    definition_type_params: many DefinitionTypeParam,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// The generic arguments a type-system extension applies, `< Name+ >`.
  ///
  /// Bare name tokens: an extension applies arguments and may not default them, so its members
  /// carry nothing a node could hold.
  ExtensionTypeGenerics => K::ExtensionTypeGenerics {
    /// Every argument applied, in order.
    names: toks K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// The generic parameters an executable definition declares, `< Name+ >`.
  ///
  /// [`ExtensionTypeGenerics`]' shape under the other kind. A fragment definition can hold **two**
  /// of these — see [`FragmentDefinition`](super::executable::FragmentDefinition).
  ExecutableDefinitionTypeGenerics => K::ExecutableDefinitionTypeGenerics {
    /// Every parameter declared, in order.
    names: toks K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A definition's name, `Name DefinitionTypeGenerics?`.
  ///
  /// Every SDL definition's name and every operation's, so `query Q<T = Int>` and
  /// `type T<A = Int>` reach the same node.
  DefinitionName => K::DefinitionName {
    /// The name itself.
    name: tok K::Name,
    /// The generic parameters it declares, if any were written.
    definition_type_generics: opt DefinitionTypeGenerics,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A type-system extension's target, `Path ExtensionTypeGenerics?`.
  ///
  /// **A path, where a [`DefinitionName`] takes a bare name.** `extend type ns::T` names a
  /// qualified target and `type ns::T` does not; the two spell one identifier for the simple case,
  /// so the nesting is the only witness.
  ExtensionName => K::ExtensionName {
    /// The path naming the extended type.
    path: opt Path,
    /// The generic arguments it applies, if any were written.
    extension_type_generics: opt ExtensionTypeGenerics,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An executable definition's name, `Name ExecutableDefinitionTypeGenerics?`.
  ///
  /// A fragment's name. Its list is the name's **own**, not the implementation list the definition
  /// declares before it.
  ExecutableDefinitionName => K::ExecutableDefinitionName {
    /// The name itself.
    name: tok K::Name,
    /// The generic parameters the name declares, if any were written.
    executable_definition_type_generics: opt ExecutableDefinitionTypeGenerics,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One `where` predicate, `TypePath : TypePath (& TypePath)*`.
  ///
  /// **The getter is plural, and that is forced.** The constrained type and its bounds are all
  /// [`TypePath`]s under one node, so an `opt` getter would answer the constrained type and every
  /// bound would be unreachable. The first is the constrained type and the rest are its bounds;
  /// the `:` and the `&`s are bare tokens between them.
  WherePredicate => K::WherePredicate {
    /// The constrained type followed by its bounds, in order.
    type_paths: many TypePath,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A `where` clause, `where WherePredicate (, WherePredicate)*`.
  ///
  /// Undelimited, so this node exists only where a `where` was written — the same ruling
  /// [`Directives`](super::directive::Directives) records, applied to the other undelimited
  /// optional shape.
  WhereClause => K::WhereClause {
    /// Every predicate in the clause, in order.
    where_predicates: many WherePredicate,
  }
);
