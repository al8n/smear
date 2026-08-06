//! Typed wrappers over the three type-reference node kinds.
//!
//! A type reference is a three-way union — `NamedType | ListType | NonNullType` — and this layer
//! has no union type, so every position that holds one carries three `opt` getters of which
//! exactly one answers. See `ast/value.rs`'s module docs for the same ruling at greater width.
//!
//! The nesting is genuine rather than notional: `[Int!]!` is a [`NonNullType`] over a
//! [`ListType`] over a [`NonNullType`] over a [`NamedType`], and each level is reached by asking
//! for the kind rather than for "the child".

use crate::{ast_node, graphql::kinds::SyntaxKind as K};

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A named type reference.
  NamedType => K::NamedType {
    /// The type's name.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A list type reference, `[T]`.
  ListType => K::ListType {
    /// The element type, when it is a named type.
    named_type: opt NamedType,
    /// The element type, when it is itself a list.
    list_type: opt ListType,
    /// The element type, when it is non-null.
    non_null_type: opt NonNullType,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A non-null type reference, `T!`.
  ///
  /// The `!` is a suffix, so this node is a retro-wrap around a type that was already committed;
  /// its inner type is therefore never another [`NonNullType`] — `T!!` has no production.
  NonNullType => K::NonNullType {
    /// The wrapped type, when it is a named type.
    named_type: opt NamedType,
    /// The wrapped type, when it is a list.
    list_type: opt ListType,
  }
);
