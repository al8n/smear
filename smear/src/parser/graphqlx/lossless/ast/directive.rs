//! Typed wrappers over the four argument and directive node kinds.
//!
//! # The wrapper levels are kept, not skipped
//!
//! [`Directives`] wraps the [`Directive`]s and [`Arguments`] wraps the [`Argument`]s, and the
//! ruling — recorded here because this is where it first lands — is that a getter **must not reach
//! through** such a node. A [`Field`](super::selection::Field) answers `Option<Directives>`, and
//! the walk to one [`Directive`] goes through [`Directives::directives`].
//!
//! Two reasons, and the second is the one that decides it:
//!
//! - `cast::children` looks at direct children only, so a `directives: many Directive` getter on a
//!   field would compile, navigate, and yield nothing forever — the silent failure mode a wrapper
//!   level exists to prevent.
//! - The node's *absence* is information. The production opens no node at all when no `@` was
//!   written, so `Option<Directives>` distinguishes "no directives" from "an empty list". A getter
//!   that reached through would flatten both to an empty iterator.
//!
//! The same ruling covers [`Arguments`], `ArgumentsDefinition`, `FieldsDefinition`,
//! `InputFieldsDefinition`, `EnumValuesDefinition`, `VariablesDefinition`,
//! `RootOperationTypesDefinition`, `ImplementInterfaces`, `UnionMemberTypes`, `ImportList`,
//! `WhereClause` and all three generic lists.
//!
//! # The one divergence, and it is in every tree
//!
//! **A directive's name is a [`TypePath`], not a `Name` token.** `@ns::cache<Int>` parses,
//! `@::ns::d` parses, and even `@deprecated` nests `Directive > TypePath > Path`. A port of
//! GraphQL's `name: tok K::Name` would compile here and answer `None` forever, the `@`'s name
//! never being a direct token child of the directive at all.
//!
//! What did **not** widen: an [`Argument`]'s key is still a plain `Name`, exactly as an
//! [`ObjectField`](super::value::ObjectField)'s is.

use crate::{
  ast_node,
  parser::graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      ty::TypePath,
      value::{
        BooleanValue, EnumValue, FloatValue, IntValue, ListValue, MapValue, NullValue, ObjectValue,
        SetValue, StringValue, VariableValue,
      },
    },
  },
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One argument, `name : value`.
  ///
  /// One node kind for both the executable and the constant spelling: the grammar's
  /// `Argument[Const]` forbids a variable in a const position, and this suite leaves that to
  /// validation so the offending node is still there to point at.
  Argument => K::Argument {
    /// The argument's name.
    name: tok K::Name,
    /// The argument's value, when it is a variable.
    variable_value: opt VariableValue,
    /// The argument's value, when it is an integer literal.
    int_value: opt IntValue,
    /// The argument's value, when it is a float literal.
    float_value: opt FloatValue,
    /// The argument's value, when it is a string literal.
    string_value: opt StringValue,
    /// The argument's value, when it is `true` or `false`.
    boolean_value: opt BooleanValue,
    /// The argument's value, when it is `null`.
    null_value: opt NullValue,
    /// The argument's value, when it is an enum value.
    enum_value: opt EnumValue,
    /// The argument's value, when it is a list.
    list_value: opt ListValue,
    /// The argument's value, when it is a set.
    set_value: opt SetValue,
    /// The argument's value, when it is a map.
    map_value: opt MapValue,
    /// The argument's value, when it is an object.
    object_value: opt ObjectValue,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An argument list, `( … )`.
  ///
  /// Delimited, so an empty `()` is a real, written-down list and gets its node — unlike
  /// [`Directives`], and unlike a `VariablesDefinition`, whose emptiness the grammar rejects.
  Arguments => K::Arguments {
    /// Every argument in the list, in order.
    arguments: many Argument,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One directive, `@ TypePath Arguments?`.
  Directive => K::Directive {
    /// The directive's name, which the `@` precedes — a whole path with optional generics, for the
    /// reason this module's docs record.
    type_path: opt TypePath,
    /// The directive's arguments, if it was given any.
    arguments: opt Arguments,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A run of directives.
  ///
  /// Undelimited, so this node exists only where at least one directive was written — see this
  /// module's docs.
  Directives => K::Directives {
    /// Every directive in the run, in order.
    directives: many Directive,
  }
);
