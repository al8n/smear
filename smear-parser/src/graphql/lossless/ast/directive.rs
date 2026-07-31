//! Typed wrappers over the four argument and directive node kinds.
//!
//! # The wrapper levels are kept, not skipped
//!
//! `Directives` wraps the `Directive`s and `Arguments` wraps the `Argument`s, and Task 10's
//! ruling — recorded here because this is where it first lands — is that a getter **must not
//! reach through** such a node. A [`Field`](super::selection::Field) answers
//! `Option<Directives>`, and the walk to one [`Directive`] goes through
//! [`Directives::directives`].
//!
//! Two reasons, and the second is the one that decides it:
//!
//! - `cast::children` looks at direct children only, so a `directives: many Directive` getter on
//!   a field would compile, navigate, and yield nothing forever — the silent failure mode a
//!   wrapper level exists to prevent.
//! - The node's *absence* is information. `directives` opens no node at all when no `@` was
//!   written, so `Option<Directives>` distinguishes "no directives" from "an empty list", which
//!   is exactly the distinction Task 6 arranged the node placement to preserve. A getter that
//!   reached through would flatten both to an empty iterator.
//!
//! The same ruling covers `Arguments`, `ArgumentsDefinition`, `FieldsDefinition`,
//! `InputFieldsDefinition`, `EnumValuesDefinition`, `VariablesDefinition`,
//! `RootOperationTypeDefinitions`, `ImplementsInterfaces` and `UnionMemberTypes`.

use crate::{
  ast_node,
  graphql::{
    kinds::SyntaxKind as K,
    lossless::ast::value::{
      BooleanValue, EnumValue, FloatValue, IntValue, ListValue, NullValue, ObjectValue,
      StringValue, Variable,
    },
  },
};

ast_node!(
  /// One argument, `name : value`.
  ///
  /// One node kind for both the executable and the constant spelling: the spec's `Argument[Const]`
  /// forbids a variable in a const position, and this suite leaves that to validation so the
  /// offending node is still there to point at.
  Argument => K::Argument {
    /// The argument's name.
    name: tok K::Name,
    /// The argument's value, when it is a variable.
    variable: opt Variable,
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
    /// The argument's value, when it is an object.
    object_value: opt ObjectValue,
  }
);

ast_node!(
  /// An argument list, `( … )`.
  ///
  /// Delimited, so an empty `()` is a real, written-down list and gets its node.
  Arguments => K::Arguments {
    /// Every argument in the list, in order.
    arguments: many Argument,
  }
);

ast_node!(
  /// One directive, `@name` with an optional argument list.
  Directive => K::Directive {
    /// The directive's name, which the `@` precedes.
    name: tok K::Name,
    /// The directive's arguments, if it was given any.
    arguments: opt Arguments,
  }
);

ast_node!(
  /// A run of directives.
  ///
  /// Undelimited, so this node exists only where at least one directive was written — see this
  /// module's docs.
  Directives => K::Directives {
    /// Every directive in the run, in order.
    directives: many Directive,
  }
);
