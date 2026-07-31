//! Typed wrappers over the five executable-definition node kinds.
//!
//! # A definition's name sits behind its keyword, and both are `Name` tokens
//!
//! This kind space has no per-keyword token kind and no `Name` node: `fragment`, `F` and `on` are
//! three [`Name`](K::Name) tokens under one [`FragmentDefinition`], so `cast::token`'s
//! first-match answer is the keyword. [`FragmentDefinition::name`] is therefore a `tok_nth`
//! getter — the second `Name`, which is the position the production's two consecutive `expect`s
//! force.
//!
//! [`OperationDefinition`] is the exception and needs no index: its keyword lives inside an
//! [`OperationType`] node, so the operation's own name is the only `Name` directly beneath it.

use crate::{
  ast_node,
  graphql::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      definition::{Description, OperationType},
      directive::Directives,
      selection::SelectionSet,
      ty::{ListType, NamedType, NonNullType},
      value::{DefaultValue, Variable},
    },
  },
};

ast_node!(
  /// One variable definition, `$v : T = default @directives`.
  VariableDefinition => K::VariableDefinition {
    /// The variable being defined.
    variable: opt Variable,
    /// The variable's type, when it is a named type.
    named_type: opt NamedType,
    /// The variable's type, when it is a list.
    list_type: opt ListType,
    /// The variable's type, when it is non-null.
    non_null_type: opt NonNullType,
    /// The variable's default, if it was given one.
    default_value: opt DefaultValue,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  /// An operation's variable definitions, `( … )`.
  VariablesDefinition => K::VariablesDefinition {
    /// Every variable defined, in order.
    variable_definitions: many VariableDefinition,
  }
);

ast_node!(
  /// An operation definition — the shorthand `{ … }` or the full keyword form.
  OperationDefinition => K::OperationDefinition {
    /// The operation's description, if one was written before it.
    ///
    /// Not in the GraphQL grammar; `syntactic/` accepts a leading string here as a
    /// frozen-parser compatibility extension and the acceptance-parity gate compares the two.
    description: opt Description,
    /// The operation's keyword, absent in the shorthand form.
    operation_type: opt OperationType,
    /// The operation's name, if it was given one.
    ///
    /// The keyword is inside [`OperationType`], so this needs no index — see the module docs.
    name: tok K::Name,
    /// The operation's variable definitions, if it declares any.
    variables_definition: opt VariablesDefinition,
    /// The operation's directives, if it was given any.
    directives: opt Directives,
    /// The operation's selection set.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  /// A fragment definition, `fragment F on T { … }`.
  FragmentDefinition => K::FragmentDefinition {
    /// The fragment's description, if one was written before it.
    description: opt Description,
    /// The fragment's name — the **second** `Name` token, after the `fragment` keyword.
    name: tok_nth 1 K::Name,
    /// The type condition's type. There is no `TypeCondition` node in this kind space.
    type_condition: opt NamedType,
    /// The fragment's directives, if it was given any.
    directives: opt Directives,
    /// The fragment's selection set.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  /// An executable-only document root.
  ///
  /// Not what [`parse_str`](crate::graphql::lossless::parse_str) builds — that parses the mixed
  /// [`Document`](super::document::Document). A consumer that will only accept executable syntax
  /// drives `executable_document` directly.
  ExecutableDocument => K::ExecutableDocument {
    /// Every operation the document defines, in order.
    operation_definitions: many OperationDefinition,
    /// Every fragment the document defines, in order.
    fragment_definitions: many FragmentDefinition,
  }
);
