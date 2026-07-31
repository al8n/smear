//! Typed wrappers over the eleven value node kinds.
//!
//! # Why a value position carries nine getters instead of one
//!
//! `apollo-parser` gives a value position a single `value() -> Option<Value>`, where `Value` is a
//! generated **union enum** casting on any of the value kinds. This layer has no such union:
//! Task 10's inventory is one wrapper per node kind and a union is not a node kind, so
//! `ObjectField`, `DefaultValue` and `Argument` each expose one `opt` getter per value kind and
//! at most one of the nine answers `Some`.
//!
//! That is complete and it is checkable — every getter is a kind test with a decline to assert —
//! but it does lose one thing a union would keep: **document order across kinds**. `ListValue`
//! yields its integers and its strings through separate iterators, so a consumer that needs a
//! list's members in the order they were written must walk `syntax().children()` itself. A
//! `Value` union is the Phase B answer; it is out of scope here rather than overlooked.

use crate::{ast_node, graphql::kinds::SyntaxKind as K};

ast_node!(
  /// A variable, `$name`.
  ///
  /// The same node kind in a value position and in a variable *definition*: a variable is a
  /// variable wherever it appears, and a typed accessor that had to match two kinds for it would
  /// be paying for a distinction the grammar does not make.
  Variable => K::Variable {
    /// The variable's name, which the `$` precedes.
    name: tok K::Name,
  }
);

ast_node!(
  /// An integer literal value.
  IntValue => K::IntValue {
    /// The literal token.
    int_token: tok K::Int,
  }
);

ast_node!(
  /// A float literal value.
  FloatValue => K::FloatValue {
    /// The literal token.
    float_token: tok K::Float,
  }
);

ast_node!(
  /// A string literal value, inline or block.
  ///
  /// One node kind over two token kinds: the block/inline distinction is the *token*'s and
  /// survives on it, so a consumer that cares reads
  /// [`string_token`](StringValue::string_token)`().kind()` and one that does not is spared
  /// matching two node kinds for one grammar production.
  StringValue => K::StringValue {
    /// The literal token — [`String`](K::String) for `"s"`, [`BlockString`](K::BlockString) for
    /// `"""s"""`.
    string_token: tok_any K::String | K::BlockString,
  }
);

ast_node!(
  /// A `true` or `false` value.
  BooleanValue => K::BooleanValue {
    /// The `true` or `false` token, which the lexer hands back as an ordinary name.
    name: tok K::Name,
  }
);

ast_node!(
  /// A `null` value.
  NullValue => K::NullValue {
    /// The `null` token, which the lexer hands back as an ordinary name.
    name: tok K::Name,
  }
);

ast_node!(
  /// An enum value.
  ///
  /// The same node kind in a value position and in an enum value *definition*, for the reason
  /// [`Variable`] records.
  EnumValue => K::EnumValue {
    /// The value's name.
    name: tok K::Name,
  }
);

ast_node!(
  /// A list value, `[ … ]`.
  ///
  /// One iterator per member kind — see this module's docs for what that costs and why.
  ListValue => K::ListValue {
    /// Every variable in the list.
    variables: many Variable,
    /// Every integer literal in the list.
    int_values: many IntValue,
    /// Every float literal in the list.
    float_values: many FloatValue,
    /// Every string literal in the list.
    string_values: many StringValue,
    /// Every boolean in the list.
    boolean_values: many BooleanValue,
    /// Every `null` in the list.
    null_values: many NullValue,
    /// Every enum value in the list.
    enum_values: many EnumValue,
    /// Every nested list in the list.
    list_values: many ListValue,
    /// Every object in the list.
    object_values: many ObjectValue,
  }
);

ast_node!(
  /// An object value, `{ … }`.
  ObjectValue => K::ObjectValue {
    /// Every field of the object, in order.
    object_fields: many ObjectField,
  }
);

ast_node!(
  /// One field of an object value, `name : value`.
  ObjectField => K::ObjectField {
    /// The field's name.
    name: tok K::Name,
    /// The field's value, when it is a variable.
    variable: opt Variable,
    /// The field's value, when it is an integer literal.
    int_value: opt IntValue,
    /// The field's value, when it is a float literal.
    float_value: opt FloatValue,
    /// The field's value, when it is a string literal.
    string_value: opt StringValue,
    /// The field's value, when it is `true` or `false`.
    boolean_value: opt BooleanValue,
    /// The field's value, when it is `null`.
    null_value: opt NullValue,
    /// The field's value, when it is an enum value.
    enum_value: opt EnumValue,
    /// The field's value, when it is a list.
    list_value: opt ListValue,
    /// The field's value, when it is an object.
    object_value: opt ObjectValue,
  }
);

ast_node!(
  /// A default value, `= value`.
  ///
  /// The spec's `DefaultValue` takes a **const** value, which forbids a variable. This suite
  /// keeps that out of the shape and leaves it to validation, so
  /// [`variable`](DefaultValue::variable) can genuinely answer `Some` — and a consumer reporting
  /// the mistake needs it to.
  DefaultValue => K::DefaultValue {
    /// The default, when it is a variable.
    variable: opt Variable,
    /// The default, when it is an integer literal.
    int_value: opt IntValue,
    /// The default, when it is a float literal.
    float_value: opt FloatValue,
    /// The default, when it is a string literal.
    string_value: opt StringValue,
    /// The default, when it is `true` or `false`.
    boolean_value: opt BooleanValue,
    /// The default, when it is `null`.
    null_value: opt NullValue,
    /// The default, when it is an enum value.
    enum_value: opt EnumValue,
    /// The default, when it is a list.
    list_value: opt ListValue,
    /// The default, when it is an object.
    object_value: opt ObjectValue,
  }
);
