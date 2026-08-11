//! Typed wrappers over the fourteen value node kinds.
//!
//! # Why a value position carries eleven getters instead of one
//!
//! `apollo-parser` gives a value position a single `value() -> Option<Value>`, where `Value` is a
//! generated **union enum** casting on any of the value kinds. This layer has no such union: the
//! inventory is one wrapper per node kind and a union is not a node kind, so [`ObjectField`],
//! [`DefaultValue`] and [`Argument`](super::directive::Argument) each expose one `opt` getter per
//! value kind and at most one of the eleven answers `Some`. Eleven and not GraphQL's nine, because
//! [`SetValue`] and [`MapValue`] are values here too.
//!
//! That is complete and it is checkable — every getter is a kind test with a decline to assert —
//! but it does lose one thing a union would keep: **document order across kinds**. [`ListValue`]
//! yields its integers and its strings through separate iterators, so a consumer that needs a
//! list's members in the order they were written must walk
//! [`syntax`](ListValue::syntax)`().children()` itself. A `Value` union is out of scope here
//! rather than overlooked.
//!
//! # The two GraphQLx shapes that force a *plural* getter where GraphQL takes a singular one
//!
//! - **[`MapEntry`] holds two values.** `Value => Value`, both halves full values and a map key
//!   not restricted to a name, so an `opt` getter would answer the key whenever the two are the
//!   same kind and the value would be unreachable. Its eleven getters are therefore `many`, and a
//!   consumer that needs the halves apart walks the entry's children: the key is the first, the
//!   value the one after the `=>`.
//! - **[`SetValue`] holds a run of them.** `set { … }` is a collection like [`ListValue`], not a
//!   single-valued position.
//!
//! # An enum value wraps a whole path
//!
//! Divergence 9's value half: `graphqlx::ast::EnumValue` carries a `Path`, so `Colour` is
//! `EnumValue > Path > Name` and `::ns::Colour` is a fully qualified one. [`EnumValue::path`] is a
//! *child* getter for that reason, where GraphQL's `EnumValue::name` is a token getter. The SDL
//! half diverges the other way — see
//! [`EnumValueDefinition`](super::definition::EnumValueDefinition), whose value is a bare `Name`.

use crate::{
  ast_node,
  parser::graphqlx::{kinds::SyntaxKind as K, lossless::ast::ty::Path},
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A variable, `$name`.
  ///
  /// The same node kind in a value position and in a variable *definition*: a variable is a
  /// variable wherever it appears, and a typed accessor that had to match two kinds for it would
  /// be paying for a distinction the grammar does not make.
  ///
  /// Named `VariableValue` and not `Variable`, because this kind space takes its node names from
  /// GraphQLx's own AST carriers.
  VariableValue => K::VariableValue {
    /// The variable's name, which the `$` precedes.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An integer literal value.
  IntValue => K::IntValue {
    /// The literal token — one image over all four radices, the radix staying readable from its
    /// text.
    int_token: tok K::Int,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A float literal value.
  FloatValue => K::FloatValue {
    /// The literal token — one image over both radices.
    float_token: tok K::Float,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A string literal value, inline or block.
  ///
  /// One node kind over two token kinds: the block/inline distinction is the *token*'s and
  /// survives on it, so a consumer that cares reads
  /// [`string_token`](StringValue::string_token)`().kind()` and one that does not is spared
  /// matching two node kinds for one grammar production. The one position that narrows — an
  /// import's `from "…"` — narrows on the token, so it is the same node either way.
  StringValue => K::StringValue {
    /// The literal token — [`InlineString`](K::InlineString) for `"s"`,
    /// [`BlockString`](K::BlockString) for `"""s"""`.
    string_token: tok_any K::InlineString | K::BlockString,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A `true` or `false` value.
  BooleanValue => K::BooleanValue {
    /// The `true` or `false` token, which the lexer hands back as an ordinary name.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A `null` value.
  NullValue => K::NullValue {
    /// The `null` token, which the lexer hands back as an ordinary name.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An enum value in a **value** position — a whole path, not a name.
  ///
  /// See this module's docs for divergence 9 and for the SDL half, which goes the other way.
  EnumValue => K::EnumValue {
    /// The path naming the value, `Colour` or `::ns::Colour`.
    path: opt Path,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A list value, `[ … ]`.
  ///
  /// One iterator per member kind — see this module's docs for what that costs and why.
  ListValue => K::ListValue {
    /// Every variable in the list.
    variable_values: many VariableValue,
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
    /// Every set in the list.
    set_values: many SetValue,
    /// Every map in the list.
    map_values: many MapValue,
    /// Every object in the list.
    object_values: many ObjectValue,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A set value, `set { … }` — GraphQLx only.
  ///
  /// [`ListValue`]'s shape under a contextual keyword. The keyword is *inside* the node, the
  /// production retro-wrapping at a mark minted in front of it, so `set` alone and `set::Thing` —
  /// which are ordinary enum values — stay distinguishable from this without re-reading the text.
  SetValue => K::SetValue {
    /// Every variable in the set.
    variable_values: many VariableValue,
    /// Every integer literal in the set.
    int_values: many IntValue,
    /// Every float literal in the set.
    float_values: many FloatValue,
    /// Every string literal in the set.
    string_values: many StringValue,
    /// Every boolean in the set.
    boolean_values: many BooleanValue,
    /// Every `null` in the set.
    null_values: many NullValue,
    /// Every enum value in the set.
    enum_values: many EnumValue,
    /// Every list in the set.
    list_values: many ListValue,
    /// Every nested set in the set.
    set_values: many SetValue,
    /// Every map in the set.
    map_values: many MapValue,
    /// Every object in the set.
    object_values: many ObjectValue,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A map value, `map { k => v, … }` — GraphQLx only.
  MapValue => K::MapValue {
    /// Every entry of the map, in order.
    map_entries: many MapEntry,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One entry of a map value, `key => value` — GraphQLx only.
  ///
  /// **Both halves are full values, so every getter here is plural** — see this module's docs.
  MapEntry => K::MapEntry {
    /// Every half of the entry that is a variable.
    variable_values: many VariableValue,
    /// Every half of the entry that is an integer literal.
    int_values: many IntValue,
    /// Every half of the entry that is a float literal.
    float_values: many FloatValue,
    /// Every half of the entry that is a string literal.
    string_values: many StringValue,
    /// Every half of the entry that is a boolean.
    boolean_values: many BooleanValue,
    /// Every half of the entry that is `null`.
    null_values: many NullValue,
    /// Every half of the entry that is an enum value.
    enum_values: many EnumValue,
    /// Every half of the entry that is a list.
    list_values: many ListValue,
    /// Every half of the entry that is a set.
    set_values: many SetValue,
    /// Every half of the entry that is a nested map.
    map_values: many MapValue,
    /// Every half of the entry that is an object.
    object_values: many ObjectValue,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An object value, `{ … }`.
  ObjectValue => K::ObjectValue {
    /// Every field of the object, in order.
    object_fields: many ObjectField,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One field of an object value, `name : value`.
  ///
  /// **The key is a plain `Name`, not a path.** GraphQLx widened the enum value and the directive
  /// name and left the object field's key alone, so `{ ns::a: 1 }` is not a field with a qualified
  /// key — the `::` ends the field and is recovered.
  ObjectField => K::ObjectField {
    /// The field's name.
    name: tok K::Name,
    /// The field's value, when it is a variable.
    variable_value: opt VariableValue,
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
    /// The field's value, when it is a set.
    set_value: opt SetValue,
    /// The field's value, when it is a map.
    map_value: opt MapValue,
    /// The field's value, when it is an object.
    object_value: opt ObjectValue,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A default value, `= value`.
  ///
  /// The grammar takes a **const** value here, which forbids a variable. This suite keeps that out
  /// of the shape and leaves it to validation, so [`variable_value`](DefaultValue::variable_value)
  /// can genuinely answer `Some` — and a consumer reporting the mistake needs it to.
  ///
  /// A generic parameter's default is **not** one of these: `Name = Type` introduces a type
  /// reference, so [`DefinitionTypeParam`](super::generic::DefinitionTypeParam) projects the four
  /// type kinds and never this node. The two spell the same token and mean different things.
  DefaultValue => K::DefaultValue {
    /// The default, when it is a variable.
    variable_value: opt VariableValue,
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
    /// The default, when it is a set.
    set_value: opt SetValue,
    /// The default, when it is a map.
    map_value: opt MapValue,
    /// The default, when it is an object.
    object_value: opt ObjectValue,
  }
);
