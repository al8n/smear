//! Typed wrappers over the twenty-two SDL definition node kinds.
//!
//! # Every named definition reaches its name by position
//!
//! `scalar S`, `type T`, `directive @d` — the keyword and the name are both
//! [`Name`](K::Name) tokens under the same node, this kind space having no per-keyword token
//! kind and no `Name` node, so `cast::token`'s first-match answer is the keyword. Each `name`
//! getter below is therefore `tok_nth 1`: the **second** `Name`, which is the position the
//! production's two consecutive `expect`s force. A definition whose name is missing errs out of
//! the production entirely, so there is no second `Name` for the index to land on wrongly.
//!
//! Two nodes take no `name` getter at all, and both are grammar rather than oversight:
//! [`SchemaDefinition`], because a schema has nothing to be called, and
//! [`EnumValueDefinition`], whose name lives inside the [`EnumValue`] node it wraps.
//!
//! **`repeatable` gets no getter**, and it is the counter-example that bounds the positional
//! rule: it is a `Name` at index 2 when written and absent when not, so an index would answer
//! `on` for a directive that is not repeatable. A consumer testing for it walks the node's
//! tokens.
//!
//! # `Description` wraps its string token directly
//!
//! Unlike `apollo-parser`, which nests a `STRING_VALUE` inside its `DESCRIPTION`, so
//! [`Description::string_token`] is a token getter and not a child getter — and it has to reach
//! two token kinds, `"doc"` being a [`String`](K::String) and `"""doc"""` a
//! [`BlockString`](K::BlockString).

use crate::{
  ast_node,
  parser::graphql::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      directive::Directives,
      ty::{ListType, NamedType, NonNullType},
      value::{DefaultValue, EnumValue},
    },
  },
};

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A definition's leading documentation string.
  Description => K::Description {
    /// The string token — [`String`](K::String) for `"doc"`, [`BlockString`](K::BlockString) for
    /// `"""doc"""`. One getter reaches both; the kind stays on the token.
    string_token: tok_any K::String | K::BlockString,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// One input value definition — an argument definition, or an input object's field.
  InputValueDefinition => K::InputValueDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The input value's name.
    ///
    /// No index: this production opens with the name, no keyword before it.
    name: tok K::Name,
    /// The input value's type, when it is a named type.
    named_type: opt NamedType,
    /// The input value's type, when it is a list.
    list_type: opt ListType,
    /// The input value's type, when it is non-null.
    non_null_type: opt NonNullType,
    /// The input value's default, if it was given one.
    default_value: opt DefaultValue,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A field's or directive's argument definitions, `( … )`.
  ArgumentsDefinition => K::ArgumentsDefinition {
    /// Every argument defined, in order.
    input_value_definitions: many InputValueDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// One field definition, `name(args) : T @directives`.
  FieldDefinition => K::FieldDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The field's name. No index: this production opens with the name.
    name: tok K::Name,
    /// The field's argument definitions, if it declares any.
    arguments_definition: opt ArgumentsDefinition,
    /// The field's type, when it is a named type.
    named_type: opt NamedType,
    /// The field's type, when it is a list.
    list_type: opt ListType,
    /// The field's type, when it is non-null.
    non_null_type: opt NonNullType,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An object or interface type's fields block, `{ … }`.
  FieldsDefinition => K::FieldsDefinition {
    /// Every field defined, in order.
    field_definitions: many FieldDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An input object type's fields block, `{ … }`.
  ///
  /// A separate node kind from [`FieldsDefinition`] because the two blocks admit different
  /// members; a typed accessor telling them apart at run time would be paying for a difference
  /// the grammar already makes.
  InputFieldsDefinition => K::InputFieldsDefinition {
    /// Every input field defined, in order.
    input_value_definitions: many InputValueDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An `implements A & B` clause.
  ImplementsInterfaces => K::ImplementsInterfaces {
    /// Every interface named, in order.
    interfaces: many NamedType,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A union's `= A | B` member list.
  UnionMemberTypes => K::UnionMemberTypes {
    /// Every member type named, in order.
    member_types: many NamedType,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A directive definition's `on FIELD | QUERY` location list.
  ///
  /// A location has no node kind of its own, so the locations are bare [`Name`](K::Name) tokens
  /// inside this one node — several of one kind, which `cast::token` cannot express.
  DirectiveLocations => K::DirectiveLocations {
    /// Every location named, in order.
    locations: toks K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// One enum value definition.
  EnumValueDefinition => K::EnumValueDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The value itself, as the same [`EnumValue`] node a value position builds.
    enum_value: opt EnumValue,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An enum type's values block, `{ … }`.
  EnumValuesDefinition => K::EnumValuesDefinition {
    /// Every value defined, in order.
    enum_value_definitions: many EnumValueDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An operation type keyword — `query`, `mutation` or `subscription`.
  ///
  /// One node kind for both positions it appears in: introducing an operation, and naming a root
  /// operation type. A formatter or an accessor finds the same node either way.
  OperationType => K::OperationType {
    /// The keyword token, which the lexer hands back as an ordinary name.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// One root operation type definition, `query : Q`.
  RootOperationTypeDefinition => K::RootOperationTypeDefinition {
    /// Which operation this root serves.
    operation_type: opt OperationType,
    /// The type that serves it.
    named_type: opt NamedType,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A schema's root operation types block, `{ … }`.
  RootOperationTypeDefinitions => K::RootOperationTypeDefinitions {
    /// Every root operation type defined, in order.
    root_operation_type_definitions: many RootOperationTypeDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A scalar type definition.
  ScalarTypeDefinition => K::ScalarTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The scalar's name — the second `Name`, after the `scalar` keyword.
    name: tok_nth 1 K::Name,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An object type definition.
  ObjectTypeDefinition => K::ObjectTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The type's name — the second `Name`, after the `type` keyword.
    name: tok_nth 1 K::Name,
    /// The type's `implements` clause, if it has one.
    implements_interfaces: opt ImplementsInterfaces,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The type's fields, if it declares any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An interface type definition.
  ///
  /// [`ObjectTypeDefinition`]'s shape keyword for keyword; the two grammars are identical after
  /// their keyword.
  InterfaceTypeDefinition => K::InterfaceTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The interface's name — the second `Name`, after the `interface` keyword.
    name: tok_nth 1 K::Name,
    /// The interface's own `implements` clause, if it has one.
    implements_interfaces: opt ImplementsInterfaces,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The interface's fields, if it declares any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A union type definition.
  UnionTypeDefinition => K::UnionTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The union's name — the second `Name`, after the `union` keyword.
    name: tok_nth 1 K::Name,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The union's members, if it names any.
    union_member_types: opt UnionMemberTypes,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An enum type definition.
  EnumTypeDefinition => K::EnumTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The enum's name — the second `Name`, after the `enum` keyword.
    name: tok_nth 1 K::Name,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The enum's values, if it declares any.
    enum_values_definition: opt EnumValuesDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An input object type definition.
  InputObjectTypeDefinition => K::InputObjectTypeDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The input object's name — the second `Name`, after the `input` keyword.
    name: tok_nth 1 K::Name,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The input object's fields, if it declares any.
    input_fields_definition: opt InputFieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A directive definition.
  DirectiveDefinition => K::DirectiveDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The directive's name — the second `Name`, after the `directive` keyword and the `@`.
    ///
    /// The optional `repeatable` and the `on` are `Name` tokens too, but both follow the name,
    /// so the index is unaffected by either.
    name: tok_nth 1 K::Name,
    /// The directive's argument definitions, if it declares any.
    arguments_definition: opt ArgumentsDefinition,
    /// The locations the directive may be applied at.
    directive_locations: opt DirectiveLocations,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A schema definition.
  ///
  /// No `name` getter: a schema has nothing to be called, so the one `Name` directly beneath this
  /// node is the `schema` keyword.
  SchemaDefinition => K::SchemaDefinition {
    /// The definition's description, if it was given one.
    description: opt Description,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The schema's root operation types.
    root_operation_type_definitions: opt RootOperationTypeDefinitions,
  }
);
