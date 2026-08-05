//! Typed wrappers over the seven type-system extension kinds and the two document roots.
//!
//! # An extension's name is its *third* `Name`, not its second
//!
//! `extend`, the shape keyword and the extended type's name are three [`Name`](K::Name) tokens
//! under one node — the `extend` lands inside its extension, not beside it, because the
//! production is a retro-wrap around a mark minted before it. So every `name` getter here is
//! `tok_nth 2` where `ast/definition.rs`'s are `tok_nth 1`. [`SchemaExtension`] takes no `name`
//! getter at all, a schema having nothing to be called.
//!
//! # A document sorts its definitions by kind
//!
//! `apollo-parser` gives [`Document`] one `definitions()` over a generated `Definition` union
//! enum. This layer has no union, so a document exposes one iterator per definition kind —
//! seventeen for the mixed root, fifteen for the SDL-only one. Complete and checkable, but it
//! loses document order **across** kinds; a formatter walks
//! [`syntax`](Document::syntax)`().children()` instead. See `ast/value.rs`'s module docs for the
//! same ruling and the same Phase B answer.

use crate::{
  ast_node,
  graphql::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      definition::{
        DirectiveDefinition, EnumTypeDefinition, EnumValuesDefinition, FieldsDefinition,
        ImplementsInterfaces, InputFieldsDefinition, InputObjectTypeDefinition,
        InterfaceTypeDefinition, ObjectTypeDefinition, RootOperationTypeDefinitions,
        ScalarTypeDefinition, SchemaDefinition, UnionMemberTypes, UnionTypeDefinition,
      },
      directive::Directives,
      executable::{FragmentDefinition, OperationDefinition},
    },
  },
};

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A scalar type extension, `extend scalar S @d`.
  ///
  /// The one extension whose directives the grammar makes mandatory — it has no other tail.
  ScalarTypeExtension => K::ScalarTypeExtension {
    /// The extended scalar's name — the third `Name`, after `extend` and `scalar`.
    name: tok_nth 2 K::Name,
    /// The directives the extension adds.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// An object type extension, `extend type T …`.
  ObjectTypeExtension => K::ObjectTypeExtension {
    /// The extended type's name — the third `Name`, after `extend` and `type`.
    name: tok_nth 2 K::Name,
    /// The interfaces the extension adds, if any.
    implements_interfaces: opt ImplementsInterfaces,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The fields the extension adds, if any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// An interface type extension, `extend interface N …`.
  InterfaceTypeExtension => K::InterfaceTypeExtension {
    /// The extended interface's name — the third `Name`, after `extend` and `interface`.
    name: tok_nth 2 K::Name,
    /// The interfaces the extension adds, if any.
    implements_interfaces: opt ImplementsInterfaces,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The fields the extension adds, if any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A union type extension, `extend union U …`.
  UnionTypeExtension => K::UnionTypeExtension {
    /// The extended union's name — the third `Name`, after `extend` and `union`.
    name: tok_nth 2 K::Name,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The members the extension adds, if any.
    union_member_types: opt UnionMemberTypes,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// An enum type extension, `extend enum E …`.
  EnumTypeExtension => K::EnumTypeExtension {
    /// The extended enum's name — the third `Name`, after `extend` and `enum`.
    name: tok_nth 2 K::Name,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The values the extension adds, if any.
    enum_values_definition: opt EnumValuesDefinition,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// An input object type extension, `extend input In …`.
  InputObjectTypeExtension => K::InputObjectTypeExtension {
    /// The extended input object's name — the third `Name`, after `extend` and `input`.
    name: tok_nth 2 K::Name,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The input fields the extension adds, if any.
    input_fields_definition: opt InputFieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A schema extension, `extend schema …`.
  ///
  /// No `name` getter: the two `Name` tokens beneath this node are `extend` and `schema`.
  SchemaExtension => K::SchemaExtension {
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The root operation types the extension adds, if any.
    root_operation_type_definitions: opt RootOperationTypeDefinitions,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// A mixed executable-plus-type-system document — what
  /// [`parse_str`](crate::graphql::lossless::parse_str) builds.
  Document => K::Document {
    /// Every operation the document defines, in order.
    operation_definitions: many OperationDefinition,
    /// Every fragment the document defines, in order.
    fragment_definitions: many FragmentDefinition,
    /// Every scalar type the document defines, in order.
    scalar_type_definitions: many ScalarTypeDefinition,
    /// Every object type the document defines, in order.
    object_type_definitions: many ObjectTypeDefinition,
    /// Every interface type the document defines, in order.
    interface_type_definitions: many InterfaceTypeDefinition,
    /// Every union type the document defines, in order.
    union_type_definitions: many UnionTypeDefinition,
    /// Every enum type the document defines, in order.
    enum_type_definitions: many EnumTypeDefinition,
    /// Every input object type the document defines, in order.
    input_object_type_definitions: many InputObjectTypeDefinition,
    /// Every directive the document defines, in order.
    directive_definitions: many DirectiveDefinition,
    /// Every schema the document defines, in order.
    schema_definitions: many SchemaDefinition,
    /// Every scalar type extension in the document, in order.
    scalar_type_extensions: many ScalarTypeExtension,
    /// Every object type extension in the document, in order.
    object_type_extensions: many ObjectTypeExtension,
    /// Every interface type extension in the document, in order.
    interface_type_extensions: many InterfaceTypeExtension,
    /// Every union type extension in the document, in order.
    union_type_extensions: many UnionTypeExtension,
    /// Every enum type extension in the document, in order.
    enum_type_extensions: many EnumTypeExtension,
    /// Every input object type extension in the document, in order.
    input_object_type_extensions: many InputObjectTypeExtension,
    /// Every schema extension in the document, in order.
    schema_extensions: many SchemaExtension,
  }
);

ast_node!(
  lang = crate::graphql::kinds::GraphQLLang;
  /// An SDL-only document root.
  ///
  /// [`Document`] without the two executable kinds, which is the whole difference between the
  /// two roots. Not reachable from [`parse_str`](crate::graphql::lossless::parse_str); a
  /// schema-only consumer drives `type_system_document` directly.
  TypeSystemDocument => K::TypeSystemDocument {
    /// Every scalar type the document defines, in order.
    scalar_type_definitions: many ScalarTypeDefinition,
    /// Every object type the document defines, in order.
    object_type_definitions: many ObjectTypeDefinition,
    /// Every interface type the document defines, in order.
    interface_type_definitions: many InterfaceTypeDefinition,
    /// Every union type the document defines, in order.
    union_type_definitions: many UnionTypeDefinition,
    /// Every enum type the document defines, in order.
    enum_type_definitions: many EnumTypeDefinition,
    /// Every input object type the document defines, in order.
    input_object_type_definitions: many InputObjectTypeDefinition,
    /// Every directive the document defines, in order.
    directive_definitions: many DirectiveDefinition,
    /// Every schema the document defines, in order.
    schema_definitions: many SchemaDefinition,
    /// Every scalar type extension in the document, in order.
    scalar_type_extensions: many ScalarTypeExtension,
    /// Every object type extension in the document, in order.
    object_type_extensions: many ObjectTypeExtension,
    /// Every interface type extension in the document, in order.
    interface_type_extensions: many InterfaceTypeExtension,
    /// Every union type extension in the document, in order.
    union_type_extensions: many UnionTypeExtension,
    /// Every enum type extension in the document, in order.
    enum_type_extensions: many EnumTypeExtension,
    /// Every input object type extension in the document, in order.
    input_object_type_extensions: many InputObjectTypeExtension,
    /// Every schema extension in the document, in order.
    schema_extensions: many SchemaExtension,
  }
);
