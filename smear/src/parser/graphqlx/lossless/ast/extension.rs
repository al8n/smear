//! Typed wrappers over the seven type-system extension node kinds.
//!
//! One file rather than a section of `ast/document.rs`, which is where the GraphQL twin keeps
//! them: GraphQLx's productions put the extensions in their own `lossless/extension.rs`, and the
//! wrappers sit beside the productions that build them.
//!
//! # An extension's target is a path, and it is a node
//!
//! `extend`, the shape keyword and the target are all under one node, the production being a
//! retro-wrap around a mark minted before the `extend`. In GraphQL that makes three `Name` tokens
//! and forces a `tok_nth 2` getter on every extension. Here the target is an
//! [`ExtensionName`] node holding a whole path, so the two keywords are the node's only direct
//! `Name` tokens and no getter reaches either.
//!
//! [`SchemaExtension`] takes no target getter at all, a schema having nothing to be called.
//!
//! # No extension is described
//!
//! A description may precede only an executable or a type-system *definition*. A string written in
//! front of an `extend` is reported and stays **outside** the extension's node — so there is no
//! `description` getter here, and a consumer that finds a string before an extension is looking at
//! a diagnostic's subject rather than at the extension's own child.

use crate::{
  ast_node,
  graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      definition::{
        EnumValuesDefinition, FieldsDefinition, ImplementInterfaces, InputFieldsDefinition,
        RootOperationTypesDefinition, UnionMemberTypes,
      },
      directive::Directives,
      generic::{ExtensionName, WhereClause},
    },
  },
};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A scalar type extension, `extend scalar S @d`.
  ///
  /// The one extension whose directives the grammar makes mandatory — it has no other tail.
  ScalarTypeExtension => K::ScalarTypeExtension {
    /// The extended scalar's name.
    extension_name: opt ExtensionName,
    /// The directives the extension adds.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An object type extension, `extend type T …`.
  ObjectTypeExtension => K::ObjectTypeExtension {
    /// The extended type's name, and the generic arguments the extension applies.
    extension_name: opt ExtensionName,
    /// The interfaces the extension adds, if any.
    implement_interfaces: opt ImplementInterfaces,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The `where` clause the extension adds, if any.
    where_clause: opt WhereClause,
    /// The fields the extension adds, if any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An interface type extension, `extend interface N …`.
  InterfaceTypeExtension => K::InterfaceTypeExtension {
    /// The extended interface's name, and the generic arguments the extension applies.
    extension_name: opt ExtensionName,
    /// The interfaces the extension adds, if any.
    implement_interfaces: opt ImplementInterfaces,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The `where` clause the extension adds, if any.
    where_clause: opt WhereClause,
    /// The fields the extension adds, if any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A union type extension, `extend union U …`.
  ///
  /// Its `where` clause comes after the members and requires them, exactly as
  /// [`UnionTypeDefinition`](super::definition::UnionTypeDefinition)'s does.
  UnionTypeExtension => K::UnionTypeExtension {
    /// The extended union's name, and the generic arguments the extension applies.
    extension_name: opt ExtensionName,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The members the extension adds, if any.
    union_member_types: opt UnionMemberTypes,
    /// The `where` clause the extension adds, if any.
    where_clause: opt WhereClause,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An enum type extension, `extend enum E …`.
  EnumTypeExtension => K::EnumTypeExtension {
    /// The extended enum's name, and the generic arguments the extension applies.
    extension_name: opt ExtensionName,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The values the extension adds, if any.
    enum_values_definition: opt EnumValuesDefinition,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An input object type extension, `extend input In …`.
  InputObjectTypeExtension => K::InputObjectTypeExtension {
    /// The extended input object's name, and the generic arguments the extension applies.
    extension_name: opt ExtensionName,
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The `where` clause the extension adds, if any.
    where_clause: opt WhereClause,
    /// The input fields the extension adds, if any.
    input_fields_definition: opt InputFieldsDefinition,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A schema extension, `extend schema …`.
  ///
  /// No name getter: the two `Name` tokens beneath this node are `extend` and `schema`.
  SchemaExtension => K::SchemaExtension {
    /// The directives the extension adds, if any.
    directives: opt Directives,
    /// The root operation types the extension adds, if any.
    root_operation_types_definition: opt RootOperationTypesDefinition,
  }
);
