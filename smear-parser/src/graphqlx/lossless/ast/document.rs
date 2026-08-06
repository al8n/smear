//! Typed wrappers over the two document roots this file owns.
//!
//! The third — [`ExecutableDocument`](super::executable::ExecutableDocument) — sits beside the
//! executable definitions it holds, exactly as the production that builds it does.
//!
//! # A document sorts its entries by kind
//!
//! `apollo-parser` gives a document one `definitions()` over a generated `Definition` union enum.
//! This layer has no union, so a document exposes one iterator per entry kind — eighteen for the
//! mixed root, sixteen for the SDL-only one. Complete and checkable, but it loses document order
//! **across** kinds; a formatter walks [`syntax`](Document::syntax)`().children()` instead. See
//! `ast/value.rs`'s module docs for the same ruling and the same Phase B answer.
//!
//! **Both roots admit imports**, which is the one entry kind GraphQL's twin has no counterpart
//! for, and it is why the counts here are one higher than the shapes otherwise suggest.

use crate::{
  ast_node,
  graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      definition::{
        DirectiveDefinition, EnumTypeDefinition, InputObjectTypeDefinition,
        InterfaceTypeDefinition, ObjectTypeDefinition, ScalarTypeDefinition, SchemaDefinition,
        UnionTypeDefinition,
      },
      executable::{FragmentDefinition, OperationDefinition},
      extension::{
        EnumTypeExtension, InputObjectTypeExtension, InterfaceTypeExtension, ObjectTypeExtension,
        ScalarTypeExtension, SchemaExtension, UnionTypeExtension,
      },
      import::ImportDefinition,
    },
  },
};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A mixed executable-plus-type-system document — what
  /// [`parse_str`](crate::graphqlx::lossless::parse_str) builds.
  Document => K::Document {
    /// Every import the document makes, in order.
    import_definitions: many ImportDefinition,
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
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An SDL-only document root.
  ///
  /// [`Document`] without the two executable kinds, which is the whole difference between the two
  /// roots. Not what [`parse_str`](crate::graphqlx::lossless::parse_str) builds — a schema-only
  /// consumer calls
  /// [`parse_type_system_document`](crate::graphqlx::lossless::parse_type_system_document), which
  /// reports an executable definition rather than admitting one.
  TypeSystemDocument => K::TypeSystemDocument {
    /// Every import the document makes, in order.
    import_definitions: many ImportDefinition,
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
