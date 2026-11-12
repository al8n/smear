//! Typed wrappers over the two document roots this file owns, and over the inline module whose
//! body holds what the SDL-only root does.
//!
//! The third root — [`ExecutableDocument`](super::executable::ExecutableDocument) — sits beside
//! the executable definitions it holds, exactly as the production that builds it does.
//!
//! # A document sorts its entries by kind
//!
//! `apollo-parser` gives a document one `definitions()` over a generated `Definition` union enum.
//! This layer has no union, so a document exposes one iterator per entry kind — twenty for the
//! mixed root, eighteen for the SDL-only one, and the same eighteen for a module's body. Complete
//! and checkable, but it loses document order **across** kinds; a formatter walks
//! [`syntax`](Document::syntax)`().children()` instead. See `ast/value.rs`'s module docs for the
//! same ruling and the same Phase B answer.
//!
//! **Both roots admit `use` items and modules**, the two entry kinds GraphQL's twin has no
//! counterpart for, and they are why the counts here are two higher than the shapes otherwise
//! suggest.
//!
//! # A module's two names are a keyword and its name, read as one run
//!
//! A [`ModDefinition`]'s `mod` and the name after it are both [`Name`](K::Name) tokens directly
//! beneath it, with no node between them, so they come back through one getter,
//! [`names`](ModDefinition::names), in that order: two getters would have to tell two tokens of
//! one kind apart by position, and this dialect has no positional getter.

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
        DirectiveExtension, EnumTypeExtension, InputObjectTypeExtension, InterfaceTypeExtension,
        ObjectTypeExtension, ScalarTypeExtension, SchemaExtension, UnionTypeExtension,
      },
      use_tree::UseDefinition,
    },
  },
};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An inline module, `mod name { … }`.
  ///
  /// Its body is no node of its own: the braces are the module's tokens and its items are its
  /// direct children, sorted by kind as a document's are.
  ModDefinition => K::ModDefinition {
    /// The module's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The module's own names: the `mod` keyword, then the module's name — see this module's docs.
    names: toks K::Name,
    /// Every `use` item in the body, in order.
    use_definitions: many UseDefinition,
    /// Every module nested in the body, in order.
    mod_definitions: many ModDefinition,
    /// Every scalar type the body defines, in order.
    scalar_type_definitions: many ScalarTypeDefinition,
    /// Every object type the body defines, in order.
    object_type_definitions: many ObjectTypeDefinition,
    /// Every interface type the body defines, in order.
    interface_type_definitions: many InterfaceTypeDefinition,
    /// Every union type the body defines, in order.
    union_type_definitions: many UnionTypeDefinition,
    /// Every enum type the body defines, in order.
    enum_type_definitions: many EnumTypeDefinition,
    /// Every input object type the body defines, in order.
    input_object_type_definitions: many InputObjectTypeDefinition,
    /// Every directive the body defines, in order.
    directive_definitions: many DirectiveDefinition,
    /// Every schema the body defines, in order.
    schema_definitions: many SchemaDefinition,
    /// Every scalar type extension in the body, in order.
    scalar_type_extensions: many ScalarTypeExtension,
    /// Every object type extension in the body, in order.
    object_type_extensions: many ObjectTypeExtension,
    /// Every interface type extension in the body, in order.
    interface_type_extensions: many InterfaceTypeExtension,
    /// Every union type extension in the body, in order.
    union_type_extensions: many UnionTypeExtension,
    /// Every enum type extension in the body, in order.
    enum_type_extensions: many EnumTypeExtension,
    /// Every input object type extension in the body, in order.
    input_object_type_extensions: many InputObjectTypeExtension,
    /// Every schema extension in the body, in order.
    schema_extensions: many SchemaExtension,
    /// Every directive extension in the body, in order.
    directive_extensions: many DirectiveExtension,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A mixed executable-plus-type-system document — what
  /// [`parse_document`](crate::graphqlx::lossless::parse_document) builds.
  Document => K::Document {
    /// Every `use` item in the document, in order.
    use_definitions: many UseDefinition,
    /// Every module at the document's top level, in order.
    mod_definitions: many ModDefinition,
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
    /// Every directive extension in the document, in order.
    directive_extensions: many DirectiveExtension,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An SDL-only document root.
  ///
  /// [`Document`] without the two executable kinds, which is the whole difference between the two
  /// roots. Not what [`parse_document`](crate::graphqlx::lossless::parse_document) builds — a
  /// schema-only consumer calls
  /// [`parse_type_system_document`](crate::graphqlx::lossless::parse_type_system_document), which
  /// reports an executable definition rather than admitting one.
  TypeSystemDocument => K::TypeSystemDocument {
    /// Every `use` item in the document, in order.
    use_definitions: many UseDefinition,
    /// Every module at the document's top level, in order.
    mod_definitions: many ModDefinition,
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
    /// Every directive extension in the document, in order.
    directive_extensions: many DirectiveExtension,
  }
);
