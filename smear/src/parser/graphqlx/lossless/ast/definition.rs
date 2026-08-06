//! Typed wrappers over the twenty SDL definition node kinds.
//!
//! # No wrapper in this dialect reaches a token by index, and that is a grammar fact
//!
//! The GraphQL twin's `scalar S` puts the keyword and the name under one node as two
//! [`Name`](K::Name) tokens, so its `name` getter has to be `tok_nth 1` — the second `Name` — and
//! its module docs spend a paragraph on the position that made the rule (`repeatable`, a `Name` at
//! index 2 when written and absent when not).
//!
//! **GraphQLx has no such getter anywhere.** Every named SDL definition's name is a
//! [`DefinitionName`] node and every extension's target an
//! [`ExtensionName`](super::generic::ExtensionName) node, so a definition's keyword is the only
//! direct `Name` token it has and the name is reached as a child. `repeatable` and `on` in a
//! [`DirectiveDefinition`] are still bare `Name` tokens and still take no getter — but here that
//! costs nothing, because the name they would have shifted is not a token in the first place.
//!
//! The plan predicted the opposite ("GraphQLx has more of these, not fewer"). The prediction was
//! read off the *keyword* count; what decides the question is where the name lives, and GraphQLx
//! moved it into a node.
//!
//! # A description is a token here, not a node
//!
//! Six kinds in this dialect carry one, and every `description` getter is `tok_any` over the two
//! string images. [`executable`](super::executable)'s module docs carry the census's reason.
//!
//! # Two names the kind space spells differently from GraphQL's
//!
//! [`ImplementInterfaces`] (not `ImplementsInterfaces`) and [`RootOperationTypesDefinition`] (not
//! `RootOperationTypeDefinitions`) are GraphQLx's own AST carrier names, which is where this kind
//! space takes every node name from. The wrapper follows the kind, so the two dialects' wrappers
//! do not line up under a shared name even where their shapes agree.

use crate::{
  ast_node,
  parser::graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      directive::Directives,
      generic::{DefinitionName, WhereClause},
      ty::{DefinitionTypePath, ListType, MapType, SetType, TypePath},
      value::DefaultValue,
    },
  },
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One input value definition — an argument definition, or an input object's field.
  InputValueDefinition => K::InputValueDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The input value's name.
    ///
    /// The definition's only direct `Name` token: its type is a node and its description a string.
    name: tok K::Name,
    /// The input value's type, when it is a path type.
    definition_type_path: opt DefinitionTypePath,
    /// The input value's type, when it is a list.
    list_type: opt ListType,
    /// The input value's type, when it is a set.
    set_type: opt SetType,
    /// The input value's type, when it is a map.
    map_type: opt MapType,
    /// The input value's default, if it was given one.
    default_value: opt DefaultValue,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A field's or directive's argument definitions, `( … )`.
  ArgumentsDefinition => K::ArgumentsDefinition {
    /// Every argument defined, in order.
    input_value_definitions: many InputValueDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One field definition, `name(args) : T @directives`.
  FieldDefinition => K::FieldDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The field's name — its only direct `Name` token, the argument names living inside the
    /// [`ArgumentsDefinition`].
    name: tok K::Name,
    /// The field's argument definitions, if it declares any.
    arguments_definition: opt ArgumentsDefinition,
    /// The field's type, when it is a path type.
    definition_type_path: opt DefinitionTypePath,
    /// The field's type, when it is a list.
    list_type: opt ListType,
    /// The field's type, when it is a set.
    set_type: opt SetType,
    /// The field's type, when it is a map.
    map_type: opt MapType,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An object or interface type's fields block, `{ … }`.
  FieldsDefinition => K::FieldsDefinition {
    /// Every field defined, in order.
    field_definitions: many FieldDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
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
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An `implements A & B` clause.
  ///
  /// Spelled `ImplementInterfaces` — see this module's docs.
  ImplementInterfaces => K::ImplementInterfaces {
    /// Every interface named, in order.
    ///
    /// A [`TypePath`] each, so `implements ns::I<A>` names one qualified interface with generic
    /// arguments rather than an interface followed by junk.
    type_paths: many TypePath,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A union's `= A | B` member list.
  UnionMemberTypes => K::UnionMemberTypes {
    /// Every member type named, in order.
    type_paths: many TypePath,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
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
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One enum value definition.
  ///
  /// **The value is a bare `Name` token**, where a value position's enum value is a whole
  /// [`EnumValue`](super::value::EnumValue) over a path. Divergence 9's SDL half, and it is the
  /// one place the two dialects' enum handling crosses over: GraphQL routes both positions through
  /// one production and GraphQLx cannot, an SDL enum value being unqualifiable.
  EnumValueDefinition => K::EnumValueDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The value's name — its only direct `Name` token.
    name: tok K::Name,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An enum type's values block, `{ … }`.
  EnumValuesDefinition => K::EnumValuesDefinition {
    /// Every value defined, in order.
    enum_value_definitions: many EnumValueDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One root operation type definition, `query : Q`.
  ///
  /// GraphQLx has no `OperationType` node, so the keyword is a bare token here exactly as it is on
  /// an [`OperationDefinition`](super::executable::OperationDefinition) — and it is this node's
  /// only direct `Name` token, the target being a [`TypePath`].
  RootOperationTypeDefinition => K::RootOperationTypeDefinition {
    /// Which operation this root serves.
    operation_type: tok K::Name,
    /// The type that serves it.
    type_path: opt TypePath,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A schema's root operation types block, `{ … }`.
  ///
  /// Spelled `RootOperationTypesDefinition` — see this module's docs.
  RootOperationTypesDefinition => K::RootOperationTypesDefinition {
    /// Every root operation type defined, in order.
    root_operation_type_definitions: many RootOperationTypeDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A scalar type definition.
  ScalarTypeDefinition => K::ScalarTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The scalar's name.
    definition_name: opt DefinitionName,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An object type definition.
  ObjectTypeDefinition => K::ObjectTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The type's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The type's `implements` clause, if it has one.
    implement_interfaces: opt ImplementInterfaces,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The type's `where` clause, if it has one.
    ///
    /// A sibling of the fields block, not a wrapper around it: the census gives the constrained
    /// shape no kind of its own, both halves already being nodes.
    where_clause: opt WhereClause,
    /// The type's fields, if it declares any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An interface type definition.
  ///
  /// [`ObjectTypeDefinition`]'s shape keyword for keyword; the two grammars are identical after
  /// their keyword.
  InterfaceTypeDefinition => K::InterfaceTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The interface's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The interface's own `implements` clause, if it has one.
    implement_interfaces: opt ImplementInterfaces,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The interface's `where` clause, if it has one.
    where_clause: opt WhereClause,
    /// The interface's fields, if it declares any.
    fields_definition: opt FieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A union type definition.
  ///
  /// The one definition whose `where` clause comes **after** the thing it constrains and requires
  /// it: the production parses the members and then the clause, which is the reverse of every
  /// other `where` site. The getters are order-blind, so that shows up only in the tree.
  UnionTypeDefinition => K::UnionTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The union's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The union's members, if it names any.
    union_member_types: opt UnionMemberTypes,
    /// The union's `where` clause, if it has one.
    where_clause: opt WhereClause,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An enum type definition.
  ///
  /// The one type-system definition with **no** `where` clause in its grammar.
  EnumTypeDefinition => K::EnumTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The enum's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The enum's values, if it declares any.
    enum_values_definition: opt EnumValuesDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An input object type definition.
  InputObjectTypeDefinition => K::InputObjectTypeDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The input object's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The input object's `where` clause, if it has one.
    where_clause: opt WhereClause,
    /// The input object's fields, if it declares any.
    input_fields_definition: opt InputFieldsDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A directive definition.
  ///
  /// **`repeatable` gets no getter.** It is a bare `Name` token between the argument definitions
  /// and the `on`, so reaching it means walking the node's tokens and testing the spelling — which
  /// is what a consumer wanting it must do. Unlike in GraphQL, its optionality costs nothing else:
  /// the directive's own name is a [`DefinitionName`] node, so no index shifts behind it.
  ///
  /// This is also the one `where` site the grammar puts at the *end* of a definition, forcing
  /// nothing after it.
  DirectiveDefinition => K::DirectiveDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The directive's name, and the generic parameters it declares.
    definition_name: opt DefinitionName,
    /// The directive's argument definitions, if it declares any.
    arguments_definition: opt ArgumentsDefinition,
    /// The locations the directive may be applied at.
    directive_locations: opt DirectiveLocations,
    /// The definition's `where` clause, if it has one.
    where_clause: opt WhereClause,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A schema definition.
  ///
  /// No name getter of any form: a schema has nothing to be called, so the one `Name` token
  /// directly beneath this node is the `schema` keyword and there is no [`DefinitionName`] child.
  SchemaDefinition => K::SchemaDefinition {
    /// The definition's description, if it was given one.
    description: tok_any K::InlineString | K::BlockString,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
    /// The schema's root operation types.
    root_operation_types_definition: opt RootOperationTypesDefinition,
  }
);
