//! Typed wrappers over the five executable-definition node kinds.
//!
//! # A description is a **token**, not a node
//!
//! GraphQL's kind space has a `Description` node wrapping the string; GraphQLx's has none — the
//! census's *one token is not a region* — so a described definition carries its description as a
//! direct string token and every `description` getter here is `tok_any`. A port of GraphQL's
//! `description: opt Description` would compile and answer `None` forever.
//!
//! Two kinds share the two string images, and the difference is the level: a description is a bare
//! token of the definition, and a string *value* is a
//! [`StringValue`](super::value::StringValue) node. The two can therefore never be confused, which
//! is the reason the census gave for admitting one kind and not the other.
//!
//! # An operation's keyword is a token and its name is a node
//!
//! GraphQL wraps `query` in an `OperationType` node; GraphQLx's census gives one keyword token no
//! kind of its own, so [`OperationDefinition::operation_type`] is a `tok` getter — and it is exact
//! rather than positional, the operation's *name* living inside a
//! [`DefinitionName`] and the description being a string token. The shorthand `{ … }` form answers
//! `None`, which is the whole difference between the two forms and is visible in the tree.
//!
//! # A fragment declares two generic lists, and only the level tells them apart
//!
//! `fragment <T, U> F<A, B> on X { f }`. The first list is the definition's *implementation*
//! generics and is a **sibling** of the name node; the second is the name's own and is a **child**
//! of it. They are the same node kind, and parsing them in the other order consumes the same
//! tokens and re-prints the same text — so
//! [`FragmentDefinition::executable_definition_type_generics`], matching direct children only,
//! answers the first and never the second.

use crate::{
  ast_node,
  parser::graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      directive::Directives,
      generic::{
        DefinitionName, ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, WhereClause,
      },
      import::ImportDefinition,
      selection::{SelectionSet, TypeCondition},
      ty::{DefinitionTypePath, ListType, MapType, SetType},
      value::{DefaultValue, VariableValue},
    },
  },
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One variable definition, `$v : T = default @directives`.
  ///
  /// The directives here are **const**, and this is the one place in an executable document where
  /// they are. That is a validation flavour rather than a shape, so the wrapper is the same
  /// [`Directives`] every other position reaches.
  VariableDefinition => K::VariableDefinition {
    /// The definition's description, if one was written before it — a bare string token; see this
    /// module's docs.
    description: tok_any K::InlineString | K::BlockString,
    /// The variable being defined.
    variable_value: opt VariableValue,
    /// The variable's type, when it is a path type.
    definition_type_path: opt DefinitionTypePath,
    /// The variable's type, when it is a list.
    list_type: opt ListType,
    /// The variable's type, when it is a set.
    set_type: opt SetType,
    /// The variable's type, when it is a map.
    map_type: opt MapType,
    /// The variable's default, if it was given one.
    default_value: opt DefaultValue,
    /// The definition's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An operation's variable definitions, `( … )`.
  VariablesDefinition => K::VariablesDefinition {
    /// Every variable defined, in order.
    variable_definitions: many VariableDefinition,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An operation definition — the shorthand `{ … }` or the full keyword form.
  OperationDefinition => K::OperationDefinition {
    /// The operation's description, if one was written before it.
    description: tok_any K::InlineString | K::BlockString,
    /// The operation's keyword — `query`, `mutation` or `subscription` — absent in the shorthand
    /// form.
    ///
    /// A bare token, and the operation's only direct `Name` token; see this module's docs.
    operation_type: tok K::Name,
    /// The operation's name, if it was given one.
    ///
    /// A [`DefinitionName`], so `query Q<T = Int>` is grammatical and the parameters are inside
    /// this node rather than beside it.
    definition_name: opt DefinitionName,
    /// The operation's variable definitions, if it declares any.
    variables_definition: opt VariablesDefinition,
    /// The operation's directives, if it was given any.
    directives: opt Directives,
    /// The operation's `where` clause, if it has one.
    ///
    /// The clause and the selection set are **siblings**: the census gives the constrained shape
    /// no kind of its own, both halves already being nodes. Only the named form admits one — the
    /// shorthand's selection set is parsed without it.
    where_clause: opt WhereClause,
    /// The operation's selection set.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A fragment definition, `fragment F on T { … }`.
  FragmentDefinition => K::FragmentDefinition {
    /// The fragment's description, if one was written before it.
    description: tok_any K::InlineString | K::BlockString,
    /// The fragment's **implementation** generics, if it declares any.
    ///
    /// The list written before the name. The name's own list is a child of
    /// [`ExecutableDefinitionName`] and is not reachable here — see this module's docs.
    executable_definition_type_generics: opt ExecutableDefinitionTypeGenerics,
    /// The fragment's name.
    executable_definition_name: opt ExecutableDefinitionName,
    /// The fragment's type condition.
    ///
    /// Recovered rather than required: a definition whose `on` is missing keeps its name, its type
    /// and its selection set, so this can answer `Some` over a node the diagnostic is about.
    type_condition: opt TypeCondition,
    /// The fragment's directives, if it was given any.
    directives: opt Directives,
    /// The fragment's `where` clause, if it has one.
    where_clause: opt WhereClause,
    /// The fragment's selection set.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An executable-only document root.
  ///
  /// Not what [`parse_document`](crate::parser::graphqlx::lossless::parse_document) builds — that parses
  /// the mixed [`Document`](super::document::Document). A consumer that will only accept
  /// executable syntax calls
  /// [`parse_executable_document`](crate::parser::graphqlx::lossless::parse_executable_document), which
  /// reports a type-system definition rather than admitting one. It admits imports, which
  /// is why it has three iterators where GraphQL's has two.
  ExecutableDocument => K::ExecutableDocument {
    /// Every import the document makes, in order.
    import_definitions: many ImportDefinition,
    /// Every operation the document defines, in order.
    operation_definitions: many OperationDefinition,
    /// Every fragment the document defines, in order.
    fragment_definitions: many FragmentDefinition,
  }
);
