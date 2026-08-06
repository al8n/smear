//! Gate 4 — the typed accessor layer over the GraphQLx lossless CST.
//!
//! Two halves. The **shaped** half, first, is one test per grammar area, each walking a real tree
//! through the wrappers rather than through
//! [`SyntaxNode`](smear_parser::graphqlx::lossless::SyntaxNode). The **totality** half, from
//! [`the_probe_table_is_exactly_the_shipped_inventory`] on, sweeps all 78 wrappers and all 297
//! getters over the shared corpus and answers five questions the shaped half cannot: whether the
//! probe table still matches what the crate ships, whether every getter ever *projected* anything,
//! whether every getter's name states the kind it projects, whether each node kind casts to exactly
//! one wrapper, and whether a getter's answer is ever distinguishable from a kind-blind rival's.
//!
//! **Every fixture is chosen so each getter has something to get wrong**, which is the lesson the
//! GraphQL twin recorded: over `"{ a }"` a `cast::child` is indistinguishable from "take the first
//! child" and a `cast::token` from "take the first token", so a fixture of that shape proves
//! nothing about the getter it exercises. Concretely, every assertion below is of one of three
//! sharp forms:
//!
//! - a cast answers `Some` for its own kind **and `None` for a near neighbour** — `SetValue` and
//!   `ObjectValue` are both brace-delimited collections, `ObjectTypeDefinition` and
//!   `ObjectTypeExtension` differ only by an `extend`, `SetType` and `MapType` differ only by a
//!   `=>`;
//! - an `opt` getter answers **the specific** child rather than merely *a* child — asserted by the
//!   child's text, with a sibling of a different kind sitting before it;
//! - a `tok` getter is proved where the wanted token is **neither the node's first token nor the
//!   only one of its kind** — a field's own name behind an `Alias` that holds another `Name`, an
//!   import specifier's name in front of the `as` that follows it.
//!
//! # What GraphQLx's shape buys, and what it costs
//!
//! Every SDL definition's name is a [`DefinitionName`] **node** and every extension's target is an
//! [`ExtensionName`] node, so the positional `tok_nth` getters the GraphQL twin needs for
//! `scalar S` and `extend scalar S` have **no counterpart here at all** — see
//! `src/graphqlx/lossless/ast/definition.rs`'s module docs. What GraphQLx adds instead is the pair
//! of positions where the grammar puts *two* values or *two* types under one node with nothing
//! between them but a `=>`: [`MapEntry`] and [`MapType`]. Those cannot take `opt` getters, and the
//! two tests below assert the plural form answers both halves.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use std::{
  collections::{BTreeMap, BTreeSet},
  path::PathBuf,
};

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    GraphQLxLang, SyntaxNode, SyntaxToken,
    ast::{
      Alias, Argument, Arguments, ArgumentsDefinition, AstChildren, AstTokens, BooleanValue,
      CastNode, DefaultValue, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam,
      DefinitionTypePath, Directive, DirectiveDefinition, DirectiveLocations, Directives, Document,
      EnumTypeDefinition, EnumTypeExtension, EnumValue, EnumValueDefinition, EnumValuesDefinition,
      ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, ExecutableDocument,
      ExtensionName, ExtensionTypeGenerics, Field, FieldDefinition, FieldsDefinition, FloatValue,
      FragmentDefinition, FragmentSpread, ImplementInterfaces, ImportDefinition, ImportList,
      InlineFragment, InputFieldsDefinition, InputObjectTypeDefinition, InputObjectTypeExtension,
      InputValueDefinition, IntValue, InterfaceTypeDefinition, InterfaceTypeExtension, ListType,
      ListValue, MapEntry, MapType, MapValue, NamedSpecifier, NullValue, ObjectField,
      ObjectTypeDefinition, ObjectTypeExtension, ObjectValue, OperationDefinition, Path,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, SelectionSet, SetType, SetValue,
      StringValue, TypeCondition, TypeGenerics, TypePath, TypeSystemDocument, UnionMemberTypes,
      UnionTypeDefinition, UnionTypeExtension, VariableDefinition, VariableValue,
      VariablesDefinition, WhereClause, WherePredicate, WildcardSpecifier,
    },
    generic::test_support::{
      parse_definition_type_generics, parse_executable_definition_type_generics,
      parse_extension_name, parse_where_clause,
    },
    parse_document, parse_executable_document, parse_type_system_document,
    selection::test_support::parse_selection_set,
    ty::test_support::parse_type_ref,
    value::test_support::parse_value,
  },
};

/// The first descendant of `root` that casts to `N`.
///
/// The whole fixture is searched rather than one known child, so a test states *which node* it is
/// about by naming a wrapper — and a wrapper pointed at a kind the fixture never builds panics
/// here rather than quietly asserting over the wrong node.
fn first<N: CastNode<GraphQLxLang>>(root: &SyntaxNode) -> N {
  root
    .descendants()
    .find_map(N::cast_node)
    .expect("the fixture holds a node of this wrapper's kind")
}

/// The text of a token getter's answer, or `None`.
fn tok(t: Option<SyntaxToken>) -> Option<String> {
  t.map(|t| t.text().to_string())
}

/// The texts a `many` getter's iterator yields, in order.
macro_rules! texts {
  ($iter:expr) => {
    $iter
      .map(|n| n.syntax().text().to_string())
      .collect::<Vec<String>>()
  };
}

/// The texts a `toks` getter's iterator yields, in order.
macro_rules! tok_texts {
  ($iter:expr) => {
    $iter.map(|t| t.text().to_string()).collect::<Vec<String>>()
  };
}

/// The text of an `opt` getter's answer, or `None`.
macro_rules! node_text {
  ($opt:expr) => {
    $opt.map(|n| n.syntax().text().to_string())
  };
}

// ---------------------------------------------------------------------------------------------
// value
// ---------------------------------------------------------------------------------------------

#[test]
fn the_value_wrappers_project_the_kind_they_name() {
  let parse = parse_value("{ a: [1, 2.5], b: set { X }, c: map { 1 => ::ns::Y } }");
  assert!(!parse.has_errors(), "the value fixture must parse clean");
  let root = parse.syntax();

  // `ObjectValue` and `SetValue` are both `{ … }` collections, so the cast has something to get
  // wrong.
  let object: ObjectValue = first(&root);
  assert!(SetValue::cast_node(object.syntax().clone()).is_none());
  assert_eq!(
    texts!(object.object_fields()),
    vec![
      "a: [1, 2.5]".to_string(),
      "b: set { X }".to_string(),
      "c: map { 1 => ::ns::Y }".to_string(),
    ]
  );

  // A list holding one integer and one float: a getter that projected the wrong kind would answer
  // the other member.
  let list: ListValue = first(&root);
  assert_eq!(texts!(list.int_values()), vec!["1".to_string()]);
  assert_eq!(texts!(list.float_values()), vec!["2.5".to_string()]);
  assert!(texts!(list.string_values()).is_empty());

  let set: SetValue = first(&root);
  assert_eq!(texts!(set.enum_values()), vec!["X ".to_string()]);

  // Divergence 9: an enum value wraps a whole `Path`, so this getter reaches a node and not a
  // token, and the qualified spelling is what proves the path level is real.
  let enum_value: EnumValue = root
    .descendants()
    .filter_map(EnumValue::cast_node)
    .find(|e| e.syntax().text().to_string().starts_with("::ns::Y"))
    .expect("the fixture holds a fully qualified enum value");
  // The trailing space is inside the path: its terminating `::` peek crosses whatever follows the
  // last segment, and a node's extent runs to wherever the parse had reached when it closed.
  assert_eq!(node_text!(enum_value.path()), Some("::ns::Y ".to_string()));

  // `MapEntry` holds two values with nothing between them but the `=>`, so both halves come back
  // through the plural getters — the shape the module docs record as GraphQLx's own.
  let entry: MapEntry = first(&root);
  assert_eq!(texts!(entry.int_values()), vec!["1".to_string()]);
  assert_eq!(texts!(entry.enum_values()), vec!["::ns::Y ".to_string()]);

  // The object field's key is a plain `Name` and its value is a node, so `tok` answers the key.
  let field = object
    .object_fields()
    .next()
    .expect("the object has a first field");
  assert_eq!(tok(field.name()), Some("a".to_string()));
  assert_eq!(node_text!(field.list_value()), Some("[1, 2.5]".to_string()));
  assert!(field.object_value().is_none());
}

// ---------------------------------------------------------------------------------------------
// ty
// ---------------------------------------------------------------------------------------------

#[test]
fn the_type_wrappers_keep_the_bang_on_the_type_it_modifies() {
  let parse = parse_type_ref("[::ns::Box<Int!>]!");
  assert!(!parse.has_errors(), "the list fixture must parse clean");
  let root = parse.syntax();

  let list: ListType = first(&root);
  assert!(SetType::cast_node(list.syntax().clone()).is_none());
  // The `!` is a token child of the type it modifies, and both levels carry one here — so a
  // getter reaching descendants rather than direct children could not tell them apart.
  assert_eq!(tok(list.bang_token()), Some("!".to_string()));
  assert_eq!(
    node_text!(list.definition_type_path()),
    Some("::ns::Box<Int!>".to_string())
  );
  assert!(list.list_type().is_none());

  // A path's leading `::` is a token of the path, and the only way to read it off the token
  // getters is that a fully qualified path has as many separators as it has names.
  let path: Path = first(&root);
  assert_eq!(tok_texts!(path.names()), vec!["ns", "Box"]);
  assert_eq!(tok_texts!(path.separators()), vec!["::", "::"]);

  // `SetType` and `MapType` are the same bytes up to the `=>`, which is the whole reason the kind
  // is decided by a retro-wrap.
  let map_parse = parse_type_ref("<Int => [String]>!");
  assert!(!map_parse.has_errors(), "the map fixture must parse clean");
  let map_root = map_parse.syntax();
  let map: MapType = first(&map_root);
  assert!(SetType::cast_node(map.syntax().clone()).is_none());
  assert_eq!(tok(map.bang_token()), Some("!".to_string()));
  assert_eq!(
    texts!(map.definition_type_paths()),
    vec!["Int ".to_string()]
  );
  assert_eq!(texts!(map.list_types()), vec!["[String]".to_string()]);
}

// ---------------------------------------------------------------------------------------------
// directive
// ---------------------------------------------------------------------------------------------

#[test]
fn a_directives_name_is_a_type_path_and_its_arguments_are_a_level_down() {
  let parse = parse_selection_set("{ f @ns::cache<Int>(ttl: 5, tag: \"x\") @b }");
  assert!(
    !parse.has_errors(),
    "the directive fixture must parse clean"
  );
  let root = parse.syntax();

  let directives: Directives = first(&root);
  assert_eq!(
    texts!(directives.directives()),
    vec![
      "@ns::cache<Int>(ttl: 5, tag: \"x\")".to_string(),
      "@b ".to_string(),
    ]
  );

  let directive: Directive = first(&root);
  assert!(Directives::cast_node(directive.syntax().clone()).is_none());
  // Divergence 10: the name after the `@` is a whole `TypePath`, generics and all.
  assert_eq!(
    node_text!(directive.type_path()),
    Some("ns::cache<Int>".to_string())
  );

  let arguments: Arguments = first(&root);
  assert_eq!(
    texts!(arguments.arguments()),
    vec!["ttl: 5".to_string(), "tag: \"x\"".to_string()]
  );

  // Two arguments of two different value kinds, so a getter projecting the wrong one answers the
  // other argument's value rather than nothing.
  let argument: Argument = first(&root);
  assert_eq!(tok(argument.name()), Some("ttl".to_string()));
  assert_eq!(node_text!(argument.int_value()), Some("5".to_string()));
  assert!(argument.string_value().is_none());
}

// ---------------------------------------------------------------------------------------------
// selection
// ---------------------------------------------------------------------------------------------

#[test]
fn a_fields_own_name_is_not_its_alias() {
  let parse = parse_selection_set("{ alias: field(a: 1) @d { x } ... on T @e { y } ...F }");
  assert!(
    !parse.has_errors(),
    "the selection fixture must parse clean"
  );
  let root = parse.syntax();

  let set: SelectionSet = first(&root);
  assert_eq!(texts!(set.fields()).len(), 1);
  assert_eq!(texts!(set.inline_fragments()).len(), 1);
  assert_eq!(texts!(set.fragment_spreads()).len(), 1);

  // The aliased field puts two `Name` tokens in play and only the second is the field's own; the
  // first belongs to the `Alias` node, so a direct-children scan answers correctly and a
  // descendant scan would not.
  let field: Field = first(&root);
  assert_eq!(node_text!(field.alias()), Some("alias:".to_string()));
  assert_eq!(tok(field.name()), Some("field".to_string()));
  assert_eq!(node_text!(field.arguments()), Some("(a: 1)".to_string()));
  assert_eq!(node_text!(field.directives()), Some("@d ".to_string()));
  assert_eq!(node_text!(field.selection_set()), Some("{ x }".to_string()));

  // GraphQLx has a `TypeCondition` node where GraphQL's kind space has none.
  let inline: InlineFragment = first(&root);
  let condition: TypeCondition = first(&root);
  assert_eq!(
    node_text!(inline.type_condition()),
    Some("on T ".to_string())
  );
  assert_eq!(node_text!(condition.type_path()), Some("T ".to_string()));

  // A spread's target is a `TypePath` too, so `...ns::F` is one spread — and the getter is proved
  // against the inline fragment's own condition path sitting earlier in the same tree.
  let spread: FragmentSpread = first(&root);
  assert_eq!(node_text!(spread.type_path()), Some("F ".to_string()));
  assert!(spread.directives().is_none());
}

// ---------------------------------------------------------------------------------------------
// executable
// ---------------------------------------------------------------------------------------------

#[test]
fn an_operations_keyword_is_a_token_and_its_name_is_a_node() {
  let parse = parse_document("query Q<T = Int>($v: [T!] = [1] @d) @e { f }");
  assert!(
    !parse.has_errors(),
    "the operation fixture must parse clean"
  );
  let root = parse.syntax();

  let operation: OperationDefinition = first(&root);
  // GraphQLx has no `OperationType` node: the keyword is the operation's only direct `Name` token,
  // its name living inside a `DefinitionName`.
  assert_eq!(tok(operation.operation_type()), Some("query".to_string()));
  assert_eq!(
    node_text!(operation.definition_name()),
    Some("Q<T = Int>".to_string())
  );
  assert!(operation.description().is_none());

  let variables: VariablesDefinition = first(&root);
  assert_eq!(texts!(variables.variable_definitions()).len(), 1);

  let variable: VariableDefinition = first(&root);
  assert_eq!(
    node_text!(variable.variable_value()),
    Some("$v".to_string())
  );
  assert_eq!(node_text!(variable.list_type()), Some("[T!] ".to_string()));
  assert!(variable.definition_type_path().is_none());
  assert_eq!(
    node_text!(variable.default_value()),
    Some("= [1]".to_string())
  );
  assert_eq!(node_text!(variable.directives()), Some("@d".to_string()));
}

#[test]
fn a_fragments_two_generic_lists_are_a_sibling_and_a_child() {
  let parse = parse_document("\"doc\" fragment <T> F<U> on X @d where A: B { f }");
  assert!(!parse.has_errors(), "the fragment fixture must parse clean");
  let root = parse.syntax();

  let fragment: FragmentDefinition = first(&root);
  // Divergence 12: the description is a bare string token, not a node — the kind space has no
  // `Description` kind, so a getter reaching for one would find nothing forever.
  assert_eq!(tok(fragment.description()), Some("\"doc\"".to_string()));
  // Divergence 13: the implementation list is a **sibling** of the name node and the name's own
  // list is a **child** of it. Both are the same node kind, so only the level tells them apart.
  assert_eq!(
    node_text!(fragment.executable_definition_type_generics()),
    Some("<T>".to_string())
  );
  let name: ExecutableDefinitionName = first(&root);
  assert_eq!(
    node_text!(fragment.executable_definition_name()),
    Some("F<U>".to_string())
  );
  assert_eq!(
    node_text!(name.executable_definition_type_generics()),
    Some("<U>".to_string())
  );
  assert_eq!(tok(name.name()), Some("F".to_string()));
  assert_eq!(
    node_text!(fragment.type_condition()),
    Some("on X ".to_string())
  );
  assert_eq!(node_text!(fragment.directives()), Some("@d ".to_string()));
  assert_eq!(
    node_text!(fragment.where_clause()),
    Some("where A: B ".to_string())
  );
  assert_eq!(
    node_text!(fragment.selection_set()),
    Some("{ f }".to_string())
  );
}

// ---------------------------------------------------------------------------------------------
// definition
// ---------------------------------------------------------------------------------------------

#[test]
fn an_sdl_definitions_name_is_a_node_and_never_an_index() {
  let parse = parse_document(
    "\"doc\" type T<A = Int> implements I & J @d where A: B { f(x: Int = 1): [T!]! @e }",
  );
  assert!(!parse.has_errors(), "the object fixture must parse clean");
  let root = parse.syntax();

  let object: ObjectTypeDefinition = first(&root);
  // `interface T { … }` is the same shape after its keyword, so the cast has something to get
  // wrong.
  assert!(InterfaceTypeDefinition::cast_node(object.syntax().clone()).is_none());
  assert_eq!(tok(object.description()), Some("\"doc\"".to_string()));
  assert_eq!(
    node_text!(object.definition_name()),
    Some("T<A = Int>".to_string())
  );
  assert_eq!(
    node_text!(object.implement_interfaces()),
    Some("implements I & J ".to_string())
  );
  assert_eq!(node_text!(object.directives()), Some("@d ".to_string()));
  assert_eq!(
    node_text!(object.where_clause()),
    Some("where A: B ".to_string())
  );

  let interfaces: ImplementInterfaces = first(&root);
  assert_eq!(texts!(interfaces.type_paths()), vec!["I ", "J "]);

  let clause: WhereClause = first(&root);
  assert_eq!(texts!(clause.where_predicates()).len(), 1);
  let predicate: WherePredicate = first(&root);
  assert_eq!(texts!(predicate.type_paths()), vec!["A", "B "]);

  let fields: FieldsDefinition = first(&root);
  assert_eq!(texts!(fields.field_definitions()).len(), 1);

  let field: FieldDefinition = first(&root);
  assert_eq!(tok(field.name()), Some("f".to_string()));
  assert_eq!(
    node_text!(field.arguments_definition()),
    Some("(x: Int = 1)".to_string())
  );
  assert_eq!(node_text!(field.list_type()), Some("[T!]!".to_string()));
  assert!(field.definition_type_path().is_none());
  assert_eq!(node_text!(field.directives()), Some("@e ".to_string()));

  let input: InputValueDefinition = first(&root);
  assert_eq!(tok(input.name()), Some("x".to_string()));
  assert_eq!(
    node_text!(input.definition_type_path()),
    Some("Int ".to_string())
  );
  assert_eq!(node_text!(input.default_value()), Some("= 1".to_string()));
}

// ---------------------------------------------------------------------------------------------
// extension
// ---------------------------------------------------------------------------------------------

#[test]
fn an_extensions_target_is_a_path_where_a_definitions_name_is_not() {
  let parse = parse_document("extend type ns::T<A> implements I @d { f: Int }");
  assert!(
    !parse.has_errors(),
    "the extension fixture must parse clean"
  );
  let root = parse.syntax();

  let extension: ObjectTypeExtension = first(&root);
  // An extension and its definition differ only by an `extend`, which is inside the extension's
  // own node — so this is the sharpest near neighbour in the whole kind space.
  assert!(ObjectTypeDefinition::cast_node(extension.syntax().clone()).is_none());
  assert_eq!(
    node_text!(extension.extension_name()),
    Some("ns::T<A>".to_string())
  );
  assert_eq!(
    node_text!(extension.implement_interfaces()),
    Some("implements I ".to_string())
  );
  assert_eq!(node_text!(extension.directives()), Some("@d ".to_string()));
  assert_eq!(
    node_text!(extension.fields_definition()),
    Some("{ f: Int }".to_string())
  );
  assert!(extension.where_clause().is_none());

  let name: ExtensionName = first(&root);
  assert_eq!(node_text!(name.path()), Some("ns::T".to_string()));
  assert_eq!(
    node_text!(name.extension_type_generics()),
    Some("<A>".to_string())
  );

  // The one extension whose directives the grammar makes mandatory, and which therefore has no
  // other tail to confuse the getter with.
  let scalar_parse = parse_document("extend scalar S @d");
  assert!(
    !scalar_parse.has_errors(),
    "the scalar fixture must parse clean"
  );
  let scalar: ScalarTypeExtension = first(&scalar_parse.syntax());
  assert_eq!(node_text!(scalar.extension_name()), Some("S ".to_string()));
  assert_eq!(node_text!(scalar.directives()), Some("@d".to_string()));
}

// ---------------------------------------------------------------------------------------------
// generic
// ---------------------------------------------------------------------------------------------

#[test]
fn the_three_generic_lists_are_told_apart_by_their_members() {
  // Only a definition's list has member nodes, and only its members can carry a default.
  let params_parse = parse_definition_type_generics("<A, B = [Int!]>");
  assert!(
    !params_parse.has_errors(),
    "the parameter fixture must parse clean"
  );
  let params_root = params_parse.syntax();
  let params: DefinitionTypeGenerics = first(&params_root);
  assert_eq!(texts!(params.definition_type_params()).len(), 2);
  let defaulted = params
    .definition_type_params()
    .nth(1)
    .expect("the list has a second parameter");
  assert_eq!(tok(defaulted.name()), Some("B".to_string()));
  assert_eq!(
    node_text!(defaulted.list_type()),
    Some("[Int!]".to_string())
  );
  assert!(defaulted.definition_type_path().is_none());
  let first_param: DefinitionTypeParam = first(&params_root);
  assert_eq!(tok(first_param.name()), Some("A".to_string()));
  assert!(first_param.list_type().is_none());

  // The other two lists hold bare `Name` tokens, several of one kind, which `cast::token` cannot
  // express.
  let exec_parse = parse_executable_definition_type_generics("<A, B>");
  assert!(
    !exec_parse.has_errors(),
    "the executable list must parse clean"
  );
  let exec_root = exec_parse.syntax();
  let exec: ExecutableDefinitionTypeGenerics = first(&exec_root);
  assert_eq!(tok_texts!(exec.names()), vec!["A", "B"]);

  // An extension's target is a path and a definition's name is not.
  let ext_parse = parse_extension_name("ns::T<A>");
  assert!(
    !ext_parse.has_errors(),
    "the extension name must parse clean"
  );
  let ext_root = ext_parse.syntax();
  let ext: ExtensionName = first(&ext_root);
  assert!(DefinitionName::cast_node(ext.syntax().clone()).is_none());
  assert_eq!(node_text!(ext.path()), Some("ns::T".to_string()));

  // A predicate's constrained type and its bounds are all `TypePath`s under one node.
  let where_parse = parse_where_clause("where A: B & C<X>, ::d::E: F");
  assert!(
    !where_parse.has_errors(),
    "the where fixture must parse clean"
  );
  let where_root = where_parse.syntax();
  let clause: WhereClause = first(&where_root);
  assert_eq!(texts!(clause.where_predicates()).len(), 2);
  let predicate: WherePredicate = first(&where_root);
  assert_eq!(texts!(predicate.type_paths()), vec!["A", "B ", "C<X>"]);
}

// ---------------------------------------------------------------------------------------------
// import
// ---------------------------------------------------------------------------------------------

#[test]
fn an_import_clause_is_a_list_or_a_wildcard_and_never_both() {
  let parse = parse_document("import { A as ns::B, * as w } from \"m\"");
  assert!(!parse.has_errors(), "the import fixture must parse clean");
  let root = parse.syntax();

  let import: ImportDefinition = first(&root);
  assert!(import.wildcard_specifier().is_none());
  assert_eq!(
    node_text!(import.import_list()),
    Some("{ A as ns::B, * as w }".to_string())
  );
  assert_eq!(node_text!(import.source()), Some("\"m\"".to_string()));
  let source: StringValue = first(&root);
  assert_eq!(tok(source.string_token()), Some("\"m\"".to_string()));

  let list: ImportList = first(&root);
  assert_eq!(texts!(list.named_specifiers()).len(), 1);
  assert_eq!(texts!(list.wildcard_specifiers()).len(), 1);

  // The specifier's own name sits in front of the `as`, which lexes as another `Name` token under
  // the same node — so the `tok` getter has something to get wrong.
  let named: NamedSpecifier = first(&root);
  assert_eq!(tok(named.name()), Some("A".to_string()));
  assert_eq!(node_text!(named.alias()), Some("ns::B, ".to_string()));

  let wildcard: WildcardSpecifier = first(&root);
  assert_eq!(node_text!(wildcard.alias()), Some("w ".to_string()));

  // The other clause form, where the wildcard is the whole clause.
  let bare = parse_document("import * as w from \"m\"");
  assert!(!bare.has_errors(), "the wildcard clause must parse clean");
  let bare_import: ImportDefinition = first(&bare.syntax());
  assert!(bare_import.import_list().is_none());
  assert_eq!(
    node_text!(bare_import.wildcard_specifier()),
    Some("* as w ".to_string())
  );
}

// ---------------------------------------------------------------------------------------------
// document
// ---------------------------------------------------------------------------------------------

#[test]
fn a_document_sorts_its_entries_by_kind() {
  let parse = parse_document(
    "import { A } from \"m\"\n\
     query Q { f }\n\
     fragment F on T { g }\n\
     type T { f: Int }\n\
     extend type T @d\n",
  );
  assert!(!parse.has_errors(), "the document fixture must parse clean");
  let root = parse.syntax();

  let document: Document = first(&root);
  // The two roots differ only in which entries they admit, and the mixed one is a superset — so a
  // wrapper pointed at the other kind would find nothing.
  assert!(TypeSystemDocument::cast_node(document.syntax().clone()).is_none());
  assert_eq!(texts!(document.import_definitions()).len(), 1);
  assert_eq!(texts!(document.operation_definitions()).len(), 1);
  assert_eq!(texts!(document.fragment_definitions()).len(), 1);
  assert_eq!(texts!(document.object_type_definitions()).len(), 1);
  assert_eq!(texts!(document.object_type_extensions()).len(), 1);
  // Every other bucket is empty, which is what says the sort is by kind and not by position.
  assert!(texts!(document.scalar_type_definitions()).is_empty());
  assert!(texts!(document.interface_type_definitions()).is_empty());
  assert!(texts!(document.schema_definitions()).is_empty());
  assert!(texts!(document.schema_extensions()).is_empty());
}

// ===========================================================================================
// Task 20 — the totality half
// ===========================================================================================

/// The two ends of a `rowan::TextRange`.
type Span = (u32, u32);

/// What a getter projected, reduced to something a **kind-blind rival** can be compared against.
///
/// The rival is the whole point. "This getter answered `Some`" is nearly free — over `{ a }` a
/// `cast::child` is indistinguishable from "take the first child", which is the fixture weakness
/// this file's own module docs open on. What is not free is answering *differently* from a getter
/// that ignored kinds altogether, and that is a property of the material this gate sweeps rather
/// than of the getter, so it is measured rather than claimed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Projection {
  /// A singular getter's answer: the span of what it found, if it found anything.
  One(Option<Span>),
  /// A plural getter's answer: how many it yielded, and where the first one starts.
  Many(usize, Option<Span>),
}

impl Projection {
  /// Whether the getter projected anything at all.
  fn answered(self) -> bool {
    match self {
      Self::One(found) => found.is_some(),
      Self::Many(count, _) => count > 0,
    }
  }
}

/// Something a getter can project — one of the 78 wrappers, or a token.
///
/// The trait exists for its two associated functions: an element type knows whether its getters
/// look at a node's *children* or at its *tokens*, and therefore what the kind-blind rival on that
/// side is.
trait Element: Sized {
  /// This element's own span.
  fn span(&self) -> Span;
  /// "Take the first one, whatever kind it is."
  fn rival_one(parent: &SyntaxNode) -> Projection;
  /// "Take all of them, whatever kind they are."
  fn rival_many(parent: &SyntaxNode) -> Projection;
}

fn span_of(range: rowan::TextRange) -> Span {
  (range.start().into(), range.end().into())
}

impl Element for SyntaxToken {
  fn span(&self) -> Span {
    span_of(self.text_range())
  }

  fn rival_one(parent: &SyntaxNode) -> Projection {
    Projection::One(
      parent
        .children_with_tokens()
        .find_map(|child| child.into_token())
        .map(|t| span_of(t.text_range())),
    )
  }

  fn rival_many(parent: &SyntaxNode) -> Projection {
    let spans: Vec<Span> = parent
      .children_with_tokens()
      .filter_map(|child| child.into_token())
      .map(|t| span_of(t.text_range()))
      .collect();
    Projection::Many(spans.len(), spans.first().copied())
  }
}

/// The node-side rival for [`Element::rival_one`], shared by all 78 wrappers.
fn first_child_node(parent: &SyntaxNode) -> Projection {
  Projection::One(parent.children().next().map(|n| span_of(n.text_range())))
}

/// The node-side rival for [`Element::rival_many`], shared by all 78 wrappers.
fn all_child_nodes(parent: &SyntaxNode) -> Projection {
  let spans: Vec<Span> = parent.children().map(|n| span_of(n.text_range())).collect();
  Projection::Many(spans.len(), spans.first().copied())
}

/// A getter's return type, reduced to a [`Projection`] alongside the rival it must beat.
trait Projected {
  /// What this answer projected.
  fn projection(self) -> Projection;
  /// What a kind-blind getter of the same arity would have answered on the same node.
  fn rival(parent: &SyntaxNode) -> Projection;
}

impl<T: Element> Projected for Option<T> {
  fn projection(self) -> Projection {
    Projection::One(self.map(|found| found.span()))
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    T::rival_one(parent)
  }
}

impl<N: Element + CastNode<GraphQLxLang>> Projected for AstChildren<N> {
  fn projection(self) -> Projection {
    let spans: Vec<Span> = self.map(|found| found.span()).collect();
    Projection::Many(spans.len(), spans.first().copied())
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    N::rival_many(parent)
  }
}

impl Projected for AstTokens {
  fn projection(self) -> Projection {
    let spans: Vec<Span> = self.map(|found| found.span()).collect();
    Projection::Many(spans.len(), spans.first().copied())
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    SyntaxToken::rival_many(parent)
  }
}

/// Declare the wrapper/getter table this gate walks, and generate the four things read off it.
///
/// Entries mirror the `ast_node!` invocations exactly — the same wrapper name, the same getter
/// names, the same forms, in the same order — minus the target kinds, which
/// [`the_probe_table_is_exactly_the_shipped_inventory`] reads out of the sources instead.
///
/// Each getter is named **once**, and the name is both the method called and the label recorded,
/// through `stringify!`. That is not a stylistic choice: a table that spelled the label separately
/// could record `Field::name` against a call to `Field::alias()` and report full coverage while
/// leaving a getter untouched.
macro_rules! wrapper_table {
  ($( $ty:ident { $( $form:ident $getter:ident ),* $(,)? } )*) => {
    /// Every `(wrapper, getter, form)` triple the table declares.
    const DECLARED: &[(&str, &str, &str)] = &[
      $( $( (stringify!($ty), stringify!($getter), stringify!($form)), )* )*
    ];

    /// Every wrapper the table declares, in declaration order.
    const TABLE_WRAPPERS: &[&str] = &[ $( stringify!($ty), )* ];

    $(
      impl Element for $ty {
        fn span(&self) -> Span {
          span_of(self.syntax().text_range())
        }

        fn rival_one(parent: &SyntaxNode) -> Projection {
          first_child_node(parent)
        }

        fn rival_many(parent: &SyntaxNode) -> Projection {
          all_child_nodes(parent)
        }
      }
    )*

    /// Cast `node` to every wrapper that accepts it, and call every one of that wrapper's getters.
    ///
    /// Deliberately not `else if`: a node accepted by two wrappers is a defect
    /// ([`each_node_kind_casts_to_exactly_the_wrapper_that_names_it`]), and a chain that stopped at
    /// the first match would hide it here.
    fn probe(node: &SyntaxNode, reg: &mut Registry) {
      $(
        if let Some(typed) = $ty::cast_node(node.clone()) {
          reg.cast(stringify!($ty));
          $(
            reg.record(stringify!($ty), stringify!($getter), node, typed.$getter());
          )*
        }
      )*
    }

    /// Every wrapper whose cast accepts `node` — one row of the 78x78 cast matrix.
    fn accepting_wrappers(node: &SyntaxNode) -> Vec<&'static str> {
      let mut out = Vec::new();
      $( if $ty::cast_node(node.clone()).is_some() { out.push(stringify!($ty)); } )*
      out
    }
  };
}

wrapper_table! {
  VariableValue { tok name }
  IntValue { tok int_token }
  FloatValue { tok float_token }
  StringValue { tok_any string_token }
  BooleanValue { tok name }
  NullValue { tok name }
  EnumValue { opt path }
  ListValue {
    many variable_values, many int_values, many float_values, many string_values,
    many boolean_values, many null_values, many enum_values, many list_values, many set_values,
    many map_values, many object_values,
  }
  SetValue {
    many variable_values, many int_values, many float_values, many string_values,
    many boolean_values, many null_values, many enum_values, many list_values, many set_values,
    many map_values, many object_values,
  }
  MapValue { many map_entries }
  MapEntry {
    many variable_values, many int_values, many float_values, many string_values,
    many boolean_values, many null_values, many enum_values, many list_values, many set_values,
    many map_values, many object_values,
  }
  ObjectValue { many object_fields }
  ObjectField {
    tok name, opt variable_value, opt int_value, opt float_value, opt string_value,
    opt boolean_value, opt null_value, opt enum_value, opt list_value, opt set_value,
    opt map_value, opt object_value,
  }
  DefaultValue {
    opt variable_value, opt int_value, opt float_value, opt string_value, opt boolean_value,
    opt null_value, opt enum_value, opt list_value, opt set_value, opt map_value,
    opt object_value,
  }
  Path { toks names, toks separators }
  TypeGenerics { many definition_type_paths, many list_types, many set_types, many map_types }
  DefinitionTypePath { opt path, opt type_generics, tok bang_token }
  ListType { opt definition_type_path, opt list_type, opt set_type, opt map_type, tok bang_token }
  SetType { opt definition_type_path, opt list_type, opt set_type, opt map_type, tok bang_token }
  MapType {
    many definition_type_paths, many list_types, many set_types, many map_types, tok bang_token,
  }
  TypePath { opt path, opt type_generics }
  Argument {
    tok name, opt variable_value, opt int_value, opt float_value, opt string_value,
    opt boolean_value, opt null_value, opt enum_value, opt list_value, opt set_value,
    opt map_value, opt object_value,
  }
  Arguments { many arguments }
  Directive { opt type_path, opt arguments }
  Directives { many directives }
  Alias { tok name }
  Field { opt alias, tok name, opt arguments, opt directives, opt selection_set }
  TypeCondition { opt type_path }
  FragmentSpread { opt type_path, opt directives }
  InlineFragment { opt type_condition, opt directives, opt selection_set }
  SelectionSet { many fields, many fragment_spreads, many inline_fragments }
  VariableDefinition {
    tok_any description, opt variable_value, opt definition_type_path, opt list_type,
    opt set_type, opt map_type, opt default_value, opt directives,
  }
  VariablesDefinition { many variable_definitions }
  OperationDefinition {
    tok_any description, tok operation_type, opt definition_name, opt variables_definition,
    opt directives, opt where_clause, opt selection_set,
  }
  FragmentDefinition {
    tok_any description, opt executable_definition_type_generics, opt executable_definition_name,
    opt type_condition, opt directives, opt where_clause, opt selection_set,
  }
  ExecutableDocument {
    many import_definitions, many operation_definitions, many fragment_definitions,
  }
  InputValueDefinition {
    tok_any description, tok name, opt definition_type_path, opt list_type, opt set_type,
    opt map_type, opt default_value, opt directives,
  }
  ArgumentsDefinition { many input_value_definitions }
  FieldDefinition {
    tok_any description, tok name, opt arguments_definition, opt definition_type_path,
    opt list_type, opt set_type, opt map_type, opt directives,
  }
  FieldsDefinition { many field_definitions }
  InputFieldsDefinition { many input_value_definitions }
  ImplementInterfaces { many type_paths }
  UnionMemberTypes { many type_paths }
  DirectiveLocations { toks locations }
  EnumValueDefinition { tok_any description, tok name, opt directives }
  EnumValuesDefinition { many enum_value_definitions }
  RootOperationTypeDefinition { tok operation_type, opt type_path }
  RootOperationTypesDefinition { many root_operation_type_definitions }
  ScalarTypeDefinition { tok_any description, opt definition_name, opt directives }
  ObjectTypeDefinition {
    tok_any description, opt definition_name, opt implement_interfaces, opt directives,
    opt where_clause, opt fields_definition,
  }
  InterfaceTypeDefinition {
    tok_any description, opt definition_name, opt implement_interfaces, opt directives,
    opt where_clause, opt fields_definition,
  }
  UnionTypeDefinition {
    tok_any description, opt definition_name, opt directives, opt union_member_types,
    opt where_clause,
  }
  EnumTypeDefinition {
    tok_any description, opt definition_name, opt directives, opt enum_values_definition,
  }
  InputObjectTypeDefinition {
    tok_any description, opt definition_name, opt directives, opt where_clause,
    opt input_fields_definition,
  }
  DirectiveDefinition {
    tok_any description, opt definition_name, opt arguments_definition, opt directive_locations,
    opt where_clause,
  }
  SchemaDefinition { tok_any description, opt directives, opt root_operation_types_definition }
  ScalarTypeExtension { opt extension_name, opt directives }
  ObjectTypeExtension {
    opt extension_name, opt implement_interfaces, opt directives, opt where_clause,
    opt fields_definition,
  }
  InterfaceTypeExtension {
    opt extension_name, opt implement_interfaces, opt directives, opt where_clause,
    opt fields_definition,
  }
  UnionTypeExtension {
    opt extension_name, opt directives, opt union_member_types, opt where_clause,
  }
  EnumTypeExtension { opt extension_name, opt directives, opt enum_values_definition }
  InputObjectTypeExtension {
    opt extension_name, opt directives, opt where_clause, opt input_fields_definition,
  }
  SchemaExtension { opt directives, opt root_operation_types_definition }
  DefinitionTypeParam {
    tok name, opt definition_type_path, opt list_type, opt set_type, opt map_type,
  }
  DefinitionTypeGenerics { many definition_type_params }
  ExtensionTypeGenerics { toks names }
  ExecutableDefinitionTypeGenerics { toks names }
  DefinitionName { tok name, opt definition_type_generics }
  ExtensionName { opt path, opt extension_type_generics }
  ExecutableDefinitionName { tok name, opt executable_definition_type_generics }
  WherePredicate { many type_paths }
  WhereClause { many where_predicates }
  NamedSpecifier { tok name, opt alias }
  WildcardSpecifier { opt alias }
  ImportList { many named_specifiers, many wildcard_specifiers }
  ImportDefinition { opt import_list, opt wildcard_specifier, opt source }
  Document {
    many import_definitions, many operation_definitions, many fragment_definitions,
    many scalar_type_definitions, many object_type_definitions, many interface_type_definitions,
    many union_type_definitions, many enum_type_definitions, many input_object_type_definitions,
    many directive_definitions, many schema_definitions, many scalar_type_extensions,
    many object_type_extensions, many interface_type_extensions, many union_type_extensions,
    many enum_type_extensions, many input_object_type_extensions, many schema_extensions,
  }
  TypeSystemDocument {
    many import_definitions, many scalar_type_definitions, many object_type_definitions,
    many interface_type_definitions, many union_type_definitions, many enum_type_definitions,
    many input_object_type_definitions, many directive_definitions, many schema_definitions,
    many scalar_type_extensions, many object_type_extensions, many interface_type_extensions,
    many union_type_extensions, many enum_type_extensions, many input_object_type_extensions,
    many schema_extensions,
  }
}

/// How often one getter was called, and how often it projected something.
#[derive(Debug, Default, Clone, Copy)]
struct Answers {
  /// How many nodes of the getter's own kind it ran on.
  calls: usize,
  /// On how many of those it returned `Some`, or a non-empty iterator.
  hits: usize,
}

/// What one sweep observed.
#[derive(Default)]
struct Registry {
  /// Wrapper name -> how many nodes it cast.
  casts: BTreeMap<&'static str, usize>,
  /// `(wrapper, getter)` -> its call and hit counts.
  getters: BTreeMap<(&'static str, &'static str), Answers>,
  /// The getters seen, at least once, to answer differently from their kind-blind rival.
  discriminated: BTreeSet<(&'static str, &'static str)>,
  /// One real node per node kind seen, for the cast matrix.
  representatives: BTreeMap<K, SyntaxNode>,
  /// Every kind seen as a **token**, which is how [`K::Gap`] occurs and the only way it does.
  token_kinds: BTreeSet<K>,
}

impl Registry {
  fn cast(&mut self, wrapper: &'static str) {
    *self.casts.entry(wrapper).or_default() += 1;
  }

  fn record<P: Projected>(
    &mut self,
    wrapper: &'static str,
    getter: &'static str,
    parent: &SyntaxNode,
    answer: P,
  ) {
    let projection = answer.projection();
    let entry = self.getters.entry((wrapper, getter)).or_default();
    entry.calls += 1;
    entry.hits += usize::from(projection.answered());
    if projection != P::rival(parent) {
      self.discriminated.insert((wrapper, getter));
    }
  }

  /// Probe every node in one tree, root included.
  fn walk(&mut self, root: &SyntaxNode) {
    for element in root.descendants_with_tokens() {
      match element {
        rowan::NodeOrToken::Node(node) => {
          self
            .representatives
            .entry(node.kind())
            .or_insert_with(|| node.clone());
          probe(&node, self);
        }
        rowan::NodeOrToken::Token(token) => {
          self.token_kinds.insert(token.kind());
        }
      }
    }
  }

  /// Parse one source through all three roots and probe every tree.
  ///
  /// All three, because `parse_document` reaches 76 of the 78 wrappers structurally:
  /// [`ExecutableDocument`] and [`TypeSystemDocument`] are *roots*, and a parse has one. Driving
  /// the other two over the same bytes is what gate 3 does for the same reason.
  fn sweep_source(&mut self, src: &str) {
    self.walk(&parse_document(src).syntax());
    self.walk(&parse_executable_document(src).syntax());
    self.walk(&parse_type_system_document(src).syntax());
  }

  /// The wrappers this sweep never cast.
  fn uncast(&self) -> BTreeSet<&'static str> {
    TABLE_WRAPPERS
      .iter()
      .copied()
      .filter(|w| !self.casts.contains_key(w))
      .collect()
  }

  /// The getters this sweep never called at all.
  fn uncalled(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| !self.getters.contains_key(key))
      .collect()
  }

  /// The getters this sweep called but which never projected anything.
  fn unanswered(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| self.getters.get(key).is_none_or(|a| a.hits == 0))
      .collect()
  }

  /// The getters this sweep never saw answer differently from a kind-blind rival.
  fn undiscriminated(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| !self.discriminated.contains(key))
      .collect()
  }
}

/// Every `.graphqlx` file in the shared GraphQLx corpus, in a deterministic order.
///
/// Read unchanged, exactly as gates 1, 2, 3 and 5 read it — this gate adds no corpus entry, so a
/// green here is a statement about the same material the other four hold.
fn corpus() -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the GraphQLx corpus at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
    .collect();
  files.sort();
  files
    .into_iter()
    .map(|p| {
      let name = p.file_name().unwrap().to_string_lossy().to_string();
      let src = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", p.display()));
      (name, src)
    })
    .collect()
}

/// Sources this gate adds on top of the corpus, and what each one is for.
///
/// The corpus reaches **every one of the 78 wrappers** on its own and calls all 297 getters, but 63
/// of those getters had nothing to project from it: a getter is only exercised by a document that
/// writes the shape it reaches for, and no corpus entry writes a float inside a set, a map on both
/// halves of a map entry, a description on a scalar, or a `where` on an extension. These four are
/// that complement and nothing else — they were written against the measured list in
/// [`CORPUS_CANNOT_PROJECT`], which is what keeps them honest, and
/// [`every_added_source_earns_its_place`] leaves each of them out in turn to check it is still the
/// only thing in the sweep carrying what it carries.
///
/// They are fixtures rather than corpus entries on purpose: the corpus is shared with gates 1, 2, 3
/// and 5, and a file added here would move gate 5's golden set for a reason that has nothing to do
/// with tree shape.
const FIXTURES: &[(&str, &str)] = &[
  // Every value kind in every value position the grammar has: a variable's default, a list's
  // members, a set's members, both halves of a map entry, and an object's fields. The corpus writes
  // integers, strings and objects freely and almost nothing else, so 35 of the 63 gaps below are
  // here.
  (
    "every_value_kind_in_every_value_position",
    "query Q($a: Int = 1.5, $b: Int = \"s\", $c: Int = null, $d: Int = EV, \
     $e: Int = set { 1 }, $f: Int = map { 1 => 2 }, $g: Int = {k: 1}) { \
     h(list: [$v, true, 1.5, null, EV, set { 1 }, map { 1 => 2 }, {k: 1}], \
     st: set { \"s\", true, 1.5, null, [1], set { 2 }, map { 1 => 2 }, {k: 1} }, \
     mp: map { true => false, 1.5 => 2.5, null => null, map { 1 => 2 } => map { 3 => 4 } }, \
     obj: {v: $x, b: true, f: 1.5, n: null, e: EV, l: [1], s: set { 1 }, m: map { 1 => 2 }}) }",
  ),
  // A description on every definition that takes one, and a directive on the two definition kinds
  // no corpus entry decorates. This dialect has no `Description` node, so each of these is a bare
  // string token read off the definition itself.
  (
    "a_description_on_every_definition_that_takes_one",
    "\"scalar\" scalar S @d \
     \"interface\" interface I implements J @d { a: Int } \
     \"union\" union U @d = A \
     \"enum\" enum E @d { X } \
     \"input\" input In @d { a: Int } \
     \"directive\" directive @dd(x: Int) on FIELD \
     \"schema\" schema @d { query: Q } \
     \"query\" query Q @dq { f }",
  ),
  // The three extension tails the corpus never writes: a `where` on an interface, a union and an
  // input object, plus the interface extension's `implements`.
  (
    "extensions_that_carry_a_where_clause",
    "extend interface I<A> implements J where A: Node { a: Int } \
     extend union U<A> = C where A: Node \
     extend input In<A> where A: Node { a: Int }",
  ),
  // A set type and a map type in every position that admits a type reference — a variable
  // definition, a generic parameter's default, an input value definition, a field's own type, and
  // nested inside each of the three composite types. GraphQLx's two extra type constructors are
  // what divergences 1 and 2 add, and the corpus exercises them only at the top level.
  (
    "a_set_and_a_map_type_in_every_type_position",
    "query Q($a: <Int>, $b: <Int => Int>) { f } \
     type T<A = [Int], B = <Int>, C = <Int => Int>> { \
     g(x: [Int], y: <Int>, z: <Int => Int>): P<<Int>> \
     h: [<Int>] \
     i: [<Int => Int>] \
     j: <[Int]> \
     k: <<Int => Int>> \
     l: <Int => <Int>> \
     m: <Int => <Int => Int>> \
     }",
  ),
];

/// Every source this gate adds on top of the corpus, valid and malformed alike.
fn added_sources() -> Vec<(&'static str, &'static str)> {
  FIXTURES.iter().chain(RECOVERY_FIXTURES).copied().collect()
}

/// A sweep over the corpus plus `extras`.
fn sweep_over(extras: &[(&str, &str)]) -> Registry {
  let mut reg = Registry::default();
  for (_, src) in corpus() {
    reg.sweep_source(&src);
  }
  for (_, src) in extras {
    reg.sweep_source(src);
  }
  reg
}

/// A sweep over the corpus alone.
fn sweep_corpus() -> Registry {
  sweep_over(&[])
}

/// A sweep over the corpus and this gate's own fixtures, valid and malformed alike.
fn sweep_everything() -> Registry {
  sweep_over(&added_sources())
}

/// A sweep over everything **except** the source at `omit` — the leave-one-out that makes
/// [`every_added_source_earns_its_place`] a measurement rather than a claim.
fn sweep_without(omit: usize) -> Registry {
  let all = added_sources();
  let kept: Vec<(&str, &str)> = all
    .iter()
    .enumerate()
    .filter(|(i, _)| *i != omit)
    .map(|(_, entry)| *entry)
    .collect();
  sweep_over(&kept)
}

// ---- the shipped inventory, read back out of the wrapper sources ----

/// One getter as the wrapper sources actually spell it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct SourceGetter {
  wrapper: String,
  name: String,
  form: String,
  /// Everything after the form: a wrapper type, a `K::` path, or `K::A | K::B`.
  target: String,
}

/// The ten files the 78 `ast_node!` invocations live in.
///
/// Ten and not the GraphQL twin's seven: this dialect puts the seven type-system extensions in
/// `extension.rs` and the generic machinery in `generic.rs`, and adds `import.rs` for a production
/// GraphQL has no counterpart for at all.
const AST_FILES: &[&str] = &[
  "value.rs",
  "ty.rs",
  "directive.rs",
  "selection.rs",
  "executable.rs",
  "definition.rs",
  "extension.rs",
  "generic.rs",
  "import.rs",
  "document.rs",
];

/// Strip every `//`-introduced comment, line by line.
///
/// Enough for these ten files, which hold `ast_node!` invocations and doc comments and no string
/// literals at all. If one ever gains a literal containing `//` the parse below breaks loudly, in
/// [`the_probe_table_is_exactly_the_shipped_inventory`], rather than quietly.
fn without_comments(src: &str) -> String {
  src
    .lines()
    .map(|line| match line.find("//") {
      Some(at) => &line[..at],
      None => line,
    })
    .collect::<Vec<_>>()
    .join("\n")
}

/// Every getter the crate ships, parsed out of the `ast_node!` invocations themselves.
///
/// The hand-maintained table above is what this gate *walks*; this is what the crate *exposes*, and
/// the two are compared. Parsing source text in a test is unusual and is the point: no declaration
/// a human maintains can be the sole witness for "nothing is missing", because the thing that goes
/// missing is the declaration.
fn source_inventory() -> Vec<SourceGetter> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("src")
    .join("graphqlx")
    .join("lossless")
    .join("ast");
  let mut out = Vec::new();
  for file in AST_FILES {
    let path = dir.join(file);
    let raw = std::fs::read_to_string(&path)
      .unwrap_or_else(|e| panic!("{} is unreadable: {e}", path.display()));
    let src = without_comments(&raw);
    let mut rest = src.as_str();
    while let Some(at) = rest.find("ast_node!(") {
      rest = &rest[at + "ast_node!(".len()..];
      let mut depth = 1usize;
      let mut end = rest.len();
      for (i, ch) in rest.char_indices() {
        match ch {
          '(' => depth += 1,
          ')' => {
            depth -= 1;
            if depth == 0 {
              end = i;
              break;
            }
          }
          _ => {}
        }
      }
      let body = &rest[..end];
      rest = &rest[end..];

      let (header, tail) = body
        .split_once('{')
        .unwrap_or_else(|| panic!("{file}: an ast_node! body opens a brace"));
      let getters = tail
        .rsplit_once('}')
        .unwrap_or_else(|| panic!("{file}: an ast_node! body closes its brace"))
        .0;
      // The header is `lang = <path>; <docs> Name`, and the wrapper is its **last** word. Taking
      // the whole trimmed prefix would answer `lang = crate::graphqlx::kinds::GraphQLxLang; Name`
      // — a wrapper name no census entry matches, which turns this inventory into a
      // uniformly-wrong list rather than an empty one.
      let wrapper = header
        .split_once("=>")
        .unwrap_or_else(|| panic!("{file}: an ast_node! header reads `Name => K::Kind`"))
        .0
        .split_whitespace()
        .next_back()
        .unwrap_or_else(|| panic!("{file}: an ast_node! header names no wrapper"))
        .to_string();

      for entry in getters.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
          continue;
        }
        let (name, spec) = entry
          .split_once(':')
          .unwrap_or_else(|| panic!("{file}: `{entry}` is not `getter: form target`"));
        let mut words = spec.split_whitespace();
        let form = words
          .next()
          .unwrap_or_else(|| panic!("{file}: `{entry}` names no getter form"))
          .to_string();
        out.push(SourceGetter {
          wrapper: wrapper.clone(),
          name: name.trim().to_string(),
          form,
          target: words.collect::<Vec<_>>().join(" "),
        });
      }
    }
  }
  out
}

/// `InputValueDefinition` -> `input_value_definition`.
fn snake(camel: &str) -> String {
  let mut out = String::with_capacity(camel.len() + 4);
  for (i, ch) in camel.char_indices() {
    if ch.is_ascii_uppercase() {
      if i != 0 {
        out.push('_');
      }
      out.push(ch.to_ascii_lowercase());
    } else {
      out.push(ch);
    }
  }
  out
}

/// `map_entry` -> `map_entries`; `list_value` -> `list_values`.
///
/// A plural getter is named for its element in the plural, and this dialect has one element whose
/// plural is not formed by appending an `s`. Spelling the rule rather than pinning `MapValue` as an
/// exception keeps [`every_getter_is_named_for_what_it_projects`] a statement about *names* — a
/// getter called `map_entrys` would be a defect in the crate, not in this census.
fn plural(base: &str) -> String {
  match base.strip_suffix('y') {
    Some(stem) if !stem.ends_with(['a', 'e', 'i', 'o', 'u']) => format!("{stem}ies"),
    _ => format!("{base}s"),
  }
}

/// `InlineString` -> `["Inline", "String"]`.
fn camel_words(camel: &str) -> Vec<&str> {
  let mut out = Vec::new();
  let mut start = 0usize;
  for (i, ch) in camel.char_indices() {
    if i != 0 && ch.is_ascii_uppercase() {
      out.push(&camel[start..i]);
      start = i;
    }
  }
  if start < camel.len() {
    out.push(&camel[start..]);
  }
  out
}

/// The longest trailing run of camel words every alternative shares, if there is one.
///
/// What a multi-kind token getter projects. `K::InlineString | K::BlockString` is not two unrelated
/// things: both are strings, and `String` is the word a getter over the pair should be named for.
/// A pair with nothing in common — the shape a mis-aimed `tok_any` would have — answers `None`, and
/// the caller then falls back to the first alternative, which reds the name check rather than
/// inventing a common word that is not there.
fn common_camel_suffix(alternatives: &[String]) -> Option<String> {
  let words: Vec<Vec<&str>> = alternatives.iter().map(|a| camel_words(a)).collect();
  let shortest = words.iter().map(Vec::len).min()?;
  let mut shared = 0usize;
  while shared < shortest {
    let head = words[0][words[0].len() - 1 - shared];
    if !words.iter().all(|w| w[w.len() - 1 - shared] == head) {
      break;
    }
    shared += 1;
  }
  if shared == 0 {
    return None;
  }
  let last = &words[0];
  Some(last[last.len() - shared..].concat())
}

/// `K::Name` -> `Name`; `TypePath` -> `TypePath`.
fn bare_kind(word: &str) -> &str {
  word.strip_prefix("K::").unwrap_or(word)
}

/// The kind or wrapper a getter projects, out of its raw target text.
fn projected(form: &str, target: &str) -> String {
  if form == "tok_any" {
    let alternatives: Vec<String> = target
      .split('|')
      .map(|alt| bare_kind(alt.trim()).to_string())
      .collect();
    return common_camel_suffix(&alternatives).unwrap_or_else(|| {
      alternatives
        .first()
        .unwrap_or_else(|| panic!("`{form} {target}` names no target"))
        .clone()
    });
  }
  let word = target
    .split_whitespace()
    .next()
    .unwrap_or_else(|| panic!("`{form} {target}` names no target"));
  bare_kind(word).to_string()
}

/// Print a set one entry per line, for a failure message worth reading.
fn listed<T: core::fmt::Debug>(items: impl IntoIterator<Item = T>) -> String {
  items
    .into_iter()
    .map(|item| format!("\n  {item:?}"))
    .collect::<String>()
}

/// Getters whose name is not the plain snake of the kind they project, with that projection pinned.
///
/// Twenty-one of 297, in two classes.
///
/// **Role-named, twenty.** Each names a position the grammar has and the kind space does not.
/// Fourteen are `description`: divergence 12 removed GraphQL's `Description` node, so a description
/// here is a bare `K::InlineString` or `K::BlockString` token hanging off the definition, and a
/// getter called `string_token` would say what it holds while saying nothing about which of a
/// definition's several string-shaped positions it is. The two `operation_type`s and
/// [`DirectiveLocations::locations`] are `K::Name` tokens the grammar tells apart by where they sit
/// in the sentence rather than by kind. The two specifier `alias`es and `ImportDefinition::source`
/// are import roles, a production GraphQL has no counterpart for at all.
///
/// **Receiver-elided, one.** `Path::separators` projects `K::PathSeparator` and drops the word the
/// receiver already says; `Path::path_separators` would stutter. It is here rather than in a rule
/// because a rule that let any getter drop its wrapper's name as a prefix would be a rule invented
/// for one getter, and it would weaken the check for the other 296.
///
/// Every entry carries its projection, so a getter re-aimed at another kind reds here rather than
/// slipping through as a pinned exception.
///
/// [`DirectiveLocations::locations`]:
///   smear_parser::graphqlx::lossless::ast::DirectiveLocations::locations
const NAME_EXCEPTIONS: &[(&str, &str, &str, &str)] = &[
  ("DirectiveDefinition", "description", "tok_any", "String"),
  ("EnumTypeDefinition", "description", "tok_any", "String"),
  ("EnumValueDefinition", "description", "tok_any", "String"),
  ("FieldDefinition", "description", "tok_any", "String"),
  ("FragmentDefinition", "description", "tok_any", "String"),
  (
    "InputObjectTypeDefinition",
    "description",
    "tok_any",
    "String",
  ),
  ("InputValueDefinition", "description", "tok_any", "String"),
  (
    "InterfaceTypeDefinition",
    "description",
    "tok_any",
    "String",
  ),
  ("ObjectTypeDefinition", "description", "tok_any", "String"),
  ("OperationDefinition", "description", "tok_any", "String"),
  ("ScalarTypeDefinition", "description", "tok_any", "String"),
  ("SchemaDefinition", "description", "tok_any", "String"),
  ("UnionTypeDefinition", "description", "tok_any", "String"),
  ("VariableDefinition", "description", "tok_any", "String"),
  ("DirectiveLocations", "locations", "toks", "Name"),
  ("ImportDefinition", "source", "opt", "StringValue"),
  ("NamedSpecifier", "alias", "opt", "Path"),
  ("OperationDefinition", "operation_type", "tok", "Name"),
  ("Path", "separators", "toks", "PathSeparator"),
  (
    "RootOperationTypeDefinition",
    "operation_type",
    "tok",
    "Name",
  ),
  ("WildcardSpecifier", "alias", "opt", "Path"),
];

#[test]
fn the_probe_table_is_exactly_the_shipped_inventory() {
  let shipped = source_inventory();

  // Positive control on the parser itself. A source scan that silently found nothing would make
  // every comparison below vacuous, so the counts are pinned before they are used.
  assert_eq!(
    shipped.len(),
    297,
    "the wrapper sources should declare 297 getters"
  );
  let shipped_wrappers: BTreeSet<&str> = shipped.iter().map(|g| g.wrapper.as_str()).collect();
  assert_eq!(shipped_wrappers.len(), 78, "…over 78 wrappers");
  assert_eq!(DECLARED.len(), 297, "the table declares 297 getters");
  assert_eq!(TABLE_WRAPPERS.len(), 78, "…over 78 wrappers");

  let table: BTreeSet<(&str, &str, &str)> = DECLARED.iter().copied().collect();
  assert_eq!(table.len(), DECLARED.len(), "the table repeats an entry");

  let shipped_set: BTreeSet<(&str, &str, &str)> = shipped
    .iter()
    .map(|g| (g.wrapper.as_str(), g.name.as_str(), g.form.as_str()))
    .collect();

  let missing: BTreeSet<_> = shipped_set.difference(&table).collect();
  let extra: BTreeSet<_> = table.difference(&shipped_set).collect();
  assert!(
    missing.is_empty(),
    "the crate ships getters this gate never calls, because they are absent from the table:{}",
    listed(missing)
  );
  assert!(
    extra.is_empty(),
    "the table names getters the crate does not ship:{}",
    listed(extra)
  );

  let table_wrappers: BTreeSet<&str> = TABLE_WRAPPERS.iter().copied().collect();
  assert_eq!(table_wrappers, shipped_wrappers);
}

#[test]
fn every_getter_is_named_for_what_it_projects() {
  // The defect this catches is the one no other gate can see: a getter aimed at a **plausible
  // neighbour** — `interface_type_definitions: many ObjectTypeDefinition` — which compiles,
  // navigates, answers `Some`, round-trips and keeps every byte. Only the name disagrees with the
  // kind, so the name is what has to be checked, for all 297 rather than for the handful a fixture
  // happens to count.
  let mut pinned: BTreeSet<(String, String, String, String)> = NAME_EXCEPTIONS
    .iter()
    .map(|(w, n, f, t)| (w.to_string(), n.to_string(), f.to_string(), t.to_string()))
    .collect();
  let mut offenders = Vec::new();

  for g in source_inventory() {
    let projects = projected(&g.form, &g.target);
    let key = (
      g.wrapper.clone(),
      g.name.clone(),
      g.form.clone(),
      projects.clone(),
    );
    if NAME_EXCEPTIONS
      .iter()
      .any(|(w, n, _, _)| *w == g.wrapper && *n == g.name)
    {
      assert!(
        pinned.remove(&key),
        "{}::{} is pinned as a naming exception, but it now reads `{} {}`",
        g.wrapper,
        g.name,
        g.form,
        g.target
      );
      continue;
    }

    let base = snake(&projects);
    let expected: Vec<String> = match g.form.as_str() {
      "opt" => vec![base],
      "many" => vec![plural(&base)],
      // A plural token getter is named for its token, in the plural.
      "toks" => vec![plural(&base), format!("{base}_tokens")],
      // A singular token getter is named for its token, optionally with a `_token` suffix where the
      // bare name would collide with a node getter (`int_token`, not `int`).
      _ => vec![base.clone(), format!("{base}_token")],
    };
    if !expected.contains(&g.name) {
      offenders.push(format!(
        "{}::{} projects {} but is not named any of {:?}",
        g.wrapper, g.name, projects, expected
      ));
    }
  }

  assert!(
    offenders.is_empty(),
    "a getter's name must state the kind it projects:{}",
    listed(offenders)
  );
  assert!(
    pinned.is_empty(),
    "these naming exceptions are pinned but no longer exist:{}",
    listed(pinned)
  );
}

/// The wrappers that name themselves through a **node**, and which node kind each one uses.
///
/// The positive control for [`no_getter_reaches_a_name_by_counting_keywords`]. "This dialect has no
/// positional getter" is a claim about an absence, and an absence is satisfied just as well by an
/// inventory that lost its getters as by a grammar that never needed them — so the fifteen
/// positions where the twin *would* have counted are pinned by what they hold instead.
///
/// Fifteen is the twin's fourteen plus one. GraphQL spells `type T` as two `K::Name` tokens under
/// one parent and reaches the second by index, fourteen times over; here each of those is a
/// [`DefinitionName`] or an [`ExtensionName`] node. The extra one is
/// [`OperationDefinition`]: GraphQL wraps `query` in an `OperationType` **node**, which leaves the
/// operation's own name as the first bare `K::Name` and reachable with a plain `tok`, while this
/// dialect has no such node and puts the name in a [`DefinitionName`] instead.
const NAME_NODE_CARRIERS: &[(&str, &str, &str)] = &[
  ("DirectiveDefinition", "definition_name", "DefinitionName"),
  ("EnumTypeDefinition", "definition_name", "DefinitionName"),
  ("EnumTypeExtension", "extension_name", "ExtensionName"),
  (
    "FragmentDefinition",
    "executable_definition_name",
    "ExecutableDefinitionName",
  ),
  (
    "InputObjectTypeDefinition",
    "definition_name",
    "DefinitionName",
  ),
  (
    "InputObjectTypeExtension",
    "extension_name",
    "ExtensionName",
  ),
  (
    "InterfaceTypeDefinition",
    "definition_name",
    "DefinitionName",
  ),
  ("InterfaceTypeExtension", "extension_name", "ExtensionName"),
  ("ObjectTypeDefinition", "definition_name", "DefinitionName"),
  ("ObjectTypeExtension", "extension_name", "ExtensionName"),
  ("OperationDefinition", "definition_name", "DefinitionName"),
  ("ScalarTypeDefinition", "definition_name", "DefinitionName"),
  ("ScalarTypeExtension", "extension_name", "ExtensionName"),
  ("UnionTypeDefinition", "definition_name", "DefinitionName"),
  ("UnionTypeExtension", "extension_name", "ExtensionName"),
];

#[test]
fn no_getter_reaches_a_name_by_counting_keywords() {
  // Task 10's ruling 2 in the GraphQL twin became fourteen `tok_nth 1 K::Name` and
  // `tok_nth 2 K::Name` getters, each counting past a definition's or an extension's keywords —
  // and the twin's gate 4 then had to assert that every one of those indices was right, because an
  // index correct for `type T` is wrong for `extend type T`. **This dialect has no such getter**,
  // and the whole defect class with it: a name is a node here, found by kind.
  let inventory = source_inventory();
  let positional: Vec<String> = inventory
    .iter()
    .filter(|g| g.form == "tok_nth")
    .map(|g| format!("{}::{} — {} {}", g.wrapper, g.name, g.form, g.target))
    .collect();
  assert!(
    positional.is_empty(),
    "a positional getter has appeared, and with it a class of off-by-one defect this dialect's \
     shape had foreclosed:{}",
    listed(positional)
  );

  // The control. Without it the assertion above is satisfied by an inventory that found nothing.
  let carriers: BTreeSet<(&str, &str, String)> = inventory
    .iter()
    .filter(|g| {
      matches!(
        g.target.as_str(),
        "DefinitionName" | "ExtensionName" | "ExecutableDefinitionName"
      )
    })
    .map(|g| {
      assert_eq!(
        g.form, "opt",
        "{}::{} names itself through a node, so its getter is singular",
        g.wrapper, g.name
      );
      (g.wrapper.as_str(), g.name.as_str(), g.target.clone())
    })
    .collect();
  let pinned: BTreeSet<(&str, &str, String)> = NAME_NODE_CARRIERS
    .iter()
    .map(|(w, n, t)| (*w, *n, t.to_string()))
    .collect();
  assert_eq!(
    carriers, pinned,
    "the set of wrappers that name themselves through a node moved"
  );
  assert_eq!(carriers.len(), 15);
}

#[test]
fn every_wrapper_casts_from_a_real_parse() {
  let reg = sweep_everything();
  assert!(
    reg.uncast().is_empty(),
    "these wrappers never cast a single node in the whole sweep, which is indistinguishable from \
     their kind being misspelled:{}",
    listed(reg.uncast())
  );
  assert_eq!(reg.casts.len(), 78);
}

#[test]
fn every_getter_is_called_and_projects_something() {
  let reg = sweep_everything();
  assert!(
    reg.uncalled().is_empty(),
    "these getters were never called — no node of their wrapper's kind occurs in the sweep:{}",
    listed(reg.uncalled())
  );
  assert!(
    reg.unanswered().is_empty(),
    "these getters were called but never projected anything, so nothing here distinguishes them \
     from a getter pointed at a kind that cannot occur:{}",
    listed(reg.unanswered())
  );
  assert_eq!(reg.getters.len(), 297);
}

#[test]
fn each_node_kind_casts_to_exactly_the_wrapper_that_names_it() {
  // The near-neighbour decline, made total. The shaped half above asserts it for the sharpest pairs
  // by hand; this asserts it for every kind against all 78 wrappers at once, and it is the check a
  // wrapper pointed at a *neighbouring* kind fails — that wrapper then accepts a node another
  // wrapper already answers for, and the row carries two names instead of one.
  let reg = sweep_everything();
  let mut checked = 0;
  for (kind, node) in &reg.representatives {
    let named = format!("{kind:?}");
    let expected: Vec<&str> = TABLE_WRAPPERS
      .iter()
      .copied()
      .filter(|w| *w == named)
      .collect();
    assert_eq!(
      accepting_wrappers(node),
      expected,
      "a {named} node must cast to {expected:?} and to nothing else"
    );
    checked += 1;
  }

  // `Root` and `Error` are bookkeeping kinds with no wrapper by design, so their rows are the two
  // empty ones — and they are only evidence if the sweep actually reached them.
  assert!(
    reg.representatives.contains_key(&K::Root) && reg.representatives.contains_key(&K::Error),
    "the sweep must reach the two bookkeeping node kinds too, or their empty rows prove nothing"
  );
  assert!(accepting_wrappers(&reg.representatives[&K::Root]).is_empty());
  assert!(accepting_wrappers(&reg.representatives[&K::Error]).is_empty());

  // `Gap` is the third bookkeeping kind and it casts to nothing for a stronger reason: it is never
  // a node at all. The sink tiles an unlexable byte range as a **token**, so there is no
  // `SyntaxNode` to present to a cast — asserted here rather than assumed, with the token side as
  // its control, because "no wrapper accepts a Gap" would otherwise be true of a kind the parser
  // had simply stopped emitting.
  assert!(
    !reg.representatives.contains_key(&K::Gap),
    "a Gap occurred as a node; it is a token tile and the cast matrix has no row for it"
  );
  assert!(
    reg.token_kinds.contains(&K::Gap),
    "no Gap token occurred in the whole sweep, so the assertion above is about nothing"
  );

  // 78 wrappers plus `Root` and `Error`; `Gap` is excluded by the assertion above.
  assert_eq!(checked, 80);
}

/// Every getter the shared corpus alone calls but never gives anything to project.
///
/// The census behind [`FIXTURES`], and the reason each of those four exists. It is pinned rather
/// than described because the alternative is a comment: a corpus entry added later that happens to
/// write a set of floats would make one fixture redundant, and nothing would say so —
/// [`every_added_source_earns_its_place`] is the other half of that guard.
///
/// Note what is *not* here. The corpus reaches all 78 wrappers and calls all 297 getters on its
/// own; every entry below is a getter with a node to run on and nothing in it to find, which is a
/// statement about what GraphQLx the corpus writes rather than about what the typed layer can
/// reach.
///
/// The 63 cluster in three places, and all three are GraphQLx's own additions showing up as corpus
/// gaps: 35 are a value kind in a value position (the corpus writes integers, strings and objects
/// and little else, so `set`, `map`, floats, booleans, `null`, enums and variables are missing from
/// most positions), 15 are a set type or a map type somewhere other than the top level, and 7 are a
/// description on a definition the corpus never describes.
const CORPUS_CANNOT_PROJECT: &[(&str, &str)] = &[
  ("DefaultValue", "enum_value"),
  ("DefaultValue", "float_value"),
  ("DefaultValue", "map_value"),
  ("DefaultValue", "null_value"),
  ("DefaultValue", "object_value"),
  ("DefaultValue", "set_value"),
  ("DefaultValue", "string_value"),
  ("DefinitionTypeParam", "list_type"),
  ("DefinitionTypeParam", "map_type"),
  ("DefinitionTypeParam", "set_type"),
  ("DirectiveDefinition", "description"),
  ("EnumTypeDefinition", "description"),
  ("InputObjectTypeDefinition", "description"),
  ("InputObjectTypeExtension", "where_clause"),
  ("InputValueDefinition", "list_type"),
  ("InputValueDefinition", "map_type"),
  ("InputValueDefinition", "set_type"),
  ("InterfaceTypeDefinition", "description"),
  ("InterfaceTypeDefinition", "directives"),
  ("InterfaceTypeExtension", "implement_interfaces"),
  ("InterfaceTypeExtension", "where_clause"),
  ("ListType", "map_type"),
  ("ListType", "set_type"),
  ("ListValue", "boolean_values"),
  ("ListValue", "enum_values"),
  ("ListValue", "float_values"),
  ("ListValue", "map_values"),
  ("ListValue", "null_values"),
  ("ListValue", "object_values"),
  ("ListValue", "set_values"),
  ("ListValue", "variable_values"),
  ("MapEntry", "boolean_values"),
  ("MapEntry", "float_values"),
  ("MapEntry", "map_values"),
  ("MapEntry", "null_values"),
  ("MapType", "map_types"),
  ("MapType", "set_types"),
  ("ObjectField", "boolean_value"),
  ("ObjectField", "enum_value"),
  ("ObjectField", "float_value"),
  ("ObjectField", "list_value"),
  ("ObjectField", "map_value"),
  ("ObjectField", "null_value"),
  ("ObjectField", "set_value"),
  ("ObjectField", "variable_value"),
  ("OperationDefinition", "directives"),
  ("ScalarTypeDefinition", "description"),
  ("SchemaDefinition", "description"),
  ("SetType", "list_type"),
  ("SetType", "map_type"),
  ("SetValue", "boolean_values"),
  ("SetValue", "float_values"),
  ("SetValue", "list_values"),
  ("SetValue", "map_values"),
  ("SetValue", "null_values"),
  ("SetValue", "object_values"),
  ("SetValue", "set_values"),
  ("SetValue", "string_values"),
  ("TypeGenerics", "set_types"),
  ("UnionTypeDefinition", "description"),
  ("UnionTypeExtension", "where_clause"),
  ("VariableDefinition", "map_type"),
  ("VariableDefinition", "set_type"),
];

#[test]
fn the_shared_corpus_alone_reaches_every_wrapper_and_calls_every_getter() {
  let reg = sweep_corpus();
  assert!(
    reg.uncast().is_empty(),
    "the corpus alone must reach every wrapper — these need a fixture, and that is a corpus gap \
     worth knowing about:{}",
    listed(reg.uncast())
  );
  assert!(
    reg.uncalled().is_empty(),
    "the corpus alone must call every getter:{}",
    listed(reg.uncalled())
  );
  assert_eq!(
    reg.unanswered(),
    CORPUS_CANNOT_PROJECT
      .iter()
      .copied()
      .collect::<BTreeSet<_>>(),
    "the corpus's projection gap moved; re-check whether FIXTURES still earns its keep"
  );
}

#[test]
fn every_fixture_is_a_document_the_suite_accepts() {
  // A fixture that quietly failed to parse would still make getters answer — recovery keeps the
  // subtrees it managed to build — so "the sweep is green" would then rest on trees nobody meant to
  // write. Each fixture is a document this suite accepts outright.
  for (name, src) in FIXTURES {
    let parse = parse_document(src);
    assert!(
      !parse.has_errors(),
      "fixture `{name}` does not parse cleanly: {:?}",
      parse
        .diagnostics()
        .iter()
        .map(|d| d.span())
        .collect::<Vec<_>>()
    );
  }
}

/// Deliberately malformed sources, swept for one property nothing well-formed can show.
///
/// A list wrapper's children are all of one kind by construction — a `FieldsDefinition` holds
/// `FieldDefinition`s and nothing else — so on every valid document `field_definitions()` and "hand
/// back every child node" are the same function. Recovery is the only thing that ever puts a
/// foreign child in such a list, and these eight are where it does.
///
/// **They are eight and not twenty-three because that is what was measured**, and
/// [`every_added_source_earns_its_place`] re-measures it every run. Twenty-three malformed shapes
/// were probed and fifteen dropped, in two groups:
///
/// - **Six the corpus already covers.** A hole in a selection set, an enum body, an import list, a
///   list value, a set value or a generic argument list separates nothing the `invalid_*` corpus
///   entries do not separate already.
/// - **Nine that produce no foreign child at all.** Five aimed at `Directives`,
///   `ImplementInterfaces`, `UnionMemberTypes`, `WhereClause` and `WherePredicate`, and four aimed
///   at putting an `Error` in front of a definition's name. Both fail for the same structural
///   reason, which [`a_hole_in_front_of_a_definitions_name_costs_the_whole_definition`] pins: a
///   definition that fails outside its body unwinds past its own mark, so the tree keeps sibling
///   `Error`s at document level and loses the enclosing node entirely. Those wrappers are in
///   [`UNDISCRIMINATED`].
///
/// One of the eight is worth a line on its own. `type T { a: Int ! b: Int }` — the shape the
/// GraphQL twin uses for this — **parses clean here**, because `Int !` is a non-null marker and the
/// `!` is consumed by the type rather than left as a hole. The `*` in its place is an
/// [`Asterisk`](K::Asterisk), which no field definition admits.
const RECOVERY_FIXTURES: &[(&str, &str)] = &[
  // An `Error` child inside an `ObjectValue`, beside two real `ObjectField`s.
  ("an_object_value_with_a_hole", "{ f(o: {a: 1 ! b: 2}) }"),
  // …inside a `MapValue`.
  (
    "a_map_value_with_a_hole",
    "{ f(m: map { 1 => 2 ! 3 => 4 }) }",
  ),
  // …inside a `RootOperationTypesDefinition`.
  (
    "a_schema_body_with_a_hole",
    "schema { query: Q ! mutation: M }",
  ),
  // …inside a `DefinitionTypeGenerics`.
  (
    "a_generic_parameter_list_with_a_hole",
    "type T<A, ! , B> { f: Int }",
  ),
  // …inside a `FieldsDefinition`. The hole is a `*` and not the twin's `!`, because `Int !` is a
  // non-null marker and this shape parses clean with one — see this const's docs.
  ("a_fields_body_with_a_hole", "type T { a: Int * b: Int }"),
  // …inside an `ArgumentsDefinition`.
  (
    "an_arguments_definition_with_a_hole",
    "type T { f(a: Int * b: Int): Int }",
  ),
  // …inside an `InputFieldsDefinition`.
  ("an_input_body_with_a_hole", "input In { a: Int * b: Int }"),
  // …inside a `VariablesDefinition`.
  (
    "a_variables_definition_with_a_hole",
    "query Q($a: Int * $b: Int) { f }",
  ),
];

#[test]
fn every_added_source_earns_its_place() {
  // **The one check the GraphQL twin leaves to a comment.** Over there the recovery fixtures carry
  // a paragraph saying twelve shapes were probed and three kept; nothing re-measures it, so a
  // corpus entry added later that happens to write the same shape would make a fixture dead weight
  // and no test would say so. Here every added source is left out in turn, and it has to be the
  // only thing in the whole sweep that produces some hit or separates some getter.
  //
  // Leave-one-out rather than "adds something to the corpus": two fixtures that both write a set
  // value would each pass the weaker form while either alone would do.
  let full = sweep_everything();
  let full_unanswered = full.unanswered();
  let full_undiscriminated = full.undiscriminated();
  let sources = added_sources();
  let mut freeloaders = Vec::new();
  for (index, (name, _)) in sources.iter().enumerate() {
    let without = sweep_without(index);
    // Named rather than counted. Dropping a source can only ever *grow* these two sets, so a count
    // would do — but a count says "one thing was lost" where the name says which, and the whole
    // reason this test exists is to be read when it fires.
    let unanswered = without.unanswered();
    let undiscriminated = without.undiscriminated();
    let lost_hits: Vec<_> = unanswered.difference(&full_unanswered).collect();
    let lost_separations: Vec<_> = undiscriminated.difference(&full_undiscriminated).collect();
    if lost_hits.is_empty() && lost_separations.is_empty() {
      freeloaders.push(*name);
    }
  }
  assert!(
    freeloaders.is_empty(),
    "these sources are carried by the sweep and contribute nothing the rest of it does not \
     already have — either the corpus grew into their shape or they were never needed:{}",
    listed(freeloaders)
  );
  assert_eq!(sources.len(), 12);
}

#[test]
fn a_hole_in_front_of_a_definitions_name_costs_the_whole_definition() {
  // **This is why 14 of [`UNDISCRIMINATED`] cannot be closed, and it is measured rather than
  // asserted away.** Every SDL definition and every extension names itself through a node here, and
  // that node is always the first child — so `definition_name()` and "take the first child node"
  // are the same function on every tree the grammar builds. The one thing that would separate them
  // is a foreign child sitting in front of the name, and recovery does not produce one: a
  // definition that fails before its body unwinds **past its own mark**, so the tree keeps sibling
  // `Error` nodes at document level and no definition node at all.
  let lost = parse_document("type * T { f: Int }");
  assert!(lost.has_errors(), "the probe must be malformed");
  let root = lost.syntax();
  assert!(
    !root
      .descendants()
      .any(|n| n.kind() == K::ObjectTypeDefinition),
    "the definition survived, so a hole before its name is now reachable and 14 census entries \
     can be closed"
  );
  assert!(
    root.descendants().any(|n| n.kind() == K::Error),
    "the junk went somewhere other than an Error node"
  );

  // The contrast, and the reason the census is 46 rather than 50: a hole **inside** a body keeps
  // the enclosing node, and the `Error` then lands in a list wrapper where a kind-blind rival can
  // see it. `VariablesDefinition` is where that happens, which is what
  // [`RECOVERY_FIXTURES`] is built out of.
  let kept = parse_document("query Q(* $a: Int) { f }");
  assert!(kept.has_errors(), "the contrast must be malformed too");
  let variables = kept
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::VariablesDefinition)
    .expect("a hole inside the parentheses keeps the VariablesDefinition");
  assert_eq!(
    variables.children().next().map(|n| n.kind()),
    Some(K::Error),
    "the Error must be the list's first child, or it separates nothing from `all child nodes`"
  );
}

#[test]
fn every_recovery_fixture_really_is_malformed() {
  // The mirror of [`every_fixture_is_a_document_the_suite_accepts`]. These exist to put an `Error`
  // node inside a list wrapper; one that quietly started parsing cleanly would stop doing that, and
  // [`UNDISCRIMINATED`] would grow entries with no explanation attached.
  for (name, src) in RECOVERY_FIXTURES {
    assert!(
      parse_document(src).has_errors(),
      "recovery fixture `{name}` parses cleanly, so it no longer holds a recovery hole"
    );
  }
}

/// The getters no tree this gate sweeps ever separates from a kind-blind rival, and why.
///
/// 46 of 297. Every other getter is **measured** to answer differently, at least once, from a
/// getter of the same arity that ignored kinds entirely — the property that makes this sweep's
/// fixtures worth anything: over `"{ a }"` a `cast::child` is indistinguishable from "take the
/// first child", and a suite built out of fixtures like that reports full coverage of a layer it
/// never tested.
///
/// The 46 fall into four shapes, and none of them is a weak fixture:
///
/// - **The wanted child is always the node's first, 28.** Every `definition_name`, every
///   `extension_name`, every `type_path` and every `path` opens its parent. See the finding below.
/// - **The wanted token is always the node's first, 8.** An `Alias` is `name :`, an `Argument` is
///   `name : value`, a `DefinitionName` is `Name <A>`. "The first token" and "the `Name` token" are
///   the same token in every tree the grammar builds.
/// - **The node holds exactly one token, and the getter wants it, 5.** `IntValue`, `FloatValue`,
///   `StringValue`, `BooleanValue`, `NullValue`.
/// - **Every child the node can have is of the projected kind, 5.** `Directives`,
///   `ImplementInterfaces`, `UnionMemberTypes`, `WhereClause` and `WherePredicate`, whose parents
///   admit exactly one child kind each and which recovery cannot be made to violate — see
///   [`RECOVERY_FIXTURES`].
///
/// # The finding: this census is **larger** than the GraphQL twin's, and the design predicted the
/// opposite
///
/// GraphQL pins 24 of 204, 11.8%. This pins 46 of 297, 15.5% — more, absolutely and
/// proportionally. The design expected fewer, on the reasoning that divergences 1, 2, 10 and 11
/// replace single-token positions with paths and generics lists, so nodes that held exactly one
/// child over there hold structure here. **That reasoning has the direction backwards**, and the
/// measurement is what shows it: promoting a token position to a *node* position does not separate
/// a getter from a kind-blind rival, because the node it projects is then its parent's first child.
/// GraphQL's one-token `NamedType::name` becomes GraphQLx's `TypePath::path` — undiscriminated
/// either way, only the class changes.
///
/// What actually drives the difference is the other half of the same divergence. GraphQL reads a
/// definition's name with `tok_nth 1 K::Name`, and a positional getter is separable **by being
/// positional**: index 1 is not "the first token", so it beats the rival on every tree. Fourteen
/// getters over there are discriminated for exactly that reason. Here those fourteen are
/// [`DefinitionName`] and [`ExtensionName`] nodes, found by kind, sitting first — so the dialect
/// traded a whole class of off-by-one defect (which
/// [`no_getter_reaches_a_name_by_counting_keywords`] records as gone) for a class of blindness in
/// the gate that watches it. **The typed layer is safer and this gate is weaker, in the same spot,
/// for the same reason.** That is the trade, and it belongs on the page rather than in a count
/// nobody compared.
const UNDISCRIMINATED: &[(&str, &str)] = &[
  ("Alias", "name"),
  ("Argument", "name"),
  ("BooleanValue", "name"),
  ("DefinitionName", "definition_type_generics"),
  ("DefinitionName", "name"),
  ("DefinitionTypeParam", "name"),
  ("DefinitionTypePath", "path"),
  ("Directive", "type_path"),
  ("DirectiveDefinition", "definition_name"),
  ("Directives", "directives"),
  ("EnumTypeDefinition", "definition_name"),
  ("EnumTypeExtension", "extension_name"),
  ("EnumValue", "path"),
  ("EnumValueDefinition", "directives"),
  (
    "ExecutableDefinitionName",
    "executable_definition_type_generics",
  ),
  ("ExecutableDefinitionName", "name"),
  ("ExtensionName", "path"),
  ("FloatValue", "float_token"),
  ("FragmentSpread", "type_path"),
  ("ImplementInterfaces", "type_paths"),
  ("InputObjectTypeDefinition", "definition_name"),
  ("InputObjectTypeExtension", "extension_name"),
  ("IntValue", "int_token"),
  ("InterfaceTypeDefinition", "definition_name"),
  ("InterfaceTypeExtension", "extension_name"),
  ("NamedSpecifier", "alias"),
  ("NamedSpecifier", "name"),
  ("NullValue", "name"),
  ("ObjectField", "name"),
  ("ObjectTypeDefinition", "definition_name"),
  ("ObjectTypeExtension", "extension_name"),
  ("RootOperationTypeDefinition", "operation_type"),
  ("RootOperationTypeDefinition", "type_path"),
  ("ScalarTypeDefinition", "definition_name"),
  ("ScalarTypeExtension", "extension_name"),
  ("SchemaExtension", "directives"),
  ("StringValue", "string_token"),
  ("TypeCondition", "type_path"),
  ("TypePath", "path"),
  ("UnionMemberTypes", "type_paths"),
  ("UnionTypeDefinition", "definition_name"),
  ("UnionTypeExtension", "extension_name"),
  ("VariableDefinition", "variable_value"),
  ("WhereClause", "where_predicates"),
  ("WherePredicate", "type_paths"),
  ("WildcardSpecifier", "alias"),
];

#[test]
fn every_getter_but_these_answers_differently_from_a_kind_blind_rival() {
  let reg = sweep_everything();
  let undiscriminated = reg.undiscriminated();
  let pinned: BTreeSet<(&str, &str)> = UNDISCRIMINATED.iter().copied().collect();
  let regressed: BTreeSet<_> = undiscriminated.difference(&pinned).collect();
  let improved: BTreeSet<_> = pinned.difference(&undiscriminated).collect();
  assert!(
    regressed.is_empty(),
    "these getters stopped being separable from a kind-blind rival, which means the material this \
     gate sweeps got weaker rather than the layer getting better:{}",
    listed(regressed)
  );
  assert!(
    improved.is_empty(),
    "these getters are now separable from a kind-blind rival — good news, and the census has to \
     say so:{}",
    listed(improved)
  );
  assert_eq!(
    DECLARED.len() - undiscriminated.len(),
    251,
    "251 of the 297 getters are proved to beat a kind-blind rival"
  );
}
