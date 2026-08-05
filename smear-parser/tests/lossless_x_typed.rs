//! Gate 4 — the typed accessor layer over the GraphQLx lossless CST.
//!
//! One test per grammar area, each walking a real tree through the wrappers rather than through
//! [`SyntaxNode`](smear_parser::graphqlx::lossless::SyntaxNode). Task 20 extends this file with the
//! totality half; what is here is Task 15's own gate.
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

use smear_parser::graphqlx::lossless::{
  GraphQLxLang, SyntaxNode, SyntaxToken,
  ast::{
    Argument, Arguments, CastNode, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam,
    Directive, Directives, Document, EnumValue, ExecutableDefinitionName,
    ExecutableDefinitionTypeGenerics, ExtensionName, Field, FieldDefinition, FieldsDefinition,
    FragmentDefinition, FragmentSpread, ImplementInterfaces, ImportDefinition, ImportList,
    InlineFragment, InputValueDefinition, InterfaceTypeDefinition, ListType, ListValue, MapEntry,
    MapType, NamedSpecifier, ObjectTypeDefinition, ObjectTypeExtension, ObjectValue,
    OperationDefinition, Path, ScalarTypeExtension, SelectionSet, SetType, SetValue, StringValue,
    TypeCondition, TypeSystemDocument, VariableDefinition, VariablesDefinition, WhereClause,
    WherePredicate, WildcardSpecifier,
  },
  generic::test_support::{
    parse_definition_type_generics, parse_executable_definition_type_generics,
    parse_extension_name, parse_where_clause,
  },
  parse_str,
  selection::test_support::parse_selection_set,
  ty::test_support::parse_type_ref,
  value::test_support::parse_value,
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
fn a_fields_own_name_is_not_its_aliass() {
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
  let parse = parse_str("query Q<T = Int>($v: [T!] = [1] @d) @e { f }");
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
  let parse = parse_str("\"doc\" fragment <T> F<U> on X @d where A: B { f }");
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
  let parse =
    parse_str("\"doc\" type T<A = Int> implements I & J @d where A: B { f(x: Int = 1): [T!]! @e }");
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
  let parse = parse_str("extend type ns::T<A> implements I @d { f: Int }");
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
  let scalar_parse = parse_str("extend scalar S @d");
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
  let parse = parse_str("import { A as ns::B, * as w } from \"m\"");
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
  let bare = parse_str("import * as w from \"m\"");
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
  let parse = parse_str(
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
