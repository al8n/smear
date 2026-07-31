#![cfg(feature = "rowan")]

//! Task 8's gate, part one: the twenty-two SDL definition node kinds, their trivia-invariance,
//! their verbatim round-trip, and the emptiness verdicts that must agree with `syntactic/`
//! production by production.
//!
//! **These tests drive `parse_str`, not a per-production driver.** Task 8 replaces the
//! `document` stub with the real production, so the public entry point finally reaches every
//! kind below — and asserting through it is strictly stronger than a driver would be, because
//! it also pins the dispatch that chooses each production.
//!
//! Every assertion is on the node-kind sequence, the node's own text, or both. Three earlier
//! tasks were bitten by a mutation that left the kind vector untouched and moved only a node's
//! extent, so every retro-wrap here — the `Description` mark above all — carries a node-**text**
//! assertion beside its kind vector.

use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{Parse, parse_str},
};

/// The tree's node kinds in pre-order, ignoring tokens and trivia.
fn kinds(parse: &Parse) -> Vec<K> {
  parse.syntax().descendants().map(|n| n.kind()).collect()
}

/// Every byte the tree kept, in order — the assertion a boolean verdict cannot fake.
fn text(parse: &Parse) -> String {
  parse.syntax().text().to_string()
}

/// The text of every node of `kind`, in pre-order — what a node *covers*, which is where a
/// mark placed one token too early or too late shows up.
fn texts_of(parse: &Parse, kind: K) -> Vec<String> {
  parse
    .syntax()
    .descendants()
    .filter(|n| n.kind() == kind)
    .map(|n| n.text().to_string())
    .collect()
}

// ---- Description, and the mark that puts it inside its definition --------------------------

#[test]
fn a_description_lives_inside_the_definition_it_describes() {
  let parse = parse_str("\"doc\" scalar S");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::ScalarTypeDefinition,
      K::Description
    ]
  );
  assert_eq!(texts_of(&parse, K::Description), ["\"doc\""]);
  // The load-bearing assertion: the definition's node starts at the *description*, which is
  // only true if the dispatcher's mark is minted before the description is committed and spent
  // by the definition's own retro-wrap. A mark minted after the description leaves `Description`
  // a sibling of the definition instead of a child.
  assert_eq!(
    texts_of(&parse, K::ScalarTypeDefinition),
    ["\"doc\" scalar S"]
  );
}

#[test]
fn a_block_string_is_a_description_too() {
  let parse = parse_str("\"\"\"doc\"\"\" type T");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::ObjectTypeDefinition,
      K::Description
    ]
  );
  assert_eq!(texts_of(&parse, K::Description), ["\"\"\"doc\"\"\""]);
}

#[test]
fn a_definition_does_not_start_at_the_trivia_before_its_description() {
  // Task 6's ruling for `NonNullType`, at the top level: a definition starts at its own first
  // token, not at the blank line above it.
  //
  // **What defends it is the document loop's own `peek_kind`, not the ordering inside
  // `definition`.** Swapping `definition`'s peek and its `cst_mark` reds nothing — measured, and
  // recorded as a silent pass rather than glossed — because the loop's condition has already
  // crossed the trivia by the time either line runs. This assertion is still the one that would
  // catch a future caller reaching `definition` without peeking, which is why it stays.
  let parse = parse_str("  # lead\n  \"doc\" scalar S");
  assert_eq!(
    texts_of(&parse, K::ScalarTypeDefinition),
    ["\"doc\" scalar S"]
  );
}

#[test]
fn no_description_means_no_description_node() {
  let parse = parse_str("scalar S");
  assert_eq!(
    kinds(&parse),
    vec![K::Root, K::Document, K::ScalarTypeDefinition]
  );
}

// ---- The six type definitions --------------------------------------------------------------

#[test]
fn a_scalar_type_definition_is_its_keyword_name_and_directives() {
  assert_eq!(
    kinds(&parse_str("scalar S @d")),
    vec![
      K::Root,
      K::Document,
      K::ScalarTypeDefinition,
      K::Directives,
      K::Directive
    ]
  );
}

#[test]
fn an_object_type_definition_carries_every_component() {
  assert_eq!(
    kinds(&parse_str(
      "type T implements A & B @d { f(x: Int = 1 @e): String! @g }"
    )),
    vec![
      K::Root,
      K::Document,
      K::ObjectTypeDefinition,
      K::ImplementsInterfaces,
      K::NamedType,
      K::NamedType,
      K::Directives,
      K::Directive,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      K::NamedType,
      K::DefaultValue,
      K::IntValue,
      K::Directives,
      K::Directive,
      K::NonNullType,
      K::NamedType,
      K::Directives,
      K::Directive,
    ]
  );
}

#[test]
fn an_interface_type_definition_carries_every_component() {
  assert_eq!(
    kinds(&parse_str("interface I implements A { f: Int }")),
    vec![
      K::Root,
      K::Document,
      K::InterfaceTypeDefinition,
      K::ImplementsInterfaces,
      K::NamedType,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::NamedType,
    ]
  );
}

#[test]
fn a_union_type_definition_lists_its_members() {
  // A leading `|` is accepted — `syntactic/`'s `union_members_after_equal` takes an optional
  // one, and gate 1 compares verdicts input by input.
  let parse = parse_str("union U = | A | B");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::UnionTypeDefinition,
      K::UnionMemberTypes,
      K::NamedType,
      K::NamedType,
    ]
  );
  assert!(!parse.has_errors(), "a leading `|` is accepted");
}

#[test]
fn an_enum_type_definition_wraps_each_value() {
  assert_eq!(
    kinds(&parse_str("enum E { \"doc\" A @d B }")),
    vec![
      K::Root,
      K::Document,
      K::EnumTypeDefinition,
      K::EnumValuesDefinition,
      K::EnumValueDefinition,
      K::Description,
      K::EnumValue,
      K::Directives,
      K::Directive,
      K::EnumValueDefinition,
      K::EnumValue,
    ]
  );
}

#[test]
fn an_input_object_type_definition_wraps_each_field() {
  assert_eq!(
    kinds(&parse_str("input I { a: Int = 1 }")),
    vec![
      K::Root,
      K::Document,
      K::InputObjectTypeDefinition,
      K::InputFieldsDefinition,
      K::InputValueDefinition,
      K::NamedType,
      K::DefaultValue,
      K::IntValue,
    ]
  );
}

// ---- Schema and directive definitions ------------------------------------------------------

#[test]
fn a_schema_definition_wraps_each_root_operation_type() {
  assert_eq!(
    kinds(&parse_str("schema @d { query: Q mutation: M }")),
    vec![
      K::Root,
      K::Document,
      K::SchemaDefinition,
      K::Directives,
      K::Directive,
      K::RootOperationTypeDefinitions,
      K::RootOperationTypeDefinition,
      K::OperationType,
      K::NamedType,
      K::RootOperationTypeDefinition,
      K::OperationType,
      K::NamedType,
    ]
  );
}

#[test]
fn a_directive_definition_carries_its_arguments_and_locations() {
  let parse = parse_str("directive @d(a: Int) repeatable on | FIELD | QUERY");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::DirectiveDefinition,
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      K::NamedType,
      K::DirectiveLocations,
    ]
  );
  assert!(!parse.has_errors(), "a leading `|` is accepted");
  // A location has no node kind of its own, so the locations surface as bare `Name` tokens
  // inside the one `DirectiveLocations` node.
  assert_eq!(texts_of(&parse, K::DirectiveLocations), ["| FIELD | QUERY"]);
}

#[test]
fn a_location_that_names_no_directive_location_is_reported() {
  // `syntactic/`'s `location` admits only the nineteen spelled locations and errors otherwise,
  // and gate 1 compares verdicts. The name is still consumed, so the tree keeps its shape.
  let parse = parse_str("directive @d on NOPE");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::DirectiveDefinition,
      K::DirectiveLocations
    ]
  );
  assert_eq!(text(&parse), "directive @d on NOPE");
  assert!(parse.has_errors(), "`NOPE` is no directive location");
}

// ---- The undelimited repetitions, and the trailing-trivia law they inherit ------------------

#[test]
fn an_undelimited_clause_ends_with_its_trailing_trivia_inside_it() {
  // Task 6's law, third occurrence. `ImplementsInterfaces`, `UnionMemberTypes` and
  // `DirectiveLocations` all end on a peek for their own separator, and that peek must cross
  // the trailing trivia to learn no separator follows — while the node is still open. A node's
  // extent is `[mark, now]` and `cst_finish` has no `_at` form, so a node cannot end in the
  // past. Text fidelity is untouched; only `text_range` is one trivia run longer.
  assert_eq!(
    texts_of(
      &parse_str("type T implements A\nscalar S"),
      K::ImplementsInterfaces
    ),
    ["implements A\n"]
  );
  assert_eq!(
    texts_of(&parse_str("union U = A\nscalar S"), K::UnionMemberTypes),
    ["= A\n"]
  );
  assert_eq!(
    texts_of(
      &parse_str("directive @d on FIELD\nscalar S"),
      K::DirectiveLocations
    ),
    ["FIELD\n"]
  );
  // A *delimited* repetition is immune: its closer ends the node before anything after it is
  // read.
  assert_eq!(
    texts_of(
      &parse_str("type T { a: Int }\nscalar S"),
      K::FieldsDefinition
    ),
    ["{ a: Int }"]
  );
}

// ---- Emptiness, decided per production against `syntactic/` ---------------------------------

#[test]
fn every_one_or_more_shape_reports_when_it_is_empty() {
  // `syntactic/` marks each of these "nonempty" and enforces it with `.at_least(1)`, and gate 1
  // compares the two suites' verdicts input by input. This is the **opposite** ruling from
  // Task 6's `Arguments`, where `syntactic/` documents the lenient `()` as accepted — the two
  // are decided one production at a time, not by a house rule.
  for src in [
    "type T { f(): Int }", // ArgumentsDefinition
    "type T {}",           // FieldsDefinition
    "input I {}",          // InputFieldsDefinition
    "enum E {}",           // EnumValuesDefinition
    "schema {}",           // RootOperationTypeDefinitions
    "type T implements",   // ImplementsInterfaces
    "union U =",           // UnionMemberTypes
    "directive @d on",     // DirectiveLocations
  ] {
    let parse = parse_str(src);
    assert!(
      parse.has_errors(),
      "{src:?} is empty where the grammar says one-or-more"
    );
    assert_eq!(text(&parse), src, "{src:?} must still round-trip");
  }
}

#[test]
fn an_enum_value_may_not_be_true_false_or_null() {
  // `syntactic/`'s `take_enum_value` refuses the three reserved spellings. The token is still
  // consumed and still becomes an `EnumValue`, so the tree keeps the node a diagnostic points
  // at — the same trade `type_condition` makes for a missing `on`.
  for src in ["enum E { true }", "enum E { false }", "enum E { null }"] {
    let parse = parse_str(src);
    assert!(parse.has_errors(), "{src:?} names a reserved value");
    assert_eq!(text(&parse), src);
  }
  assert!(!parse_str("enum E { truthy }").has_errors());
}

// ---- Trivia invariance ----------------------------------------------------------------------

#[test]
fn trivia_does_not_change_a_definition_shape() {
  let compact = "type T implements A&B@d{f(x:Int=1):String!}";
  let padded = "  type T implements  A  &  B  @d  { # why\n f ( x : Int = 1 ) : String ! , }  ";
  assert_eq!(
    kinds(&parse_str(compact)),
    kinds(&parse_str(padded)),
    "the padded form must have the compact form's shape"
  );
  assert_eq!(text(&parse_str(padded)), padded);
}

#[test]
fn trivia_does_not_change_a_schema_or_directive_definition_shape() {
  let compact = "schema{query:Q}directive@d(a:Int)repeatable on|FIELD";
  let padded = "  schema  { query : Q }  directive  @d ( a : Int ) repeatable  on | FIELD  ";
  assert_eq!(kinds(&parse_str(compact)), kinds(&parse_str(padded)));
  assert_eq!(text(&parse_str(padded)), padded);
}

// ---- Reachability and round-trip -------------------------------------------------------------

#[test]
fn every_definition_kind_is_reachable() {
  // A production nobody can reach is a production nobody tests.
  let src = concat!(
    "\"d\" scalar S @a\n",
    "type T implements A & B @a { f(x: Int = 1): String! }\n",
    "interface I { g: Int }\n",
    "union U = A | B\n",
    "enum E { X }\n",
    "input N { y: Int }\n",
    "schema { query: Q }\n",
    "directive @dd(z: Int) repeatable on FIELD\n",
  );
  let got = kinds(&parse_str(src));
  for want in [
    K::Description,
    K::InputValueDefinition,
    K::ArgumentsDefinition,
    K::FieldDefinition,
    K::FieldsDefinition,
    K::InputFieldsDefinition,
    K::ImplementsInterfaces,
    K::UnionMemberTypes,
    K::DirectiveLocations,
    K::EnumValueDefinition,
    K::EnumValuesDefinition,
    K::OperationType,
    K::RootOperationTypeDefinition,
    K::RootOperationTypeDefinitions,
    K::ScalarTypeDefinition,
    K::ObjectTypeDefinition,
    K::InterfaceTypeDefinition,
    K::UnionTypeDefinition,
    K::EnumTypeDefinition,
    K::InputObjectTypeDefinition,
    K::DirectiveDefinition,
    K::SchemaDefinition,
  ] {
    assert!(
      got.contains(&want),
      "{want:?} was not produced by the corpus"
    );
  }
  assert!(
    !parse_str(src).has_errors(),
    "the corpus must parse cleanly"
  );
  assert_eq!(text(&parse_str(src)), src);
}

#[test]
fn every_definition_form_round_trips_verbatim() {
  for src in [
    "scalar S",
    "\"\"\"doc\"\"\"\nscalar S @a(b: 1)",
    "type T",
    "type T implements &A&B",
    "union U = |A",
    "enum E{A,B}",
    "input I{a:Int}",
    "schema{query:Q,mutation:M,subscription:S}",
    "directive @d on |FIELD|QUERY",
    "\u{feff}type T { a: [Int!]! }\r\n",
    "  # only a comment\n",
  ] {
    assert_eq!(
      text(&parse_str(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}
