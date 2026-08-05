//! The GraphQLx SDL definition productions.
//!
//! **These tests drive the productions directly, not through `parse_str`**, for the reason
//! `tests/lossless_x_value.rs` records: `parse_str` is still Task 9's drain-everything stub.
//!
//! Every test asserts the **node-kind pre-order** of a real parse, and several also assert a
//! node's *extent*, because the defects this area is prone to change the tree's shape while
//! leaving the text byte-identical: a `where` clause nested under the block it constrains instead
//! of beside it, a description outside the definition it describes, an SDL enum value wrapped in
//! the `EnumValue` node a *value* position builds. No round-trip gate can see any of those.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::definition::test_support::{
    parse_arguments_definition, parse_directive_definition, parse_directive_locations,
    parse_enum_values_definition, parse_field_definition, parse_fields_definition,
    parse_implements_interfaces, parse_input_fields_definition, parse_input_value_definition,
    parse_root_operation_types_definition, parse_schema_definition, parse_type_system_definition,
    parse_union_member_types,
  },
};

/// The tree's node kinds in pre-order, ignoring tokens and trivia.
macro_rules! kinds {
  ($driver:ident, $src:expr) => {
    $driver($src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<K>>()
  };
}

/// Every byte the tree kept, in order — the assertion a boolean verdict cannot fake.
macro_rules! text {
  ($driver:ident, $src:expr) => {
    $driver($src).syntax().text().to_string()
  };
}

/// The text of the first descendant of `kind`, so a test can pin *which* bytes a node covers.
macro_rules! node_text {
  ($driver:ident, $src:expr, $kind:expr) => {
    $driver($src)
      .syntax()
      .descendants()
      .find(|n| n.kind() == $kind)
      .map(|n| n.text().to_string())
  };
}

// ---------------------------------------------------------------------------------------------
// The described members.
// ---------------------------------------------------------------------------------------------

#[test]
fn an_input_value_definition_holds_its_type_default_and_const_directives() {
  assert_eq!(
    kinds!(parse_input_value_definition, "a: Int = 1 @d"),
    vec![
      K::Root,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path,
      K::DefaultValue,
      K::IntValue,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
  assert!(
    parse_input_value_definition("a: Int = $v").has_errors(),
    "an input value definition's default is const"
  );
  assert!(
    parse_input_value_definition("a: Int @d(x: $v)").has_errors(),
    "and so are its directives"
  );
}

/// A description is a **bare string token** inside the member it describes, not a node.
///
/// The kind space has no `Description` kind — the Task 8 census rejects it under *one token is not
/// a region*, recording that a string **value** is a `StringValue` node "so the two can never be
/// confused". Reaching for `string_value` here produces the same text and a tree in which a
/// description and a default string value are indistinguishable.
#[test]
fn a_description_is_a_token_inside_the_member_it_describes() {
  assert_eq!(
    kinds!(parse_input_value_definition, "\"doc\" a: Int"),
    vec![
      K::Root,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path
    ],
    "no StringValue node appears"
  );
  assert_eq!(
    node_text!(
      parse_input_value_definition,
      "\"doc\" a: Int",
      K::InputValueDefinition
    )
    .as_deref(),
    Some("\"doc\" a: Int"),
    "the description is inside the member, which only the extent can show"
  );
  assert_eq!(
    kinds!(parse_field_definition, "\"\"\"doc\"\"\" f: Int"),
    vec![K::Root, K::FieldDefinition, K::DefinitionTypePath, K::Path],
    "a block string description is the same shape"
  );
}

#[test]
fn a_field_definition_holds_its_arguments_type_and_directives() {
  assert_eq!(
    kinds!(parse_field_definition, "f(a: Int): [String!]! @d"),
    vec![
      K::Root,
      K::FieldDefinition,
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path,
      K::ListType,
      K::DefinitionTypePath,
      K::Path,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
}

#[test]
fn the_three_delimited_member_blocks_are_three_node_kinds() {
  assert_eq!(
    kinds!(parse_fields_definition, "{ f: Int }"),
    vec![
      K::Root,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::DefinitionTypePath,
      K::Path
    ]
  );
  assert_eq!(
    kinds!(parse_input_fields_definition, "{ f: Int }"),
    vec![
      K::Root,
      K::InputFieldsDefinition,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path
    ],
    "an input block holds InputValueDefinitions, not FieldDefinitions"
  );
  assert_eq!(
    kinds!(parse_arguments_definition, "(f: Int)"),
    vec![
      K::Root,
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path
    ]
  );
}

#[test]
fn every_plus_block_reports_its_empty_form_without_eating_its_closer() {
  type Driver = fn(&str) -> smear_parser::graphqlx::lossless::Parse;
  for (driver, src) in [
    (parse_fields_definition as Driver, "{}"),
    (parse_input_fields_definition as Driver, "{}"),
    (parse_arguments_definition as Driver, "()"),
    (parse_enum_values_definition as Driver, "{}"),
    (parse_root_operation_types_definition as Driver, "{}"),
  ] {
    let parse = driver(src);
    assert!(
      parse.has_errors(),
      "`syntactic/` marks every one of these `at_least(1)`: {src}"
    );
    assert_eq!(parse.syntax().text().to_string(), src);
    assert_eq!(
      parse
        .syntax()
        .descendants()
        .filter(|n| n.kind() == K::Error)
        .count(),
      0,
      "the report must consume nothing, or the block eats its own closer: {src}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// Divergence 9, the SDL half — an SDL enum value is a plain Name.
// ---------------------------------------------------------------------------------------------

/// In an `EnumValueDefinition` the value stays a bare `Name` token; in a *value* position it is a
/// whole `EnumValue > Path`.
///
/// **GraphQL routes both through one production and GraphQLx cannot** (`value.rs:279` against
/// `definition/enum_type.rs:5-28`). Porting the shared route gives `EnumValueDefinition >
/// EnumValue > Path` here, which re-prints identically and tells the typed layer that an SDL enum
/// value may be qualified — which it may not.
#[test]
fn an_sdl_enum_value_is_a_name_and_not_an_enum_value_node() {
  assert_eq!(
    kinds!(parse_enum_values_definition, "{ RED GREEN }"),
    vec![
      K::Root,
      K::EnumValuesDefinition,
      K::EnumValueDefinition,
      K::EnumValueDefinition
    ],
    "no EnumValue and no Path node: the value is one token"
  );
  assert_eq!(
    kinds!(parse_enum_values_definition, "{ \"doc\" RED @d }"),
    vec![
      K::Root,
      K::EnumValuesDefinition,
      K::EnumValueDefinition,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
  assert!(
    parse_enum_values_definition("{ null }").has_errors(),
    "`take_enum_value` refuses the three reserved spellings, and gate 1 compares verdicts"
  );
  assert_eq!(
    text!(parse_enum_values_definition, "{ null }"),
    "{ null }",
    "and the token is still kept, so a diagnostic has something to point at"
  );
}

// ---------------------------------------------------------------------------------------------
// The undelimited clauses.
// ---------------------------------------------------------------------------------------------

#[test]
fn an_implements_clause_holds_type_paths_not_bare_names() {
  assert_eq!(
    kinds!(parse_implements_interfaces, "implements A & ns::B<Int>"),
    vec![
      K::Root,
      K::ImplementInterfaces,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ],
    "GraphQLx widens an interface to a generic-capable TypePath"
  );
  assert_eq!(
    kinds!(parse_implements_interfaces, "implements & A"),
    vec![K::Root, K::ImplementInterfaces, K::TypePath, K::Path],
    "a leading `&` is accepted"
  );
}

#[test]
fn a_union_member_clause_holds_type_paths() {
  assert_eq!(
    kinds!(parse_union_member_types, "= | A | ns::B"),
    vec![
      K::Root,
      K::UnionMemberTypes,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ]
  );
}

#[test]
fn a_directive_location_is_a_token_and_an_unknown_spelling_is_reported() {
  assert_eq!(
    kinds!(parse_directive_locations, "FIELD | OBJECT"),
    vec![K::Root, K::DirectiveLocations],
    "a Location has no kind of its own: the census gives one identifier token none"
  );
  let parse = parse_directive_locations("| FIELD | NOPE");
  assert!(
    parse.has_errors(),
    "`syntactic/`'s `location` admits only the nineteen spellings"
  );
  assert_eq!(
    text!(parse_directive_locations, "| FIELD | NOPE"),
    "| FIELD | NOPE",
    "and the offending name is still consumed into the list a diagnostic points at"
  );
  assert!(
    !parse_directive_locations(
      "QUERY | MUTATION | SUBSCRIPTION | FIELD | FRAGMENT_DEFINITION \
       | FRAGMENT_SPREAD | INLINE_FRAGMENT | VARIABLE_DEFINITION | SCHEMA | SCALAR | OBJECT \
       | FIELD_DEFINITION | ARGUMENT_DEFINITION | INTERFACE | UNION | ENUM | ENUM_VALUE \
       | INPUT_OBJECT | INPUT_FIELD_DEFINITION"
    )
    .has_errors(),
    "all nineteen spellings are admitted — the predicate is Task 7's frozen macro"
  );
}

// ---------------------------------------------------------------------------------------------
// The type definitions.
// ---------------------------------------------------------------------------------------------

#[test]
fn each_type_definition_keyword_opens_its_own_node() {
  for (src, kind) in [
    ("scalar S @d", K::ScalarTypeDefinition),
    ("type T { f: Int }", K::ObjectTypeDefinition),
    ("interface I { f: Int }", K::InterfaceTypeDefinition),
    ("union U = A | B", K::UnionTypeDefinition),
    ("enum E { RED }", K::EnumTypeDefinition),
    ("input In { f: Int }", K::InputObjectTypeDefinition),
    ("schema { query: Q }", K::SchemaDefinition),
    ("directive @d on FIELD", K::DirectiveDefinition),
  ] {
    let parse = parse_type_system_definition(src);
    assert!(!parse.has_errors(), "{src} must parse clean");
    assert_eq!(
      parse.syntax().descendants().nth(1).map(|n| n.kind()),
      Some(kind),
      "{src} must open {kind:?}"
    );
    assert_eq!(parse.syntax().text().to_string(), src);
  }
}

/// A definition's name is a `DefinitionName`, and its generics may carry defaults.
#[test]
fn a_definition_name_carries_its_declared_generics() {
  assert_eq!(
    kinds!(parse_type_system_definition, "scalar S<T = Int>"),
    vec![
      K::Root,
      K::ScalarTypeDefinition,
      K::DefinitionName,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::DefinitionTypePath,
      K::Path
    ]
  );
}

/// A description is retro-wrapped **into** the definition it describes.
#[test]
fn a_description_lives_inside_the_definition_it_describes() {
  assert_eq!(
    node_text!(
      parse_type_system_definition,
      "\"doc\" scalar S",
      K::ScalarTypeDefinition
    )
    .as_deref(),
    Some("\"doc\" scalar S"),
    "a mark minted one token late leaves the kind vector unchanged and the extent wrong"
  );
  assert_eq!(
    kinds!(parse_type_system_definition, "\"doc\" scalar S"),
    vec![K::Root, K::ScalarTypeDefinition, K::DefinitionName],
    "and still no Description node"
  );
}

/// Each definition reaches **its own** block production, not merely *a* block production.
///
/// `each_type_definition_keyword_opens_its_own_node` sees only the outermost node, and the block
/// tests above drive each block directly — so a definition wired to the wrong block is invisible
/// to both. Measured: `constrained_fields` collapsed to one block production for objects and input
/// objects alike is **green** against every other test in this file, and it makes
/// `input In { f: Int }` build a `FieldsDefinition` of `FieldDefinition`s, which re-prints
/// identically and tells the typed layer that an input field may declare arguments.
#[test]
fn each_definition_reaches_its_own_block_and_clause_productions() {
  for (src, expected) in [
    (
      "type T implements A { f: Int }",
      vec![
        K::ObjectTypeDefinition,
        K::DefinitionName,
        K::ImplementInterfaces,
        K::TypePath,
        K::Path,
        K::FieldsDefinition,
        K::FieldDefinition,
        K::DefinitionTypePath,
        K::Path,
      ],
    ),
    (
      "interface I implements A { f: Int }",
      vec![
        K::InterfaceTypeDefinition,
        K::DefinitionName,
        K::ImplementInterfaces,
        K::TypePath,
        K::Path,
        K::FieldsDefinition,
        K::FieldDefinition,
        K::DefinitionTypePath,
        K::Path,
      ],
    ),
    (
      "input In { f: Int }",
      vec![
        K::InputObjectTypeDefinition,
        K::DefinitionName,
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        K::DefinitionTypePath,
        K::Path,
      ],
    ),
    (
      "enum E { RED }",
      vec![
        K::EnumTypeDefinition,
        K::DefinitionName,
        K::EnumValuesDefinition,
        K::EnumValueDefinition,
      ],
    ),
    (
      "union U = A",
      vec![
        K::UnionTypeDefinition,
        K::DefinitionName,
        K::UnionMemberTypes,
        K::TypePath,
        K::Path,
      ],
    ),
    (
      "schema { query: Q }",
      vec![
        K::SchemaDefinition,
        K::RootOperationTypesDefinition,
        K::RootOperationTypeDefinition,
        K::TypePath,
        K::Path,
      ],
    ),
  ] {
    let parse = parse_type_system_definition(src);
    assert!(!parse.has_errors(), "{src} must parse clean");
    let mut want = vec![K::Root];
    want.extend(expected);
    assert_eq!(
      parse
        .syntax()
        .descendants()
        .map(|n| n.kind())
        .collect::<Vec<K>>(),
      want,
      "{src}"
    );
  }
}

/// Every SDL directive position is **const**, and each one is a separate literal in the source.
///
/// The flavour is invisible to a node-kind pre-order and to the text — a `VariableValue` node is
/// built either way, the rejection being a diagnostic — so only the verdict moves. Measured:
/// flipping all nine `Constness::Const` sites in `definition.rs` to `NonConst` red **one** test
/// before this one existed, which left eight positions with no witness at all.
#[test]
fn every_sdl_directive_position_is_const() {
  type Driver = fn(&str) -> smear_parser::graphqlx::lossless::Parse;
  for (driver, bad, good) in [
    (
      parse_input_value_definition as Driver,
      "a: Int @d(x: $v)",
      "a: Int @d(x: 1)",
    ),
    (
      parse_field_definition as Driver,
      "f: Int @d(x: $v)",
      "f: Int @d(x: 1)",
    ),
    (
      parse_enum_values_definition as Driver,
      "{ RED @d(x: $v) }",
      "{ RED @d(x: 1) }",
    ),
    (
      parse_type_system_definition as Driver,
      "scalar S @d(x: $v)",
      "scalar S @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "type T @d(x: $v)",
      "type T @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "interface I @d(x: $v)",
      "interface I @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "union U @d(x: $v)",
      "union U @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "enum E @d(x: $v)",
      "enum E @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "input In @d(x: $v)",
      "input In @d(x: 1)",
    ),
    (
      parse_type_system_definition as Driver,
      "schema @d(x: $v) { query: Q }",
      "schema @d(x: 1) { query: Q }",
    ),
  ] {
    assert!(
      driver(bad).has_errors(),
      "a variable is a grammar error in this const position: {bad}"
    );
    assert!(
      !driver(good).has_errors(),
      "and the same position with a literal is clean: {good}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// Divergence 16 — a present `where` makes the following block mandatory.
// ---------------------------------------------------------------------------------------------

/// The cross-component constraint: `where` is optional, the block is optional, and **together**
/// they are not.
///
/// This is the shape the design's vanishing-default sketch could not express, and the shape a
/// per-component port silently loses: each half is individually optional, so a production that
/// simply parses both accepts `type T where A: B` with no body and disagrees with `syntactic/`
/// on a verdict gate 1 compares.
#[test]
fn a_where_clause_makes_the_following_block_mandatory_at_all_three_sites() {
  for (constrained, unconstrained) in [
    ("type T where A: B", "type T"),
    ("interface I where A: B", "interface I"),
    ("input In where A: B", "input In"),
  ] {
    assert!(
      !parse_type_system_definition(unconstrained).has_errors(),
      "{unconstrained} is well formed: the block alone is optional"
    );
    let parse = parse_type_system_definition(constrained);
    assert!(
      parse.has_errors(),
      "{constrained} must report: a present `where` makes the block mandatory"
    );
    assert_eq!(
      parse.syntax().text().to_string(),
      constrained,
      "and every byte is still kept"
    );
  }
  // With the block, all three are clean and the clause is a SIBLING of it.
  assert_eq!(
    kinds!(
      parse_type_system_definition,
      "type T<A> where A: B { f: Int }"
    ),
    vec![
      K::Root,
      K::ObjectTypeDefinition,
      K::DefinitionName,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::DefinitionTypePath,
      K::Path
    ],
    "the census gives `Constrained` no kind, so the clause and the block are siblings"
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 17 — a union's `where` forces a preceding `= members`.
// ---------------------------------------------------------------------------------------------

/// The *other* cross-component constraint, and it points the other way: the clause comes **after**
/// the members and requires them, where an object's clause comes before its block and requires it.
///
/// The expectation is `Equal`, not `LBrace`. And the order is the code's, not the kind space's
/// prose: `union_after_keyword` parses `try_union_members` and *then* `try_where_clause`
/// (`definition/union.rs:87-95`).
#[test]
fn a_union_where_clause_requires_its_members_and_follows_them() {
  assert!(
    !parse_type_system_definition("union U").has_errors(),
    "members alone are optional"
  );
  assert!(
    !parse_type_system_definition("union U = A where A: B").has_errors(),
    "and with members the clause is well formed"
  );
  let parse = parse_type_system_definition("union U where A: B");
  assert!(
    parse.has_errors(),
    "a union `where` with no members must report"
  );
  assert_eq!(parse.syntax().text().to_string(), "union U where A: B");
  assert_eq!(
    kinds!(parse_type_system_definition, "union U = A where A: B"),
    vec![
      K::Root,
      K::UnionTypeDefinition,
      K::DefinitionName,
      K::UnionMemberTypes,
      K::TypePath,
      K::Path,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ],
    "members first, then the clause — the reverse order re-prints identically"
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 18 — a directive definition's `where` is a trailing suffix that forces nothing.
// ---------------------------------------------------------------------------------------------

#[test]
fn a_directive_definition_takes_a_trailing_where_that_forces_nothing() {
  assert_eq!(
    kinds!(
      parse_directive_definition,
      "directive @d<T>(a: Int) repeatable on FIELD | OBJECT where T: Node"
    ),
    vec![
      K::Root,
      K::DirectiveDefinition,
      K::DefinitionName,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      K::DefinitionTypePath,
      K::Path,
      K::DirectiveLocations,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ]
  );
  assert!(
    !parse_directive_definition("directive @d on FIELD where T: Node").has_errors(),
    "nothing is forced after the clause"
  );
}

/// **The position that makes the second lookahead token load-bearing.**
///
/// A trailing `where` is followed by the next top-level definition, whose keyword is an ordinary
/// `Identifier` — the same head a predicate's bounded type has. A one-significant-token
/// continuation test reads `type` as another predicate, consumes it, fails on the missing `:` and
/// costs the whole `ObjectTypeDefinition`. Only the *second* token separates
/// `(Identifier, Colon | PathSeparator | LAngle)` from `(Identifier, Identifier)`, and over a
/// trivia-surfacing stream a fixed `peek::<U2>()` answers about the trivia between them — which
/// is why the trivia-laden spelling is asserted beside the compact one.
#[test]
fn a_trailing_where_clause_stops_before_the_next_definition() {
  for tail in [
    " type X { f: Int }",
    "\n\ntype X { f: Int }",
    "\n# a comment\n\ntype X { f: Int }",
  ] {
    let src = format!("directive @d on FIELD where T: Node{tail}");
    let parse = parse_type_system_definition(&src);
    assert!(
      !parse.has_errors(),
      "the clause must end at `Node`, leaving `type X` to the next definition: {src:?}"
    );
    let kinds: Vec<K> = parse.syntax().descendants().map(|n| n.kind()).collect();
    assert_eq!(
      kinds,
      vec![
        K::Root,
        K::DirectiveDefinition,
        K::DefinitionName,
        K::DirectiveLocations,
        K::WhereClause,
        K::WherePredicate,
        K::TypePath,
        K::Path,
        K::TypePath,
        K::Path
      ],
      "and `type` must not have been swallowed as a bounded type: {src:?}"
    );
    assert_eq!(parse.syntax().text().to_string(), src);
  }
}

// ---------------------------------------------------------------------------------------------
// Schema definitions and root operation types.
// ---------------------------------------------------------------------------------------------

#[test]
fn a_root_operation_type_definition_targets_a_type_path() {
  assert_eq!(
    kinds!(parse_root_operation_types_definition, "{ query: ns::Q }"),
    vec![
      K::Root,
      K::RootOperationTypesDefinition,
      K::RootOperationTypeDefinition,
      K::TypePath,
      K::Path
    ],
    "the operation keyword is a bare token; the target is a qualified TypePath"
  );
  assert!(
    parse_root_operation_types_definition("{ nope: Q }").has_errors(),
    "a name that is not one of the three operation types is reported"
  );
}

#[test]
fn a_schema_definition_requires_its_block() {
  assert!(
    parse_schema_definition("schema @d").has_errors(),
    "`schema_after_keyword` calls the block directly, not through a `try_` wrapper"
  );
  assert_eq!(
    kinds!(parse_schema_definition, "schema @d { query: Q }"),
    vec![
      K::Root,
      K::SchemaDefinition,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::RootOperationTypesDefinition,
      K::RootOperationTypeDefinition,
      K::TypePath,
      K::Path
    ]
  );
}

#[test]
fn an_unknown_definition_keyword_is_reported_and_skipped() {
  let parse = parse_type_system_definition("nonsense S");
  assert!(parse.has_errors());
  assert_eq!(
    text!(parse_type_system_definition, "nonsense S"),
    "nonsense S"
  );
}
