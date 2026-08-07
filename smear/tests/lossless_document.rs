#![cfg(all(feature = "rowan", feature = "graphql"))]

//! Task 8's gate, part two: the seven type-system extensions, the two document roots, and the
//! top-level recovery point — the one place in this suite where a production's `Err` is caught
//! rather than propagated.
//!
//! **This file is where `has_errors()` becomes trustworthy.** Until Task 8, `trivia::expect`
//! returned `Err` without emitting and `parse_document` bound the driver's result to `_out`, so a
//! parse that failed outright reported *nothing* and read as a success. Task 11's parity gate
//! cannot use `has_errors()` as a verdict until that holds, so it is asserted here directly
//! rather than left as a side effect of the recovery tests.

use smear::parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{Parse, parse_document, parse_type_system_document},
};

/// The tree's node kinds in pre-order, ignoring tokens and trivia.
fn kinds(parse: &Parse) -> Vec<K> {
  parse.syntax().descendants().map(|n| n.kind()).collect()
}

/// Every byte the tree kept, in order — the assertion a boolean verdict cannot fake.
fn text(parse: &Parse) -> String {
  parse.syntax().text().to_string()
}

/// The text of every node of `kind`, in pre-order.
fn texts_of(parse: &Parse, kind: K) -> Vec<String> {
  parse
    .syntax()
    .descendants()
    .filter(|n| n.kind() == kind)
    .map(|n| n.text().to_string())
    .collect()
}

// ---- The seven extensions --------------------------------------------------------------------

#[test]
fn every_extension_kind_is_reachable_and_wraps_its_extend() {
  for (src, want) in [
    (
      "extend scalar S @d",
      vec![
        K::Root,
        K::Document,
        K::ScalarTypeExtension,
        K::Directives,
        K::Directive,
      ],
    ),
    (
      "extend type T implements A",
      vec![
        K::Root,
        K::Document,
        K::ObjectTypeExtension,
        K::ImplementsInterfaces,
        K::NamedType,
      ],
    ),
    (
      "extend interface I { a: Int }",
      vec![
        K::Root,
        K::Document,
        K::InterfaceTypeExtension,
        K::FieldsDefinition,
        K::FieldDefinition,
        K::NamedType,
      ],
    ),
    (
      "extend union U = A",
      vec![
        K::Root,
        K::Document,
        K::UnionTypeExtension,
        K::UnionMemberTypes,
        K::NamedType,
      ],
    ),
    (
      "extend enum E { A }",
      vec![
        K::Root,
        K::Document,
        K::EnumTypeExtension,
        K::EnumValuesDefinition,
        K::EnumValueDefinition,
        K::EnumValue,
      ],
    ),
    (
      "extend input I { a: Int }",
      vec![
        K::Root,
        K::Document,
        K::InputObjectTypeExtension,
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        K::NamedType,
      ],
    ),
    (
      "extend schema { query: Q }",
      vec![
        K::Root,
        K::Document,
        K::SchemaExtension,
        K::RootOperationTypeDefinitions,
        K::RootOperationTypeDefinition,
        K::OperationType,
        K::NamedType,
      ],
    ),
  ] {
    let parse = parse_document(src);
    assert_eq!(kinds(&parse), want, "{src:?}");
    assert!(!parse.has_errors(), "{src:?} must parse cleanly");
    assert_eq!(text(&parse), src, "{src:?} must round-trip verbatim");
  }
}

#[test]
fn an_extension_node_starts_at_its_extend_keyword() {
  // The `extend` is consumed by the dispatcher — the shape keyword after it is what picks the
  // production — so the node is a retro-wrap and its mark must ride at the `extend` itself. A
  // mark minted one token later leaves the kind vector untouched and only shortens the node.
  assert_eq!(
    texts_of(
      &parse_document("  extend scalar S @d"),
      K::ScalarTypeExtension
    ),
    ["extend scalar S @d"]
  );
}

#[test]
fn an_extension_with_no_body_is_reported() {
  // All seven extension shapes require at least one trailing component; `syntactic/`'s
  // `extension.rs` errors on each bare form ("directives or …"), and gate 1 compares verdicts.
  for src in [
    "extend scalar S",
    "extend type T",
    "extend interface I",
    "extend union U",
    "extend enum E",
    "extend input I",
    "extend schema",
  ] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src:?} has no body and must report");
    assert_eq!(text(&parse), src, "{src:?} must still round-trip");
  }
}

#[test]
fn a_description_before_an_extension_is_reported() {
  // `syntactic/`'s `described_definition_after_string` matches only the eight *definition*
  // keywords, so a description followed by `extend` is rejected there. The tree still keeps the
  // extension, because a lossless consumer needs the nodes to point a diagnostic at.
  let parse = parse_document("\"doc\" extend scalar S @d");
  assert!(parse.has_errors(), "a described extension must report");
  assert!(kinds(&parse).contains(&K::ScalarTypeExtension));
  assert_eq!(text(&parse), "\"doc\" extend scalar S @d");
}

// ---- The two document roots --------------------------------------------------------------------

#[test]
fn a_document_mixes_executable_and_type_system_definitions() {
  let src = "query Q { a }\nfragment F on U { b }\ntype T { c: Int }\nextend type T @d\n";
  assert_eq!(
    kinds(&parse_document(src)),
    vec![
      K::Root,
      K::Document,
      K::OperationDefinition,
      K::OperationType,
      K::SelectionSet,
      K::Field,
      K::FragmentDefinition,
      K::NamedType,
      K::SelectionSet,
      K::Field,
      K::ObjectTypeDefinition,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::NamedType,
      K::ObjectTypeExtension,
      K::Directives,
      K::Directive,
    ]
  );
  assert_eq!(text(&parse_document(src)), src);
}

#[test]
fn a_described_executable_definition_is_accepted() {
  // `syntactic/`'s `executable_definition` takes an optional leading string as a frozen-parser
  // compatibility extension. Task 7 could not accept it — `Description` is this task's kind —
  // and gate 1 would have seen the divergence.
  let parse = parse_document("\"doc\" query Q { a }");
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::OperationDefinition,
      K::Description,
      K::OperationType,
      K::SelectionSet,
      K::Field,
    ]
  );
  assert!(!parse.has_errors(), "a described operation is accepted");
  assert_eq!(
    texts_of(&parse, K::OperationDefinition),
    ["\"doc\" query Q { a }"]
  );
}

#[test]
fn an_operation_keyword_is_one_operation_type_node() {
  // Task 7 consumed `query`/`mutation`/`subscription` as a bare `Name` because `OperationType`
  // is this task's kind, and `RootOperationTypeDefinition` needs the same production. Two
  // positions parsing one construct differently is a seam; they are unified here.
  assert_eq!(
    texts_of(&parse_document("query Q { a }"), K::OperationType),
    ["query"]
  );
  assert_eq!(
    texts_of(&parse_document("schema { query: Q }"), K::OperationType),
    ["query"]
  );
  // The shorthand has no keyword and therefore no node.
  assert!(!kinds(&parse_document("{ a }")).contains(&K::OperationType));
}

#[test]
fn a_document_covers_the_whole_file_including_its_trailing_trivia() {
  // The undelimited-repetition law at the top level: the loop's terminating peek crosses the
  // trailing trivia to learn no further definition follows. Here that is not a compromise — a
  // document *is* the whole file, leading and trailing trivia included.
  let src = "\u{feff}# lead\nscalar S\n\n# trail\n";
  assert_eq!(texts_of(&parse_document(src), K::Document), [src]);
  assert_eq!(text(&parse_document(src)), src);
}

#[test]
fn an_empty_document_is_reported() {
  // `syntactic/`'s `document` is `.at_least(1)` — "nonempty".
  let parse = parse_document("");
  assert_eq!(kinds(&parse), vec![K::Root, K::Document]);
  assert!(parse.has_errors(), "an empty document must report");
}

#[test]
fn a_type_system_document_takes_definitions_and_extensions_but_no_operation() {
  let src = "scalar S\nextend scalar S @d\n";
  let parse = parse_type_system_document(src);
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::TypeSystemDocument,
      K::ScalarTypeDefinition,
      K::ScalarTypeExtension,
      K::Directives,
      K::Directive,
    ]
  );
  assert_eq!(text(&parse), src);
  assert!(!parse.has_errors());

  // An executable definition is not a type-system definition.
  let parse = parse_type_system_document("query Q { a }");
  assert!(
    parse.has_errors(),
    "an operation is no type-system definition"
  );
  assert_eq!(text(&parse), "query Q { a }");
}

// ---- The top-level recovery point ---------------------------------------------------------------

#[test]
fn junk_between_two_definitions_costs_one_error_node() {
  // The assertion the plan names: recovery earns its keep only if BOTH definitions survive.
  let src = "type A { x: Int }\n!!!\ntype B { y: Int }";
  let parse = parse_document(src);
  assert_eq!(
    kinds(&parse),
    vec![
      K::Root,
      K::Document,
      K::ObjectTypeDefinition,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::NamedType,
      K::Error,
      K::ObjectTypeDefinition,
      K::FieldsDefinition,
      K::FieldDefinition,
      K::NamedType,
    ]
  );
  assert_eq!(text(&parse), src);
  assert!(parse.has_errors(), "`!!!` is no definition");
}

#[test]
fn a_failed_definition_is_caught_and_the_document_continues() {
  // A production `Err` — here `type` with no name — used to be swallowed by `parse_document`'s
  // `_out`, leaving the remainder uncommitted and `finish` panicking on `UncoveredGap`. The
  // document loop catches it, resynchronises to the next definition keyword, and continues.
  let src = "type { x: Int }\ntype B { y: Int }";
  let parse = parse_document(src);
  assert_eq!(text(&parse), src, "every byte must still reach the tree");
  assert!(parse.has_errors(), "a nameless `type` must report");
  assert!(
    kinds(&parse).contains(&K::ObjectTypeDefinition),
    "the definition after the failure must still be parsed"
  );
  // The wreckage is attributed rather than left as a loose child of `Document`.
  assert_eq!(texts_of(&parse, K::Error), ["{ x: Int }\n"]);
}

#[test]
fn a_resync_that_lands_on_a_definition_head_does_not_eat_it() {
  // `field_definition`'s missing `:` fails standing exactly on `scalar`, so the resync's
  // balanced scan stops having skipped **zero** tokens — the head it was hunting is already at
  // hand. A consume-one fallback copied from `recover::unexpected` fires there and eats the
  // keyword, and the whole `ScalarTypeDefinition` disappears; the text still round-trips, so
  // only a node assertion can see it. Found by mutation, not by reading.
  let src = "type T { a scalar S }";
  let parse = parse_document(src);
  assert_eq!(text(&parse), src);
  assert!(
    kinds(&parse).contains(&K::ScalarTypeDefinition),
    "the definition the resync stopped at must be parsed, not attributed: {:?}",
    kinds(&parse)
  );
}

#[test]
fn a_failed_parse_reports_and_that_is_the_parity_gate_precondition() {
  // `trivia::expect` used to return `Err` without emitting, so every one of these read as a
  // clean parse. Gate 1 compares `has_errors()` against `syntactic/` input by input, so this is
  // the assertion that makes the gate meaningful at all.
  for src in [
    "type",                 // no name
    "type T { a }",         // a field definition with no `: Type`
    "input I { a }",        // an input value with no `: Type`
    "schema",               // no root operation types
    "directive d on FIELD", // no `@`
    "fragment F",           // no type condition and no selection set
  ] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src:?} is malformed and must report");
    assert_eq!(text(&parse), src, "{src:?} must still round-trip");
  }
}

#[test]
fn garbage_running_to_end_of_input_terminates() {
  // No sync point is left, so the balanced skip commits nothing and the loop would spin on the
  // same token forever without the consume-one fallback. A regression here **hangs** rather
  // than fails.
  let src = "scalar S ! ! !";
  let parse = parse_document(src);
  assert_eq!(text(&parse), src);
  assert!(parse.has_errors());
}

#[test]
fn a_stray_closer_at_the_top_level_does_not_stall() {
  // `}` is a sync point for the balanced skip, so the skip matches it at zero cost and makes no
  // progress. Only the consume-one fallback breaks the tie.
  let src = "scalar S } scalar T";
  let parse = parse_document(src);
  assert_eq!(text(&parse), src);
  assert!(parse.has_errors());
  assert_eq!(
    kinds(&parse)
      .into_iter()
      .filter(|k| *k == K::ScalarTypeDefinition)
      .count(),
    2,
    "both scalars must survive the stray closer"
  );
}

#[test]
fn an_unterminated_definition_terminates_and_keeps_its_text() {
  for src in ["type T {", "type T { a: Int", "schema {", "directive @d("] {
    let parse = parse_document(src);
    assert_eq!(text(&parse), src, "{src:?} must round-trip verbatim");
    assert!(
      parse.has_errors(),
      "{src:?} is unterminated and must report"
    );
  }
}

#[test]
fn every_document_form_round_trips_verbatim() {
  for src in [
    "",
    "   ",
    "\u{feff}",
    "# just a comment",
    "query Q { a }\ntype T { b: Int }\n",
    "extend schema @d\nextend type T implements A & B @e { c: Int }\n",
    "\"doc\"\nunion U = |A|B\n\n",
    "!!!",
    "type A { x: Int }\n!!!\ntype B { y: Int }",
    "{ a } { b }",
  ] {
    assert_eq!(
      text(&parse_document(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}
