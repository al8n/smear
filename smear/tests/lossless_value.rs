#![cfg(all(feature = "rowan", feature = "graphql"))]

//! Task 5's gate: the eleven value node kinds, their trivia-invariance, their verbatim
//! round-trip, and the two recovery shapes that keep a malformed value from costing the rest
//! of the file.
//!
//! **These tests drive `value` (and `default_value`) directly, not through `parse_document`.**
//! That is a deliberate departure from the plan's Task 5 Step 1, and it is not a matter of
//! taste: `document` is still Task 3's stub, so nothing under `parse_document` ever reaches a value
//! production. Written the plan's way, `an_object_value_wraps_each_field` would *fail* and
//! `trivia_inside_a_list_does_not_change_its_shape` would *pass vacuously* — both sides of its
//! `assert_eq!` being the one-element `[Root]` of an empty tree — which is the weak-assertion
//! failure mode this plan has now been bitten by twice. The `parse_document` forms are kept below,
//! `#[ignore]`d, so Task 8 has them to switch on.

use smear::parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::value::test_support::{parse_const_value, parse_default_value, parse_value},
};

/// The tree's node kinds in pre-order, ignoring tokens and trivia.
fn node_kinds(src: &str) -> Vec<K> {
  parse_value(src)
    .syntax()
    .descendants()
    .map(|n| n.kind())
    .collect()
}

/// Every byte the tree kept, in order — the assertion a boolean verdict cannot fake.
fn tree_text(src: &str) -> String {
  parse_value(src).syntax().text().to_string()
}

#[test]
fn a_list_value_nests_its_elements() {
  assert_eq!(
    node_kinds("[1, 2]"),
    vec![K::Root, K::ListValue, K::IntValue, K::IntValue]
  );
}

#[test]
fn an_object_value_wraps_each_field() {
  assert_eq!(
    node_kinds("{x: 1}"),
    vec![K::Root, K::ObjectValue, K::ObjectField, K::IntValue]
  );
}

#[test]
fn trivia_around_and_inside_a_list_does_not_change_its_shape() {
  // The trivia atoms' whole purpose, asserted at the production level. Both the expected
  // vector and the pairing are asserted: an equality between two empty trees would agree
  // without either side being right.
  let want = vec![K::Root, K::ListValue, K::IntValue, K::IntValue];
  let padded = "  [ 1 , # c\n 2 ] ";
  assert_eq!(node_kinds("[1, 2]"), want, "the compact form");
  assert_eq!(node_kinds(padded), want, "the trivia-laden form");
  // …and every byte of the padded form is still in the tree.
  assert_eq!(tree_text(padded), padded);
}

#[test]
fn every_value_kind_is_reachable() {
  // A production nobody can reach is a production nobody tests.
  for (src, want) in [
    ("$v", vec![K::Root, K::Variable]),
    ("1", vec![K::Root, K::IntValue]),
    ("1.5", vec![K::Root, K::FloatValue]),
    (r#""s""#, vec![K::Root, K::StringValue]),
    (r#""""s""""#, vec![K::Root, K::StringValue]),
    ("true", vec![K::Root, K::BooleanValue]),
    ("false", vec![K::Root, K::BooleanValue]),
    ("null", vec![K::Root, K::NullValue]),
    ("EN", vec![K::Root, K::EnumValue]),
    ("[1]", vec![K::Root, K::ListValue, K::IntValue]),
    (
      "{k: 1}",
      vec![K::Root, K::ObjectValue, K::ObjectField, K::IntValue],
    ),
  ] {
    assert_eq!(node_kinds(src), want, "{src:?}");
    assert!(!parse_value(src).has_errors(), "{src:?} must parse cleanly");
  }
}

#[test]
fn boolean_null_and_enum_are_told_apart_by_spelling() {
  // The three name-shaped values differ only in their text; the dispatcher must read it.
  assert_eq!(node_kinds("true"), vec![K::Root, K::BooleanValue]);
  assert_eq!(node_kinds("null"), vec![K::Root, K::NullValue]);
  assert_eq!(node_kinds("nullish"), vec![K::Root, K::EnumValue]);
  assert_eq!(node_kinds("truthy"), vec![K::Root, K::EnumValue]);
}

#[test]
fn every_value_form_round_trips_verbatim() {
  for src in [
    "$v",
    "1",
    "-1",
    "1.5",
    r#""s""#,
    r#""""block""""#,
    "true",
    "null",
    "EN",
    "[]",
    "[1, [2], {a: 3}]",
    "{}",
    "{a: 1, b: {c: [true, null]}}",
    "  [ 1 , # trailing comment\n 2 ] ",
    "\u{feff}\t[1]\r\n",
  ] {
    assert_eq!(tree_text(src), src, "{src:?} must round-trip verbatim");
  }
}

#[test]
fn a_default_value_wraps_its_equals_and_value() {
  assert_eq!(
    parse_default_value("= 1")
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<_>>(),
    vec![K::Root, K::DefaultValue, K::IntValue]
  );
  assert_eq!(
    parse_default_value(" = [1] ").syntax().text().to_string(),
    " = [1] "
  );
}

// ---- `Value[Const]`: the flavour threaded through every value production -------------------

/// A `$` in a const position is reported **and still built**.
///
/// **The load-bearing assertion is the third one, not the first.** `has_errors()` alone would be
/// satisfied by a const flavour that *bailed out* on a variable — which is the shape that costs a
/// lossless consumer the very node the diagnostic points at, and which the suite's stated reason
/// for deferring the rule was meant to avoid. So the const parse's node sequence and its text are
/// asserted against the **non-const parse of the same source**: the two trees must be identical,
/// and the verdict must be the only difference between them.
#[test]
fn a_variable_in_a_const_position_is_reported_and_still_built() {
  for (src, want) in [
    ("$v", vec![K::Root, K::Variable]),
    ("[$v]", vec![K::Root, K::ListValue, K::Variable]),
    (
      "{k: $v}",
      vec![K::Root, K::ObjectValue, K::ObjectField, K::Variable],
    ),
    // Padded and mixed, so "the text survived" is a claim that can be wrong: over a
    // whitespace-free witness, comparing against `src` is indistinguishable from comparing
    // against the concatenated token texts.
    (
      " [ 1 , $v ] ",
      vec![K::Root, K::ListValue, K::IntValue, K::Variable],
    ),
    // Nesting: the parameter has to ride all the way down, not just be read at the top.
    (
      "{a: [{b: $v}]}",
      vec![
        K::Root,
        K::ObjectValue,
        K::ObjectField,
        K::ListValue,
        K::ObjectValue,
        K::ObjectField,
        K::Variable,
      ],
    ),
  ] {
    let konst = parse_const_value(src);
    let plain = parse_value(src);

    assert!(
      konst.has_errors(),
      "{src:?}: a variable is not a production of `Value[Const]`"
    );
    assert!(
      !plain.has_errors(),
      "{src:?}: a variable is exactly what an ordinary value position is for"
    );

    let konst_kinds: Vec<K> = konst.syntax().descendants().map(|n| n.kind()).collect();
    let plain_kinds: Vec<K> = plain.syntax().descendants().map(|n| n.kind()).collect();
    assert_eq!(konst_kinds, want, "{src:?}: the const tree lost a node");
    assert_eq!(
      konst_kinds, plain_kinds,
      "{src:?}: constness moved the tree, and it must move only the verdict"
    );
    assert_eq!(
      konst.syntax().text().to_string(),
      src,
      "{src:?}: a rejected const value still keeps every byte"
    );
  }
}

/// A const position accepts everything a value position does, minus the variable.
///
/// The control for the test above: without it, a const flavour that rejected *every* value would
/// satisfy every `has_errors()` assertion there.
#[test]
fn a_const_position_accepts_every_value_that_is_not_a_variable() {
  for src in [
    "1",
    "1.5",
    "\"s\"",
    "\"\"\"b\"\"\"",
    "true",
    "false",
    "null",
    "EV",
    "[1, 2]",
    "{k: 1}",
    "[]",
    "{}",
    "[{k: [1]}]",
  ] {
    let parse = parse_const_value(src);
    assert!(
      !parse.has_errors(),
      "{src:?}: a const position rejected a perfectly good const value"
    );
    assert_eq!(parse.syntax().text().to_string(), src, "{src:?}");
  }
}

/// `DefaultValue` is const in **both** the positions the grammar puts it in, so its production
/// takes no flavour argument and this is the only test that can say so.
#[test]
fn a_default_value_is_const_wherever_it_appears() {
  let rejected = parse_default_value(" = $v ");
  assert!(
    rejected.has_errors(),
    "`DefaultValue` takes a `Value[Const]`"
  );
  assert_eq!(
    rejected
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<_>>(),
    vec![K::Root, K::DefaultValue, K::Variable],
    "the `Variable` node is built anyway"
  );
  assert_eq!(rejected.syntax().text().to_string(), " = $v ");

  assert!(
    !parse_default_value(" = 1 ").has_errors(),
    "a const default value is fine"
  );
}

// ---- Recovery: the two delimited shapes, and the termination law ------------------------
//
// Each of these inputs is one the plan's Step 4 names. They are timing-sensitive by nature: a
// recovery helper that returns `Ok` without consuming turns its caller's `while` into an
// infinite loop, so a regression here hangs rather than fails. That is exactly why each helper
// gets its own input.

#[test]
fn an_unterminated_list_terminates_and_keeps_its_text() {
  let src = "[1, 2";
  assert_eq!(
    node_kinds(src),
    vec![K::Root, K::ListValue, K::IntValue, K::IntValue]
  );
  assert_eq!(tree_text(src), src);
  assert!(
    parse_value(src).has_errors(),
    "an unclosed `[` must be reported"
  );
}

#[test]
fn an_unterminated_object_terminates_and_keeps_its_text() {
  let src = "{a: 1";
  assert_eq!(
    node_kinds(src),
    vec![K::Root, K::ObjectValue, K::ObjectField, K::IntValue]
  );
  assert_eq!(tree_text(src), src);
  assert!(
    parse_value(src).has_errors(),
    "an unclosed `{{` must be reported"
  );
}

#[test]
fn garbage_inside_a_list_becomes_an_error_node_and_the_list_continues() {
  let src = "[ ! ]";
  // The `!` is attributed — it is inside an `Error` node, not silently a child of the list —
  // and the list still closes on its own `]`.
  assert_eq!(node_kinds(src), vec![K::Root, K::ListValue, K::Error]);
  assert_eq!(tree_text(src), src);
  assert!(parse_value(src).has_errors(), "`!` is not a value");

  // …and the elements on either side of the garbage still parse.
  let src = "[1 ! 2]";
  assert_eq!(
    node_kinds(src),
    vec![K::Root, K::ListValue, K::IntValue, K::Error, K::IntValue]
  );
  assert_eq!(tree_text(src), src);
}

#[test]
fn garbage_running_to_end_of_input_still_terminates() {
  // The case that makes `sync_balanced` alone insufficient: there is no sync point left, so
  // the balanced skip commits nothing and the caller would spin forever on the same token.
  let src = "[1 ! ! !";
  assert_eq!(
    node_kinds(src),
    vec![
      K::Root,
      K::ListValue,
      K::IntValue,
      K::Error,
      K::Error,
      K::Error
    ]
  );
  assert_eq!(tree_text(src), src);
}

#[test]
fn a_stray_closer_inside_a_list_does_not_stall() {
  // `)` is a sync point for the balanced skip, so the skip matches it at zero cost and makes
  // no progress. Only the consume-one fallback breaks the tie.
  let src = "[1 ) 2]";
  assert_eq!(
    node_kinds(src),
    vec![K::Root, K::ListValue, K::IntValue, K::Error, K::IntValue]
  );
  assert_eq!(tree_text(src), src);
}

#[test]
fn garbage_inside_an_object_becomes_an_error_node_and_the_object_continues() {
  let src = "{a: 1 ! b: 2}";
  assert_eq!(
    node_kinds(src),
    vec![
      K::Root,
      K::ObjectValue,
      K::ObjectField,
      K::IntValue,
      K::Error,
      K::ObjectField,
      K::IntValue
    ]
  );
  assert_eq!(tree_text(src), src);
}

#[test]
fn nested_garbage_is_skipped_as_one_region() {
  // `sync_balanced`'s nesting awareness: the `(` … `)` run is crossed whole rather than
  // stopping at the `1` inside it, and the whole run becomes **one** `Error` node rather than
  // one per token. Asserting the node's text, not just its presence, is what makes "one
  // region" a claim the tree can refute.
  let src = "[! (1) 2]";
  assert_eq!(
    node_kinds(src),
    vec![K::Root, K::ListValue, K::Error, K::IntValue]
  );
  let error_text: Vec<String> = parse_value(src)
    .syntax()
    .descendants()
    .filter(|n| n.kind() == K::Error)
    .map(|n| n.text().to_string())
    .collect();
  assert_eq!(error_text, vec!["! (1) ".to_string()]);
  assert_eq!(tree_text(src), src);
}

// ---- The plan's own Task 5 Step 1 tests, switched on in Task 8 ---------------------------
//
// These are the plan's four tests verbatim in intent. They were `#[ignore]`d through Tasks 5
// to 7, because `parse_document` drove only the drain-everything `document` stub and every
// `node_kinds` answered `[Root]` — which would have failed two of them and, worse, passed the
// other two *vacuously*, both sides of the trivia comparison being the same empty tree.
//
// Task 8 gives `document` a body, so they are live. **Their node vectors needed no change**:
// the plan predicted `[Root, Document, OperationDefinition, SelectionSet, Field, …]` and that
// is what the shipped productions build — a shorthand operation carries no `OperationType`
// node, so the kind this task added does not appear here.

#[cfg(test)]
mod through_parse_document {
  use super::K;
  use smear::parser::graphql::lossless::parse_document;

  fn node_kinds(src: &str) -> Vec<K> {
    parse_document(src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect()
  }

  #[test]
  fn a_list_value_nests_its_elements() {
    assert_eq!(
      node_kinds("{ f(a: [1, 2]) }"),
      vec![
        K::Root,
        K::Document,
        K::OperationDefinition,
        K::SelectionSet,
        K::Field,
        K::Arguments,
        K::Argument,
        K::ListValue,
        K::IntValue,
        K::IntValue,
      ]
    );
  }

  #[test]
  fn an_object_value_wraps_each_field() {
    assert_eq!(
      node_kinds("{ f(a: {x: 1}) }"),
      vec![
        K::Root,
        K::Document,
        K::OperationDefinition,
        K::SelectionSet,
        K::Field,
        K::Arguments,
        K::Argument,
        K::ObjectValue,
        K::ObjectField,
        K::IntValue,
      ]
    );
  }

  #[test]
  fn trivia_inside_a_list_does_not_change_its_shape() {
    assert_eq!(
      node_kinds("{ f(a: [1, 2]) }"),
      node_kinds("{ f(a: [ 1 , # c\n 2 ]) }")
    );
  }

  #[test]
  fn every_value_kind_is_reachable() {
    let src = r#"{ f(a: $v, b: 1, c: 1.5, d: "s", e: true, g: null, h: EN, i: [1], j: {k: 1}) }"#;
    let got = node_kinds(src);
    for want in [
      K::Variable,
      K::IntValue,
      K::FloatValue,
      K::StringValue,
      K::BooleanValue,
      K::NullValue,
      K::EnumValue,
      K::ListValue,
      K::ObjectValue,
      K::ObjectField,
    ] {
      assert!(got.contains(&want), "{want:?} was not produced by {src:?}");
    }
  }
}
