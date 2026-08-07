#![cfg(all(feature = "rowan", feature = "graphql"))]

//! Task 6's gate: the seven type-reference, argument and directive node kinds, their
//! trivia-invariance, their verbatim round-trip, and the recovery shapes that keep a malformed
//! type or argument list from costing the rest of the file.
//!
//! **These tests drive the productions directly, not through `parse_document`.** `document` is
//! still Task 3's stub, so nothing under `parse_document` reaches a type or a directive yet;
//! written the other way every assertion here would compare one empty tree against another and
//! pass without a single production existing. That is the vacuous-assertion failure mode Task 5
//! recorded, and the reason `value.rs` grew its own drivers first.
//!
//! Every assertion is on the node-kind sequence, the node's own text, or both — never on a bare
//! boolean, which is the shape three earlier mutations survived.

use smear::parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    Parse,
    directive::test_support::{
      parse_argument, parse_arguments, parse_const_directives, parse_directive, parse_directives,
    },
    ty::test_support::parse_type_ref,
  },
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
/// mark placed one token too early shows up.
fn texts_of(parse: &Parse, kind: K) -> Vec<String> {
  parse
    .syntax()
    .descendants()
    .filter(|n| n.kind() == kind)
    .map(|n| n.text().to_string())
    .collect()
}

// ---- Type references ---------------------------------------------------------------------

#[test]
fn a_named_type_is_one_node() {
  assert_eq!(kinds(&parse_type_ref("Int")), vec![K::Root, K::NamedType]);
  assert_eq!(texts_of(&parse_type_ref("Int"), K::NamedType), ["Int"]);
}

#[test]
fn a_list_type_nests_its_element() {
  assert_eq!(
    kinds(&parse_type_ref("[Int]")),
    vec![K::Root, K::ListType, K::NamedType]
  );
  assert_eq!(
    kinds(&parse_type_ref("[[Int]]")),
    vec![K::Root, K::ListType, K::ListType, K::NamedType]
  );
}

#[test]
fn a_bang_retro_wraps_the_type_it_follows() {
  assert_eq!(
    kinds(&parse_type_ref("Int!")),
    vec![K::Root, K::NonNullType, K::NamedType]
  );
  assert_eq!(
    kinds(&parse_type_ref("[Int]!")),
    vec![K::Root, K::NonNullType, K::ListType, K::NamedType]
  );
  assert_eq!(
    kinds(&parse_type_ref("[Int!]!")),
    vec![
      K::Root,
      K::NonNullType,
      K::ListType,
      K::NonNullType,
      K::NamedType
    ]
  );
  // …and the wrap covers the type it follows, not merely the `!`.
  assert_eq!(
    texts_of(&parse_type_ref("[Int!]!"), K::NonNullType),
    ["[Int!]!", "Int!"]
  );
}

#[test]
fn no_bang_means_no_non_null_node() {
  // A retro-wrap that always fires is as wrong as one that never does.
  for src in ["Int", "[Int]", "[[Int]]"] {
    assert!(
      !kinds(&parse_type_ref(src)).contains(&K::NonNullType),
      "{src:?} has no `!` and must produce no NonNullType"
    );
  }
}

#[test]
fn trivia_does_not_change_a_type_reference_shape() {
  // The trivia atoms' whole purpose, asserted at the production level — and the one shape in
  // this task that depends on crossing trivia *between* two tokens of the same node, since the
  // `!` is decided after the inner type has already been committed.
  let want = vec![
    K::Root,
    K::NonNullType,
    K::ListType,
    K::NonNullType,
    K::NamedType,
  ];
  let padded = "  [ Int ! ] ! ";
  assert_eq!(kinds(&parse_type_ref("[Int!]!")), want, "the compact form");
  assert_eq!(
    kinds(&parse_type_ref(padded)),
    want,
    "the trivia-laden form"
  );
  // Every byte survives…
  assert_eq!(text(&parse_type_ref(padded)), padded);
  // …and the leading trivia stays OUTSIDE the retro-wrap: the mark is minted after the head
  // peek, so a `NonNullType` starts at its own first token rather than at the previous one's
  // trailing whitespace.
  assert_eq!(
    texts_of(&parse_type_ref(padded), K::NonNullType),
    ["[ Int ! ] !", "Int !"]
  );
}

#[test]
fn every_type_reference_form_round_trips_verbatim() {
  for src in [
    "Int",
    "Int!",
    "[Int]",
    "[Int!]!",
    "[[String!]!]!",
    "  [ Int ! ] ! ",
    "[Int # a comment\n]",
    "\u{feff}\t[Int]\r\n",
  ] {
    assert_eq!(
      text(&parse_type_ref(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}

// ---- Arguments ---------------------------------------------------------------------------

#[test]
fn an_argument_pairs_a_name_with_a_value() {
  assert_eq!(
    kinds(&parse_argument("a: 1")),
    vec![K::Root, K::Argument, K::IntValue]
  );
  assert_eq!(texts_of(&parse_argument("a: 1"), K::Argument), ["a: 1"]);
}

#[test]
fn an_arguments_list_wraps_each_argument() {
  assert_eq!(
    kinds(&parse_arguments("(a: 1, b: $v)")),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Argument,
      K::Variable
    ]
  );
}

#[test]
fn an_empty_argument_list_is_accepted() {
  // `syntactic/`'s `arguments` documents the lenient `()` spelling as accepted, and gate 1
  // compares the two suites' verdicts input by input — so this suite must accept it too, even
  // though the spec's `Arguments` production says `+` and `apollo-parser` reports it.
  let parse = parse_arguments("()");
  assert_eq!(kinds(&parse), vec![K::Root, K::Arguments]);
  assert!(!parse.has_errors(), "`()` must not be an error here");
}

#[test]
fn trivia_does_not_change_an_argument_list_shape() {
  let want = vec![K::Root, K::Arguments, K::Argument, K::IntValue];
  let padded = "  ( # why\n a : 1 , ) ";
  assert_eq!(kinds(&parse_arguments("(a:1)")), want, "the compact form");
  assert_eq!(
    kinds(&parse_arguments(padded)),
    want,
    "the trivia-laden form"
  );
  assert_eq!(text(&parse_arguments(padded)), padded);
}

// ---- Directives --------------------------------------------------------------------------

#[test]
fn a_directive_carries_its_name_and_arguments() {
  assert_eq!(
    kinds(&parse_directive("@include(if: true)")),
    vec![
      K::Root,
      K::Directive,
      K::Arguments,
      K::Argument,
      K::BooleanValue
    ]
  );
  assert_eq!(
    kinds(&parse_directive("@deprecated")),
    vec![K::Root, K::Directive]
  );
}

#[test]
fn a_run_of_directives_is_one_node() {
  assert_eq!(
    kinds(&parse_directives("@a @b(x: 1)")),
    vec![
      K::Root,
      K::Directives,
      K::Directive,
      K::Directive,
      K::Arguments,
      K::Argument,
      K::IntValue
    ]
  );
}

#[test]
fn no_directive_means_no_directives_node() {
  // An empty `Directives` node is a lie the typed layer cannot see past — and, worse, it would
  // swallow the leading trivia of whatever follows it.
  assert_eq!(kinds(&parse_directives("  ")), vec![K::Root]);
  assert_eq!(kinds(&parse_directives("x")), vec![K::Root]);
  assert_eq!(text(&parse_directives("  ")), "  ");

  // The leading trivia before a real directive stays outside the node too, because the head
  // peek crosses it before the node is opened.
  //
  // The **trailing** trivia does not, and that is forced rather than chosen: the loop's
  // terminating peek has to cross it to learn there is no further `@`, and it crosses it while
  // the node is still open — a node always ends at the current position, there being no
  // `cst_finish_at`. Every *undelimited* repetition inherits this; `Arguments` has no such tail
  // because its `)` closes the node. Asserted rather than glossed, so a later trivia-rebalancing
  // pass shows up here instead of silently moving node boundaries.
  let parse = parse_directives("  @a @b ");
  assert_eq!(texts_of(&parse, K::Directives), ["@a @b "]);
  assert_eq!(text(&parse), "  @a @b ");
}

#[test]
fn every_directive_form_round_trips_verbatim() {
  for src in [
    "@a",
    "@a(x: 1)",
    "@a @b",
    "  @a( x : 1 )  @b  ",
    "@a # trailing\n@b",
  ] {
    assert_eq!(
      text(&parse_directives(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}

// ---- `Directives[Const]`: the same node shape, a different verdict -------------------------

/// A `Variable` in a **const** directive's arguments is reported and the tree is untouched.
///
/// `Directives[Const]` is reached from every SDL position and from a `VariableDefinition`, and
/// the flavour rides in as an argument rather than forking the production — so the only thing
/// that can distinguish the two entry points is the verdict. That is what is asserted: the const
/// parse and the ordinary parse of the same bytes must produce the **same node sequence and the
/// same text**, and disagree on `has_errors()` alone.
///
/// Asserting the boolean by itself would be satisfied by a const flavour that bailed out on the
/// `$` and dropped the `Directive`, `Arguments` and `Argument` nodes with it.
#[test]
fn a_variable_in_a_const_directive_argument_is_reported_and_still_built() {
  for (src, want) in [
    (
      "@d(a: $v)",
      vec![
        K::Root,
        K::Directives,
        K::Directive,
        K::Arguments,
        K::Argument,
        K::Variable,
      ],
    ),
    // Nested inside a list inside an object, and padded — the parameter has to reach the leaf,
    // and "the text survived" has to be a claim that could be wrong.
    (
      "  @d(a: {k: [$v]}) @e  ",
      vec![
        K::Root,
        K::Directives,
        K::Directive,
        K::Arguments,
        K::Argument,
        K::ObjectValue,
        K::ObjectField,
        K::ListValue,
        K::Variable,
        K::Directive,
      ],
    ),
  ] {
    let konst = parse_const_directives(src);
    let plain = parse_directives(src);

    assert!(
      konst.has_errors(),
      "{src:?}: a variable is not a production of `Value[Const]`"
    );
    assert!(
      !plain.has_errors(),
      "{src:?}: an executable directive position takes variables"
    );
    assert_eq!(kinds(&konst), want, "{src:?}: the const tree lost a node");
    assert_eq!(
      kinds(&konst),
      kinds(&plain),
      "{src:?}: constness moved the tree, and it must move only the verdict"
    );
    assert_eq!(
      text(&konst),
      src,
      "{src:?}: every byte survives a rejection"
    );
  }
}

/// The control: a const directive position accepts everything that is not a variable.
///
/// Without it, a `const_directives` that rejected every argument would satisfy every
/// `has_errors()` assertion above.
#[test]
fn a_const_directive_position_accepts_every_argument_that_is_not_a_variable() {
  for src in [
    "@d",
    "@d()",
    "@d(a: 1)",
    "@d(a: [1], b: {k: \"s\"}, c: null) @e(x: EV)",
  ] {
    let parse = parse_const_directives(src);
    assert!(
      !parse.has_errors(),
      "{src:?}: a const directive position rejected a perfectly good const argument"
    );
    assert_eq!(text(&parse), src, "{src:?}");
  }
}

// ---- Recovery, and the termination law ---------------------------------------------------
//
// Every input below is one a recovery helper must make progress on. A helper that returns `Ok`
// without consuming turns its caller's loop into an infinite loop, so a regression here **hangs
// rather than fails** — which is why each one gets its own named test.

#[test]
fn garbage_in_a_type_position_becomes_an_error_node() {
  let src = "[!]";
  assert_eq!(
    kinds(&parse_type_ref(src)),
    vec![K::Root, K::ListType, K::Error]
  );
  assert_eq!(text(&parse_type_ref(src)), src);
  assert!(parse_type_ref(src).has_errors(), "`!` is not a type");
}

#[test]
fn junk_between_a_list_element_and_its_closer_is_attributed() {
  let src = "[Int Foo]";
  assert_eq!(
    kinds(&parse_type_ref(src)),
    vec![K::Root, K::ListType, K::NamedType, K::Error]
  );
  assert_eq!(texts_of(&parse_type_ref(src), K::Error), ["Foo"]);
  assert_eq!(text(&parse_type_ref(src)), src);
}

#[test]
fn an_unterminated_list_type_terminates_and_keeps_its_text() {
  let src = "[Int";
  assert_eq!(
    kinds(&parse_type_ref(src)),
    vec![K::Root, K::ListType, K::NamedType]
  );
  assert_eq!(text(&parse_type_ref(src)), src);
  assert!(
    parse_type_ref(src).has_errors(),
    "an unclosed `[` must be reported"
  );
}

#[test]
fn garbage_inside_an_argument_list_becomes_an_error_node_and_the_list_continues() {
  let src = "(a: 1 ! b: 2)";
  assert_eq!(
    kinds(&parse_arguments(src)),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Error,
      K::Argument,
      K::IntValue
    ]
  );
  assert_eq!(text(&parse_arguments(src)), src);
}

#[test]
fn an_unterminated_argument_list_terminates_and_keeps_its_text() {
  let src = "(a: 1";
  assert_eq!(
    kinds(&parse_arguments(src)),
    vec![K::Root, K::Arguments, K::Argument, K::IntValue]
  );
  assert_eq!(text(&parse_arguments(src)), src);
  assert!(
    parse_arguments(src).has_errors(),
    "an unclosed `(` must be reported"
  );
}

#[test]
fn a_stray_closer_inside_an_argument_list_does_not_stall() {
  // `]` is a sync point for the balanced skip, so the skip matches it at zero cost and makes no
  // progress. Only the consume-one fallback breaks the tie.
  let src = "(a: 1 ] b: 2)";
  assert_eq!(
    kinds(&parse_arguments(src)),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Error,
      K::Argument,
      K::IntValue
    ]
  );
  assert_eq!(text(&parse_arguments(src)), src);
}

#[test]
fn garbage_running_to_end_of_input_inside_an_argument_list_terminates() {
  // No sync point is left, so the balanced skip commits nothing and the caller would spin on
  // the same token forever without the fallback.
  let src = "(a: 1 ! ! !";
  assert_eq!(
    kinds(&parse_arguments(src)),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Error,
      K::Error,
      K::Error
    ]
  );
  assert_eq!(text(&parse_arguments(src)), src);
}
