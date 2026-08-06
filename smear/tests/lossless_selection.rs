#![cfg(feature = "rowan")]

//! Task 7's gate: the ten selection and executable-definition node kinds, their
//! trivia-invariance, their verbatim round-trip, and the recovery shapes that keep a malformed
//! selection or definition from costing the rest of the file.
//!
//! **These tests drive the productions directly, not through `parse_document`.** `document` is
//! still Task 3's stub, so nothing under `parse_document` reaches a selection yet; written the
//! other way every assertion here would compare one empty tree against another and pass without a
//! single production existing. That is the vacuous-assertion failure mode Task 5 recorded, and
//! the reason `value.rs` grew its own drivers first.
//!
//! Every assertion is on the node-kind sequence, the node's own text, or both — never on a bare
//! boolean, which is the shape four earlier mutations survived. The `Alias` retro-wrap in
//! particular is asserted on **text**: a mark placed one token late leaves the kind sequence
//! untouched and only the node's extent moves.

use smear::parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    Parse,
    executable::test_support::{
      parse_fragment_definition, parse_operation_definition, parse_variables_definition,
    },
    parse_executable_document,
    selection::test_support::{parse_field, parse_selection, parse_selection_set},
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
/// mark placed one token too late shows up.
fn texts_of(parse: &Parse, kind: K) -> Vec<String> {
  parse
    .syntax()
    .descendants()
    .filter(|n| n.kind() == kind)
    .map(|n| n.text().to_string())
    .collect()
}

// ---- Fields and the alias retro-wrap ------------------------------------------------------

#[test]
fn a_field_is_one_node() {
  assert_eq!(kinds(&parse_field("a")), vec![K::Root, K::Field]);
  assert_eq!(texts_of(&parse_field("a"), K::Field), ["a"]);
}

#[test]
fn a_colon_retro_wraps_the_name_before_it_as_an_alias() {
  assert_eq!(
    kinds(&parse_field("a: b")),
    vec![K::Root, K::Field, K::Alias]
  );
  // The wrap covers the name AND the colon — not merely the colon. A mark minted one token
  // late leaves the kind vector above unchanged and only moves this extent.
  assert_eq!(texts_of(&parse_field("a: b"), K::Alias), ["a:"]);
}

#[test]
fn no_colon_means_no_alias_node() {
  // A retro-wrap that always fires is as wrong as one that never does.
  for src in ["a", "a(x: 1)", "a @d", "a { b }"] {
    assert!(
      !kinds(&parse_field(src)).contains(&K::Alias),
      "{src:?} has no `:` and must produce no Alias"
    );
  }
}

#[test]
fn trivia_between_a_name_and_its_colon_still_makes_an_alias() {
  // The probe's own skip, asserted where it is load-bearing: the `:` is decided after the name
  // is already committed, so `try_eat` is the only atom that can cross what sits between them.
  // Driven through `selection_set` so the field's leading trivia is crossed by the loop's
  // dispatch peek, exactly as it is in a real document.
  let want = vec![K::Root, K::SelectionSet, K::Field, K::Alias];
  assert_eq!(kinds(&parse_selection_set("{ a : b }")), want);
  assert_eq!(kinds(&parse_selection_set("{ a # c\n: b }")), want);
  assert_eq!(
    texts_of(&parse_selection_set("{ a # c\n: b }"), K::Alias),
    ["a # c\n:"]
  );
}

#[test]
fn a_field_carries_its_arguments_directives_and_selection_set() {
  assert_eq!(
    kinds(&parse_field("a(x: 1) @d { b }")),
    vec![
      K::Root,
      K::Field,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Directives,
      K::Directive,
      K::SelectionSet,
      K::Field
    ]
  );
}

// ---- Selection sets ------------------------------------------------------------------------

#[test]
fn a_selection_set_wraps_each_selection() {
  assert_eq!(
    kinds(&parse_selection_set("{ a b }")),
    vec![K::Root, K::SelectionSet, K::Field, K::Field]
  );
}

#[test]
fn a_field_ends_with_its_trailing_trivia_inside_it() {
  // The law Task 6's `Directives` established, inherited here: a `Field`'s last three
  // components are all optional, so deciding they are absent means crossing whatever follows
  // the field — while the field's node is still open. A node's extent is `[mark, now]` and
  // `cst_finish` has no `_at` form, so a node cannot be ended in the past. Text fidelity is
  // untouched; only `text_range` is one trivia run longer.
  //
  // Asserted rather than glossed, so a later trivia-rebalancing pass shows up here instead of
  // silently moving node boundaries.
  //
  // Note the SECOND field: the law does not stop at the last member of a run. `b`'s optional
  // tails are decided by peeks that cross the space before the `}` — which belongs to the
  // enclosing set by every reading — so `b` too ends one trivia run long. That is the same
  // forcing, not a separate defect, and no production can narrow it without a lookahead the
  // atom set deliberately does not have.
  assert_eq!(
    texts_of(&parse_selection_set("{ a b }"), K::Field),
    ["a ", "b "]
  );
  // A *delimited* tail is immune: the closer ends the node before anything after it is read.
  assert_eq!(
    texts_of(&parse_selection_set("{ a { b } c }"), K::SelectionSet),
    ["{ a { b } c }", "{ b }"]
  );
}

#[test]
fn an_empty_selection_set_is_reported() {
  // The spec's `SelectionSet: { Selection+ }`, and `syntactic/` rejects `{}` explicitly
  // ("one-or-more, so an empty `{}` errors"). Gate 1 compares the two suites' verdicts input by
  // input, so this suite must reject it too — the opposite ruling from `Arguments`, where
  // `syntactic/` documents the lenient `()` as accepted and Task 6 followed it.
  let parse = parse_selection_set("{}");
  assert_eq!(kinds(&parse), vec![K::Root, K::SelectionSet]);
  assert_eq!(text(&parse), "{}");
  assert!(
    parse.has_errors(),
    "`{{}}` has no selection and must report"
  );
}

#[test]
fn trivia_does_not_change_a_selection_set_shape() {
  let want = vec![K::Root, K::SelectionSet, K::Field, K::Alias, K::Field];
  let padded = "  { # why\n a : b , c } ";
  assert_eq!(
    kinds(&parse_selection_set("{a:b c}")),
    want,
    "the compact form"
  );
  assert_eq!(kinds(&parse_selection_set(padded)), want, "the padded form");
  assert_eq!(text(&parse_selection_set(padded)), padded);
}

// ---- Fragment spreads and inline fragments -------------------------------------------------

#[test]
fn a_fragment_spread_is_the_spread_and_its_name() {
  assert_eq!(
    kinds(&parse_selection("...F")),
    vec![K::Root, K::FragmentSpread]
  );
  assert_eq!(
    kinds(&parse_selection("...F @d")),
    vec![K::Root, K::FragmentSpread, K::Directives, K::Directive]
  );
  // The `...` is inside the node the retro-wrap creates, not a loose sibling of it.
  assert_eq!(
    texts_of(&parse_selection("  ...F"), K::FragmentSpread),
    ["...F"]
  );
}

#[test]
fn an_inline_fragment_carries_its_type_condition() {
  assert_eq!(
    kinds(&parse_selection("... on User { a }")),
    vec![
      K::Root,
      K::InlineFragment,
      K::NamedType,
      K::SelectionSet,
      K::Field
    ]
  );
  // `on` is a contextual keyword and the type condition has no node kind of its own, so the
  // condition surfaces as the `NamedType` after the keyword.
  assert_eq!(
    texts_of(&parse_selection("... on User { a }"), K::NamedType),
    ["User"]
  );
}

#[test]
fn an_inline_fragment_may_have_no_type_condition() {
  assert_eq!(
    kinds(&parse_selection("... { a }")),
    vec![K::Root, K::InlineFragment, K::SelectionSet, K::Field]
  );
  assert_eq!(
    kinds(&parse_selection("... @d { a }")),
    vec![
      K::Root,
      K::InlineFragment,
      K::Directives,
      K::Directive,
      K::SelectionSet,
      K::Field
    ]
  );
}

#[test]
fn a_name_that_is_not_on_is_a_fragment_spread() {
  // The whole `...`-head ambiguity, both ways: only the spelling `on` makes an inline fragment.
  assert!(kinds(&parse_selection("...only")).contains(&K::FragmentSpread));
  assert!(!kinds(&parse_selection("...only")).contains(&K::InlineFragment));
  assert!(kinds(&parse_selection("... on X { a }")).contains(&K::InlineFragment));
}

#[test]
fn every_selection_form_round_trips_verbatim() {
  for src in [
    "{ a }",
    "{ a b }",
    "{ a: b }",
    "{ a(x: 1) @d { b } }",
    "{ ...F }",
    "{ ... on U { a } }",
    "  { # c\n a , b }  ",
    "\u{feff}{a}\r\n",
  ] {
    assert_eq!(
      text(&parse_selection_set(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}

// ---- Variable definitions ------------------------------------------------------------------

#[test]
fn a_variable_definition_pairs_a_variable_with_a_type() {
  assert_eq!(
    kinds(&parse_variables_definition("($a: Int! = 1 @d)")),
    vec![
      K::Root,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::Variable,
      K::NonNullType,
      K::NamedType,
      K::DefaultValue,
      K::IntValue,
      K::Directives,
      K::Directive
    ]
  );
}

#[test]
fn a_variables_definition_wraps_each_definition() {
  assert_eq!(
    kinds(&parse_variables_definition("($a: Int, $b: [String])")),
    vec![
      K::Root,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::Variable,
      K::NamedType,
      K::VariableDefinition,
      K::Variable,
      K::ListType,
      K::NamedType
    ]
  );
}

#[test]
fn an_empty_variables_definition_is_reported() {
  // `syntactic/` rejects the empty `()` here ("one-or-more, so an empty `()` errors"), unlike
  // its `Arguments`, which accepts it. Gate 1 compares verdicts per input, so the two rulings
  // are followed one production at a time rather than unified.
  let parse = parse_variables_definition("()");
  assert_eq!(kinds(&parse), vec![K::Root, K::VariablesDefinition]);
  assert_eq!(text(&parse), "()");
  assert!(parse.has_errors(), "`()` has no variable and must report");
}

// ---- Operation and fragment definitions ----------------------------------------------------

#[test]
fn a_shorthand_operation_is_just_a_selection_set() {
  assert_eq!(
    kinds(&parse_operation_definition("{ a }")),
    vec![K::Root, K::OperationDefinition, K::SelectionSet, K::Field]
  );
}

#[test]
fn a_named_operation_carries_its_name_variables_and_directives() {
  // `K::OperationType` arrived in Task 8, which unified this position with
  // `RootOperationTypeDefinition`'s on one production. Task 7 consumed the keyword as a bare
  // `Name` because the kind was not yet its to spend; two positions parsing one construct
  // differently is the seam bugs live in, so the vectors below gained the node rather than the
  // production keeping the divergence.
  assert_eq!(
    kinds(&parse_operation_definition("query Q($a: Int) @d { b }")),
    vec![
      K::Root,
      K::OperationDefinition,
      K::OperationType,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::Variable,
      K::NamedType,
      K::Directives,
      K::Directive,
      K::SelectionSet,
      K::Field
    ]
  );
  // Every operation keyword reaches the same production.
  for src in ["query { a }", "mutation { a }", "subscription { a }"] {
    assert_eq!(
      kinds(&parse_operation_definition(src)),
      vec![
        K::Root,
        K::OperationDefinition,
        K::OperationType,
        K::SelectionSet,
        K::Field
      ],
      "{src:?}"
    );
  }
  // The shorthand has no keyword, so it has no `OperationType` node — the retro-wrap must not
  // fire where there is nothing to wrap.
  assert!(!kinds(&parse_operation_definition("{ a }")).contains(&K::OperationType));
}

#[test]
fn a_fragment_definition_carries_its_type_condition() {
  assert_eq!(
    kinds(&parse_fragment_definition("fragment F on User @d { a }")),
    vec![
      K::Root,
      K::FragmentDefinition,
      K::NamedType,
      K::Directives,
      K::Directive,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(
    texts_of(
      &parse_fragment_definition("fragment F on User { a }"),
      K::NamedType
    ),
    ["User"]
  );
}

#[test]
fn an_executable_document_holds_every_definition() {
  let src = "query Q { a }\nfragment F on U { b }\n{ c }";
  assert_eq!(
    kinds(&parse_executable_document(src)),
    vec![
      K::Root,
      K::ExecutableDocument,
      K::OperationDefinition,
      K::OperationType,
      K::SelectionSet,
      K::Field,
      K::FragmentDefinition,
      K::NamedType,
      K::SelectionSet,
      K::Field,
      K::OperationDefinition,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(text(&parse_executable_document(src)), src);
  // A definition that ends on its own closer is immune to the trailing-trivia law: the `}`
  // ends `OperationDefinition` before the newline after it is read.
  assert_eq!(
    texts_of(&parse_executable_document(src), K::OperationDefinition),
    ["query Q { a }", "{ c }"]
  );
}

#[test]
fn an_empty_executable_document_is_reported() {
  // `syntactic/` rejects an empty input ("one-or-more, so an empty input errors").
  let parse = parse_executable_document("");
  assert_eq!(kinds(&parse), vec![K::Root, K::ExecutableDocument]);
  assert!(parse.has_errors(), "an empty document must report");
}

#[test]
fn every_executable_form_round_trips_verbatim() {
  for src in [
    "query Q($a: Int! = 1) @d { a { b } }",
    "fragment F on U @d { a }",
    "{ a }",
    "query { a }\n\n# trailing\n",
    "\u{feff}mutation M { a }\r\n",
  ] {
    assert_eq!(
      text(&parse_executable_document(src)),
      src,
      "{src:?} must round-trip verbatim"
    );
  }
}

// ---- Recovery, and the termination law -----------------------------------------------------
//
// Every input below is one a recovery helper must make progress on. A helper that returns `Ok`
// without consuming turns its caller's loop into an infinite loop, so a regression here **hangs
// rather than fails** — which is why each one gets its own named test.

#[test]
fn junk_inside_a_selection_set_becomes_an_error_node() {
  let src = "{ a ! b }";
  assert_eq!(
    kinds(&parse_selection_set(src)),
    vec![K::Root, K::SelectionSet, K::Field, K::Error, K::Field]
  );
  assert_eq!(text(&parse_selection_set(src)), src);
  assert!(parse_selection_set(src).has_errors(), "`!` is no selection");
}

#[test]
fn junk_before_a_spread_does_not_swallow_it() {
  // `...` is a selection head, so a value-position sync set that stops only at names would
  // skip straight past it and turn the fragment spread into junk. This is the assertion that
  // pays for widening `recover::is_sync_point`.
  let src = "{ a ! ...F }";
  assert_eq!(
    kinds(&parse_selection_set(src)),
    vec![
      K::Root,
      K::SelectionSet,
      K::Field,
      K::Error,
      K::FragmentSpread
    ]
  );
  assert_eq!(text(&parse_selection_set(src)), src);
}

#[test]
fn an_unterminated_selection_set_terminates_and_keeps_its_text() {
  let src = "{ a";
  assert_eq!(
    kinds(&parse_selection_set(src)),
    vec![K::Root, K::SelectionSet, K::Field]
  );
  assert_eq!(text(&parse_selection_set(src)), src);
  assert!(
    parse_selection_set(src).has_errors(),
    "an unclosed `{{` must be reported"
  );
}

#[test]
fn garbage_running_to_end_of_input_inside_a_selection_set_terminates() {
  // No sync point is left, so the balanced skip commits nothing and the loop would spin on the
  // same token forever without `unexpected`'s consume-one fallback.
  let src = "{ a ! ! !";
  assert_eq!(
    kinds(&parse_selection_set(src)),
    vec![
      K::Root,
      K::SelectionSet,
      K::Field,
      K::Error,
      K::Error,
      K::Error
    ]
  );
  assert_eq!(text(&parse_selection_set(src)), src);
}

#[test]
fn a_spread_with_no_tail_leaves_the_closer_for_its_enclosing_set() {
  // `report_unexpected` consumes nothing, which is the whole point here: `unexpected` would
  // eat the `}` (a sync point) into an `Error` node and the selection set would then run to
  // end of input looking for a closer it had already swallowed.
  let src = "{ ... }";
  assert_eq!(
    kinds(&parse_selection_set(src)),
    vec![K::Root, K::SelectionSet]
  );
  assert_eq!(text(&parse_selection_set(src)), src);
  assert!(
    parse_selection_set(src).has_errors(),
    "a `...` with no tail must be reported"
  );
}

#[test]
fn a_missing_on_is_reported_and_the_fragment_definition_survives() {
  // `on` lexes as an ordinary `Identifier`, so only its spelling separates a type condition
  // from a name that ran on. The report consumes nothing, so the name that *is* there still
  // becomes the condition's `NamedType` and the definition keeps its shape.
  let src = "fragment F User { a }";
  assert_eq!(
    kinds(&parse_fragment_definition(src)),
    vec![
      K::Root,
      K::FragmentDefinition,
      K::NamedType,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(text(&parse_fragment_definition(src)), src);
  assert!(
    parse_fragment_definition(src).has_errors(),
    "a missing `on` must be reported"
  );
}

#[test]
fn a_fragment_named_on_is_reported_and_the_definition_survives() {
  // `FragmentName: Name but not "on"` is a grammar rule, not a validation one — the spec spends
  // a production on the exclusion and so does `syntactic/`'s `fragment_name`. It is reported
  // here and the name is **still consumed**, so `fragment on on T { f }` differs from the
  // accepted spelling in the verdict and in nothing else.
  //
  // The comparison against the accepted spelling is the load-bearing assertion: `has_errors()`
  // alone would be satisfied by a check that bailed out and cost the definition its
  // `NamedType`, its `SelectionSet` and its `Field`.
  let rejected = "fragment on on T { f }";
  let accepted = "fragment Fr on T { f }";
  let want = vec![
    K::Root,
    K::FragmentDefinition,
    K::NamedType,
    K::SelectionSet,
    K::Field,
  ];

  assert_eq!(kinds(&parse_fragment_definition(rejected)), want);
  assert_eq!(kinds(&parse_fragment_definition(accepted)), want);
  assert_eq!(text(&parse_fragment_definition(rejected)), rejected);
  assert!(
    parse_fragment_definition(rejected).has_errors(),
    "`on` is the one spelling a fragment may not be called"
  );
  assert!(
    !parse_fragment_definition(accepted).has_errors(),
    "every other spelling is fine"
  );
  // The definition keeps the excluded name, so a diagnostic has something to point at.
  assert_eq!(
    texts_of(&parse_fragment_definition(rejected), K::FragmentDefinition),
    [rejected]
  );

  // The exclusion is `on` exactly — neither a prefix of it nor a different case.
  for name in ["onn", "On", "ON", "one", "o"] {
    let src = format!("fragment {name} on T {{ f }}");
    assert!(
      !parse_fragment_definition(&src).has_errors(),
      "{src:?}: only the exact spelling `on` is excluded"
    );
  }
}

#[test]
fn a_variable_definitions_directives_and_default_value_are_const() {
  // The one const position inside an executable document: the spec writes
  // `VariableDefinition: Variable : Type DefaultValue? Directives[Const]?`, and `syntactic/`'s
  // `variable_definition` calls `const_directives` where its `operation_definition` calls plain
  // `directives`. Reading the split as "SDL is const, executable is not" gets this wrong, so
  // both halves are pinned here rather than left to the SDL tests.
  for src in ["($a: Int = $b)", "($a: Int @d(x: $b))"] {
    let parse = parse_variables_definition(src);
    assert!(
      parse.has_errors(),
      "{src:?}: a variable definition's default value and directives are both const"
    );
    assert_eq!(
      text(&parse),
      src,
      "{src:?}: every byte survives a rejection"
    );
    // The `$b` still became a `Variable` node — two of them in each source, the definition's
    // own and the rejected one.
    assert_eq!(
      kinds(&parse).iter().filter(|k| **k == K::Variable).count(),
      2,
      "{src:?}: the rejected variable is still a node"
    );
  }

  // The control, in the same shape: an operation's own directives are **not** const.
  let executable = "query Q @d(x: $a) { f }";
  assert!(
    !parse_operation_definition(executable).has_errors(),
    "{executable:?}: an operation's directives take variables"
  );
}

#[test]
fn an_unrecognised_definition_head_is_attributed_and_the_document_continues() {
  // The document-level loop's termination argument: `unexpected` consumes at least one token
  // whenever input remains, so an unrecognised head costs one `Error` node and no more.
  let src = "nope { a }";
  assert_eq!(
    kinds(&parse_executable_document(src)),
    vec![
      K::Root,
      K::ExecutableDocument,
      K::Error,
      K::OperationDefinition,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(text(&parse_executable_document(src)), src);
}
