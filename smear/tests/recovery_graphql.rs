#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphql",
  feature = "parser"
))]

//! Recovery in GraphQL's parser layers: a definition that stands after a broken one is kept.
//!
//! The syntactic layer is fail-fast and answers no document for a broken text. The lossless layer
//! recovers, and these tests hold it to what a broken definition costs: itself, and not the closer
//! of the block it stands in or anything after that block.
//!
//! Every text the lossless tests read is also held to round-trip. Nothing here asserts what a
//! layer lends its sink but the codes of a string its line ends, which tell the lexer's error from
//! the parser's, and the one error counted is a budget's refusal, which is reported once.

use smear::parser::graphql::Syntactic;

/// A type whose field has no type, and a scalar after it.
const MISSING_FIELD_TYPE: &str = "type T { f: } scalar S";

#[test]
fn the_syntactic_layer_answers_no_document_for_a_broken_text() {
  assert!(
    Syntactic::new()
      .type_system_document(MISSING_FIELD_TYPE, &mut ())
      .is_err(),
    "the syntactic layer stops at the first syntax error"
  );
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use core::ops::Range;

  use smear::{
    diagnostic::Diagnose,
    lexer::limits::LosslessLimits,
    parser::graphql::{
      Lossless, Recovery,
      kinds::SyntaxKind as K,
      lossless::{
        GraphqlLosslessErrorValue, Parse, SyntaxNode, parse_document, parse_document_with_limits,
        parse_type_system_document_with_limits,
      },
    },
  };
  use tokora::emitter::Severity;

  use super::MISSING_FIELD_TYPE;

  /// What the lossless layer answered for one text.
  struct Read {
    tree: SyntaxNode,
    definitions: usize,
    recovery: Recovery,
    errors: bool,
  }

  /// `text` through the lossless layer, held to round-trip.
  fn read(text: &str) -> Read {
    let parsed = Lossless::new()
      .type_system_document(text, &mut ())
      .expect("short enough for a tree");
    let tree: SyntaxNode = parsed.tree().syntax();
    assert_eq!(tree.text().to_string(), text, "the tree spells its text");
    Read {
      tree,
      definitions: parsed.document().definitions().len(),
      recovery: parsed.recovery(),
      errors: parsed.tree().has_errors(),
    }
  }

  /// The `kind` nodes that are direct children of the `container` node under `tree`, each as it
  /// is written.
  fn entries(tree: &SyntaxNode, container: K, kind: K) -> Vec<String> {
    tree
      .children()
      .find(|node: &SyntaxNode| node.kind() == container)
      .expect("the document's node")
      .children()
      .filter(|child: &SyntaxNode| child.kind() == kind)
      .map(|child: SyntaxNode| child.text().to_string().trim().to_string())
      .collect()
  }

  /// The scalars that are direct children of the document's node, each as it is written.
  fn top_level_scalars(tree: &SyntaxNode) -> Vec<String> {
    entries(tree, K::TypeSystemDocument, K::ScalarTypeDefinition)
  }

  #[test]
  fn a_missing_field_type_keeps_the_definition_after_it() {
    for text in [
      MISSING_FIELD_TYPE,
      // An argument's type, an argument's value, a default and a list's element type.
      "type T { f(a: ): Int } scalar S",
      "type T @d(a: ) { f: Int } scalar S",
      "input I { a: Int = } scalar S",
      "type T { f: [] } scalar S",
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      assert_eq!(top_level_scalars(&answer.tree), ["scalar S"], "{text}");
      assert_eq!(answer.definitions, 1, "{text}");
      assert_eq!(answer.recovery, Recovery::new(1, 1), "{text}");
    }
  }

  #[test]
  fn an_unclosed_list_keeps_the_definition_after_it() {
    for text in [
      "type T { f: [Int } scalar S",
      "type T { f: Int @d(a: 1 } scalar S",
      "type T { f: Int @d(a: [1 } scalar S",
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      assert_eq!(top_level_scalars(&answer.tree), ["scalar S"], "{text}");
      // A closer has no image, so `T` is kept beside `S`.
      assert_eq!(answer.definitions, 2, "{text}");
      assert_eq!(answer.recovery, Recovery::new(2, 0), "{text}");
    }
  }

  /// An arguments definition that meets the `}` of the type around it reports its `)` as missing
  /// and leaves the brace. The field then fails for want of its `:`, and the type with it, and
  /// the root takes the brace as a stray and reads the next definition whole. It refuses the
  /// removal of that arm in `arguments_definition`, without which the list takes the brace as
  /// junk and reads `type` as an argument's name, which fails at `R`, and `R` is lost to the skip
  /// that follows.
  #[test]
  fn an_unclosed_arguments_definition_leaves_the_definition_after_the_brace() {
    let answer = read("type T { f(a: Int } type R { g: Int }");
    assert!(answer.errors);
    assert_eq!(
      entries(&answer.tree, K::TypeSystemDocument, K::ObjectTypeDefinition),
      ["type R { g: Int }"]
    );
    assert_eq!(answer.definitions, 1);
  }

  /// A type missing in front of junk that opens parentheses. The skip that takes the junk stops
  /// before the `}`, which closes the brace of the type around the list, a brace the skip did not
  /// open: the list leaves it, the field fails for want of its `:`, the root takes the brace, and
  /// `R` is read whole. A skip blind to which closer answers which opener took the brace with the
  /// junk, the list read `type` as an argument's name, and `R` was lost to the skips after it.
  #[test]
  fn a_skip_inside_a_list_leaves_the_brace_of_the_block_around_it() {
    let answer = read("type T { f(a: ( ( } type R { g: Int }");
    assert!(answer.errors);
    assert_eq!(
      entries(&answer.tree, K::TypeSystemDocument, K::ObjectTypeDefinition),
      ["type R { g: Int }"]
    );
    assert_eq!(answer.definitions, 1);
  }

  /// The scalars `text` keeps at the type-system root, through the layer, and at the mixed root,
  /// through the door: each must be `kept`.
  fn keeps_at_both_roots(text: &str, kept: &[&str]) {
    let answer = read(text);
    assert!(answer.errors, "{text}");
    assert_eq!(top_level_scalars(&answer.tree), kept, "{text}");
    let mixed: SyntaxNode = parse_document(text).syntax();
    assert_eq!(
      entries(&mixed, K::Document, K::ScalarTypeDefinition),
      kept,
      "{text} at the mixed root"
    );
  }

  /// The witnesses of the review's third and fourth rounds, and the third's kin, at both roots
  /// that hold a type. The list has taken its own `(` and the second `(` is junk; the skip that
  /// takes it opens a brace, and closers inside that brace close the parentheses the skip opened.
  /// One depth over every pair, the skip's count, comes back to zero exactly before the list's own
  /// `)`, so the skip stops there, the list closes on it, `A` keeps its directive and `B` is read
  /// whole.
  ///
  /// Two versions of the scan lost `B` here, each by taking the list's own `)` to bring a count of
  /// its own to zero, after which the list read `scalar` as an argument's name and failed at `B`.
  /// The scan of round two (005e784c) counted the other pairs only outside its braces: the first
  /// three rows refuse it, the closer a `)` or a `]`, and in the third a brace inside a brace. The
  /// scan of round three (fc85192c) let its count of the other pairs saturate inside the brace,
  /// so the `( (` after it raised the count past one depth: the last row refuses it.
  #[test]
  fn a_skip_keeps_the_lists_own_closer() {
    for (text, a) in [
      ("scalar A @d(( { ) } ) scalar B", "scalar A @d(( { ) } )"),
      ("scalar A @d(( { ] } ) scalar B", "scalar A @d(( { ] } )"),
      (
        "scalar A @d(( { { ) } } ) scalar B",
        "scalar A @d(( { { ) } } )",
      ),
      (
        "scalar A @d(( { ) ) ( ( } ) ) scalar B",
        "scalar A @d(( { ) ) ( ( } ) )",
      ),
    ] {
      keeps_at_both_roots(text, &[a, "scalar B"]);
    }
  }

  /// One depth over every pair can come back to zero inside a brace the skip opened, and the skip
  /// then stops at the next restart point there, the brace left open behind it, as the scan before
  /// the rule of the braces did and as this one does on purpose. At a root that keeps `B` and `C`:
  /// the root reads `B`, takes the stray `}` and reads `C`. A scan that never stopped inside a
  /// brace it opened (fc85192c) took `scalar B }` into its hole and lost `B`.
  ///
  /// In an argument list the same stop is the old answer, kept on purpose and pinned here: the
  /// skip stops at `x` inside its brace, the list reads `x` as an argument's name and fails on the
  /// `}` after it, `A` is lost with it, and the root reads `B` after skipping to it.
  #[test]
  fn a_skip_stops_inside_a_brace_it_opened_as_it_always_did() {
    keeps_at_both_roots("( { ) ) scalar B } scalar C", &["scalar B", "scalar C"]);
    keeps_at_both_roots("scalar A @d(( { ) ) x } ) scalar B", &["scalar B"]);
  }

  /// The `kind` nodes anywhere in `tree`, each as it is written.
  fn written(tree: &SyntaxNode, kind: K) -> Vec<String> {
    tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == kind)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect()
  }

  /// The first `(` is junk at the root. The skip from it crosses `type T { f: Int }`, takes the
  /// second `(` and then a `}` it did not open, and finds no restart point after that: no match,
  /// so the fallback takes the `(` and the root reads `T` whole, as before the rule of the braces.
  /// The skip from the second `(` takes the `}` and stops at `scalar`, a hole holding a `}` it did
  /// not open, so only the `(` in front of the `}` is kept; the root takes the `}` and reads `S`.
  ///
  /// Three versions of the scan lost `T` here, each stopping before that `}` on the skip from the
  /// first `(` and taking `( type T { f: Int } (` into one hole: the scan of round two
  /// (005e784c), the scan of round three (fc85192c), and the old scan with one stop more
  /// (3da6e901).
  #[test]
  fn a_skip_that_finds_no_stop_keeps_what_it_kept_before() {
    let text = "( type T { f: Int } ( } scalar S";
    keeps_at_both_roots(text, &["scalar S"]);
    let answer = read(text);
    assert_eq!(
      entries(&answer.tree, K::TypeSystemDocument, K::ObjectTypeDefinition),
      ["type T { f: Int }"]
    );
    let mixed: SyntaxNode = parse_document(text).syntax();
    assert_eq!(
      entries(&mixed, K::Document, K::ObjectTypeDefinition),
      ["type T { f: Int }"],
      "{text} at the mixed root"
    );
  }

  /// The same skip in an argument list. The second `(` is junk of the list, and the skip from it
  /// crosses `b: { c: 2 }`, takes the `}` of the type around the list, one it did not open, and
  /// finds no restart point after that. No match, so the fallback takes the `(`, as it did before
  /// the rule of the braces; the list then reads `b: { c: 2 }` as its argument, meets the `}`,
  /// reports its `)` missing and leaves the brace, and the type closes on it.
  ///
  /// The three versions of the scan that lost `T` above stopped before that `}` and took
  /// `( b: { c: 2 }` into one hole, and the list read no `b`.
  #[test]
  fn a_skip_in_a_list_that_finds_no_stop_keeps_what_it_kept_before() {
    let text = "type T { f: Int @d(a: 1 ( b: { c: 2 } }";
    let answer = read(text);
    assert!(answer.errors);
    let mixed: SyntaxNode = parse_document(text).syntax();
    for (root, tree) in [("type-system", &answer.tree), ("mixed", &mixed)] {
      assert_eq!(
        written(tree, K::Argument),
        ["a: 1", "b: { c: 2 }"],
        "{text} at the {root} root"
      );
      assert_eq!(
        written(tree, K::ObjectTypeDefinition),
        [text],
        "{text} at the {root} root"
      );
    }
  }

  /// A variables definition that meets a `}` reports its `)` as missing and leaves the brace.
  /// No block of the grammar stands around an operation, so the brace here is one a failed
  /// definition left open: the root takes it as a stray, and reads the operation after it whole.
  /// It refuses the removal of that arm in `variables_definition`, without which the list takes
  /// the brace and every token after it as its own junk, to the end of the text, and loses `R`.
  ///
  /// Through the document door, the one root that holds a type and an operation.
  #[test]
  fn an_unclosed_variables_definition_leaves_the_operation_after_the_brace() {
    let text = "type T { f query Q($a: Int } query R { g }";
    let parse: Parse = parse_document(text);
    let tree: SyntaxNode = parse.syntax();
    assert_eq!(tree.text().to_string(), text, "the tree spells its text");
    assert!(parse.has_errors());
    assert_eq!(
      entries(&tree, K::Document, K::OperationDefinition),
      ["query R { g }"]
    );
  }

  // ── A string its line ends ─────────────────────────────────────────────────────────────────

  /// A type whose first field's deprecation reason has no closing quote on its line, and two
  /// fields after it, the second deprecated with a reason of its own.
  const UNCLOSED_REASON: &str = "type Query {\n  a: String @deprecated(reason: \"oops)\n  \
                                 b: Int\n  c: String @deprecated(reason: \"fine\")\n}\n";

  /// The codes of the errors the lossless layer lends a sink for `text`, in the order it lends
  /// them.
  fn codes(text: &str) -> Vec<String> {
    let mut lent: Vec<GraphqlLosslessErrorValue<&str>> = Vec::new();
    let _ = Lossless::new()
      .type_system_document(text, &mut lent)
      .expect("short enough for a tree");
    lent
      .iter()
      .map(|error| error.code().as_str().to_string())
      .collect()
  }

  /// The tokens of `kind` anywhere in `tree`, each as it is written.
  fn tokens(tree: &SyntaxNode, kind: K) -> Vec<String> {
    tree
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
      .filter(|token| token.kind() == kind)
      .map(|token| token.text().to_string())
      .collect()
  }

  /// An inline string ends at its line, so the second line's literal is `"oops)` alone: the one
  /// gap, and the one error the lexer reports, which comes first. The lines after it are tokens
  /// again, so `"fine"` is a string with both its quotes and no token but a line break runs over a
  /// line's end. The literal used to run on to the opening quote of `"fine"`, and that string's
  /// closing quote then opened one the text ended inside.
  ///
  /// The `(` the broken line leaves open still costs what an unclosed argument list costs, which
  /// the lexer cannot mend: the list reads `b` as the reason's value and `Int` as the name of an
  /// argument whose `:` is missing at `c`, so `b` and `c` are not fields of `Query`. Those are the
  /// parser's errors, after the lexer's.
  #[test]
  fn a_string_its_line_ends_leaves_the_lines_after_it_to_the_grammar() {
    let answer = read(UNCLOSED_REASON);
    assert!(answer.errors);
    assert_eq!(gaps(&answer.tree), [(String::from("\"oops)"), 45..51)]);
    assert_eq!(tokens(&answer.tree, K::String), ["\"fine\""]);
    for token in answer
      .tree
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
    {
      assert!(
        token.kind() == K::Newline || !token.text().contains('\n'),
        "{token:?} runs over a line's end"
      );
    }

    let codes = codes(UNCLOSED_REASON);
    assert_eq!(
      codes.first().map(String::as_str),
      Some("smear::syntax::unterminated-string"),
      "{codes:?}"
    );
    assert!(
      codes.len() > 1
        && codes[1..]
          .iter()
          .all(|code| code != "smear::syntax::unterminated-string"),
      "the parser's errors follow the lexer's one: {codes:?}"
    );
  }

  /// The same broken line in a type that closes after it. The argument list meets the type's
  /// `}`, reports it as the value that is missing and as its own `)`, and leaves it, so the type
  /// closes on it and the definition after it is read whole. The literal used to run to the end of
  /// the text, the brace and the next definition with it.
  #[test]
  fn a_string_its_line_ends_keeps_the_definition_after_it() {
    let text = "type Query {\n  a: String @deprecated(reason: \"oops)\n}\n\
                type Mutation {\n  b: Int\n}\n";
    let answer = read(text);
    assert!(answer.errors);
    assert_eq!(gaps(&answer.tree), [(String::from("\"oops)"), 45..51)]);
    assert_eq!(
      entries(&answer.tree, K::TypeSystemDocument, K::ObjectTypeDefinition),
      [
        "type Query {\n  a: String @deprecated(reason: \"oops)\n}",
        "type Mutation {\n  b: Int\n}"
      ]
    );
    assert_eq!(
      written(&answer.tree, K::FieldDefinition),
      ["a: String @deprecated(reason: \"oops)", "b: Int"]
    );
    assert_eq!(
      codes(text),
      [
        "smear::syntax::unterminated-string",
        "smear::syntax::unexpected-token",
        "smear::syntax::unexpected-token"
      ]
    );
  }

  // ── A budget that runs out while a skip is scanned again ───────────────────────────────────

  /// The fifth external review's witness: three `(` of junk at the root, a `}` no skip opened, two
  /// `)` and a definition.
  const REPLAYED: &str = "(((}))scalar B";

  /// [`REPLAYED`] at both roots under a produce-event budget of `max`, each root's name with its
  /// parse.
  fn replayed(max: usize) -> [(&'static str, Parse); 2] {
    let limits = LosslessLimits::default().with_max_produce_events(max);
    [
      (
        "type-system",
        parse_type_system_document_with_limits(REPLAYED, limits),
      ),
      ("mixed", parse_document_with_limits(REPLAYED, limits)),
    ]
  }

  /// The gap tiles anywhere in `tree`, text the parse never read, each as it is written and where.
  fn gaps(tree: &SyntaxNode) -> Vec<(String, Range<usize>)> {
    tree
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
      .filter(|token| token.kind() == K::Gap)
      .map(|token| {
        let range = token.text_range();
        (
          token.text().to_string(),
          usize::from(range.start())..usize::from(range.end()),
        )
      })
      .collect()
  }

  /// [`REPLAYED`] under the produce-event budget, the budget no rollback refunds.
  ///
  /// tokora charges one produce event for each item its one lexing site hands back, a token, a
  /// space or a lexer error, whether the parse consumes it or only peeks it into the cache
  /// (`InputRef::lex_within_boundary`, `spend()` after `lexer.lex()` hands an item back). Taking a
  /// token out of the cache charges nothing, and neither does the end of the input: `lex()`
  /// handing back nothing is charged nothing, and with the budget spent, `settle_met_ceiling`
  /// answers a genuine end by position, before it lexes anything.
  ///
  /// The text is nine lexemes. The root peeks the first `(`, event 1, and the skip from it lexes
  /// the next six, up to `scalar`, as events 2 to 7, and finds its restart point there. The skip as
  /// it always ran kept that hole and read the space and `B` as events 8 and 9: nine events in all.
  /// Here the hole holds a `}` the skip did not open, so the attempt is declined. The second scan
  /// lexes the three `(` and the `}` again as events 8 to 11 and keeps the hole `(((` in front of
  /// the `}`. The root takes the `}` from the cache, then reads `)`, `)`, `scalar`, the space and
  /// `B` as events 12 to 16. So `B` is read from a budget of 16 on.
  ///
  /// Below 16 the budget runs out first and the parse ends where it does. At 9 and 10 it runs out
  /// inside the second scan, which keeps `((` or `(((` with no hole. At 11 to 15 it runs out after
  /// the second scan, before `B`. At 9 the tree holds `((` and the gap of what was never read,
  /// nothing else, and the refusal is reported once, at the end of what the parse committed.
  #[test]
  fn a_budget_that_runs_out_while_a_skip_is_scanned_again_ends_the_parse_there() {
    for (root, parse) in replayed(9) {
      let tree: SyntaxNode = parse.syntax();
      assert_eq!(tree.text().to_string(), REPLAYED, "at the {root} root");
      assert!(
        written(&tree, K::ScalarTypeDefinition).is_empty(),
        "at the {root} root: `B` is read"
      );
      assert!(
        written(&tree, K::Error).is_empty(),
        "at the {root} root: a hole is kept"
      );
      assert_eq!(
        gaps(&tree),
        [(String::from("(}))scalar B"), 2..REPLAYED.len())],
        "at the {root} root: what the parse never read"
      );
      let refusals = parse
        .diagnostics()
        .iter()
        .filter(|diagnostic| {
          diagnostic.severity() == Severity::Error && diagnostic.span() == (2..2)
        })
        .count();
      assert_eq!(refusals, 1, "at the {root} root: the budget's refusal");
    }

    for (budget, parses) in [("16", replayed(16)), ("none", replayed(usize::MAX))] {
      for (root, parse) in parses {
        let tree: SyntaxNode = parse.syntax();
        assert_eq!(
          tree.text().to_string(),
          REPLAYED,
          "at the {root} root under {budget}"
        );
        assert_eq!(
          written(&tree, K::ScalarTypeDefinition),
          ["scalar B"],
          "at the {root} root under {budget}"
        );
        assert_eq!(
          written(&tree, K::Error),
          ["(((", "}", ")", ")"],
          "at the {root} root under {budget}: the hole in front of the `}}`, then the junk"
        );
        assert!(gaps(&tree).is_empty(), "at the {root} root under {budget}");
      }
    }
  }

  /// Every budget from none to one past 16, the least at which [`REPLAYED`] reads `B`: the tree
  /// spells its text at each, and `B` is read at 16 and 17 and at none below. A change that makes
  /// recovery here dearer or cheaper moves the threshold, and this names the budget it moved at.
  #[test]
  fn a_budget_that_runs_out_while_a_skip_is_scanned_again_reads_b_from_sixteen_on() {
    for max in 0..=17 {
      for (root, parse) in replayed(max) {
        let tree: SyntaxNode = parse.syntax();
        assert_eq!(
          tree.text().to_string(),
          REPLAYED,
          "at the {root} root under max_produce_events({max})"
        );
        assert_eq!(
          written(&tree, K::ScalarTypeDefinition) == ["scalar B"],
          max >= 16,
          "at the {root} root under max_produce_events({max}): whether `B` is read"
        );
      }
    }
  }

  // ── A list on a definition's header that has lost its `)`: smear issue #295 ─────────────────

  /// The errors `layer` lends a sink for `text`, in the order it lends them, each as its code and
  /// its primary span.
  fn lent(layer: &Lossless, text: &str) -> Vec<(String, Range<usize>)> {
    let mut errors: Vec<GraphqlLosslessErrorValue<&str>> = Vec::new();
    let _ = layer
      .type_system_document(text, &mut errors)
      .expect("short enough for a tree");
    errors
      .iter()
      .map(|error| {
        let span = error.primary().span();
        (
          error.code().as_str().to_string(),
          span.map_or(0, |span| span.start())..span.map_or(0, |span| span.end()),
        )
      })
      .collect()
  }

  /// The definitions and extensions that are direct children of the document, each as it is
  /// written.
  fn definitions(tree: &SyntaxNode) -> Vec<String> {
    tree
      .children()
      .find(|node: &SyntaxNode| node.kind() == K::TypeSystemDocument)
      .expect("the document's node")
      .children()
      .filter(|node: &SyntaxNode| {
        matches!(
          node.kind(),
          K::ScalarTypeDefinition
            | K::ObjectTypeDefinition
            | K::InterfaceTypeDefinition
            | K::UnionTypeDefinition
            | K::EnumTypeDefinition
            | K::InputObjectTypeDefinition
            | K::SchemaDefinition
            | K::DirectiveDefinition
            | K::ScalarTypeExtension
            | K::ObjectTypeExtension
        )
      })
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect()
  }

  /// How many lexemes `text` is, each byte of trivia one of them: the tokens of its tree.
  fn lexemes(text: &str) -> usize {
    parse_document(text)
      .syntax()
      .descendants_with_tokens()
      .filter(|element| element.as_token().is_some())
      .count()
  }

  /// GraphQLx's twin: a list on a header that has lost its `)` ends in front of its owner's body,
  /// where the `}` that balances the body's `{` is written and the end of input or the start of a
  /// definition stands behind it. One report, at the `{`; the definition is built with its member
  /// and the next one read. On each owner that reads a body.
  #[test]
  fn a_headers_list_that_lost_its_closer_leaves_the_owners_body() {
    for (text, kept) in [
      (
        "type T @d(x: 1 { f: Int } scalar S",
        &["type T @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "interface N @d(x: 1 { f: Int } scalar S",
        &["interface N @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "input I @d(x: 1 { zz: Int } scalar S",
        &["input I @d(x: 1 { zz: Int }", "scalar S"][..],
      ),
      (
        "enum E @d(x: 1 { A B } scalar S",
        &["enum E @d(x: 1 { A B }", "scalar S"][..],
      ),
      (
        "schema @d(x: 1 { query: Q } scalar S",
        &["schema @d(x: 1 { query: Q }", "scalar S"][..],
      ),
      (
        "extend type T @d(x: 1 { f: Int } scalar S",
        &["extend type T @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 { f: Int }",
        &["type T @d(x: 1 { f: Int }"][..],
      ),
    ] {
      let at = text.rfind('{').expect("the body's brace");
      assert_eq!(
        lent(&Lossless::new(), text),
        [(String::from("smear::syntax::unexpected-token"), at..at + 1)],
        "{text}: one report, at the body's brace"
      );
      let answer = read(text);
      assert_eq!(written(&answer.tree, K::Argument), ["x: 1"], "{text}");
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }
  }

  /// Where the owner reads no body a `{` on its header is never its own, and the list reads it as
  /// it always did, as junk: the members inside the group are the list's, and the reports are the
  /// ones the list always made.
  #[test]
  fn an_owner_that_reads_no_body_reads_a_brace_on_its_header_as_before() {
    for (text, member, members, reports) in [
      (
        "scalar A @d(x: 1 { a: 1 }",
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "union U @d(x: 1 { a: 1 }",
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "extend scalar A @d(x: 1 { a: 1 }",
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "directive @d(a: Int { b: Int }",
        K::InputValueDefinition,
        &["a: Int", "b: Int"][..],
        5,
      ),
    ] {
      assert_eq!(written(&read(text).tree, member), members, "{text}");
      assert_eq!(lent(&Lossless::new(), text).len(), reports, "{text}");
    }
  }

  /// A list on a header that has lost its `)` ends in front of the start of a definition, of
  /// `repeatable` and `on` on a directive definition, of a union's members and of the owner's next
  /// directive, each on its test; one report, the definition kept and the next read. `extend` with
  /// the keyword of what it extends is one start.
  #[test]
  fn a_headers_list_that_lost_its_closer_leaves_what_its_owner_reads_next() {
    for (text, kept, reports) in [
      (
        "type T @d(x: 1 scalar S",
        &["type T @d(x: 1", "scalar S"][..],
        1,
      ),
      (
        "scalar A @d(x: 1 scalar S",
        &["scalar A @d(x: 1", "scalar S"][..],
        1,
      ),
      (
        "type T @d(x: 1 \"doc\" scalar S",
        &["type T @d(x: 1", "\"doc\" scalar S"][..],
        1,
      ),
      (
        "type T @d(x: 1 extend type U { g: Int } scalar S",
        &["type T @d(x: 1", "extend type U { g: Int }", "scalar S"][..],
        1,
      ),
      (
        "directive @d(a: Int on FIELD scalar S",
        &["directive @d(a: Int on FIELD", "scalar S"][..],
        1,
      ),
      (
        "directive @d(a: Int = 1 repeatable on FIELD scalar S",
        &["directive @d(a: Int = 1 repeatable on FIELD", "scalar S"][..],
        1,
      ),
      (
        "directive @d(a: Int) @x(y: 1 on FIELD scalar S",
        &["directive @d(a: Int) @x(y: 1 on FIELD", "scalar S"][..],
        1,
      ),
      (
        "union U @d(x: 1 = A | B scalar S",
        &["union U @d(x: 1 = A | B", "scalar S"][..],
        1,
      ),
      (
        "type T @d(x: 1 @e { f: Int } scalar S",
        &["type T @d(x: 1 @e { f: Int }", "scalar S"][..],
        1,
      ),
    ] {
      assert_eq!(lent(&Lossless::new(), text).len(), reports, "{text}");
      assert_eq!(definitions(&read(text).tree), kept, "{text}");
    }
  }

  /// Where a test says nothing the list reads its head as it always did: the list's `)` behind an
  /// `@` or an `=`, a keyword at the end of input, a body with junk behind it, a keyword behind a
  /// keyword, and a described keyword whose token behind stands past the cache, which the window
  /// does not read; and a list in a member reads as it did.
  #[test]
  fn a_headers_list_reads_as_before_where_no_test_holds() {
    for (text, kept, reports) in [
      (
        "type T @d(x: 1 @e) { f: Int } scalar S",
        &["scalar S"][..],
        2,
      ),
      (
        "union U @d(x: 1 = 2) = A | B",
        &["union U @d(x: 1 = 2) = A | B"][..],
        2,
      ),
      ("type T @d(x: 1 scalar", &[][..], 1),
      (
        "type T @d(x: 1 { f: Int } junk scalar S",
        &["scalar S"][..],
        3,
      ),
      ("scalar A @d(x: 1 type scalar S", &["scalar S"][..], 1),
      ("directive @d(a: Int \"doc\" scalar S", &[][..], 1),
      ("directive @d(a: Int \"doc\"scalar S", &[][..], 1),
      ("type T { f: Int @d(x: 1 scalar S", &[][..], 1),
    ] {
      assert_eq!(lent(&Lossless::new(), text).len(), reports, "{text}");
      assert_eq!(definitions(&read(text).tree), kept, "{text}");
    }
  }

  /// A member, or a directive's name, spelled like a keyword a root restarts at with a token
  /// behind it that may begin that definition is no evidence, wherever the chain reaches it: the
  /// keyword may be a definition's. The list skips its junk as it always did, the owner is lost at
  /// the `:` its next argument misses, and the root reads the definition whole, where the evidence
  /// would have made its keyword a member or a directive, and its name and what follows the
  /// owner's.
  #[test]
  fn a_member_spelled_like_a_definitions_keyword_is_no_evidence() {
    for (text, kind, kept) in [
      (
        "union U @d(x: 1 = A | enum query { VALUE }",
        K::EnumTypeDefinition,
        "enum query { VALUE }",
      ),
      (
        "type T @d(x: 1 @a @enum query { VALUE }",
        K::EnumTypeDefinition,
        "enum query { VALUE }",
      ),
      // `directive` stands in the middle of the chain, and `on FIELD` behind its last directive.
      (
        "directive @a(@e @\ndirective @b on FIELD",
        K::DirectiveDefinition,
        "directive @b on FIELD",
      ),
    ] {
      assert_eq!(
        written(&read(text).tree, kind),
        [kept],
        "{text} at the type-system root"
      );
      assert_eq!(
        written(&parse_document(text).syntax(), kind),
        [kept],
        "{text} at the mixed root"
      );
    }
  }

  /// GraphQLx's twin: what a look-ahead crosses on its way to its evidence is asked a token at a
  /// time, at every depth, so a definition that begins inside it is no evidence: a keyword inside
  /// a directive's arguments and inside the body at the head; and a `)` inside a directive's
  /// arguments with a `[` still open in front of it. The list skips its junk as it always did, and
  /// the root reads the definition, where the evidence would have made it a value or a member. In
  /// the first the owner is itself a scalar, so `S` stands beside it.
  #[test]
  fn a_definition_inside_what_a_look_ahead_crosses_is_no_evidence() {
    let text = "scalar A @d(@e(x:[\nscalar S\n])";
    for (root, tree) in [
      ("type-system", read(text).tree),
      ("mixed", parse_document(text).syntax()),
    ] {
      let scalars = written(&tree, K::ScalarTypeDefinition);
      assert!(
        scalars.iter().any(|scalar| scalar == "scalar S"),
        "{text} at the {root} root: {scalars:?}"
      );
    }

    for text in [
      "type T @d(x: 1 { f: Int @deprecated scalar S }",
      "type T @d(x: 1 @e(y: [1) scalar S ])",
    ] {
      for (root, tree) in [
        ("type-system", read(text).tree),
        ("mixed", parse_document(text).syntax()),
      ] {
        assert_eq!(
          written(&tree, K::ScalarTypeDefinition),
          ["scalar S"],
          "{text} at the {root} root"
        );
      }
    }

    let text = "enum E @d(x: 1 { A B scalar S } type T { f: Int }";
    for (root, tree) in [
      ("type-system", read(text).tree),
      ("mixed", parse_document(text).syntax()),
    ] {
      assert_eq!(
        written(&tree, K::ScalarTypeDefinition),
        ["scalar S"],
        "{text} at the {root} root"
      );
      assert_eq!(
        written(&tree, K::ObjectTypeDefinition),
        ["type T { f: Int }"],
        "{text} at the {root} root"
      );
    }
  }

  /// GraphQLx's twin: a mixed root reads as an operation a `{` its scan leaves at the root, so a
  /// look-ahead finds nothing where main would read one: at a `{` inside a directive's arguments,
  /// at the end of input behind a body reached through a directive, and behind a body that leaves
  /// a bracket open at its `}`, which hides from main's scan, blind to which closer answers which
  /// opener, the definition behind it. The root reads the operation main reads, and the
  /// definition after it.
  #[test]
  fn an_operation_main_reads_behind_a_header_is_no_evidence() {
    for (text, operation) in [
      ("type T @d(@e(x: [\n{ me }\n])", "{ me }"),
      ("type T @d(x: 1 @e @g\n{ me }", "{ me }"),
      ("type T @d(x: 1 @e @g { f(b: 1 } scalar S", "{ f(b: 1 }"),
    ] {
      assert_eq!(
        written(&parse_document(text).syntax(), K::OperationDefinition),
        [operation],
        "{text}"
      );
    }

    let text = "type T @d(x: 1 @e @g { f(b: 1 } scalar S";
    for (root, tree) in [
      ("type-system", read(text).tree),
      ("mixed", parse_document(text).syntax()),
    ] {
      assert_eq!(
        written(&tree, K::ScalarTypeDefinition),
        ["scalar S"],
        "{text} at the {root} root"
      );
    }
  }

  /// GraphQLx's twin: a look-ahead crosses brackets only as deep as the parse has left, and only
  /// where each closes the one open last: the reading that would take its evidence descends a
  /// frame for each, where main scans them flat, and a refusal there ends the document. Inside a
  /// directive's arguments, `}` answers no `[` under the default ceiling, and under a raised one
  /// 300 `[` nest past what a crossing holds; inside a body, `)` answers no `[`. The type-system
  /// root lends no refusal, and both roots read the definition behind as main does, which a
  /// refusal would have ended the document in front of.
  #[test]
  fn a_look_ahead_crosses_no_deeper_than_the_parse_reads() {
    let unpaired = format!("type T @d(x: 1 @e(y: {}1) scalar S", "[ } ".repeat(30));
    let deep = format!(
      "type T @d(x: 1 @e(y: {}1{}) scalar S",
      "[".repeat(300),
      "]".repeat(300)
    );
    let body = format!(
      "type T @d(x: 1 {{ f: Int @x(a: {}1) }} scalar S",
      "[ ) ".repeat(30)
    );
    for (text, limits) in [
      (unpaired, LosslessLimits::default()),
      (deep, LosslessLimits::with_max_nesting_depth(1000)),
      (body, LosslessLimits::default()),
    ] {
      let errors = lent(&Lossless::with_limits(limits), &text);
      assert!(
        !errors
          .iter()
          .any(|(code, _)| code == "smear::syntax::nesting-limit"),
        "{text}: {errors:?}"
      );
      for (root, parse) in [
        ("mixed", parse_document_with_limits(&text, limits)),
        (
          "type-system",
          parse_type_system_document_with_limits(&text, limits),
        ),
      ] {
        assert_eq!(
          written(&parse.syntax(), K::ScalarTypeDefinition),
          ["scalar S"],
          "{text} at the {root} root"
        );
      }
    }
  }

  /// GraphQLx's twin: a look-ahead takes at most 256 tokens, trivia among them, from the head it
  /// is made at. Codex's witness stands five thousand blanks inside the body it would cross: a
  /// look-ahead that crossed them all spent the scan allowance with what it gave back, so that the
  /// list's next skip took one token and `T` was lost. Here it stops at the cap, the list skips
  /// its junk as main does, and `T` keeps its field.
  #[test]
  fn a_look_ahead_takes_no_more_than_its_cap() {
    let text = ["type T @d({(x y)", &" ".repeat(5000), "}) { f: Int }"].concat();
    assert_eq!(
      written(&read(&text).tree, K::FieldDefinition),
      ["f: Int"],
      "{text}"
    );
  }

  /// GraphQLx's twin: with 246 blanks in front of `f` the look-ahead takes 256 tokens, to the
  /// blank in front of `S`, and the list ends in front of the body; with 247 it would take a
  /// 257th, finds nothing, and the list reads its junk as main reads it, `f: Int` among its
  /// arguments. `S` is a definition either way.
  #[test]
  fn a_look_ahead_finds_its_evidence_up_to_its_cap_and_none_past_it() {
    for (blanks, kept) in [(246, true), (247, false)] {
      let text = ["type T @d(x: 1 {", &" ".repeat(blanks), "f: Int } scalar S"].concat();
      let tree = read(&text).tree;
      let fields = written(&tree, K::FieldDefinition);
      assert_eq!(
        fields.len(),
        usize::from(kept),
        "{blanks} blanks: {fields:?}"
      );
      let arguments = written(&tree, K::Argument);
      assert_eq!(
        arguments.iter().any(|argument| argument == "f: Int"),
        !kept,
        "{blanks} blanks: {arguments:?}"
      );
      assert_eq!(top_level_scalars(&tree), ["scalar S"], "{blanks} blanks");
    }
  }

  /// The least produce-event budget under which the parse of `text` is not refused.
  fn least_budget(text: &str) -> usize {
    (0..=4 * lexemes(text))
      .find(|&max| {
        let limits = LosslessLimits::default().with_max_produce_events(max);
        lent(&Lossless::with_limits(limits), text)
          .iter()
          .all(|(code, _)| code.as_str() != "smear::syntax::token-budget")
      })
      .unwrap_or_else(|| panic!("{text} reads under no budget"))
  }

  /// No valid document makes a stop, and in one the peek window lexes nothing twice: a header's
  /// argument named like a definition's keyword reads under one produce-event for each lexeme and
  /// one more, the least budget the same text reads under with a name that spells nothing, and so
  /// does a described member of a directive definition, whose name's token behind stands past the
  /// cache and is not read. With lexer errors in front of that token the window would have lexed
  /// them all to fill its one slot, and each again where the member reads them. A window lexes
  /// twice only a run of lexer errors it meets the end of input across, until tokora retains the
  /// end of input after an error run, and a valid document holds no lexer error.
  #[test]
  fn a_valid_header_reads_as_before() {
    for text in [
      "type T @d(type: 1 input: 2) { f: Int }",
      "directive @d(on: Int repeatable: Int) on FIELD",
      "directive @d(\"doc\" type: Int) on FIELD",
    ] {
      assert!(!read(text).errors, "{text}");
    }
    let text = "type T @d(type: 1 input: 2) { f: Int }";
    let limits = LosslessLimits::default().with_max_produce_events(lexemes(text) + 1);
    assert!(
      lent(&Lossless::with_limits(limits), text).is_empty(),
      "{text}"
    );
    for (text, plain, more) in [
      (
        "type T @d(type: 1 input: 2) { f: Int }",
        "type T @d(tipe: 1 inpot: 2) { f: Int }",
        0,
      ),
      (
        "directive @d(\"doc\" type: Int) on FIELD",
        "directive @d(\"doc\" tipe: Int) on FIELD",
        0,
      ),
      (
        "directive @d(\"doc\" type : Int) on FIELD",
        "directive @d(\"doc\" tipe : Int) on FIELD",
        0,
      ),
      (
        "directive @d(\"doc\" type??: Int) on FIELD scalar S",
        "directive @d(\"doc\" tipe??: Int) on FIELD scalar S",
        0,
      ),
    ] {
      assert_eq!(least_budget(text), least_budget(plain) + more, "{text}");
    }
  }

  /// A directive definition reads no location at the start of a definition: at the head of its
  /// locations and behind a `|` such a keyword is reported as a location missing, at the keyword
  /// and before anything behind it is read, and left to the root where a look-ahead behind that
  /// report finds a definition starting there, across any trivia: the root reads the definition
  /// behind. The first error is where it was, and a valid directive definition makes no report.
  #[test]
  fn a_directive_definition_reads_no_location_at_the_start_of_a_definition() {
    // Each with the token its first error stands at: the `:` its member misses behind the list,
    // and the `on` or the location missing at the keyword.
    for (text, kept, first) in [
      (
        "directive @d(a: Int scalar S",
        &["directive @d(a: Int", "scalar S"][..],
        "S",
      ),
      (
        "directive @d scalar S",
        &["directive @d", "scalar S"][..],
        "scalar",
      ),
      (
        "directive @d on scalar S",
        &["directive @d on", "scalar S"][..],
        "scalar",
      ),
      (
        "directive @d on scalar  S",
        &["directive @d on", "scalar  S"][..],
        "scalar",
      ),
      (
        "directive @d(a: Int repeatable on scalar  S",
        &["directive @d(a: Int repeatable on", "scalar  S"][..],
        "on",
      ),
      (
        "directive @d on FIELD | type U { g: Int }",
        &["directive @d on FIELD |", "type U { g: Int }"][..],
        "type",
      ),
    ] {
      assert_eq!(definitions(&read(text).tree), kept, "{text}");
      let at = text.rfind(first).expect("the first error's token");
      assert_eq!(
        lent(&Lossless::new(), text).first(),
        Some(&(
          String::from("smear::syntax::unexpected-token"),
          at..at + first.len()
        )),
        "{text}"
      );
    }

    // The keyword's report is made before the look-ahead lexes the character behind it, at the
    // head of the locations and behind a `|` alike.
    for text in [
      "directive @d on scalar?S",
      "directive @d on FIELD | scalar?S",
    ] {
      let at = text.find("scalar").expect("the keyword");
      assert_eq!(
        lent(&Lossless::new(), text).first(),
        Some(&(
          String::from("smear::syntax::unexpected-token"),
          at..at + "scalar".len()
        )),
        "{text}"
      );
    }

    let text = "directive @d on FIELD | OBJECT scalar S";
    assert!(lent(&Lossless::new(), text).is_empty(), "{text}");
    assert_eq!(
      definitions(&read(text).tree),
      ["directive @d on FIELD | OBJECT", "scalar S"],
      "{text}"
    );
  }

  /// The report at a head no valid list holds is made before the look-ahead behind it, so no
  /// produce-event budget can stop the parse in front of it: under every budget the parse either
  /// stops before it reaches the `{`, with the budget's refusal its one error, or lends the
  /// unexpected token at the `{` first. And a lexer's error in front of a keyword's report keeps
  /// its place: the window lexes no further than the member there reads to its `:`.
  #[test]
  fn a_headers_first_report_keeps_its_place() {
    let text = "type T @d(x: 1 { f: Int } scalar S";
    let at = text.find('{').expect("the body's brace");
    let brace = (String::from("smear::syntax::unexpected-token"), at..at + 1);
    let mut reached = 0usize;
    for max in 0..=4 * lexemes(text) {
      let limits = LosslessLimits::default().with_max_produce_events(max);
      let errors = lent(&Lossless::with_limits(limits), text);
      if errors.len() == 1 && errors[0].0 == "smear::syntax::token-budget" {
        continue;
      }
      reached += 1;
      assert_eq!(
        errors.first(),
        Some(&brace),
        "max_produce_events({max}): {errors:?}"
      );
    }
    assert!(reached > 0, "no budget let the parse reach the brace");

    let text = "type T @d(x: 1 scalar ? S";
    let at = text.find('?').expect("the unknown character");
    assert_eq!(
      lent(&Lossless::new(), text).first(),
      Some(&(String::from("smear::syntax::unknown-character"), at..at + 1)),
      "{text}"
    );
  }

  /// A stop's report is built from the token the window found, so no budget spent behind that
  /// window can leave the stop without it: under every produce-event budget the parse either stops
  /// before the report, with the budget's refusal its one error, or lends the `:` missing at `S`
  /// first. A window the refusal cuts short is no answer, and the member's own read meets the
  /// refusal. In the last two texts the token behind `scalar` stands past the cache, no stop is
  /// made, and the member misses its `:` there itself.
  #[test]
  fn a_stops_report_comes_from_the_window_that_found_it() {
    for text in [
      "type T @d(x: 1 scalar S",
      "directive @d(\"d\"scalar S",
      "directive @d(\"d\" scalar S",
    ] {
      let at = text.rfind('S').expect("the name behind the keyword");
      let colon = (String::from("smear::syntax::unexpected-token"), at..at + 1);
      let mut reached = 0usize;
      for max in 0..=4 * lexemes(text) {
        let limits = LosslessLimits::default().with_max_produce_events(max);
        let errors = lent(&Lossless::with_limits(limits), text);
        if errors.len() == 1 && errors[0].0 == "smear::syntax::token-budget" {
          continue;
        }
        reached += 1;
        assert_eq!(
          errors.first(),
          Some(&colon),
          "{text} under max_produce_events({max}): {errors:?}"
        );
      }
      assert!(
        reached > 0,
        "{text}: no budget let the parse reach the report"
      );
    }
  }
}
