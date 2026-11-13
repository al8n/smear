#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphqlx",
  feature = "parser"
))]

//! Recovery in GraphQLx's parser layers: what stands beside a broken item is kept, where it stands.
//!
//! The syntactic layer is fail-fast and answers no document for a broken text. The lossless layer
//! recovers, and these tests hold it to where the recovery puts what it keeps: an item of a module
//! stays an item of that module, a definition after a module stays outside it, and one broken item
//! costs that item alone.
//!
//! Every text the lossless tests read is also held to round-trip. Nothing here asserts what a
//! layer lends its sink, which `layers_handover_graphqlx.rs` holds: where a test counts what a
//! mistake costs, it counts the error diagnostics of the tree.

use smear::parser::graphqlx::Syntactic;

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
    lexer::limits::LosslessLimits,
    parser::graphqlx::{
      Lossless, Recovery, Syntactic,
      ast::{TypeSystemDocument, TypeSystemItem},
      kinds::SyntaxKind as K,
      lossless::{Parse, SyntaxNode, parse_document, parse_type_system_document},
    },
  };
  use tokora::emitter::Severity;

  use super::MISSING_FIELD_TYPE;

  /// What the lossless layer answered for one text.
  struct Read<'a> {
    tree: SyntaxNode,
    document: TypeSystemDocument<&'a str>,
    recovery: Recovery,
    errors: bool,
    /// How many error diagnostics the tree holds.
    reports: usize,
  }

  /// `text` through the lossless layer at its default limits.
  fn read(text: &str) -> Read<'_> {
    read_with(&Lossless::new(), text)
  }

  /// `text` through `layer`, held to round-trip.
  fn read_with<'a>(layer: &Lossless, text: &'a str) -> Read<'a> {
    let parsed = layer
      .type_system_document(text, &mut ())
      .expect("short enough for a tree");
    let tree: SyntaxNode = parsed.tree().syntax();
    assert_eq!(tree.text().to_string(), text, "the tree spells its text");
    Read {
      tree,
      document: parsed.document().clone(),
      recovery: parsed.recovery(),
      errors: parsed.tree().has_errors(),
      reports: parsed
        .tree()
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .count(),
    }
  }

  /// The document's own node.
  fn top(tree: &SyntaxNode) -> SyntaxNode {
    tree
      .children()
      .find(|node: &SyntaxNode| node.kind() == K::TypeSystemDocument)
      .expect("the document's node")
  }

  /// The first module, anywhere in `tree`, whose text opens with `opens`.
  fn module(tree: &SyntaxNode, opens: &str) -> SyntaxNode {
    tree
      .descendants()
      .find(|node: &SyntaxNode| {
        node.kind() == K::ModDefinition && node.text().to_string().starts_with(opens)
      })
      .unwrap_or_else(|| panic!("no module opens with `{opens}`"))
  }

  /// The scalars that are direct children of `node`, each as it is written.
  fn scalars(node: &SyntaxNode) -> Vec<String> {
    node
      .children()
      .filter(|child: &SyntaxNode| child.kind() == K::ScalarTypeDefinition)
      .map(|child: SyntaxNode| child.text().to_string().trim().to_string())
      .collect()
  }

  /// The kinds of the nodes that are direct children of `node`.
  fn kinds(node: &SyntaxNode) -> Vec<K> {
    node
      .children()
      .map(|child: SyntaxNode| child.kind())
      .collect()
  }

  /// Whether a `}` is a direct child of `node`.
  fn holds_a_closing_brace(node: &SyntaxNode) -> bool {
    node
      .children_with_tokens()
      .any(|element| element.kind() == K::RBrace)
  }

  /// The items of the module `item` is.
  fn items<'d, 'a>(item: &'d TypeSystemItem<&'a str>) -> &'d [TypeSystemItem<&'a str>] {
    match item {
      TypeSystemItem::Mod(module) => module.items(),
      _ => panic!("the item is no module"),
    }
  }

  // ── The review's regression, and the other routes to it ────────────────────────────────────

  #[test]
  fn a_broken_item_leaves_the_modules_other_items_in_the_module() {
    let answer = read("mod m { scalar X type T { f } }");
    assert!(answer.errors);

    let document = top(&answer.tree);
    assert_eq!(kinds(&document), [K::ModDefinition]);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar X"]);
    assert!(scalars(&document).is_empty(), "X is declared at the top");

    let definitions = answer.document.definitions();
    assert_eq!(definitions.len(), 1);
    let inside = items(&definitions[0]);
    assert_eq!(inside.len(), 1);
    assert!(matches!(inside[0], TypeSystemItem::Definition(_)));
    assert_eq!(answer.recovery.projected(), 2);
    assert!(!answer.recovery.is_complete());
  }

  #[test]
  fn junk_with_braces_does_not_close_the_module() {
    for text in [
      "mod m { query Q { f } scalar X }",
      "mod m { tpye T { f: Int } scalar X }",
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      assert_eq!(kinds(&top(&answer.tree)), [K::ModDefinition], "{text}");
      assert_eq!(
        scalars(&module(&answer.tree, "mod m")),
        ["scalar X"],
        "{text}"
      );
      assert_eq!(answer.recovery, Recovery::new(2, 1), "{text}");
    }
  }

  #[test]
  fn a_refusal_inside_a_module_keeps_the_module_around_what_it_completed() {
    for (what, limits, text) in [
      (
        "the nesting ceiling",
        LosslessLimits::with_max_nesting_depth(4),
        "mod m { scalar A type T { f: [[[[[[Int]]]]]] } scalar Z }",
      ),
      (
        "the token budget",
        LosslessLimits::default().with_max_tokens(16),
        "mod m { scalar A scalar B scalar C scalar D scalar Z }",
      ),
    ] {
      let answer = read_with(&Lossless::with_limits(limits), text);
      assert!(answer.errors, "{what}");

      let inside = scalars(&module(&answer.tree, "mod m"));
      assert_eq!(
        inside.first().map(String::as_str),
        Some("scalar A"),
        "{what}: the item completed before the refusal is in the module"
      );
      let everywhere = answer
        .tree
        .descendants()
        .filter(|node: &SyntaxNode| node.kind() == K::ScalarTypeDefinition)
        .count();
      assert_eq!(
        inside.len(),
        everywhere,
        "{what}: an item stands outside the module"
      );
      assert!(
        !inside.iter().any(|scalar: &String| scalar == "scalar Z"),
        "{what}: the document went on past the refusal"
      );
    }
  }

  /// A refusal that ends the document while the lexer reads on. Both cases above stop the lexer,
  /// after which nothing could be read whatever a body did, so neither tells a body that returns
  /// a refusal from one that catches it. Here the parse's own nesting ceiling refuses, under
  /// three bodies whose junk keeps the lexer's tally low.
  ///
  /// The ceiling and the counts, from `depth::descend` and the limits:
  ///
  /// - `with_max_nesting_depth(3)` gives the parse `parse_ceiling()`, 3, which is below
  ///   `HARD_MAX`, and every body takes one frame through `descend` before it reads its `{`.
  ///   Nothing else in this text descends, so the bodies of `a`, `b` and `c` are the three
  ///   frames, and `d`'s, the fourth, is refused before its `{` is read.
  /// - The lexer's tally is blind to which closer answers which opener, and each `)` undoes the
  ///   `{` in front of it: the tally reads 1 after each body's `{` and 0 after each `)`, never
  ///   more than 1, so the lexer's own ceiling of 3 never trips.
  /// - Each `)` is junk between two items, one report each, and `descend` reports the refusal:
  ///   four. The refusal ends the document, so no body on the way out reports anything, and
  ///   nothing after it is read: `D`, inside the refused body, and `C`, `B` and `Z` after it.
  ///
  /// A body that caught the refusal as an ordinary failure would skip to its next item and read
  /// on: it would parse `C`, `B` and `Z`.
  #[test]
  fn a_refusal_with_the_lexer_still_reading_ends_the_document_inside_the_modules() {
    let limits = LosslessLimits::with_max_nesting_depth(3);
    assert_eq!(limits.parse_ceiling(), 3);
    let text = "mod a { ) mod b { ) mod c { ) mod d { scalar D } } scalar C } scalar B } scalar Z";
    let answer = read_with(&Lossless::with_limits(limits), text);
    assert!(answer.errors);
    // Each module keeps its node inside the one it was written in, and only `d`'s body, the one
    // past the ceiling, was not entered.
    for (name, depth) in [("mod a", 1), ("mod b", 2), ("mod c", 3), ("mod d", 4)] {
      let m = module(&answer.tree, name);
      assert_eq!(
        m.ancestors()
          .filter(|node: &SyntaxNode| node.kind() == K::ModDefinition)
          .count(),
        depth,
        "{name}: not inside the modules around it"
      );
      assert_eq!(
        m.children_with_tokens()
          .any(|element| element.kind() == K::LBrace),
        name != "mod d",
        "{name}: a body entered past the ceiling, or not entered under it"
      );
    }
    assert!(
      !answer
        .tree
        .descendants()
        .any(|node: SyntaxNode| node.kind() == K::ScalarTypeDefinition),
      "an item after the refusal was read"
    );
    assert_eq!(
      answer.reports, 4,
      "one report for each `)` and one for the refusal"
    );
  }

  // ── The reverse direction ──────────────────────────────────────────────────────────────────

  #[test]
  fn a_closer_after_a_description_still_closes_the_module() {
    let answer = read("mod m { \"doc\" } scalar Y");
    assert!(answer.errors);
    let m = module(&answer.tree, "mod m");
    assert!(scalars(&m).is_empty());
    assert!(holds_a_closing_brace(&m), "the module lost its `}}`");
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Y"]);
    assert_eq!(answer.document.definitions().len(), 2);
    assert!(items(&answer.document.definitions()[0]).is_empty());
  }

  #[test]
  fn a_closer_after_extend_still_closes_the_module() {
    let answer = read("mod m { extend } scalar Y");
    assert!(answer.errors);
    let m = module(&answer.tree, "mod m");
    assert!(scalars(&m).is_empty());
    assert!(holds_a_closing_brace(&m), "the module lost its `}}`");
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Y"]);
    assert_eq!(answer.document.definitions().len(), 2);
    assert!(items(&answer.document.definitions()[0]).is_empty());
  }

  /// `text` through the two roots a module is an entry of, the mixed root and the type-system
  /// root, as each root's name, its tree and where its parse reported an error, each tree held to
  /// round-trip. Through the doors rather than the layer, which reads the type-system root alone.
  fn at_both_roots(text: &str) -> [(&'static str, SyntaxNode, Vec<Range<usize>>); 2] {
    [
      ("mixed", parse_document(text)),
      ("type-system", parse_type_system_document(text)),
    ]
    .map(|(root, parse): (&'static str, Parse)| {
      let tree: SyntaxNode = parse.syntax();
      assert_eq!(
        tree.text().to_string(),
        text,
        "{root}: the tree spells its text"
      );
      let errors: Vec<Range<usize>> = parse
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .map(|diagnostic| diagnostic.span())
        .collect();
      (root, tree, errors)
    })
  }

  /// With no brace open no block stands around the item, so a closer after its description or
  /// its `extend` closes nothing the item stands in: the dispatcher takes it, with its one report.
  /// A dispatcher that left it would have the root's loop report the same token a second time.
  #[test]
  fn a_closer_after_a_description_or_extend_at_a_root_is_one_report() {
    for opener in ["\"d\"", "extend"] {
      for closer in ["}", ")", "]", ">"] {
        let text = format!("{opener} {closer}");
        let reported: Range<usize> = text.len() - closer.len()..text.len();
        for (root, _, errors) in at_both_roots(&text) {
          assert_eq!(
            errors,
            core::slice::from_ref(&reported),
            "{text} at the {root} root"
          );
        }
      }
    }
  }

  /// Inside a module a brace is open, so a `}` after a description or an `extend` is left to the
  /// body: one report, and the `}` is the module's own. A dispatcher that took it would cost no
  /// second report, since a body whose brace an item consumed is over, but the module would hold
  /// no `}` of its own.
  #[test]
  fn a_modules_brace_after_a_description_or_extend_is_one_report_and_its_closer() {
    for text in ["mod m { \"d\" }", "mod m { extend }"] {
      let reported: Range<usize> = text.len() - 1..text.len();
      for (root, tree, errors) in at_both_roots(text) {
        assert_eq!(
          errors,
          core::slice::from_ref(&reported),
          "{text} at the {root} root"
        );
        assert!(
          holds_a_closing_brace(&module(&tree, "mod m")),
          "{text} at the {root} root: the module lost its `}}`"
        );
      }
    }
  }

  /// A `)`, `]` or `>` closes no block an item stands in, even inside a module: after a
  /// description or an `extend` the dispatcher takes it with one report, and the item after it
  /// stays in the module. A dispatcher that left it would have the body report it again as junk.
  #[test]
  fn a_bracket_after_a_description_or_extend_in_a_module_is_one_report() {
    for opener in ["\"d\"", "extend"] {
      for closer in [")", "]", ">"] {
        let text = format!("mod m {{ {opener} {closer} scalar X }}");
        let at: usize = text.find(closer).expect("the text holds its closer");
        let reported: Range<usize> = at..at + closer.len();
        for (root, tree, errors) in at_both_roots(&text) {
          assert_eq!(
            errors,
            core::slice::from_ref(&reported),
            "{text} at the {root} root"
          );
          let m = module(&tree, "mod m");
          assert_eq!(scalars(&m), ["scalar X"], "{text} at the {root} root");
          assert!(holds_a_closing_brace(&m), "{text} at the {root} root");
        }
      }
    }
  }

  // ── Depth and counting ─────────────────────────────────────────────────────────────────────

  #[test]
  fn a_broken_item_in_a_nested_module_costs_only_that_item() {
    let answer = read(
      "mod a { scalar A1 mod b { scalar B1 mod c { scalar C1 type T { f } scalar C2 } scalar B2 } \
       scalar A2 }",
    );
    assert!(answer.errors);
    assert_eq!(
      scalars(&module(&answer.tree, "mod a")),
      ["scalar A1", "scalar A2"]
    );
    assert_eq!(
      scalars(&module(&answer.tree, "mod b")),
      ["scalar B1", "scalar B2"]
    );
    assert_eq!(
      scalars(&module(&answer.tree, "mod c")),
      ["scalar C1", "scalar C2"]
    );
    assert!(scalars(&top(&answer.tree)).is_empty());
    // Three modules and six scalars.
    assert_eq!(answer.recovery.projected(), 9);
    assert!(!answer.recovery.is_complete());
  }

  #[test]
  fn a_keyword_named_field_in_wreckage_restarts_nothing() {
    let answer = read("mod m { type T { f type: Int } scalar W }");
    assert!(answer.errors);
    assert_eq!(kinds(&top(&answer.tree)), [K::ModDefinition]);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar W"]);
    assert!(
      !answer
        .tree
        .descendants()
        .any(|node: SyntaxNode| node.kind() == K::ObjectTypeDefinition),
      "the `type` inside the wreckage started a definition"
    );
  }

  #[test]
  fn a_module_with_a_broken_header_is_one_skipped_element() {
    let answer = read("mod { scalar S } scalar Z");
    assert!(answer.errors);
    // The tree keeps the scalar where it was written. The document has no place for it: a
    // module with no name is no item, and what is inside it is not counted.
    assert_eq!(scalars(&module(&answer.tree, "mod {")), ["scalar S"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
    assert_eq!(answer.document.definitions().len(), 1);
    assert_eq!(answer.recovery, Recovery::new(1, 1));
  }

  #[test]
  fn recovery_counts_every_item_at_every_depth() {
    let text = "use ^::dep::Id mod m { scalar X mod n { scalar Y } } scalar Z";
    let lossless = read(text);
    assert!(!lossless.errors);
    let syntactic = Syntactic::new()
      .type_system_document(text, &mut ())
      .expect("the syntactic layer reads the document");
    // The `use` item, two modules and three scalars.
    assert_eq!(lossless.recovery, Recovery::new(6, 0));
    assert_eq!(lossless.recovery, syntactic.recovery());
  }

  #[test]
  fn skipped_counts_inside_modules() {
    for (what, text, recovery) in [
      (
        "an item that does not project",
        "mod m { scalar X extend type T }",
        Recovery::new(2, 1),
      ),
      (
        "a recovery hole",
        "mod m { scalar X ! }",
        Recovery::new(2, 1),
      ),
      (
        "a nested module whose header does not project",
        "mod m { scalar X mod { scalar S } }",
        Recovery::new(2, 1),
      ),
      (
        "a token that is no item",
        "mod m { \"doc\" }",
        Recovery::new(1, 1),
      ),
      ("a gap tile", "mod m { \"oops", Recovery::new(1, 1)),
    ] {
      let answer = read(text);
      assert!(answer.errors, "{what}");
      assert_eq!(answer.document.definitions().len(), 1, "{what}");
      assert_eq!(answer.recovery, recovery, "{what}");
    }

    // The string that never ends runs to the end of the text, so no token is lexed from it: its
    // bytes are a gap tile, and the tile is the module's, behind the space the body's peek read.
    let answer = read("mod m { \"oops");
    assert!(
      module(&answer.tree, "mod m")
        .children_with_tokens()
        .any(|element| element.kind() == K::Gap),
      "the gap tile is not the module's"
    );
  }

  // ── A missing component, and the definition after it ───────────────────────────────────────

  #[test]
  fn a_missing_field_type_keeps_the_definition_after_it() {
    for text in [
      MISSING_FIELD_TYPE,
      // An argument's type, an argument's value, a default, a list's element type and a generic
      // list's first argument.
      "type T { f(a: ): Int } scalar S",
      "type T @d(a: ) { f: Int } scalar S",
      "input I { a: Int = } scalar S",
      "type T { f: [] } scalar S",
      "type T { f: Box<> } scalar S",
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
      assert_eq!(answer.document.definitions().len(), 1, "{text}");
      assert_eq!(answer.recovery, Recovery::new(1, 1), "{text}");
    }
  }

  // ── Brackets that are not closed, and braces that do not balance ───────────────────────────

  #[test]
  fn an_unclosed_parenthesis_inside_a_module_does_not_move_its_end() {
    let answer = read("mod m { type T { f(a: Int: Int } scalar Y } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar Y"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
  }

  #[test]
  fn an_unclosed_angle_between_items_does_not_move_the_modules_end() {
    let answer = read("mod m { < scalar X } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar X"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
  }

  #[test]
  fn an_unclosed_bracket_between_items_does_not_move_the_modules_end() {
    let answer = read("mod m { [ scalar X } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar X"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
  }

  #[test]
  fn an_unclosed_angle_in_a_type_keeps_the_next_item_in_the_module_and_the_next_definition_out() {
    let answer = read("mod m { type T { f: Box<Int } scalar Y } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar Y"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
    // The list's `>` has no image, so `T` is kept too: the module, `T`, `Y` and `Z`.
    assert_eq!(answer.recovery, Recovery::new(4, 0));
  }

  #[test]
  fn an_unclosed_bracket_in_a_type_keeps_the_next_item_in_the_module_and_the_next_definition_out() {
    let answer = read("mod m { type T { f: [Int } scalar Y } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar Y"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
    assert_eq!(answer.recovery, Recovery::new(4, 0));
  }

  #[test]
  fn an_unclosed_list_keeps_the_definition_after_it() {
    for text in [
      "type T { f: Box<Int } scalar S",
      "type T { f: [Int } scalar S",
      "type T { f: Int @d(a: 1 } scalar S",
      "type T { f: Int @d(a: [1 } scalar S",
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
      // A closer has no image, so `T` is kept beside `S`.
      assert_eq!(answer.recovery, Recovery::new(2, 0), "{text}");
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
    let operations: Vec<String> = tree
      .children()
      .find(|node: &SyntaxNode| node.kind() == K::Document)
      .expect("the document's node")
      .children()
      .filter(|node: &SyntaxNode| node.kind() == K::OperationDefinition)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(operations, ["query R { g }"]);
  }

  #[test]
  fn a_stray_closing_parenthesis_in_a_broken_item_does_not_end_the_module_early() {
    let answer = read("mod m { type T { ) f } scalar W } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar W"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
  }

  /// The text that once ended a body by its rule 1, which no text reaches now. The two `(` are
  /// junk of the argument list, and the skip that takes them stops before the first `}`, which
  /// closes a brace the skip did not open: the list leaves it to the fields, the field fails on
  /// it, and the body's own skip takes it as a brace the failed item left open. The second `}` is
  /// the module's. A skip blind to which closer answers which opener took both, and the module's
  /// node held no `}` of its own.
  #[test]
  fn a_skip_in_an_item_leaves_the_braces_it_did_not_open() {
    let answer = read("mod m { type T { f(a: Int ( ( } } ) scalar W");
    assert!(answer.errors);
    let m = module(&answer.tree, "mod m");
    assert_eq!(m.text().to_string(), "mod m { type T { f(a: Int ( ( } }");
    assert!(holds_a_closing_brace(&m), "the module lost its `}}`");
    assert!(scalars(&m).is_empty());
    assert_eq!(scalars(&top(&answer.tree)), ["scalar W"]);
  }

  #[test]
  fn unbalanced_braces_are_read_as_written() {
    // A `}` missing: the module's own closes `T`, and what follows is read inside the module.
    let text = "mod m { type T { f: Int scalar Y } scalar Z";
    let answer = read(text);
    assert!(answer.errors);
    let m = module(&answer.tree, "mod m");
    assert_eq!(usize::from(m.text_range().end()), text.len());
    assert_eq!(scalars(&m), ["scalar Z"]);
    assert_eq!(kinds(&top(&answer.tree)), [K::ModDefinition]);

    // A `}` too many: the module ends at the first, and the second is a stray at the top.
    let answer = read("mod m { scalar X } } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar X"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
    assert_eq!(answer.recovery, Recovery::new(3, 1));
  }

  #[test]
  fn a_scan_that_finds_nothing_gives_the_brace_count_back() {
    // Each skip crosses braces and finds no item to restart at, so it rewinds. A count that
    // outlived a rewind would read a brace fewer and take the last `}` for the module's own.
    let text = "mod m { type T { f { a } }";
    let answer = read(text);
    assert!(answer.errors);
    let m = module(&answer.tree, "mod m");
    assert!(!holds_a_closing_brace(&m));
    assert_eq!(usize::from(m.text_range().end()), text.len());
  }

  #[test]
  fn a_module_of_nothing_but_brackets_and_junk_ends() {
    let mut bodies: Vec<String> = [") ] > ( [ <", "( [ < ) ] >", "< ( [ > ) ]", "{", "{ { {"]
      .iter()
      .map(|body: &&str| (*body).to_string())
      .collect();
    bodies.push("! ".repeat(10_000));
    for body in &bodies {
      let text = format!("mod m {{ {body}");
      // `read` holds the round trip; the parse returning at all is the rest of the claim.
      assert!(read(&text).errors, "{}", &text[..text.len().min(32)]);
    }
  }

  // ── A skip closes no brace it did not open ─────────────────────────────────────────────────

  /// The external review's witness. Every delimiter balances in number.
  const WITNESS: &str = "mod m { scalar A @d((( } mod n { ))) scalar B }";

  /// The text each of `items` spans in `text`.
  fn spelled<'t>(text: &'t str, items: &[TypeSystemItem<&str>]) -> Vec<&'t str> {
    items
      .iter()
      .map(|item: &TypeSystemItem<&str>| &text[item.span().start()..item.span().end()])
      .collect()
  }

  /// The two `(` after the arguments' own are junk of that list. The skip that takes them stops
  /// before the `}`, which closes the brace of `m`, one the skip did not open: the list leaves
  /// it, `A` ends, and `m` closes on it. `n` is a module of its own and `B` its item, in the tree
  /// and in what the recovering projection answers, where `A`, broken, is not kept. A skip blind
  /// to which closer answers which opener took `( ( } mod n { ) )` and came back into `m` at the
  /// count it had left, so `B` was read in `m`, `n`'s `}` closed `m`, and `n` was no module.
  #[test]
  fn the_witness_keeps_each_item_in_the_module_its_braces_give_it() {
    let answer = read(WITNESS);
    assert!(answer.errors);
    assert_eq!(
      kinds(&top(&answer.tree)),
      [K::ModDefinition, K::ModDefinition]
    );
    let m = module(&answer.tree, "mod m");
    assert_eq!(m.text().to_string(), "mod m { scalar A @d((( }");
    assert_eq!(scalars(&m), ["scalar A @d((("]);
    let n = module(&answer.tree, "mod n");
    assert_eq!(n.text().to_string(), "mod n { ))) scalar B }");
    assert_eq!(scalars(&n), ["scalar B"]);

    let definitions = answer.document.definitions();
    assert_eq!(
      spelled(WITNESS, definitions),
      ["mod m { scalar A @d((( }", "mod n { ))) scalar B }"]
    );
    assert!(items(&definitions[0]).is_empty(), "`A` is kept");
    assert_eq!(spelled(WITNESS, items(&definitions[1])), ["scalar B"]);
  }

  /// The witness's kin: the same crossing behind other openers and in other lists. In each, `m`
  /// closes on the `}` its braces give it and `B` is an item of `n`.
  ///
  /// The first five refuse a skip blind to which closer answers which opener, and in none of them
  /// was `n` a module. With angles, with brackets behind a parenthesis, and in a definition's
  /// generic list, that skip came back into `m` and read `B` there. In a type's generic list it
  /// took the fields' `}` and `m`'s, came back into `m` behind `n`'s `{`, and closed `m` on `n`'s
  /// `}`. In a field's argument list it took both as well and came back into nothing, so `m` had
  /// no `}` of its own and `B` was read at the root. The last one no skip ever reached: each `[`
  /// there is the head of a list type, and each list leaves the `}` of the block around it by its
  /// own test. It refuses a list that took that `}` as junk.
  #[test]
  fn a_skip_crosses_no_brace_whatever_it_has_open() {
    for (what, text, own) in [
      (
        "angles in an argument list",
        "mod m { scalar A @d(<<< } mod n { >>>) scalar B }",
        "mod m { scalar A @d(<<< }",
      ),
      (
        "brackets behind a parenthesis",
        "mod m { scalar A @d(([[ } mod n { ]]) ) scalar B }",
        "mod m { scalar A @d(([[ }",
      ),
      (
        "a definition's generic list",
        "mod m { type T<<< } mod n { >>> scalar B }",
        "mod m { type T<<< }",
      ),
      (
        "a type's generic list",
        "mod m { type T { f: Box<<< } } mod n { >>> scalar B }",
        "mod m { type T { f: Box<<< } }",
      ),
      (
        "a field's argument list",
        "mod m { type T { f((( } } mod n { ))) scalar B }",
        "mod m { type T { f((( } }",
      ),
      (
        "list types, which no skip reaches",
        "mod m { type T { f: [[[Int } } mod n { ]]] scalar B }",
        "mod m { type T { f: [[[Int } }",
      ),
    ] {
      let answer = read(text);
      assert!(answer.errors, "{what}");
      assert_eq!(
        kinds(&top(&answer.tree)),
        [K::ModDefinition, K::ModDefinition],
        "{what}"
      );
      let m = module(&answer.tree, "mod m");
      assert_eq!(m.text().to_string(), own, "{what}");
      assert!(holds_a_closing_brace(&m), "{what}");
      let n = module(&answer.tree, "mod n");
      assert_eq!(scalars(&n), ["scalar B"], "{what}");
      assert!(holds_a_closing_brace(&n), "{what}");
    }
  }

  /// The crossing out of a nested module into the one around it: `n` is written in `p`, after
  /// `m`, and that is where it stays. A skip blind to which closer answers which opener came back
  /// into `m`, read `B` there, and `n` was never a module.
  #[test]
  fn a_skip_crosses_no_brace_out_of_a_nested_module() {
    let answer = read("mod p { mod m { scalar A @d((( } mod n { ))) scalar B } }");
    assert!(answer.errors);
    assert_eq!(kinds(&top(&answer.tree)), [K::ModDefinition]);
    let p = module(&answer.tree, "mod p");
    assert_eq!(kinds(&p), [K::ModDefinition, K::ModDefinition]);
    let m = module(&answer.tree, "mod m");
    assert_eq!(m.text().to_string(), "mod m { scalar A @d((( }");
    let n = module(&answer.tree, "mod n");
    assert_eq!(n.text().to_string(), "mod n { ))) scalar B }");
    assert_eq!(scalars(&n), ["scalar B"]);
    assert!(holds_a_closing_brace(&p));
  }

  /// Two walks the review's examples named, which report a missing member and leave what stands
  /// there. Neither ever took the `}`: before the rule of the braces and since, `m` closes on its
  /// first `}`, and `X` is read at the root, after the junk there. A walk that took the `}` as
  /// junk and read on would come back into `m` behind the `{` the junk opens and read `X` there.
  #[test]
  fn a_walk_that_misses_a_member_leaves_the_brace() {
    for (text, own) in [
      (
        "mod m { union U = A | } | { | B scalar X }",
        "mod m { union U = A | }",
      ),
      (
        "mod m { type T implements } { f scalar X }",
        "mod m { type T implements }",
      ),
    ] {
      let answer = read(text);
      assert!(answer.errors, "{text}");
      let m = module(&answer.tree, "mod m");
      assert_eq!(m.text().to_string(), own, "{text}");
      assert_eq!(scalars(&top(&answer.tree)), ["scalar X"], "{text}");
    }
  }

  /// At a root a `}` is any production's, so a skip there stops before one as well, and the
  /// dispatcher takes it on its next turn: `n` after it is read as the module it is written as.
  /// A skip blind to which closer answers which opener took `( ( } mod n { ) )`, and `B` was read
  /// at the root.
  #[test]
  fn a_skip_at_a_root_leaves_the_brace_and_the_module_after_it() {
    let text = "( ( } mod n { ) ) scalar B }";
    for (root, tree, _) in at_both_roots(text) {
      let n = module(&tree, "mod n");
      assert_eq!(
        n.text().to_string(),
        "mod n { ) ) scalar B }",
        "{text} at the {root} root"
      );
      assert_eq!(scalars(&n), ["scalar B"], "{text} at the {root} root");
    }
  }

  // ── Through the analyzer ───────────────────────────────────────────────────────────────────

  #[cfg(feature = "analysis")]
  mod analysis {
    use smear::{
      analysis::graphqlx::{
        DefId, Layout, Limits, Model, ModuleId, Package, SourceFile, Tier, analyze_with,
      },
      parser::graphqlx::Lossless,
    };

    /// The names of the definitions `module` holds, in id order.
    fn names(model: &Model, module: ModuleId) -> Vec<&str> {
      model
        .module(module)
        .defs()
        .into_iter()
        .map(|id: DefId| model.def(id).name())
        .collect()
    }

    #[test]
    fn a_broken_module_never_declares_its_items_at_the_files_root() {
      for (text, name, in_the_module) in [
        ("mod m { scalar X type T { f } }", "X", true),
        // A name the root must not gain whatever a module may do with it.
        ("mod m { scalar Int type T { f } }", "Int", false),
      ] {
        let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
          .with_files([SourceFile::new("schema.graphqlx", text)]);
        let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
        let model: &Model = analysis.model();
        let app_root: ModuleId = model.package(model.root_package()).root_module();
        assert!(
          !names(model, app_root).contains(&name),
          "{text}: the file's root declares `{name}`"
        );
        if in_the_module {
          let m: ModuleId = model.module_by_path("^::app::m").expect("the `mod m`");
          assert_eq!(names(model, m), [name], "{text}");
        }
      }
    }

    /// The external review's witness through the analyzer: `B` is declared in `n`, where its
    /// braces put it, and nothing named `B` in `m`. A skip blind to which closer answers which
    /// opener declared `m::B`, and no module `n`.
    #[test]
    fn the_witness_declares_each_item_in_the_module_its_braces_give_it() {
      let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
        .with_files([SourceFile::new("schema.graphqlx", super::WITNESS)]);
      let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
      let model: &Model = analysis.model();
      let m: ModuleId = model.module_by_path("^::app::m").expect("the `mod m`");
      let n: ModuleId = model.module_by_path("^::app::n").expect("the `mod n`");
      assert!(!names(model, m).contains(&"B"), "`m` declares `B`");
      assert_eq!(names(model, n), ["B"]);
    }
  }
}
