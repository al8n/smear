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

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
#[path = "support/braces.rs"]
mod braces;

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
      lossless::{
        Parse, SyntaxNode, parse_document, parse_document_with_limits, parse_type_system_document,
        parse_type_system_document_with_limits,
      },
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
  ///
  /// Of a module, that is the module's own closer in every text this file asks it of, and it is
  /// not in general: being a direct child is no proof that a brace is the module's own, and
  /// `support/braces.rs` says why and tells the braces by count. An item that fails leaves the
  /// tokens it took to the module, its `{` among them, but no production of an item fails after
  /// taking a `}` of its own unless a refusal stops a `use` tree after its group, and none of these
  /// texts has one.
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
      // The module never closes, and nothing but what the refusal left unread follows it.
      let held =
        super::braces::faithful(&answer.tree).unwrap_or_else(|why| panic!("{what}: {why}"));
      assert_eq!(held.read, 1, "{what}");
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
    // Three modules that never close, one inside the other, and the refused one inside them, with
    // nothing but what the refusal left unread after them.
    let held = super::braces::faithful(&answer.tree).unwrap_or_else(|why| panic!("{why}"));
    assert_eq!(held.read, 4);
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
    // The module never closes, at the end of the text.
    let held = super::braces::faithful(&answer.tree).unwrap_or_else(|why| panic!("{why}"));
    assert_eq!(held.read, 1);

    // A `}` too many: the module ends at the first, and the second is a stray at the top.
    let answer = read("mod m { scalar X } } scalar Z");
    assert!(answer.errors);
    assert_eq!(scalars(&module(&answer.tree, "mod m")), ["scalar X"]);
    assert_eq!(scalars(&top(&answer.tree)), ["scalar Z"]);
    assert_eq!(answer.recovery, Recovery::new(3, 1));
    let held = super::braces::faithful(&answer.tree).unwrap_or_else(|why| panic!("{why}"));
    assert_eq!(held.read, 1);
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

  /// The document's own node, whichever root built it.
  fn document_of(tree: &SyntaxNode) -> SyntaxNode {
    tree
      .children()
      .find(|node: &SyntaxNode| matches!(node.kind(), K::Document | K::TypeSystemDocument))
      .expect("the document's node")
  }

  /// The witnesses of the review's third and fourth rounds, and the third's kin. The list has taken
  /// its own `(` and the second `(` is junk; the skip that takes it opens a brace, and closers
  /// inside that brace close the parentheses the skip opened. One depth over every pair, the skip's
  /// count, comes back to zero exactly before the list's own `)`, so the skip stops there, the list
  /// closes on it, `A` keeps its directive and `B` is read whole.
  ///
  /// Two versions of the scan lost `B` here, each by taking the list's own `)` to bring a count of
  /// its own to zero, after which the list read `scalar` as an argument's name and failed at `B`.
  /// The scan of round two (005e784c) counted the other pairs only outside its braces and never
  /// saw the closer inside one: the first four rows refuse it, the closer a `)`, a `]` or a `>`,
  /// and in the fourth a brace inside a brace. The scan of round three (fc85192c) counted them
  /// inside its braces too but let the count saturate there, so the `( (` after the brace raised
  /// it past one depth: the last row refuses it.
  #[test]
  fn a_skip_keeps_the_lists_own_closer() {
    for (text, a) in [
      ("scalar A @d(( { ) } ) scalar B", "scalar A @d(( { ) } )"),
      ("scalar A @d(( { ] } ) scalar B", "scalar A @d(( { ] } )"),
      ("scalar A @d(( { > } ) scalar B", "scalar A @d(( { > } )"),
      (
        "scalar A @d(( { { ) } } ) scalar B",
        "scalar A @d(( { { ) } } )",
      ),
      (
        "scalar A @d(( { ) ) ( ( } ) ) scalar B",
        "scalar A @d(( { ) ) ( ( } ) )",
      ),
    ] {
      for (root, tree, _) in at_both_roots(text) {
        assert_eq!(
          scalars(&document_of(&tree)),
          [a, "scalar B"],
          "{text} at the {root} root"
        );
      }
    }
  }

  /// The two witnesses of [`a_skip_keeps_the_lists_own_closer`] that each lost `B`, inside a
  /// module: `B` is an item of `m`, and `m` is faithful to the braces as written. A scan that lost
  /// `B` lost it here too, the body's skip taking it after the list's failure.
  #[test]
  fn a_skip_keeps_the_lists_own_closer_in_a_module() {
    for (text, a) in [
      (
        "mod m { scalar A @d(( { ) } ) scalar B }",
        "scalar A @d(( { ) } )",
      ),
      (
        "mod m { scalar A @d(( { ) ) ( ( } ) ) scalar B }",
        "scalar A @d(( { ) ) ( ( } ) )",
      ),
    ] {
      for (root, tree, _) in at_both_roots(text) {
        assert_eq!(
          scalars(&module(&tree, "mod m")),
          [a, "scalar B"],
          "{text} at the {root} root"
        );
        let held = super::braces::faithful(&tree)
          .unwrap_or_else(|why| panic!("{text} at the {root} root: {why}"));
        assert_eq!(
          (held.read, held.broken),
          (1, 1),
          "{text} at the {root} root"
        );
      }
    }
  }

  /// One depth over every pair can come back to zero inside a brace the skip opened, a `)` closing
  /// the `{`, and the skip then stops at the next restart point there, the brace left open behind
  /// it, as the scan before the rule of the braces did and as this one does on purpose. At a root
  /// that keeps what the brace holds: the skip stops at `scalar B`, the root reads `B`, takes the
  /// stray `}` and reads `C`. A scan that never stopped inside a brace it opened (fc85192c) took
  /// `scalar B }` into its hole, and lost `B`.
  ///
  /// The same stop in an argument list is the old answer, kept on purpose and pinned here: the skip
  /// stops at `x` inside its brace, the list reads `x` as an argument's name and fails on the `}`
  /// after it, `A` is lost with it, and the root reads `B` after skipping to it.
  #[test]
  fn a_skip_stops_inside_a_brace_it_opened_as_it_always_did() {
    for (text, kept) in [
      ("( { ) ) scalar B } scalar C", &["scalar B", "scalar C"][..]),
      ("scalar A @d(( { ) ) x } ) scalar B", &["scalar B"][..]),
    ] {
      for (root, tree, _) in at_both_roots(text) {
        assert_eq!(
          scalars(&document_of(&tree)),
          kept,
          "{text} at the {root} root"
        );
      }
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

  // ── A skip that finds no stop keeps what it kept before ────────────────────────────────────

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
    for (root, tree, _) in at_both_roots(text) {
      assert_eq!(
        written(&tree, K::ObjectTypeDefinition),
        ["type T { f: Int }"],
        "{text} at the {root} root"
      );
      assert_eq!(
        scalars(&document_of(&tree)),
        ["scalar S"],
        "{text} at the {root} root"
      );
    }
  }

  /// The same skip in an argument list. The second `(` is junk of the list, and the skip from it
  /// crosses `b: { c: 2 }`, takes the `}` of the block around the list, one it did not open, and
  /// finds no restart point after that. No match, so the fallback takes the `(`, as it did before
  /// the rule of the braces; the list then reads `b: { c: 2 }` as its argument, meets the `}`,
  /// reports its `)` missing and leaves the brace, and the block closes on it.
  ///
  /// The three versions of the scan that lost `T` above stopped before that `}` and took
  /// `( b: { c: 2 }` into one hole, and the list read no `b`.
  #[test]
  fn a_skip_in_a_list_that_finds_no_stop_keeps_what_it_kept_before() {
    let text = "type T { f: Int @d(a: 1 ( b: { c: 2 } }";
    for (root, tree, _) in at_both_roots(text) {
      assert_eq!(
        written(&tree, K::Argument),
        ["a: 1", "b: { c: 2 }"],
        "{text} at the {root} root"
      );
      assert_eq!(
        written(&tree, K::ObjectTypeDefinition),
        [text],
        "{text} at the {root} root"
      );
    }

    let text = "mod m { scalar A @d(a: 1 ( b: { c: 2 } }";
    for (root, tree, _) in at_both_roots(text) {
      assert_eq!(
        written(&tree, K::Argument),
        ["a: 1", "b: { c: 2 }"],
        "{text} at the {root} root"
      );
      let m = module(&tree, "mod m");
      assert_eq!(m.text().to_string(), text, "{text} at the {root} root");
      assert!(holds_a_closing_brace(&m), "{text} at the {root} root");
      let held = super::braces::faithful(&tree)
        .unwrap_or_else(|why| panic!("{text} at the {root} root: {why}"));
      assert_eq!(
        (held.read, held.broken),
        (1, 1),
        "{text} at the {root} root"
      );
    }
  }

  // ── The braces hold whatever stops a skip ──────────────────────────────────────────────────

  /// Texts whose skips take a `}` they did not open, read under the limits that can stop them.
  ///
  /// The first four are the review's: the round-two witness, the list that finds no stop and the
  /// root's witness each inside a module, and a module inside a module. The fifth is the round-two
  /// witness with a deeper run behind the `}`, so that a nesting ceiling can stop its skip after
  /// the brace as well.
  const UNDER_LIMITS: [&str; 5] = [
    WITNESS,
    "mod m { scalar A @d(a: 1 ( b: { c: 2 } }",
    "mod m { ( type T { f: Int } ( } scalar S }",
    "mod a { mod b { scalar A @d(( } } scalar B }",
    "mod m { scalar A @d((( } mod n { ((( ))) )) scalar B }",
  ];

  /// `text` through both GraphQLx roots under `limits`, named `limit` in every message: each tree
  /// spells its text, and every module in it is faithful to the braces as written.
  fn faithful_under(text: &str, limits: LosslessLimits, limit: &str) {
    for (root, parse) in [
      ("mixed", parse_document_with_limits(text, limits)),
      (
        "type-system",
        parse_type_system_document_with_limits(text, limits),
      ),
    ] {
      let tree: SyntaxNode = parse.syntax();
      assert_eq!(
        tree.text().to_string(),
        text,
        "{text} at the {root} root under {limit}: the tree spells its text"
      );
      super::braces::faithful(&tree)
        .unwrap_or_else(|why| panic!("{text} at the {root} root under {limit}: {why}"));
    }
  }

  /// How many lexemes `text` is, each byte of trivia one of them: the tokens of its tree.
  fn lexemes(text: &str) -> usize {
    parse_document(text)
      .syntax()
      .descendants_with_tokens()
      .filter(|element| element.as_token().is_some())
      .count()
  }

  /// The most brackets of every kind open at once in `text`, counted as the lexer's tally counts
  /// them: blind to which closer answers which opener, and never below zero.
  fn deepest(text: &str) -> usize {
    let mut open = 0usize;
    let mut most = 0usize;
    for c in text.chars() {
      match c {
        '(' | '[' | '{' | '<' => {
          open += 1;
          most = most.max(open);
        }
        ')' | ']' | '}' | '>' => open = open.saturating_sub(1),
        _ => {}
      }
    }
    most
  }

  /// The rule of the braces at every token budget, from none to one past the text's lexemes.
  ///
  /// `with_max_tokens(n)` lets the lexer produce `n` lexemes, every byte of trivia among them, and
  /// refuses the next. In [`WITNESS`] the argument list takes its own `(` as lexeme 13 and peeks the
  /// junk `(` at 14; the skip from there takes 15 and 16, then `m`'s `}` as 17, and lexes on to the
  /// `)` that stops it, lexeme 27. So budgets 17 to 26 stop that skip after it has taken the `}`.
  /// 1e4023b8 kept what the skip had taken, `m`'s `}` among it, so `m` held no `}` of its own; from
  /// budget 19 on, where `mod` follows that `}` inside `m`, the walk of `support/braces.rs` fails on
  /// it. Here the skip is taken back and `m` closes on its own `}`, which
  /// [`a_limit_after_a_brace_the_skip_did_not_open_leaves_it_to_its_module`] pins at 17. The fifth
  /// text reaches the same edge at budgets 17 to 35 and the fourth at 22 and 23. The second's `}` is
  /// its last lexeme, so no budget stops its skip after it, and the third's junk is a module body's,
  /// which the body's own skip takes.
  #[test]
  fn the_braces_hold_under_every_token_budget() {
    for text in UNDER_LIMITS {
      for max in 0..=lexemes(text) + 1 {
        faithful_under(
          text,
          LosslessLimits::default().with_max_tokens(max),
          &format!("max_tokens({max})"),
        );
      }
    }
  }

  /// The same at every nesting ceiling, from none to one past the most brackets the text opens.
  ///
  /// The lexer's bracket tally refuses a bracket that would put more than the ceiling open, and a
  /// skip that takes brackets can meet it. In the fifth text the tally reads 4 at the second junk
  /// `(` and at `n`'s `{`, and 5, 6 and 7 at the three `(` behind it, all inside the skip from the
  /// first junk `(`, so ceilings 4, 5 and 6 stop that skip after it has taken `m`'s `}`. In the
  /// first four nothing a skip takes behind such a `}` opens more brackets than were open in front
  /// of it, so no ceiling stops a skip there. The parse's own ceiling is the same number, and the
  /// only frames in front of the skip are the module's body and the argument list.
  #[test]
  fn the_braces_hold_under_every_nesting_ceiling() {
    for text in UNDER_LIMITS {
      for max in 0..=deepest(text) + 1 {
        faithful_under(
          text,
          LosslessLimits::with_max_nesting_depth(max),
          &format!("max_nesting_depth({max})"),
        );
      }
    }
  }

  /// The same at every produce-event budget up to four times the text's lexemes, room for what a
  /// parse that takes a skip back lexes twice.
  ///
  /// This budget is the one no rollback refunds. A skip it stops after the `}` is taken back like
  /// the others, and the second scan, which would have to lex again what the budget no longer pays
  /// for, is refused before it takes anything, so the parse ends in front of the junk with every
  /// module around it unclosed. In [`WITNESS`] nothing is lexed twice before the skip, so budgets
  /// 17 to 26 stop it after the `}`, as they do with `max_tokens`.
  #[test]
  fn the_braces_hold_under_every_produce_event_budget() {
    for text in UNDER_LIMITS {
      for max in 0..=4 * lexemes(text) {
        faithful_under(
          text,
          LosslessLimits::default().with_max_produce_events(max),
          &format!("max_produce_events({max})"),
        );
      }
    }
  }

  /// [`WITNESS`] at `with_max_tokens(17)`, the first budget at which 1e4023b8 broke the rule of the
  /// braces. The limit stops the skip from the junk `(` as it reads lexeme 18, the space after the
  /// `}` it has taken. 1e4023b8 kept that prefix: `m`'s `}` stood inside `A`, and `m`'s text read the
  /// same with no `}` of its own. Here the skip is taken back and scanned again, the hole of the two
  /// parentheses is kept in `A` in front of the `}`, the list leaves the `}`, and `m` closes on it.
  /// The parse then reads the space again, meets the limit there, and reports it once.
  #[test]
  fn a_limit_after_a_brace_the_skip_did_not_open_leaves_it_to_its_module() {
    let limits = LosslessLimits::default().with_max_tokens(17);
    let brace = WITNESS.find('}').expect("the witness holds a `}`");
    let refused: Range<usize> = brace + 1..brace + 2;
    for (root, parse) in [
      ("mixed", parse_document_with_limits(WITNESS, limits)),
      (
        "type-system",
        parse_type_system_document_with_limits(WITNESS, limits),
      ),
    ] {
      let tree: SyntaxNode = parse.syntax();
      assert_eq!(tree.text().to_string(), WITNESS, "at the {root} root");
      let m = module(&tree, "mod m");
      assert_eq!(
        m.text().to_string(),
        "mod m { scalar A @d((( }",
        "at the {root} root"
      );
      assert!(
        holds_a_closing_brace(&m),
        "at the {root} root: `m` holds no `}}` of its own"
      );
      assert_eq!(scalars(&m), ["scalar A @d((("], "at the {root} root");
      let a = m
        .children()
        .find(|node: &SyntaxNode| node.kind() == K::ScalarTypeDefinition)
        .expect("`A` is an item of `m`");
      assert_eq!(
        written(&a, K::Error),
        ["(("],
        "at the {root} root: the hole of the two parentheses"
      );
      let reports = parse
        .diagnostics()
        .iter()
        .filter(|diagnostic| {
          diagnostic.severity() == Severity::Error && diagnostic.span() == refused
        })
        .count();
      assert_eq!(reports, 1, "at the {root} root: the limit's report");
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
