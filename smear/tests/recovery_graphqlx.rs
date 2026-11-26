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
//! layer lends its sink, which `layers_handover_graphqlx.rs` holds, but the codes of a string its
//! line ends, which tell the lexer's error from the parser's: where a test counts what a mistake
//! costs, it counts the error diagnostics of the tree.

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
    diagnostic::Diagnose,
    lexer::limits::LosslessLimits,
    parser::graphqlx::{
      Lossless, Recovery, Syntactic,
      ast::{TypeSystemDocument, TypeSystemItem},
      error::GraphqlxError,
      kinds::SyntaxKind as K,
      lossless::{
        GraphqlxLosslessErrorValue, Parse, SyntaxNode, parse_document, parse_document_with_limits,
        parse_executable_document, parse_type_system_document,
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

  /// A field's arguments definition that meets the `}` of the type around it reports its `)` as
  /// missing and leaves the brace. The field then fails for want of its `:`, and the type with it,
  /// and the root takes the brace as a stray and reads the next definition whole. It refuses the
  /// removal of that arm in `arguments_definition`, without which the list takes the brace as junk
  /// and reads `type` as an argument's name, which fails at `R`.
  #[test]
  fn an_unclosed_arguments_definition_leaves_the_definition_after_the_brace() {
    let text = "type T { f(a: Int } type R { g: Int }";
    let answer = read(text);
    assert!(answer.errors, "{text}");
    let types: Vec<String> = top(&answer.tree)
      .children()
      .filter(|node: &SyntaxNode| node.kind() == K::ObjectTypeDefinition)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(types, ["type R { g: Int }"], "{text}");
  }

  // ── A string its line ends ─────────────────────────────────────────────────────────────────

  /// A type whose first field's deprecation reason has no closing quote on its line, and two
  /// fields after it, the second deprecated with a reason of its own.
  const UNCLOSED_REASON: &str = "type Query {\n  a: String @deprecated(reason: \"oops)\n  \
                                 b: Int\n  c: String @deprecated(reason: \"fine\")\n}\n";

  /// The codes of the errors the lossless layer lends a sink for `text`, in the order it lends
  /// them.
  fn codes(text: &str) -> Vec<String> {
    let mut lent: Vec<GraphqlxLosslessErrorValue<&str>> = Vec::new();
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

  /// GraphQL's twin, through this dialect's layer: an inline string ends at its line, so the second
  /// line's literal is `"oops)` alone, the one gap and the first error, the lexer's. The lines
  /// after it are tokens again, so `"fine"` is a string with both its quotes and no token but a
  /// line break runs over a line's end.
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
    assert_eq!(tokens(&answer.tree, K::InlineString), ["\"fine\""]);
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
    let types: Vec<String> = top(&answer.tree)
      .children()
      .filter(|node: &SyntaxNode| node.kind() == K::ObjectTypeDefinition)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(
      types,
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
        "mod m { scalar A @d(< < < } mod n { >>>) scalar B }",
        "mod m { scalar A @d(< < < }",
      ),
      (
        "brackets behind a parenthesis",
        "mod m { scalar A @d(([[ } mod n { ]]) ) scalar B }",
        "mod m { scalar A @d(([[ }",
      ),
      (
        "a definition's generic list",
        "mod m { type T< < < } mod n { >>> scalar B }",
        "mod m { type T< < < }",
      ),
      (
        "a type's generic list",
        "mod m { type T { f: Box< < < } } mod n { >>> scalar B }",
        "mod m { type T { f: Box< < < } }",
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

  /// The fifth external review's witness, `(((}))scalar B`, where GraphQLx reads it through this
  /// helper with a module's brace in it.
  ///
  /// A module's body hands its own junk to the body's skip, not to this helper, and
  /// `mod m { (((}))scalar B }` puts the module's own `}` where the witness's stands. So the witness
  /// stands behind an argument list: the list takes the first `(` as its own, the three after it
  /// are the witness's junk, and `m`'s `}` is the brace the skip did not open.
  const REPLAYED: &str = "mod m { scalar A @d((((}))scalar B }";

  /// [`REPLAYED`] at every produce-event budget from none to one past the least at which it reads
  /// `B`, held to the braces as written at both roots.
  ///
  /// Every space is a lexeme, and nothing is lexed twice before the skip. The list takes its own
  /// `(` as lexeme 13 and peeks the junk `(` at 14. The skip from there lexes 15 to 19, `m`'s `}` at
  /// 17, and stops at `scalar`, lexeme 20, so the parse has spent 20 events. The hole holds the
  /// `}`, so the attempt is declined, and the second scan lexes 14 to 17 again as events 21 to 24.
  /// Budgets 20 to 23 stop that second scan after a skip that found its restart point: it keeps
  /// nothing, `(`, `((` or `(((`, and `m` is left open. Budgets 17 to 19 stop the skip itself after
  /// the `}`, and the second scan is refused before it takes anything. From 24 on `m` closes on its
  /// own `}`. The root then reads `)`, `)`, `scalar`, the space and `B` as events 25 to 29, so `B`
  /// is read from 29 on, and the sweep runs to 30.
  #[test]
  fn the_braces_hold_while_a_skip_is_scanned_again_under_every_produce_event_budget() {
    for max in 0..=30 {
      faithful_under(
        REPLAYED,
        LosslessLimits::default().with_max_produce_events(max),
        &format!("max_produce_events({max})"),
      );
    }
  }

  /// The last token of `node` that is neither trivia nor text the parse never read: its text, its
  /// range, and whether it is a child of `node` itself.
  fn last_read(node: &SyntaxNode) -> (String, Range<usize>, bool) {
    let token = node
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
      .filter(|token| {
        !matches!(
          token.kind(),
          K::Space | K::Tab | K::Newline | K::Comma | K::Comment | K::Bom | K::Gap
        )
      })
      .last()
      .expect("the node holds a token the parse read");
    let range = token.text_range();
    (
      token.text().to_string(),
      usize::from(range.start())..usize::from(range.end()),
      token.parent().as_ref() == Some(node),
    )
  }

  /// The gap tiles anywhere in `node`, text the parse never read, each as it is written and where.
  fn gaps(node: &SyntaxNode) -> Vec<(String, Range<usize>)> {
    node
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

  /// [`WITNESS`] at `with_max_tokens(17)`, the first budget at which 1e4023b8 broke the rule of the
  /// braces. The limit stops the skip from the junk `(` as it reads lexeme 18, the space after the
  /// `}` it has taken. 1e4023b8 kept that prefix, so `m`'s `}` stood inside `A` and `m` held none
  /// of its own. Here the skip is taken back and scanned again, the hole of the two parentheses is
  /// kept in `A` in front of the `}`, the list leaves the `}`, and `m` closes on it. The parse then
  /// reads the space again, meets the limit there, and reports it once.
  ///
  /// What the parse never read, ` mod n { ))) scalar B }`, is one gap tile, and tokora tiles it at
  /// the token it trails, before the close after that token is read: it stands in `m`, after `m`'s
  /// own `}`. So `m`'s node runs to the end of the text, and its last token that is neither trivia
  /// nor text the parse never read is its own `}`.
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
      assert_eq!(m.text().to_string(), WITNESS, "at the {root} root");
      assert_eq!(
        last_read(&m),
        (String::from("}"), brace..brace + 1, true),
        "at the {root} root: `m`'s last token read is not its own `}}`"
      );
      assert_eq!(
        gaps(&m),
        [(
          String::from(" mod n { ))) scalar B }"),
          brace + 1..WITNESS.len()
        )],
        "at the {root} root: what the parse never read"
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

  /// A byte the lexer refuses right after a module's `}`, in the middle of a document. Its gap tile
  /// is text the parse never read, and tokora tiles it at the token it trails, before the close
  /// after that token is read: it stands in `m`, after `m`'s own `}`. The definition after it is
  /// read at the root, and `m` is faithful to the braces as written.
  #[test]
  fn a_gap_right_after_a_modules_brace_stands_in_the_module() {
    let text = "mod m { scalar A }? scalar B";
    let brace = text.find('}').expect("the text holds a `}`");
    for (root, tree, _) in at_both_roots(text) {
      let m = module(&tree, "mod m");
      assert_eq!(
        m.text().to_string(),
        "mod m { scalar A }?",
        "at the {root} root"
      );
      assert_eq!(
        last_read(&m),
        (String::from("}"), brace..brace + 1, true),
        "at the {root} root: `m`'s last token read is not its own `}}`"
      );
      assert_eq!(
        gaps(&m),
        [(String::from("?"), brace + 1..brace + 2)],
        "at the {root} root: the byte the lexer refused"
      );
      assert_eq!(scalars(&m), ["scalar A"], "at the {root} root");
      assert_eq!(
        scalars(&document_of(&tree)),
        ["scalar B"],
        "at the {root} root"
      );
      let held = super::braces::faithful(&tree)
        .unwrap_or_else(|why| panic!("{text} at the {root} root: {why}"));
      assert_eq!(held.read, 1, "at the {root} root");
    }
  }

  // ── An expression outside a block ──────────────────────────────────────────────────────────

  /// A `(` where the run reads an operand is one value, to its own `)`: the map entry keeps its
  /// `=>`, and the field after the map stays a field.
  #[test]
  fn a_parenthesis_a_run_reads_as_an_operand_leaves_the_arrow_to_its_entry() {
    let answer = read("input I { a: T = map!{ 1 + (2) => 3 } b: Int }");
    assert_eq!(answer.reports, 1, "one report, at the `+`");
    let entry = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::MapEntry)
      .expect("the map's entry");
    assert!(
      entry
        .children_with_tokens()
        .any(|element| element.kind() == K::FatArrow),
      "the entry keeps its `=>`"
    );
    let members: Vec<String> = answer
      .tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(members.len(), 2, "{members:?}");
    assert_eq!(members[1], "b: Int", "`b` stays a field of `I`");
  }

  /// A schema map's key that begins with `(` is read to its own `)`, so the entry keeps its `=>`
  /// and its value, and the field after the map stays a field.
  #[test]
  fn a_key_that_begins_with_a_parenthesis_leaves_the_arrow_to_its_entry() {
    let answer = read("input I { a: T = map!{ (2) => 3 } b: Int }");
    assert_eq!(answer.reports, 1, "one report, at the `(`");
    let entry = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::MapEntry)
      .expect("the map's entry");
    assert!(
      entry
        .children_with_tokens()
        .any(|element| element.kind() == K::FatArrow),
      "the entry keeps its `=>`"
    );
    assert_eq!(
      kinds(&entry).last(),
      Some(&K::IntValue),
      "the entry keeps its value"
    );
    let members = answer
      .tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .count();
    assert_eq!(members, 2, "`b` stays a field of `I`");
  }

  /// A skip inside a block stops at the block's `}`, with its `(` still open, so nothing after the
  /// `}` is lexed in the block's mode and then again outside it: under a produce-event budget of
  /// one event for each lexeme, and one more, the text reads to its end. The `(` is read by its
  /// own production, which leaves the block's `}` and reports its `)` missing in front of it.
  #[test]
  fn a_skip_inside_a_block_reads_nothing_past_the_blocks_brace() {
    let text = "input I { a: T = [const { 1 (2 } -2] }";
    let limits = LosslessLimits::default().with_max_produce_events(lexemes(text) + 1);
    let answer = read_with(&Lossless::with_limits(limits), text);
    assert_eq!(
      answer.reports, 2,
      "two reports, at the `(` and the `)` missing at the `}}`"
    );
    let list = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      .expect("the list");
    assert_eq!(kinds(&list), [K::ConstBlock, K::IntValue]);
    let last = list.children().last().expect("the list's second element");
    assert_eq!(
      last.text().to_string(),
      "-2",
      "lexed once, outside the block"
    );
  }

  /// A missing operand inside a block leaves the delimiter its production reads next: a map entry
  /// its `=>`, and the member loop its `,`. So the entries after it are read, and the definition
  /// goes on.
  #[test]
  fn a_missing_operand_in_a_block_leaves_the_delimiter_its_production_owns() {
    for text in [
      "input I { a: T = const { map!{ => 1, 2 => 3 } } b: Int }",
      "input I { a: T = const { map!{ 1 => , 2 => 3 } } b: Int }",
    ] {
      let answer = read(text);
      assert_eq!(
        answer.reports, 1,
        "{text}: one report, at the missing operand"
      );
      let entries = answer
        .tree
        .descendants()
        .filter(|node: &SyntaxNode| node.kind() == K::MapEntry)
        .count();
      assert_eq!(entries, 2, "{text}");
      let members = answer
        .tree
        .descendants()
        .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
        .count();
      assert_eq!(members, 2, "{text}: `b` stays a field of `I`");
    }
  }

  /// A skip inside a block stops in front of a `=>`, which only a map entry reads: junk after an
  /// entry's key leaves the entry its arrow, and the entries after it are read.
  #[test]
  fn a_skip_inside_a_block_leaves_a_map_entry_its_arrow() {
    let text = "input I { a: T = const { map!{ 1 + => 3, 4 => 5 } } b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "one report, at the `+`");
    let entries = answer
      .tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == K::MapEntry)
      .count();
    assert_eq!(entries, 2);
    let members = answer
      .tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .count();
    assert_eq!(members, 2, "`b` stays a field of `I`");
  }

  // ── Recovery inside a block, by what each production owns ─────────────────────────────────

  /// The aggregate a member stands in, the kind of the member's node, and how it is written: the
  /// member a hole stands in front of, which recovery at the hole must leave to its aggregate.
  type Later = Option<(K, K, &'static str)>;

  /// The audit's 34 holes inside a block, each written `@`, with the member after the hole where
  /// one stands. Each stands in `input I { a : T = … }`, see [`at_hole`].
  const HOLES: &[(&str, Later)] = &[
    ("const { 1 @ }", None),
    ("const { @ 1 }", None),
    ("const { ( 1 @ ) }", None),
    ("const { ( @ 1 ) }", None),
    (
      "const { [ 1 @ , 2 ] }",
      Some((K::ListValue, K::Expression, "2")),
    ),
    ("const { [ 1 , @ 2 ] }", None),
    (
      "const { set!{ 1 @ , 2 } }",
      Some((K::SetValue, K::Expression, "2")),
    ),
    ("const { set @ {1} }", None),
    (
      "const { map!{ 1 @ => 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ @ 1 => 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ 1 => 2 @ , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ 1 => @ 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { { a : 1 @ , c : 2 } }",
      Some((K::ObjectValue, K::ObjectField, "c : 2")),
    ),
    (
      "const { { a @ : 1 , c : 2 } }",
      Some((K::ObjectValue, K::ObjectField, "c : 2")),
    ),
    (
      "const { { a : @ 1 , c : 2 } }",
      Some((K::ObjectValue, K::ObjectField, "c : 2")),
    ),
    ("const { { @ a : 1 } }", None),
    ("const { Url ( 1 @ ) }", None),
    (
      "const { P { x : 1 @ , y : 2 } }",
      Some((K::TypedObjectValue, K::ObjectField, "y : 2")),
    ),
    ("const { Box < i32 @ > ( 1 ) }", None),
    ("const { Box < @ i32 > ( 1 ) }", None),
    ("const { Box < [ i32 @ ] > ( 1 ) }", None),
    ("const { Box < i32 > @ ( 1 ) }", None),
    ("const { const { 1 @ } }", None),
    ("const { a @ :: c }", None),
    ("const { a :: @ c }", None),
    (
      "const { map!{ 1 => [ 2 @ ] , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ 1 => ( 2 @ ) , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ 1 => { a : 2 @ } , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ ( 1 @ ) => 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ [ 1 @ ] => 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { map!{ Box < i32 @ > ( 1 ) => 2 , 3 => 4 } }",
      Some((K::MapValue, K::MapEntry, "3 => 4")),
    ),
    (
      "const { [ ( 1 @ ) , 2 ] }",
      Some((K::ListValue, K::Expression, "2")),
    ),
    (
      "const { [ { a : 1 @ } , 2 ] }",
      Some((K::ListValue, K::Expression, "2")),
    ),
    ("const { ( [ 1 @ ] ) }", None),
  ];

  /// The audit's eighteen tokens a hole is filled with, a `{ }` one of them, so the junk holds a
  /// `}` only behind its `{`.
  const JUNK: &[&str] = &[
    "1", "x", ",", "=>", "(", ")", "[", "]", "{ }", "{", ":", "+", "<", ">", "::", "!", "$", "~",
  ];

  /// `hole` filled with `junk`, an empty one taking the hole and its space away, as the default of
  /// a field `zz` follows, and a scalar after the input.
  ///
  /// A `,` stands between the block and `zz`: a comma is a separator only to a lexer in a block's
  /// mode, so a lexer still in the mode after the block's `}` shows as a separator outside it.
  fn at_hole(hole: &str, junk: &str) -> String {
    let value = if junk.is_empty() {
      hole.replacen("@ ", "", 1)
    } else {
      hole.replacen('@', junk, 1)
    };
    format!("input I {{ a : T = {value} , zz : Int }}\nscalar S")
  }

  /// Whether `junk` at `hole` closes a bracket the hole stands inside, which it did not open.
  fn closes_what_it_did_not_open(hole: &str, junk: &str) -> bool {
    let opener = match junk {
      ")" => "(",
      "]" => "[",
      ">" => "<",
      _ => return false,
    };
    let before = &hole[..hole.find('@').expect("a hole")];
    let mut open: Vec<&str> = Vec::new();
    for token in before.split(' ') {
      match token {
        "(" | "[" | "<" | "{" | "set!{" | "map!{" => open.push(token),
        ")" | "]" | ">" | "}" => {
          open.pop();
        }
        _ => {}
      }
    }
    open.contains(&opener)
  }

  /// The byte of the `}` that balances the `{` at `open`, counting braces alone, if one does.
  fn balancing_brace(text: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (at, byte) in text.bytes().enumerate().skip(open) {
      match byte {
        b'{' => depth += 1,
        b'}' => {
          depth -= 1;
          if depth == 0 {
            return Some(at);
          }
        }
        _ => {}
      }
    }
    None
  }

  /// The falsifier for recovery inside a block, reduced to runs of none or one token: the audit's
  /// 34 holes, each filled from its alphabet of eighteen.
  ///
  /// 1. For every text, each block's node ends at the `}` its own braces give it, counting braces
  ///    alone from its `{`, and the lexer is out of the block's mode after it: no separator stands
  ///    outside a block.
  /// 2. Where the junk's braces balance, which is every token of the alphabet but the lone `{`,
  ///    `zz` is a field and `S` a scalar.
  /// 3. At the eighteen holes in front of a later member, for junk that leaves no `[` or `{` open,
  ///    closes nothing it did not open and holds no `<`, the later member is a node of its
  ///    aggregate. An unclosed `(` is in the family.
  #[test]
  fn recovery_inside_a_block_keeps_the_boundary_and_the_later_member() {
    let mut texts = 0usize;
    let mut later = 0usize;
    for (hole, member) in HOLES {
      for junk in core::iter::once("").chain(JUNK.iter().copied()) {
        let text = at_hole(hole, junk);
        let answer = read(&text);
        texts += 1;
        // 1. The block's braces, and the mode after them.
        for block in answer
          .tree
          .descendants()
          .filter(|node: &SyntaxNode| node.kind() == K::ConstBlock)
        {
          let own = block
            .descendants_with_tokens()
            .filter_map(|element| element.into_token())
            .find(|token| token.kind() == K::LBrace)
            .unwrap_or_else(|| panic!("{text}: a block with no `{{`"));
          let closer = balancing_brace(&text, usize::from(own.text_range().start()))
            .unwrap_or_else(|| panic!("{text}: no brace balances the block's"));
          assert_eq!(
            usize::from(block.text_range().end()),
            closer + 1,
            "{text}: the block at {:?} ends at a `}}` its braces do not give it",
            block.text_range()
          );
        }
        for token in answer
          .tree
          .descendants_with_tokens()
          .filter_map(|element| element.into_token())
          .filter(|token| token.kind() == K::Separator)
        {
          assert!(
            token
              .parent_ancestors()
              .any(|node: SyntaxNode| node.kind() == K::ConstBlock),
            "{text}: a separator at {:?} outside every block",
            token.text_range()
          );
        }
        // 2. What follows the block, where the junk's braces balance.
        if junk != "{" {
          assert!(
            answer.tree.descendants().any(|node: SyntaxNode| {
              node.kind() == K::InputValueDefinition
                && node.text().to_string().trim().starts_with("zz")
            }),
            "{text}: `zz` is no field"
          );
          assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
        }
        // 3. The later member, for the family that can keep it.
        if let Some((aggregate, kind, written)) = member
          && !matches!(junk, "[" | "{" | "<")
          && !closes_what_it_did_not_open(hole, junk)
        {
          later += 1;
          assert!(
            answer.tree.descendants().any(|node: SyntaxNode| {
              node.kind() == *aggregate
                && node.children().any(|child: SyntaxNode| {
                  child.kind() == *kind && child.text().to_string().trim() == *written
                })
            }),
            "{text}: `{written}` is no member of its {aggregate:?}"
          );
        }
      }
    }
    assert_eq!(texts, 34 * 19);
    assert!(later >= 18 * 12, "only {later} texts held the later member");
  }

  /// The defect rows the falsifier does not reach: junk of two tokens in a map's key, a
  /// parenthesis inside a list that a map entry's value holds, a list in a map's key that never
  /// closes, a group in the junk after a set's member, type arguments that meet an entry's `=>`, a
  /// list type that meets the arguments' `>`, and a collection with no `{`. Each keeps the
  /// definition and the field after it, and what the row names.
  #[test]
  fn each_defect_inside_a_block_keeps_the_definition_and_what_its_row_names() {
    /// The entries of the first map in `tree`, each as it is written.
    fn entries(tree: &SyntaxNode) -> Vec<String> {
      tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == K::MapValue)
        .expect("a map")
        .children()
        .filter(|node: &SyntaxNode| node.kind() == K::MapEntry)
        .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
        .collect()
    }
    /// How many fields the input holds.
    fn fields(tree: &SyntaxNode) -> usize {
      tree
        .descendants()
        .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
        .count()
    }
    for (row, text, names) in [
      // A2: `2` is junk after the key, `3` is read on to the `=>`, and the entry keeps both halves.
      (
        "A2",
        "input I { a: T = const { map!{ 1 2 3 => 4 } } b: Int }",
        &["1 2 3 => 4"][..],
      ),
      // A4: the parenthesis leaves the list its `]` and the map its `,`.
      (
        "A4",
        "input I { a: T = const { map!{ 1 => [(2], 3 => 4 } } b: Int }",
        &["1 => [(2]", "3 => 4"][..],
      ),
      // A5: the list leaves the entry its `=>`.
      (
        "A5",
        "input I { a: T = const { map!{ [1 => 2, 3 => 4 } } b: Int }",
        &["[1 => 2", "3 => 4"][..],
      ),
    ] {
      let answer = read(text);
      assert_eq!(
        fields(&answer.tree),
        2,
        "{row} {text}: `b` stays a field of `I`"
      );
      assert_eq!(entries(&answer.tree), names, "{row} {text}");
    }

    // A9 and A10: the type arguments, and a type missing in them, leave the entry its `=>`. After
    // a lone `<` a comma is trivia and no separator, until a `>` or a brace, so what follows the
    // entry's value up to the next `=>` is the entry's and no entry of its own: the lexer's rule,
    // and no row's.
    let answer = read("input I { a: T = const { map!{ Box< => 1, 2 => 3 } } b: Int }");
    assert_eq!(fields(&answer.tree), 2, "A9: `b` stays a field of `I`");
    assert_eq!(
      entries(&answer.tree).first().map(String::as_str),
      Some("Box< => 1, 2"),
      "A9: {:?}",
      entries(&answer.tree)
    );
    let answer = read("input I { a: T = const { map!{ x < 1 => 2, 3 => 4 } } b: Int }");
    assert_eq!(fields(&answer.tree), 2, "A10: `b` stays a field of `I`");
    assert!(
      entries(&answer.tree)
        .first()
        .is_some_and(|entry| entry.starts_with("x < 1 => 2")),
      "A10: {:?}",
      entries(&answer.tree)
    );

    // A11: the list type leaves the arguments their `>`, and the typed value its `(1)`.
    let answer = read("input I { a: T = const { Box<[i32 >(1) } b: Int }");
    assert_eq!(fields(&answer.tree), 2, "A11: `b` stays a field of `I`");
    let typed = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::TypedValue)
      .expect("A11: the typed value keeps its body");
    assert_eq!(typed.text().to_string(), "Box<[i32 >(1)", "A11");

    // A8: after a member and the junk behind it, a `{` is read as an object, so its `}` is its own
    // and the set keeps the member after it.
    let answer = read("input I { a: T = const { set!{ 1 ! {} , 2 } } b: Int }");
    assert_eq!(fields(&answer.tree), 2, "A8: `b` stays a field of `I`");
    let members: Vec<String> = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::SetValue)
      .expect("A8: the set")
      .children()
      .filter(|node: &SyntaxNode| node.kind() == K::Expression)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(
      members.last().map(String::as_str),
      Some("2"),
      "A8: {members:?}"
    );

    // A14: `set!` with no `{` is a form with no body, and the block goes on.
    let answer = read("input I { a: T = const { set! 1 } b: Int }");
    assert_eq!(fields(&answer.tree), 2, "A14: `b` stays a field of `I`");
    let set = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::SetValue)
      .expect("A14: the set");
    assert_eq!(set.text().to_string().trim(), "set!", "A14");
  }

  /// Outside a block, a run after a value at a member position leaves the next member's name,
  /// which a `:` follows (A25), and a map entry an expression stands in ends where its `=>` is
  /// missing rather than unwinding the definition (A26), a key's group that never closes stopping
  /// in front of the entry's `=>` (A27). What main keeps is kept.
  #[test]
  fn a_run_outside_a_block_keeps_the_next_member_and_entry() {
    /// The direct children of kind `kind` of the first node of kind `parent`, each as written.
    fn members(tree: &SyntaxNode, parent: K, kind: K) -> Vec<String> {
      tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == parent)
        .unwrap_or_else(|| panic!("no {parent:?}"))
        .children()
        .filter(|node: &SyntaxNode| node.kind() == kind)
        .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
        .collect()
    }
    // A25, after a member's default, in an object, and in a directive's arguments.
    let answer = read("input I { a: T = 1 + b: Int }");
    assert_eq!(answer.reports, 1, "A25: one report, at the `+`");
    assert_eq!(
      members(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition
      ),
      ["a: T = 1 +", "b: Int"]
    );
    let answer = read("input I { a: T = { a: 1 + b: 2 } zz: Int }");
    assert_eq!(answer.reports, 1, "A25: one report, at the `+`");
    assert_eq!(
      members(&answer.tree, K::ObjectValue, K::ObjectField),
      ["a: 1", "b: 2"]
    );
    let answer = read("input I { a: T @d(x: 1 + y: 2) zz: Int }");
    assert_eq!(answer.reports, 1, "A25: one report, at the `+`");
    assert_eq!(
      members(&answer.tree, K::Arguments, K::Argument),
      ["x: 1", "y: 2"]
    );

    // A26 and A27: the map keeps the entry after the junk, and `b` stays a field.
    for (row, text) in [
      ("A26", "input I { a: T = map!{ 1 => 2 () 3 => 4 } b: Int }"),
      ("A27", "input I { a: T = map!{ (1 => 2 3 => 4 } b: Int }"),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{row} {text}: one report, at the `(`");
      let entries = members(&answer.tree, K::MapValue, K::MapEntry);
      assert_eq!(
        entries.last().map(String::as_str),
        Some("3 => 4"),
        "{row} {text}: {entries:?}"
      );
      assert_eq!(
        members(
          &answer.tree,
          K::InputFieldsDefinition,
          K::InputValueDefinition
        )
        .len(),
        2,
        "{row} {text}: `b` stays a field of `I`"
      );
    }
  }

  /// A run in place of a member's value, an operator's token where the value would be, leaves the
  /// next member's name to the member loop, as a run after the value does: after a member's `=`,
  /// in an object and in a directive's arguments. The member after it is kept, as main keeps it.
  #[test]
  fn a_run_in_place_of_a_members_value_leaves_the_next_member() {
    for (text, parent, kind, written) in [
      (
        "input I { a: T = + b: Int }",
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        "b: Int",
      ),
      (
        "input I { a: T = { a: + b: 2 } zz: Int }",
        K::ObjectValue,
        K::ObjectField,
        "b: 2",
      ),
      (
        "input I { a: T @d(x: + y: 2) zz: Int }",
        K::Arguments,
        K::Argument,
        "y: 2",
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}: one report, at the `+`");
      let members: Vec<String> = answer
        .tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == parent)
        .unwrap_or_else(|| panic!("{text}: no {parent:?}"))
        .children()
        .filter(|node: &SyntaxNode| node.kind() == kind)
        .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
        .collect();
      assert_eq!(
        members.last().map(String::as_str),
        Some(written),
        "{text}: {members:?}"
      );
    }
  }

  /// The direct children of kind `kind` of the first node of kind `parent` in `tree`, each as it
  /// is written.
  fn members_of(tree: &SyntaxNode, parent: K, kind: K) -> Vec<String> {
    tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == parent)
      .unwrap_or_else(|| panic!("no {parent:?}"))
      .children()
      .filter(|node: &SyntaxNode| node.kind() == kind)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect()
  }

  /// A `(` a run reads as an operand is closed by its `)` through a brace it opened after it, and
  /// the read goes on to that brace's own `}`: in `( { x: 1 ) }` the group ends with the brace. So
  /// the definition's `}` stays the definition's and the member after the run is a member of `I`,
  /// as main keeps it, whose skip counts every pair in one depth.
  #[test]
  fn a_group_closed_inside_its_brace_ends_with_the_brace() {
    let text = "input I { a: T = 1 + ( { x: 1 ) } b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "one report, at the `+`");
    assert_eq!(
      members_of(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition
      ),
      ["a: T = 1 + ( { x: 1 ) }", "b: Int"]
    );
    let fields = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::InputFieldsDefinition)
      .expect("the input's fields");
    assert_eq!(
      fields.text().to_string(),
      &text["input I ".len()..],
      "`I` ends at its own `}}`"
    );
  }

  /// Whether the first node of kind `kind` in `tree` runs to the end of `text` and ends on a `}`:
  /// the definition whose members it holds closes at its own brace.
  fn closes_at_the_end(tree: &SyntaxNode, kind: K, text: &str) -> bool {
    closes_in_front_of(tree, kind, text, "")
  }

  /// Whether the first node of kind `kind` in `tree` ends on a `}` with nothing behind it in
  /// `text` but `after`: the definition whose members it holds closes at its own brace, in front
  /// of what `after` writes.
  fn closes_in_front_of(tree: &SyntaxNode, kind: K, text: &str, after: &str) -> bool {
    tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == kind)
      .is_some_and(|node: SyntaxNode| {
        let written = node.text().to_string();
        written.ends_with('}')
          && text
            .strip_suffix(after)
            .is_some_and(|text: &str| text.ends_with(written.as_str()))
      })
  }

  /// The direct child nodes of `node`, each as its kind and as it is written.
  fn child_nodes(node: &SyntaxNode) -> Vec<(K, String)> {
    node
      .children()
      .map(|child: SyntaxNode| (child.kind(), child.text().to_string().trim().to_string()))
      .collect()
  }

  /// The error diagnostics of `text` read as a type-system document, each as its span.
  fn error_spans(text: &str) -> Vec<Range<usize>> {
    parse_type_system_document(text)
      .diagnostics()
      .iter()
      .filter(|diagnostic| diagnostic.severity() == Severity::Error)
      .map(|diagnostic| diagnostic.span())
      .collect()
  }

  /// The errors the lossless layer lends a sink for `text` under `layer`, in the order it lends
  /// them, each as its code and its primary span.
  fn lent(layer: &Lossless, text: &str) -> Vec<(String, Range<usize>)> {
    let mut errors: Vec<GraphqlxLosslessErrorValue<&str>> = Vec::new();
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

  /// A member position of a run, `OPERAND` standing where its operand does: the node of the list
  /// the run stands in, the kind of that list's members and how the member written after the run
  /// reads, and the node of the definition's members and what the text writes after it, in front
  /// of which the definition closes at its own brace.
  type AtAMember = (&'static str, K, K, &'static str, K, &'static str);

  /// The five member positions of a run: after a member's default and in its place, after a
  /// default between a field's parentheses, after an object field's value, and after a directive
  /// argument's. The lists of the third and the fifth close with `)`.
  const AT_A_MEMBER: [AtAMember; 5] = [
    (
      "input I { a: T = 1 + OPERAND b: Int }",
      K::InputFieldsDefinition,
      K::InputValueDefinition,
      "b: Int",
      K::InputFieldsDefinition,
      "",
    ),
    (
      "input I { a: T = + OPERAND b: Int }",
      K::InputFieldsDefinition,
      K::InputValueDefinition,
      "b: Int",
      K::InputFieldsDefinition,
      "",
    ),
    (
      "type T { f(a: T = 1 + OPERAND b: Int): Int g: Int }",
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      "b: Int",
      K::FieldsDefinition,
      "",
    ),
    (
      "input I { a: T = { a: 1 + OPERAND b: 2 } zz: Int }",
      K::ObjectValue,
      K::ObjectField,
      "b: 2",
      K::InputFieldsDefinition,
      "",
    ),
    (
      "input I { a: T @d(x: 1 + OPERAND y: 2) zz: Int }",
      K::Arguments,
      K::Argument,
      "y: 2",
      K::InputFieldsDefinition,
      "",
    ),
  ];

  /// Three member positions whose list closes with `)`: after a default between a field's
  /// parentheses, after a directive argument's value inside a definition, and after one on a
  /// definition's header, where the definition after it is kept as well.
  const IN_PARENTHESES: [AtAMember; 3] = [
    (
      "type T { f(a: T = 1 + OPERAND zz: Int): Int g: Int }",
      K::ArgumentsDefinition,
      K::InputValueDefinition,
      "zz: Int",
      K::FieldsDefinition,
      "",
    ),
    (
      "input I { a: T @d(x: 1 + OPERAND y: 2) zz: Int }",
      K::Arguments,
      K::Argument,
      "y: 2",
      K::InputFieldsDefinition,
      "",
    ),
    (
      "type T @d(x: 1 + OPERAND y: 2) { f: Int } scalar S",
      K::Arguments,
      K::Argument,
      "y: 2",
      K::FieldsDefinition,
      " scalar S",
    ),
  ];

  /// `operand` at each of `positions`: the run's report is the only one, the list the run stands
  /// in holds two members, the second of them the one written after the run, and the definition
  /// closes at its own brace.
  fn leaves_the_next_member_at(operand: &str, positions: &[AtAMember]) {
    for &(template, list, kind, next, closes, after) in positions {
      let text = template.replace("OPERAND", operand);
      let answer = read(&text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      let kept = members_of(&answer.tree, list, kind);
      assert_eq!(kept.len(), 2, "{text}: {kept:?}");
      assert_eq!(
        kept.last().map(String::as_str),
        Some(next),
        "{text}: {kept:?}"
      );
      assert!(
        closes_in_front_of(&answer.tree, closes, &text, after),
        "{text}: the definition closes at its own brace"
      );
    }
  }

  /// `operand` at each of [`AT_A_MEMBER`]'s positions, as [`leaves_the_next_member_at`] reads it.
  fn leaves_the_next_member(operand: &str) {
    leaves_the_next_member_at(operand, &AT_A_MEMBER);
  }

  /// An operand a run reads at a member position leaves the member after the run to the member
  /// loop, whatever the operand is, and the definition closes at its own brace: a path or a
  /// variable whose name is missing or is the next member's; a failure inside a bracket; a list or
  /// type arguments left open; an operator's `>` inside a list; a `)` that closes its `(` through a
  /// brace; nested runs; and a block. At all five member positions: a list or type arguments left
  /// open inside an argument list stop at the next argument's name, as they do everywhere else.
  ///
  /// A `(` left open, alone or as a typed value's, stops at the next member's name only where the
  /// list closes with `}`. Where it closes with `)` the `(` reads on to the list's own `)`, as
  /// main's skip reads a `(` (R21): `a_parenthesis_left_open_reads_on_to_the_lists_own_closer`.
  #[test]
  fn a_run_leaves_the_next_member() {
    for operand in [
      "~",
      "~::",
      "^::",
      "..::..::",
      "c::",
      "$",
      "{ x: ~ }",
      "{ x 1 }",
      "[~ c]",
      "Url(~)",
      "P{ x: ~ }",
      "Box<~>(1)",
      "map!{ ~ => 1 }",
      "set!",
      "Box<",
      "[c",
      "[1 + 2 >> 3",
      "( { x: 1 ) }",
      "{ x: 1 + { q: ~ } bad: ~ }",
      "Box<[i32>(1)",
      "c < d",
      "const { 2 }",
    ] {
      leaves_the_next_member(operand);
    }
    let in_braces = [AT_A_MEMBER[0], AT_A_MEMBER[1], AT_A_MEMBER[3]];
    for operand in ["(c", "Url(c"] {
      leaves_the_next_member_at(operand, &in_braces);
    }
  }

  /// No operator's token closes a bracket a run opened: a `>`, alone or half of a `>>`, closes only
  /// type arguments that are the innermost thing open. So a list, a parenthesis or type arguments
  /// left open still stop at the next member's name, a parenthesis in a list that closes with `)`
  /// too, since a `>` that closes nothing is a closer astray (R21).
  #[test]
  fn no_operators_token_closes_a_bracket_of_a_run() {
    for operand in ["[1 + 2 >> 3", "(1 >> 2", "[c > d", "Box<[i32 >> 2"] {
      leaves_the_next_member(operand);
    }
  }

  /// A brace a run's operand opens is read to the `}` that balances it, whatever stands between: a
  /// closer of another kind is that brace's junk, and what was opened inside the brace closes with
  /// it. So the member after the run stays a member, and a list's element after it stays an
  /// element. The review's fourth is `a_group_closed_inside_its_brace_ends_with_the_brace`.
  #[test]
  fn a_run_reads_a_brace_to_its_own_closer() {
    for operand in ["{ x: 1 ) y: 2 }", "P { x: [1 }", "set!{ ( }"] {
      leaves_the_next_member(operand);
    }
    let text = "input I { a: T = [1 + { x: (1 } 5] zz: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    let list = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      .expect("the list");
    assert_eq!(
      child_nodes(&list),
      [
        (K::IntValue, String::from("1")),
        (K::Error, String::from("+")),
        (K::Error, String::from("{ x: (1 }")),
        (K::IntValue, String::from("5")),
      ],
      "{text}: `5` is an element"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );
  }

  /// A value that follows an operand the run read whole ends the run, as one that follows a scalar
  /// does, so in a list or a set it is the next element. The review's map, r6-2, is in
  /// `the_runs_review_rounds_4_to_6_raised_keep_what_follows_them`.
  #[test]
  fn a_run_ends_at_a_value_behind_a_closed_operand() {
    let text = "input I { a: T = [1 + { x: ~ } c 5] b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    let list = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      .expect("the list");
    assert_eq!(
      child_nodes(&list),
      [
        (K::IntValue, String::from("1")),
        (K::Error, String::from("+")),
        (K::Error, String::from("{ x: ~ }")),
        (K::EnumValue, String::from("c")),
        (K::IntValue, String::from("5")),
      ],
      "{text}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );

    let text = "input I { a: T = set!{ 1 + [~] c } b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    let set = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::SetValue)
      .expect("the set");
    assert_eq!(
      child_nodes(&set),
      [
        (K::IntValue, String::from("1")),
        (K::Error, String::from("+")),
        (K::Error, String::from("[~]")),
        (K::EnumValue, String::from("c")),
      ],
      "{text}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );
  }

  /// A map entry's key that a run stands in leaves the entry its `=>` at every depth its group has
  /// open outside a brace, so the entry keeps its value and the entries after it are read.
  #[test]
  fn a_key_leaves_its_arrow_at_every_depth() {
    for (text, first) in [
      (
        "input I { a: T = map!{ 1 ((2 => 3 4 => 5 } b: Int }",
        "1 ((2 => 3",
      ),
      (
        "input I { a: T = map!{ ([2 => 3 4 => 5 } b: Int }",
        "([2 => 3",
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      assert_eq!(
        members_of(&answer.tree, K::MapValue, K::MapEntry),
        [first, "4 => 5"],
        "{text}"
      );
      assert!(
        closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
        "{text}"
      );
    }
  }

  /// A closer the run did not open ends it, inside a list or a parenthesis of its own too, and is
  /// its owner's: an argument list and a typed value close on their own `)`. Where the list's `]`
  /// meets a `(` the run left open, the list closes there, and its owner reads what follows it.
  #[test]
  fn a_run_stops_at_a_closer_it_did_not_open() {
    for (text, owner, written) in [
      (
        "input I { a: T @d(x: 1 + [c) zz: Int }",
        K::Arguments,
        "(x: 1 + [c)",
      ),
      (
        "input I { a: T = Url(1 + [c) zz: Int }",
        K::TypedValue,
        "Url(1 + [c)",
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      let node = answer
        .tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == owner)
        .unwrap_or_else(|| panic!("{text}: no {owner:?}"));
      assert_eq!(
        node.text().to_string(),
        written,
        "{text}: it ends at its own `)`"
      );
      let fields = members_of(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition,
      );
      assert_eq!(fields.len(), 2, "{text}: {fields:?}");
      assert_eq!(
        fields.last().map(String::as_str),
        Some("zz: Int"),
        "{text}: {fields:?}"
      );
    }

    let text = "input I { a: T = [1 + (2 3] 4] zz: Int }";
    let answer = read(text);
    let fields = members_of(
      &answer.tree,
      K::InputFieldsDefinition,
      K::InputValueDefinition,
    );
    assert_eq!(
      fields.last().map(String::as_str),
      Some("zz: Int"),
      "{text}: {fields:?}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );
  }

  /// A scalar and a complete path among a run's operands are the values they are, so the brief's
  /// four pins keep their trees: a number is a number wherever it stands, the one a `-` begins
  /// among them, and a rooted or a qualified path is an `EnumValue` between the operators'
  /// `Error`s.
  #[test]
  fn a_scalar_and_a_complete_path_stay_the_values_they_are() {
    for (text, parent, expected) in [
      (
        "input I { a: T = [1 << 2, 3] }",
        K::ListValue,
        &[K::IntValue, K::Error, K::IntValue, K::IntValue][..],
      ),
      (
        "input I { a: T = [1, <<2>>, 3] }",
        K::ListValue,
        &[K::IntValue, K::Error, K::IntValue, K::Error, K::IntValue][..],
      ),
      (
        "input I { a: u32 = 256 >> 4 }",
        K::InputValueDefinition,
        &[
          K::DefinitionTypePath,
          K::DefaultValue,
          K::Error,
          K::IntValue,
        ][..],
      ),
      (
        "input I { a: u32 = HOUR-1 }",
        K::InputValueDefinition,
        &[K::DefinitionTypePath, K::DefaultValue, K::IntValue][..],
      ),
      (
        "input I { a: T = 2 * ~::HOUR + c::d b: Int }",
        K::InputValueDefinition,
        &[
          K::DefinitionTypePath,
          K::DefaultValue,
          K::Error,
          K::EnumValue,
          K::Error,
          K::EnumValue,
        ][..],
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      let node = answer
        .tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == parent)
        .unwrap_or_else(|| panic!("{text}: no {parent:?}"));
      assert_eq!(kinds(&node), expected, "{text}");
    }
  }

  /// Every operand in brackets, and a variable, is one `Error` that holds no node, reported by no
  /// one since the run reported once: a typed value, a typed object, a list, a set, a map, an
  /// object, a typed value with type arguments and a variable, each in front of the next member.
  #[test]
  fn an_operand_in_brackets_is_one_error() {
    for (expression, operand) in [
      ("2 * Seconds(30)", "Seconds(30)"),
      ("1 + P { x: 1 }", "P { x: 1 }"),
      ("1 + [1 2]", "[1 2]"),
      ("1 + set!{ 1 }", "set!{ 1 }"),
      ("1 + map!{ 1 => 2 }", "map!{ 1 => 2 }"),
      ("1 + { x: 1 }", "{ x: 1 }"),
      ("1 + Box<i32>(1)", "Box<i32>(1)"),
      ("1 + $v", "$v"),
    ] {
      let text = format!("input I {{ a: T = {expression} b: Int }}");
      let answer = read(&text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      let member = answer
        .tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
        .expect("the first member");
      let last = member.children().last().expect("the member's last node");
      assert_eq!(last.kind(), K::Error, "{text}");
      assert_eq!(last.text().to_string().trim(), operand, "{text}");
      assert_eq!(
        last.children().count(),
        0,
        "{text}: the `Error` holds no node"
      );
      assert!(
        closes_at_the_end(&answer.tree, K::InputFieldsDefinition, &text),
        "{text}"
      );
      assert_eq!(
        members_of(
          &answer.tree,
          K::InputFieldsDefinition,
          K::InputValueDefinition
        )
        .last()
        .map(String::as_str),
        Some("b: Int"),
        "{text}"
      );
    }
  }

  /// A `const { … }` block among a run's operands is read as a block, in the block's mode: it is a
  /// `ConstBlock`. So a number refused right in front of its `}` leaves that `}` to the block, as it
  /// would not outside the mode, where the refusal takes the brace, and the member after the run is
  /// kept.
  #[test]
  fn a_block_among_a_runs_operands_is_read_as_a_block() {
    let text = "input I { a: T = 1 + const { 2 } b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    let member = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .expect("the first member");
    assert_eq!(
      kinds(&member),
      [
        K::DefinitionTypePath,
        K::DefaultValue,
        K::Error,
        K::ConstBlock
      ],
      "{text}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );

    let text = "input I { a: T = 1 + const { 2.} b: Int }";
    let answer = read(text);
    assert!(
      answer
        .tree
        .descendants()
        .any(|node: SyntaxNode| node.kind() == K::ConstBlock),
      "{text}: the block"
    );
    let fields = members_of(
      &answer.tree,
      K::InputFieldsDefinition,
      K::InputValueDefinition,
    );
    assert_eq!(fields.len(), 2, "{text}: {fields:?}");
    assert_eq!(
      fields.last().map(String::as_str),
      Some("b: Int"),
      "{text}: {fields:?}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}"
    );
  }

  /// A run takes each token once and gives none back. It asks about no name inside a brace, so a
  /// run whose operand nests braces sixteen deep reads to its end under a produce-event budget of
  /// one event for each lexeme and one more, as r6-3's two deep does. Where it asks about the names
  /// behind a `(` left open, a name and what follows it are read once more, so three such names read
  /// to the end under three events for each lexeme.
  #[test]
  fn a_run_reads_no_token_twice() {
    let mut nested = String::from("{ q: ~ }");
    for _ in 0..15 {
      nested = format!("{{ x: 1 + {nested} bad: ~ }}");
    }
    for text in [
      String::from("input I { a: T = 1 + { x: 1 + { q: ~ } bad: ~ } b: Int }"),
      format!("input I {{ a: T = 1 + {nested} b: Int }}"),
    ] {
      let limits = LosslessLimits::default().with_max_produce_events(lexemes(&text) + 1);
      let answer = read_with(&Lossless::with_limits(limits), &text);
      assert_eq!(answer.reports, 1, "{text}: the run's report alone");
      assert!(
        closes_at_the_end(&answer.tree, K::InputFieldsDefinition, &text),
        "{text}: read to its end"
      );
    }

    let text = "input I { a: T = 1 + (c d b: Int }";
    let limits = LosslessLimits::default().with_max_produce_events(3 * lexemes(text));
    let answer = read_with(&Lossless::with_limits(limits), text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    assert_eq!(
      members_of(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition
      ),
      ["a: T = 1 + (c d", "b: Int"],
      "{text}"
    );
  }

  /// What a run spends on its brackets is linear in what it takes: a closer asks a count of its
  /// kind and never searches what is open. So a hundred thousand pairs inside one brace, each of
  /// which opens an entry, a `(` or the `<` behind a name, and closes nothing, read in linear time,
  /// where a search of the open entries for each closer would take five thousand million steps.
  #[test]
  fn a_run_matches_its_brackets_in_linear_time() {
    const PAIRS: usize = 100_000;
    for pair in ["( ] ", "a < ] "] {
      let text = format!(
        "input I {{ a: T = 1 + {{ {} }} b: Int }}",
        pair.repeat(PAIRS)
      );
      let answer = read(&text);
      assert_eq!(answer.reports, 1, "{pair:?}: the run's report alone");
      let fields = members_of(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition,
      );
      assert_eq!(fields.len(), 2, "{pair:?}");
      assert_eq!(
        fields.last().map(String::as_str),
        Some("b: Int"),
        "{pair:?}"
      );
    }
  }

  /// The texts of the review's rounds 4 to 6 that hold a run, as each was written: r4-1, r4-2, r4-3
  /// with its gap of 1 024 spaces, r5-1, r5-2, r6-1, r6-2 and r6-3.
  fn reviewed_runs() -> Vec<String> {
    let gap = " ".repeat(1024);
    vec![
      String::from("input I { a: T = + b: Int }"),
      String::from("input I { a: T = 1 + ~ b: Int }"),
      format!("input I {{ a: T = 1 + b{gap}: Int }}"),
      String::from("input I { a: T = 1 + ~:: b: Int }"),
      String::from("input I { a: T = 1 + { x: ~ } b: Int }"),
      String::from("input I { a: T = 1 + [1 + 2 >> 3 b: Int }"),
      String::from("input I { a: T = map!{ 1 => + { x: ~ } next => 4 } }"),
      String::from("input I { a: T = 1 + { x: 1 + { q: ~ } bad: ~ } b: Int }"),
    ]
  }

  /// What the review's rounds 4 to 6 raised outside a block keeps what main keeps: one report, at
  /// the run's first token, and the member or the entry the text writes after the run. r4-2's `~`
  /// is one `Error`, r6-1's run is two, its operator's and its list's, and the definition closes at
  /// its own brace.
  #[test]
  fn the_runs_review_rounds_4_to_6_raised_keep_what_follows_them() {
    let gap = " ".repeat(1024);
    let texts = reviewed_runs();
    let expected: [(K, K, [String; 2]); 8] = [
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [String::from("a: T = +"), String::from("b: Int")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [String::from("a: T = 1 + ~"), String::from("b: Int")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [String::from("a: T = 1 +"), format!("b{gap}: Int")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [String::from("a: T = 1 + ~::"), String::from("b: Int")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [String::from("a: T = 1 + { x: ~ }"), String::from("b: Int")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [
          String::from("a: T = 1 + [1 + 2 >> 3"),
          String::from("b: Int"),
        ],
      ),
      (
        K::MapValue,
        K::MapEntry,
        [String::from("1 => + { x: ~ }"), String::from("next => 4")],
      ),
      (
        K::InputFieldsDefinition,
        K::InputValueDefinition,
        [
          String::from("a: T = 1 + { x: 1 + { q: ~ } bad: ~ }"),
          String::from("b: Int"),
        ],
      ),
    ];
    for (text, (parent, kind, members)) in texts.iter().zip(expected) {
      let at = text.find('+').expect("the run's first token");
      let report = at..at + 1;
      assert_eq!(error_spans(text), [report], "{text}: one report");
      let answer = read(text);
      assert_eq!(members_of(&answer.tree, parent, kind), members, "{text}");
      assert!(
        closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
        "{text}: the definition closes at its own brace"
      );
    }

    let text = "input I { a: T = 1 + ~ b: Int }";
    let member = read(text)
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .expect("the first member");
    assert_eq!(
      child_nodes(&member)
        .into_iter()
        .filter(|(kind, _)| *kind == K::Error)
        .map(|(_, written)| written)
        .collect::<Vec<String>>(),
      ["+", "~"],
      "{text}: the `~` is one `Error`"
    );

    let text = "input I { a: T = 1 + [1 + 2 >> 3 b: Int }";
    let member = read(text)
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .expect("the first member");
    assert_eq!(
      child_nodes(&member)
        .into_iter()
        .filter(|(kind, _)| *kind == K::Error)
        .map(|(_, written)| written)
        .collect::<Vec<String>>(),
      ["+", "[1 + 2 >> 3"],
      "{text}"
    );
  }

  /// What the review's rounds 4 to 6 raised that reaches no run reads as it did. An executable
  /// document's type arguments and a variable's default at the end of input first err where the
  /// input ends (r4-4, r4-5); a `set!` with no `{` inside a block, under a nesting ceiling the two
  /// braces around it fill, is refused at the `{` it is missing and not at the depth (r5-3); and a
  /// module the executable root refuses holds an enum value and an object, no typed form (r6-4).
  #[test]
  fn the_inputs_review_rounds_4_to_6_raised_that_hold_no_run_read_as_before() {
    for text in ["query Q($v: Box<", "query Q($v: T = ~"] {
      let errors: Vec<Range<usize>> = parse_executable_document(text)
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .map(|diagnostic| diagnostic.span())
        .collect();
      assert_eq!(
        errors.first(),
        Some(&(text.len()..text.len())),
        "{text}: {errors:?}"
      );
    }

    let text = "input I { a: T = const { set! } b: Int }";
    let errors = lent(
      &Lossless::with_limits(LosslessLimits::with_max_nesting_depth(2)),
      text,
    );
    assert_eq!(
      errors.first(),
      Some(&(String::from("smear::syntax::unexpected-token"), 30..31)),
      "{text}: {errors:?}"
    );
    assert!(
      !errors
        .iter()
        .any(|(code, _)| code == "smear::syntax::nesting-limit"),
      "{text}: {errors:?}"
    );

    let text = "mod M { input I { a: T = [P { x: 1 }] } }";
    let tree: SyntaxNode = parse_executable_document(text).syntax();
    let list = tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      .expect("the list");
    assert_eq!(kinds(&list), [K::EnumValue, K::ObjectValue], "{text}");
  }

  /// Each text of the review's rounds 4 to 6 that holds a run, cut at every token boundary behind
  /// the run's first token: the first error lent is the run's report at that token, and each after
  /// it is an owner's, an unclosed delimiter or the end of input where the owner needed more. What
  /// the run leaves open is no report of its own.
  #[test]
  fn a_run_cut_anywhere_is_its_own_first_error() {
    for text in reviewed_runs() {
      let at = text.find('+').expect("the run's first token");
      let boundaries: Vec<usize> = parse_document(&text)
        .syntax()
        .descendants_with_tokens()
        .filter_map(|element| element.into_token())
        .map(|token| usize::from(token.text_range().end()))
        .filter(|end| *end > at)
        .collect();
      for cut in boundaries {
        let prefix = &text[..cut];
        let errors = lent(&Lossless::new(), prefix);
        assert_eq!(
          errors.first(),
          Some(&(String::from("smear::syntax::unexpected-token"), at..at + 1)),
          "{prefix}: {errors:?}"
        );
        assert!(
          errors.iter().skip(1).all(|(code, _)| matches!(
            code.as_str(),
            "smear::syntax::unclosed-delimiter" | "smear::syntax::unexpected-end-of-input"
          )),
          "{prefix}: {errors:?}"
        );
      }
    }
  }

  /// The rows where a run keeps less than main, one for each cause, as they stand, so that a change
  /// to the reader is seen. A brace left open reads to the brace that balances it, the
  /// definition's (A). A closer of another kind inside a `[` ends the run in front of it, and its
  /// owner reads it as main does (B). A `]` inside a brace closes no `(`, so after the brace the `(`
  /// takes the argument list's `)` (C). A `)` closes its `(` through a brace, the brace ends the
  /// group, and the `)` after it closes the argument list (D).
  #[test]
  fn the_rows_below_main_keep_what_their_cause_leaves() {
    let text = "input I { a: T = 1 + { x: 1 zz: Int } scalar S";
    let document = top(&read(text).tree);
    assert!(
      !kinds(&document).contains(&K::InputObjectTypeDefinition),
      "{text}: A, the definition is lost"
    );
    assert!(scalars(&document).is_empty(), "{text}: A, and the next one");

    for (text, arguments) in [
      (
        "input I { a: T @d(x: 1 + [1 ) 2] y: 2) zz: Int } scalar S",
        &["x: 1"][..],
      ),
      (
        "input I { a: T @d(x: 1 + ( { ) ) ( ( } ) y: 2) zz: Int } scalar S",
        &["x: 1"][..],
      ),
      (
        "input I { a: T @d(x: 1 + ( { ] }) zz: Int } scalar S",
        &["x: 1", "zz: Int"][..],
      ),
    ] {
      let answer = read(text);
      assert_eq!(
        members_of(&answer.tree, K::Arguments, K::Argument),
        arguments,
        "{text}"
      );
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
    }
    for text in [
      "input I { a: T @d(x: 1 + [1 ) 2] y: 2) zz: Int } scalar S",
      "input I { a: T @d(x: 1 + ( { ) ) ( ( } ) y: 2) zz: Int } scalar S",
      "input I { a: T = [1 + [1 ) 2] 5] zz: Int } scalar S",
    ] {
      let fields = members_of(
        &read(text).tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition,
      );
      assert_eq!(
        fields.last().map(String::as_str),
        Some("zz: Int"),
        "{text}: B and D keep the member after the owner"
      );
    }
    let text = "input I { a: T @d(x: 1 + ( { ] }) zz: Int } scalar S";
    assert_eq!(
      members_of(
        &read(text).tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition
      )
      .len(),
      1,
      "{text}: C, `zz` is the list's"
    );

    let text = "input I { a: T = [1 + [1 ) 2] 5] zz: Int } scalar S";
    let answer = read(text);
    let list = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      .expect("the list");
    assert_eq!(
      child_nodes(&list),
      [
        (K::IntValue, String::from("1")),
        (K::Error, String::from("+")),
        (K::Error, String::from("[1")),
        (K::Error, String::from(")")),
        (K::IntValue, String::from("2")),
      ],
      "{text}: B, the list closes on its `]`"
    );
    assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
  }

  /// A group the text closes, in a member list that closes with `)`, is read whole: a name inside
  /// it is the group's, and its own `)` closes it, so the list closes at its own `)` and what
  /// follows the list is its owner's (R21). Round 7's text: the directive keeps its one argument,
  /// with the run's two `Error`s behind it, and the field after the header is a field of `T`. And
  /// six such groups, one in a typed value, one inside another and two with a list around or
  /// inside, each in front of the next member of three lists that close with `)`: the member is
  /// the list's second, and the definition closes at its own brace. A typed value's group between
  /// a field's parentheses keeps the argument definition after it and the field after the field.
  #[test]
  fn a_group_that_closes_is_read_whole_where_the_list_closes_with_a_parenthesis() {
    let text = "type T @d(x: 1 + (a: 2)) { f: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 1, "{text}: the run's report alone");
    let at = text.find('+').expect("the run's first token");
    let report = at..at + 1;
    assert_eq!(
      error_spans(text),
      [report],
      "{text}: at the run's first token"
    );
    let arguments = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::Arguments)
      .expect("the directive's arguments");
    assert_eq!(
      child_nodes(&arguments),
      [
        (K::Argument, String::from("x: 1")),
        (K::Error, String::from("+")),
        (K::Error, String::from("(a: 2)")),
      ],
      "{text}"
    );
    assert_eq!(
      members_of(&answer.tree, K::FieldsDefinition, K::FieldDefinition),
      ["f: Int"],
      "{text}"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::FieldsDefinition, text),
      "{text}: `T` ends at its own `}}`"
    );

    for operand in [
      "(a: 2)",
      "(1 a: 2)",
      "Url(a: 2)",
      "(a: (b: 2))",
      "[(a: 2)]",
      "([a: 2])",
    ] {
      leaves_the_next_member_at(operand, &IN_PARENTHESES);
    }

    let text = "type T { f(a: T = 1 + P(k: 2) zz: Int): Int g: Int }";
    leaves_the_next_member_at("P(k: 2)", &IN_PARENTHESES[..1]);
    assert_eq!(
      members_of(&read(text).tree, K::FieldsDefinition, K::FieldDefinition)
        .last()
        .map(String::as_str),
      Some("g: Int"),
      "{text}: `g` is a field of `T`"
    );
  }

  /// Where the list of members closes with `)`, a `(` the run's junk never closes, with no closer
  /// astray in it, reads on to the list's own `)`, as main's skip reads a `(` (R21). Inside a
  /// definition's braces main's skip finds no end there and falls back a token at a time, so main
  /// keeps the next argument and the field behind the list. The run keeps neither: `y` is inside
  /// the group, and `zz` is read as an argument of the list, which then meets the definition's `}`.
  #[test]
  fn a_parenthesis_left_open_reads_on_to_the_lists_own_closer() {
    let text = "input I { a: T @d(x: 1 + ( [ [ y: 2) zz: Int }";
    let answer = read(text);
    assert_eq!(
      answer.reports, 2,
      "{text}: the run's report, and the list's `)` missing at the `}}`"
    );
    assert_eq!(
      members_of(&answer.tree, K::Arguments, K::Argument),
      ["x: 1", "zz: Int"],
      "{text}"
    );
    assert_eq!(
      members_of(
        &answer.tree,
        K::InputFieldsDefinition,
        K::InputValueDefinition
      )
      .len(),
      1,
      "{text}: `zz` is no field of `I`"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}: `I` ends at its own `}}`"
    );
  }

  /// Where the list of members closes with `}`, the next member's name still ends a `(` of the run,
  /// closed or not: a `)` the run leaves behind is a stray to such a list and costs it no member,
  /// and the stop is what keeps the member behind a `(` left open (R21). So a group the text closes
  /// is cut at the name inside it: what follows is a field `a` with no type and a stray `)`, three
  /// reports where main makes one, and `b` and `I` are kept.
  #[test]
  fn a_name_inside_a_parenthesis_still_ends_it_where_the_list_closes_with_a_brace() {
    let text = "input I { a: T = 1 + (a: 2) b: Int }";
    let answer = read(text);
    assert_eq!(answer.reports, 3, "{text}");
    let fields = answer
      .tree
      .descendants()
      .find(|node: &SyntaxNode| node.kind() == K::InputFieldsDefinition)
      .expect("the input's fields");
    assert_eq!(
      child_nodes(&fields),
      [
        (K::InputValueDefinition, String::from("a: T = 1 + (")),
        (K::InputValueDefinition, String::from("a: 2")),
        (K::Error, String::from(")")),
        (K::InputValueDefinition, String::from("b: Int")),
      ],
      "{text}"
    );
    let typeless = fields.children().nth(1).expect("the field `a: 2`");
    assert!(
      !kinds(&typeless).contains(&K::DefinitionTypePath),
      "{text}: the field `a` has no type"
    );
    assert!(
      closes_at_the_end(&answer.tree, K::InputFieldsDefinition, text),
      "{text}: `I` ends at its own `}}`"
    );
  }

  /// A type's arguments that meet the end of input in an executable document ask for their first
  /// type there, as main's do: the first error is the end of input, where that type should stand,
  /// and not the `<` unclosed that a schema's type reports first. A variable's type, a type
  /// condition, a directive's name, an operation's parameter and its `where` clause.
  #[test]
  fn an_executable_documents_type_arguments_at_the_end_are_read_as_mains() {
    for text in [
      "query Q($v: Box<",
      "query Q { ... on Box<",
      "query Q @d<",
      "query Q<T = Box<",
      "query Q<T> where T: Box<",
    ] {
      let errors: Vec<Range<usize>> = parse_executable_document(text)
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.severity() == Severity::Error)
        .map(|diagnostic| diagnostic.span())
        .collect();
      assert_eq!(
        errors.first(),
        Some(&(text.len()..text.len())),
        "{text}: {errors:?}"
      );
    }
  }

  /// A module the executable root refuses is still read, in the grammar that root's constant
  /// positions read, as main read it: no typed form, no block and no run of an expression reaches
  /// the recovery tree, at a member's default or in a directive's arguments, in a definition, a
  /// field's arguments, an extension or a module inside the module; a list keeps an enum value
  /// and an object apart; and a type's arguments at the end of input ask for their first type.
  #[test]
  fn a_module_the_executable_root_refuses_reads_as_the_executable_document_does() {
    for text in [
      "mod M { input I { a: T = [P { x: 1 }] } }",
      "mod M { input I { a: T = Url(1) } }",
      "mod M { input I { a: T = const { 1 } } }",
      "mod M { type T { f(a: X = [P { x: 1 }]): Int } }",
      "mod M { scalar S @d(x: Box<i32>(1)) }",
      "mod M { extend scalar S @d(x: P { x: 1 }) }",
      "mod M { mod N { input I { a: T = [P { x: 1 }] } } }",
    ] {
      let tree: SyntaxNode = parse_executable_document(text).syntax();
      assert_eq!(
        tree.text().to_string(),
        text,
        "{text}: the tree spells its text"
      );
      let schema_only: Vec<String> = tree
        .descendants()
        .filter(|node: &SyntaxNode| {
          matches!(
            node.kind(),
            K::TypedObjectValue | K::TypedValue | K::ConstBlock | K::Expression
          )
        })
        .map(|node: SyntaxNode| format!("{:?} {}", node.kind(), node.text()))
        .collect();
      assert!(schema_only.is_empty(), "{text}: {schema_only:?}");
      if let Some(list) = tree
        .descendants()
        .find(|node: &SyntaxNode| node.kind() == K::ListValue)
      {
        let items: Vec<K> = list
          .children()
          .map(|node: SyntaxNode| node.kind())
          .collect();
        assert_eq!(items, [K::EnumValue, K::ObjectValue], "{text}");
      }
    }

    // No run reads on after a default: the member ends at its value, as main's does.
    let text = "mod M { input I { a: T = 1 + 2 b: Int } }";
    let tree: SyntaxNode = parse_executable_document(text).syntax();
    let fields: Vec<String> = tree
      .descendants()
      .filter(|node: &SyntaxNode| node.kind() == K::InputValueDefinition)
      .map(|node: SyntaxNode| node.text().to_string().trim().to_string())
      .collect();
    assert_eq!(fields, ["a: T = 1", "b: Int"], "{text}");

    // The module is refused first, and then the end of input where the first type argument should
    // stand, before the `<` unclosed.
    let text = "mod M { type T { f: Box<";
    let errors: Vec<Range<usize>> = parse_executable_document(text)
      .diagnostics()
      .iter()
      .filter(|diagnostic| diagnostic.severity() == Severity::Error)
      .map(|diagnostic| diagnostic.span())
      .collect();
    assert_eq!(errors.first(), Some(&(0..3)), "{text}: {errors:?}");
    assert_eq!(
      errors.get(1),
      Some(&(text.len()..text.len())),
      "{text}: {errors:?}"
    );
  }

  // ── A list on a definition's header that has lost its `)`: smear issue #295 ─────────────────

  /// The definitions and extensions that are direct children of the document, each as it is
  /// written.
  fn definitions(tree: &SyntaxNode) -> Vec<String> {
    top(tree)
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

  /// A list on a header that has lost its `)` ends in front of its owner's body, where the `}`
  /// that balances the body's `{` is written and the end of input or the start of a definition
  /// stands behind it: one report, at the `{`, the definition built with its member, and the next
  /// definition read. On each owner that reads a body, and behind another argument, a list or an
  /// object.
  #[test]
  fn a_headers_list_that_lost_its_closer_leaves_the_owners_body() {
    for (text, arguments, kept) in [
      (
        "type T @d(x: 1 { f: Int } scalar S",
        &["x: 1"][..],
        &["type T @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "interface N @d(x: 1 { f: Int } scalar S",
        &["x: 1"][..],
        &["interface N @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "input I @d(x: 1 { zz: Int } scalar S",
        &["x: 1"][..],
        &["input I @d(x: 1 { zz: Int }", "scalar S"][..],
      ),
      (
        "enum E @d(x: 1 { A B } scalar S",
        &["x: 1"][..],
        &["enum E @d(x: 1 { A B }", "scalar S"][..],
      ),
      (
        "schema @d(x: 1 { query: Q } scalar S",
        &["x: 1"][..],
        &["schema @d(x: 1 { query: Q }", "scalar S"][..],
      ),
      (
        "extend type T @d(x: 1 { f: Int } scalar S",
        &["x: 1"][..],
        &["extend type T @d(x: 1 { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 y: 2 { f: Int } scalar S",
        &["x: 1", "y: 2"][..],
        &["type T @d(x: 1 y: 2 { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: [1 2] { f: Int } scalar S",
        &["x: [1 2]"][..],
        &["type T @d(x: [1 2] { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: { a: 1 } { f: Int } scalar S",
        &["x: { a: 1 }"][..],
        &["type T @d(x: { a: 1 } { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 { f: Int }",
        &["x: 1"][..],
        &["type T @d(x: 1 { f: Int }"][..],
      ),
    ] {
      let at = text.rfind('{').expect("the body's brace");
      let brace = at..at + 1;
      assert_eq!(
        error_spans(text),
        [brace],
        "{text}: one report, at the body's brace"
      );
      let answer = read(text);
      assert_eq!(
        members_of(&answer.tree, K::Arguments, K::Argument),
        arguments,
        "{text}"
      );
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }
  }

  /// Where the owner reads no body a `{` on its header is never its own, and the list reads it as
  /// it always did, as junk: the members inside the group are the list's, and the reports are the
  /// ones the list always made. A scalar, a union, a directive definition, a scalar's extension,
  /// and a scalar in a module, which closes at its own `}`.
  #[test]
  fn an_owner_that_reads_no_body_reads_a_brace_on_its_header_as_before() {
    for (text, list, member, members, reports) in [
      (
        "scalar A @d(x: 1 { a: 1 }",
        K::Arguments,
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "union U @d(x: 1 { a: 1 }",
        K::Arguments,
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "extend scalar A @d(x: 1 { a: 1 }",
        K::Arguments,
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "mod m { scalar A @d(x: 1 { a: 1 } }",
        K::Arguments,
        K::Argument,
        &["x: 1", "a: 1"][..],
        3,
      ),
      (
        "directive @d(a: Int { b: Int }",
        K::ArgumentsDefinition,
        K::InputValueDefinition,
        &["a: Int", "b: Int"][..],
        5,
      ),
    ] {
      let answer = read(text);
      assert_eq!(members_of(&answer.tree, list, member), members, "{text}");
      assert_eq!(answer.reports, reports, "{text}");
    }
    assert!(holds_a_closing_brace(&module(
      &read("mod m { scalar A @d(x: 1 { a: 1 } }").tree,
      "mod m"
    )));
  }

  /// A list on a header that has lost its `)` ends in front of the start of a definition: a keyword
  /// a root restarts at, with a token behind it that is neither a `:` nor a second such keyword.
  /// One report, the `:` the member there would miss, or the unexpected token at a string; the
  /// definition is kept and the next one read. A description's string in front of the keyword is
  /// that definition's, and `extend` with the keyword of what it extends is one start, so what
  /// follows is the extension the text writes.
  #[test]
  fn a_headers_list_that_lost_its_closer_leaves_the_next_definition() {
    for (text, kept) in [
      (
        "type T @d(x: 1 scalar S",
        &["type T @d(x: 1", "scalar S"][..],
      ),
      (
        "scalar A @d(x: 1 scalar S",
        &["scalar A @d(x: 1", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 { f: Int } type U { g: Int }",
        &["type T @d(x: 1 { f: Int }", "type U { g: Int }"][..],
      ),
      (
        "type T @d(x: 1 \"doc\" scalar S",
        &["type T @d(x: 1", "\"doc\" scalar S"][..],
      ),
      (
        "type T @d(x: 1 extend type U { g: Int } scalar S",
        &["type T @d(x: 1", "extend type U { g: Int }", "scalar S"][..],
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}");
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }
  }

  /// A directive definition's two lists end in front of `repeatable` and `on`, by the test the
  /// start of a definition makes of the token behind: the directive definition keeps what it
  /// writes, and the next definition is read.
  #[test]
  fn a_directive_definitions_list_that_lost_its_closer_leaves_its_tail() {
    for (text, kept, reports) in [
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
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, reports, "{text}");
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }
  }

  /// A list on a header that has lost its `)` also ends in front of a union's members, in front of
  /// the owner's next directive and in front of `where` where the owner reads one, each on its
  /// evidence: the list keeps its own argument and the owner reads the rest. Where the list's `)`
  /// stands behind the `@` or the `=`, or a `:` behind the name after the `@`, the head is the
  /// list's junk, as it always was.
  #[test]
  fn a_headers_list_that_lost_its_closer_leaves_the_members_the_next_directive_and_where() {
    for (text, kept) in [
      (
        "union U @d(x: 1 = A | B scalar S",
        &["union U @d(x: 1 = A | B", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 @e { f: Int } scalar S",
        &["type T @d(x: 1 @e { f: Int }", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 @e(y: 2) @g scalar S",
        &["type T @d(x: 1 @e(y: 2) @g", "scalar S"][..],
      ),
      (
        "type T @d(x: 1 where K: L { f: Int } scalar S",
        &["type T @d(x: 1 where K: L { f: Int }", "scalar S"][..],
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}");
      assert_eq!(
        members_of(&answer.tree, K::Arguments, K::Argument),
        ["x: 1"],
        "{text}"
      );
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }

    for (text, kept, reports) in [
      // `e` is then an argument's name, and the type is lost at its missing `:`.
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
      (
        "type T @d(x: 1 @ e: 1) { f: Int }",
        &["type T @d(x: 1 @ e: 1) { f: Int }"][..],
        1,
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, reports, "{text}");
      assert_eq!(definitions(&answer.tree), kept, "{text}");
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
    for text in [
      "union U @d(x: 1 = A | enum query { VALUE }",
      "type T @d(x: 1 @a @enum query { VALUE }",
    ] {
      for (root, tree, _) in at_both_roots(text) {
        assert_eq!(
          written(&tree, K::EnumTypeDefinition),
          ["enum query { VALUE }"],
          "{text} at the {root} root"
        );
      }
    }

    // `directive` stands in the middle of the chain, and `on FIELD` behind its last directive.
    let text = "directive @a(@e @\ndirective @b on FIELD";
    for (root, tree, _) in at_both_roots(text) {
      assert_eq!(
        written(&tree, K::DirectiveDefinition),
        ["directive @b on FIELD"],
        "{text} at the {root} root"
      );
    }
  }

  /// What a look-ahead crosses on its way to its evidence is asked a token at a time, at every
  /// depth, so a definition that begins inside it is no evidence: a keyword inside a directive's
  /// arguments, inside a path's type arguments, on a directive and on a union's member, and inside
  /// the body at the head; and a `)` inside a directive's arguments with a `[` still open in front
  /// of it. The list skips its junk as it always did, and the root reads the definition, where the
  /// evidence would have made it a value, a type argument or a member. In the first two the owner
  /// is itself a scalar, so `S` stands beside it.
  #[test]
  fn a_definition_inside_what_a_look_ahead_crosses_is_no_evidence() {
    for text in [
      "scalar A @d(@e(x:[\nscalar S\n])",
      "scalar A @d(@e<[\nscalar S\n]>",
    ] {
      for (root, tree, _) in at_both_roots(text) {
        let scalars = written(&tree, K::ScalarTypeDefinition);
        assert!(
          scalars.iter().any(|scalar| scalar == "scalar S"),
          "{text} at the {root} root: {scalars:?}"
        );
      }
    }

    for text in [
      "union U @d(x: 1 = A<[\nscalar S\n]>",
      "type T @d(x: 1 { f: Int @deprecated scalar S }",
      "type T @d(x: 1 @e(y: [1) scalar S ])",
    ] {
      for (root, tree, _) in at_both_roots(text) {
        assert_eq!(
          written(&tree, K::ScalarTypeDefinition),
          ["scalar S"],
          "{text} at the {root} root"
        );
      }
    }

    let text = "enum E @d(x: 1 { A B scalar S } type T { f: Int }";
    for (root, tree, _) in at_both_roots(text) {
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

  /// A mixed root reads as an operation a `{` its scan leaves at the root, so a look-ahead finds
  /// nothing where main would read one: at a `{` inside a directive's arguments, at the end of
  /// input behind a body reached through a directive, and behind a body that leaves a bracket open
  /// at its `}`, which hides from main's scan, blind to which closer answers which opener, the
  /// definition behind it. The root reads the operation main reads, and the definition after it.
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
    for (root, tree, _) in at_both_roots(text) {
      assert_eq!(
        written(&tree, K::ScalarTypeDefinition),
        ["scalar S"],
        "{text} at the {root} root"
      );
    }
  }

  /// A look-ahead crosses brackets only as deep as the parse has left, and only where each closes
  /// the one open last: the reading that would take its evidence descends a frame for each, where
  /// main scans them flat, and a refusal there ends the document. Inside a directive's arguments,
  /// `}` answers no `[` under the default ceiling, and under a raised one 300 `[` nest past what a
  /// crossing holds; inside a body, `)` answers no `[`. The type-system root lends no refusal, and
  /// both roots read the definition behind as main does, which a refusal would have ended the
  /// document in front of.
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

  /// A look-ahead takes at most 256 tokens, trivia among them, from the head it is made at, so
  /// what it lexes and gives back stays a constant there. Codex's witness stands five thousand
  /// blanks inside the body it would cross: a look-ahead that crossed them all spent the scan
  /// allowance with what it gave back, so that the list's next skip took one token and `T` was
  /// lost. Here it stops at the cap, finds nothing, and leaves the allowance open, the list skips
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

  /// The cap counts from the head: with 246 blanks in front of `f` the look-ahead takes 256
  /// tokens, to the blank in front of `S`, finds `scalar S` behind the body, and the list ends in
  /// front of the body; with 247 it would take a 257th, finds nothing, and the list reads its junk
  /// as main reads it, `f: Int` among its arguments. `S` is a definition either way.
  #[test]
  fn a_look_ahead_finds_its_evidence_up_to_its_cap_and_none_past_it() {
    for (blanks, kept) in [(246, true), (247, false)] {
      let text = ["type T @d(x: 1 {", &" ".repeat(blanks), "f: Int } scalar S"].concat();
      let answer = read(&text);
      let fields = written(&answer.tree, K::FieldDefinition);
      assert_eq!(
        fields.len(),
        usize::from(kept),
        "{blanks} blanks: {fields:?}"
      );
      let arguments = written(&answer.tree, K::Argument);
      assert_eq!(
        arguments.iter().any(|argument| argument == "f: Int"),
        !kept,
        "{blanks} blanks: {arguments:?}"
      );
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{blanks} blanks");
    }
  }

  /// The reading that would take a look-ahead's evidence requests a parser frame at a group's own
  /// opener, and at `set!` or `map!` in front of the `{` it asks for next, and the look-ahead asks
  /// for both. Codex's witness, under a nesting ceiling of one: `@e`'s arguments would take the
  /// one frame `@d`'s leave free, and `set!` would request a second, which ends the document. The
  /// list reads as main reads it, no root is refused, and `S` is a definition at both.
  #[test]
  fn a_look_ahead_asks_for_every_frame_the_reading_would_request() {
    let text = "scalar A @d(] @e(x:set!) scalar S";
    let limits = LosslessLimits::with_max_nesting_depth(1);
    let errors = lent(&Lossless::with_limits(limits), text);
    assert!(
      !errors
        .iter()
        .any(|(code, _)| code == "smear::syntax::nesting-limit"),
      "{text}: {errors:?}"
    );
    for (root, parse) in [
      ("mixed", parse_document_with_limits(text, limits)),
      (
        "type-system",
        parse_type_system_document_with_limits(text, limits),
      ),
    ] {
      let scalars = written(&parse.syntax(), K::ScalarTypeDefinition);
      assert!(
        scalars.iter().any(|scalar| scalar == "scalar S"),
        "{text} at the {root} root: {scalars:?}"
      );
    }
  }

  /// A brace inside a header's list stays the list's junk, as it always was, where no evidence of a
  /// body stands behind its `}` or no `}` balances it: the list reads the members around and inside
  /// it, and the definition keeps its own body.
  #[test]
  fn a_brace_inside_a_headers_list_stays_the_lists() {
    for (text, arguments, field, reports) in [
      (
        "type T @d({ a: 1 }) { f: Int } scalar S",
        &["a: 1"][..],
        "f: Int",
        2,
      ),
      (
        "type T @d(x: 1 { a: 1 } y: 2) { f: Int } scalar S",
        &["x: 1", "a: 1", "y: 2"][..],
        "f: Int",
        2,
      ),
      (
        "type T @d(x: 1 { }) { f: Int } scalar S",
        &["x: 1"][..],
        "f: Int",
        2,
      ),
      (
        "type T @d(x: 1 { } } ) { f: Int } scalar S",
        &["x: 1"][..],
        "f: Int",
        3,
      ),
      (
        "type T @d(x: 1 { f: Int ) { g: Int } scalar S",
        &["x: 1", "f: Int"][..],
        "g: Int",
        1,
      ),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, reports, "{text}");
      assert_eq!(
        members_of(&answer.tree, K::Arguments, K::Argument),
        arguments,
        "{text}"
      );
      assert_eq!(
        members_of(&answer.tree, K::FieldsDefinition, K::FieldDefinition),
        [field],
        "{text}"
      );
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
    }
  }

  /// No valid document makes a stop: a member named like a definition's keyword or like a word its
  /// owner reads has its `:` behind it, and a described member is a member.
  #[test]
  fn a_valid_header_reads_as_before() {
    for text in [
      "type T @d(type: 1 input: 2) { f: Int }",
      "directive @d(on: Int repeatable: Int where: Int) on FIELD",
      "directive @d(\"doc\" type: Int) on FIELD",
      "scalar X @extern(lang: \"rust\", type: \"x\")",
    ] {
      let answer = read(text);
      assert!(!answer.errors, "{text}");
      assert_eq!(definitions(&answer.tree), [text], "{text}");
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

  /// The peek window lexes nothing twice where it meets no end of input across a run of lexer
  /// errors: a header's argument named like a definition's keyword reads under the least
  /// produce-event budget the same text reads under with a name that spells nothing, and so does a
  /// described member of a directive definition, whose name's token behind stands past the cache
  /// and is not read. With lexer errors in front of that token the window would have lexed them
  /// all to fill its one slot, and each again where the member reads them. The last text's errors
  /// have a `:` behind them. Where a window meets the end of input across such a run, the parse
  /// lexes the run a second time, until tokora retains the end of input after an error run.
  #[test]
  fn the_window_lexes_nothing_twice() {
    let text = "scalar X @extern(lang: \"rust\", type: \"x\")";
    let limits = LosslessLimits::default().with_max_produce_events(lexemes(text) + 1);
    assert!(
      !read_with(&Lossless::with_limits(limits), text).errors,
      "{text}"
    );

    for (text, plain, more) in [
      (
        "scalar X @extern(lang: \"rust\", type: \"x\")",
        "scalar X @extern(lang: \"rust\", tipe: \"x\")",
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

  /// A list in a member reads as it did: inside a definition's braces a keyword is a member's name,
  /// which loses the definition at its missing `:`, and a `{` is the list's junk.
  #[test]
  fn a_list_in_a_member_reads_as_before() {
    for (text, reports) in [
      ("type T { f: Int @d(x: 1 scalar S", 1),
      ("type T { f(a: Int scalar S", 1),
      ("type T { f: Int @d(x: 1 { g: Int } scalar S", 3),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, reports, "{text}");
      assert!(definitions(&answer.tree).is_empty(), "{text}");
    }
  }

  /// Where a test says nothing the list reads its head as it always did: a keyword at the end of
  /// input, a `{` that nothing balances, a body with junk behind it, a keyword behind a keyword,
  /// a keyword with two blanks behind it, which the window does not see past, and a described
  /// keyword whose token behind stands past the cache, which the window does not read.
  #[test]
  fn a_headers_list_reads_as_before_where_no_test_holds() {
    for (text, kept, reports) in [
      ("type T @d(x: 1 scalar", &[][..], 1),
      ("type T @d(x: 1 {", &["type T @d(x: 1 {"][..], 2),
      (
        "type T @d(x: 1 { f: Int",
        &["type T @d(x: 1 { f: Int"][..],
        2,
      ),
      (
        "type T @d(x: 1 { f: Int } junk scalar S",
        &["scalar S"][..],
        3,
      ),
      ("scalar A @d(x: 1 type scalar S", &["scalar S"][..], 1),
      ("type T @d(x: 1 scalar  S", &[][..], 1),
      ("directive @d(a: Int \"doc\" scalar S", &[][..], 1),
      ("directive @d(a: Int \"doc\"scalar S", &[][..], 1),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, reports, "{text}");
      assert_eq!(definitions(&answer.tree), kept, "{text}");
    }
  }

  /// In a module a header's list stands one brace deep and still ends in front of its owner's
  /// body: the definition keeps its field, the item behind it is the module's, and the module
  /// closes at its own `}`, which may stand behind the body as its evidence.
  #[test]
  fn a_headers_list_in_a_module_leaves_the_body_to_its_owner() {
    for (text, items) in [
      (
        "mod m { type T @d(x: 1 { f: Int } scalar S }",
        &["scalar S"][..],
      ),
      ("mod m { type T @d(x: 1 { f: Int } }", &[][..]),
    ] {
      let answer = read(text);
      assert_eq!(answer.reports, 1, "{text}");
      assert_eq!(
        members_of(&answer.tree, K::FieldsDefinition, K::FieldDefinition),
        ["f: Int"],
        "{text}"
      );
      let m = module(&answer.tree, "mod m");
      assert_eq!(scalars(&m), items, "{text}");
      assert!(
        holds_a_closing_brace(&m),
        "{text}: `m` closes at its own brace"
      );
    }
  }

  /// A run on a header keeps the definition. A `(` the run leaves open takes the list's `)` and
  /// the argument in front of it (R21), and the list then ends in front of the body; where the
  /// run's junk takes the list's `)`, the definition keeps all it writes.
  #[test]
  fn a_run_on_a_header_keeps_the_definition() {
    for (operand, behind) in [
      ("(c", " y: 2"),
      ("([2", " y: 2"),
      ("Url(c", " y: 2"),
      ("( { ] }", ""),
      ("( { > }", ""),
      ("(1 >> 2", ""),
    ] {
      let text = format!("type T @d(x: 1 + {operand}{behind}) {{ f: Int }} scalar S");
      let answer = read(&text);
      assert_eq!(
        members_of(&answer.tree, K::Arguments, K::Argument),
        ["x: 1"],
        "{text}"
      );
      assert_eq!(
        members_of(&answer.tree, K::FieldsDefinition, K::FieldDefinition),
        ["f: Int"],
        "{text}"
      );
      assert_eq!(scalars(&top(&answer.tree)), ["scalar S"], "{text}");
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
      let answer = read(text);
      assert_eq!(definitions(&answer.tree), kept, "{text}");
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
    let answer = read(text);
    assert_eq!(answer.reports, 0, "{text}");
    assert_eq!(
      definitions(&answer.tree),
      ["directive @d on FIELD | OBJECT", "scalar S"],
      "{text}"
    );
  }

  /// The report at a head no valid list holds is made before the look-ahead behind it, so no
  /// produce-event budget can stop the parse in front of it: under every budget the parse either
  /// stops before it reaches the `{`, with the budget's refusal its one error, or lends the
  /// unexpected token at the `{` first.
  #[test]
  fn a_headers_report_comes_before_its_look_ahead_under_every_budget() {
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

  /// A lexer's error in front of a stop's report keeps its place: the window lexes no further than
  /// the member there reads to its `:`, so the unknown character is the first error, in both
  /// layers, as it always was.
  #[test]
  fn a_lexers_error_behind_a_keyword_on_a_header_keeps_its_place() {
    let text = "type T @d(x: 1 scalar ? S";
    let at = text.find('?').expect("the unknown character");
    let unknown = (String::from("smear::syntax::unknown-character"), at..at + 1);
    assert_eq!(
      lent(&Lossless::new(), text).first(),
      Some(&unknown),
      "{text}"
    );

    let mut syntactic: Vec<GraphqlxError<&str>> = Vec::new();
    let _ = Syntactic::new().type_system_document(text, &mut syntactic);
    let first = syntactic
      .first()
      .expect("the syntactic layer refuses the text");
    let span = first.primary().span();
    assert_eq!(
      (
        String::from(first.code().as_str()),
        span.map_or(0, |span| span.start())..span.map_or(0, |span| span.end()),
      ),
      unknown,
      "{text}"
    );
  }

  // ── Through the analyzer ───────────────────────────────────────────────────────────────────

  #[cfg(feature = "analysis")]
  mod analysis {
    use smear::{
      analysis::graphqlx::{
        DefId, Layout, Limits, Model, ModuleId, Options, Package, SourceFile, Tier, analyze_with,
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
        let (analysis, _trees) = analyze_with(
          &Lossless::new(),
          &root,
          &[],
          &Limits::new(),
          &Options::new(),
        );
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
      let (analysis, _trees) = analyze_with(
        &Lossless::new(),
        &root,
        &[],
        &Limits::new(),
        &Options::new(),
      );
      let model: &Model = analysis.model();
      let m: ModuleId = model.module_by_path("^::app::m").expect("the `mod m`");
      let n: ModuleId = model.module_by_path("^::app::n").expect("the `mod n`");
      assert!(!names(model, m).contains(&"B"), "`m` declares `B`");
      assert_eq!(names(model, n), ["B"]);
    }
  }
}
