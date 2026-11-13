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
//! layer lends its sink or how many errors it counts.

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
  use smear::parser::graphql::{
    Lossless, Recovery,
    kinds::SyntaxKind as K,
    lossless::{Parse, SyntaxNode, parse_document},
  };

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
}
