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
