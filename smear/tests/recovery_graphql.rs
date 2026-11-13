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
  use smear::parser::graphql::{Lossless, Recovery, kinds::SyntaxKind as K, lossless::SyntaxNode};

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

  /// The scalars that are direct children of the document's node, each as it is written.
  fn top_level_scalars(tree: &SyntaxNode) -> Vec<String> {
    tree
      .children()
      .find(|node: &SyntaxNode| node.kind() == K::TypeSystemDocument)
      .expect("the document's node")
      .children()
      .filter(|child: &SyntaxNode| child.kind() == K::ScalarTypeDefinition)
      .map(|child: SyntaxNode| child.text().to_string().trim().to_string())
      .collect()
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
}
