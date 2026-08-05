//! The GraphQLx type-system extensions and the three document roots.
//!
//! **This is the first file that can drive `parse_str`**, because it is the task that replaces
//! Task 9's drain-everything stub with the real `document_entry`. Every earlier test file says why
//! it could not: written through the stub, an assertion compared two one-element `[Root]` trees and
//! passed vacuously.
//!
//! The assertions stay node-kind pre-orders and node extents. The document level's own defects are
//! text-invisible in the same way the productions' were: a description attached to the wrong entry,
//! an extension parsed as a definition, an entry that swallowed the next one.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    extension::test_support::{parse_type_system_extension, parse_union_type_extension},
    parse_str,
  },
};

/// The tree's node kinds in pre-order, ignoring tokens and trivia.
macro_rules! kinds {
  ($driver:ident, $src:expr) => {
    $driver($src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<K>>()
  };
}

/// The text of the first descendant of `kind`, so a test can pin *which* bytes a node covers.
macro_rules! node_text {
  ($driver:ident, $src:expr, $kind:expr) => {
    $driver($src)
      .syntax()
      .descendants()
      .find(|n| n.kind() == $kind)
      .map(|n| n.text().to_string())
  };
}

// ---------------------------------------------------------------------------------------------
// The seven extensions.
// ---------------------------------------------------------------------------------------------

#[test]
fn each_extension_keyword_opens_its_own_node_over_an_extension_name() {
  for (src, kind) in [
    ("extend scalar S @d", K::ScalarTypeExtension),
    ("extend type T { f: Int }", K::ObjectTypeExtension),
    ("extend interface I { f: Int }", K::InterfaceTypeExtension),
    ("extend union U = A", K::UnionTypeExtension),
    ("extend enum E { RED }", K::EnumTypeExtension),
    ("extend input In { f: Int }", K::InputObjectTypeExtension),
    ("extend schema { query: Q }", K::SchemaExtension),
  ] {
    let parse = parse_type_system_extension(src);
    assert!(!parse.has_errors(), "{src} must parse clean");
    assert_eq!(
      parse.syntax().descendants().nth(1).map(|n| n.kind()),
      Some(kind),
      "{src} must open {kind:?}"
    );
    assert_eq!(parse.syntax().text().to_string(), src);
    assert_eq!(
      node_text!(parse_type_system_extension, src, kind).as_deref(),
      Some(src),
      "the `extend` keyword is inside the extension node, not beside it"
    );
  }
}

/// Every extension needs **at least one** component after its name.
///
/// `syntactic/`'s six extension tails each end in a `(None, None, …) => Err` arm, and gate 1
/// compares verdicts. The name alone re-prints as valid text, so only the verdict can see it.
#[test]
fn a_bare_extension_with_no_components_reports() {
  for src in [
    "extend scalar S",
    "extend type T",
    "extend interface I",
    "extend union U",
    "extend enum E",
    "extend input In",
    "extend schema",
  ] {
    let parse = parse_type_system_extension(src);
    assert!(parse.has_errors(), "{src} must report");
    assert_eq!(parse.syntax().text().to_string(), src);
  }
}

/// An extension's target is an `ExtensionName`, which is a `Path` and may carry generic arguments.
#[test]
fn an_extension_names_a_path_with_optional_generic_arguments() {
  assert_eq!(
    kinds!(parse_type_system_extension, "extend type ns::T<A> @d"),
    vec![
      K::Root,
      K::ObjectTypeExtension,
      K::ExtensionName,
      K::Path,
      K::ExtensionTypeGenerics,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
}

/// Divergence 16's two **extension** sites, which the design omitted.
#[test]
fn a_where_clause_makes_an_extension_block_mandatory() {
  for (constrained, unconstrained) in [
    ("extend type T @d where A: B", "extend type T @d"),
    ("extend input In @d where A: B", "extend input In @d"),
  ] {
    assert!(
      !parse_type_system_extension(unconstrained).has_errors(),
      "{unconstrained} is well formed: the block alone is optional"
    );
    let parse = parse_type_system_extension(constrained);
    assert!(
      parse.has_errors(),
      "{constrained} must report: a present `where` makes the block mandatory"
    );
    assert_eq!(parse.syntax().text().to_string(), constrained);
  }
  assert!(!parse_type_system_extension("extend type T where A: B { f: Int }").has_errors());
  assert!(!parse_type_system_extension("extend input In where A: B { f: Int }").has_errors());
}

/// Divergence 17's **extension** site: the clause follows the members and requires them.
#[test]
fn an_extension_union_where_clause_requires_its_members() {
  assert!(!parse_union_type_extension("extend union U = A where A: B").has_errors());
  let parse = parse_union_type_extension("extend union U @d where A: B");
  assert!(
    parse.has_errors(),
    "a union extension's `where` with no members must report `Equal`"
  );
  assert_eq!(
    kinds!(parse_union_type_extension, "extend union U = A where A: B"),
    vec![
      K::Root,
      K::UnionTypeExtension,
      K::ExtensionName,
      K::Path,
      K::UnionMemberTypes,
      K::TypePath,
      K::Path,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ],
    "members first, then the clause — the reverse order re-prints identically"
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 22 — who may carry a description.
// ---------------------------------------------------------------------------------------------

/// A description may precede an **executable or type-system definition**, and nothing else.
///
/// `entry_after_description` commits to a definition and never reaches an import or an extension
/// (`graphqlx/syntactic/document.rs:210-221`), so `"doc" import * from "x"` and
/// `"doc" extend type T @d` are documents `syntactic/` rejects. Both re-print byte for byte and
/// both build every node they would have built undescribed, so only the verdict moves — which is
/// exactly why the mutation "allow a description before an import" is invisible to a round-trip
/// gate and to a golden tree alike.
#[test]
fn a_description_may_precede_a_definition_and_not_an_import_or_an_extension() {
  for src in [
    "\"doc\" scalar S",
    "\"doc\" query Q { f }",
    "\"doc\" fragment F on T { g }",
  ] {
    assert!(
      !parse_str(src).has_errors(),
      "a description belongs on a definition: {src}"
    );
  }
  for src in ["\"doc\" import * from \"x\"", "\"doc\" extend type T @d"] {
    let parse = parse_str(src);
    assert!(
      parse.has_errors(),
      "an import and an extension are deliberately undescribed: {src}"
    );
    assert_eq!(
      parse.syntax().text().to_string(),
      src,
      "and every byte is still kept"
    );
  }
  // The described import still builds its ImportDefinition, so a diagnostic has a node to point
  // at — the same trade every rejected-but-kept shape in this suite makes.
  assert!(
    parse_str("\"doc\" import * from \"x\"")
      .syntax()
      .descendants()
      .any(|n| n.kind() == K::ImportDefinition)
  );
}

// ---------------------------------------------------------------------------------------------
// The document roots.
// ---------------------------------------------------------------------------------------------

#[test]
fn a_document_holds_imports_definitions_and_extensions_side_by_side() {
  let src = "import { A } from \"a.graphql\"\n\
             \"doc\" type T { f: Int }\n\
             extend type T @d\n\
             query Q { f }\n";
  let parse = parse_str(src);
  assert!(!parse.has_errors(), "the mixed document must parse clean");
  assert_eq!(parse.syntax().text().to_string(), src, "round trip");
  assert_eq!(
    parse
      .syntax()
      .children()
      .next()
      .map(|n| n.kind())
      .expect("a document node"),
    K::Document
  );
  assert_eq!(
    parse
      .syntax()
      .descendants()
      .filter(|n| matches!(
        n.kind(),
        K::ImportDefinition
          | K::ObjectTypeDefinition
          | K::ObjectTypeExtension
          | K::OperationDefinition
      ))
      .map(|n| n.kind())
      .collect::<Vec<K>>(),
    vec![
      K::ImportDefinition,
      K::ObjectTypeDefinition,
      K::ObjectTypeExtension,
      K::OperationDefinition
    ],
    "four entries, in source order, each its own node"
  );
  assert_eq!(
    node_text!(
      parse_str,
      "\"doc\" type T { f: Int }",
      K::ObjectTypeDefinition
    )
    .as_deref(),
    Some("\"doc\" type T { f: Int }"),
    "the description is retro-wrapped into the definition"
  );
}

/// The document node covers **every byte**, leading and trailing trivia included.
#[test]
fn a_document_covers_the_whole_file() {
  let src = "\n# leading\nscalar S\n\n# trailing\n";
  let parse = parse_str(src);
  assert!(!parse.has_errors());
  assert_eq!(
    node_text!(parse_str, src, K::Document).as_deref(),
    Some(src),
    "a document IS the whole file"
  );
}

#[test]
fn an_empty_document_reports_and_is_still_a_node() {
  let parse = parse_str("");
  assert!(parse.has_errors(), "`syntactic/` rejects an empty document");
  assert_eq!(kinds!(parse_str, ""), vec![K::Root, K::Document]);
}

/// A failed entry is resynchronised past, and the entries after it survive.
///
/// This is the property the drain-everything stub could not have: an `Err` escaping an entry left
/// the rest of the source uncommitted, and `finish` refused it as an `UncoveredGap`. The entry
/// drains what an escape left behind, which turns the one failure mode `parse_str` could not
/// report into a reportable parse.
#[test]
fn a_broken_entry_costs_itself_and_not_the_rest_of_the_document() {
  let src = "type T { f: Int }\n!!!\nscalar S\n";
  let parse = parse_str(src);
  assert!(parse.has_errors());
  assert_eq!(parse.syntax().text().to_string(), src, "round trip holds");
  assert_eq!(
    parse
      .syntax()
      .descendants()
      .filter(|n| matches!(n.kind(), K::ObjectTypeDefinition | K::ScalarTypeDefinition))
      .map(|n| n.kind())
      .collect::<Vec<K>>(),
    vec![K::ObjectTypeDefinition, K::ScalarTypeDefinition],
    "the definitions on both sides of the junk survive"
  );
}

/// After a broken entry, **every** entry head is a restart point — including `import`.
///
/// `resync_to_definition`'s predicate is a table of fourteen spellings, and a missing row costs the
/// entry after the failure: the skip runs past its head and folds the whole entry into the `Error`
/// region. The text still round-trips — that is what makes the row invisible — so the witness has
/// to be the *node* the following entry should have built. Measured: dropping `Import` from the
/// table is green against every other test in this file, and `import` is the row GraphQL's own
/// table does not have, so it is the one a port would be missing.
#[test]
fn every_entry_head_is_a_restart_point_after_a_broken_entry() {
  for (tail, expected) in [
    ("import * from \"x\"", K::ImportDefinition),
    ("extend type T @d", K::ObjectTypeExtension),
    ("\"doc\" scalar S", K::ScalarTypeDefinition),
    ("type T { f: Int }", K::ObjectTypeDefinition),
    ("interface I { f: Int }", K::InterfaceTypeDefinition),
    ("union U = A", K::UnionTypeDefinition),
    ("enum E { RED }", K::EnumTypeDefinition),
    ("input In { f: Int }", K::InputObjectTypeDefinition),
    ("schema { query: Q }", K::SchemaDefinition),
    ("directive @d on FIELD", K::DirectiveDefinition),
    ("query Q { f }", K::OperationDefinition),
    ("mutation M { f }", K::OperationDefinition),
    ("subscription S { f }", K::OperationDefinition),
    ("fragment F on T { f }", K::FragmentDefinition),
  ] {
    // `scalar @d` fails inside `definition_name`, which is what sends the loop to the resync.
    let src = format!("scalar @d\n{tail}\n");
    let parse = parse_str(&src);
    assert!(parse.has_errors(), "the broken entry must report: {src:?}");
    assert_eq!(
      parse.syntax().text().to_string(),
      src,
      "round trip: {src:?}"
    );
    assert!(
      parse.syntax().descendants().any(|n| n.kind() == expected),
      "the entry after the failure must survive as {expected:?}: {src:?}"
    );
  }
}

/// Every byte reaches the tree, whatever the input.
#[test]
fn parse_str_round_trips_every_fixture() {
  for src in [
    "",
    "   ",
    "# just a comment\n",
    "{ f }",
    "import * as ns from \"x\"",
    "extend schema @d",
    "type T<A = Int> implements ns::I<A> @d where A: Node { f(a: Int = 1): [A!]! @d }",
    "directive @d<T>(a: Int) repeatable on FIELD | OBJECT where T: Node\ntype X { f: Int }",
    "query Q($x: set<Int> = set { 1 }) { a: f(m: map { 1 => 2 }) { ...F ... on T { g } } }",
    "!!! not a document at all &&&",
    "type T { f: Int",
    "union U = | A | ::ns::B where A: C & D",
  ] {
    let parse = parse_str(src);
    assert_eq!(
      parse.syntax().text().to_string(),
      src,
      "text fidelity must hold for every input, valid or not: {src:?}"
    );
  }
}
