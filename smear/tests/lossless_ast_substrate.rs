#![cfg(feature = "rowan")]
// `smear-parser` is `#![deny(missing_docs)]` and Task 10 generates its fifty-nine wrappers
// *inside* it, so the macro's ability to carry documentation onto the items it emits is a hard
// requirement rather than a nicety. Denying the lint here is what tests it: an attribute that
// the macro dropped would surface as an error on the generated `pub` item.
#![deny(missing_docs)]

//! Task 9's gate: the `ast_node!` macro over tokora's `CastNode` substrate.
//!
//! The substrate itself is upstream — `CastNode`, `NodeChildren` and the `cast::{child,
//! children, token}` helpers are tokora's since PR #132 — so what is under test here is the
//! macro that projects them onto one node kind, and the `AstChildren` alias that pins the
//! language. Local wrappers prove it without waiting for Task 10's fifty-nine.
//!
//! **The fixtures are chosen so that each getter form has something to get wrong.** A parent
//! with one child of one kind cannot tell `cast::child` apart from "take the first child", and
//! a node whose wanted token happens to be its first cannot tell `cast::token` apart from "take
//! the first token" — so no fixture here has that shape.

use smear::parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    GraphQLLang,
    ast::{AstChildren, CastNode, NodeChildren, cast},
    parse_document,
  },
};

// Every doc comment below sits **inside** the invocation's delimiters. That is the only
// placement the macro can see: a `///` written above `ast_node!(…)` documents the *invocation*,
// which rustc discards with an `unused_doc_comment` warning and `missing_docs` then reports as
// an undocumented item. The `deny` at the top of this file is what turns that trap into a
// failure here instead of in Task 10.
smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// A document.
  TestDoc => K::Document {
    /// Every operation definition in the document.
    definitions: many TestOpDef,
  }
);

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// An operation definition.
  TestOpDef => K::OperationDefinition {
    /// The operation's selection set.
    selection_set: opt TestSelSet,
    /// The operation's directives.
    directives: opt TestDirectives,
    /// The operation's own name token.
    name: tok K::Name,
  }
);

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// A selection set.
  TestSelSet => K::SelectionSet {
    /// Every field selected.
    fields: many TestField,
    /// A `SelectionSet` owns no `Name` token — every one beneath it belongs to a `Field`, so
    /// this getter exists only to be asserted `None`.
    name: tok K::Name,
  }
);

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// A field selection.
  TestField => K::Field {
    /// The field's own name token.
    name: tok K::Name,
  }
);

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// A directives list.
  TestDirectives => K::Directives {
    /// Every directive in the list.
    directives: many TestDirective,
  }
);

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// A single directive.
  TestDirective => K::Directive {
    /// The directive's name token, which its `@` precedes.
    name: tok K::Name,
  }
);

// An empty getter list, which several of Task 10's wrappers will have: the muncher's base arm
// has to accept a body with nothing in it, not merely the tail of a non-empty one.
smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
  /// An operation-type keyword, which carries nothing but its own token.
  TestOpType => K::OperationType {}
);

/// The text of a token getter's answer, or `None`.
fn tok_text(t: Option<smear::parser::graphql::lossless::SyntaxToken>) -> Option<String> {
  t.map(|t| t.text().to_string())
}

#[test]
fn cast_node_accepts_its_own_kind_and_rejects_others() {
  let p = parse_document("{ a }");
  let root = p.syntax();
  let doc = root.children().find(|n| n.kind() == K::Document).unwrap();
  assert!(TestDoc::cast_node(doc.clone()).is_some());
  assert!(
    TestDoc::cast_node(root.clone()).is_none(),
    "Root must not cast to Document"
  );

  // A near neighbour, not merely the parent: two node kinds that sit one step apart in the same
  // tree must not cast into each other's wrapper.
  let set = root
    .descendants()
    .find(|n| n.kind() == K::SelectionSet)
    .unwrap();
  assert!(TestSelSet::cast_node(set.clone()).is_some());
  assert!(
    TestField::cast_node(set).is_none(),
    "SelectionSet must not cast to Field"
  );

  // A wrapper declared with an empty getter list is still a wrapper.
  let op_type = parse_document("query Q { a }")
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::OperationType)
    .unwrap();
  let typed = TestOpType::cast_node(op_type.clone()).expect("an empty body still casts");
  assert_eq!(typed.syntax(), &op_type);
}

#[test]
fn syntax_round_trips_so_a_consumer_can_cross_back_to_rowan() {
  // `CastNode` is a one-way door — it has no `syntax()`. This asserts the inherent method the
  // macro generates in its place, which is the only way back out to untyped rowan.
  let p = parse_document("{ a }");
  let root = p.syntax();
  let doc = root.children().find(|n| n.kind() == K::Document).unwrap();
  let typed = TestDoc::cast_node(doc.clone()).unwrap();
  assert_eq!(
    typed.syntax(),
    &doc,
    "the wrapper must be transparent over its node"
  );
}

#[test]
fn the_generated_getters_navigate_through_tokoras_cast_helpers() {
  // Tree (probed, not assumed):
  //   Document > OperationDefinition > [OperationType, Space, Name "Q", Space, Directives,
  //                                     SelectionSet]
  //   Directives > Directive > [At "@", Name "d", Space]
  //   SelectionSet > [LBrace, Space, Field "a ", Field "b ", RBrace]
  let p = parse_document("query Q @d { a b }");
  let root = p.syntax();
  let doc: TestDoc = cast::child(&root).expect("Root wraps one Document");
  let op = doc
    .definitions()
    .next()
    .expect("`many` yields the operation");

  // `opt`: the selection set is the operation's *last* node child, behind an `OperationType` and
  // a `Directives` that both decline the cast. "Take the first child and cast it" answers `None`
  // here; `cast::child` scans until a cast succeeds.
  let set = op
    .selection_set()
    .expect("`opt` must find the selection set");
  assert_eq!(set.syntax().text().to_string(), "{ a b }");
  let dirs = op
    .directives()
    .expect("`opt` must find the directives list");
  assert_eq!(dirs.syntax().text().to_string(), "@d ");

  // `many` returns the language-pinned alias, and the alias is the upstream iterator.
  let fields: AstChildren<TestField> = set.fields();
  let _: NodeChildren<TestField, GraphQLLang> = set.fields();
  assert_eq!(
    fields
      .filter_map(|f| tok_text(f.name()))
      .collect::<Vec<_>>(),
    ["a", "b"],
    "`many` must yield both fields, in order"
  );

  // `tok`, the sharp one. `OperationDefinition`'s first *direct* token is a Space, and the first
  // `Name` *anywhere beneath* it is `query`, inside the `OperationType` node. Only a getter that
  // filters direct token children by kind answers `"Q"` — "first token" answers `" "`, and
  // "first matching descendant token" answers `"query"`.
  assert_eq!(
    tok_text(op.name()).as_deref(),
    Some("Q"),
    "`tok` must find the operation's own Name token"
  );

  // `tok` again where the wanted token is not the node's first: `@` precedes the directive name.
  let d = dirs
    .directives()
    .next()
    .expect("`many` must find the directive");
  assert_eq!(
    tok_text(d.name()).as_deref(),
    Some("d"),
    "`tok` must skip the `@` and answer the Name"
  );
}

#[test]
fn many_filters_by_kind_rather_than_yielding_every_child() {
  let p = parse_document("query Q { a } fragment F on T { c }");
  let root = p.syntax();
  let doc: TestDoc = cast::child(&root).expect("Root wraps one Document");

  // Fixture guard first: an unfiltered `many` and a filtered one agree whenever every child
  // matches, so this test is worthless unless the Document really carries a non-matching sibling.
  assert_eq!(
    doc.syntax().children().count(),
    2,
    "fixture must carry a definition that is NOT an OperationDefinition"
  );
  assert_eq!(
    doc.definitions().count(),
    1,
    "`many` must decline the FragmentDefinition"
  );
}

#[test]
fn a_tok_getter_declines_a_token_that_belongs_to_a_child_node() {
  // The negative half of `cast::token`'s contract, on a node that has no `Name` of its own but
  // plenty beneath it. Without this, a getter built on `descendants_with_tokens` passes every
  // positive assertion above by luck of document order.
  let p = parse_document("query Q { a b }");
  let root = p.syntax();
  let set: TestSelSet = cast::child(&root)
    .and_then(|d: TestDoc| d.definitions().next())
    .and_then(|op| op.selection_set())
    .expect("the operation has a selection set");
  let field: TestField = set.fields().next().expect("the set has a field");

  // Fixture guard: the decline below is only meaningful if the tokens are actually there.
  assert!(
    set
      .syntax()
      .descendants_with_tokens()
      .any(|e| e.kind() == K::Name),
    "fixture guard: the selection set must contain Name tokens, just not as direct children"
  );
  assert_eq!(
    tok_text(set.name()),
    None,
    "`tok` must not reach into a child node for its token"
  );
  // And the same tokens answer from the node that does own them.
  assert_eq!(tok_text(field.name()).as_deref(), Some("a"));
}
