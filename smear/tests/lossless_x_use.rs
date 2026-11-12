//! The GraphQLx `use` productions and the extension half of the generic family.
//!
//! **Nothing here can be ported.** GraphQL has no `use` item, no generic parameters and no
//! extension name, so every production these tests cover is new code against a grammar the other
//! dialect does not have — and the "fork the GraphQL production and edit it" habit the previous
//! three tasks build is exactly the wrong instinct.
//!
//! As elsewhere in this suite the assertions are node-kind pre-orders and node extents rather than
//! verdicts, because the defects in this area change nesting while leaving the text identical: a
//! `use` group is the same bytes whether its trees are nodes or loose tokens, and an extension's
//! generic list is the same bytes whether it carries the extension kind or the executable one.
#![cfg(all(feature = "rowan_0_17", feature = "graphqlx"))]

use smear::parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    generic::test_support::{parse_extension_name, parse_extension_type_generics},
    ty::test_support::parse_type_generics,
    use_tree::test_support::{parse_use_definition, parse_use_tree},
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

/// Every byte the tree kept, in order — the assertion a boolean verdict cannot fake.
macro_rules! text {
  ($driver:ident, $src:expr) => {
    $driver($src).syntax().text().to_string()
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

/// The text of every descendant of `kind`, in pre-order — which bytes each of a group's trees
/// covers.
macro_rules! node_texts {
  ($driver:ident, $src:expr, $kind:expr) => {
    $driver($src)
      .syntax()
      .descendants()
      .filter(|n| n.kind() == $kind)
      .map(|n| n.text().to_string())
      .collect::<Vec<String>>()
  };
}

// ---------------------------------------------------------------------------------------------
// Divergence 21 — the `use` family.
// ---------------------------------------------------------------------------------------------

/// A tree's path is a `Path` child, and the alias after it is a bare name.
///
/// The alias narrows to a `Name`, one token, so it has no node: a second `Path` in this tree would
/// be an alias still parsed as a path.
#[test]
fn a_use_tree_holds_its_path_and_its_alias_is_a_name() {
  assert_eq!(
    kinds!(parse_use_tree, "a::b::C"),
    vec![K::Root, K::UseTree, K::Path]
  );
  assert_eq!(
    kinds!(parse_use_tree, "a::b::C as D"),
    vec![K::Root, K::UseTree, K::Path],
    "the alias is one Name token, not a Path"
  );
  assert!(!parse_use_tree("a::b::C as D").has_errors());
  assert_eq!(
    node_text!(parse_use_tree, "a::b::C as D", K::UseTree).as_deref(),
    Some("a::b::C as D"),
    "`as` and the alias are inside the tree, not beside it"
  );
}

/// A glob or a group follows the `::` after a path, and that `::` is the tree's, not the path's.
///
/// A [`Path`](K::Path) ends at its last name. The `::` in front of a `*` or a `{` is read only
/// once the token after it has said which, so it lands in the tree, outside the path.
#[test]
fn a_glob_or_a_group_follows_the_path_and_its_separator_is_the_trees() {
  assert_eq!(
    kinds!(parse_use_tree, "a::b::*"),
    vec![K::Root, K::UseTree, K::Path]
  );
  assert_eq!(
    node_text!(parse_use_tree, "a::b::*", K::Path).as_deref(),
    Some("a::b"),
    "the path stops before the `::` that leads to the glob"
  );
  assert_eq!(
    kinds!(parse_use_tree, "a::{b, c::*, d::{e}}"),
    vec![
      K::Root,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path
    ],
    "a group opens no node: its trees are children of the tree that holds it, and they nest"
  );
  assert!(!parse_use_tree("a::{b, c::*, d::{e}}").has_errors());
}

/// A bare root's tokens are the tree's own; a rooted path's are its `Path`'s.
///
/// The root is read before anything says whether a path follows it, and the tree retro-wraps it
/// into the path once a name does.
#[test]
fn a_bare_root_belongs_to_the_tree_and_a_rooted_path_to_its_path() {
  assert_eq!(
    kinds!(parse_use_tree, "~::*"),
    vec![K::Root, K::UseTree],
    "a bare root has no path to lead"
  );
  assert_eq!(
    kinds!(parse_use_tree, "..::..::{A}"),
    vec![K::Root, K::UseTree, K::UseTree, K::Path]
  );
  assert_eq!(
    kinds!(parse_use_tree, "^::{a, b}"),
    vec![
      K::Root,
      K::UseTree,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path
    ]
  );
  assert_eq!(
    node_text!(parse_use_tree, "^::ingraph::{MediaId}", K::Path).as_deref(),
    Some("^::ingraph"),
    "the root's tokens are wrapped into the path that follows them"
  );
  for src in ["~::*", "..::..::{A}", "^::{a, b}", "^::ingraph::{MediaId}"] {
    assert!(!parse_use_tree(src).has_errors(), "{src}");
  }
}

/// A string source is a `StringValue` child, and the names after its `::` are a relative `Path`.
#[test]
fn a_string_source_is_a_string_value_and_its_names_are_a_path() {
  for (src, expected) in [
    ("\"./vision\"", vec![K::Root, K::UseTree, K::StringValue]),
    (
      "\"./vision\" as v",
      vec![K::Root, K::UseTree, K::StringValue],
    ),
    (
      "\"../shared\"::MediaId",
      vec![K::Root, K::UseTree, K::StringValue, K::Path],
    ),
    (
      "\"./media\"::vision::Image as Img",
      vec![K::Root, K::UseTree, K::StringValue, K::Path],
    ),
    (
      "\"../shared\"::{MediaId}",
      vec![K::Root, K::UseTree, K::StringValue, K::UseTree, K::Path],
    ),
    (
      "\"./media\"::vision::*",
      vec![K::Root, K::UseTree, K::StringValue, K::Path],
    ),
  ] {
    assert!(!parse_use_tree(src).has_errors(), "{src}");
    assert_eq!(kinds!(parse_use_tree, src), expected, "{src}");
  }
  assert_eq!(
    node_text!(parse_use_tree, "\"./media\"::vision::*", K::Path).as_deref(),
    Some("vision"),
    "the source's `::` and the glob's are the tree's own; the path is the names between them"
  );
}

/// `as` after a complete path is always the keyword, and an item named `as` is written where the
/// grammar reads a name: first in its group, or straight after a `::`.
///
/// So `{b, as as x}` is `{b as as, x}`. The member texts are the witness: the verdict is clean
/// either way, and only where each tree ends tells the two readings apart.
#[test]
fn as_after_a_path_is_the_keyword_and_a_name_where_a_name_is_read() {
  assert_eq!(
    node_texts!(parse_use_tree, "a::{b, as as x}", K::UseTree),
    vec!["a::{b, as as x}", "b, as as", "x"],
    "the first `as` after `b` is the keyword, and the second is the alias it takes"
  );
  assert_eq!(
    node_texts!(parse_use_tree, "a::{as, b}", K::UseTree),
    vec!["a::{as, b}", "as, ", "b"],
    "first in its group, `as` is the item's own name"
  );
  assert_eq!(
    node_text!(parse_use_tree, "a::as as x", K::Path).as_deref(),
    Some("a::as "),
    "after a `::`, `as` is a name of the path, and the `as` after it is the keyword"
  );
  for src in ["a::{b, as as x}", "a::{as, b}", "a::as as x"] {
    assert!(!parse_use_tree(src).has_errors(), "{src}");
  }
}

#[test]
fn a_use_definition_holds_the_keyword_and_one_tree() {
  assert_eq!(
    kinds!(parse_use_definition, "use \"./x\"::{A, B as C}"),
    vec![
      K::Root,
      K::UseDefinition,
      K::UseTree,
      K::StringValue,
      K::UseTree,
      K::Path,
      K::UseTree,
      K::Path
    ],
    "the source is a StringValue node; `use` and `as` are bare keyword tokens"
  );
  assert_eq!(
    node_text!(parse_use_definition, "use \"x\"::*", K::UseDefinition).as_deref(),
    Some("use \"x\"::*"),
    "the `use` keyword is inside the definition"
  );
  assert!(!parse_use_definition("use \"x\"::*").has_errors());
}

/// The source narrows to an **inline** string, and it narrows on the *token*, not on the node.
///
/// The syntactic tree's source is an `InlineStringValue`, so a block string there is a grammar
/// error — but the node here is a `StringValue` either way, which is the census's recorded ruling
/// for `InlineStringValue`. A port that reached for a distinct node kind would be inventing one the
/// space does not have; a port that forgot the narrowing accepts a document `syntactic/` rejects.
#[test]
fn a_use_source_must_be_an_inline_string() {
  let src = "use \"\"\"x\"\"\"::*";
  let parse = parse_use_definition(src);
  assert!(parse.has_errors(), "a block string is not a use source");
  assert_eq!(
    kinds!(parse_use_definition, src),
    vec![K::Root, K::UseDefinition, K::UseTree, K::StringValue],
    "and the node is still a StringValue: the narrowing is the token's"
  );
  assert_eq!(text!(parse_use_definition, src), src);
}

/// `as` is read by **spelling**, and a wrong one is not an alias.
///
/// `use` and `as` are ordinary identifier tokens to the lexer, so nothing about the token *kind*
/// tells `as` from any other name, and the witness has to be a *shape*: with `to` in its place the
/// tree ends at `a`, and the two tokens after it are not the tree's.
#[test]
fn the_alias_keyword_is_read_by_spelling() {
  assert!(
    !parse_use_tree("a as b").has_errors(),
    "the right spelling is clean"
  );
  assert_eq!(
    node_text!(parse_use_tree, "a to b", K::UseTree).as_deref(),
    Some("a "),
    "a tree is an undelimited shape, so the peek that learned `to` is not `::` crossed the space \
     before it while the path was still open — the law every optional tail inherits"
  );
}

/// Every shape the syntactic layer refuses and this layer still builds reports, keeps every
/// byte, and keeps the tree it would have built: an empty group, a root or a string source inside
/// a group, a block-string source, and an alias on a glob or on a group.
#[test]
fn a_refused_use_shape_reports_and_still_builds_its_tree() {
  for (src, expected) in [
    ("a::{}", vec![K::Root, K::UseTree, K::Path]),
    (
      "a::{~::b}",
      vec![K::Root, K::UseTree, K::Path, K::UseTree, K::Path],
    ),
    ("a::{..::*}", vec![K::Root, K::UseTree, K::Path, K::UseTree]),
    (
      "a::{\"x\"::b}",
      vec![
        K::Root,
        K::UseTree,
        K::Path,
        K::UseTree,
        K::StringValue,
        K::Path,
      ],
    ),
    (
      "\"\"\"x\"\"\"::B",
      vec![K::Root, K::UseTree, K::StringValue, K::Path],
    ),
    ("a::* as b", vec![K::Root, K::UseTree, K::Path]),
    (
      "a::{b} as c",
      vec![K::Root, K::UseTree, K::Path, K::UseTree, K::Path],
    ),
  ] {
    let parse = parse_use_tree(src);
    assert!(parse.has_errors(), "the syntactic layer refuses {src}");
    assert_eq!(
      parse.syntax().text().to_string(),
      src,
      "and every byte is kept"
    );
    assert_eq!(kinds!(parse_use_tree, src), expected, "{src}");
  }
}

/// A bare `*` is not a tree, so inside a group it is junk the group skips, and the group still
/// closes on its own `}`.
#[test]
fn a_bare_glob_inside_a_group_is_skipped_and_the_group_still_closes() {
  let src = "a::{*}";
  assert!(parse_use_tree(src).has_errors(), "a bare `*` is not a tree");
  assert_eq!(text!(parse_use_tree, src), src);
  assert_eq!(
    kinds!(parse_use_tree, src),
    vec![K::Root, K::UseTree, K::Path, K::Error]
  );
}

/// What a tree may not end on — `as` with no name, a `::` with nothing after it, an open group,
/// and `use` with no tree — reports, and the tree still closes its node.
#[test]
fn an_unfinished_use_reports_and_keeps_its_nodes() {
  for (src, expected) in [
    ("a as", vec![K::Root, K::UseTree, K::Path]),
    ("a::", vec![K::Root, K::UseTree, K::Path]),
    (
      "a::{b",
      vec![K::Root, K::UseTree, K::Path, K::UseTree, K::Path],
    ),
  ] {
    let parse = parse_use_tree(src);
    assert!(parse.has_errors(), "{src}");
    assert_eq!(parse.syntax().text().to_string(), src);
    assert_eq!(kinds!(parse_use_tree, src), expected, "{src}");
  }
  let parse = parse_use_definition("use");
  assert!(parse.has_errors(), "`use` with no tree");
  assert_eq!(
    kinds!(parse_use_definition, "use"),
    vec![K::Root, K::UseDefinition]
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 14 — the extension half of the generic family.
// ---------------------------------------------------------------------------------------------

/// An extension's generic list holds **bare names with no defaults**, and it is its own kind.
///
/// Three lists, three kinds, and only one of them has a member node — `extension_type_param` is a
/// single identifier token and the census gives it none. The three are the same bytes for `<T>`,
/// so a list wired to the wrong kind re-prints identically and only the pre-order sees it.
#[test]
fn an_extension_generic_list_is_its_own_kind_and_holds_bare_names() {
  assert_eq!(
    kinds!(parse_extension_type_generics, "<T, U>"),
    vec![K::Root, K::ExtensionTypeGenerics],
    "no member node: an extension type param is one token"
  );
  assert!(
    parse_extension_type_generics("<T = Int>").has_errors(),
    "an extension applies generic ARGUMENTS and declares no defaults"
  );
  assert!(
    parse_extension_type_generics("<>").has_errors(),
    "`extension_type_generics` is `at_least(1)`"
  );
  assert_eq!(
    kinds!(parse_extension_type_generics, "<>"),
    vec![K::Root, K::ExtensionTypeGenerics],
    "and the empty report consumes nothing"
  );
}

/// An extension's target is a **`Path`**, where a definition's name is a bare `Name` token.
///
/// `extension_name` calls `path` and `definition_name` calls `take_name`
/// (`generic/mod.rs:363` against `:327`), so `extend type ns::T<Int>` names a qualified target and
/// `type ns::T` does not. Both spell one identifier for the simple case, which is why the
/// one-segment spelling is asserted here and not only the qualified one.
#[test]
fn an_extension_name_wraps_a_path_where_a_definition_name_wraps_a_token() {
  assert_eq!(
    kinds!(parse_extension_name, "T"),
    vec![K::Root, K::ExtensionName, K::Path],
    "even the one-segment spelling nests ExtensionName > Path"
  );
  assert_eq!(
    kinds!(parse_extension_name, "ns::T<A, B>"),
    vec![K::Root, K::ExtensionName, K::Path, K::ExtensionTypeGenerics]
  );
  assert_eq!(
    node_text!(parse_extension_name, "ns::T<A>", K::ExtensionName).as_deref(),
    Some("ns::T<A>")
  );
}

// ---------------------------------------------------------------------------------------------
// The `<>` balanced pair — this task's novel shape.
// ---------------------------------------------------------------------------------------------

/// An unterminated generic list reports `Unclosed::Angle` and still closes its node.
///
/// The `<>` pair is depth-counted by the lexer alongside the other three, so `recover`'s
/// classifier has four rows and `lossless/mod.rs`'s `unclosed` list four entries. Both halves are
/// load-bearing and neither is visible in the text: the classifier decides where a *skip* stops
/// (`tests/lossless_x_selection.rs`'s nested-pair test), and the `unclosed` entry decides whether
/// an unterminated list reports a typed `Unclosed::Angle` or the catch-all's untyped note
/// (`tests/lossless_x_errors.rs`).
#[test]
fn an_unterminated_generic_list_reports_and_keeps_every_byte() {
  type Driver = fn(&str) -> smear::parser::graphqlx::lossless::Parse;
  for (driver, src) in [
    (parse_extension_type_generics as Driver, "<A"),
    (parse_type_generics as Driver, "<A"),
  ] {
    let parse = driver(src);
    assert!(parse.has_errors(), "{src} must report an unclosed `<`");
    assert_eq!(parse.syntax().text().to_string(), src);
  }
  assert_eq!(
    kinds!(parse_extension_name, "T<A"),
    vec![K::Root, K::ExtensionName, K::Path, K::ExtensionTypeGenerics],
    "the list still closes its node, so the rest of the file keeps its structure"
  );
}
