//! The GraphQLx import productions and the extension half of the generic family.
//!
//! **Nothing here can be ported.** GraphQL has no imports, no generic parameters and no extension
//! name, so every production these tests cover is new code against a grammar the other dialect
//! does not have — and the "fork the GraphQL production and edit it" habit the previous three
//! tasks build is exactly the wrong instinct.
//!
//! As elsewhere in this suite the assertions are node-kind pre-orders and node extents rather than
//! verdicts, because the defects in this area change nesting while leaving the text identical: a
//! `{ … }` import list is the same bytes whether its members are nodes or loose tokens, and an
//! extension's generic list is the same bytes whether it carries the extension kind or the
//! executable one.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear::parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    generic::test_support::{parse_extension_name, parse_extension_type_generics},
    import::test_support::{
      parse_import_clause, parse_import_definition, parse_import_list, parse_import_member,
    },
    ty::test_support::parse_type_generics,
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

// ---------------------------------------------------------------------------------------------
// Divergence 21 — the import family.
// ---------------------------------------------------------------------------------------------

#[test]
fn an_import_member_is_a_named_or_a_wildcard_specifier() {
  assert_eq!(
    kinds!(parse_import_member, "A"),
    vec![K::Root, K::NamedSpecifier]
  );
  assert_eq!(
    kinds!(parse_import_member, "A as ns::B"),
    vec![K::Root, K::NamedSpecifier, K::Path],
    "the alias is a whole Path, not a name — `optional_alias` calls `path`"
  );
  assert_eq!(
    kinds!(parse_import_member, "*"),
    vec![K::Root, K::WildcardSpecifier]
  );
  assert_eq!(
    kinds!(parse_import_member, "* as ns"),
    vec![K::Root, K::WildcardSpecifier, K::Path]
  );
  assert_eq!(
    node_text!(parse_import_member, "A as ns::B", K::NamedSpecifier).as_deref(),
    Some("A as ns::B"),
    "the `as` keyword is inside the specifier, not beside it"
  );
}

/// An `ImportMember` is a **choice**, so the winner is the direct child of the list.
///
/// The census gives `ImportMember` no kind — "a choice of NamedSpecifier | WildcardSpecifier" —
/// so a wrapper here would nest exactly one child and re-print identically.
#[test]
fn an_import_list_holds_its_members_directly() {
  assert_eq!(
    kinds!(parse_import_list, "{ A, B as C, * }"),
    vec![
      K::Root,
      K::ImportList,
      K::NamedSpecifier,
      K::NamedSpecifier,
      K::Path,
      K::WildcardSpecifier
    ],
    "the comma is trivia, so the members are told apart by shape and not by it"
  );
  assert!(
    parse_import_list("{}").has_errors(),
    "`import_list` is `at_least(1)`"
  );
  assert_eq!(
    kinds!(parse_import_list, "{}"),
    vec![K::Root, K::ImportList],
    "and the report consumes nothing, so the `}}` stays the loop's"
  );
}

/// An `ImportClause` is the other choice with no kind: a bare wildcard, or a list.
#[test]
fn an_import_clause_is_a_wildcard_or_a_list_and_opens_no_node_of_its_own() {
  assert_eq!(
    kinds!(parse_import_clause, "*"),
    vec![K::Root, K::WildcardSpecifier]
  );
  assert_eq!(
    kinds!(parse_import_clause, "{ A }"),
    vec![K::Root, K::ImportList, K::NamedSpecifier]
  );
  let parse = parse_import_clause("A");
  assert!(
    parse.has_errors(),
    "a bare name is neither a wildcard nor a list"
  );
}

#[test]
fn an_import_definition_holds_its_clause_and_its_source() {
  assert_eq!(
    kinds!(
      parse_import_definition,
      "import { A, * as ns } from \"./x.graphql\""
    ),
    vec![
      K::Root,
      K::ImportDefinition,
      K::ImportList,
      K::NamedSpecifier,
      K::WildcardSpecifier,
      K::Path,
      K::StringValue
    ],
    "the source is a StringValue node; `import`, `from` and `as` are bare keyword tokens"
  );
  assert_eq!(
    node_text!(
      parse_import_definition,
      "import * from \"x\"",
      K::ImportDefinition
    )
    .as_deref(),
    Some("import * from \"x\""),
    "the `import` keyword is inside the definition"
  );
  assert!(!parse_import_definition("import * from \"x\"").has_errors());
}

/// The source narrows to an **inline** string, and it narrows on the *token*, not on the node.
///
/// `import_definition_after_keyword` calls `inline_string_value`, so a block string there is a
/// grammar error — but the node above it is a `StringValue` either way, which is the census's
/// recorded ruling for `InlineStringValue`. A port that reached for a distinct node kind would be
/// inventing one the space does not have; a port that forgot the narrowing accepts a document
/// `syntactic/` rejects.
#[test]
fn an_import_source_must_be_an_inline_string() {
  let parse = parse_import_definition("import * from \"\"\"x\"\"\"");
  assert!(parse.has_errors(), "a block string is not an import source");
  assert_eq!(
    kinds!(parse_import_definition, "import * from \"\"\"x\"\"\""),
    vec![
      K::Root,
      K::ImportDefinition,
      K::WildcardSpecifier,
      K::StringValue
    ],
    "and the node is still a StringValue: the narrowing is the token's"
  );
  assert_eq!(
    text!(parse_import_definition, "import * from \"\"\"x\"\"\""),
    "import * from \"\"\"x\"\"\""
  );
}

/// The three keywords are read by **spelling**, and a wrong one is a grammar error.
///
/// `import`, `from` and `as` are ordinary identifier tokens to the lexer, so nothing about the
/// token *kind* distinguishes `from` from any other name. Measured: a `from` read as
/// `expect(Identifier)` rather than through the keyword projection is **green** against every
/// other test here — `import * "x"` still reports (there is no identifier at all), so only a
/// wrongly-*spelled* keyword can catch it, and `import * to "x"` is a document `syntactic/`
/// rejects and this suite would have accepted.
#[test]
fn the_import_keywords_are_read_by_spelling() {
  assert!(
    parse_import_definition("import * to \"x\"").has_errors(),
    "`to` is not `from`"
  );
  assert!(
    !parse_import_definition("import * as ns from \"x\"").has_errors(),
    "and the right spelling is clean"
  );
  // `as` is read the same way, and its witness has to be a *shape* rather than a verdict: a
  // braced list is comma-tolerant, so `{ A to B }` is three members in both suites and reports
  // nothing. What separates the two readings is whether the specifier swallowed a Path.
  assert_eq!(
    kinds!(parse_import_member, "A to B"),
    vec![K::Root, K::NamedSpecifier],
    "`to` is not `as`, so this specifier has no alias"
  );
  assert_eq!(
    node_text!(parse_import_member, "A to B", K::NamedSpecifier).as_deref(),
    Some("A "),
    "a specifier is an undelimited shape, so the peek that learned `to` is not `as` crossed the \
     space before it while the node was still open — the law every optional tail inherits"
  );
}

#[test]
fn a_missing_from_is_reported_and_the_rest_of_the_import_survives() {
  let parse = parse_import_definition("import * \"x\"");
  assert!(parse.has_errors());
  assert_eq!(
    text!(parse_import_definition, "import * \"x\""),
    "import * \"x\""
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
