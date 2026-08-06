//! The GraphQLx value and type-reference productions.
//!
//! **These tests drive the productions directly, not through `parse_document`.** `parse_document`
//! is still Task 9's drain-everything stub, so nothing under it reaches a value production;
//! written through it, every assertion here would compare two one-element `[Root]` trees and pass
//! vacuously. That is the weak-assertion failure mode this plan has been bitten by before, and the
//! `lossless_drivers!` block in each production file exists precisely so it cannot recur.
//!
//! Every test asserts the **node-kind pre-order** of a real parse rather than a verdict. A verdict
//! is blind to a lost node, and a lost node is the defect class the golden gate exists for; a
//! pre-order sees it here, one production at a time, before there is a document to hide it in.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    ty::test_support::{
      parse_path, parse_path_or_recover, parse_type_generics, parse_type_path, parse_type_ref,
    },
    value::test_support::{parse_const_value, parse_default_value, parse_map_entry, parse_value},
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

// ---------------------------------------------------------------------------------------------
// The shapes GraphQLx shares with GraphQL. Short, because the interesting half is below.
// ---------------------------------------------------------------------------------------------

#[test]
fn the_scalar_values_each_open_one_node() {
  assert_eq!(kinds!(parse_value, "1"), vec![K::Root, K::IntValue]);
  assert_eq!(kinds!(parse_value, "1.5"), vec![K::Root, K::FloatValue]);
  assert_eq!(kinds!(parse_value, "\"s\""), vec![K::Root, K::StringValue]);
  assert_eq!(
    kinds!(parse_value, "\"\"\"b\"\"\""),
    vec![K::Root, K::StringValue],
    "a block string is the same node kind as an inline one; the token says which"
  );
  assert_eq!(kinds!(parse_value, "true"), vec![K::Root, K::BooleanValue]);
  assert_eq!(kinds!(parse_value, "false"), vec![K::Root, K::BooleanValue]);
  assert_eq!(kinds!(parse_value, "null"), vec![K::Root, K::NullValue]);
  assert_eq!(
    kinds!(parse_value, "$x"),
    vec![K::Root, K::VariableValue],
    "the node kind is VariableValue, GraphQLx's own AST name, not GraphQL's Variable"
  );
}

#[test]
fn a_list_value_nests_its_elements() {
  assert_eq!(
    kinds!(parse_value, "[1, 2]"),
    vec![K::Root, K::ListValue, K::IntValue, K::IntValue]
  );
}

#[test]
fn an_object_value_wraps_each_field_and_the_key_stays_a_bare_name() {
  assert_eq!(
    kinds!(parse_value, "{x: 1}"),
    vec![K::Root, K::ObjectValue, K::ObjectField, K::IntValue],
    "an ObjectField's key is a Name token, not a Path node — only the value side widened"
  );
}

#[test]
fn a_default_value_wraps_the_value_after_its_equals() {
  assert_eq!(
    kinds!(parse_default_value, "= 1"),
    vec![K::Root, K::DefaultValue, K::IntValue]
  );
}

// ---------------------------------------------------------------------------------------------
// Divergences 1, 8, 9 — the enum value is a path.
// ---------------------------------------------------------------------------------------------

/// An enum value wraps a whole `Path`, and the path is one node however many segments it has.
///
/// **This is the divergence that appears in every GraphQLx tree.** GraphQL's `EnumValue` holds one
/// `Name` token; this one holds a `Path` node. A port that kept GraphQL's shape would produce
/// `EnumValue` with the name tokens directly inside it, which round-trips identically, passes every
/// verdict gate, and makes the typed layer unable to ask an enum value for its segments.
#[test]
fn an_enum_value_wraps_a_path_node() {
  assert_eq!(
    kinds!(parse_value, "Colour"),
    vec![K::Root, K::EnumValue, K::Path]
  );
  assert_eq!(
    kinds!(parse_value, "ns::Colour"),
    vec![K::Root, K::EnumValue, K::Path],
    "a two-segment path is still one Path node"
  );
  assert_eq!(text!(parse_value, "ns::Colour"), "ns::Colour");
}

/// Divergence 8: a bare `::` in value position opens a fully qualified enum value.
///
/// GraphQL has no value head that is not a name, a literal or a bracket, so this arm has no
/// counterpart to port and is the one a port would simply be missing — after which `::ns::Colour`
/// would reach `recover::unexpected` and become an `Error` node that still round-trips.
#[test]
fn a_leading_path_separator_opens_a_fully_qualified_enum_value() {
  assert_eq!(
    kinds!(parse_value, "::ns::Colour"),
    vec![K::Root, K::EnumValue, K::Path],
    "a leading `::` is part of the path, not junk in front of it"
  );
  assert_eq!(text!(parse_value, "::ns::Colour"), "::ns::Colour");

  // The qualifier is a token child of the Path, which is how a consumer reads `fully_qualified`.
  let parse = parse_value("::ns::Colour");
  let path = parse
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::Path)
    .expect("the parse produced no Path node");
  assert_eq!(
    path
      .children_with_tokens()
      .filter_map(|e| e.into_token())
      .map(|t| t.kind())
      .collect::<Vec<_>>(),
    vec![K::PathSeparator, K::Name, K::PathSeparator, K::Name],
    "the leading `::` must be inside the Path, or nothing records that it was fully qualified"
  );
}

/// The three reserved spellings are reserved in a path's **first** segment only.
///
/// `null` is a null value; `x::null` is an enum value whose second segment happens to be spelled
/// `null`. The syntactic layer gets this by only ever consulting the keyword on the token the fused
/// dispatch consumed, and `path_tail` never looks at a spelling at all.
#[test]
fn the_reserved_spellings_are_reserved_only_in_the_first_segment() {
  assert_eq!(kinds!(parse_value, "null"), vec![K::Root, K::NullValue]);
  assert_eq!(
    kinds!(parse_value, "x::null"),
    vec![K::Root, K::EnumValue, K::Path],
    "`null` after a separator is an ordinary segment"
  );
  assert_eq!(
    kinds!(parse_value, "x::true"),
    vec![K::Root, K::EnumValue, K::Path]
  );
}

// ---------------------------------------------------------------------------------------------
// Divergences 6 and 7 — set and map, and the contextual dispatch.
// ---------------------------------------------------------------------------------------------

/// `set { … }` and `map { … }` are collection constructors, and the keyword is inside the node.
#[test]
fn set_and_map_open_their_own_nodes_over_the_keyword() {
  assert_eq!(
    kinds!(parse_value, "set { 1, 2 }"),
    vec![K::Root, K::SetValue, K::IntValue, K::IntValue]
  );
  assert_eq!(
    kinds!(parse_value, "map { 1 => 2 }"),
    vec![K::Root, K::MapValue, K::MapEntry, K::IntValue, K::IntValue]
  );

  // The keyword is a child of the node, not a sibling of it. Retro-wrapping from a mark minted
  // before the keyword is the only thing that puts it there, and a node opened at the `{` would
  // round-trip identically while losing the one token that says which collection it is.
  let parse = parse_value("set { 1 }");
  let set = parse
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::SetValue)
    .expect("the parse produced no SetValue node");
  assert_eq!(
    set.text().to_string(),
    "set { 1 }",
    "the SetValue node must span its keyword"
  );
}

/// Divergence 7, and the novel shape this task's mutation targets.
///
/// `set` and `map` are collection constructors **only** when the immediately following token is a
/// `{`; everywhere else they are ordinary path segments. `{ a: set }` is an object whose field
/// holds an enum value spelled `set`, and treating the keyword as a constructor regardless would
/// turn that into a malformed set.
#[test]
fn set_and_map_are_ordinary_enum_values_when_no_brace_follows() {
  assert_eq!(
    kinds!(parse_value, "set"),
    vec![K::Root, K::EnumValue, K::Path],
    "a bare `set` is an enum value"
  );
  assert_eq!(
    kinds!(parse_value, "map"),
    vec![K::Root, K::EnumValue, K::Path]
  );
  assert_eq!(
    kinds!(parse_value, "{ a: set }"),
    vec![
      K::Root,
      K::ObjectValue,
      K::ObjectField,
      K::EnumValue,
      K::Path
    ],
    "`set` as a field's value is an enum value, not a collection constructor"
  );
  assert_eq!(
    kinds!(parse_value, "set::Thing"),
    vec![K::Root, K::EnumValue, K::Path],
    "a `set` that continues into a path is one path of two segments"
  );
  assert_eq!(text!(parse_value, "set::Thing"), "set::Thing");
}

/// The lookahead is over the next **significant** token, not the next raw one.
///
/// The reason `collection_or_enum` retro-wraps instead of peeking two tokens: over a
/// trivia-surfacing stream the token after `set` is usually a space, so a fixed two-token peek
/// answers `Space` and every padded set in the corpus becomes an enum value followed by a stray
/// object. Both forms below must produce the same tree.
#[test]
fn the_set_lookahead_crosses_trivia_before_it_answers() {
  let want = vec![K::Root, K::SetValue, K::IntValue];
  assert_eq!(kinds!(parse_value, "set{1}"), want, "the compact form");
  assert_eq!(
    kinds!(parse_value, "set # c\n\t{ 1 }"),
    want,
    "a comment and a tab between the keyword and the brace"
  );
  assert_eq!(text!(parse_value, "set # c\n\t{ 1 }"), "set # c\n\t{ 1 }");
}

/// A map entry is `Value => Value`, and both halves are full values.
#[test]
fn a_map_entry_takes_a_full_value_on_each_side() {
  assert_eq!(
    kinds!(parse_map_entry, "[1] => { a: 2 }"),
    vec![
      K::Root,
      K::MapEntry,
      K::ListValue,
      K::IntValue,
      K::ObjectValue,
      K::ObjectField,
      K::IntValue
    ],
    "a map key is not restricted to a name"
  );
}

// ---------------------------------------------------------------------------------------------
// Constness.
// ---------------------------------------------------------------------------------------------

/// A variable in a const position is reported **and still built**.
#[test]
fn a_variable_in_a_const_position_is_reported_and_still_built() {
  let parse = parse_const_value("$x");
  assert!(parse.has_errors(), "a `$` in a const position must report");
  assert_eq!(
    parse
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<_>>(),
    vec![K::Root, K::VariableValue],
    "the node is built anyway, so a diagnostic has something to point at"
  );
  assert_eq!(parse.syntax().text().to_string(), "$x");

  // And the same source in a non-const position is clean, so the report is the constness and not
  // something else about `$x`.
  assert!(!parse_value("$x").has_errors());
}

/// Constness rides all the way down into a set, a map and a nested list.
#[test]
fn constness_reaches_every_nested_value_position() {
  for src in [
    "[$x]",
    "{a: $x}",
    "set { $x }",
    "map { $x => 1 }",
    "map { 1 => $x }",
  ] {
    assert!(
      parse_const_value(src).has_errors(),
      "{src}: the const flavour must reach the nested variable"
    );
    assert!(
      !parse_value(src).has_errors(),
      "{src}: and the non-const flavour must not report it"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// Divergences 2, 3, 4, 5 — paths, generics and type references.
// ---------------------------------------------------------------------------------------------

/// A type reference's four heads, each reaching its own node kind.
#[test]
fn a_type_reference_has_four_heads() {
  assert_eq!(
    kinds!(parse_type_ref, "Int"),
    vec![K::Root, K::DefinitionTypePath, K::Path]
  );
  assert_eq!(
    kinds!(parse_type_ref, "::ns::Int"),
    vec![K::Root, K::DefinitionTypePath, K::Path]
  );
  assert_eq!(
    kinds!(parse_type_ref, "[Int]"),
    vec![K::Root, K::ListType, K::DefinitionTypePath, K::Path]
  );
  assert_eq!(
    kinds!(parse_type_ref, "<Int>"),
    vec![K::Root, K::SetType, K::DefinitionTypePath, K::Path]
  );
}

/// Divergence 5: `<…>` is a set until a `=>` says otherwise.
///
/// The decision is made *after* the first inner type is parsed, so the node kind is unknown when
/// the `<` is committed and the wrap has to be retroactive. A production that guessed `SetType` up
/// front and corrected later cannot: the event is already in the stream.
#[test]
fn the_angle_form_is_a_set_until_a_fat_arrow_makes_it_a_map() {
  assert_eq!(
    kinds!(parse_type_ref, "<Int>"),
    vec![K::Root, K::SetType, K::DefinitionTypePath, K::Path]
  );
  assert_eq!(
    kinds!(parse_type_ref, "<K => V>"),
    vec![
      K::Root,
      K::MapType,
      K::DefinitionTypePath,
      K::Path,
      K::DefinitionTypePath,
      K::Path
    ]
  );

  // The retro-wrap covers the opener: a node that began after the `<` would round-trip identically
  // and would not contain the token that makes it a set.
  let parse = parse_type_ref("<K => V>");
  let map = parse
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::MapType)
    .expect("the parse produced no MapType node");
  assert_eq!(map.text().to_string(), "<K => V>");
}

/// Divergence 4: `!` is a token child of the node it follows, and there is no wrapper.
///
/// GraphQLx's kind space has no non-null node — Task 8's census would have refused one, because
/// nothing in `graphqlx/{ast,syntactic}` names it — so the `!` has to land *inside* the type it
/// modifies. A production that ate it outside its own node would leave it as a sibling: the text
/// round-trips either way, and every consumer asking "is this type non-null?" gets the wrong
/// answer.
#[test]
fn the_non_null_marker_is_a_child_of_the_type_it_modifies() {
  for (src, kind) in [
    ("Int!", K::DefinitionTypePath),
    ("[Int]!", K::ListType),
    ("<Int>!", K::SetType),
    ("<K => V>!", K::MapType),
  ] {
    let parse = parse_type_ref(src);
    let node = parse
      .syntax()
      .descendants()
      .find(|n| n.kind() == kind)
      .unwrap_or_else(|| panic!("{src}: the parse produced no {kind:?} node"));
    assert_eq!(
      node.text().to_string(),
      src,
      "{src}: the {kind:?} node must span its own `!`"
    );
    assert!(
      node
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .any(|t| t.kind() == K::Bang),
      "{src}: the `!` must be a direct token child of the {kind:?}"
    );
  }

  // And no kind in the space claims to be a non-null wrapper, so this shape is the only one
  // available rather than one of two.
  assert!(
    !K::ALL.iter().any(|k| format!("{k:?}").contains("NonNull")),
    "the space has gained a non-null kind; the `!` folding above is no longer the only shape"
  );
}

/// Divergence 2: a type path takes generic arguments, and they nest.
#[test]
fn a_path_takes_generic_arguments() {
  assert_eq!(
    kinds!(parse_type_ref, "Map<K, V>"),
    vec![
      K::Root,
      K::DefinitionTypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path,
      K::DefinitionTypePath,
      K::Path
    ],
    "the comma is trivia, so the two arguments are just two type references"
  );
  assert_eq!(
    kinds!(parse_type_ref, "Outer<Inner<T>>"),
    vec![
      K::Root,
      K::DefinitionTypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ],
    "`>>` is two tokens to this lexer, so a nested list closes without a special case"
  );
}

/// A `TypePath` is a path with generics and **no** `!`.
///
/// The difference from `DefinitionTypePath` is exactly the `!`, and it is the difference between a
/// type position and every other position a path appears in. A `TypePath` that ate a following `!`
/// would silently accept `implements Foo!`.
#[test]
fn a_type_path_carries_generics_but_never_a_bang() {
  assert_eq!(
    kinds!(parse_type_path, "ns::Foo<T>"),
    vec![
      K::Root,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ]
  );

  let parse = parse_type_path("Foo!");
  let type_path = parse
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::TypePath)
    .expect("the parse produced no TypePath node");
  assert_eq!(
    type_path.text().to_string(),
    "Foo",
    "a TypePath must stop before a `!`; the caller decides what that `!` means"
  );
}

/// A bare path is one node, and its leading trivia stays outside it.
///
/// The `NamedType` lesson from Phase A, restated for the production that inherited the position:
/// `node(…)` opens before its inner parser runs, so a production that did not cross its leading
/// trivia first would produce a `Path` spanning `" Q"`. Six goldens had to be re-blessed the last
/// time this was got wrong.
#[test]
fn a_path_node_starts_at_its_own_first_token() {
  let parse = parse_path("  \n Colour");
  let path = parse
    .syntax()
    .descendants()
    .find(|n| n.kind() == K::Path)
    .expect("the parse produced no Path node");
  assert_eq!(
    path.text().to_string(),
    "Colour",
    "the leading trivia belongs beside the Path, not inside it"
  );
  assert_eq!(parse.syntax().text().to_string(), "  \n Colour");
}

/// Trivia changes no shape anywhere in this task's grammar.
#[test]
fn trivia_does_not_change_any_shape_in_this_area() {
  for (compact, padded) in [
    ("[1,2]", "  [ 1 , # c\n 2 ] "),
    ("{a:1}", "{ a : 1 , }"),
    ("set{1}", " set  { 1 } "),
    ("map{1=>2}", "map { 1 => 2 }"),
    ("::a::b", " :: a :: b "),
  ] {
    assert_eq!(
      kinds!(parse_value, compact),
      kinds!(parse_value, padded),
      "{compact:?} and {padded:?} must have the same shape"
    );
    // The pairing is only evidence if the shape is non-trivial; an empty tree equals an empty tree.
    assert!(kinds!(parse_value, compact).len() >= 2, "{compact:?}");
    assert_eq!(text!(parse_value, padded), padded);
  }

  for (compact, padded) in [("[Int]!", " [ Int ] ! "), ("<K=>V>", "< K => V >")] {
    assert_eq!(
      kinds!(parse_type_ref, compact),
      kinds!(parse_type_ref, padded),
      "{compact:?} and {padded:?} must have the same shape"
    );
    assert_eq!(text!(parse_type_ref, padded), padded);
  }
}

// ---------------------------------------------------------------------------------------------
// Recovery.
// ---------------------------------------------------------------------------------------------

/// Junk inside a list costs one `Error` node, not the rest of the parse.
#[test]
fn junk_inside_a_list_is_attributed_to_an_error_node() {
  assert_eq!(
    kinds!(parse_value, "[1, ), 2]"),
    vec![K::Root, K::ListValue, K::IntValue, K::Error, K::IntValue],
    "the stray closer becomes an Error node and the list still closes on its own `]`"
  );
  assert_eq!(text!(parse_value, "[1, ), 2]"), "[1, ), 2]");
}

/// A skipped region counts `<` and `>` depth exactly as it counts the other three pairs.
///
/// **This test exists because its absence was measured.** `recover.rs`'s `delimiters` classifier
/// carries a fourth pair GraphQL's does not, and deleting those two arms left every other test in
/// this file — including the unterminated-`<` test below — green: that one reaches
/// `unclosed_angle` at end of input, which is a different code path from `sync_balanced`'s depth
/// counting, and every other recovery case in this area happens to contain no angle bracket at
/// all. The gap was found by mutating and re-measuring, not by reading.
///
/// What the depth counting buys, measured both ways: with the pair, the junk `<2>` is **one**
/// `Error` node; without it, `<` is neutral, the `2` behind it is a depth-zero sync point, and the
/// same source becomes `Error("<")`, a real `IntValue`, and `Error(">")` — three nodes, one of them
/// a value the grammar never admitted. The nested form is sharper still: `<<2>>` becomes
/// `Error("<<")`, an `IntValue` and **two** `Error`s.
///
/// The byte-for-byte text is identical under both, which is why no round-trip gate can see this.
#[test]
fn a_skipped_region_counts_angle_depth_like_every_other_pair() {
  assert_eq!(
    kinds!(parse_value, "[1, <2>, 3]"),
    vec![K::Root, K::ListValue, K::IntValue, K::Error, K::IntValue],
    "the whole `<2>` is one skipped region, not three"
  );
  assert_eq!(
    kinds!(parse_value, "[1, <<2>>, 3]"),
    vec![K::Root, K::ListValue, K::IntValue, K::Error, K::IntValue],
    "nesting inside the skipped region is crossed, not restarted at"
  );
  assert_eq!(
    kinds!(parse_value, "{a: 1, <2>, b: 2}"),
    vec![
      K::Root,
      K::ObjectValue,
      K::ObjectField,
      K::IntValue,
      K::Error,
      K::ObjectField,
      K::IntValue
    ],
    "and the same inside an object, where the members either side must survive"
  );

  // The `Error` node's own text, which is what pins *where* the skip stopped rather than merely how
  // many nodes it produced.
  let parse = parse_value("[1, <2>, 3]");
  let holes: Vec<String> = parse
    .syntax()
    .descendants()
    .filter(|n| n.kind() == K::Error)
    .map(|n| n.text().to_string())
    .collect();
  assert_eq!(holes, vec!["<2>, ".to_string()]);
  assert_eq!(text!(parse_value, "[1, <2>, 3]"), "[1, <2>, 3]");
}

/// An unterminated angle-delimited shape reports and still closes its node.
///
/// This is the *other* half of the `<>` pair's story, and the two are genuinely different code
/// paths: this one never reaches `sync_balanced` at all, because there is nothing left to skip.
#[test]
fn an_unterminated_generic_list_reports_and_keeps_its_bytes() {
  let parse = parse_type_ref("Foo<T");
  assert!(parse.has_errors(), "an unterminated `<` must report");
  assert_eq!(parse.syntax().text().to_string(), "Foo<T");
  assert_eq!(
    parse
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect::<Vec<_>>(),
    vec![
      K::Root,
      K::DefinitionTypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ],
    "the nodes still close, so the rest of a document would keep its structure"
  );
}

/// A head that starts no path is recovered rather than aborting the caller.
#[test]
fn a_non_path_head_recovers_instead_of_erroring_out() {
  assert_eq!(
    kinds!(parse_path_or_recover, ")"),
    vec![K::Root, K::Error],
    "the recovering path door attributes the junk and returns Ok"
  );
  assert!(parse_path_or_recover(")").has_errors());
}

/// An empty generic list is an error, not an empty list.
///
/// `graphqlx/syntactic/ty.rs`'s `.at_least(1)` says a `<>` has no valid reading, and the lossless
/// production has to agree or it would accept a shape the syntactic suite rejects — which is
/// precisely the disagreement gate 1 exists to catch, one layer earlier.
#[test]
fn an_empty_generic_list_reports() {
  let parse = parse_type_generics("<>");
  assert!(parse.has_errors(), "`<>` must report");
  assert_eq!(parse.syntax().text().to_string(), "<>");
}

// ---------------------------------------------------------------------------------------------
// The two head tables and their predicates.
// ---------------------------------------------------------------------------------------------

/// Every head the tables name really does open its production, and the predicates agree with the
/// sets.
///
/// `recover.rs` carries `VALUE_HEADS`/`starts_value` and `TYPE_HEADS`/`starts_type` as a slice and
/// a `match` guard, because the diagnostic path needs a `&'static [Kind]` and the member loops need
/// a predicate. Two spellings of one fact drift; this is the door that catches it, and it does so
/// **behaviourally** — a kind is in the set iff a source beginning with it produces something other
/// than a bare `Error`.
#[test]
fn every_named_value_head_actually_opens_a_value() {
  for (src, first) in [
    ("$x", K::VariableValue),
    ("1", K::IntValue),
    ("1.5", K::FloatValue),
    ("\"s\"", K::StringValue),
    ("\"\"\"b\"\"\"", K::StringValue),
    ("[]", K::ListValue),
    ("{}", K::ObjectValue),
    ("a", K::EnumValue),
    ("::a", K::EnumValue),
  ] {
    let kinds = kinds!(parse_value, src);
    assert_eq!(
      kinds.get(1),
      Some(&first),
      "{src:?} is a named value head and did not open a {first:?}"
    );
  }

  // …and a kind the set does *not* name reaches the recovery instead. Without this the loop above
  // would pass for a set that named every kind in the lexer.
  for src in [")", "!", "=>", "*", "|"] {
    assert_eq!(
      kinds!(parse_value, src),
      vec![K::Root, K::Error],
      "{src:?} is not a value head and must recover"
    );
  }
}

/// The type table, the same way.
#[test]
fn every_named_type_head_actually_opens_a_type() {
  for (src, first) in [
    ("Int", K::DefinitionTypePath),
    ("::Int", K::DefinitionTypePath),
    ("[Int]", K::ListType),
    ("<Int>", K::SetType),
  ] {
    assert_eq!(
      kinds!(parse_type_ref, src).get(1),
      Some(&first),
      "{src:?} is a named type head and did not open a {first:?}"
    );
  }

  for src in [")", "!", "$x", "1"] {
    assert_eq!(
      kinds!(parse_type_ref, src),
      vec![K::Root, K::Error],
      "{src:?} is not a type head and must recover"
    );
  }
}
