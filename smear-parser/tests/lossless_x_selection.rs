//! The GraphQLx argument, directive, selection and executable-definition productions.
//!
//! **These tests drive the productions directly, not through `parse_document`.** `parse_document`
//! is still Task 9's drain-everything stub, so nothing under it reaches any production here;
//! written through it, every assertion would compare two one-element `[Root]` trees and pass
//! vacuously.
//! `tests/lossless_x_value.rs` records the same reasoning for the value family.
//!
//! Every test asserts the **node-kind pre-order** of a real parse rather than a verdict. A verdict
//! is blind to a lost node and to a *misplaced* one, and the misplaced node is the harder defect:
//! a tree whose nesting is wrong still re-prints its source byte for byte, so no round-trip gate
//! can see it. Several tests below exist only to pin nesting that text fidelity cannot.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use smear_parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{
    directive::test_support::{
      parse_argument, parse_arguments, parse_const_directives, parse_directive, parse_directives,
    },
    executable::test_support::{
      parse_fragment_definition, parse_operation_definition, parse_variables_definition,
    },
    generic::test_support::{
      parse_definition_name, parse_definition_type_generics, parse_executable_definition_name,
      parse_executable_definition_type_generics, parse_where_clause, parse_where_predicate,
    },
    parse_executable_document,
    selection::test_support::{
      parse_field, parse_selection, parse_selection_set, parse_type_condition,
    },
    ty::test_support::parse_type_path,
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

/// The text of the first descendant of `kind`, so a test can pin *which* bytes a node covers
/// rather than only that the node exists.
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
// Arguments — the shape GraphQLx shares with GraphQL, with GraphQLx values inside.
// ---------------------------------------------------------------------------------------------

#[test]
fn an_argument_wraps_its_name_colon_and_value() {
  assert_eq!(
    kinds!(parse_argument, "x: 1"),
    vec![K::Root, K::Argument, K::IntValue]
  );
  assert_eq!(
    kinds!(parse_argument, "x: set { 1 }"),
    vec![K::Root, K::Argument, K::SetValue, K::IntValue],
    "an argument's value is a GraphQLx value, so a set constructor reaches it"
  );
}

#[test]
fn an_argument_list_is_a_node_even_when_it_is_empty() {
  assert_eq!(kinds!(parse_arguments, "()"), vec![K::Root, K::Arguments]);
  assert!(
    !parse_arguments("()").has_errors(),
    "`graphqlx/syntactic/argument/mod.rs`'s collection has no `at_least(1)`, so the lenient \
     empty spelling is accepted and the two suites agree"
  );
  assert_eq!(
    kinds!(parse_arguments, "(a: 1, b: $v)"),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Argument,
      K::VariableValue
    ]
  );
}

#[test]
fn junk_inside_an_argument_list_costs_one_error_node_and_not_the_list() {
  let parse = parse_arguments("(a: 1, !, b: 2)");
  assert!(parse.has_errors());
  assert_eq!(
    kinds!(parse_arguments, "(a: 1, !, b: 2)"),
    vec![
      K::Root,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Error,
      K::Argument,
      K::IntValue
    ]
  );
  assert_eq!(text!(parse_arguments, "(a: 1, !, b: 2)"), "(a: 1, !, b: 2)");
}

#[test]
fn an_unterminated_argument_list_reports_and_still_closes_its_node() {
  let parse = parse_arguments("(a: 1");
  assert!(
    parse.has_errors(),
    "the `()` pair must reach unclosed_parens"
  );
  assert_eq!(text!(parse_arguments, "(a: 1"), "(a: 1");
}

// ---------------------------------------------------------------------------------------------
// Divergence 10 — a directive's name is a TypePath.
// ---------------------------------------------------------------------------------------------

/// A directive's name is a whole `TypePath`, generics included.
///
/// **This is the divergence a port would simply not have.** GraphQL's `directive` is
/// `@ Name Arguments?`; writing that here leaves `@ns::deprecated<T>` reporting after the `ns`,
/// and — worse for a gate to catch — `@deprecated` would build `Directive` with a bare `Name`
/// token where every consumer expects a `TypePath > Path`. That tree round-trips identically.
#[test]
fn a_directive_name_is_a_type_path() {
  assert_eq!(
    kinds!(parse_directive, "@deprecated"),
    vec![K::Root, K::Directive, K::TypePath, K::Path],
    "even the one-segment spelling nests TypePath > Path"
  );
  assert_eq!(
    kinds!(parse_directive, "@ns::deprecated"),
    vec![K::Root, K::Directive, K::TypePath, K::Path]
  );
  assert_eq!(
    kinds!(parse_directive, "@ns::cache<Int>"),
    vec![
      K::Root,
      K::Directive,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ],
    "a directive name may carry generic arguments"
  );
  assert_eq!(
    kinds!(parse_directive, "@::ns::d"),
    vec![K::Root, K::Directive, K::TypePath, K::Path],
    "a fully qualified directive name keeps its leading `::` inside the Path"
  );
}

#[test]
fn a_directive_holds_its_arguments_inside_its_own_node() {
  assert_eq!(
    kinds!(parse_directive, "@d(if: true)"),
    vec![
      K::Root,
      K::Directive,
      K::TypePath,
      K::Path,
      K::Arguments,
      K::Argument,
      K::BooleanValue
    ]
  );
  assert_eq!(
    node_text!(parse_directive, "@d(if: true)", K::Directive).as_deref(),
    Some("@d(if: true)")
  );
}

#[test]
fn no_directive_means_no_directives_node() {
  assert_eq!(
    kinds!(parse_directives, "  "),
    vec![K::Root],
    "an absent run opens nothing, so a typed accessor cannot answer Some(<empty>)"
  );
  assert_eq!(
    kinds!(parse_directives, "@a @b"),
    vec![
      K::Root,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
}

#[test]
fn a_const_directive_run_reports_a_variable_in_an_argument_and_still_builds_it() {
  let parse = parse_const_directives("@d(x: $v)");
  assert!(
    parse.has_errors(),
    "a `$` in a Directives[Const] argument is a grammar error"
  );
  assert_eq!(
    kinds!(parse_const_directives, "@d(x: $v)"),
    vec![
      K::Root,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::Arguments,
      K::Argument,
      K::VariableValue
    ],
    "the rejection is a diagnostic; every byte is still kept and the node is still built"
  );
}

/// Recovery inside a directive name's generic list counts `<` and `>` as a pair.
///
/// **The `<>` row of `recover::delimiters` is the only thing that makes this shape right, and
/// dropping it leaves the text byte-identical.** Without the pair the skip stops at the `B` inside
/// the nested `<B>` — an `Identifier` is a depth-zero sync point and a `<` is neither that nor a
/// pair opener — so the *inner* `>` closes the outer list, `C` and the real `>` fall out of the
/// node entirely, and the tree is a different shape that re-prints the same source. Task 10
/// measured that mutation as GREEN because every recovery test in the area happened to contain no
/// angle bracket; this is the test that closes it.
///
/// The junk token has to be chosen with care, and two obvious candidates are not junk at all: a
/// `!` after a type is that type's non-null marker (`definition_type_path` eats it), and a `$` is
/// a depth-zero **sync point**, so the balanced scan stops on it having crossed nothing and the
/// classifier is never consulted. `@` is neither — it begins no type, ends no skip, and opens no
/// pair — so the scan genuinely runs and the nested `<B>` is the only thing that decides where it
/// stops.
#[test]
fn a_skip_inside_a_generic_list_crosses_a_nested_angle_pair_whole() {
  let src = "@d<A @ <B> C>";
  assert_eq!(
    kinds!(parse_directive, src),
    vec![
      K::Root,
      K::Directive,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path,
      K::Error,
      K::DefinitionTypePath,
      K::Path
    ],
    "the `@ <B>` run is one Error node and `C` is still an argument of the OUTER list"
  );
  assert!(
    node_text!(parse_directive, src, K::Error)
      .as_deref()
      .is_some_and(|t| t.starts_with("@ <B>")),
    "the nested pair is crossed whole rather than stopping the skip at its first name"
  );
  assert_eq!(
    node_text!(parse_directive, src, K::TypeGenerics).as_deref(),
    Some("<A @ <B> C>"),
    "the generic list closes on its OWN `>`, not on the nested one"
  );
  assert_eq!(text!(parse_directive, src), src);
}

// ---------------------------------------------------------------------------------------------
// Selections — fields, aliases and the four-armed `...` dispatch.
// ---------------------------------------------------------------------------------------------

#[test]
fn a_field_wraps_its_alias_arguments_directives_and_selection_set() {
  assert_eq!(kinds!(parse_field, "f"), vec![K::Root, K::Field]);
  assert_eq!(
    kinds!(parse_field, "a: f"),
    vec![K::Root, K::Field, K::Alias],
    "the alias is a retro-wrap over the name already committed and the colon after it"
  );
  assert_eq!(
    node_text!(parse_field, "a: f", K::Alias).as_deref(),
    Some("a:")
  );
  assert_eq!(
    kinds!(parse_field, "a: f(x: 1) @d { g }"),
    vec![
      K::Root,
      K::Field,
      K::Alias,
      K::Arguments,
      K::Argument,
      K::IntValue,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ]
  );
}

/// An empty delimited list is **reported without consuming**, so its closer stays the loop's.
///
/// Every `+` collection in this task makes the same check, and reaching for the consuming
/// `recover::unexpected` instead of `recover::report_unexpected` is the plausible slip: both
/// report, both keep every byte, and the diagnostic set is not compared by any gate. The
/// difference is that the consuming form eats the closer — a closer *is* a depth-zero sync
/// point — after which the list runs to end of input hunting a `}` it had already swallowed and
/// emits a second, spurious unclosed-delimiter diagnostic. The **only** witness is the extra
/// `Error` child, which is why each assertion below is a pre-order and not a verdict.
///
/// Measured: `{}`'s check swapped to the consuming form is invisible to every other test in this
/// file.
#[test]
fn an_empty_delimited_list_reports_without_eating_its_own_closer() {
  assert!(
    parse_selection_set("{}").has_errors(),
    "`graphqlx/syntactic/selection/mod.rs`'s `at_least(1)` rejects `{{}}`, and gate 1 compares \
     verdicts"
  );
  assert_eq!(
    kinds!(parse_selection_set, "{}"),
    vec![K::Root, K::SelectionSet],
    "no Error child: the `}}` was consumed by the set's own loop"
  );
  assert_eq!(text!(parse_selection_set, "{}"), "{}");

  assert!(parse_definition_type_generics("<>").has_errors());
  assert_eq!(
    kinds!(parse_definition_type_generics, "<>"),
    vec![K::Root, K::DefinitionTypeGenerics]
  );
  assert_eq!(text!(parse_definition_type_generics, "<>"), "<>");

  assert!(parse_executable_definition_type_generics("<>").has_errors());
  assert_eq!(
    kinds!(parse_executable_definition_type_generics, "<>"),
    vec![K::Root, K::ExecutableDefinitionTypeGenerics]
  );
  assert_eq!(text!(parse_executable_definition_type_generics, "<>"), "<>");
}

#[test]
fn junk_inside_a_selection_set_costs_one_error_node_and_not_the_set() {
  assert_eq!(
    kinds!(parse_selection_set, "{ a ! b }"),
    vec![K::Root, K::SelectionSet, K::Field, K::Error, K::Field]
  );
  assert_eq!(text!(parse_selection_set, "{ a ! b }"), "{ a ! b }");
}

/// Divergence 11: the `...` dispatch has **four** arms, and two of them are GraphQLx's alone.
#[test]
fn the_spread_dispatch_has_four_arms() {
  assert_eq!(
    kinds!(parse_selection, "... F"),
    vec![K::Root, K::FragmentSpread, K::TypePath, K::Path],
    "an Identifier after `...` is a spread, and its target is a TypePath"
  );
  assert_eq!(
    kinds!(parse_selection, "... ::ns::F"),
    vec![K::Root, K::FragmentSpread, K::TypePath, K::Path],
    "GraphQLx only: a `::` after `...` is a fully qualified spread"
  );
  assert_eq!(
    kinds!(parse_selection, "... on T { f }"),
    vec![
      K::Root,
      K::InlineFragment,
      K::TypeCondition,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(
    kinds!(parse_selection, "... @d { f }"),
    vec![
      K::Root,
      K::InlineFragment,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ],
    "an untyped inline fragment with directives"
  );
  assert_eq!(
    kinds!(parse_selection, "... { f }"),
    vec![K::Root, K::InlineFragment, K::SelectionSet, K::Field],
    "an untyped inline fragment with neither condition nor directives"
  );
}

/// A fragment spread's target keeps its generic arguments, which GraphQL's name cannot hold.
#[test]
fn a_fragment_spread_target_may_carry_generics() {
  assert_eq!(
    kinds!(parse_selection, "... F<Int>"),
    vec![
      K::Root,
      K::FragmentSpread,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ]
  );
}

/// The retro-wrap starts at the `...`, not after it.
///
/// A `node` opened *after* the `...` was committed would leave the spread token as a sibling of
/// the node it introduces. The text is identical either way, so only the node's own extent can
/// tell the two apart.
#[test]
fn a_spread_selection_holds_its_own_spread_token() {
  assert_eq!(
    node_text!(parse_selection, "... F", K::FragmentSpread).as_deref(),
    Some("... F")
  );
  assert_eq!(
    node_text!(parse_selection, "... on T { f }", K::InlineFragment).as_deref(),
    Some("... on T { f }")
  );
}

/// GraphQLx **has** a `TypeCondition` node where GraphQL's kind space has none.
#[test]
fn a_type_condition_is_its_own_node() {
  assert_eq!(
    kinds!(parse_type_condition, "on T"),
    vec![K::Root, K::TypeCondition, K::TypePath, K::Path]
  );
  assert_eq!(
    node_text!(parse_type_condition, "on T", K::TypeCondition).as_deref(),
    Some("on T"),
    "the `on` keyword is inside the condition, not beside it"
  );
  assert_eq!(
    kinds!(parse_type_condition, "on ::ns::T<Int>"),
    vec![
      K::Root,
      K::TypeCondition,
      K::TypePath,
      K::Path,
      K::TypeGenerics,
      K::DefinitionTypePath,
      K::Path
    ]
  );
}

#[test]
fn a_missing_on_keeps_the_condition_and_costs_one_diagnostic() {
  let parse = parse_type_condition("T");
  assert!(parse.has_errors());
  assert_eq!(
    kinds!(parse_type_condition, "T"),
    vec![K::Root, K::TypeCondition, K::TypePath, K::Path],
    "the name that IS there is still the condition's type; eating it would cost the subtree"
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 12 — descriptions where GraphQL has none.
// ---------------------------------------------------------------------------------------------

/// A description is a **bare string token** inside the node it describes, not a node of its own.
///
/// The kind space has no `Description` kind and that is a derived fact, not a preference:
/// `tests/lossless_x_kinds.rs`'s census rejects it under *one token is not a region*, with the
/// recorded reason that a description is a token child of the definition's own retro-wrapped node
/// while every string **value** is a `StringValue` node. A port that reached for `string_value`
/// here would put a `StringValue` node in a description position, which round-trips identically
/// and makes the two indistinguishable to the typed layer.
#[test]
fn a_variable_definition_may_carry_a_description_and_it_is_not_a_node() {
  assert_eq!(
    kinds!(parse_variables_definition, "(\"doc\" $x: Int)"),
    vec![
      K::Root,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::VariableValue,
      K::DefinitionTypePath,
      K::Path
    ],
    "no StringValue node appears: the description is a token"
  );
  assert_eq!(
    node_text!(
      parse_variables_definition,
      "(\"doc\" $x: Int)",
      K::VariableDefinition
    )
    .as_deref(),
    Some("\"doc\" $x: Int"),
    "the description is INSIDE the definition it describes, which only the extent can show"
  );
}

#[test]
fn a_variable_definition_holds_its_default_and_its_const_directives() {
  assert_eq!(
    kinds!(parse_variables_definition, "($x: Int = 1 @d)"),
    vec![
      K::Root,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::VariableValue,
      K::DefinitionTypePath,
      K::Path,
      K::DefaultValue,
      K::IntValue,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path
    ]
  );
  assert!(
    parse_variables_definition("($x: Int @d(y: $v))").has_errors(),
    "a variable definition's directives are Directives[Const] — the one const position inside \
     an executable document"
  );
}

#[test]
fn an_empty_variables_definition_reports() {
  assert!(
    parse_variables_definition("()").has_errors(),
    "`at_least(1)`, unlike Arguments — the two rulings are followed one production at a time"
  );
  assert_eq!(
    kinds!(parse_variables_definition, "()"),
    vec![K::Root, K::VariablesDefinition]
  );
}

// ---------------------------------------------------------------------------------------------
// Divergence 13 — the two generic lists of an executable definition.
// ---------------------------------------------------------------------------------------------

/// `fragment <T> F<U> on X { f }` has **two** generic lists and they are two node kinds.
///
/// The implementation generics are parsed *before* the name
/// (`graphqlx/syntactic/executable/mod.rs:535-536`); the name's own generics are parsed inside
/// [`ExecutableDefinitionName`]. Parsing them in the other order still consumes the same tokens
/// and still re-prints the same text — the pre-order is the only thing that sees the swap.
#[test]
fn a_fragment_definition_has_two_distinct_generic_lists() {
  assert_eq!(
    kinds!(parse_fragment_definition, "fragment <T> F<U> on X { f }"),
    vec![
      K::Root,
      K::FragmentDefinition,
      K::ExecutableDefinitionTypeGenerics,
      K::ExecutableDefinitionName,
      K::ExecutableDefinitionTypeGenerics,
      K::TypeCondition,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ],
    "the implementation list is a SIBLING of the name; the name's own list is a CHILD of it"
  );
  assert_eq!(
    node_text!(
      parse_fragment_definition,
      "fragment <T> F<U> on X { f }",
      K::ExecutableDefinitionName
    )
    .as_deref(),
    Some("F<U>"),
    "the name node covers the name and its own generics, and nothing before them"
  );
}

#[test]
fn a_fragment_definition_needs_neither_generic_list() {
  assert_eq!(
    kinds!(parse_fragment_definition, "fragment F on X { f }"),
    vec![
      K::Root,
      K::FragmentDefinition,
      K::ExecutableDefinitionName,
      K::TypeCondition,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ]
  );
}

#[test]
fn a_fragment_may_be_named_on_because_graphqlx_does_not_reserve_it() {
  let parse = parse_fragment_definition("fragment on on X { f }");
  assert!(
    !parse.has_errors(),
    "GraphQL spends a production on `FragmentName: Name but not on`; GraphQLx's \
     `executable_definition_name` is a plain `take_name`, so the exclusion does not exist here \
     and gate 1 compares verdicts"
  );
}

// ---------------------------------------------------------------------------------------------
// The generic productions Task 11's executable definitions reach.
// ---------------------------------------------------------------------------------------------

#[test]
fn an_executable_definition_generic_list_holds_bare_names() {
  assert_eq!(
    kinds!(parse_executable_definition_type_generics, "<T, U>"),
    vec![K::Root, K::ExecutableDefinitionTypeGenerics],
    "its members are bare Name tokens, so no member node appears"
  );
  assert_eq!(
    kinds!(parse_executable_definition_name, "F"),
    vec![K::Root, K::ExecutableDefinitionName]
  );
}

/// A definition's generic list holds `DefinitionTypeParam` nodes, and a param may have a default.
///
/// This is the list an **operation** name reaches — `named_operation_after_head` calls
/// `try_definition_name`, not the executable one — so `query Q<T = Int>` is grammatical and the
/// two lists are genuinely different node kinds rather than one kind used twice.
#[test]
fn a_definition_generic_list_holds_params_that_may_default() {
  assert_eq!(
    kinds!(parse_definition_type_generics, "<T>"),
    vec![K::Root, K::DefinitionTypeGenerics, K::DefinitionTypeParam]
  );
  assert_eq!(
    kinds!(parse_definition_type_generics, "<T = Int>"),
    vec![
      K::Root,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::DefinitionTypePath,
      K::Path
    ],
    "a default is a bare `= Type`, not a DefaultValue node — the `=` introduces a type here"
  );
  assert_eq!(
    kinds!(parse_definition_name, "Q<T = Int>"),
    vec![
      K::Root,
      K::DefinitionName,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::DefinitionTypePath,
      K::Path
    ]
  );
}

#[test]
fn a_where_predicate_is_a_type_path_colon_and_ampersand_separated_bounds() {
  assert_eq!(
    kinds!(parse_where_predicate, "A: B"),
    vec![
      K::Root,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ]
  );
  assert_eq!(
    kinds!(parse_where_predicate, "A: B & C"),
    vec![
      K::Root,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ]
  );
}

#[test]
fn a_where_clause_holds_every_predicate_and_its_keyword() {
  assert_eq!(
    kinds!(parse_where_clause, "where A: B, C: D"),
    vec![
      K::Root,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path
    ],
    "the comma is trivia, so the two predicates are told apart by their shape and not by it"
  );
  assert_eq!(
    node_text!(parse_where_clause, "where A: B", K::WhereClause).as_deref(),
    Some("where A: B"),
    "the `where` keyword is inside the clause"
  );
}

// ---------------------------------------------------------------------------------------------
// Operations, fragments and the document.
// ---------------------------------------------------------------------------------------------

#[test]
fn a_shorthand_operation_is_one_node_over_its_selection_set() {
  assert_eq!(
    kinds!(parse_operation_definition, "{ f }"),
    vec![K::Root, K::OperationDefinition, K::SelectionSet, K::Field]
  );
}

#[test]
fn a_named_operation_holds_its_keyword_name_variables_and_directives() {
  assert_eq!(
    kinds!(parse_operation_definition, "query Q($x: Int) @d { f }"),
    vec![
      K::Root,
      K::OperationDefinition,
      K::DefinitionName,
      K::VariablesDefinition,
      K::VariableDefinition,
      K::VariableValue,
      K::DefinitionTypePath,
      K::Path,
      K::Directives,
      K::Directive,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(
    node_text!(
      parse_operation_definition,
      "query Q { f }",
      K::OperationDefinition
    )
    .as_deref(),
    Some("query Q { f }"),
    "`query` is a bare token child: the census rejects an OperationType node as one token"
  );
}

/// Divergence 19: a `where` clause may precede an operation's already-mandatory selection set.
#[test]
fn an_operation_may_carry_a_where_clause_before_its_selection_set() {
  assert_eq!(
    kinds!(parse_operation_definition, "query Q<T> where T: Node { f }"),
    vec![
      K::Root,
      K::OperationDefinition,
      K::DefinitionName,
      K::DefinitionTypeGenerics,
      K::DefinitionTypeParam,
      K::WhereClause,
      K::WherePredicate,
      K::TypePath,
      K::Path,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ],
    "the clause and the set are SIBLINGS: the census gives `Constrained` no kind of its own"
  );
}

#[test]
fn an_operation_missing_its_selection_set_after_a_where_reports_from_the_set() {
  let parse = parse_operation_definition("query Q where T: Node");
  assert!(
    parse.has_errors(),
    "divergence 19: the block after an executable `where` is already mandatory, so the absence \
     surfaces as the selection set's own error rather than as an Expectation::LBrace from the \
     where site"
  );
  assert_eq!(
    text!(parse_operation_definition, "query Q where T: Node"),
    "query Q where T: Node"
  );
}

#[test]
fn an_executable_document_holds_every_definition_and_its_descriptions() {
  assert_eq!(
    kinds!(
      parse_executable_document,
      "\"doc\" query Q { f }\nfragment F on T { g }"
    ),
    vec![
      K::Root,
      K::ExecutableDocument,
      K::OperationDefinition,
      K::DefinitionName,
      K::SelectionSet,
      K::Field,
      K::FragmentDefinition,
      K::ExecutableDefinitionName,
      K::TypeCondition,
      K::TypePath,
      K::Path,
      K::SelectionSet,
      K::Field
    ]
  );
  assert_eq!(
    node_text!(
      parse_executable_document,
      "\"doc\" query Q { f }",
      K::OperationDefinition
    )
    .as_deref(),
    Some("\"doc\" query Q { f }"),
    "the description is retro-wrapped INTO the definition it describes"
  );
}

#[test]
fn an_empty_executable_document_reports_and_is_still_a_node() {
  let parse = parse_executable_document("");
  assert!(parse.has_errors());
  assert_eq!(
    kinds!(parse_executable_document, ""),
    vec![K::Root, K::ExecutableDocument]
  );
}

#[test]
fn an_unknown_definition_head_is_reported_and_skipped() {
  let parse = parse_executable_document("nonsense { f }");
  assert!(parse.has_errors());
  assert_eq!(
    text!(parse_executable_document, "nonsense { f }"),
    "nonsense { f }"
  );
}

// ---------------------------------------------------------------------------------------------
// Constness — the flavour no tree shape and no text can show.
// ---------------------------------------------------------------------------------------------

/// A `$` reaches every non-const position, and is reported in every const one.
///
/// **`Constness` is threaded, not inferred**, so each of these positions is a separate literal in
/// the source and a wrong one is invisible to everything else in this file: the node-kind
/// pre-order is identical, the text is identical, and a `VariableValue` node is built either way
/// because the rejection is a diagnostic rather than a refusal to parse. Only the verdict moves.
///
/// This test exists because the mutation *"a field's arguments made `Constness::Const`"* was
/// measured **green** against every other assertion here — and `user(id: $id)` is the commonest
/// query there is, so that defect would reject ordinary documents while every structural gate
/// stayed quiet.
#[test]
fn the_constness_flavour_is_threaded_correctly_into_every_position() {
  // Non-const: a variable is exactly what these positions are for.
  for src in ["f(id: $v)", "f @d(x: $v)", "a: f(id: $v) @d(y: $v)"] {
    assert!(
      !parse_field(src).has_errors(),
      "a field's arguments and directives are the non-const flavour: {src}"
    );
  }
  assert!(
    !parse_directives("@d(x: $v)").has_errors(),
    "an executable directive run threads NonConst down into its own arguments"
  );
  for src in [
    "... F @d(x: $v)",
    "... on T @d(x: $v) { f }",
    "... @d(x: $v) { f }",
  ] {
    assert!(
      !parse_selection(src).has_errors(),
      "every spread arm's directives are the non-const flavour: {src}"
    );
  }
  assert!(
    !parse_operation_definition("query Q($v: Int) @d(x: $v) { f }").has_errors(),
    "an operation's directives are the non-const flavour"
  );
  assert!(
    !parse_fragment_definition("fragment F on T @d(x: $v) { f }").has_errors(),
    "a fragment definition's directives are the non-const flavour"
  );

  // Const: the three positions inside an executable document where a `$` is a grammar error.
  assert!(
    parse_const_directives("@d(x: $v)").has_errors(),
    "an SDL directive run threads Const down into its own arguments"
  );
  assert!(
    parse_variables_definition("($x: Int @d(y: $v))").has_errors(),
    "a variable definition's directives are Directives[Const]"
  );
  assert!(
    parse_variables_definition("($x: Int = $v)").has_errors(),
    "a DefaultValue is const in both positions the grammar puts it in"
  );
}

// ---------------------------------------------------------------------------------------------
// The head sets and their predicates agree.
// ---------------------------------------------------------------------------------------------

/// A `TypePath` is reachable from three Task 11 positions and is one node kind in all of them.
///
/// Guards against the port that widened only the directive name, or only the spread target: the
/// three call sites are separate lines and a missed one is invisible to every other test here.
#[test]
fn every_task_11_type_path_position_builds_the_same_node() {
  let expected = vec![K::Root, K::TypePath, K::Path];
  assert_eq!(kinds!(parse_type_path, "ns::T"), expected);
  assert_eq!(
    kinds!(parse_directive, "@ns::T")
      .into_iter()
      .filter(|k| matches!(k, K::Root | K::TypePath | K::Path))
      .collect::<Vec<_>>(),
    expected
  );
  assert_eq!(
    kinds!(parse_selection, "... ns::T")
      .into_iter()
      .filter(|k| matches!(k, K::Root | K::TypePath | K::Path))
      .collect::<Vec<_>>(),
    expected
  );
}
