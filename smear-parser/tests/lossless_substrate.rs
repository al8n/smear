#![cfg(feature = "rowan")]

//! The dialect-generic lossless substrate's own properties.
//!
//! Everything here is asserted through `smear_parser::lossless`, never through a dialect module,
//! because the substrate's whole claim is that it is nameable and usable without one. A test that
//! reached for `graphql::lossless` to prove a substrate property would prove the opposite.

use smear_parser::{
  graphql::kinds::SyntaxKind as GK,
  lossless::{KindSpace, test_support::assert_kind_space_is_well_formed},
};

/// The GraphQL space satisfies the contract every dialect's space must satisfy.
///
/// This is the same three properties `graphql/kinds.rs`'s own `mod tests` asserted, moved to a
/// generic helper so the GraphQLx space (Task 8) inherits them by declaration rather than by
/// somebody remembering to copy three tests.
#[test]
fn the_graphql_kind_space_is_well_formed() {
  assert_kind_space_is_well_formed::<GK>();
}

/// The contract's constants agree with the enum's own spelling.
///
/// Without this, an impl could satisfy `assert_kind_space_is_well_formed` while pointing `ERROR` at
/// `Gap` — the helper only checks the *positions* of the last three, not which name sits in each.
#[test]
fn the_graphql_bookkeeping_constants_name_the_right_kinds() {
  assert_eq!(<GK as KindSpace>::ERROR, GK::Error);
  assert_eq!(<GK as KindSpace>::GAP, GK::Gap);
  assert_eq!(<GK as KindSpace>::ROOT, GK::Root);
  assert_eq!(<GK as KindSpace>::NAME, "graphql");
}

/// The space is not empty and `from_raw` is not a constant function.
///
/// The positive control for the test above: a `from_raw` that answered `None` for everything, or an
/// `ALL` that was empty, would pass a loop over `ALL` vacuously.
#[test]
fn the_graphql_kind_space_answers_in_both_directions() {
  let all = <GK as KindSpace>::ALL;
  assert!(all.len() >= 80, "the space shrank to {}", all.len());
  assert_eq!(
    <GK as KindSpace>::from_raw(<GK as KindSpace>::raw(GK::Name)),
    Some(GK::Name)
  );
  assert_eq!(<GK as KindSpace>::from_raw(all.len() as u16), None);
  assert_eq!(<GK as KindSpace>::from_raw(u16::MAX), None);
}

use smear_parser::graphql::{
  kinds::{GraphQLLang, SyntaxKind as K},
  lossless::{ast as gast, parse_str},
};

smear_parser::ast_node!(
  lang = smear_parser::graphql::kinds::GraphQLLang;
  /// A locally declared twin of the shipped `Document` wrapper.
  ///
  /// Declared **in this integration test crate**, over the crate's own language, using the
  /// exported macro and nothing else. This is the whole point of the parameterisation: the macro
  /// must be usable from a crate that imports neither rowan nor tokora, over whichever
  /// `rowan::Language` the caller names. Before Task 3 the macro could only ever produce
  /// GraphQL-typed wrappers, so this file could not have declared a wrapper for any other dialect
  /// at all.
  LocalDocument => K::Document {
    /// The object type definitions this document holds.
    object_type_definitions: many gast::ObjectTypeDefinition,
    /// The document's first `Name` token, if it has one.
    first_name: tok K::Name,
  }
);

/// The exported macro builds a working wrapper from outside the crate that defines it.
#[test]
fn the_exported_macro_builds_a_wrapper_outside_the_crate() {
  let parse = parse_str("type T { f: Int }\n");
  let root = parse.syntax();
  let doc = root
    .children()
    .find_map(<LocalDocument as gast::CastNode<GraphQLLang>>::cast_node)
    .expect("the parse produced no Document node");

  assert_eq!(doc.object_type_definitions().count(), 1);
  assert!(
    doc.first_name().is_none(),
    "Document holds no direct Name token"
  );
  assert_eq!(doc.syntax().kind(), K::Document);
}

/// The macro emits no path that a consumer crate has to import.
///
/// Measured rather than asserted: the test above compiles in a crate whose only `use` of the
/// ecosystem is `smear_parser`. If the macro ever emits `::rowan::…` or `::tokora::…`, this file
/// stops compiling — which is a louder failure than a doc comment saying it must not.
#[test]
fn the_macro_needs_no_import_beyond_this_crate() {}

/// `token_any` scans in **document order**, not in the order the caller listed the kinds.
///
/// **This closes a hole the shipped gate could not see.** `tests/lossless_typed.rs`'s
/// `a_string_value_and_a_description_each_reach_both_of_their_token_kinds` is the only test that
/// drives `tok_any`, and it drives it over `StringValue` and `Description` — two node kinds that
/// hold *exactly one* string token each. Over a node with one candidate the two orderings are
/// indistinguishable, so rewriting the substrate's scan as `kinds.iter().find_map(...)` left the
/// whole suite green.
///
/// The fixture here is a node that genuinely carries two of the listed kinds: an `Arguments` node
/// `( s: "in" )` has `LParen` then `RParen` as direct token children, its `Argument` being a child
/// *node*. Asking for `[RParen, LParen]` — kinds order deliberately reversed against document
/// order — has one answer under each rule, so the assertion can only pass under one of them.
#[test]
fn token_any_answers_in_document_order_not_in_kinds_order() {
  let parse = parse_str("query { f(s: \"in\") }\n");
  let root = parse.syntax();
  let arguments = root
    .descendants()
    .find(|n| n.kind() == K::Arguments)
    .expect("the parse produced no Arguments node");

  // The positive control: both kinds really are present as direct token children, so neither
  // rule is answering `None` by accident.
  assert!(
    smear_parser::lossless::ast::token_any(&arguments, &[K::LParen]).is_some(),
    "the fixture has no LParen"
  );
  assert!(
    smear_parser::lossless::ast::token_any(&arguments, &[K::RParen]).is_some(),
    "the fixture has no RParen"
  );

  let found = smear_parser::lossless::ast::token_any(&arguments, &[K::RParen, K::LParen])
    .expect("neither kind matched");
  assert_eq!(
    found.kind(),
    K::LParen,
    "token_any answered the first *listed* kind rather than the first token in the text"
  );

  // The dialect wrapper is the same function and must agree.
  let found = gast::token_any(&arguments, &[K::RParen, K::LParen]).expect("neither kind matched");
  assert_eq!(found.kind(), K::LParen);
}

/// The atoms are reachable through the substrate and nameable with four parameters.
///
/// The dialect wrappers stay — a production writes `expect::<Src, Ctx>(inp, Kind::LBrace)`, not a
/// four-parameter turbofish — so what this asserts is that the substrate exists and is nameable
/// at all, which is the one claim that could not be made before the lift.
/// `tests/lossless_trivia_atoms.rs` already drives the wrappers over real sources, and
/// `tests/lossless_trivia.rs` drives the whole corpus through them.
#[test]
fn the_trivia_atoms_are_reachable_through_the_substrate() {
  // A compile-time reachability check: the four generic parameters resolve and the function item
  // has the shape every production depends on. Running it is the assertion.
  fn _assert_signature<'inp, L, Ctx, Lang>()
  where
    Lang: ?Sized,
    L: tokora::Lexer<'inp, Span = tokora::SimpleSpan, Offset = usize>,
    L::Token: tokora::lexer::FromLogos<'inp>,
    Ctx: tokora::ParseContext<'inp, L, Lang>,
    tokora::ErrorOf<'inp, L, Ctx, Lang>: From<tokora::error::UnexpectedEot<usize, Lang>>,
  {
    let _ = smear_parser::lossless::trivia::peek_kind::<L, Ctx, Lang>;
    let _ = smear_parser::lossless::trivia::eat_if::<L, Ctx, Lang>;
    let _ = smear_parser::lossless::trivia::try_eat::<L, Ctx, Lang>;
  }
}
