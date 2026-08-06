#![cfg(feature = "rowan")]

//! The dialect-generic lossless substrate's own properties.
//!
//! Everything here is asserted through `smear::parser::lossless`, never through a dialect module,
//! because the substrate's whole claim is that it is nameable and usable without one. A test that
//! reached for `graphql::lossless` to prove a substrate property would prove the opposite.

use smear::parser::{
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

use smear::parser::graphql::{
  kinds::{GraphQLLang, SyntaxKind as K},
  lossless::{ast as gast, parse_document},
};

smear::ast_node!(
  lang = smear::parser::graphql::kinds::GraphQLLang;
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
  let parse = parse_document("type T { f: Int }\n");
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
/// ecosystem is `smear`. If the macro ever emits `::rowan::…` or `::tokora::…`, this file
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
  let parse = parse_document("query { f(s: \"in\") }\n");
  let root = parse.syntax();
  let arguments = root
    .descendants()
    .find(|n| n.kind() == K::Arguments)
    .expect("the parse produced no Arguments node");

  // The positive control: both kinds really are present as direct token children, so neither
  // rule is answering `None` by accident.
  assert!(
    smear::parser::lossless::ast::token_any(&arguments, &[K::LParen]).is_some(),
    "the fixture has no LParen"
  );
  assert!(
    smear::parser::lossless::ast::token_any(&arguments, &[K::RParen]).is_some(),
    "the fixture has no RParen"
  );

  let found = smear::parser::lossless::ast::token_any(&arguments, &[K::RParen, K::LParen])
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
    let _ = smear::parser::lossless::trivia::peek_kind::<L, Ctx, Lang>;
    let _ = smear::parser::lossless::trivia::eat_if::<L, Ctx, Lang>;
    let _ = smear::parser::lossless::trivia::try_eat::<L, Ctx, Lang>;
  }
}

/// The two dialects' coverage tallies are separate lanes and do not contaminate each other.
///
/// This is the property a single thread-local `Vec<u32>` could not have, and it is the reason the
/// counter had to be redesigned rather than moved. Before the GraphQLx space exists there is only
/// one lane to check, so this asserts the *mechanism*: the lane is addressable by kind space, and
/// resetting it clears exactly it.
#[cfg(feature = "lossless-coverage")]
#[test]
fn a_coverage_lane_is_per_kind_space() {
  use smear::parser::lossless::coverage;

  coverage::reset::<GK>();
  let _ = parse_document("type T { f: Int }\n");
  let after = coverage::hits_of::<GK>(GK::ObjectTypeDefinition);
  assert!(after >= 1, "the graphql lane recorded nothing");

  // Resetting a *different* lane leaves this one alone. This is the assertion that a degenerate
  // tally — one keyed by a constant on every door, which is what the pre-lift single `Vec<u32>`
  // amounts to — cannot pass, and it does not need a second dialect to exist: a second
  // `KindSpace` impl declared in this test crate is a second lane by definition.
  coverage::reset::<OtherSpace>();
  assert_eq!(
    coverage::hits_of::<GK>(GK::ObjectTypeDefinition),
    after,
    "resetting another kind space's lane cleared the graphql lane, so the tally is not keyed"
  );

  // A second reset of *this* lane clears it; the assertion is that the lane is addressable at
  // all, which is what a keyed tally buys over a single global one.
  coverage::reset::<GK>();
  assert_eq!(coverage::hits_of::<GK>(GK::ObjectTypeDefinition), 0);
}

/// A second kind space, declared here so the lane property has two lanes to be about.
///
/// It is never parsed into and never emits a node — it exists only to be a distinct
/// [`KindSpace::NAME`], which is the whole key the tally is addressed by. Three kinds, because
/// the contract's bookkeeping triple is the last three and a space needs at least that many.
#[cfg(feature = "lossless-coverage")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OtherSpace {
  Error,
  Gap,
  Root,
}

#[cfg(feature = "lossless-coverage")]
impl KindSpace for OtherSpace {
  const NAME: &'static str = "other-space-for-the-lane-test";
  const ERROR: Self = Self::Error;
  const GAP: Self = Self::Gap;
  const ROOT: Self = Self::Root;
  const ALL: &'static [Self] = &[Self::Error, Self::Gap, Self::Root];

  fn raw(self) -> u16 {
    self as u16
  }

  fn from_raw(raw: u16) -> Option<Self> {
    Self::ALL.get(raw as usize).copied()
  }
}

/// The second space is itself well-formed, so the lane test is not keyed off a broken one.
#[cfg(feature = "lossless-coverage")]
#[test]
fn the_other_space_is_well_formed() {
  assert_kind_space_is_well_formed::<OtherSpace>();
}

/// `Parse` is generic over the language and still answers the three questions every gate asks.
#[test]
fn the_parse_surface_is_language_generic() {
  let parse: smear::parser::lossless::runner::Parse<GraphQLLang> =
    parse_document("type T { f: Int }\n");
  assert!(!parse.has_errors());
  assert_eq!(parse.syntax().text().to_string(), "type T { f: Int }\n");
  assert!(parse.diagnostics().is_empty());
}

/// A **declined** retro-wrap is not counted, only an accepted one.
///
/// **This closes a hole the shipped counter test could not see.** `lossless_trivia.rs`'s
/// `the_hit_counter_distinguishes_a_reached_production_from_an_unreached_one` drives
/// `ObjectTypeDefinition` and `EnumTypeDefinition`, both opened with `node` — the `ParseInput`
/// shape, whose success test is simply `Ok`. Nothing exercised the `TryParseInput` shape, so
/// relaxing its `Ok(Accept)` test to a bare `is_ok()` left the whole suite green.
///
/// The distinction is the counter's entire worth for the two retro-wrap probes: `node_at` over
/// `try_eat` runs on **every** field and **every** type reference, so counting declines would
/// report thousands of `Alias` and `NonNullType` hits over a corpus containing neither, and the
/// coverage gate would read as satisfied by productions the corpus never reached.
#[cfg(feature = "lossless-coverage")]
#[test]
fn a_declined_retro_wrap_is_not_counted() {
  use smear::parser::lossless::coverage;

  coverage::reset::<GK>();
  // Two fields and two type references, so both retro-wrap probes run — and decline: no `:` in
  // alias position, no `!` anywhere.
  let parse = parse_document("query { a b } type T { f: Int }\n");
  assert!(!parse.has_errors());

  // The positive control: the probes really did run, which is what makes the two zeroes below
  // evidence rather than an absence of parsing.
  assert!(
    coverage::hits_of::<GK>(GK::Field) >= 2,
    "the fixture opened no Field, so nothing ran a retro-wrap probe"
  );
  assert!(
    coverage::hits_of::<GK>(GK::NamedType) >= 1,
    "the fixture opened no NamedType, so nothing ran the non-null probe"
  );

  assert_eq!(
    coverage::hits_of::<GK>(GK::Alias),
    0,
    "a declined `node_at` over try_eat(Colon) was counted"
  );
  assert_eq!(
    coverage::hits_of::<GK>(GK::NonNullType),
    0,
    "a declined `node_at` over try_eat(Bang) was counted"
  );

  // And an *accepted* one is counted, so the impl is not simply never firing.
  coverage::reset::<GK>();
  let parse = parse_document("query { alias: a } type T { f: Int! }\n");
  assert!(!parse.has_errors());
  assert_eq!(coverage::hits_of::<GK>(GK::Alias), 1);
  assert_eq!(coverage::hits_of::<GK>(GK::NonNullType), 1);
}

/// A materialization failure names the dialect whose sink produced it.
///
/// **The `space` argument's only job, and until now nothing reached it.** `finish_root` is shared
/// by both dialects, so its panic is the one place a reader of a malformed-stream crash learns
/// *which* suite emitted it — and replacing the message with a bare `unwrap()` left every test
/// green, because no production can sever the token channel and no probe tried to.
/// `structure_without_tokens` is that probe: it wraps a node over a nonempty source and commits
/// nothing. An unclosed node is *not* the shape to probe with — `finish_partial` closes one by
/// design, so it never reaches the arm.
#[test]
#[should_panic(expected = "the graphql lossless sink emitted a malformed event stream")]
fn a_malformed_stream_panics_naming_the_dialect() {
  let _ = smear::parser::graphql::lossless::runner::test_support::structure_without_tokens("a", 0);
}

/// Every delimiter pair a dialect declares reaches **its own** report, and one it does not
/// declare reaches the catch-all.
///
/// **This is the only door onto `lossless_error_impls!`'s `unclosed` list, and it has to be a
/// direct call rather than a parse.** `Parse` drops the typed payload at the materialization
/// boundary — deliberately, so a `Parse` can outlive the source — so every parse-level assertion
/// about an unterminated `[` can say no more than `has_errors()`. Task 6 measured that: swapping
/// `"[]" => unclosed_list` for `"[]" => unclosed_object`, and replacing the catch-all arm with
/// `unreachable!()`, both left the whole suite green.
///
/// The list is a **macro argument**, so a dialect states its pairs rather than inheriting a body,
/// and GraphQLx will state a fourth (`<>`). A silent list is one a second dialect can get wrong in
/// a way nothing reports.
///
/// The `<>` case is not synthetic: tokora ships `UnclosedAngle`, GraphQL's grammar opens no angle
/// bracket and its list therefore omits the pair, so the catch-all is genuinely reachable here —
/// and reaching it is what proves the arm produces an error instead of panicking.
#[test]
fn each_declared_delimiter_pair_reaches_its_own_report() {
  use smear::parser::{
    graphql::{
      GraphQL,
      error::{ErrorData, Unclosed as DialectUnclosed},
      lossless::GraphqlLosslessErrors,
    },
    lexer::graphql::lossless::LosslessLexer,
  };
  use tokora::{
    SimpleSpan,
    emitter::FromUnclosed,
    error::{UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen},
  };

  type Errs = GraphqlLosslessErrors<&'static str>;
  type Lx<'a> = LosslessLexer<'a, &'a str>;

  let span = SimpleSpan::new(3, 4);

  /// The container holds exactly one error; hand back the discriminant its data carries.
  macro_rules! only_data {
    ($err:expr) => {{
      let errs: Errs = <Errs as FromUnclosed<'_, Lx<'_>, GraphQL>>::from_unclosed($err);
      let mut it = errs.into_iter();
      let first = it.next().expect("the conversion produced no error at all");
      assert!(it.next().is_none(), "the conversion produced more than one");
      first
    }};
  }

  let bracket = only_data!(UnclosedBracket::<SimpleSpan, GraphQL>::bracket_of(span));
  assert!(
    matches!(bracket.data(), ErrorData::Unclosed(DialectUnclosed::List)),
    "`[]` did not reach unclosed_list: {:?}",
    bracket.data()
  );

  let brace = only_data!(UnclosedBrace::<SimpleSpan, GraphQL>::brace_of(span));
  assert!(
    matches!(brace.data(), ErrorData::Unclosed(DialectUnclosed::Object)),
    "`{{}}` did not reach unclosed_object: {:?}",
    brace.data()
  );

  let paren = only_data!(UnclosedParen::<SimpleSpan, GraphQL>::paren_of(span));
  assert!(
    matches!(
      paren.data(),
      ErrorData::Unclosed(DialectUnclosed::Parentheses)
    ),
    "`()` did not reach unclosed_parentheses: {:?}",
    paren.data()
  );

  // The pair GraphQL does not declare. The catch-all must answer, and must answer with an error.
  let angle = only_data!(UnclosedAngle::<SimpleSpan, GraphQL>::angle_of(span));
  assert!(
    matches!(angle.data(), ErrorData::Other(note) if note == "unclosed delimiter"),
    "an undeclared pair did not reach the catch-all: {:?}",
    angle.data()
  );

  // The span survives every arm — the diagnostic points at the opener, not at the end of input.
  assert_eq!(bracket.span().start(), 3);
}
