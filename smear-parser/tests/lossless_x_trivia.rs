#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! Gate 2: GraphQLx trivia injection, with a per-production hit counter.
//!
//! The invariant: **trivia is invisible to the grammar.** Padding every token boundary of a valid
//! document with any amount of any ignorable form must change neither the verdict nor the shape of
//! the tree — only its bytes. That is the property the whole `lossless/trivia.rs` atom set exists to
//! provide, and it is the one a trivia-surfacing lexer makes easy to lose: a single decision point
//! that peeks without committing leading trivia turns a comment into a syntax error.
//!
//! # What GraphQLx puts at stake that GraphQL does not
//!
//! GraphQL's discriminating junctions are all *delimiters* — `$ x`, `@ d`, `... F`, `alias : f`,
//! `Int !`, `f (a: 1)` — and a skip lost at one of them costs a rejection. GraphQLx has three
//! junctions where the **node kind itself** is decided on what the next significant token turns out
//! to be, so a lost skip there is not a rejection but a *different tree*:
//!
//! - `set`/`map` followed by `{` is a [`SetValue`](K::SetValue)/[`MapValue`](K::MapValue); the same
//!   spelling not followed by `{` is an ordinary [`EnumValue`](K::EnumValue). The production commits
//!   the keyword and retro-wraps, so the `{` may be any amount of trivia away.
//! - `<T>` is a [`SetType`](K::SetType) and `<K => V>` a [`MapType`](K::MapType), decided on the
//!   token after the first inner type.
//! - a `where` predicate's continuation is a **two-token** window, and `peek_second_kind` is the
//!   only atom in the suite that has to cross trivia *between* two tokens rather than in front of
//!   one. A fixed `peek::<U2>()` answers `Space` for `A : B`; this gate is what would notice.
//!
//! The first two are why the shape comparison is not redundant with the verdict one here the way it
//! nearly is in GraphQL: `set {1}` mis-decided still parses, it just parses as something else.
//!
//! # The alphabet
//!
//! All eight forms `LosslessToken::is_trivia` admits, one variant per form: space, tab, newline,
//! carriage return, CRLF, comment, comma and BOM. It is not restated here — it is
//! [`padding::ALPHABET`], the single copy gate 1 also reads, which is how "the padded set is exactly
//! the one gate 2 derives" is discharged by identity rather than by two constants agreeing.
//!
//! **What the last four buy is the lexer's trivia classification, not the token mapper.** Phase A
//! measured the mapper claim wrong twice: the mapper cannot *forget* an arm, and a mapper with a
//! *wrong* arm leaves this gate green — `lossless_x_kind_map.rs` is the file that owns that
//! question. What they buy is that **no corpus entry contains a carriage return, a tab or a BOM**,
//! which [`the_corpus_supplies_no_carriage_return_tab_or_bom`] pins over `tests/corpusx/`; without
//! these four the sweep would only ever confirm that the atoms cross a space, a newline, a comment
//! and a comma.
//!
//! **The BOM probe was re-run for GraphQLx and it passed.** Phase A recorded that the GraphQL
//! syntactic lexer skips a UTF-8 BOM at any offset as a behavioural property of a hand-written
//! dispatch loop rather than a spec guarantee, so it cannot be inherited. It is re-measured here
//! against the *GraphQLx* syntactic lexer, standing rather than once —
//! [`the_graphqlx_syntactic_lexer_skips_a_bom_at_any_offset`] — because gate 1 runs that lexer over
//! every padded variant this alphabet produces, and a position-sensitive BOM would turn all
//! ninety-odd padded BOM variants into spurious parity failures.
//!
//! # The counter
//!
//! An injection gate that reports success without reporting *reach* is the vacuous case: it cannot
//! tell a corpus that exercises every production from one that exercises three shapes many times.
//! So the productions are instrumented — see [`smear_parser::graphqlx::lossless::coverage`] — and
//! [`trivia_injection_preserves_the_verdict_and_the_shape`] asserts every node kind a production can
//! open was opened at least once during its own sweep. Run it with `--features lossless-coverage`;
//! without the feature the instrumentation is not compiled and the assertion is not made.
//!
//! Both exception sets are **derived**, not hand-listed. [`node_kinds`] takes the kind space minus
//! its image prefix and minus the bookkeeping triple the [`KindSpace`] trait names, so a kind added
//! anywhere in the node block defaults into the must-be-covered set — the safe direction — and a
//! seventy-ninth node kind nobody reaches is a failure rather than an omission.
//!
//! And the counter has a blind spot of its own, which the plan does not name and a mutation found:
//! a **loop that runs exactly once** reports full coverage while its continuation test is dead.
//! [`every_looping_production_is_written_with_two_members_somewhere`] is the answer;
//! [`REPETITIONS_THAT_MUST_CONTINUE`] carries the measurement that motivated it.
//!
//! # Two assertions that no mutation reds on today's tree, and why both stay
//!
//! Recorded rather than trimmed, because "nothing catches this" and "this catches nothing" are
//! different claims and only the first is true here.
//!
//! - **The shape comparison.** Dropped, every product mutation in this task's report is still red:
//!   in GraphQLx a decision point that stops crossing trivia *rejects*, so the verdict assertion
//!   fires first. What the shape half would catch is a padded question answered with a
//!   valid-but-different tree, which no current production can produce and the next one might.
//! - **The two alternate-root invariance comparisons.** They are now paired with a syntactic anchor
//!   that subsumes them for every defect confined to the lossless suite. What remains theirs alone
//!   is the case gate 1 records for the mixed root: both suites moving together. A defect in
//!   `syntactic/` and one in `lossless/` that agreed would satisfy the anchor and fail here.
//!
//! The anchor itself is not defence in depth — it is the fix for a measured hole. Breaking
//! `type_system_document`'s loop guard so it stops crossing trailing trivia flips that root from
//! accept to reject on every corpus entry, compact and padded alike; invariance is satisfied by both
//! sides moving, and before the anchor the whole suite was green on it.

use std::collections::BTreeSet;

use smear_lexer::graphqlx::{
  lossless::{LosslessLexer, LosslessTokenKind as LK},
  syntactic::{SyntacticLexer, SyntacticTokenKind as SK},
};
use smear_parser::{
  graphqlx::{
    GraphQLx,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlxErrors,
    kinds::SyntaxKind as K,
    lossless::{
      Parse, kind_map::token_kind, parse_executable_document, parse_str, parse_type_system_document,
    },
    syntactic::{GraphqlxLexer, definition::type_system_document, executable::executable_document},
  },
  lossless::KindSpace,
};
use tokora::{Lexer as _, Parse as _, Parser};

#[path = "support/graphqlx_padding.rs"]
mod padding;

use padding::{ALPHABET, UNPADDABLE, corpus_files, inject, name_of, token_boundaries};

/// The two node kinds [`parse_str`] structurally cannot reach, and the reason each is out.
///
/// Both are **roots**, and a parse has exactly one. `parse_str` drives the mixed
/// [`Document`](K::Document), so the SDL-only and the executable-only roots are reachable only
/// through their own entries — a property of the suite's shape rather than a corpus gap, and no
/// entry could ever close it. The gate drives all three roots for that reason; this list is what
/// keeps a *third* unreached kind from hiding behind the two that are explained.
///
/// Listed in [`K::ALL`] declaration order, which is the order the check below reports.
const UNREACHABLE_FROM_PARSE_STR: &[K] = &[K::ExecutableDocument, K::TypeSystemDocument];

/// Repetition productions whose **continuation** the corpus has to exercise, as (container, member).
///
/// The hit counter makes an unreached production visible. It does **not** make a production whose
/// loop body runs exactly once visible, and that is a distinct hole with the same symptom: the count
/// is non-zero, the coverage assertion is satisfied, and a continuation test that had stopped
/// working would still be green.
///
/// It is not hypothetical. Making [`peek_second_kind`]'s inner peek stop crossing the trivia
/// *between* its two tokens — the one atom GraphQLx adds to the shared set, and the whole reason
/// `where_clause` asks a two-token question — left this entire gate green, because every `where`
/// clause in `tests/corpusx/` carried exactly one predicate and a loop that stops after one is
/// indistinguishable from a loop that was going to stop anyway.
/// `valid_x_where_multiple_predicates.graphqlx` was added to close it.
///
/// A count ratio is **not** the test. `ObjectValue` and `ObjectField` tally equal over this corpus
/// and their loop iterates perfectly well — `valid_value_object.graphqlx` writes `{}` and
/// `{ k: 1, j: "s" }`, and the empty one pays for the two-field one. So the assertion is structural:
/// somewhere in the corpus, one container node holds two members.
///
/// [`peek_second_kind`]: smear_parser::graphqlx::lossless::trivia
const REPETITIONS_THAT_MUST_CONTINUE: &[(K, K)] = &[
  (K::WhereClause, K::WherePredicate),
  (K::DefinitionTypeGenerics, K::DefinitionTypeParam),
  (K::MapValue, K::MapEntry),
  (K::ImportList, K::NamedSpecifier),
  (K::Arguments, K::Argument),
  (K::ObjectValue, K::ObjectField),
  (K::SelectionSet, K::Field),
];

/// The corpus entries this gate pads: the valid half.
///
/// Invalid entries are out of scope here — gate 1 is the one widened to them. That the valid half is
/// also entirely *paddable* is not assumed:
/// [`the_padded_set_is_gate_ones_set_restricted_to_the_valid_half`] asserts no valid entry is in
/// [`UNPADDABLE`], so a `valid_` entry the lexer starts refusing is a failure rather than a silent
/// forty-fifth skip.
fn valid_corpus() -> Vec<(String, String)> {
  corpus_files()
    .into_iter()
    .filter(|p| name_of(p).starts_with("valid_"))
    .map(|p| {
      let src = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", p.display()));
      (name_of(&p), src)
    })
    .collect()
}

/// Does the **syntactic** SDL-only root reject `src`?
///
/// Gate 1 anchors the mixed root against the syntactic suite and cannot reach the other two, since
/// `parse_str` drives only one of the three. These two functions carry that anchor to the roots gate
/// 1 leaves out, and they are what stops this gate's alternate-root checks from being pure
/// self-invariance — the failure mode measured as `M10`.
///
/// A whole-input verdict, for the reason gate 1 records: tokora's `parse_str` does not check for
/// end-of-input, and these are nonetheless whole-input productions because their repetition
/// re-enters the entry dispatcher on trailing junk and fails there.
fn syntactic_type_system_has_errors(src: &str) -> bool {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(type_system_document)
  .parse_str(src)
  .is_err()
}

/// Does the **syntactic** executable-only root reject `src`? The twin of
/// [`syntactic_type_system_has_errors`].
fn syntactic_executable_has_errors(src: &str) -> bool {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(executable_document)
  .parse_str(src)
  .is_err()
}

/// The node pre-order of a parse as `(depth, kind)` — the tree's *shape*, with its bytes projected
/// away.
///
/// **Depth, not kind alone.** A bare kind pre-order cannot tell a node re-parented onto its own
/// sibling from the tree it should have been: the walk visits the same kinds in the same order
/// either way. Carrying the depth costs nothing and is strictly more of the tree, which matters here
/// because [`trivia_injection_preserves_the_verdict_and_the_shape`] is the only place in this file
/// that compares two trees at all.
///
/// What it deliberately does **not** carry is byte extent or token attribution. Gate 5's golden trees
/// own those, and duplicating them here would make two gates fail for one defect while neither
/// explained it.
fn shape(parse: &Parse) -> Vec<(usize, K)> {
  parse
    .syntax()
    .descendants()
    .map(|n| {
      let depth = n.ancestors().count() - 1;
      (depth, n.kind())
    })
    .collect()
}

/// The node kinds a production can open, derived from the kind space rather than listed.
///
/// Two exclusions, each taken from a declaration the kind space already makes rather than from a
/// list this file would have to maintain:
///
/// - the **token images** are `K::ALL`'s first [`K::IMAGE_BLOCK`] entries, non-trivia and trivia
///   alike. A committed token enters the tree through the mapper, not through `node`.
/// - the **bookkeeping triple** is whatever [`KindSpace`] names as `ERROR`, `GAP` and `ROOT`.
///   `Error` is opened by `recover.rs` through the raw `cst_start_at`/`cst_finish` pair rather than
///   through `node`; `Gap` is minted by the sink; `Root` is named once, at `finish`.
///
/// Everything else is a node kind, so a **newly added** kind defaults into the must-be-covered set,
/// which is the safe direction: an uncovered new production is loud, and a new token image put in
/// the wrong block is loud too.
fn node_kinds() -> Vec<K> {
  let bookkeeping = [
    <K as KindSpace>::ERROR,
    <K as KindSpace>::GAP,
    <K as KindSpace>::ROOT,
  ];
  K::ALL
    .iter()
    .copied()
    .skip(K::IMAGE_BLOCK)
    .filter(|k| !bookkeeping.contains(k))
    .collect()
}

/// Gate 2 proper: the verdict and the shape survive every form of trivia at every boundary, and —
/// with `--features lossless-coverage` — the sweep reached every production while doing it.
///
/// # Why the coverage assertion lives inside this test rather than beside it
///
/// The counter is thread-local and the harness gives each `#[test]` its own thread, which is what
/// keeps one gate's coverage from depending on which other test ran first. The price is that the
/// measurement and the assertion have to share a test body: a sibling test would read its own
/// thread's counter and find it empty.
#[test]
fn trivia_injection_preserves_the_verdict_and_the_shape() {
  #[cfg(feature = "lossless-coverage")]
  smear_parser::graphqlx::lossless::coverage::reset();

  let entries = valid_corpus();
  assert!(
    entries.len() >= 20,
    "only {} valid corpus entries; the sweep is too thin to mean anything",
    entries.len()
  );

  let mut padded_parses = 0usize;
  for (name, src) in &entries {
    let boundaries = token_boundaries(src)
      .unwrap_or_else(|| panic!("{name}: a valid corpus entry must lex, and this one does not"));
    assert!(
      boundaries.len() >= 3,
      "{name}: {} token boundaries — a one-token entry cannot exercise an interior junction",
      boundaries.len()
    );

    let compact = parse_str(src);
    assert!(
      !compact.has_errors(),
      "{name}: the unpadded entry does not parse — this is a corpus fault, not an injection one"
    );
    let compact_shape = shape(&compact);
    // The two roots `parse_str` cannot reach, over the unpadded bytes. Hoisted out of the form
    // loop: their verdicts do not depend on which form is being injected, and re-deriving them
    // eight times would inflate every count in the coverage report by the same factor.
    //
    // Anchored against the **syntactic** suite rather than only against themselves — see
    // `syntactic_type_system_has_errors`. Invariance alone was measured insufficient: a defect that
    // moves an alternate root's compact verdict moves its padded one identically, and the
    // comparison below stays green through both.
    let compact_sdl = parse_type_system_document(src).has_errors();
    let compact_executable = parse_executable_document(src).has_errors();
    assert_eq!(
      compact_sdl,
      syntactic_type_system_has_errors(src),
      "{name}: the lossless SDL-only root and the syntactic one disagree on the unpadded bytes"
    );
    assert_eq!(
      compact_executable,
      syntactic_executable_has_errors(src),
      "{name}: the lossless executable-only root and the syntactic one disagree on the unpadded \
       bytes"
    );

    for (form, pad) in ALPHABET {
      let padded_src = inject(src, &boundaries, pad);
      let padded = parse_str(&padded_src);
      assert!(
        !padded.has_errors(),
        "{name} padded with {form}: the parse reported an error, so some decision point looked at \
         the head without committing the trivia in front of it"
      );
      // Defence in depth, deliberately kept: dropping this line leaves every mutation in this
      // task's report still red, because in GraphQLx a decision point that stops crossing trivia
      // *rejects* rather than building a differently shaped tree, and the verdict above fires
      // first. It stays because that is a property of today's grammar and not of the gate — the
      // next production that answers a padded question with a valid-but-different tree has no
      // other witness at this scale.
      assert_eq!(
        shape(&padded),
        compact_shape,
        "{name} padded with {form}: the node pre-order changed, so the trivia reached the grammar \
         instead of only the tree"
      );
      padded_parses += 1;

      // The same padded bytes through the two roots `parse_str` cannot reach, so the coverage claim
      // below covers them too. Only the verdict is compared: an alternate root rejects most of this
      // corpus, and a *rejected* parse's shape is a function of where the recovery holes fell, which
      // trivia may legitimately move.
      //
      // Each is asserted twice — against its own compact answer, and against the syntactic suite's
      // answer for the same padded bytes. The second is not redundant, and the measurement that
      // says so is `M10` in this task's report: breaking `type_system_document`'s loop guard so it
      // stops crossing trailing trivia flips that root from accept to reject on **every** corpus
      // entry, compact and padded alike, and the invariance comparison is satisfied by both sides
      // moving together. It is the same shape as Task 16's finding, one root over.
      let padded_sdl = parse_type_system_document(&padded_src).has_errors();
      let padded_executable = parse_executable_document(&padded_src).has_errors();
      assert_eq!(
        padded_sdl, compact_sdl,
        "{name} padded with {form}: the SDL-only root changed its verdict"
      );
      assert_eq!(
        padded_executable, compact_executable,
        "{name} padded with {form}: the executable-only root changed its verdict"
      );
      assert_eq!(
        padded_sdl,
        syntactic_type_system_has_errors(&padded_src),
        "{name} padded with {form}: the lossless SDL-only root and the syntactic one disagree"
      );
      assert_eq!(
        padded_executable,
        syntactic_executable_has_errors(&padded_src),
        "{name} padded with {form}: the lossless executable-only root and the syntactic one \
         disagree"
      );
    }
  }

  assert_eq!(
    padded_parses,
    entries.len() * ALPHABET.len(),
    "the sweep did not run every form over every entry"
  );

  #[cfg(feature = "lossless-coverage")]
  {
    use smear_parser::graphqlx::lossless::coverage;

    let mut unhit = Vec::new();
    let mut report = Vec::new();
    for kind in node_kinds() {
      let hits = coverage::hits_of(kind);
      report.push(format!("{kind:?} = {hits}"));
      if hits == 0 {
        unhit.push(kind);
      }
    }
    println!(
      "gate 2 coverage over {padded_parses} padded parses ({} kinds):\n  {}",
      report.len(),
      report.join("\n  ")
    );
    assert!(
      unhit.is_empty(),
      "the injection sweep never reached {} of the {} node kinds: {unhit:?} — their trivia \
       handling is unverified, and the gate's silence about them reads exactly like success",
      unhit.len(),
      report.len()
    );
  }
}

/// The counter is a measurement, so it must be able to answer "no".
///
/// Without this, an instrumentation that counted every kind unconditionally — or a `hits_of` that
/// returned a constant — would satisfy the coverage assertion above forever. The probe is a source
/// that opens exactly one of two node kinds, so one count must move and the other must not;
/// asserting only that some count is non-zero would pass on a counter that never resets.
///
/// The GraphQLx-specific half is the last block. The two dialects share
/// [`crate::lossless::coverage`]'s tally and key their lanes by [`KindSpace::NAME`], so a lane
/// mix-up would show up as a GraphQL parse moving a GraphQLx count — and it would show up *only*
/// here, because every other test in this file speaks one dialect.
#[cfg(feature = "lossless-coverage")]
#[test]
fn the_hit_counter_distinguishes_a_reached_production_from_an_unreached_one() {
  use smear_parser::graphqlx::lossless::coverage;

  coverage::reset();
  assert_eq!(
    coverage::hits_of(K::ObjectTypeDefinition),
    0,
    "reset left a count behind"
  );

  let one = parse_str("type T { f: Int }");
  assert!(!one.has_errors());
  assert_eq!(
    coverage::hits_of(K::ObjectTypeDefinition),
    1,
    "one object type must count once"
  );
  assert_eq!(
    coverage::hits_of(K::EnumTypeDefinition),
    0,
    "there is no enum in that source"
  );

  let two = parse_str("type A { f: Int } type B { g: Int }");
  assert!(!two.has_errors());
  assert_eq!(
    coverage::hits_of(K::ObjectTypeDefinition),
    3,
    "the counter must accumulate across parses until it is reset"
  );

  coverage::reset();
  assert_eq!(
    coverage::hits_of(K::ObjectTypeDefinition),
    0,
    "reset must clear every slot, not only the ones it was asked about"
  );

  // `hits()` is the whole vector and must agree with the per-kind door, or the report the gate
  // prints and the assertion the gate makes could disagree about the same run.
  parse_str("enum E { A }");
  let all = coverage::hits();
  assert_eq!(all.len(), K::ALL.len(), "the tally is indexed by raw kind");
  assert_eq!(
    all[K::EnumTypeDefinition.raw() as usize],
    coverage::hits_of(K::EnumTypeDefinition)
  );
  assert_eq!(all[K::EnumTypeDefinition.raw() as usize], 1);

  // The lanes are per dialect. A GraphQL parse must move GraphQL's tally and leave this one where
  // it was, or one dialect's `reset` would erase the other's measurement mid-gate.
  #[cfg(feature = "graphql")]
  {
    use smear_parser::graphql::{kinds::SyntaxKind as G, lossless as graphql_lossless};

    graphql_lossless::coverage::reset();
    let before = coverage::hits_of(K::EnumTypeDefinition);
    graphql_lossless::parse_str("enum E { A }");
    assert_eq!(
      graphql_lossless::coverage::hits_of(G::EnumTypeDefinition),
      1,
      "the GraphQL parse did not reach the GraphQL lane"
    );
    assert_eq!(
      coverage::hits_of(K::EnumTypeDefinition),
      before,
      "a GraphQL parse moved the GraphQLx lane; the two dialects share one tally"
    );
  }
}

/// Every alphabet entry really is trivia, and really is trivia *everywhere*.
///
/// Two failure modes this closes. An entry that is not ignorable at all — say a stray `#` without
/// its terminator — would make the sweep above a test of something else entirely, and it would
/// announce itself as a grammar bug. And an entry that is ignorable only at offset 0 — the BOM is
/// the standing candidate, since the GraphQL spec requires it to be ignorable without saying where —
/// would pass a leading-only probe and fail on somebody's file.
///
/// The measurement is the token stream itself: padding a fixed source at every boundary with the
/// form under test must leave the **non-trivia** tokens exactly as they were.
#[test]
fn every_alphabet_entry_is_trivia_at_every_position() {
  fn significant(src: &str) -> Vec<LK> {
    let mut lexer = LosslessLexer::<'_, &str>::new(src);
    let mut kinds = Vec::new();
    while let Some(result) = lexer.lex() {
      let token = result.unwrap_or_else(|e| panic!("{src:?} must lex: {e:?}"));
      if !token.is_trivia() {
        kinds.push(token.kind());
      }
    }
    kinds
  }

  let want = significant(PROBE);
  assert_eq!(
    want.len(),
    PROBE_SIGNIFICANT_TOKENS,
    "the probe must carry every junction this gate cares about"
  );

  let boundaries = token_boundaries(PROBE).expect("the probe lexes");
  for (form, pad) in ALPHABET {
    let padded = inject(PROBE, &boundaries, pad);
    assert!(
      padded.len() > PROBE.len(),
      "{form}: injection added nothing"
    );
    assert_eq!(
      significant(&padded),
      want,
      "{form}: padding changed the significant token stream, so this form is not ignorable at \
       every boundary"
    );
  }
}

/// The GraphQLx source every probe in this file pads, and every junction it was chosen for.
///
/// `type T<A> where A: Node { f: set<A>! @d(x: $v) }` in its tightest spelling. The GraphQL
/// junctions — `name:`, `[`type, type`!`, `@`name, name`(`, `$`name — plus the three GraphQLx
/// junctions where the node *kind* is at stake: `<` after a name, `>` closing it, and the two-token
/// `where` window.
const PROBE: &str = "type T<A>where A:Node{f:set<A>!@d(x:$v)}";

/// How many significant tokens [`PROBE`] carries.
///
/// A positive control on the probe rather than a fact about the grammar: an edit that shortened it
/// to something that no longer crosses a `where` clause would otherwise leave both probes passing
/// over a source that had stopped testing what they name.
///
/// The same number for both lexers, which is itself a finding worth having in a constant: GraphQLx's
/// lossless lexer surfaces trivia and its syntactic one drops it, and on a source with no trivia
/// those two must produce token streams of the same length or the alphabet check below would be
/// comparing two different tilings.
const PROBE_SIGNIFICANT_TOKENS: usize = 26;

/// Every form this gate injects is one the **syntactic** lexer is blind to as well.
///
/// This is the load-bearing half of the plan's Task 16 Step 6. Gate 1 runs the syntactic suite over
/// every padded variant this alphabet produces, so a form that the syntactic lexer tolerates only in
/// some positions would not read as a lexer difference — it would read as a *parity failure*, in
/// gate 1, on whichever entry happened to place it awkwardly. The alphabet has to be checked against
/// both lexers or gate 1 is measuring this file's choices.
///
/// Asserted at the token-stream level rather than through a parse, so a form that changed the
/// syntactic lexer's output without changing any verdict is still visible.
#[test]
fn every_alphabet_entry_is_invisible_to_the_syntactic_lexer_too() {
  fn syntactic_kinds(src: &str) -> Vec<SK> {
    let mut lexer = SyntacticLexer::<str>::new(src);
    let mut kinds = Vec::new();
    while let Some(result) = lexer.lex() {
      let token = result.unwrap_or_else(|e| panic!("{src:?} must lex syntactically: {e:?}"));
      kinds.push(token.kind());
    }
    kinds
  }

  let want = syntactic_kinds(PROBE);
  assert_eq!(
    want.len(),
    PROBE_SIGNIFICANT_TOKENS,
    "the syntactic lexer drops trivia before the grammar exists, so it must see exactly the \
     significant tokens the lossless lexer surfaces"
  );

  let boundaries = token_boundaries(PROBE).expect("the probe lexes");
  for (form, pad) in ALPHABET {
    let padded = inject(PROBE, &boundaries, pad);
    assert_eq!(
      syntactic_kinds(&padded),
      want,
      "{form}: the syntactic lexer's token stream changed under an ignorable form, so gate 1's \
       padded sweep would report a parity failure that is really a lexer disagreement"
    );
  }
}

/// The GraphQLx syntactic lexer skips a UTF-8 BOM at **any** offset, not only at offset zero.
///
/// The plan requires this to be re-measured rather than inherited: Phase A established it for the
/// *GraphQL* syntactic lexer through two independent position-blind paths and recorded it as a
/// behavioural property of a hand-written dispatch loop, not a spec guarantee. GraphQLx has its own
/// lexer and could have made the other choice.
///
/// It did not, so `bom` stays in the alphabet. Had it been position-sensitive the answer would have
/// been to drop BOM from the injection alphabet and say so — never to weaken gate 1, whose padded
/// sweep runs this lexer over every variant.
///
/// The four sources are the plan's four, verbatim, and the interior two are the ones that matter.
#[test]
fn the_graphqlx_syntactic_lexer_skips_a_bom_at_any_offset() {
  fn syntactic_kinds(src: &str) -> Vec<SK> {
    let mut lexer = SyntacticLexer::<str>::new(src);
    let mut kinds = Vec::new();
    while let Some(result) = lexer.lex() {
      kinds.push(
        result
          .unwrap_or_else(|e| panic!("{src:?} must lex: {e:?}"))
          .kind(),
      );
    }
    kinds
  }

  let plain = syntactic_kinds("{ a }");
  assert_eq!(
    plain,
    vec![SK::LBrace, SK::Identifier, SK::RBrace],
    "the control must be three significant tokens, or the comparisons below are vacuous"
  );

  for src in [
    "\u{FEFF}{ a }",         // offset 0 — the position the spec speaks about
    "{ a\u{FEFF} }",         // after a token, inside the document
    "{ \u{FEFF}a }",         // between trivia and a token
    "{ a }\u{FEFF}",         // after the last token
    "{\u{FEFF} a }",         // immediately after an opener
    "\u{FEFF}\u{FEFF}{ a }", // twice over, since one BOM could be special-cased
  ] {
    assert_eq!(
      syntactic_kinds(src),
      plain,
      "{src:?}: the GraphQLx syntactic lexer is position-sensitive about the BOM, so `bom` must \
       come out of the injection alphabet — do not weaken gate 1 to accommodate it"
    );
  }
}

/// The alphabet reaches every trivia image the kind space has, and reaches nothing else.
///
/// The mechanical half of "the eight forms are exactly what `is_trivia` admits". Two directions:
///
/// - **soundness** — every token every form produces is trivia, and maps into the image block;
/// - **completeness** — the trivia images are a *suffix* of the image block (the kind space's
///   documented layout: twenty-six non-trivia images, then six trivia ones), and the alphabet
///   reaches every kind in that suffix.
///
/// The suffix formulation is what makes this survive a change rather than merely describe today. A
/// seventh trivia image, wherever it is inserted in the trivia block, is a kind at or after the
/// first alphabet-reached index that the alphabet does not reach — which is exactly what the loop
/// below rejects. A hand-written "the alphabet has eight entries" would have said nothing about it.
#[test]
fn the_alphabet_reaches_every_trivia_image_in_the_kind_space() {
  let mut reached_lexer_kinds: BTreeSet<LK> = BTreeSet::new();
  let mut reached_images: BTreeSet<u16> = BTreeSet::new();
  for (form, pad) in ALPHABET {
    let mut lexer = LosslessLexer::<'_, &str>::new(pad);
    let mut tokens = 0usize;
    while let Some(result) = lexer.lex() {
      let token = result.unwrap_or_else(|e| panic!("{form}: {pad:?} must lex: {e:?}"));
      assert!(
        token.is_trivia(),
        "{form}: {pad:?} lexes {:?}, which `is_trivia` does not admit",
        token.kind()
      );
      reached_lexer_kinds.insert(token.kind());
      reached_images.insert(token_kind(&token));
      tokens += 1;
    }
    assert!(tokens > 0, "{form}: {pad:?} lexes to nothing at all");
  }

  // Eight forms, eight *lexer* kinds: the alphabet is one form per ignorable token, so a form that
  // silently duplicated another's classification would show up as seven.
  assert_eq!(
    reached_lexer_kinds.len(),
    8,
    "the alphabet reaches {} lexer trivia kinds, not eight: {reached_lexer_kinds:?}",
    reached_lexer_kinds.len()
  );

  // …and six *images*, the three line terminators having been folded onto one.
  assert_eq!(
    reached_images.len(),
    6,
    "the alphabet reaches {} trivia images, not six: {:?}",
    reached_images.len(),
    reached_images
      .iter()
      .map(|raw| K::from_raw(*raw))
      .collect::<Vec<_>>()
  );

  let first_trivia_image = (0..K::IMAGE_BLOCK)
    .find(|i| reached_images.contains(&(*i as u16)))
    .expect("the alphabet must reach at least one image");
  for index in first_trivia_image..K::IMAGE_BLOCK {
    let kind = K::ALL[index];
    assert!(
      reached_images.contains(&kind.raw()),
      "{kind:?} sits inside the kind space's trivia suffix and no alphabet form produces it; the \
       injection sweep would never exercise it"
    );
  }
  for index in 0..first_trivia_image {
    let kind = K::ALL[index];
    assert!(
      !reached_images.contains(&kind.raw()),
      "{kind:?} is a non-trivia image and an alphabet form produces it; the trivia images are no \
       longer a suffix of the image block"
    );
  }
}

/// Task 16 Step 6's other half: gate 1's padded set is this gate's, widened to the invalid entries.
///
/// The alphabet, the boundary scan and the injection are the *same items* — `tests/support/`
/// `graphqlx_padding.rs` — so the only part of "exactly the one gate 2 derives" that a runtime
/// assertion can still add is the **entry** half, and it is the half that could drift on its own:
/// gate 1 pads every lexable entry, this gate pads the valid ones, and those two descriptions
/// coincide only while every valid entry lexes.
///
/// Three properties. No valid entry is unpaddable, so this gate skips nothing silently; gate 1's set
/// is this one plus the lexable invalid entries, counted; and the widening is non-empty, so the two
/// gates are not accidentally running the identical sweep twice under two names.
#[test]
fn the_padded_set_is_gate_ones_set_restricted_to_the_valid_half() {
  let valid: Vec<String> = valid_corpus().into_iter().map(|(name, _)| name).collect();
  for name in &valid {
    assert!(
      !UNPADDABLE.contains(&name.as_str()),
      "{name} is a valid entry and unpaddable; this gate would skip it and say nothing"
    );
    let src = std::fs::read_to_string(
      std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("corpusx")
        .join(name),
    )
    .unwrap();
    assert!(
      token_boundaries(&src).is_some(),
      "{name} is a valid entry the boundary scan cannot tile"
    );
  }

  let lexable = corpus_files()
    .iter()
    .filter(|p| {
      let src = std::fs::read_to_string(p).unwrap();
      token_boundaries(&src).is_some()
    })
    .count();
  assert_eq!(
    lexable,
    corpus_files().len() - UNPADDABLE.len(),
    "gate 1 pads every lexable entry, and the lexable count no longer agrees with UNPADDABLE"
  );
  assert!(
    lexable > valid.len(),
    "gate 1's padded set is not a strict widening of this gate's; the invalid half contributes \
     nothing and gate 1 is running this sweep a second time"
  );
}

/// The padding is real, and the shape comparison is not a self-comparison.
///
/// [`trivia_injection_preserves_the_verdict_and_the_shape`] compares two `Vec<K>`. Both halves of
/// that comparison have a way of going quietly inert — an [`inject`] that returned its input, or a
/// [`shape`] that answered the same thing for every tree — and either would leave the gate green
/// forever. So each is driven until it disagrees with itself.
#[test]
fn the_injection_and_the_shape_projection_are_both_live() {
  const SRC: &str = "type T{a:Int}";
  let boundaries = token_boundaries(SRC).expect("the fixture lexes");
  assert_eq!(
    boundaries,
    vec![0, 4, 5, 6, 7, 8, 9, 12, 13],
    "the boundary set is the token tiling of the source, offset 0 and the end included"
  );

  for (form, pad) in ALPHABET {
    let padded = inject(SRC, &boundaries, pad);
    assert_ne!(padded, SRC, "{form}: injection was a no-op");
    assert_eq!(
      padded.len(),
      SRC.len() + boundaries.len() * pad.len(),
      "{form}: injection did not reach every boundary"
    );
    // The leading boundary is the only position that can discriminate a first-atom skip — whatever
    // atom runs in interior position has already had the trivia crossed for it — so it is asserted
    // by itself rather than left to the length arithmetic.
    assert!(
      padded.starts_with(pad),
      "{form}: nothing was injected before the first token"
    );
    assert!(
      padded.ends_with(pad),
      "{form}: nothing was injected after the last token"
    );
  }

  assert_ne!(
    shape(&parse_str("type T{a:Int}")),
    shape(&parse_str("enum E{A}")),
    "the shape projection answers the same thing for two different trees"
  );
}

/// A junction at which an atom's answer decides something, named by the pair that forms it.
///
/// Five, and the first three are GraphQLx's own. The last two are the retro-wrap probes GraphQL also
/// has — `try_eat` is reached from the alias probe after a field name and the non-null probe after a
/// type, and its `skip_while` is the only thing that lets either see past trivia.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Junction {
  /// `set` or `map`, trivia, then `{` — the value retro-wrap probe. Without the skip the keyword
  /// stays an [`EnumValue`](K::EnumValue) and the braces become a syntax error.
  SetOrMapThenBrace,
  /// A type, trivia, then `=>` — the map-type discriminator inside `<…>`.
  TypeThenFatArrow,
  /// A type, trivia, then `>` — the set-type discriminator, the other side of the same decision.
  TypeThenRAngle,
  /// A name, trivia, then `:` — the alias probe, and the `where` predicate's two-token window.
  NameThenColon,
  /// A type, trivia, then `!` — the non-null probe.
  TypeThenBang,
}

impl Junction {
  /// This junction's position in [`ALL_JUNCTIONS`].
  ///
  /// A **total** match with no wildcard, which is the only thing that makes [`ALL_JUNCTIONS`] safe
  /// to hand-write: a sixth variant stops this compiling, where it would otherwise be a silent
  /// omission from a list nothing else checks.
  const fn index(self) -> usize {
    match self {
      Self::SetOrMapThenBrace => 0,
      Self::TypeThenFatArrow => 1,
      Self::TypeThenRAngle => 2,
      Self::NameThenColon => 3,
      Self::TypeThenBang => 4,
    }
  }
}

/// Every [`Junction`], so the complement below is over the whole set rather than over whatever the
/// corpus happened to produce. Kept honest by [`Junction::index`].
const ALL_JUNCTIONS: &[Junction] = &[
  Junction::SetOrMapThenBrace,
  Junction::TypeThenFatArrow,
  Junction::TypeThenRAngle,
  Junction::NameThenColon,
  Junction::TypeThenBang,
];

/// Which kind-deciding junctions `tests/corpusx/` already writes with trivia in them. **Measured,
/// and the answer is not Phase A's.**
///
/// Phase A found its GraphQL corpus padding *neither* of the two junctions it names, which is what
/// let it say the injection sweep was the corpus's only witness for `try_eat`'s skip. GraphQLx pads
/// two of five, and for a reason worth keeping: `set { 1, 2 }` and `<Int => String>` are how the two
/// GraphQLx-only forms are **idiomatically written**, so the corpus that was built to cover the
/// grammar covers those two junctions as a side effect —
///
/// - `SetOrMapThenBrace` by `valid_x_set_and_map_values.graphqlx` and
///   `valid_x_map_entry_compound_key.graphqlx`;
/// - `TypeThenFatArrow` by five entries, `valid_x_set_and_map_types.graphqlx` among them.
///
/// The three that remain — `>` closing a set type, `:` after a name, `!` after a type — are still
/// written tight everywhere, so the injection sweep is still their only corpus-scale fixture. The
/// pin is a set rather than an emptiness check precisely because the interesting movement is in both
/// directions: a junction entering this set stops needing the sweep, and one leaving it starts.
const JUNCTIONS_THE_COMPACT_CORPUS_PADS: &[Junction] =
  &[Junction::SetOrMapThenBrace, Junction::TypeThenFatArrow];

/// Every kind-deciding junction `src` writes with trivia in it.
///
/// Tracks the previous **significant** token, so `set\n{` is a hit and `type T {` is not — the
/// distinction the crude "is there trivia before a `{`" scan cannot make, and the one that decides
/// whether the corpus covers the retro-wrap probe.
fn padded_junctions(src: &str) -> BTreeSet<Junction> {
  let mut found = BTreeSet::new();
  let mut lexer = LosslessLexer::<'_, &str>::new(src);
  let mut previous: Option<LK> = None;
  let mut previous_text: &str = "";
  let mut trivia_since_previous = false;
  while let Some(result) = lexer.lex() {
    let token = result.expect("a valid corpus entry must lex");
    if token.is_trivia() {
      trivia_since_previous = true;
      continue;
    }
    let span = lexer.span();
    let text = &src[span.start()..span.end()];
    if trivia_since_previous {
      match token.kind() {
        LK::LBrace
          if previous == Some(LK::Identifier) && matches!(previous_text, "set" | "map") =>
        {
          found.insert(Junction::SetOrMapThenBrace);
        }
        LK::FatArrow => {
          found.insert(Junction::TypeThenFatArrow);
        }
        LK::RAngle => {
          found.insert(Junction::TypeThenRAngle);
        }
        LK::Colon => {
          found.insert(Junction::NameThenColon);
        }
        LK::Bang => {
          found.insert(Junction::TypeThenBang);
        }
        _ => {}
      }
    }
    previous = Some(token.kind());
    previous_text = text;
    trivia_since_previous = false;
  }
  found
}

/// Which of the five junctions the compact corpus already covers, and which the sweep is the only
/// witness for.
///
/// This is the measurement behind this gate's claim to discriminate, and GraphQLx's version matters
/// more than GraphQL's because the consequence is worse. GraphQL's unpadded junctions cost a
/// *rejection* when an atom stops crossing trivia; two of GraphQLx's cost a **different tree that
/// still parses** — `set` not followed by `{` is an `EnumValue`, `<T>` without a `=>` is a
/// `SetType` — which the verdict half of this gate cannot see at all and only the shape half can.
///
/// So which junctions the corpus already reaches is asserted rather than assumed, and the answer
/// turned out to be two of five rather than Phase A's zero of two. See
/// [`JUNCTIONS_THE_COMPACT_CORPUS_PADS`]. The complement is the load-bearing half and is asserted
/// non-empty: if the corpus ever grows to cover all five, this gate stops being the sole fixture for
/// any of them and the sentence in the module docs that says otherwise has to go.
#[test]
fn the_compact_corpus_cannot_discriminate_the_kind_deciding_junctions() {
  for (position, junction) in ALL_JUNCTIONS.iter().enumerate() {
    assert_eq!(
      junction.index(),
      position,
      "{junction:?} is listed at {position} in ALL_JUNCTIONS and indexes {}",
      junction.index()
    );
  }

  let mut padded_by_the_corpus: BTreeSet<Junction> = BTreeSet::new();
  for (_name, src) in valid_corpus() {
    padded_by_the_corpus.extend(padded_junctions(&src));
  }
  let measured: Vec<Junction> = padded_by_the_corpus.iter().copied().collect();
  assert_eq!(
    measured,
    JUNCTIONS_THE_COMPACT_CORPUS_PADS.to_vec(),
    "the set of kind-deciding junctions `tests/corpusx/` already pads has moved; pin the change \
     on purpose, because every junction that enters this set stops needing the injection sweep \
     and every junction that leaves it starts needing one"
  );

  let only_the_sweep_reaches: Vec<Junction> = ALL_JUNCTIONS
    .iter()
    .copied()
    .filter(|j| !padded_by_the_corpus.contains(j))
    .collect();
  assert_eq!(
    only_the_sweep_reaches,
    vec![
      Junction::TypeThenRAngle,
      Junction::NameThenColon,
      Junction::TypeThenBang
    ],
    "the junctions only the injection sweep reaches have moved"
  );
  assert!(
    !only_the_sweep_reaches.is_empty(),
    "the compact corpus now pads every kind-deciding junction, so this gate has stopped being the \
     only witness for any of them"
  );

  // …and the padded form pads all five, which is what makes the sweep the witness for the ones
  // the corpus leaves out. Each discriminator is driven separately: a scan that only ever
  // produced `NameThenColon` would satisfy a single `is_empty()` check and say nothing about the
  // three GraphQLx ones.
  for (probe, expected) in [
    ("set{1}", Junction::SetOrMapThenBrace),
    ("map<Int=>Int>", Junction::TypeThenFatArrow),
    ("set<Int>", Junction::TypeThenRAngle),
    ("alias:f", Junction::NameThenColon),
    ("Int!", Junction::TypeThenBang),
  ] {
    assert_eq!(
      padded_junctions(probe),
      BTreeSet::new(),
      "{probe:?} is written tight and must have no padded junction"
    );
    let padded = inject(
      probe,
      &token_boundaries(probe).expect("the probe lexes"),
      " ",
    );
    assert!(
      padded_junctions(&padded).contains(&expected),
      "{probe:?} padded to {padded:?} must expose its {expected:?} junction; it exposed {:?}",
      padded_junctions(&padded)
    );
  }
}

/// Why four of the eight alphabet entries are the only source of their form.
///
/// The corpus is ordinary hand-written GraphQLx: spaces, newlines, commas and comments, and nothing
/// else. So a carriage return, a CRLF, a tab and a BOM reach the lossless parser through **this
/// gate's injection or not at all**, as far as the corpus is concerned — which is the measured
/// reason those four forms are in the alphabet, and it is not the reason the plan gave (a wrong
/// `Bom` arm in the token mapper leaves this gate green; `lossless_x_kind_map.rs` is what reds).
///
/// Asserted rather than stated, so that adding a CRLF entry to the corpus tells someone this gate's
/// alphabet has stopped being the only door.
#[test]
fn the_corpus_supplies_no_carriage_return_tab_or_bom() {
  for (name, src) in valid_corpus() {
    assert!(!src.contains('\r'), "{name} now carries a carriage return");
    assert!(!src.contains('\t'), "{name} now carries a tab");
    assert!(!src.contains('\u{FEFF}'), "{name} now carries a BOM");
  }

  // The positive control: the assertions above are three `contains` calls over strings that could
  // be empty for any number of reasons. These are the same calls, answering the other way.
  let boundaries = token_boundaries("{a}").expect("the probe lexes");
  for pad in ["\r", "\t", "\u{FEFF}"] {
    let padded = inject("{a}", &boundaries, pad);
    assert!(padded.contains(pad), "the probe did not inject {pad:?}");
  }
}

/// The two roots [`parse_str`] cannot reach are exactly the two named, and they are roots.
///
/// The coverage assertion drives three entry points to reach every node kind. That is only honest if
/// the reason `parse_str` alone falls short is structural — a parse has one root — rather than a
/// corpus gap somebody could have closed. So the claim is measured: over the whole valid corpus,
/// `parse_str` opens every node kind except those two, and each of those two is opened by its own
/// entry over the same bytes.
#[test]
fn parse_str_reaches_every_node_kind_but_the_two_alternate_roots() {
  let mut seen: BTreeSet<K> = BTreeSet::new();
  let mut sdl_root = false;
  let mut executable_root = false;
  for (_name, src) in valid_corpus() {
    seen.extend(shape(&parse_str(&src)).into_iter().map(|(_, kind)| kind));
    sdl_root |= shape(&parse_type_system_document(&src))
      .iter()
      .any(|(_, kind)| *kind == K::TypeSystemDocument);
    executable_root |= shape(&parse_executable_document(&src))
      .iter()
      .any(|(_, kind)| *kind == K::ExecutableDocument);
  }

  let missing: Vec<K> = node_kinds()
    .into_iter()
    .filter(|k| !seen.contains(k))
    .collect();
  assert_eq!(
    missing, UNREACHABLE_FROM_PARSE_STR,
    "`parse_str` over the valid corpus reaches all but {UNREACHABLE_FROM_PARSE_STR:?}; it now \
     misses {missing:?} instead"
  );
  assert!(sdl_root, "the SDL-only root never opened its own node");
  assert!(
    executable_root,
    "the executable-only root never opened its own node"
  );
}

/// Every repetition in [`REPETITIONS_THAT_MUST_CONTINUE`] is written with two members somewhere.
///
/// The hit counter's blind spot, closed. See that constant for the mutation that measured it: a
/// loop whose body runs exactly once reports full coverage while its continuation test is dead, and
/// the padded sweep — which pads *every* boundary, including the one between the two tokens the
/// continuation test reads — can only witness a broken continuation on a container that was going to
/// take a second turn.
///
/// Structural rather than arithmetic: it counts children of one parent node, so an empty container
/// elsewhere in the corpus cannot pay for a full one and hide the gap.
#[test]
fn every_looping_production_is_written_with_two_members_somewhere() {
  let mut widest: Vec<usize> = vec![0; REPETITIONS_THAT_MUST_CONTINUE.len()];
  for (_name, src) in valid_corpus() {
    for node in parse_str(&src).syntax().descendants() {
      for (slot, (container, member)) in REPETITIONS_THAT_MUST_CONTINUE.iter().enumerate() {
        if node.kind() != *container {
          continue;
        }
        let members = node
          .children()
          .filter(|child| child.kind() == *member)
          .count();
        widest[slot] = widest[slot].max(members);
      }
    }
  }

  let thin: Vec<String> = REPETITIONS_THAT_MUST_CONTINUE
    .iter()
    .zip(&widest)
    .filter(|(_, members)| **members < 2)
    .map(|((container, member), members)| {
      format!("{container:?} holds at most {members} {member:?}")
    })
    .collect();
  assert!(
    thin.is_empty(),
    "the corpus never writes a second member for: {thin:?} — the loop body runs once, so the \
     continuation test that decides whether to take another turn has no witness and the hit \
     counter reports it as covered"
  );
}

/// The must-be-covered set is every node kind, and the derivation that produces it is sound.
///
/// [`node_kinds`] is a subtraction, and a subtraction is only as good as the two things it removes.
/// So both are pinned: the images really are a prefix of length [`K::IMAGE_BLOCK`], the bookkeeping
/// triple really is the tail, and the three halves partition the space with nothing counted twice.
#[test]
fn the_covered_set_is_every_node_kind() {
  let kinds = node_kinds();
  assert_eq!(
    kinds.len(),
    78,
    "GraphQLx declares 78 node kinds; the set this gate covers is {kinds:?}"
  );
  assert_eq!(
    kinds.len() + K::IMAGE_BLOCK + 3,
    K::ALL.len(),
    "the images, the node kinds and the bookkeeping triple must partition the kind space"
  );

  // The bookkeeping triple is the tail, which is what lets `node_kinds` take a prefix skip and a
  // three-element filter rather than a hand-written exclusion list. `kinds.rs` states this as a
  // layout rule ("nothing may be appended after Root"); this is the assertion of it.
  assert_eq!(
    &K::ALL[K::ALL.len() - 3..],
    &[
      <K as KindSpace>::ERROR,
      <K as KindSpace>::GAP,
      <K as KindSpace>::ROOT
    ],
    "the bookkeeping triple is no longer the last three kinds"
  );

  // No image and no bookkeeping tile is in the covered set…
  for image in &K::ALL[..K::IMAGE_BLOCK] {
    assert!(
      !kinds.contains(image),
      "{image:?} is a token image and is also covered"
    );
  }
  for tile in [
    <K as KindSpace>::ERROR,
    <K as KindSpace>::GAP,
    <K as KindSpace>::ROOT,
  ] {
    assert!(
      !kinds.contains(&tile),
      "{tile:?} is a bookkeeping tile and is also covered"
    );
  }
  // …and the covered set is contiguous, so nothing fell out of the middle.
  assert_eq!(
    kinds,
    K::ALL[K::IMAGE_BLOCK..K::ALL.len() - 3].to_vec(),
    "the covered set is not the node block verbatim"
  );
}
