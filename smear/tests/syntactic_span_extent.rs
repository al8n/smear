#![cfg(all(feature = "graphql", feature = "parser", not(miri)))]

//! The syntactic span-extent gate for GraphQL: trivia injection, with a per-node-type
//! discrimination counter.
//!
//! # `not(miri)`, and what the Miri matrix gives up for it
//!
//! This target is the reason `.github/workflows/miri.yml` had never produced a verdict. On CI run
//! 31318425279 the `x86_64-unknown-linux-gnu` Tree Borrows cell reached
//! [`trivia_injection_leaves_every_span_on_its_own_tokens`] at 14:47:15Z and was still inside it
//! when the job was killed at 20:27Z — **five hours forty minutes in one test**, after the whole
//! lib suite had taken 10m16s and `tests/oracle.rs` 8m01s. That was not one unlucky run: five
//! consecutive runs on that branch were cancelled by the next push, and of ten cells the only two
//! that ever concluded were the i686 pair, which run `--lib` alone and so never build this. The
//! header in `ci/miri_tb.sh` predicted exactly this before it was measured and declined to cut on
//! an estimate; the measurement is now on the record, so the cut is no longer one.
//!
//! The cost is small **for the question Miri answers**, and that is the whole argument. Miri
//! decides whether an execution path has undefined behaviour. The sweep below varies the INPUT —
//! 56 corpus entries times eight ignorable forms, 504 parses — over paths the lib unit tests,
//! `oracle.rs` and `tokora_conformance.rs` already interpret in the same cell, and it spends most
//! of that time in `format!("{:#?}")` and a string walk that are this file's own safe code, with
//! nothing in them for Miri to find. What is given up is not a UB path; it is 504 re-traversals of
//! one.
//!
//! Nothing else changes. `cargo test` runs this in full on every push in
//! `.github/workflows/ci.yml`, which is where a span-extent regression is meant to be caught.
//! `ci/miri_scope.py` reads this attribute, prints the target in the Miri cell's NOT COVERED list
//! with this reason, and fails the cell if it ever runs there anyway — so the omission is audited
//! rather than silent, in both directions.
//!
//! The header names `parser` as well as the dialect because the file reads
//! `smear::parser::graphql`, and `parser` is a feature a consumer can turn off. It said `graphql`
//! alone until #136 and compiled regardless: the self dev-dependency pulled the crate's defaults
//! into every test target, so `parser` was on in every build there was and a header that omitted
//! it had nothing to be wrong about.
//!
//! The invariant: **a composite node's span is the extent of the tokens it contains.** It opens at
//! the start of its first token and closes at the end of its last one, and no ignorable byte on
//! either side belongs to it.
//!
//! # Why the syntactic suite had no gate like this, and what it cost
//!
//! On compact input every span rule coincides. Closing a node at the end of its last token and
//! closing it at wherever the parser's lookahead cursor landed are the same number until there is
//! trivia between the two, so a corpus of tightly written GraphQL — which is what a parser suite
//! naturally accumulates — cannot distinguish a span from a lookahead artifact. The lossless side
//! learned this and answered it with `lossless_trivia.rs`; the syntactic side had no counterpart,
//! and issue #68 is what grew in the gap: forty-four node types across the two dialects were
//! carrying spans that ran into the whitespace after them, and every gate in the repository was
//! green.
//!
//! [`the_issue_68_witness`] is that measurement, pinned to the byte.
//!
//! # The alphabet, the corpus, and what pins them
//!
//! Eight ignorable forms — [`extent::ALPHABET`] — at every token boundary of every `valid_` entry
//! in `tests/corpus/`, the same corpus and the same eight forms `lossless_trivia.rs` pads. That
//! shared corpus is deliberate and load-bearing in one direction: **its reach over the grammar is
//! measured there, not here.** `lossless_trivia.rs`'s coverage counter asserts these entries open
//! every node kind in the kind space, so a production nobody exercises is that gate's failure. Re-
//! deriving the same claim here would make two gates fail for one defect while neither explained
//! it. What this gate owns instead is the *syntactic* side's own reach, pinned two ways below: the
//! padded owner set must equal the compact one, and it must not shrink below what it measures
//! today.
//!
//! # The liveness floor
//!
//! A trivia-injection sweep that silently injects into nothing is the same defect one level up: it
//! reports success over spans that never sat anywhere the rules disagree. So each span is
//! classified by [`extent::discriminating`] — is there an ignorable byte immediately after its end,
//! or immediately before its start? — and
//! [`trivia_injection_leaves_every_span_on_its_own_tokens`] asserts **every node type it observed
//! was observed at least once at such a site**. A form that stopped injecting, a boundary scan that
//! returned nothing, or a corpus that lost its interior junctions all turn that assertion red
//! rather than leaving a wall of vacuous passes.
//!
//! # Two positive controls, because a checker that cannot fail proves nothing
//!
//! [`the_span_walk_reads_the_debug_rendering_this_gate_assumes`] pins the one assumption the walk
//! makes about `#[derive(Debug)]`'s output, and [`the_checker_can_answer_no`] feeds
//! [`extent::check`] a span that is wrong in each of the four ways and requires it to say so. The
//! sweep below is only as good as those two.
//!
//! # The other half of the corpus, and the emitter it needs — issue #75
//!
//! Everything above runs the `valid_` entries, which parse to completion, so **no recovery path is
//! exercised anywhere in it**. The `invalid_` entries are the other half of the same corpus and the
//! gate was silent about them, which is what made #75 — "does the span wrapper shorten a recovered
//! delimited node?" — unanswerable by running the suite.
//!
//! Answering it needs an emitter that recovers. The shipped syntactic entry points pin
//! [`Fatal`](tokora::emitter::Fatal), under which the first close-miss becomes an `Err` and no tree
//! exists to measure; but every production in this dialect is generic over its context, so a
//! recording emitter is one type alias away and is what a consumer wanting partial trees will
//! reach for. [`RecoveringCtx`] is that alias, and
//! [`recovered_spans_are_token_extents_over_the_invalid_corpus`] runs the same four-part check and
//! the same liveness floor over the `invalid_` half under it.
//!
//! [`a_recovered_group_ends_at_its_last_token_not_at_the_recovery_cursor`] is #75's own
//! measurement. tokora's delimited shapes span a recovered construct with
//! [`span_since`](tokora::InputRef::span_since) — cursor to cursor — so a group closed by
//! recovery ends at the **live lookahead cursor**, which at a wrong closer is that token's start.
//! `token_spanned` overrides it with the token extent, and the two differ by exactly the trivia in
//! between. The witness pins which of the two readings is right *in the lexer's terms*: the
//! recovery cursor is an offset the lexer reports as a token **start** and not a token **end**, so
//! the cursor reading is `end-not-token-end` — the #68 defect — and the checker is made to say so.

use std::{collections::BTreeSet, path::PathBuf};

use smear::{
  lexer::graphql::syntactic::SyntacticLexer,
  parser::graphql::{
    GraphQL,
    ast::Document,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, document},
  },
};
use tokora::{
  Lexer as _, Parse as _, Parser, ParserContext, SimpleSpan, cache::DefaultCache, emitter::Verbose,
};

#[path = "support/span_extent.rs"]
mod extent;

use extent::{ALPHABET, Extents, FoundSpan, Violation, check, discriminating, inject, spans_of};

/// The smallest number of distinct span-carrying node types this corpus reaches.
///
/// A floor, not a pin: adding a corpus entry that reaches a sixty-ninth node type is not a
/// regression, and losing one is. The value is the measurement, taken the day this gate was
/// written.
const OWNER_FLOOR: usize = 68;

/// The smallest number of `invalid_` entries the shared corpus is allowed to hold.
///
/// The measurement on the day the invalid sweep was written, as a floor. Deleting an invalid entry
/// shrinks the only corpus that reaches a recovery path.
const INVALID_ENTRY_FLOOR: usize = 31;

/// The smallest number of `invalid_` entries the **lexer** accepts.
///
/// The rest are the `invalid_lex_*` class, which has no token extents to be measured against. A
/// grammar-level invalid entry turning into a lexer-level one would silently shrink the sweep.
const LEXABLE_INVALID_FLOOR: usize = 27;

/// The smallest number of invalid variants (compact plus padded) that must **recover** into a tree.
///
/// The liveness floor for the recovery dimension: with no recovered parse the invalid sweep checks
/// nothing at all and every assertion in it passes vacuously.
const RECOVERED_VARIANT_FLOOR: usize = 45;

/// The smallest number of distinct span-carrying node types the recovered invalid parses reach.
const RECOVERED_OWNER_FLOOR: usize = 8;

/// Parses one source through the syntactic GraphQL document root, fail-fast — the shipped wiring.
fn parse(src: &str) -> Result<Document<&str>, GraphqlErrors<&str>> {
  Parser::with_parser::<'_, GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
    document,
  )
  .parse_str(src)
}

/// The recording emitter the invalid-corpus sweep runs under: the same one the lossless tower
/// pins, over this dialect's syntactic error container.
///
/// `Verbose`'s third parameter is the grammar brand, and `Emitter<'inp, L, Lang>` is implemented
/// only where it matches — a bare `Verbose::default()` leaves it at `()` and the context then
/// fails to be a `ParseContext<…, GraphQL>`.
type RecoveringEmitter<'inp> = Verbose<GraphqlErrors<&'inp str>, SimpleSpan, GraphQL>;

/// The context [`parse_recovering`] drives: this dialect's syntactic productions with the
/// close-miss diagnostics *recorded* rather than raised.
///
/// Not a shipped configuration — the entry points pin `Fatal` — but a reachable one, because the
/// productions are public and generic over `Ctx`. It is also the only configuration in which a
/// recovery span exists at all, so it is what #75 has to be answered in.
type RecoveringCtx<'inp> = ParserContext<
  'inp,
  GraphqlLexer<'inp, str>,
  RecoveringEmitter<'inp>,
  DefaultCache<'inp, GraphqlLexer<'inp, str>>,
  GraphQL,
>;

/// Parses one source through the same document root with a **recovering** emitter.
///
/// An `Ok` here over a source [`parse`] rejects is the definition of a recovered parse: the two
/// wirings differ in nothing but whether an emitted diagnostic becomes an `Err`.
fn parse_recovering(src: &str) -> Result<Document<&str>, GraphqlErrors<&str>> {
  Parser::with_parser_and_context::<
    '_,
    GraphqlLexer<'_, str>,
    Document<&str>,
    RecoveringCtx<'_>,
    _,
    GraphQL,
  >(document, ParserContext::of(RecoveringEmitter::default()))
  .parse_str(src)
}

/// Does the **lexer** accept `src` end to end?
///
/// The `invalid_lex_*` class does not, and for those there are no token extents to measure a span
/// against — the yardstick itself is missing. The sweep classifies them out rather than guessing;
/// `lossless_parity.rs` is where that class is held.
fn lexes(src: &str) -> bool {
  let mut lexer = SyntacticLexer::<str>::new(src);
  while let Some(result) = lexer.lex() {
    if result.is_err() {
      return false;
    }
  }
  true
}

/// Every token's start and end, from the **lexer**.
///
/// Never from the tree under test: reading the extents off the parse would make the yardstick a
/// function of the artifact being measured, and a production that lost a token would quietly stop
/// being checked there.
fn extents(src: &str) -> Extents {
  let mut lexer = SyntacticLexer::<str>::new(src);
  let mut out = Extents::default();
  while let Some(result) = lexer.lex() {
    result.unwrap_or_else(|e| panic!("{src:?} must lex: {e:?}"));
    out.starts.insert(lexer.span().start());
    out.ends.insert(lexer.span().end());
  }
  out
}

/// Every token boundary in `src`: offset 0, then the end of each token.
fn boundaries(src: &str) -> Vec<usize> {
  let mut lexer = SyntacticLexer::<str>::new(src);
  let mut out = vec![0usize];
  while let Some(result) = lexer.lex() {
    result.unwrap_or_else(|e| panic!("a corpus entry the sweep pads must lex: {e:?}"));
    out.push(lexer.span().end());
  }
  out.dedup();
  out
}

/// One half of the shared GraphQL corpus, selected by file-name prefix, in a deterministic order.
fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .filter(|path| {
      path
        .file_name()
        .is_some_and(|name| name.to_string_lossy().starts_with(prefix))
    })
    .collect();
  files.sort();
  files
    .into_iter()
    .map(|path| {
      let name = path
        .file_name()
        .expect("a corpus entry has a file name")
        .to_string_lossy()
        .to_string();
      let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", path.display()));
      (name, src)
    })
    .collect()
}

/// The `valid_` half of the shared GraphQL corpus.
fn valid_corpus() -> Vec<(String, String)> {
  corpus("valid_")
}

/// The `invalid_` half — the entries the shipped parser rejects and the sweep above never touches.
fn invalid_corpus() -> Vec<(String, String)> {
  corpus("invalid_")
}

/// The one assumption the span walk makes about `#[derive(Debug)]`, measured rather than trusted.
///
/// If `SimpleSpan` ever renders as a tuple struct, or the pretty printer stops nesting a field's
/// value under its owner, the walk would find no spans at all — and a sweep that checks zero spans
/// is the most convincing green wall there is. So the shape is pinned twice: against a rendering
/// this test builds by hand, and against a real parse whose spans are known.
#[test]
fn the_span_walk_reads_the_debug_rendering_this_gate_assumes() {
  assert_eq!(
    format!("{:#?}", SimpleSpan::new(3usize, 7usize)),
    "SimpleSpan {\n    start: 3,\n    end: 7,\n}",
    "`SimpleSpan`'s pretty `Debug` is the shape `spans_of` walks; it changed"
  );

  let doc = parse("type T{f:Int}").expect("the control source parses");
  let found = spans_of(&format!("{doc:#?}"));
  assert!(
    found.len() >= 8,
    "the walk found only {} spans in a document with a type, a field and a named type — it is \
     reading the rendering wrong",
    found.len()
  );

  // Owner attribution and nesting, on nodes whose extents are unambiguous in compact source.
  let named = found
    .iter()
    .find(|span| span.owner == "NamedType")
    .expect("the walk must attribute the `Int` node to `NamedType`");
  assert_eq!((named.start, named.end), (9, 12), "`Int` is at 9..12");
  let parent = named
    .parent
    .map(|index| &found[index])
    .expect("`NamedType` is nested inside a field, so the walk must give it a parent");
  assert!(
    parent.start <= named.start && named.end <= parent.end,
    "the walk's parent link must be an enclosing node, and {}..{} is not",
    parent.start,
    parent.end
  );
}

/// [`extent::check`] must be able to say "no", or every green run below means nothing.
///
/// One span wrong in each of the four ways the invariant names, over a two-token source whose
/// extents are known: `a b` lexes as `a@0..1` and `b@2..3`, so 1 is a token end and not a start, 2
/// is a start and not an end, and 4 is neither.
#[test]
fn the_checker_can_answer_no() {
  let src = "a b";
  let ext = extents(src);
  assert_eq!(ext.starts, BTreeSet::from([0, 2]));
  assert_eq!(ext.ends, BTreeSet::from([1, 3]));

  let rules = |spans: &[FoundSpan]| -> Vec<&'static str> {
    check(src, spans, &ext)
      .iter()
      .map(|violation: &Violation| violation.rule)
      .collect()
  };

  // The control: the honest span of the whole thing, and the honest span of its first token.
  let good = vec![
    FoundSpan {
      owner: "Whole".into(),
      parent: None,
      start: 0,
      end: 3,
    },
    FoundSpan {
      owner: "First".into(),
      parent: Some(0),
      start: 0,
      end: 1,
    },
  ];
  assert!(
    rules(&good).is_empty(),
    "the checker rejected a correct tree: {:?}",
    check(src, &good, &ext)
  );

  // #68 proper: a node closed at the next token's start.
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Spilled".into(),
      parent: None,
      start: 0,
      end: 2
    }]),
    ["end-not-token-end"]
  );
  // #68's other half: a node opened at the previous token's end.
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Early".into(),
      parent: None,
      start: 1,
      end: 3
    }]),
    ["start-not-token-start"]
  );
  // A zero-width position marker parked in the whitespace between the tokens.
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Absent".into(),
      parent: None,
      start: 4,
      end: 4
    }]),
    ["empty-off-boundary"]
  );
  // A child that outruns its parent.
  assert_eq!(
    rules(&[
      FoundSpan {
        owner: "Parent".into(),
        parent: None,
        start: 0,
        end: 1
      },
      FoundSpan {
        owner: "Child".into(),
        parent: Some(0),
        start: 2,
        end: 3
      },
    ]),
    ["not-contained"]
  );
  // An inverted span reports once and stops, rather than cascading into the endpoint rules.
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Backwards".into(),
      parent: None,
      start: 3,
      end: 1
    }]),
    ["inverted"]
  );

  // And the liveness classifier must also be able to say "no": in `a b` the first token's span is
  // followed by trivia and so is discriminating, while the whole-source span is flanked by nothing.
  assert!(discriminating(&good[1], &ext, src.len()));
  assert!(!discriminating(&good[0], &ext, src.len()));

  // The tokenless carve-out, both halves. A comment-only source has no token boundaries at all, so
  // the empty document node the recovering parse produces there can only sit on an input edge —
  // both of which are accepted. Anywhere else in the same source is still named, so the carve-out
  // cannot be mistaken for "zero-width spans are unchecked".
  let bare = "# c\n";
  let bare_ext = extents(bare);
  assert!(
    bare_ext.starts.is_empty() && bare_ext.ends.is_empty(),
    "a comment-only source must lex to no tokens for this control to be about anything"
  );
  let at = |offset: usize| -> Vec<&'static str> {
    check(
      bare,
      &[FoundSpan {
        owner: "Empty".into(),
        parent: None,
        start: offset,
        end: offset,
      }],
      &bare_ext,
    )
    .iter()
    .map(|violation: &Violation| violation.rule)
    .collect()
  };
  assert!(at(0).is_empty(), "offset 0 is an input edge");
  assert!(at(bare.len()).is_empty(), "the end is the other edge");
  assert_eq!(
    at(2),
    ["empty-off-boundary"],
    "an interior offset is still inside the trivia, tokens or no tokens"
  );
  // And the carve-out is scoped to the tokenless case: with a token in the source, offset 0 in
  // front of a leading run of trivia is named exactly as before.
  let leading = extents(" a");
  assert_eq!(
    check(
      " a",
      &[FoundSpan {
        owner: "Empty".into(),
        parent: None,
        start: 0,
        end: 0
      }],
      &leading
    )
    .iter()
    .map(|violation: &Violation| violation.rule)
    .collect::<Vec<_>>(),
    ["empty-off-boundary"],
  );
}

/// The gate proper: every span in every padded parse is the extent of its own tokens, and every
/// node type reached was reached somewhere the two span rules disagree.
#[test]
fn trivia_injection_leaves_every_span_on_its_own_tokens() {
  let entries = valid_corpus();
  assert!(
    entries.len() >= 20,
    "only {} valid corpus entries; the sweep is too thin to mean anything",
    entries.len()
  );

  let mut padded_parses = 0usize;
  let mut injected_bytes = 0usize;
  let mut violations: Vec<(String, String, Violation)> = Vec::new();
  let mut compact_owners: BTreeSet<String> = BTreeSet::new();
  let mut padded_owners: BTreeSet<String> = BTreeSet::new();
  let mut discriminated: BTreeSet<String> = BTreeSet::new();
  let mut discriminating_spans = 0usize;
  let mut total_spans = 0usize;

  for (name, src) in &entries {
    let marks = boundaries(src);
    assert!(
      marks.len() >= 3,
      "{name}: {} token boundaries — a one-token entry cannot exercise an interior junction",
      marks.len()
    );

    // The compact control. It must pass — a span that is wrong even without injection is a defect
    // this gate should still name — and its owner set is the yardstick the padded set is held to.
    let compact = parse(src).unwrap_or_else(|e| {
      panic!(
        "{name}: the unpadded entry does not parse — a corpus fault, not an injection one: {e:?}"
      )
    });
    let compact_spans = spans_of(&format!("{compact:#?}"));
    let compact_extents = extents(src);
    for violation in check(src, &compact_spans, &compact_extents) {
      violations.push((name.clone(), "compact".to_string(), violation));
    }
    compact_owners.extend(extent::owners(&compact_spans));

    for (form, pad) in ALPHABET {
      let padded_src = inject(src, &marks, pad);
      assert_eq!(
        padded_src.len(),
        src.len() + marks.len() * pad.len(),
        "{name} padded with {form}: the injection did not land at every boundary"
      );
      injected_bytes += marks.len() * pad.len();

      let padded = parse(&padded_src).unwrap_or_else(|e| {
        panic!(
          "{name} padded with {form}: the padded entry does not parse, so some decision point \
           looked at the head without committing the trivia in front of it: {e:?}"
        )
      });
      padded_parses += 1;

      let spans = spans_of(&format!("{padded:#?}"));
      assert!(
        !spans.is_empty(),
        "{name} padded with {form}: the walk found no spans at all, so this variant was checked \
         for nothing"
      );
      let padded_extents = extents(&padded_src);
      for violation in check(&padded_src, &spans, &padded_extents) {
        violations.push((name.clone(), (*form).to_string(), violation));
      }
      padded_owners.extend(extent::owners(&spans));

      total_spans += spans.len();
      for span in &spans {
        if discriminating(span, &padded_extents, padded_src.len()) {
          discriminating_spans += 1;
          discriminated.insert(span.owner.clone());
        }
      }
    }
  }

  assert_eq!(
    padded_parses,
    entries.len() * ALPHABET.len(),
    "the sweep did not run every form over every entry"
  );
  assert!(
    injected_bytes > 0,
    "the sweep injected nothing, so every check above was made against the compact bytes"
  );

  if !violations.is_empty() {
    let mut report: Vec<String> = violations
      .iter()
      .map(|(name, form, violation)| {
        format!(
          "  {name} [{form}] {} — {} — {}",
          violation.owner, violation.rule, violation.detail
        )
      })
      .collect();
    report.sort();
    report.dedup();
    let owners: BTreeSet<&str> = violations
      .iter()
      .map(|(_, _, violation)| violation.owner.as_str())
      .collect();
    panic!(
      "{} span-extent violations over {} node types — their spans are lookahead positions, not \
       token extents:\n{}\nnode types: {owners:?}",
      violations.len(),
      owners.len(),
      report.join("\n")
    );
  }

  // Reach, pinned two ways. Padding may not lose a node type — a form that changed the tree would
  // show up here even if every surviving span were tight — and the set may not silently shrink
  // below what it measures today.
  assert_eq!(
    padded_owners, compact_owners,
    "the padded sweep and the compact corpus disagree about which node types exist"
  );
  assert!(
    padded_owners.len() >= OWNER_FLOOR,
    "the sweep reached {} span-carrying node types, below the floor of {OWNER_FLOOR}: {padded_owners:?}",
    padded_owners.len()
  );

  // The liveness floor. Every node type must have been observed at least once where the two span
  // rules give different answers, or this gate is asserting about it without testing it.
  let undiscriminated: Vec<&String> = padded_owners.difference(&discriminated).collect();
  assert!(
    undiscriminated.is_empty(),
    "{} of the {} node types were never observed next to injected trivia, so nothing about their \
     spans was actually at stake: {undiscriminated:?}",
    undiscriminated.len(),
    padded_owners.len()
  );
  assert!(
    discriminating_spans * 2 >= total_spans,
    "only {discriminating_spans} of {total_spans} spans sat at a junction where the two rules \
     differ; padding at every boundary should put most of them there, so the injection has \
     stopped reaching the interior"
  );

  println!(
    "gate: {} entries x {} forms = {padded_parses} padded parses, {total_spans} spans, \
     {discriminating_spans} at a discriminating junction, {} node types",
    entries.len(),
    ALPHABET.len(),
    padded_owners.len()
  );
}

/// The `invalid_` half, under a recovering emitter: every span a **recovered** parse produces is
/// still the extent of its own tokens, and every node type reached was reached where the two span
/// rules disagree.
///
/// The sweep the `valid_` half cannot do. Those entries parse to completion, so nothing in them
/// enters a recovery path; these do nothing else. What comes out is not one tree per entry —
/// smear's own productions raise most of their diagnostics as `Err` rather than through the
/// emitter, so a recovering context rescues only the parses whose miss is a *tokora-owned*
/// close-miss — and the counters below say how many, so a change that quietly stops recovering
/// turns this red instead of leaving a smaller green number.
#[test]
fn recovered_spans_are_token_extents_over_the_invalid_corpus() {
  let entries = invalid_corpus();
  assert!(
    entries.len() >= INVALID_ENTRY_FLOOR,
    "only {} invalid corpus entries, below the floor of {INVALID_ENTRY_FLOOR}",
    entries.len()
  );

  let mut lexable = 0usize;
  let mut lexer_rejected = 0usize;
  let mut padded_variants = 0usize;
  let mut recovered = 0usize;
  let mut compact_recovered = 0usize;
  let mut violations: Vec<(String, String, Violation)> = Vec::new();
  let mut compact_owners: BTreeSet<String> = BTreeSet::new();
  let mut padded_owners: BTreeSet<String> = BTreeSet::new();
  let mut discriminated: BTreeSet<String> = BTreeSet::new();
  let mut discriminating_spans = 0usize;
  let mut total_spans = 0usize;

  let mut measure = |name: &String,
                     form: &str,
                     variant: &str,
                     owners: &mut BTreeSet<String>,
                     violations: &mut Vec<(String, String, Violation)>|
   -> bool {
    let Ok(doc) = parse_recovering(variant) else {
      return false;
    };
    let spans = spans_of(&format!("{doc:#?}"));
    assert!(
      !spans.is_empty(),
      "{name} [{form}]: the parse recovered but the walk found no spans, so this variant was \
       checked for nothing"
    );
    let ext = extents(variant);
    for violation in check(variant, &spans, &ext) {
      violations.push((name.clone(), form.to_string(), violation));
    }
    owners.extend(extent::owners(&spans));
    if form != "compact" {
      total_spans += spans.len();
      for span in &spans {
        if discriminating(span, &ext, variant.len()) {
          discriminating_spans += 1;
          discriminated.insert(span.owner.clone());
        }
      }
    }
    true
  };

  for (name, src) in &entries {
    // The corpus's own claim about itself. An `invalid_` entry the shipped fail-fast parser
    // accepts is a corpus fault, and it would also make every "recovered" count below a lie.
    assert!(
      parse(src).is_err(),
      "{name}: the shipped fail-fast parser accepts it, so it is not an `invalid_` entry"
    );

    // The `invalid_lex_*` class: the lexer refuses these, so there are no token extents to hold a
    // span against — the yardstick itself is absent, and guessing one would be the circularity
    // this gate exists to avoid.
    if !lexes(src) {
      lexer_rejected += 1;
      continue;
    }
    lexable += 1;

    if measure(name, "compact", src, &mut compact_owners, &mut violations) {
      compact_recovered += 1;
      recovered += 1;
    }

    let marks = boundaries(src);
    for (form, pad) in ALPHABET {
      let padded_src = inject(src, &marks, pad);
      assert_eq!(
        padded_src.len(),
        src.len() + marks.len() * pad.len(),
        "{name} padded with {form}: the injection did not land at every boundary"
      );
      assert!(
        lexes(&padded_src),
        "{name} padded with {form}: injecting at a token boundary made the source unlexable"
      );
      padded_variants += 1;
      if measure(name, form, &padded_src, &mut padded_owners, &mut violations) {
        recovered += 1;
      }
    }
  }

  if !violations.is_empty() {
    let mut report: Vec<String> = violations
      .iter()
      .map(|(name, form, violation)| {
        format!(
          "  {name} [{form}] {} — {} — {}",
          violation.owner, violation.rule, violation.detail
        )
      })
      .collect();
    report.sort();
    report.dedup();
    let owners: BTreeSet<&str> = violations
      .iter()
      .map(|(_, _, violation)| violation.owner.as_str())
      .collect();
    panic!(
      "{} span-extent violations over {} node types in recovered parses — a recovered node's span \
       is still the extent of the tokens it holds, not the position recovery stopped at:\n{}\nnode \
       types: {owners:?}",
      violations.len(),
      owners.len(),
      report.join("\n")
    );
  }

  // The recovery floor. Nothing above fires if no parse ever recovers — the sweep would report
  // success over an empty set — so the number of recovered variants is pinned, as is the fact
  // that some of them recovered without any padding at all.
  assert!(
    recovered >= RECOVERED_VARIANT_FLOOR,
    "only {recovered} of {} variants recovered into a tree, below the floor of \
     {RECOVERED_VARIANT_FLOOR}; the sweep has stopped reaching the recovery paths",
    padded_variants + lexable
  );
  assert!(
    compact_recovered > 0,
    "no unpadded invalid entry recovered, so every recovery below depends on the injection"
  );
  assert_eq!(
    lexable + lexer_rejected,
    entries.len(),
    "every invalid entry is either lexable or lexer-rejected"
  );
  assert!(
    lexable >= LEXABLE_INVALID_FLOOR,
    "only {lexable} invalid entries lex, below the floor of {LEXABLE_INVALID_FLOOR}"
  );

  // Reach, and the liveness floor — the same two the `valid_` sweep carries. The padded set is
  // held to a floor rather than to the compact set: padding can change whether an entry recovers
  // at all, which is the one way this half differs from the other.
  assert!(
    padded_owners.len() >= RECOVERED_OWNER_FLOOR,
    "the recovered sweep reached {} span-carrying node types, below the floor of \
     {RECOVERED_OWNER_FLOOR}: {padded_owners:?}",
    padded_owners.len()
  );
  assert!(
    compact_owners.is_subset(&padded_owners),
    "padding lost a node type the unpadded invalid entries reach: {:?}",
    compact_owners
      .difference(&padded_owners)
      .collect::<Vec<_>>()
  );
  let undiscriminated: Vec<&String> = padded_owners.difference(&discriminated).collect();
  assert!(
    undiscriminated.is_empty(),
    "{} of the {} node types in recovered parses were never observed next to injected trivia, so \
     nothing about their spans was actually at stake: {undiscriminated:?}",
    undiscriminated.len(),
    padded_owners.len()
  );
  assert!(
    discriminating_spans * 2 >= total_spans,
    "only {discriminating_spans} of {total_spans} spans sat at a junction where the two rules \
     differ; padding at every boundary should put most of them there"
  );

  println!(
    "gate: {} invalid entries ({lexable} lexable, {lexer_rejected} lexer-rejected) x {} forms = \
     {padded_variants} padded variants, {recovered} recovered ({compact_recovered} unpadded), \
     {total_spans} spans, {discriminating_spans} at a discriminating junction, {} node types",
    entries.len(),
    ALPHABET.len(),
    padded_owners.len()
  );
}

/// Issue #75's own measurement: where a **recovered** delimited node ends, pinned to the byte and
/// adjudicated in the lexer's terms.
///
/// Two shapes, both a bracket group closed by a `)` with a space in front of it — the value list
/// inside an argument list, and the list *type* inside a variables definition. tokora reports the
/// wrong token through the emitter and recovers with a `]` synthesized at the live cursor, spanning
/// the group with [`span_since`](tokora::InputRef::span_since) — cursor to cursor — so its own
/// answer ends at the `)`'s start. `token_spanned` overrides that with the token extent, one token
/// short.
///
/// #75 read the override as a shortening bug. It is the opposite, and the **lexer** settles it
/// without anyone asking the parser: the recovery cursor is an offset the lexer reports as a token
/// *start* and never as a token *end*, so a node closing there closes in the ignorable byte before
/// its neighbour — `end-not-token-end`, which is issue #68 exactly. Each case makes
/// [`extent::check`] say so about the cursor reading, and say nothing about the extent reading.
///
/// Why these two and not an object or a selection set, which #75 also names: in this dialect they
/// cannot be driven into a surviving wrong-closer recovery at all. `decide_object_field_head` and
/// the selection-head dispatcher raise their rejection as an `Err` rather than through the emitter,
/// so the parse dies before tokora's close-miss law runs. Only a group whose wrong closer is the
/// *enclosing* construct's real closer survives, which is what both cases below are.
#[test]
fn a_recovered_group_ends_at_its_last_token_not_at_the_recovery_cursor() {
  /// `src`, the node the recovery lands on, its token extent, and the recovery cursor the other
  /// reading would have closed at.
  fn witness(src: &str, owner: &str, extent: (usize, usize), cursor: usize) {
    let ext = extents(src);

    // It has to be a recovery, or the case is about nothing: the shipped wiring rejects it and
    // the recording one returns a tree.
    assert!(
      parse(src).is_err(),
      "{src:?}: the fail-fast parser must reject the witness, or no recovery happens in it"
    );
    let doc = parse_recovering(src).unwrap_or_else(|e| panic!("{src:?}: no recovered tree: {e:?}"));

    let spans = spans_of(&format!("{doc:#?}"));
    let found: Vec<&FoundSpan> = spans.iter().filter(|span| span.owner == owner).collect();
    assert_eq!(
      found.len(),
      1,
      "{src:?}: {owner} appears {} times",
      found.len()
    );
    assert_eq!(
      (found[0].start, found[0].end),
      extent,
      "{src:?}: the recovered {owner} must end at its last token, not at the closer it recovered \
       against"
    );

    // The other reading, adjudicated by the lexer rather than by the parser.
    assert!(
      ext.starts.contains(&cursor) && !ext.ends.contains(&cursor),
      "{src:?}: {cursor} must be a token start and not a token end for this witness to \
       discriminate at all"
    );
    assert_eq!(
      check(
        src,
        &[FoundSpan {
          owner: owner.to_string(),
          parent: None,
          start: extent.0,
          end: cursor,
        }],
        &ext,
      )
      .iter()
      .map(|violation: &Violation| violation.rule)
      .collect::<Vec<_>>(),
      ["end-not-token-end"],
      "{src:?}: ending a recovered group at the recovery cursor is #68's defect wearing a \
       different hat"
    );

    // And the tree as parsed says nothing.
    assert!(
      check(src, &spans, &ext).is_empty(),
      "{src:?}: the recovered witness violates the extent invariant: {:?}",
      check(src, &spans, &ext)
    );
  }

  // A list value: `[1 , 2` closes on the `2` it holds, not on the `)` at 14. Its enclosing argument
  // list did *not* recover — that `)` is its real closer — so the recovery sits strictly inside a
  // well-closed parent, and `not-contained` has something to be right about.
  witness("{ f(a: [1 , 2 ) }", "List", (7, 13), 14);
  let doc = parse_recovering("{ f(a: [1 , 2 ) }").expect("the witness recovers");
  let spans = spans_of(&format!("{doc:#?}"));
  let of = |owner: &str| -> (usize, usize) {
    let found: Vec<&FoundSpan> = spans.iter().filter(|span| span.owner == owner).collect();
    assert_eq!(found.len(), 1, "{owner} appears {} times", found.len());
    (found[0].start, found[0].end)
  };
  assert_eq!(
    of("ArgumentList"),
    (3, 15),
    "the parent closed on its real `)`"
  );
  assert_eq!(of("Argument"), (4, 13));

  // A list *type*, a different production family reaching the same close-miss law: `[Int` closes
  // on `Int`, not on the `)` at 17.
  witness("query Q($v: [Int ) { f }", "ListType", (12, 16), 17);
}

/// Issue #68's own measurement, pinned to the byte.
///
/// `"  type T  { f : Int }  "`, whose tokens are `type@2..6 T@7..8 {@10..11 f@12..13 :@14..15
/// Int@16..19 }@20..21`. Before the fix the parser answered `NamedType` 16..20, `FieldDefinition`
/// 12..20 and `Document` 0..21: two nodes closed at the *next* token's start, and the document
/// opened at input position 0, in front of its own leading trivia, so its two endpoints followed
/// different rules.
///
/// Kept beside the sweep rather than folded into it because a named witness is what a reader of a
/// future regression needs, and because the document's start is the one endpoint the sweep's other
/// entries reach only incidentally.
#[test]
fn the_issue_68_witness() {
  let src = "  type T  { f : Int }  ";
  assert_eq!(src.len(), 23);

  let ext = extents(src);
  assert_eq!(
    ext.starts,
    BTreeSet::from([2, 7, 10, 12, 14, 16, 20]),
    "the witness's token starts"
  );
  assert_eq!(
    ext.ends,
    BTreeSet::from([6, 8, 11, 13, 15, 19, 21]),
    "the witness's token ends"
  );

  let doc = parse(src).expect("the witness parses");
  let spans = spans_of(&format!("{doc:#?}"));
  let of = |owner: &str| -> (usize, usize) {
    let found: Vec<&FoundSpan> = spans.iter().filter(|span| span.owner == owner).collect();
    assert_eq!(found.len(), 1, "{owner} appears {} times", found.len());
    (found[0].start, found[0].end)
  };

  assert_eq!(of("NamedType"), (16, 19), "was 16..20 — `Int` plus a space");
  assert_eq!(
    of("FieldDefinition"),
    (12, 19),
    "was 12..20 — the field plus the space before `}}`"
  );
  assert_eq!(
    of("Document"),
    (2, 21),
    "was 0..21 — the document plus its own leading trivia"
  );

  assert!(
    check(src, &spans, &ext).is_empty(),
    "the witness still violates the extent invariant: {:?}",
    check(src, &spans, &ext)
  );
}
