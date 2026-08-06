#![cfg(feature = "graphql")]

//! The syntactic span-extent gate for GraphQL: trivia injection, with a per-node-type
//! discrimination counter.
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

use std::{collections::BTreeSet, path::PathBuf};

use smear_lexer::graphql::syntactic::SyntacticLexer;
use smear_parser::graphql::{
  GraphQL,
  ast::Document,
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, document},
};
use tokora::{Lexer as _, Parse as _, Parser, SimpleSpan};

#[path = "support/span_extent.rs"]
mod extent;

use extent::{ALPHABET, Extents, FoundSpan, Violation, check, discriminating, inject, spans_of};

/// The smallest number of distinct span-carrying node types this corpus reaches.
///
/// A floor, not a pin: adding a corpus entry that reaches a sixty-ninth node type is not a
/// regression, and losing one is. The value is the measurement, taken the day this gate was
/// written.
const OWNER_FLOOR: usize = 68;

/// Parses one source through the syntactic GraphQL document root.
fn parse(src: &str) -> Result<Document<&str>, GraphqlErrors<&str>> {
  Parser::with_parser::<'_, GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
    document,
  )
  .parse_str(src)
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
    result.unwrap_or_else(|e| panic!("a valid corpus entry must lex: {e:?}"));
    out.push(lexer.span().end());
  }
  out.dedup();
  out
}

/// The `valid_` half of the shared GraphQL corpus, in a deterministic order.
fn valid_corpus() -> Vec<(String, String)> {
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
        .is_some_and(|name| name.to_string_lossy().starts_with("valid_"))
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
