#![cfg(all(feature = "graphqlx", feature = "parser"))]

//! The syntactic span-extent gate for GraphQLx — the twin of `syntactic_span_extent.rs`.
//!
//! `parser` is in the header for the reason that file's header gives: the gate reads
//! `smear::parser::graphqlx`, and until #136 no build existed in which a header could omit it and
//! be caught.
//!
//! The invariant, the alphabet, the walk and the liveness floor are that file's; it carries the
//! reasoning and this one carries the dialect. What is worth saying separately is why the dialect
//! needed its own run rather than inheriting the GraphQL result: **issue #68 was three times the
//! size here.** Eleven GraphQL node types closed a span on a lookahead position; thirty-three
//! GraphQLx ones did, because every construct the dialect adds — the generic argument lists, the
//! `where` clauses, the namespaced type paths, the set and map literals — is a production that
//! peeks past its own tail to decide whether the tail is there. [`the_generics_witness`] pins one
//! of them.
//!
//! Both dialects' gates share `tests/support/span_extent.rs`, which is where the eight-form
//! alphabet, the `Debug` span walk, the four-part check and the discrimination classifier live.
//! The lossless trivia gates keep their own copy of the alphabet in
//! `tests/support/graphqlx_padding.rs`; the two are deliberately not merged, because that module's
//! boundary scan is defined over the **lossless** lexer and this gate's is defined over the
//! syntactic one, and a gate about syntactic spans should measure syntactic tokens.
//!
//! # The `invalid_` half and the recovering emitter — issue #75
//!
//! The GraphQL twin's module docs carry the reasoning; the short of it is that the `valid_` sweep
//! parses to completion and so exercises no recovery path at all, which is what made #75
//! unanswerable by running the suite. [`RecoveringCtx`] is the recording context that gives the
//! `invalid_` entries a tree to measure, and
//! [`recovered_spans_are_token_extents_over_the_invalid_corpus`] holds them to the same four-part
//! check and the same liveness floor.
//!
//! [`a_recovered_group_ends_at_its_last_token_not_at_the_recovery_cursor`] is this dialect's
//! measurement of the question itself: a group closed by recovery ends at the token extent, not at
//! the live cursor, and the lexer is what says so.

use std::{collections::BTreeSet, path::PathBuf};

use smear::{
  lexer::graphqlx::syntactic::SyntacticLexer,
  parser::graphqlx::{
    GraphQLx,
    ast::Document,
    error::GraphqlxErrors,
    syntactic::{GraphqlxLexer, document},
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
/// A floor, not a pin — see the GraphQL twin. Ninety-three against GraphQL's sixty-eight, which is
/// the dialect's extra surface showing up as extra node types rather than as extra entries.
const OWNER_FLOOR: usize = 93;

/// The floors the `invalid_` sweep carries — the GraphQL twin documents what each one is for.
const INVALID_ENTRY_FLOOR: usize = 45;
/// The smallest number of `invalid_` entries the **lexer** accepts.
const LEXABLE_INVALID_FLOOR: usize = 41;
/// The smallest number of invalid variants (compact plus padded) that must **recover** into a tree.
const RECOVERED_VARIANT_FLOOR: usize = 81;
/// The smallest number of distinct span-carrying node types the recovered invalid parses reach.
const RECOVERED_OWNER_FLOOR: usize = 16;

/// Parses one source through the syntactic GraphQLx document root, fail-fast — the shipped wiring.
fn parse(src: &str) -> Result<Document<&str>, GraphqlxErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    Document<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(document)
  .parse_str(src)
}

/// The recording emitter the invalid-corpus sweep runs under — the GraphQL twin explains the three
/// type parameters and why the grammar brand has to be spelled.
type RecoveringEmitter<'inp> = Verbose<GraphqlxErrors<&'inp str>, SimpleSpan, GraphQLx>;

/// The context [`parse_recovering`] drives: this dialect's syntactic productions with the
/// close-miss diagnostics *recorded* rather than raised.
///
/// Not a shipped configuration — the entry points pin `Fatal` — but a reachable one, because the
/// productions are public and generic over `Ctx`, and the only one in which a recovery span exists.
type RecoveringCtx<'inp> = ParserContext<
  'inp,
  GraphqlxLexer<'inp, str>,
  RecoveringEmitter<'inp>,
  DefaultCache<'inp, GraphqlxLexer<'inp, str>>,
  GraphQLx,
>;

/// Parses one source through the same document root with a **recovering** emitter.
///
/// An `Ok` here over a source [`parse`] rejects is the definition of a recovered parse: the two
/// wirings differ in nothing but whether an emitted diagnostic becomes an `Err`.
fn parse_recovering(src: &str) -> Result<Document<&str>, GraphqlxErrors<&str>> {
  Parser::with_parser_and_context::<
    '_,
    GraphqlxLexer<'_, str>,
    Document<&str>,
    RecoveringCtx<'_>,
    _,
    GraphQLx,
  >(document, ParserContext::of(RecoveringEmitter::default()))
  .parse_str(src)
}

/// Does the **lexer** accept `src` end to end?
///
/// The `invalid_lex_*` class does not, and for those there are no token extents to measure a span
/// against — the yardstick itself is missing.
fn lexes(src: &str) -> bool {
  let mut lexer = SyntacticLexer::<str>::new(src);
  while let Some(result) = lexer.lex() {
    if result.is_err() {
      return false;
    }
  }
  true
}

/// Every token's start and end, from the **lexer** rather than from the tree under test.
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

/// One half of the GraphQLx corpus, selected by file-name prefix, in a deterministic order.
fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the GraphQLx corpus at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
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

/// The `valid_` half of the GraphQLx corpus.
fn valid_corpus() -> Vec<(String, String)> {
  corpus("valid_")
}

/// The `invalid_` half — the entries the shipped parser rejects and the sweep above never touches.
fn invalid_corpus() -> Vec<(String, String)> {
  corpus("invalid_")
}

/// The walk's assumption about `#[derive(Debug)]`, re-measured against the GraphQLx AST.
///
/// Not inherited from the GraphQL gate: the two dialects derive their node types independently,
/// and a GraphQLx node that rendered its span differently would leave this whole file checking
/// nothing while the other one stayed green.
#[test]
fn the_span_walk_reads_the_debug_rendering_this_gate_assumes() {
  let doc = parse("type T<A>{f:set<A>}").expect("the control source parses");
  let found = spans_of(&format!("{doc:#?}"));
  assert!(
    found.len() >= 10,
    "the walk found only {} spans in a generic type with a set-typed field — it is reading the \
     rendering wrong",
    found.len()
  );
  assert!(
    found.iter().any(|span| span.owner == "TypeGenerics"),
    "the walk must attribute the `<A>` on the field's type to `TypeGenerics` — the GraphQLx-only \
     node this control exists for; it saw {:?}",
    extent::owners(&found)
  );
  assert!(
    found.iter().any(|span| span.parent.is_some()),
    "the walk produced no nesting at all, so `not-contained` can never fire"
  );
}

/// [`extent::check`] must be able to say "no" over GraphQLx tokens too.
///
/// The same five probes the GraphQL twin makes, over `a b`, which lexes identically in both
/// dialects. Repeated rather than shared because the checker is fed this dialect's extents in the
/// sweep below, and a shared assertion in the other binary cannot speak about this one.
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
    "the checker rejected a correct tree"
  );
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Spilled".into(),
      parent: None,
      start: 0,
      end: 2
    }]),
    ["end-not-token-end"]
  );
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Early".into(),
      parent: None,
      start: 1,
      end: 3
    }]),
    ["start-not-token-start"]
  );
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Absent".into(),
      parent: None,
      start: 4,
      end: 4
    }]),
    ["empty-off-boundary"]
  );
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
  assert_eq!(
    rules(&[FoundSpan {
      owner: "Backwards".into(),
      parent: None,
      start: 3,
      end: 1
    }]),
    ["inverted"]
  );

  assert!(discriminating(&good[1], &ext, src.len()));
  assert!(!discriminating(&good[0], &ext, src.len()));

  // The tokenless carve-out, both halves — see the GraphQL twin. A comment-only source has no
  // token boundaries, so only its two input edges can carry the empty document node a recovering
  // parse produces there; anywhere else in it is still named, and a source with tokens is
  // unaffected at offset 0.
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
  assert_eq!(
    check(
      " a",
      &[FoundSpan {
        owner: "Empty".into(),
        parent: None,
        start: 0,
        end: 0
      }],
      &extents(" a")
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

  assert_eq!(
    padded_owners, compact_owners,
    "the padded sweep and the compact corpus disagree about which node types exist"
  );
  assert!(
    padded_owners.len() >= OWNER_FLOOR,
    "the sweep reached {} span-carrying node types, below the floor of {OWNER_FLOOR}: {padded_owners:?}",
    padded_owners.len()
  );

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
/// The GraphQL twin carries the reasoning. What differs here is only the size of the corpus and
/// how far a recovering context gets through it, both of which the counters below pin.
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
    assert!(
      parse(src).is_err(),
      "{name}: the shipped fail-fast parser accepts it, so it is not an `invalid_` entry"
    );

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

/// Issue #75's own measurement in this dialect: where a **recovered** delimited node ends, pinned
/// to the byte and adjudicated in the lexer's terms.
///
/// The GraphQL twin carries the argument. The recovery cursor is an offset the lexer reports as a
/// token *start* and never as a token *end*, so closing a recovered group there is
/// `end-not-token-end` — issue #68 — and the checker is made to say so about that reading and to
/// say nothing about the token-extent one.
#[test]
fn a_recovered_group_ends_at_its_last_token_not_at_the_recovery_cursor() {
  fn witness(src: &str, owner: &str, extent: (usize, usize), cursor: usize) {
    let ext = extents(src);

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

    assert!(
      check(src, &spans, &ext).is_empty(),
      "{src:?}: the recovered witness violates the extent invariant: {:?}",
      check(src, &spans, &ext)
    );
  }

  witness("{ f(a: [1 , 2 ) }", "List", (7, 13), 14);
  witness("query Q($v: [Int ) { f }", "ListType", (12, 16), 17);
}

/// The GraphQLx witness: a generic head and a `where` clause, the two junctions the dialect adds.
///
/// `"  type T <A> where A : Node { f : Int }  "`. Both are optional tails decided on the token
/// after the one before them, so before the fix `TypePath`, `DefinitionTypeGenerics`,
/// `WherePredicate` and `WhereClause` all closed at the following token's start — the largest
/// single group in the thirty-three this dialect had.
#[test]
fn the_generics_witness() {
  let src = "  type T <A> where A : Node { f : Int }  ";
  let ext = extents(src);
  let doc = parse(src).expect("the witness parses");
  let spans = spans_of(&format!("{doc:#?}"));

  let of = |owner: &str| -> (usize, usize) {
    let found: Vec<&FoundSpan> = spans.iter().filter(|span| span.owner == owner).collect();
    assert_eq!(found.len(), 1, "{owner} appears {} times", found.len());
    (found[0].start, found[0].end)
  };

  // `<A>` closes on `>`, not on `where`.
  assert_eq!(of("DefinitionTypeGenerics"), (9, 12));
  // `A : Node` closes on `Node`, not on `{`.
  assert_eq!(of("WherePredicate"), (19, 27));
  assert_eq!(of("WhereClause"), (13, 27));
  // And the document opens on `type`, not at input position 0.
  assert_eq!(of("Document"), (2, 39));

  assert!(
    check(src, &spans, &ext).is_empty(),
    "the witness still violates the extent invariant: {:?}",
    check(src, &spans, &ext)
  );
}
