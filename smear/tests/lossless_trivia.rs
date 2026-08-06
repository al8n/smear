#![cfg(feature = "rowan")]

//! Gate 2: trivia injection, with a per-production hit counter.
//!
//! The invariant: **trivia is invisible to the grammar.** Padding every token boundary of a valid
//! document with any amount of any ignorable form must change neither the verdict nor the shape
//! of the tree — only its bytes. That is the property the whole `lossless/trivia.rs` atom set
//! exists to provide, and it is the one a trivia-surfacing lexer makes easy to lose: a single
//! decision point that peeks without committing leading trivia turns a comment into a syntax
//! error.
//!
//! # Where the trivia goes, and why that placement can discriminate
//!
//! Padding is inserted at **every** token boundary — before the first token, between every
//! adjacent pair, and after the last — rather than at the boundaries that already carry
//! whitespace. The difference is the whole gate:
//!
//! - **The leading boundary.** Whichever atom a production reaches first faces trivia only when
//!   the source starts with it. Task 4 measured this precisely: `peek_kind`'s own `skip_while` is
//!   *redundant* in interior position, because whatever atom ran before it already crossed the
//!   trivia there — so an interior-only fixture cannot tell a `peek_kind` with a skip from one
//!   without. The leading boundary is where it can.
//! - **The tight junctions.** Real GraphQL never writes `$ x`, `@ d`, `... F`, `alias : f`,
//!   `Int !`, `[ Int ]` or `f (a: 1)`, so the corpus carries no trivia at any of them — and those
//!   are exactly the junctions where an atom's answer decides something: a contextual-keyword
//!   downcast, a delimiter, or a **retro-wrap probe**. `try_eat` is reached only from the two
//!   probes (`Alias` after a field name, `NonNullType` after a type), and
//!   [`the_compact_corpus_cannot_discriminate_the_retro_wrap_probes`] is the measurement that no
//!   corpus entry pads either junction.
//!
//! **Measured, not assumed, and the first answer was wrong.** Deleting `try_eat`'s `skip_while`
//! reds this gate (`valid_document_type_system.graphql` padded with a space, on the verdict) — and
//! it also reds four hand-written fixtures elsewhere in the suite:
//! `lossless_definition.rs::trivia_does_not_change_a_definition_shape`,
//! `lossless_selection.rs::trivia_between_a_name_and_its_colon_still_makes_an_alias` and
//! `::trivia_does_not_change_a_selection_set_shape`, and
//! `lossless_ty_directive.rs::trivia_does_not_change_a_type_reference_shape`. This gate is
//! therefore not the sole witness for that skip; what it adds is *generality* — those four are
//! single sources written to pad one junction each, and this is every entry in the corpus, at
//! every boundary, in all eight forms.
//!
//! # The alphabet
//!
//! All eight forms `LosslessToken::is_trivia` admits, one variant per form:
//! space, tab, newline, carriage return, CRLF, comment, comma and BOM.
//!
//! **What the last four buy is not what the plan expected.** They do *not* exercise the token
//! mapper: mapping `Bom` to `K::Space` leaves this gate green (measured), because the gate
//! compares node kinds and a verdict, and a token image is neither — `lossless_kind_map.rs` is
//! what reds, which is the test that owns that question. Nor can a mapper *forget* an arm, the
//! match being exhaustive with no wildcard. What they buy is the lexer's trivia classification
//! and the atoms' skip over each **form**: no corpus entry contains a carriage return, a tab or a
//! BOM — zero of eighty-four, measured — so without these four the sweep would only ever confirm
//! that the atoms cross a space, a newline, a comment and a comma.
//!
//! **A BOM is injected mid-file as well as at offset 0**, which is sound because the GraphQL
//! lexers skip it at any offset — the SIMD dispatch loop's `0xEF` arm and the Logos `skip`
//! pattern are both position-blind. [`every_alphabet_entry_is_trivia_at_every_position`] is the
//! standing probe for that: it re-measures the claim on every run rather than trusting a comment.
//!
//! # The counter
//!
//! An injection gate that reports success without reporting *reach* is the vacuous case: it
//! cannot tell a corpus that exercises every production from one that exercises three shapes many
//! times. So the productions are instrumented — see
//! [`smear::parser::graphql::lossless::coverage`] — and
//! [`trivia_injection_preserves_the_verdict_and_the_shape`] asserts every node kind a production
//! can open was opened at least once during its own sweep. Run it with `--features
//! lossless-coverage`; without the feature the instrumentation is not compiled and the assertion
//! is not made.

use std::{collections::BTreeSet, path::PathBuf};

use smear::{
  lexer::graphql::lossless::LosslessLexer,
  parser::graphql::{
    kinds::SyntaxKind as K,
    lossless::{Parse, parse_document, parse_executable_document, parse_type_system_document},
  },
};
use tokora::Lexer as _;

/// The eight ignorable forms, one variant of each corpus entry per form.
///
/// The comment carries its own line terminator: a comment runs to the end of the line, so
/// `"# c"` alone would swallow the token after it and the variant would stop being an injection.
const ALPHABET: &[(&str, &str)] = &[
  ("space", " "),
  ("tab", "\t"),
  ("newline", "\n"),
  ("carriage-return", "\r"),
  ("crlf", "\r\n"),
  ("comment", "# c\n"),
  ("comma", ","),
  ("bom", "\u{FEFF}"),
];

/// The kinds no production opens, and which therefore cannot appear in a coverage report.
///
/// Everything else in [`K::ALL`] is a node kind, so [`node_kinds`] defaults a **newly added**
/// kind into the must-be-covered set — the safe direction. Adding a token image without adding it
/// here fails this gate loudly, which is the cheap half of the trade.
const NOT_OPENED_BY_A_PRODUCTION: &[K] = &[
  // Token images: committed tokens enter the tree through the mapper, not through `node`.
  K::Name,
  K::Int,
  K::Float,
  K::String,
  K::BlockString,
  K::Dollar,
  K::LParen,
  K::RParen,
  K::Spread,
  K::Colon,
  K::Equal,
  K::At,
  K::LBracket,
  K::RBracket,
  K::LBrace,
  K::RBrace,
  K::Pipe,
  K::Bang,
  K::Ampersand,
  // Trivia images.
  K::Space,
  K::Tab,
  K::Newline,
  K::Comma,
  K::Comment,
  K::Bom,
  // Bookkeeping. `Error` is opened by `recover.rs` through the raw `cst_start_at`/`cst_finish`
  // pair rather than through `node` (an empty `node(K::Error, |_| Ok(()))` would consume
  // nothing, which is the no-op that module's docs forbid); `Gap` is minted by the sink; `Root`
  // is named once, at `finish`.
  K::Error,
  K::Gap,
  K::Root,
];

/// The two node kinds [`parse_document`] structurally cannot reach, and the reason each is out.
///
/// Both are **roots**, and a parse has exactly one. `parse_document` drives the mixed
/// `Document`, so the SDL-only and the executable-only roots are reachable only through their own
/// entries — which is a property of the suite's shape, not a corpus gap, and no entry could ever
/// close it. The gate drives all three roots for that reason; this list is what keeps a *third*
/// unreached kind from hiding behind the two that are explained.
/// Listed in [`K::ALL`] declaration order, which is the order the check below reports.
const UNREACHABLE_FROM_PARSE_DOCUMENT: &[K] = &[K::ExecutableDocument, K::TypeSystemDocument];

/// Every `.graphql` file in the shared corpus, in a deterministic order.
fn corpus_files() -> Vec<PathBuf> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .collect();
  files.sort();
  files
}

/// The corpus entries this gate pads: the ones both suites accept.
///
/// Invalid entries are out of scope, and four of them are out of *reach*: injection is defined at
/// token boundaries, and a source with a lexer error in it has none to inject at. Those four are
/// the three `invalid_lex_*` entries and `invalid_unterminated_string.graphql`. The valid corpus
/// meets neither problem — every entry lexes — and gate 1's padded sweep, which is the one widened
/// to the invalid half, pins them as its own `UNPADDABLE` set.
fn valid_corpus() -> Vec<(String, String)> {
  corpus_files()
    .into_iter()
    .filter(|p| {
      p.file_name()
        .is_some_and(|n| n.to_string_lossy().starts_with("valid_"))
    })
    .map(|p| {
      let name = p.file_name().unwrap().to_string_lossy().to_string();
      let src = std::fs::read_to_string(&p).unwrap();
      (name, src)
    })
    .collect()
}

/// Every token boundary in `src`: offset 0, the end of each token, and therefore `src.len()`.
///
/// Measured with the **lexer**, not with the tree this gate is asserting about. Reading the
/// boundaries off `parse_document`'s own tokens would make the padding a function of the artifact
/// under test, so a parser that lost a token would quietly stop being padded there.
fn token_boundaries(src: &str) -> Vec<usize> {
  let mut lexer = LosslessLexer::<'_, &str>::new(src);
  let mut boundaries = vec![0usize];
  while let Some(result) = lexer.lex() {
    result.unwrap_or_else(|e| panic!("a valid corpus entry must lex: {e:?}"));
    boundaries.push(lexer.span().end());
  }
  boundaries.dedup();
  boundaries
}

/// `src` with `pad` inserted at every boundary in `boundaries`.
fn inject(src: &str, boundaries: &[usize], pad: &str) -> String {
  let mut out = String::with_capacity(src.len() + boundaries.len() * pad.len());
  let mut cursor = 0usize;
  for &boundary in boundaries {
    out.push_str(&src[cursor..boundary]);
    out.push_str(pad);
    cursor = boundary;
  }
  out.push_str(&src[cursor..]);
  out
}

/// The node-kind pre-order of a parse — the tree's *shape*, with its bytes projected away.
fn shape(parse: &Parse) -> Vec<K> {
  parse.syntax().descendants().map(|n| n.kind()).collect()
}

/// The node kinds a production can open: [`K::ALL`] minus [`NOT_OPENED_BY_A_PRODUCTION`].
fn node_kinds() -> Vec<K> {
  K::ALL
    .iter()
    .copied()
    .filter(|k| !NOT_OPENED_BY_A_PRODUCTION.contains(k))
    .collect()
}

/// Gate 2 proper: the verdict and the shape survive every form of trivia at every boundary, and
/// — with `--features lossless-coverage` — the sweep reached every production while doing it.
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
  smear::parser::graphql::lossless::coverage::reset();

  let entries = valid_corpus();
  assert!(
    entries.len() >= 20,
    "only {} valid corpus entries; the sweep is too thin to mean anything",
    entries.len()
  );

  let mut padded_parses = 0usize;
  for (name, src) in &entries {
    let boundaries = token_boundaries(src);
    assert!(
      boundaries.len() >= 3,
      "{name}: {} token boundaries — a one-token entry cannot exercise an interior junction",
      boundaries.len()
    );

    let compact = parse_document(src);
    assert!(
      !compact.has_errors(),
      "{name}: the unpadded entry does not parse — this is a corpus fault, not an injection one"
    );
    let compact_shape = shape(&compact);
    // The two roots `parse_document` cannot reach, over the unpadded bytes. Hoisted out of the form
    // loop: their verdicts do not depend on which form is being injected, and re-deriving them
    // eight times would inflate every count in the coverage report by the same factor.
    let compact_sdl = parse_type_system_document(src).has_errors();
    let compact_executable = parse_executable_document(src).has_errors();

    for (form, pad) in ALPHABET {
      let padded_src = inject(src, &boundaries, pad);
      let padded = parse_document(&padded_src);
      assert!(
        !padded.has_errors(),
        "{name} padded with {form}: the parse reported an error, so some decision point looked \
         at the head without committing the trivia in front of it"
      );
      assert_eq!(
        shape(&padded),
        compact_shape,
        "{name} padded with {form}: the node-kind pre-order changed, so the trivia reached the \
         grammar instead of only the tree"
      );
      padded_parses += 1;

      // The same padded bytes through the two roots `parse_document` cannot reach, so the coverage
      // claim below covers them too. Only the verdict is compared: an alternate root rejects most
      // of this corpus, and a *rejected* parse's shape is a function of where the recovery holes
      // fell, which trivia may legitimately move.
      assert_eq!(
        parse_type_system_document(&padded_src).has_errors(),
        compact_sdl,
        "{name} padded with {form}: the SDL-only root changed its verdict"
      );
      assert_eq!(
        parse_executable_document(&padded_src).has_errors(),
        compact_executable,
        "{name} padded with {form}: the executable-only root changed its verdict"
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
    use smear::parser::graphql::lossless::coverage;

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
/// returned a constant — would satisfy the coverage assertion above forever. The probe is a
/// source that opens exactly one of two node kinds, so one count must move and the other must
/// not; asserting only that some count is non-zero would pass on a counter that never resets.
#[cfg(feature = "lossless-coverage")]
#[test]
fn the_hit_counter_distinguishes_a_reached_production_from_an_unreached_one() {
  use smear::parser::graphql::lossless::coverage;

  coverage::reset();
  assert_eq!(
    coverage::hits_of(K::ObjectTypeDefinition),
    0,
    "reset left a count behind"
  );

  let one = parse_document("type T { f: Int }");
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

  let two = parse_document("type A { f: Int } type B { g: Int }");
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
  parse_document("enum E { A }");
  let all = coverage::hits();
  assert_eq!(all.len(), K::ALL.len(), "the tally is indexed by raw kind");
  assert_eq!(
    all[K::EnumTypeDefinition.raw() as usize],
    coverage::hits_of(K::EnumTypeDefinition)
  );
  assert_eq!(all[K::EnumTypeDefinition.raw() as usize], 1);
}

/// Every alphabet entry really is trivia, and really is trivia *everywhere*.
///
/// Two failure modes this closes. An entry that is not ignorable at all — say a stray `#` without
/// its terminator — would make the sweep above a test of something else entirely, and it would
/// announce itself as a grammar bug. And an entry that is ignorable only at offset 0 — the BOM is
/// the standing candidate, since the GraphQL spec requires it to be ignorable without saying
/// where — would pass a leading-only probe and fail on somebody's file.
///
/// The measurement is the token stream itself: padding a fixed source at every boundary with the
/// form under test must leave the **non-trivia** tokens exactly as they were.
#[test]
fn every_alphabet_entry_is_trivia_at_every_position() {
  use smear::lexer::graphql::lossless::LosslessTokenKind as LK;

  const PROBE: &str = "type T{a:[Int!]!@d(x:$v)}";

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

  // `type T { a : [ Int ! ] ! @ d ( x : $ v ) }` — nineteen significant tokens, and every tight
  // junction this gate cares about (`name:`, `[`type, type`!`, `@`name, name`(`, `$`name).
  let want = significant(PROBE);
  assert_eq!(
    want.len(),
    19,
    "the probe must carry every junction this gate cares about"
  );

  let boundaries = token_boundaries(PROBE);
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

/// The padding is real, and the shape comparison is not a self-comparison.
///
/// [`trivia_injection_preserves_the_verdict_and_the_shape`] compares two `Vec<K>`. Both halves of
/// that comparison have a way of going quietly inert — an [`inject`] that returned its input, or
/// a [`shape`] that answered the same thing for every tree — and either would leave the gate green
/// forever. So each is driven until it disagrees with itself.
#[test]
fn the_injection_and_the_shape_projection_are_both_live() {
  const SRC: &str = "type T{a:Int}";
  let boundaries = token_boundaries(SRC);
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
    // The leading boundary is the one Task 4 measured as the only discriminating position for a
    // first-atom skip, so it is asserted by itself rather than left to the length arithmetic.
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
    shape(&parse_document("type T{a:Int}")),
    shape(&parse_document("enum E{A}")),
    "the shape projection answers the same thing for two different trees"
  );
}

/// The measurement behind this gate's claim to discriminate: the compact corpus pads neither
/// retro-wrap junction.
///
/// `trivia.rs`'s `try_eat` is reached from exactly two places — the `Alias` probe after a field
/// name and the `NonNullType` probe after a type — and its `skip_while` is the only thing that
/// lets either see past trivia. No corpus entry writes `alias : f` or `Int !`, so **the corpus
/// alone cannot tell that skip's presence from its absence**: the padding is what turns these
/// eighty-four files into a fixture for it. (Four hand-written fixtures elsewhere in the suite do
/// cover the two probes — see this file's module docs — so the claim is about the corpus, not
/// about the repository.)
///
/// Recorded as an assertion rather than a comment because the day someone adds `Int !` to the
/// corpus is the day the compact half starts covering it, and the reason this gate pads every
/// boundary rather than the empty ones stops being true.
#[test]
fn the_compact_corpus_cannot_discriminate_the_retro_wrap_probes() {
  use smear::lexer::graphql::lossless::LosslessTokenKind as LK;

  /// Does `src` ever put trivia immediately before a `:` or a `!`?
  fn pads_a_probe_junction(src: &str) -> bool {
    let mut lexer = LosslessLexer::<'_, &str>::new(src);
    let mut previous_was_trivia = false;
    while let Some(result) = lexer.lex() {
      let token = result.expect("a corpus entry must lex");
      let trivia = token.is_trivia();
      if previous_was_trivia && matches!(token.kind(), LK::Colon | LK::Bang) {
        return true;
      }
      previous_was_trivia = trivia;
    }
    false
  }

  for (name, src) in valid_corpus() {
    assert!(
      !pads_a_probe_junction(&src),
      "{name} now pads a `:` or `!` junction — the compact corpus can discriminate `try_eat`'s \
       skip on its own, and this gate is no longer the only witness for it"
    );
  }

  // …and the padded form does, which is what makes the sweep above the witness.
  let probe = "alias: f";
  assert!(!pads_a_probe_junction(probe));
  assert!(pads_a_probe_junction(&inject(
    probe,
    &token_boundaries(probe),
    " "
  )));
}

/// Why four of the eight alphabet entries are the only source of their form.
///
/// The corpus is ordinary hand-written GraphQL: spaces, newlines, commas and comments, and
/// nothing else. So a carriage return, a CRLF, a tab and a BOM reach the lossless parser through
/// **this gate's injection or not at all**, as far as the corpus is concerned — which is the
/// measured reason those four forms are in the alphabet, and it is not the reason the plan gave
/// (see the module docs: a wrong `Bom` arm in the token mapper leaves this gate green).
///
/// Asserted rather than stated, so that adding a CRLF entry to the corpus tells someone this
/// gate's alphabet has stopped being the only door.
#[test]
fn the_corpus_supplies_no_carriage_return_tab_or_bom() {
  for (name, src) in valid_corpus() {
    assert!(!src.contains('\r'), "{name} now carries a carriage return");
    assert!(!src.contains('\t'), "{name} now carries a tab");
    assert!(!src.contains('\u{FEFF}'), "{name} now carries a BOM");
  }

  // The positive control: the assertions above are three `contains` calls over strings that could
  // be empty for any number of reasons. These are the same calls, answering the other way.
  let boundaries = token_boundaries("{a}");
  for pad in ["\r", "\t", "\u{FEFF}"] {
    let padded = inject("{a}", &boundaries, pad);
    assert!(padded.contains(pad), "the probe did not inject {pad:?}");
  }
}

/// The two roots [`parse_document`] cannot reach are exactly the two named, and they are roots.
///
/// The coverage assertion drives three entry points to reach 59 kinds. That is only honest if
/// the reason `parse_document` alone falls short is structural — a parse has one root — rather
/// than a corpus gap somebody could have closed. So the claim is measured: over the whole valid
/// corpus, `parse_document` opens every node kind except those two, and each of those two is
/// opened by its own entry over the same bytes.
#[test]
fn parse_document_reaches_every_node_kind_but_the_two_alternate_roots() {
  let mut seen: BTreeSet<K> = BTreeSet::new();
  let mut sdl_root = false;
  let mut executable_root = false;
  for (_name, src) in valid_corpus() {
    seen.extend(shape(&parse_document(&src)));
    sdl_root |= shape(&parse_type_system_document(&src)).contains(&K::TypeSystemDocument);
    executable_root |= shape(&parse_executable_document(&src)).contains(&K::ExecutableDocument);
  }

  let missing: Vec<K> = node_kinds()
    .into_iter()
    .filter(|k| !seen.contains(k))
    .collect();
  assert_eq!(
    missing, UNREACHABLE_FROM_PARSE_DOCUMENT,
    "`parse_document` over the valid corpus reaches all but \
     {UNREACHABLE_FROM_PARSE_DOCUMENT:?}; it now misses {missing:?} instead"
  );
  assert!(sdl_root, "the SDL-only root never opened its own node");
  assert!(
    executable_root,
    "the executable-only root never opened its own node"
  );
}

/// The must-be-covered set is the 59 node kinds, and nothing has drifted into or out of it.
#[test]
fn the_covered_set_is_every_node_kind() {
  let kinds = node_kinds();
  assert_eq!(
    kinds.len(),
    59,
    "Phase A declares 59 node kinds; the set this gate covers is {kinds:?}"
  );
  assert_eq!(
    kinds.len() + NOT_OPENED_BY_A_PRODUCTION.len(),
    K::ALL.len(),
    "the two halves of the kind space must partition it"
  );
  for excluded in NOT_OPENED_BY_A_PRODUCTION {
    assert!(
      !kinds.contains(excluded),
      "{excluded:?} is both excluded and covered"
    );
  }
}
