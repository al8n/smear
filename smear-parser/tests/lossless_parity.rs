#![cfg(feature = "rowan")]

//! Gate 1: acceptance parity between the lossless suite and the shipped `syntactic` one.
//!
//! The cross-suite invariant is a **verdict**: for every corpus entry, both suites must agree on
//! *whether* the input is valid GraphQL. Diagnostic sets are deliberately not compared — the
//! lossless suite recovers and is therefore more verbose by construction, and its holes report
//! `Severity::Warning` rather than an error precisely so a recovered parse still reads as
//! accepted.
//!
//! # Why this gate could not exist before Task 8
//!
//! `Parse::has_errors()` is the lossless verdict. Until Task 8 it lied: `trivia::expect` returned
//! `Err` without emitting, so an outright parse failure recorded nothing and read as a success.
//! That is fixed and mutation-proved in `lossless_document.rs`; this file consumes the result.
//!
//! # The failure mode this gate is most exposed to
//!
//! A parity suite where both sides accept everything passes forever and proves nothing. The
//! corpus alone cannot rule that out, so the defences are tests rather than comments:
//!
//! 1. [`both_verdicts_answer_in_both_directions`] — neither verdict function is a constant.
//! 2. [`the_corpus_can_tell_the_two_suites_apart`] — the corpus entries are *discriminating
//!    material*: twenty-eight of them separate a lossless production from a syntactic one. This
//!    is what proves the equality assertion is not vacuous, and it replaces the eleven divergence
//!    witnesses that used to carry that burden — see below.
//! 3. The `checked_valid` / `checked_invalid` counters inside the parity test itself, which fail
//!    an all-valid or an all-invalid corpus.
//! 4. [`every_corpus_entry_declares_its_expected_verdict`] — a misspelled prefix cannot smuggle
//!    an entry into the wrong class.
//! 5. [`both_suites_agree_across_the_lexer_grammar_error_boundary`] — the two suites reject the
//!    `invalid_lex_*` class through *different machinery*, and the class is checked against the
//!    channel each one used rather than against the agreeing bit alone.
//!
//! # The lexer-versus-grammar boundary, and why it needed its own corpus class
//!
//! Every other `invalid_*` entry is a **grammar** rejection: the lexer hands over tokens and the
//! grammar refuses them. That leaves the other half of the acceptance surface untested — a source
//! the lexer itself refuses, where the two suites reach their verdict through machinery that has
//! nothing in common. `syntactic/` fails the whole parse out of the lexer
//! (`ErrorData::Other("lexer error")`, one error, span `0..0`); the lossless suite has no such
//! exit — it records the lexer error as a diagnostic, keeps parsing, and tiles the refused bytes
//! into the tree as a `Gap`. Whether those two answers *agree* is the claim, and until this class
//! existed nothing measured it.
//!
//! **The class could not be written before tokora `2bbca21`.** `Sink::finish`'s zero-token wall
//! (`FinishError::StructureWithoutTokens`) refused any parse that opened a node and committed no
//! token, and a source with no lexable byte at all is exactly that: `document` opens `Document`,
//! every byte lexes as one recorded error, and not a single `Event::Token` is ever pushed. The
//! wall now additionally requires an *uncovered gap*, so a fully explained source passes. One
//! lexable byte anywhere avoided the wall, which is why the fixtures for this class carry **no
//! trailing newline** and **no explanatory comment** — a comment lexes, and a fixture that lexes
//! is not in this class. See [`LEXER_ERROR_ENTRIES`].
//!
//! The class is *not* "entries with a lexer error in them".
//! `invalid_unterminated_string.graphql` has had one since Task 11 and rejects through the lexer
//! channel too — but it lexes nine tokens first, so it never reached the wall and it does not
//! exercise the shape the fix unblocked. That contrast is asserted, not narrated.
//!
//! # The eleven divergence witnesses are gone, and where the proof of life went
//!
//! Task 11 measured eleven constructs the lossless suite accepted and `syntactic/` rejected — a
//! `Variable` in ten `Value[Const]` positions, and `on` as a `FragmentName` — and pinned them in
//! a test *because they were also the only evidence this gate's comparison could ever fail*.
//! Both rules are grammar rules rather than validation rules, so the lossless suite learned them:
//! `value.rs`'s `Constness` parameter and `executable.rs`'s fragment-name check, each reporting
//! the error **and still building the node**, which is the ruling Task 8 had already made twice.
//! All eleven now live in `tests/corpus/` as ordinary `invalid_*` entries that this gate holds.
//!
//! That left the gate with nothing separating the two suites — over 1,518 probed sources, none
//! remains — so the proof of life had to stop being a *language* divergence. It is now a
//! **production** divergence, which is permanent by construction: the lossless suite's SDL-only
//! root rejects every executable definition, while `syntactic/`'s mixed root accepts them, so the
//! corpus's twenty-eight executable entries split the two suites on demand. The gate is green
//! because `parse_str`'s root and `document` describe the same language — not because the corpus
//! is inert. Closing *that* divergence would be a bug, so unlike the eleven it cannot quietly go
//! away.
//!
//! # The corpus is run twice: compact, and padded with gate 2's trivia
//!
//! [`both_suites_agree_on_every_corpus_entry`] reads the corpus as written. Every entry is also
//! run through gate 2's injection — the same eight ignorable forms at every token boundary — by
//! [`both_suites_agree_on_every_padded_corpus_entry`], because the compact half cannot see the
//! asymmetry that makes this pairing worth gating at all: **the lossless suite is the only one of
//! the two that sees trivia**. `syntactic/`'s lexer skips it before the grammar exists; the
//! lossless lexer surfaces every byte and each atom has to cross it deliberately. An atom that
//! stops crossing turns valid formatted GraphQL into a rejection *in one suite only*, which is a
//! parity failure by construction — and no compact fixture can produce it.
//!
//! # What this gate cannot see
//!
//! A verdict is one bit. [`a_verdict_gate_is_blind_to_a_lost_definition_node`] exhibits an input
//! on which both suites agree — correctly — while the lossless tree has silently dropped a whole
//! `ObjectTypeDefinition`. Gate 3 (round-trip) is equally blind to it, because every byte
//! survives. Only gate 5's golden trees can see it.

use std::path::PathBuf;

use smear_lexer::graphql::{error::LexerErrors, lossless::LosslessLexer};
use smear_parser::graphql::{
  GraphQL,
  ast::{Document, ExecutableDocument, TypeSystemDocument},
  error::{ErrorData, GraphqlErrors},
  kinds::SyntaxKind as K,
  lossless::{parse_executable_document, parse_str, parse_type_system_document},
  syntactic::{GraphqlLexer, document, executable_document, type_system_document},
};
use tokora::{Lexer as _, Parse as _, Parser};

/// The lossless verdict: did the parse report a grammar **error**?
///
/// Recovery holes and warnings do not count — see `Parse::has_errors`.
fn lossless_has_errors(src: &str) -> bool {
  parse_str(src).has_errors()
}

/// The syntactic verdict: did the shipped `document` production reject the source?
///
/// # This is a whole-input verdict, not a prefix one
///
/// `tokora`'s `parse_str` does **not** check for end-of-input; it hands back whatever the
/// production returned. `document` is nonetheless a whole-input production, because its
/// `repeated_while` decider (`decide_definition_or_extension_head`) answers `Stop` only on
/// `None` — so trailing junk re-enters `definition_or_extension` and fails there rather than
/// being silently left behind. `syntactic/document/tests.rs`'s `reject_all!(document, "{ id } )")`
/// is that behaviour pinned upstream. Without it this helper would need an explicit exhaustion
/// check and `invalid_top_level_junk.graphql` would read as accepted.
fn syntactic_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    Document<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(document)
  .parse_str(src)
  .is_err()
}

/// The syntactic verdict for the **SDL-only** root.
///
/// Whole-input for [`syntactic_has_errors`]'s reason: `type_system_document`'s
/// `repeated_while` decider (`decide_type_system_definition_or_extension_head`) answers `Stop`
/// only on `None`, so a trailing executable definition re-enters
/// `type_system_definition_or_extension` and fails there rather than being left behind.
fn syntactic_type_system_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    TypeSystemDocument<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(src)
  .is_err()
}

/// The syntactic verdict for the **executable-only** root, [`syntactic_type_system_has_errors`]'s
/// mirror — `decide_executable_definition_head` stops only on `None` too.
fn syntactic_executable_has_errors<'inp>(src: &'inp str) -> bool {
  Parser::with_parser::<
    'inp,
    GraphqlLexer<'inp, str>,
    ExecutableDocument<&'inp str>,
    GraphqlErrors<&'inp str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(src)
  .is_err()
}

/// Which of the two machineries produced the syntactic suite's rejection.
///
/// The distinction the `invalid_lex_*` class exists to test. Both values are rejections, so a
/// verdict comparison cannot tell them apart — which is the point: parity across this boundary is
/// a claim about two *different* failure paths landing on the same bit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Channel {
  /// The lexer refused a byte, and the grammar never saw a token for it.
  Lexer,
  /// The lexer produced tokens and the grammar refused them.
  Grammar,
}

/// The `Other` payload of an error, when it has one.
fn other_text<S, T, C, E, SE>(data: &ErrorData<S, T, C, E, SE>) -> Option<&str> {
  match data {
    ErrorData::Other(text) => Some(text.as_ref()),
    _ => None,
  }
}

/// The marker this crate itself stamps on a lexer failure, **derived rather than spelled**.
///
/// `error.rs`'s `impl From<LexerErrors<…>> for GraphqlErrors<S>` discards the lexer's typed error
/// and produces one `ErrorData::Other` with a fixed message — the frozen parser's behaviour, kept
/// deliberately. Reproducing that message as a literal here would give this file a second copy of
/// a string that lives in production, and a rename would leave the classifier silently answering
/// [`Channel::Grammar`] for everything. Running the conversion instead means the classifier is
/// keyed to the impl, so a rename moves both halves together and a *removal* of the impl fails to
/// compile.
fn lexer_error_marker() -> GraphqlErrors<&'static str> {
  LexerErrors::<char, ()>::default().into()
}

/// Which channel the syntactic suite rejected `src` through; `None` when it accepted.
fn syntactic_channel(src: &str) -> Option<Channel> {
  let errors = Parser::with_parser::<
    '_,
    GraphqlLexer<'_, str>,
    Document<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(document)
  .parse_str(src)
  .err()?;

  let marker = lexer_error_marker();
  let marker = other_text(
    marker
      .first()
      .expect("the lexer-error conversion must produce an error")
      .data(),
  )
  .expect("the lexer-error conversion must produce an `Other` payload");

  Some(
    if errors.iter().any(|e| other_text(e.data()) == Some(marker)) {
      Channel::Lexer
    } else {
      Channel::Grammar
    },
  )
}

/// How many of a lossless tree's tokens the grammar committed, as opposed to the sink tiling them.
///
/// Zero is the defining property of [`LEXER_ERROR_ENTRIES`] and the exact state the upstream
/// zero-token wall used to refuse.
fn committed_tokens(src: &str) -> usize {
  parse_str(src)
    .syntax()
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| token.kind() != K::Gap)
    .count()
}

/// How many `Gap` tiles the sink laid down over `src`.
fn gap_tokens(src: &str) -> usize {
  parse_str(src)
    .syntax()
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| token.kind() == K::Gap)
    .count()
}

/// Every `.graphql` file in the shared corpus, in a deterministic order.
///
/// The corpus lives beside this file because gates 2, 3 and 5 all read it; it is not this gate's
/// private fixture set.
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

/// The cross-suite invariant: both suites must agree on *whether* the input is valid GraphQL.
/// Diagnostic sets are deliberately NOT compared — the lossless suite may be more verbose.
#[test]
fn both_suites_agree_on_every_corpus_entry() {
  let mut checked_valid = 0usize;
  let mut checked_invalid = 0usize;

  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let lossless_errs = lossless_has_errors(&src);
    let syntactic_errs = syntactic_has_errors(&src);
    assert_eq!(
      lossless_errs,
      syntactic_errs,
      "{}: lossless says errors={lossless_errs}, syntactic says errors={syntactic_errs}",
      entry.display()
    );

    // Each class is asserted against **both** suites, not only against `lossless_errs`. The
    // equality above already implies the syntactic half — but it is the one assertion in this
    // loop that no corpus entry can ever make fail, precisely because the corpus is an agreeing
    // set (see `the_two_suites_do_not_accept_the_same_language`). Neutering it would therefore
    // be a silent mutation, and these two lines are what keep that mutation harmless: without
    // them, dropping the comparison would drop the syntactic suite out of the gate entirely and
    // leave a lossless-only smoke test wearing a parity gate's name.
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    if name.starts_with("invalid_") {
      assert!(
        lossless_errs,
        "{name} is named invalid but the lossless suite accepted it"
      );
      assert!(
        syntactic_errs,
        "{name} is named invalid but the syntactic suite accepted it"
      );
      checked_invalid += 1;
    } else {
      assert!(
        !lossless_errs,
        "{name} is named valid but the lossless suite rejected it"
      );
      assert!(
        !syntactic_errs,
        "{name} is named valid but the syntactic suite rejected it"
      );
      checked_valid += 1;
    }
  }

  // The positive controls. Without both, the equality assertion above can pass vacuously
  // over a corpus that is all-valid or all-invalid.
  assert!(
    checked_valid >= 20,
    "only {checked_valid} valid entries; the corpus is too thin"
  );
  assert!(
    checked_invalid >= 5,
    "only {checked_invalid} invalid entries; no negative control"
  );
}

/// The eight ignorable forms, one variant of each corpus entry per form.
///
/// **Reproduced from `lossless_trivia.rs`, deliberately and identically.** Every gate in this
/// suite is a self-contained integration binary — `corpus_files` is already written out in three
/// of them — so gate 1 cannot import gate 2's derivation and has to restate it. Restating it
/// exactly is what makes [`both_suites_agree_on_every_padded_corpus_entry`] a run of *gate 2's
/// variants* rather than of some similar-looking set of its own: same alphabet, same boundary
/// tiling, same insertion. `the_padded_variants_are_the_ones_gate_2_derives` pins the two
/// halves a drift could hide in.
///
/// The comment carries its own line terminator: a comment runs to the end of the line, so `"# c"`
/// alone would swallow the token after it and the variant would stop being an injection.
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

/// The corpus entries that cannot be padded, because they cannot be lexed.
///
/// Injection is defined at *token* boundaries, so a source the lexer refuses byte for byte has no
/// boundaries to inject at. Four entries are in that state: the three of
/// [`LEXER_ERROR_ENTRIES`], whose whole source is one lexer error, and
/// `invalid_unterminated_string.graphql`, whose unterminated string swallows the remainder of the
/// file after nine tokens have already been lexed. `token_boundaries` answers `None` on any source
/// with a lexer error anywhere, so partial lexability does not rescue the last one.
///
/// Pinned as a set rather than skipped silently. A **new** unlexable entry is a corpus decision
/// somebody should have to make on purpose. Note the count that did *not* move: the corpus went
/// from 84 entries to 87 and this list from 1 to 4, so the padded sweep still runs exactly
/// `83 × 8 = 664` variants — the new class is unpaddable by construction and buys the padded run
/// nothing, which is why it is gated by [`both_suites_agree_across_the_lexer_grammar_error_boundary`]
/// instead.
const UNPADDABLE: &[&str] = &[
  "invalid_lex_illegal_character.graphql",
  "invalid_lex_unterminated_block_string.graphql",
  "invalid_lex_unterminated_string.graphql",
  "invalid_unterminated_string.graphql",
];

/// The corpus entries with **no lexable byte at all** — the class this gate's lexer-versus-grammar
/// boundary check is built on.
///
/// Each is a single lexeme the GraphQL lexer refuses outright, written with no trailing newline
/// and no comment, because either would lex and the entry would stop being in the class. Task 11b
/// named the three shapes: an unterminated string, an unterminated block string, and an illegal
/// character.
///
/// Pinned as an ordered set and compared against a scan of the `invalid_lex_` prefix, so neither
/// half can drift alone: deleting a fixture reds, and adding one under the prefix without adding
/// it here reds too.
const LEXER_ERROR_ENTRIES: &[&str] = &[
  "invalid_lex_illegal_character.graphql",
  "invalid_lex_unterminated_block_string.graphql",
  "invalid_lex_unterminated_string.graphql",
];

/// The corpus entry that rejects through the **lexer** channel while still lexing tokens.
///
/// The contrast that keeps [`LEXER_ERROR_ENTRIES`] honest. This one has carried a lexer error
/// since Task 11 and is classified [`Channel::Lexer`] exactly like the three above — but nine
/// tokens lex before the unterminated string swallows the rest, so `saw_token` was always true and
/// it never met the zero-token wall. "Has a lexer error" and "has no lexable byte" are different
/// claims, and this is the entry that separates them.
const LEXES_THEN_FAILS: &str = "invalid_unterminated_string.graphql";

/// Every token boundary in `src`: offset 0, the end of each token, and therefore `src.len()`.
///
/// `None` when the source does not lex — see [`UNPADDABLE`]. Measured with the **lexer**, not with
/// the tree under test: reading the boundaries off a parse would make the padding a function of
/// the artifact being asserted about, so a parser that lost a token would quietly stop being
/// padded there.
fn token_boundaries(src: &str) -> Option<Vec<usize>> {
  let mut lexer = LosslessLexer::<'_, &str>::new(src);
  let mut boundaries = vec![0usize];
  while let Some(result) = lexer.lex() {
    result.ok()?;
    boundaries.push(lexer.span().end());
  }
  boundaries.dedup();
  Some(boundaries)
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

/// Gate 1 over gate 2's padded variants: the two suites agree about trivia-laden input too.
///
/// # Why the compact corpus is not enough
///
/// The corpus is hand-written GraphQL, and hand-written GraphQL never puts trivia at the junctions
/// where a lossless atom's answer decides something — `$ x`, `@ d`, `... F`, `alias : f`, `Int !`,
/// `f (a: 1)`. Gate 2 measures that absence directly
/// (`lossless_trivia.rs::the_compact_corpus_cannot_discriminate_the_retro_wrap_probes`). So over
/// the corpus as written, an atom that has stopped crossing leading trivia is indistinguishable
/// from one that still does — in *both* suites, and therefore in their agreement as well.
///
/// Padding closes that, and it closes it in the direction this gate owns. Gate 2 already asserts
/// the lossless verdict and shape survive injection; what it cannot say is whether the **other**
/// suite came along, because it never calls `syntactic_has_errors`. `syntactic/`'s lexer skips
/// trivia before the grammar sees it, so its verdict is invariant under injection by construction
/// — which makes it a fixed reference the lossless verdict is measured against here, one padded
/// variant at a time.
///
/// # Invalid entries are padded too, which gate 2 does not do
///
/// Gate 2 pads only the valid half, because its shape comparison is meaningless over a recovered
/// parse — where the holes fell is a function of the recovery, and trivia may legitimately move
/// them. A **verdict** has no such problem: an invalid document stays invalid however it is
/// spaced. So this gate runs the whole corpus, and gate 2's set is a strict subset of it. The one
/// entry that cannot be reached at all is named in [`UNPADDABLE`] rather than skipped in silence.
#[test]
fn both_suites_agree_on_every_padded_corpus_entry() {
  let mut skipped: Vec<String> = Vec::new();
  let mut padded_valid = 0usize;
  let mut padded_invalid = 0usize;

  for entry in corpus_files() {
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    let src = std::fs::read_to_string(&entry).unwrap();

    let Some(boundaries) = token_boundaries(&src) else {
      skipped.push(name);
      continue;
    };

    // The compact verdict is the anchor: padding must move neither suite off it. Asserting only
    // `lossless == syntactic` on the padded bytes would be satisfied by both suites flipping
    // together, which is precisely what a shared corpus-reading bug would do.
    let compact_lossless = lossless_has_errors(&src);
    let compact_syntactic = syntactic_has_errors(&src);
    assert_eq!(
      compact_lossless, compact_syntactic,
      "{name}: the two suites disagree before any padding — this is the compact gate's failure, \
       reported here because the padded run rests on it"
    );

    for (form, pad) in ALPHABET {
      let padded_src = inject(&src, &boundaries, pad);
      // The injection is real. Without this the whole loop degenerates into a second, slower
      // copy of the compact gate the moment `inject` starts returning its input.
      assert!(
        padded_src.len() > src.len(),
        "{name} padded with {form}: injection added nothing"
      );

      let lossless_errs = lossless_has_errors(&padded_src);
      let syntactic_errs = syntactic_has_errors(&padded_src);
      assert_eq!(
        lossless_errs, syntactic_errs,
        "{name} padded with {form}: lossless says errors={lossless_errs}, syntactic says \
         errors={syntactic_errs} — the two suites disagree about trivia-laden input they agree \
         about compact"
      );
      assert_eq!(
        lossless_errs, compact_lossless,
        "{name} padded with {form}: the lossless verdict moved under injection, so some decision \
         point looked at the head without committing the trivia in front of it"
      );
      assert_eq!(
        syntactic_errs, compact_syntactic,
        "{name} padded with {form}: the syntactic verdict moved under injection — its lexer skips \
         trivia, so this is not a parity finding but a lexer one"
      );

      if name.starts_with("invalid_") {
        padded_invalid += 1;
      } else {
        padded_valid += 1;
      }
    }
  }

  // The exclusion is a pinned set, not a silent `continue`.
  assert_eq!(
    skipped, UNPADDABLE,
    "the set of corpus entries this gate cannot pad has changed"
  );

  // The positive controls, one per class: an all-valid or an all-invalid padded run would satisfy
  // every assertion above while measuring half of what the gate claims.
  assert!(
    padded_valid >= 160,
    "only {padded_valid} padded valid variants; the sweep is too thin"
  );
  assert!(
    padded_invalid >= 40,
    "only {padded_invalid} padded invalid variants; no negative control under injection"
  );
  assert_eq!(
    padded_valid + padded_invalid,
    (corpus_files().len() - UNPADDABLE.len()) * ALPHABET.len(),
    "the sweep did not run every form over every paddable entry"
  );
}

/// The two halves of the derivation that a drift from gate 2 could hide in.
///
/// [`both_suites_agree_on_every_padded_corpus_entry`] claims to run *gate 2's* variants, and that
/// claim rests on two reproduced pieces of code — [`ALPHABET`] and [`token_boundaries`] —
/// which nothing in the compiler holds to their originals. Both are therefore asserted against
/// fixed values that `lossless_trivia.rs` asserts against too: the boundary tiling of `type
/// T{a:Int}` is the same literal vector its
/// `the_injection_and_the_shape_projection_are_both_live` pins, and the alphabet is its eight
/// forms. A copy that has drifted fails here rather than quietly padding something else.
#[test]
fn the_padded_variants_are_the_ones_gate_2_derives() {
  assert_eq!(
    token_boundaries("type T{a:Int}"),
    Some(vec![0, 4, 5, 6, 7, 8, 9, 12, 13]),
    "the boundary tiling has drifted from the one gate 2 derives"
  );
  assert_eq!(ALPHABET.len(), 8, "gate 2 injects eight ignorable forms");
  assert_eq!(
    ALPHABET.iter().map(|(form, _)| *form).collect::<Vec<_>>(),
    [
      "space",
      "tab",
      "newline",
      "carriage-return",
      "crlf",
      "comment",
      "comma",
      "bom"
    ],
    "the alphabet has drifted from gate 2's"
  );

  // `token_boundaries` answers `None` on exactly the class `UNPADDABLE` names, and `Some` on
  // ordinary input — without both directions the skip above could be swallowing the corpus.
  assert!(token_boundaries("\"unterminated").is_none());
  assert!(token_boundaries("{ a }").is_some());
}

/// Gate 1 across the lexer-versus-grammar boundary: the two suites agree about a source the lexer
/// refuses outright, and they get there through different machinery.
///
/// # What the agreeing bit does not say
///
/// [`both_suites_agree_on_every_corpus_entry`] now covers these three entries too, and it would be
/// green over them even if the class had quietly stopped being what it claims — a fixture that
/// grew a trailing newline still rejects in both suites, and so does one replaced by ordinary
/// broken GraphQL. The verdict is one bit and this class is defined by a *shape*, so the shape is
/// asserted directly:
///
/// - **no lexable byte at all** — `token_boundaries` answers `None` and the lossless tree has zero
///   committed tokens. This is precisely the state tokora's zero-token wall used to refuse, and
///   the reason the class could not exist before `2bbca21`;
/// - **the bytes survive anyway**, carried by one `Gap` tile rather than by the grammar. The
///   round-trip law holds over a tree with no grammar tokens in it at all, which is the strongest
///   form of the lossless claim this suite makes;
/// - **the syntactic rejection is a lexer one**, classified through the crate's own conversion
///   rather than by reading a message.
///
/// # The other side of the boundary is asserted too
///
/// A class where every entry rejects proves nothing on its own. Two contrasts run in the same
/// test. The corpus's remaining invalid entries reject through [`Channel::Grammar`] — so the
/// corpus genuinely holds both channels and this class is new material rather than more of the
/// same. And [`LEXES_THEN_FAILS`] is a [`Channel::Lexer`] rejection that **does** lex tokens
/// first: without it, "rejects through the lexer channel" and "has no lexable byte" would be
/// indistinguishable, and a fixture that quietly regained a lexable byte would still pass.
#[test]
fn both_suites_agree_across_the_lexer_grammar_error_boundary() {
  let mut lexer_class: Vec<String> = Vec::new();
  let mut grammar_channel = 0usize;
  let mut lexer_channel = 0usize;

  for entry in corpus_files() {
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    let src = std::fs::read_to_string(&entry).unwrap();

    match syntactic_channel(&src) {
      None => continue,
      Some(Channel::Lexer) => lexer_channel += 1,
      Some(Channel::Grammar) => grammar_channel += 1,
    }

    if !name.starts_with("invalid_lex_") {
      continue;
    }

    // The shape. `None` from `token_boundaries` is "the lexer refused something"; zero committed
    // tokens is the stronger claim that it refused *everything*, which is the one the wall was
    // about — `LEXES_THEN_FAILS` satisfies the first and not the second.
    assert!(
      !src.is_empty(),
      "{name}: an empty file has no lexeme to refuse and never met the wall"
    );
    assert_eq!(
      token_boundaries(&src),
      None,
      "{name}: the lexer accepted this source, so it is not in the lexer-error class at all"
    );
    assert_eq!(
      committed_tokens(&src),
      0,
      "{name}: the grammar committed a token, so this fixture has a lexable byte in it and no \
       longer exercises the zero-token shape the upstream fix unblocked — a trailing newline is \
       enough to do this"
    );

    // The bytes survive with no grammar token carrying them.
    let parse = parse_str(&src);
    assert_eq!(
      parse.syntax().text().to_string(),
      src,
      "{name}: a fully gap-tiled tree did not round-trip"
    );
    assert_eq!(
      gap_tokens(&src),
      1,
      "{name}: the whole source should be one gap tile"
    );

    // The parity claim, and the channel each suite reached it through.
    assert!(
      parse.has_errors(),
      "{name}: the lossless suite accepted a source the lexer refuses byte for byte"
    );
    assert_eq!(
      syntactic_channel(&src),
      Some(Channel::Lexer),
      "{name}: the syntactic suite rejected this through the grammar, so the two suites are not \
       being compared across the boundary this class exists for"
    );
    assert_eq!(
      lossless_has_errors(&src),
      syntactic_channel(&src).is_some(),
      "{name}: lossless and syntactic disagree about a lexer error"
    );

    lexer_class.push(name);
  }

  assert_eq!(
    lexer_class, LEXER_ERROR_ENTRIES,
    "the set of corpus entries with no lexable byte has changed"
  );

  // Contrast one: the corpus holds both channels, so this class is not the whole invalid half
  // wearing a new prefix.
  assert!(
    grammar_channel >= 20,
    "only {grammar_channel} corpus entries reject through the grammar; without them the two \
     channels are not being compared, only the lexer one is being measured twice"
  );
  assert_eq!(
    lexer_channel,
    LEXER_ERROR_ENTRIES.len() + 1,
    "the lexer channel should hold this class plus {LEXES_THEN_FAILS}"
  );

  // Contrast two: a lexer-channel rejection that lexes tokens first. Same channel, different
  // shape — which is what stops the shape assertions above from being satisfiable by "has a lexer
  // error somewhere".
  let partial = std::fs::read_to_string(
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("corpus")
      .join(LEXES_THEN_FAILS),
  )
  .expect("the partially-lexing witness must exist in the shared corpus");
  assert_eq!(
    syntactic_channel(&partial),
    Some(Channel::Lexer),
    "{LEXES_THEN_FAILS} was supposed to be the same channel as the class"
  );
  assert!(
    committed_tokens(&partial) > 0,
    "{LEXES_THEN_FAILS} was supposed to lex tokens before failing — if it no longer does, it has \
     become a fourth member of the class and the contrast is gone"
  );
}

/// The channel classifier answers in both directions, and its marker comes from production code.
///
/// [`both_suites_agree_across_the_lexer_grammar_error_boundary`] rests entirely on
/// [`syntactic_channel`], and a classifier that answered [`Channel::Lexer`] for everything would
/// satisfy every assertion it makes about the class while the grammar floor still passed on
/// nothing. So the classifier is driven over sources that are not corpus entries, in both
/// directions, and its marker is checked to be the one the crate actually produces.
#[test]
fn the_error_channel_classifier_separates_a_lexer_error_from_a_grammar_one() {
  // The marker is a live conversion, not a literal: exactly one error, carrying an `Other`
  // payload. If `error.rs` ever routes lexer failures to a typed variant instead, this reds here
  // rather than silently reclassifying the whole corpus as grammar rejections.
  let marker = lexer_error_marker();
  assert_eq!(
    marker.len(),
    1,
    "the lexer-error conversion should collapse to a single error"
  );
  assert!(
    other_text(marker[0].data()).is_some(),
    "the lexer-error conversion no longer produces an `Other` payload, so the classifier cannot \
     key on it: {:?}",
    marker[0].data()
  );

  // `%` is not a GraphQL lexeme; `!` is one, and the grammar has nowhere to put it.
  assert_eq!(syntactic_channel("%"), Some(Channel::Lexer));
  assert_eq!(syntactic_channel("!"), Some(Channel::Grammar));
  assert_eq!(syntactic_channel("type T { f: Int }"), None);
  assert_ne!(
    syntactic_channel("%"),
    syntactic_channel("!"),
    "the classifier gave the same answer for an unlexable byte and a lexable one the grammar \
     rejects, so it is not classifying anything"
  );

  // And the two channels really are one verdict: both of the rejections above are rejections in
  // *both* suites, which is why only the channel — not the bit — separates them.
  for src in ["%", "!"] {
    assert!(lossless_has_errors(src), "lossless accepted {src:?}");
    assert!(syntactic_has_errors(src), "syntactic accepted {src:?}");
  }
}

/// Every corpus entry names the class it belongs to.
///
/// [`both_suites_agree_on_every_corpus_entry`] routes on the `invalid_` prefix and treats
/// *everything else* as valid, so a file called `invald_foo.graphql` would be silently asserted
/// to parse — and would then fail with a message pointing at the wrong thing entirely. The
/// prefix is a declaration; this is the check that it was actually made.
#[test]
fn every_corpus_entry_declares_its_expected_verdict() {
  let files = corpus_files();
  assert!(!files.is_empty(), "the corpus is empty");
  for entry in files {
    let name = entry.file_name().unwrap().to_string_lossy().to_string();
    assert!(
      name.starts_with("valid_") || name.starts_with("invalid_"),
      "{name}: a corpus entry must be named valid_* or invalid_*"
    );
  }
}

/// Neither verdict function is a constant.
///
/// The parity assertion compares two booleans. If either side always answered the same way the
/// comparison would still be *green over an agreeing corpus* while measuring nothing at all, so
/// each function is driven in both directions here, over inputs that are not corpus entries.
#[test]
fn both_verdicts_answer_in_both_directions() {
  const ACCEPTED: &str = "type T { f: Int }";
  const REJECTED: &str = "type T { f: }";

  assert!(
    !lossless_has_errors(ACCEPTED),
    "lossless rejected {ACCEPTED:?}"
  );
  assert!(
    lossless_has_errors(REJECTED),
    "lossless accepted {REJECTED:?}"
  );
  assert!(
    !syntactic_has_errors(ACCEPTED),
    "syntactic rejected {ACCEPTED:?}"
  );
  assert!(
    syntactic_has_errors(REJECTED),
    "syntactic accepted {REJECTED:?}"
  );
}

/// The corpus is discriminating material, and the syntactic verdict is a parse of its own.
///
/// **This is the test that makes [`both_suites_agree_on_every_corpus_entry`] mean something**, and
/// it is the replacement for the eleven divergence witnesses Task 11 pinned — see the module docs
/// for why they are gone. A parity gate over a corpus both suites happen to agree on is
/// indistinguishable from a parity gate whose comparison never runs; the difference is whether an
/// entry *could* separate them. Two independent halves establish that it could.
///
/// # Half one: the entries themselves split the two suites
///
/// The lossless suite has two roots. `parse_str` drives the **mixed** one, which is the language
/// `syntactic/`'s `document` also describes — that agreement is what the gate measures. Its
/// sibling `type_system_document` is the **SDL-only** root, and it rejects every executable
/// definition. So each corpus entry that carries one is an input on which a lossless production
/// and a syntactic production answer *differently*: the material discriminates, and the gate is
/// green because the two roots under comparison describe the same language rather than because
/// the corpus is inert.
///
/// **This divergence is permanent, which the eleven were not.** Closing it would mean an SDL-only
/// root that accepts `query Q { f }`, which is a bug rather than a fix — so unlike a grammar gap
/// waiting to be filled, this proof of life cannot be closed out from under the gate.
///
/// The floor is asserted against the corpus's own executable content rather than fixed at
/// twenty-eight, so adding an SDL entry cannot red this test and deleting every executable one
/// must.
///
/// # Half two: the syntactic verdict is not the lossless one wearing a hat
///
/// The most complete way for this gate to die is for [`syntactic_has_errors`] to stop consulting
/// the syntactic suite at all — after which every assertion in the file still passes, half one
/// included, because half one never calls it on a source it does not also parse losslessly. So
/// the syntactic side is exercised through its **output** rather than its `is_err()` bit: a
/// `Document` whose definitions can be counted cannot be produced by a `rowan` parse, and an
/// alias would fail to typecheck long before it failed an assertion.
#[test]
fn the_corpus_can_tell_the_two_suites_apart() {
  // Half one. `parse_type_system_document` is the SDL-only root's shipped entry point — the same
  // production set, entered one level up from `document`.
  let mut split = 0usize;
  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let sdl_rejects = parse_type_system_document(&src).has_errors();
    let syntactic_accepts = !syntactic_has_errors(&src);
    if sdl_rejects && syntactic_accepts {
      // The two suites answer differently about this very byte string.
      split += 1;
      // …and the mixed lossless root agrees with `syntactic/` about it, which is the whole
      // point: the disagreement is between the *roots*, not between the suites.
      assert!(
        !lossless_has_errors(&src),
        "{}: the SDL root rejects it and syntactic accepts it, but so does the mixed lossless \
         root — this entry is not the clean discriminator it is being counted as",
        entry.display()
      );
    }
  }
  assert!(
    split >= 20,
    "only {split} corpus entries separate the SDL-only lossless root from the syntactic one; \
     the corpus has lost its executable half and this gate can no longer show its comparison is \
     live"
  );

  // Half two. Not `is_err()`: the value on the `Ok` side is what a lossless parse cannot forge.
  const TWO_DEFINITIONS: &str = "query Q { f } fragment F on T { g }";
  let parsed = Parser::with_parser::<
    '_,
    GraphqlLexer<'_, str>,
    Document<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(document)
  .parse_str(TWO_DEFINITIONS)
  .expect("the syntactic suite must accept two ordinary executable definitions");

  // The count is checked against the **lossless** tree's own, not against a literal. Both sides
  // are then live values drawn from different suites, so the assertion cannot be neutered into
  // a self-comparison the way a literal one can — the mutation the parity loop above records as
  // its own structural blind spot. The literal anchor follows, so an agreement at zero on both
  // sides is still a failure.
  let lossless_definitions = parse_str(TWO_DEFINITIONS)
    .syntax()
    .descendants()
    .filter(|n| matches!(n.kind(), K::OperationDefinition | K::FragmentDefinition))
    .count();
  assert_eq!(
    parsed.definitions().len(),
    lossless_definitions,
    "the syntactic verdict must come from a real syntactic parse of the same bytes"
  );
  assert_eq!(
    lossless_definitions, 2,
    "both suites must have found the two definitions that are there"
  );
}

/// Gate 1, extended to the **two alternate roots** now that each is a shipped entry point.
///
/// [`both_suites_agree_on_every_corpus_entry`] holds the mixed root against `syntactic/`'s
/// `document`. The other two roots had no entry point to hold — they were reachable only from a
/// `test_support` driver, so every assertion over them was a lossless-versus-lossless comparison
/// with nothing on the other side (smear issue #67). Each now has one, and each is held against
/// **its own syntactic counterpart**: `parse_type_system_document` against
/// `syntactic::type_system_document`, `parse_executable_document` against
/// `syntactic::executable_document`.
///
/// # The oracle is on the other side, and it is not the same root
///
/// Anchoring both entry points against the *mixed* syntactic root would be the Task 17 mistake
/// wearing a different hat: the mixed root accepts strictly more than either alternate, so the
/// comparison would fail on every executable corpus entry and would have to be weakened into an
/// implication that proves nothing. The counterpart root is the only oracle whose language is
/// the one under test, and it is a genuinely independent parse — different lexer, different
/// error type, an AST rather than a `rowan` tree.
///
/// # Why it cannot pass vacuously
///
/// Three counters, each of which must be nonzero:
///
/// - entries the SDL-only root **accepts** and entries it **rejects**, so neither verdict
///   function is a constant over this corpus;
/// - entries on which the two alternate roots **disagree with each other**, which is what proves
///   `parse_type_system_document` and `parse_executable_document` are two functions rather than
///   one name twice.
#[test]
fn both_alternate_roots_agree_with_their_syntactic_counterparts() {
  let mut sdl_accepted = 0usize;
  let mut sdl_rejected = 0usize;
  let mut roots_disagree = 0usize;

  for entry in corpus_files() {
    let src = std::fs::read_to_string(&entry).unwrap();
    let name = entry.display();

    let lossless_sdl = parse_type_system_document(&src).has_errors();
    let lossless_executable = parse_executable_document(&src).has_errors();

    assert_eq!(
      lossless_sdl,
      syntactic_type_system_has_errors(&src),
      "{name}: the SDL-only roots disagree — lossless says has_errors={lossless_sdl}"
    );
    assert_eq!(
      lossless_executable,
      syntactic_executable_has_errors(&src),
      "{name}: the executable-only roots disagree — lossless says \
       has_errors={lossless_executable}"
    );

    if lossless_sdl {
      sdl_rejected += 1;
    } else {
      sdl_accepted += 1;
    }
    if lossless_sdl != lossless_executable {
      roots_disagree += 1;
    }
  }

  assert!(
    sdl_accepted > 0 && sdl_rejected > 0,
    "the SDL-only verdict is a constant over this corpus ({sdl_accepted} accepted, \
     {sdl_rejected} rejected); a parity assertion against it proves nothing"
  );
  assert!(
    roots_disagree >= 20,
    "only {roots_disagree} corpus entries separate the SDL-only entry point from the \
     executable-only one; the two are being compared against oracles that cannot tell them apart"
  );
}

/// The eleven constructs Task 11 measured as divergences, now agreed **and still whole**.
///
/// Each is a corpus entry, so [`both_suites_agree_on_every_corpus_entry`] already holds the
/// verdict. What a corpus entry cannot hold is the other half of the ruling: the rejection is a
/// *diagnostic*, and the tree is built anyway. Asserting the verdict alone would pass just as
/// well if the lossless suite had learned to reject these by bailing out — which would cost the
/// nodes and the bytes a lossless consumer exists for. So the text is asserted too, byte for
/// byte, against the file the corpus keeps.
#[test]
fn the_eleven_former_divergences_are_rejected_without_losing_their_text() {
  const WITNESSES: &[&str] = &[
    "invalid_const_default_in_arguments_definition.graphql",
    "invalid_const_default_in_directive_definition.graphql",
    "invalid_const_default_in_input_object.graphql",
    "invalid_const_default_in_variable_definition.graphql",
    "invalid_const_directive_on_object_type.graphql",
    "invalid_const_directive_on_enum_value.graphql",
    "invalid_const_directive_on_schema.graphql",
    "invalid_const_directive_on_scalar.graphql",
    "invalid_const_directive_on_field_definition.graphql",
    "invalid_const_directive_on_extension.graphql",
    "invalid_fragment_named_on.graphql",
  ];

  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  for name in WITNESSES {
    let path = dir.join(name);
    let src = std::fs::read_to_string(&path)
      .unwrap_or_else(|e| panic!("{name} must exist in the shared corpus: {e}"));

    assert!(
      lossless_has_errors(&src),
      "{name}: the lossless suite accepted it — the const or fragment-name rule has regressed"
    );
    assert!(
      syntactic_has_errors(&src),
      "{name}: the syntactic suite accepted it — this was never a divergence"
    );
    assert_eq!(
      parse_str(&src).syntax().text().to_string(),
      src,
      "{name}: a rejected parse still keeps every byte"
    );
  }
}

/// The compatibility extension both suites share, pinned as a *non*-divergence.
///
/// A leading `Description` on an **executable** definition is not GraphQL — `apollo-parser`
/// rejects it — but `syntactic/`'s `described_definition` accepts it for frozen-parser
/// compatibility, and `lossless/document.rs` matches that deliberately. It is therefore a
/// divergence from the *spec*, not between the two suites, and the parity gate must stay silent
/// about it: `valid_executable_described_operation.graphql` carries it as an ordinary valid
/// entry. This test states the reason that entry is filed where it is.
///
/// The neighbouring case is the opposite ruling and is pinned the same way: a description before
/// an **extension** is rejected by both (`invalid_described_extension.graphql`), because
/// `syntactic/`'s described path commits to a definition and can never reach an extension.
#[test]
fn a_description_before_an_executable_definition_is_accepted_by_both() {
  for src in [
    "\"doc\" query Q { f }",
    "\"doc\" mutation M { f }",
    "\"doc\" fragment F on T { f }",
    "\"doc\" { f }",
  ] {
    assert!(!lossless_has_errors(src), "lossless rejected {src:?}");
    assert!(!syntactic_has_errors(src), "syntactic rejected {src:?}");
  }

  for src in [
    "\"doc\" extend type T { f: Int }",
    "\"doc\" extend scalar S @d",
  ] {
    assert!(lossless_has_errors(src), "lossless accepted {src:?}");
    assert!(syntactic_has_errors(src), "syntactic accepted {src:?}");
  }
}

/// What a verdict gate structurally cannot measure.
///
/// A definition that fails partway through its body unwinds before `node_at` spends its mark, so
/// the node is never opened and the tree keeps the bytes as bare children of `Document`.
/// `apollo-parser` opens the node first and keeps it. Recorded by Task 10 and deliberately left
/// unfixed — it is Task 8 production behaviour, and moving it would move shapes that Task 8's
/// tests and Task 13b's goldens pin.
///
/// **Gate 1 is blind to it, and so is gate 3.** Both suites reject these inputs, so the parity
/// comparison is satisfied and correct; every byte survives, so a round-trip gate is satisfied
/// and correct. A whole class of structural loss lives in the space between those two greens.
/// This test is the record of that blind spot, written as an assertion so the day the behaviour
/// changes is a day someone is told.
///
/// # The loss is not "any failure in a body"
///
/// The control set below is what makes that precise, and it was found by mutation: replacing the
/// control source with `type T { x: }` changed nothing, because that input **keeps** its
/// `ObjectTypeDefinition` — the missing type recovers in place as an `Error` child and no `Err`
/// ever unwinds. The node is lost only when a production returns `Err` that propagates past the
/// `node_at` holding the mark. Asserting both halves is what stops this test degenerating into
/// "some erroring inputs lack the node", which would be true of `{ f }` as well.
#[test]
fn a_verdict_gate_is_blind_to_a_lost_definition_node() {
  const LOST: K = K::ObjectTypeDefinition;

  /// Bodies that fail by **unwinding**: the mark is never spent and the node never opens.
  const LOSES_THE_NODE: &[&str] = &[
    "type T { \"\"\"b\"\"\" \"a\" }",
    "type { x: Int }",
    "type T { : Int }",
    "type T { x Int }",
    // Padded, so the round-trip assertion below has something to be wrong about: over
    // whitespace-free witnesses alone, comparing against `src.trim()` is indistinguishable from
    // comparing against `src` (mutation, in-space silent pass).
    "\n  type T { x Int }\n",
  ];

  /// The control, and the sharp edge of the finding: these fail too — the parity gate rejects
  /// every one of them — yet each keeps its `ObjectTypeDefinition`, because each recovers in
  /// place instead of unwinding. Without this half, `!contains` would be satisfiable by any
  /// input at all that happens not to be an object type.
  const KEEPS_THE_NODE: &[&str] = &["type T { x: }", "type T { x: Int", "type T {}"];

  fn kinds_of(src: &str) -> Vec<K> {
    parse_str(src)
      .syntax()
      .descendants()
      .map(|n| n.kind())
      .collect()
  }

  for src in LOSES_THE_NODE.iter().copied() {
    // Both suites reject — so the parity loop above would pass this entry without comment.
    assert!(lossless_has_errors(src), "lossless accepted {src:?}");
    assert!(syntactic_has_errors(src), "syntactic accepted {src:?}");

    // Every byte is still there — so a round-trip gate would pass it without comment too.
    assert_eq!(
      parse_str(src).syntax().text().to_string(),
      src,
      "{src:?}: the text did not round-trip"
    );

    // And yet the node is gone.
    let kinds = kinds_of(src);
    assert!(
      !kinds.contains(&LOST),
      "{src:?}: expected no {LOST:?} in {kinds:?} — if the node is back, this blind spot has \
       been closed and the test should become the assertion that it stays closed"
    );
  }

  for src in KEEPS_THE_NODE.iter().copied() {
    assert!(
      lossless_has_errors(src),
      "control {src:?} was supposed to be a failing parse"
    );
    let kinds = kinds_of(src);
    assert!(
      kinds.contains(&LOST),
      "control {src:?}: {LOST:?} is missing from {kinds:?} — a failing parse that recovers in \
       place must still carry its definition node, or the assertion above measures nothing"
    );
  }
}
