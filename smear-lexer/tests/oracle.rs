//! Frozen golden-token oracle.
//!
//! Captures the CURRENT (Logos) lexers' output for a fixture corpus + a
//! malformed-input set, so the SIMD rewrite (phases 2-4) can be diff-tested
//! against it, and the snapshots survive as the regression oracle after Logos
//! is deleted (phase 5).
//!
//! Regenerate after an *intended* change:  BLESS=1 cargo test -p smear-lexer --test oracle
//! Verify (CI default):                     cargo test -p smear-lexer --test oracle

use std::{fs, path::PathBuf};

use tokit::lexer::Lexer as _;

/// Render a lexer's full stream to a stable, diffable string.
///
/// `Ok`  → `OK  <start>..<end>  <Debug of token>`
/// `Err` → `ERR <Debug of errors>`  (the error value carries its own span)
macro_rules! render_stream {
  ($lexer:expr) => {{
    let mut lex = $lexer;
    let mut out = String::new();
    while let Some(item) = lex.lex() {
      match item {
        Ok(tok) => {
          let span = lex.span();
          out.push_str(&format!(
            "OK  {}..{}  {:?}\n",
            span.start(),
            span.end(),
            tok
          ));
        }
        Err(errs) => {
          out.push_str(&format!("ERR {:?}\n", errs));
        }
      }
    }
    out
  }};
}

fn oracle_path(group: &str, name: &str) -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests/oracle")
    .join(group)
    .join(format!("{name}.txt"))
}

/// Compare `rendered` against the committed golden file, or (with BLESS=1) write it.
///
/// On mismatch, records `group/name` into `mismatches` instead of asserting
/// immediately, so a single test run can report every mismatching fixture
/// at once instead of aborting at the first one (useful once an in-progress
/// SIMD lexer mismatches many fixtures simultaneously). A missing golden
/// file is still a hard, immediate panic — that's a setup error, not a
/// mismatch to aggregate. BLESS mode still writes unconditionally.
fn check(group: &str, name: &str, rendered: &str, mismatches: &mut Vec<String>) {
  let path = oracle_path(group, name);
  if std::env::var_os("BLESS").is_some() {
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(&path, rendered).unwrap();
    return;
  }
  let expected = fs::read_to_string(&path).unwrap_or_else(|_| {
    panic!("missing golden file {path:?}; run: BLESS=1 cargo test -p smear-lexer --test oracle")
  });
  if rendered != expected {
    mismatches.push(format!("{group}/{name}"));
  }
}

/// Panics with the full list of mismatched `group/name` fixtures, if any.
fn assert_no_mismatches(mismatches: &[String]) {
  assert!(
    mismatches.is_empty(),
    "\noracle mismatch for {} fixture(s):\n  {}\n(if this change is intended: BLESS=1 cargo test -p smear-lexer --test oracle)\n",
    mismatches.len(),
    mismatches.join("\n  "),
  );
}

/// Valid fixtures spanning query + schema shapes and sizes.
const CORPUS: &[(&str, &str)] = &[
  (
    "tiny",
    include_str!("../../smear/tests/fixtures/executables/bench_01_tiny_simple.graphql"),
  ),
  (
    "medium_fragments",
    include_str!("../../smear/tests/fixtures/executables/bench_05_medium_fragments.graphql"),
  ),
  (
    "huge",
    include_str!("../../smear/tests/fixtures/executables/bench_10_huge_comprehensive.graphql"),
  ),
  (
    "kitchen_sink",
    include_str!("../../smear/tests/fixtures/executables/kitchen-sink_canonical.graphql"),
  ),
  (
    "schema_minimal",
    include_str!("../../smear/tests/fixtures/schemas/minimal.graphql"),
  ),
  (
    "schema_gmx",
    include_str!("../../smear/tests/fixtures/schemas/gmx_schema.graphql"),
  ),
];

/// Malformed inputs — one per Spec §6 number-error case + string edge cases.
const MALFORMED: &[(&str, &str)] = &[
  // number errors
  ("num_leading_zero_int", "{ a(x: 007) }"),
  ("num_leading_zero_neg", "{ a(x: -00) }"),
  ("num_suffix_int", "{ a(x: 123abc) }"),
  ("num_suffix_float", "{ a(x: 1.5x) }"),
  ("num_bare_fraction", "{ a(x: 1.) }"),
  ("num_bare_exponent", "{ a(x: 1e) }"),
  ("num_bare_exponent_sign", "{ a(x: 1e+) }"),
  ("num_missing_int_part", "{ a(x: .5) }"),
  ("num_stray_plus", "{ a(x: +) }"),
  ("num_stray_minus", "{ a(x: -) }"),
  ("num_leading_zero_and_fraction", "{ a(x: 00.5) }"),
  ("num_leading_zero_and_exponent", "{ a(x: 00e5) }"),
  // string edges
  ("str_unterminated_inline", "{ a(x: \"oops) }"),
  ("str_bad_escape", "{ a(x: \"a\\qb\") }"),
  ("str_bad_unicode", "{ a(x: \"\\u12zz\") }"),
  ("str_unterminated_block", "{ a(x: \"\"\"oops) }"),
  // structural
  ("bad_spread", "{ ..x }"),
  ("unknown_byte", "{ a ? b }"),
];

/// Edge-case fixtures targeting the highest-risk lexer paths that the
/// original `CORPUS`/`MALFORMED` sets left thin: block/inline string
/// escapes, valid floats, the combined two-error number path, BOM
/// handling, and union/intersection punctuation.
///
/// Encoded with raw strings (`r#"..."#`) wherever the fixture needs a
/// literal backslash to reach the GraphQL source (escapes, `\"""`), so
/// rustc does not itself interpret the escape before the lexer ever sees
/// it. `bom_prefixed` is the deliberate exception: there we *want* rustc
/// to decode `\u{FEFF}` into the real BOM byte sequence.
const EDGES: &[(&str, &str)] = &[
  // block strings (GraphQL block string = """..."""; only escape is \""")
  (
    "blk_common_indent",
    "{ a(x: \"\"\"foo\n    bar\n    baz\"\"\") }",
  ),
  ("blk_blank_lines", "{ a(x: \"\"\"\ncontent\n\n\"\"\") }"),
  (
    "blk_crlf",
    "{ a(x: \"\"\"line1\r\nline2\rline3\"\"\") }",
  ),
  ("blk_escaped_triple", r#"{ a(x: """foo\"""bar""") }"#),
  (
    "blk_multiline_plain",
    "{ a(x: \"\"\"line one\nline two\nline three\"\"\") }",
  ),
  // inline strings (valid escapes: \" \\ \/ \b \f \n \r \t, \uXXXX, and
  // per this crate \u{...} braced)
  //
  // NOTE on str_fixed_unicode / str_surrogate_pair: the task brief shows
  // these as the *decoded* characters (`"A"`, `"😀"`) — almost certainly
  // because the brief itself went through a pipeline that silently
  // resolved valid `\uXXXX` / surrogate-pair escapes (0x0041 == 'A';
  // D83D/DE00 is exactly the UTF-16 surrogate pair for U+1F600 😀) while
  // leaving `\u{1F600}` (not a bare-\u escape) and the lone, undecodable
  // surrogates untouched. A literal "A" or "😀" here would only exercise
  // `LitPlainStr`, not the fixed/surrogate escape path this fixture is
  // named for — so we restore the literal `\u` source text via raw
  // strings, per the task's own escaping-gotcha warning.
  ("str_simple_escapes", r#"{ a(x: "a\tb\nc\"d\\e\/f") }"#),
  ("str_fixed_unicode", r#"{ a(x: "\u0041") }"#),
  ("str_braced_unicode", r#"{ a(x: "\u{1F600}") }"#),
  ("str_surrogate_pair", r#"{ a(x: "\uD83D\uDE00") }"#),
  ("str_unpaired_high_surrogate", r#"{ a(x: "\uD83D") }"#),
  ("str_unpaired_low_surrogate", r#"{ a(x: "\uDE00") }"#),
  // valid floats (CORPUS/MALFORMED previously had zero valid LitFloat tokens)
  (
    "num_valid_floats",
    "{ a(x: 3.14, y: 1.5e10, z: -2.5, w: 0.0) }",
  ),
  // two-error number path: leading-zeros arm chained with a second,
  // distinct bare-continuation error (00.5 / 00e5 do NOT hit this — they
  // take the single-error longest-match arm instead).
  ("num_lz_bare_fraction", "{ a(x: 00.) }"),
  ("num_lz_bare_exponent", "{ a(x: 00e) }"),
  ("num_lz_frac_bad_exp", "{ a(x: 00.5e) }"),
  // BOM: dedicated lossless token + skip regex, previously zero coverage
  ("bom_prefixed", "\u{FEFF}{ a }"),
  // union / intersection punctuation (Pipe / Ampersand), never produced
  // by CORPUS/MALFORMED today
  (
    "sdl_union_and_interface",
    "union U = A | B  type T implements A & B { f: Int }",
  ),
];

#[test]
fn graphql_syntactic_oracle() {
  use smear_lexer::graphql::syntactic::SyntacticLexer;
  let mut mismatches = Vec::new();
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let rendered = render_stream!(SyntacticLexer::<&str>::new(src));
    check("graphql-syntactic", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

// ─── SIMD-vs-golden parity (phase 1b Task 2) ──────────────────────────────
//
// `SimdSyntacticLexer::<str>` produces `Char = char` tokens/errors -- exactly
// what the `&str`-sourced Logos oracle above was captured from -- so its
// render is byte-for-byte identical to the golden files. No conversion is
// needed (contrast the phase 1a version of this test, which could only drive
// `SimdSyntacticLexer::<[u8]>` and had to paper over the resulting `Char = u8`
// vs. `Char = char` `Debug` mismatch with a pair of text-rewriting hacks).

#[test]
fn graphql_syntactic_simd_oracle() {
  use smear_lexer::graphql::simd::SimdSyntacticLexer;
  let mut mismatches = Vec::new();
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let rendered = render_stream!(SimdSyntacticLexer::<str>::new(src));
    check("graphql-syntactic", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

// ─── SIMD source matrix: `<[u8]>` vs `<str>` (Task 2) ──────────────────────
//
// `graphql_syntactic_simd_oracle` above proves `SimdSyntacticLexer::<str>`
// matches the golden files byte-for-byte. The byte-flavored sources
// (`Char = u8`) can't be compared against those same golden files with
// `render_stream!`: `{:?}` renders `Char = u8` differently from `Char = char`
// even for identical content (e.g. `Identifier([97])` vs. `Identifier("a")`),
// and re-deriving a text transform to paper over that is exactly the parity
// hack this task retires (see the git history of this file for the deleted
// `bytes_token_to_str` / `normalize_char_field` / `render_simd_stream_as_str`
// helpers).
//
// Instead, this section proves *source-agnostic equivalence* directly: for
// every fixture, the `<[u8]>` token+error stream must carry the same kinds,
// spans, and decoded content as the `<str>` stream -- transitively proving it
// matches the golden files too, without ever comparing `Debug` text across
// `Char` types. The comparators below are structural (they pattern-match each
// error/token shape down to its `Char` leaf), not textual, so they can't be
// fooled by formatting coincidences: anything they don't recognize is a hard
// mismatch (`_ => false`), never a silent pass.
//
// The four owned/shared sources are covered too (each feature-gated), driven
// through the same lockstep comparison. They split by the primitive Logos
// scans (see `ScanSource` in `graphql/simd.rs`): `Bytes`/`BStr`/`HipByt` scan
// `[u8]` (`Char = u8`), so they reuse `same_lexer_errors` (u8-vs-char) exactly
// like `<[u8]>`; `HipStr` scans `str` (`Char = char`), so its errors are the
// *same* concrete type as the `<str>` reference's and compare by plain `==`.
// Tokens compare via `same_token` for all of them — it is generic over any
// `AsBytes` slice, so it handles `Bytes`, `&[u8]`, and `HipStr`/`HipByt` alike.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  error::{
    EscapedCharacter, FixedUnicodeEscapeError, InvalidUnicodeHexDigits, InvalidUnicodeSequence,
    StringError, StringErrors, UnicodeError,
  },
  graphql::{
    error::{DecimalError, FloatError},
    simd::{AsBytes, SimdSyntacticLexer},
    syntactic::{
      SyntacticLexerError, SyntacticLexerErrorData, SyntacticLexerErrors, SyntacticToken,
    },
  },
};
use tokit::{
  error::UnexpectedLexeme,
  utils::{Lexeme, PositionedChar},
};

/// True if two positioned characters describe the same source position and
/// (mod `u8` vs `char`) codepoint. Every byte the lexer ever flags this way
/// is ASCII, so `as u32` is a lossless, direct comparison of the two.
fn same_positioned_char(byte: &PositionedChar<u8>, str_: &PositionedChar<char>) -> bool {
  byte.position() == str_.position() && byte.char() as u32 == str_.char() as u32
}

/// True if two lexemes describe the same character-or-range, allowing the
/// `Char` leaf to differ in representation (`u8` vs `char`) but not in value.
fn same_lexeme(byte: &Lexeme<u8>, str_: &Lexeme<char>) -> bool {
  match (byte, str_) {
    (Lexeme::Char(b), Lexeme::Char(s)) => same_positioned_char(b, s),
    (Lexeme::Range(b), Lexeme::Range(s)) => b == s,
    _ => false,
  }
}

/// True if two "unexpected lexeme" errors carry the same lexeme and hint.
/// `Hint` never depends on `Char`, so it's the same concrete type either
/// side and compares directly.
fn same_unexpected_lexeme<Hint: PartialEq>(
  byte: &UnexpectedLexeme<u8, Hint>,
  str_: &UnexpectedLexeme<char, Hint>,
) -> bool {
  same_lexeme(byte.lexeme(), str_.lexeme()) && byte.hint() == str_.hint()
}

fn same_float_error(byte: &FloatError<u8>, str_: &FloatError<char>) -> bool {
  match (byte, str_) {
    (FloatError::UnexpectedSuffix(b), FloatError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (FloatError::UnexpectedLexeme(b), FloatError::UnexpectedLexeme(s)) => {
      same_unexpected_lexeme(b, s)
    }
    // `UnexpectedEnd<FloatHint>` carries no `Char` at all, so both sides are
    // literally the same concrete type -- plain equality applies.
    (FloatError::UnexpectedEnd(b), FloatError::UnexpectedEnd(s)) => b == s,
    (FloatError::LeadingZeros(b), FloatError::LeadingZeros(s)) => same_lexeme(b, s),
    (FloatError::MissingIntegerPart, FloatError::MissingIntegerPart) => true,
    _ => false,
  }
}

fn same_decimal_error(byte: &DecimalError<u8>, str_: &DecimalError<char>) -> bool {
  match (byte, str_) {
    (DecimalError::UnexpectedSuffix(b), DecimalError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (DecimalError::UnexpectedEnd(b), DecimalError::UnexpectedEnd(s)) => b == s,
    (DecimalError::LeadingZeros(b), DecimalError::LeadingZeros(s)) => same_lexeme(b, s),
    _ => false,
  }
}

fn same_escaped_character(byte: &EscapedCharacter<u8>, str_: &EscapedCharacter<char>) -> bool {
  byte.span() == str_.span()
    && byte.position() == str_.position()
    && byte.char() as u32 == str_.char() as u32
}

fn same_invalid_unicode_hex_digits(
  byte: &InvalidUnicodeHexDigits<u8>,
  str_: &InvalidUnicodeHexDigits<char>,
) -> bool {
  byte.len() == str_.len()
    && byte
      .iter()
      .zip(str_.iter())
      .all(|(b, s)| same_positioned_char(b, s))
}

fn same_invalid_unicode_sequence(
  byte: &InvalidUnicodeSequence<u8>,
  str_: &InvalidUnicodeSequence<char>,
) -> bool {
  byte.span() == str_.span()
    && same_invalid_unicode_hex_digits(byte.digits_ref(), str_.digits_ref())
}

fn same_fixed_unicode_escape_error(
  byte: &FixedUnicodeEscapeError<u8>,
  str_: &FixedUnicodeEscapeError<char>,
) -> bool {
  match (byte, str_) {
    (FixedUnicodeEscapeError::Incomplete(b), FixedUnicodeEscapeError::Incomplete(s)) => {
      same_lexeme(b, s)
    }
    (FixedUnicodeEscapeError::InvalidSequence(b), FixedUnicodeEscapeError::InvalidSequence(s)) => {
      same_invalid_unicode_sequence(b, s)
    }
    (
      FixedUnicodeEscapeError::UnpairedSurrogate(b),
      FixedUnicodeEscapeError::UnpairedSurrogate(s),
    ) => same_unexpected_lexeme(b, s),
    _ => false,
  }
}

fn same_unicode_error(byte: &UnicodeError<u8>, str_: &UnicodeError<char>) -> bool {
  match (byte, str_) {
    (UnicodeError::Fixed(b), UnicodeError::Fixed(s)) => same_fixed_unicode_escape_error(b, s),
    // `BracedUnicodeEscapeError` carries no `Char` at all -- same concrete
    // type either side.
    (UnicodeError::Braced(b), UnicodeError::Braced(s)) => b == s,
    _ => false,
  }
}

fn same_string_error(byte: &StringError<u8>, str_: &StringError<char>) -> bool {
  match (byte, str_) {
    (StringError::UnsupportedCharacter(b), StringError::UnsupportedCharacter(s)) => {
      same_lexeme(b, s)
    }
    (StringError::UnexpectedLineTerminator(b), StringError::UnexpectedLineTerminator(s)) => {
      same_unexpected_lexeme(b, s)
    }
    (StringError::UnexpectedEscapedCharacter(b), StringError::UnexpectedEscapedCharacter(s)) => {
      same_escaped_character(b, s)
    }
    // `Unterminated`'s payload (`UnexpectedEnd<LitStrDelimiterHint>`) carries
    // no `Char` -- same concrete type either side.
    (StringError::Unterminated(b), StringError::Unterminated(s)) => b == s,
    (StringError::Unicode(b), StringError::Unicode(s)) => same_unicode_error(b, s),
    (StringError::Other(b), StringError::Other(s)) => b == s,
    // `Unopened` (`UnexpectedLexeme<Option<Char>, _>`) is structurally
    // unreachable from `SimdSyntacticLexer::lex()` (the only entry into
    // string lexing is the `b'"'` dispatch arm) and no fixture exercises it;
    // falling through to the mismatch case below rather than adding an
    // `Option<Char>`-flavored comparator for zero coverage.
    _ => false,
  }
}

fn same_string_errors(byte: &StringErrors<u8>, str_: &StringErrors<char>) -> bool {
  byte.len() == str_.len()
    && byte
      .iter()
      .zip(str_.iter())
      .all(|(b, s)| same_string_error(b, s))
}

fn same_error_data(
  byte: &SyntacticLexerErrorData<u8>,
  str_: &SyntacticLexerErrorData<char>,
) -> bool {
  match (byte, str_) {
    (SyntacticLexerErrorData::Float(b), SyntacticLexerErrorData::Float(s)) => {
      same_float_error(b, s)
    }
    (SyntacticLexerErrorData::Int(b), SyntacticLexerErrorData::Int(s)) => same_decimal_error(b, s),
    (SyntacticLexerErrorData::String(b), SyntacticLexerErrorData::String(s)) => {
      same_string_errors(b, s)
    }
    (
      SyntacticLexerErrorData::UnexpectedLexeme(b),
      SyntacticLexerErrorData::UnexpectedLexeme(s),
    ) => same_lexeme(b, s),
    (SyntacticLexerErrorData::UnknownLexeme(b), SyntacticLexerErrorData::UnknownLexeme(s)) => {
      same_lexeme(b, s)
    }
    (
      SyntacticLexerErrorData::UnexpectedEndOfInput,
      SyntacticLexerErrorData::UnexpectedEndOfInput,
    ) => true,
    (
      SyntacticLexerErrorData::UnterminatedSpreadOperator,
      SyntacticLexerErrorData::UnterminatedSpreadOperator,
    ) => true,
    // The recursion-limit state error never depends on `Char`.
    (SyntacticLexerErrorData::State(b), SyntacticLexerErrorData::State(s)) => b == s,
    (SyntacticLexerErrorData::InvalidUtf8(b), SyntacticLexerErrorData::InvalidUtf8(s)) => b == s,
    (SyntacticLexerErrorData::Other(b), SyntacticLexerErrorData::Other(s)) => b == s,
    _ => false,
  }
}

fn same_lexer_error(byte: &SyntacticLexerError<u8>, str_: &SyntacticLexerError<char>) -> bool {
  byte.span() == str_.span() && same_error_data(byte.data(), str_.data())
}

fn same_lexer_errors(byte: &SyntacticLexerErrors<u8>, str_: &SyntacticLexerErrors<char>) -> bool {
  byte.len() == str_.len()
    && byte
      .iter()
      .zip(str_.iter())
      .all(|(b, s)| same_lexer_error(b, s))
}

/// True if two inline-string tokens describe the same lexical result:
/// identical raw source bytes, identical `Plain`/`Complex` classification,
/// and -- when `Complex` -- identical derived normalization metadata
/// (`required_capacity`). Comparing only the span text would miss a lexer
/// that classifies the same bytes differently or derives different
/// metadata for them, so every accessor is checked, not just the source.
fn same_inline_str<B: AsBytes>(byte: &LitInlineStr<B>, str_: &LitInlineStr<&str>) -> bool {
  if byte.source_ref().as_bytes() != str_.source_ref().as_bytes() {
    return false;
  }
  if byte.is_plain() != str_.is_plain() || byte.is_complex() != str_.is_complex() {
    return false;
  }
  match (byte, str_) {
    (LitInlineStr::Complex(b), LitInlineStr::Complex(s)) => {
      b.required_capacity() == s.required_capacity()
    }
    _ => true,
  }
}

/// True if two block-string tokens describe the same lexical result: same
/// raw source bytes, same `Plain`/`Complex` classification, and -- when
/// `Complex` -- the same derived normalization metadata across every field
/// (`required_capacity`, CR-terminator flag, leading/trailing blank-line
/// counts, common indent, total line count, escaped-triple-quote count).
/// Block strings are produced by two independently hand-written lexers
/// (`string_lexer/block/str.rs` for `<str>`, `block/u8_slice.rs` for
/// `<[u8]>` -- the latter has no unit tests of its own), so this is the one
/// place that proves they agree on more than just the matched span text.
fn same_block_str<B: AsBytes>(byte: &LitBlockStr<B>, str_: &LitBlockStr<&str>) -> bool {
  if byte.source_ref().as_bytes() != str_.source_ref().as_bytes() {
    return false;
  }
  if byte.is_plain() != str_.is_plain() || byte.is_complex() != str_.is_complex() {
    return false;
  }
  match (byte, str_) {
    (LitBlockStr::Complex(b), LitBlockStr::Complex(s)) => {
      b.required_capacity() == s.required_capacity()
        && b.has_cr_terminators() == s.has_cr_terminators()
        && b.leading_blank_lines() == s.leading_blank_lines()
        && b.trailing_blank_lines() == s.trailing_blank_lines()
        && b.common_indent() == s.common_indent()
        && b.total_lines() == s.total_lines()
        && b.num_escaped_triple_quotes() == s.num_escaped_triple_quotes()
    }
    _ => true,
  }
}

/// True if a byte-sourced token and a `str`-sourced token describe the same
/// syntactic token: same kind (via `SyntacticToken::kind()`, which drops the
/// generic source payload -- `kind()` is a bijection over the variant set, so
/// equal kinds imply the same variant tag on both sides) and, for the five
/// payload-bearing variants, the same decoded content. For `LitInlineStr`/
/// `LitBlockStr`, "same decoded content" means the same `Plain`/`Complex`
/// structure and normalization metadata (see `same_inline_str`/
/// `same_block_str`), not just equal span bytes -- two block strings in
/// particular can share a span yet disagree on classification or metadata if
/// the two hand-written lexers (str vs. `[u8]`) diverge.
fn same_token<B: AsBytes>(byte: &SyntacticToken<B>, str_: &SyntacticToken<&str>) -> bool {
  use SyntacticToken as Tok;
  if byte.kind() != str_.kind() {
    return false;
  }
  match (byte, str_) {
    (Tok::Identifier(b), Tok::Identifier(s))
    | (Tok::LitInt(b), Tok::LitInt(s))
    | (Tok::LitFloat(b), Tok::LitFloat(s)) => b.as_bytes() == s.as_bytes(),
    (Tok::LitInlineStr(b), Tok::LitInlineStr(s)) => same_inline_str(b, s),
    (Tok::LitBlockStr(b), Tok::LitBlockStr(s)) => same_block_str(b, s),
    // Every other variant carries no payload; the `kind()` check above
    // already confirmed they're the same punctuation/spread token.
    _ => true,
  }
}

/// Drives an owned/shared-source `SimdSyntacticLexer` and the reference
/// `<str>`-flavored one over the same fixture text, in lockstep, asserting
/// every token/error pair is source-agnostically equivalent (see `same_token`
/// / `same_lexer_errors` above). `$label` identifies the flavor in failure
/// messages (e.g. `"[u8]"`, `"Bytes"`). `$err_eq` is the error comparator:
/// `same_lexer_errors` for the `[u8]`-primitive sources (`Char = u8`), or
/// plain `==` for `HipStr` (`Char = char`, same error type as `<str>`).
macro_rules! assert_matches_str_stream {
  ($label:expr, $fixture:expr, $byte_lexer:expr, $str_src:expr, $err_eq:expr) => {{
    let mut byte_lex = $byte_lexer;
    let mut str_lex = SimdSyntacticLexer::<str>::new($str_src);
    let mut idx = 0usize;
    loop {
      match (byte_lex.lex(), str_lex.lex()) {
        (None, None) => break,
        (Some(Ok(bt)), Some(Ok(st))) => {
          assert_eq!(
            byte_lex.span(),
            str_lex.span(),
            "{}/{}#{idx}: span mismatch (byte={bt:?} str={st:?})",
            $label,
            $fixture
          );
          assert!(
            same_token(&bt, &st),
            "{}/{}#{idx}: content mismatch: byte={bt:?} str={st:?}",
            $label,
            $fixture
          );
        }
        (Some(Err(be)), Some(Err(se))) => {
          assert!(
            $err_eq(&be, &se),
            "{}/{}#{idx}: error mismatch:\n  byte={be:?}\n  str={se:?}",
            $label,
            $fixture
          );
        }
        (b, s) => panic!(
          "{}/{}#{idx}: shape mismatch: byte={b:?} str={s:?}",
          $label, $fixture
        ),
      }
      idx += 1;
    }
  }};
}

#[test]
fn graphql_syntactic_simd_source_matrix_u8() {
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    assert_matches_str_stream!(
      "[u8]",
      name,
      SimdSyntacticLexer::<[u8]>::new(src.as_bytes()),
      src,
      same_lexer_errors
    );
  }
}

#[cfg(feature = "bytes")]
#[test]
fn graphql_syntactic_simd_source_matrix_bytes() {
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let owned = bytes::Bytes::copy_from_slice(src.as_bytes());
    assert_matches_str_stream!(
      "Bytes",
      name,
      SimdSyntacticLexer::<bytes::Bytes>::new(&owned),
      src,
      same_lexer_errors
    );
  }
}

#[cfg(feature = "bstr")]
#[test]
fn graphql_syntactic_simd_source_matrix_bstr() {
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    assert_matches_str_stream!(
      "BStr",
      name,
      SimdSyntacticLexer::<bstr::BStr>::new(bstr::BStr::new(src.as_bytes())),
      src,
      same_lexer_errors
    );
  }
}

#[cfg(feature = "hipstr")]
#[test]
fn graphql_syntactic_simd_source_matrix_hipstr() {
  // `HipStr` scans `str`, so its errors are `SyntacticLexerErrors<char>` —
  // the exact type the `<str>` reference produces — and compare by `==`.
  fn eq_char(byte: &SyntacticLexerErrors<char>, str_: &SyntacticLexerErrors<char>) -> bool {
    byte == str_
  }
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let owned = hipstr::HipStr::from(*src);
    assert_matches_str_stream!(
      "HipStr",
      name,
      SimdSyntacticLexer::<hipstr::HipStr<'_>>::new(&owned),
      src,
      eq_char
    );
  }
}

#[cfg(feature = "hipstr")]
#[test]
fn graphql_syntactic_simd_source_matrix_hipbyt() {
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let owned = hipstr::HipByt::from(src.as_bytes());
    assert_matches_str_stream!(
      "HipByt",
      name,
      SimdSyntacticLexer::<hipstr::HipByt<'_>>::new(&owned),
      src,
      same_lexer_errors
    );
  }
}

#[test]
fn graphql_lossless_oracle() {
  use smear_lexer::graphql::lossless::LosslessLexer;
  let mut mismatches = Vec::new();
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let rendered = render_stream!(LosslessLexer::<&str>::new(src));
    check("graphql-lossless", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

/// GraphQLx-specific inputs: prefixed-radix numbers + extra punctuation.
#[cfg(feature = "graphqlx")]
const GRAPHQLX_EXTRA: &[(&str, &str)] = &[
  ("gx_hex_int", "type T { f(x: Int = 0xFF): Int }"),
  ("gx_binary_int", "type T { f(x: Int = 0b1010): Int }"),
  ("gx_octal_int", "type T { f(x: Int = 0o755): Int }"),
  ("gx_hex_float", "type T { f(x: Float = 0x1.8p3): Float }"),
  ("gx_generic", "type Box<T> { value: T }"),
  ("gx_path_sep", "type T { f: foo::Bar }"),
  ("gx_fat_arrow", "type T { m: <String => Int> }"),
  ("gx_arith", "type T { f(x: Int = 1 + 2 * 3 - 4): Int }"),
  ("gx_bad_hex", "type T { f(x: Int = 0xZZ): Int }"),
  // string coverage: an inline string with an escape + a short block string
  (
    "gx_string_and_block",
    r#""""A short block string.""" type T { f(x: String = "a\nb"): String }"#,
  ),
];

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_syntactic_oracle() {
  use smear_lexer::graphqlx::syntactic::SyntacticLexer;
  let mut mismatches = Vec::new();
  for (name, src) in GRAPHQLX_EXTRA.iter() {
    let rendered = render_stream!(SyntacticLexer::<&str>::new(src));
    check("graphqlx-syntactic", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_lossless_oracle() {
  use smear_lexer::graphqlx::lossless::LosslessLexer;
  let mut mismatches = Vec::new();
  for (name, src) in GRAPHQLX_EXTRA.iter() {
    let rendered = render_stream!(LosslessLexer::<&str>::new(src));
    check("graphqlx-lossless", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}
