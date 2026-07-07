//! Frozen golden-token oracle.
//!
//! Captures the CURRENT (Logos) lexers' output for a fixture corpus + a
//! malformed-input set, so the SIMD rewrite (phases 2-4) can be diff-tested
//! against it, and the snapshots survive as the regression oracle after Logos
//! is deleted (phase 5).
//!
//! Regenerate after an *intended* change:  BLESS=1 cargo test -p smear-lexer --test oracle
//! Verify (CI default):                     cargo test -p smear-lexer --test oracle

// Every fixture, helper, and test below exercises the `graphql` and/or
// `graphqlx` dialect lexers; with both features off there is nothing left to
// test, and leaving the file ungated would turn every helper into dead code
// under that build.
#![cfg(any(feature = "graphql", feature = "graphqlx"))]

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
#[cfg(feature = "graphql")]
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
#[cfg(feature = "graphql")]
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
#[cfg(feature = "graphql")]
const EDGES: &[(&str, &str)] = &[
  // block strings (GraphQL block string = """..."""; only escape is \""")
  (
    "blk_common_indent",
    "{ a(x: \"\"\"foo\n    bar\n    baz\"\"\") }",
  ),
  ("blk_blank_lines", "{ a(x: \"\"\"\ncontent\n\n\"\"\") }"),
  ("blk_crlf", "{ a(x: \"\"\"line1\r\nline2\rline3\"\"\") }"),
  ("blk_escaped_triple", r#"{ a(x: """foo\"""bar""") }"#),
  (
    "blk_multiline_plain",
    "{ a(x: \"\"\"line one\nline two\nline three\"\"\") }",
  ),
  // single-line plain block string: the simplest valid SIMD-emitted block
  // token (Plain, content between the delimiters, no normalization).
  ("blk_single_line_plain", "{ a(x: \"\"\"hello\"\"\") }"),
  // empty block string `""""""`: the content between the delimiters is
  // empty, so the SIMD scan sees `start == 0` — exercises the offset math at
  // its degenerate lower bound (consumed == 0 + 3 closing).
  ("blk_empty", "{ a(x: \"\"\"\"\"\") }"),
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

// ─── SIMD-vs-golden parity (phase 1b Task 2) ──────────────────────────────
//
// `SimdSyntacticLexer::<str>` produces `Char = char` tokens/errors -- exactly
// what these golden files were originally captured from via a live
// Logos-backed lexer over the full `SyntacticToken` grammar (that
// differential test, `graphql_syntactic_oracle`, was deleted in Task 4 of the
// Logos-slimming plan once this SIMD-vs-golden test below had proven the same
// thing more directly; see git history for the deleted test) -- so its render
// is byte-for-byte identical to the golden files. No conversion is needed
// (contrast the phase 1a version of this test, which could only drive
// `SimdSyntacticLexer::<[u8]>` and had to paper over the resulting `Char = u8`
// vs. `Char = char` `Debug` mismatch with a pair of text-rewriting hacks).

#[cfg(feature = "graphql")]
#[test]
fn graphql_syntactic_simd_oracle() {
  use smear_lexer::graphql::syntactic::SimdSyntacticLexer;
  let mut mismatches = Vec::new();
  for (name, src) in CORPUS.iter().chain(MALFORMED).chain(EDGES) {
    let rendered = render_stream!(SimdSyntacticLexer::<str>::new(src));
    check("graphql-syntactic", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

// ─── Low-recursion-limit SIMD parity ───────────────────────────────────────
//
// The frozen oracle above runs at the default limit (500), which no fixture
// approaches, so it cannot catch a fast-path arm that emits a token while the
// depth is already over the limit. `LogosLexer::lex` re-checks the limiter
// after every successful token and, while over the limit, returns the
// recursion error in the token's place; every SIMD fast-path emission must do
// the same. A limit of 0 puts every token after the first bracket over the
// limit, and each input keeps lexing past that first error, so the SIMD
// stream only matches the frozen expectation below if every arm re-checks.
//
// This used to compare the SIMD render directly against a live Logos-backed
// lexer (both `Char = char`, so no golden file was needed). Task 4 of the
// Logos-slimming plan severed that live comparator — the full `SyntacticToken`
// grammar it depended on is deleted in Task 5 — so each expected render below
// is frozen from that comparator's output, captured immediately before
// deletion (see `docs/superpowers/plans/2026-07-06-logos-slimming-phase2.md`,
// Task 4).

#[cfg(feature = "graphql")]
#[test]
fn graphql_syntactic_simd_low_recursion_parity() {
  use tokit::state::recursion_tracker::RecursionLimiter;

  // Each input exercises a different set of gated arms past the first
  // over-limit bracket: identifier + close-bracket, nested brackets, the
  // colon/`LitInt`/`LitFloat` arms, both inline-string arms, and spread.
  const CASES: &[(&str, &str)] = &[
    (
      "{a}",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  2..3  RBrace\n",
    ),
    (
      "{{a}}",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 1, end: 2 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  4..5  RBrace\n",
    ),
    (
      "((x))",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 1, end: 2 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  4..5  RParen\n",
    ),
    (
      "[x]",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  2..3  RBracket\n",
    ),
    (
      "{ a(b: 1, c: 1.5) }",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 3, end: 4 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  18..19  RBrace\n",
    ),
    (
      r#"{ s: "x" }"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  9..10  RBrace\n",
    ),
    (
      r#"{ e: "" }"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  8..9  RBrace\n",
    ),
    // block strings on the SIMD fast path must re-check the limiter through
    // `finish!` too: plain, empty, and complex (common-indent) bodies.
    (
      r#"{ s: """x""" }"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  13..14  RBrace\n",
    ),
    (
      r#"{ e: """""" }"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  12..13  RBrace\n",
    ),
    (
      "{ s: \"\"\"foo\n  bar\"\"\" }",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  21..22  RBrace\n",
    ),
    (
      "{ ...a }",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  7..8  RBrace\n",
    ),
  ];

  for (src, expected) in CASES {
    let simd = render_stream!(SimdSyntacticLexer::<str>::with_state(
      src,
      RecursionLimiter::with_limitation(0),
    ));
    assert_eq!(&simd, expected, "low-limit stream mismatch for {src:?}");
  }
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

// Dialect-agnostic: shared by the GraphQL comparators below and the GraphQLx
// ones further down, so this stays ungated (needed whenever either dialect
// feature is on).
use smear_lexer::{
  LitBlockStr, LitInlineStr,
  error::{
    EscapedCharacter, FixedUnicodeEscapeError, InvalidUnicodeHexDigits, InvalidUnicodeSequence,
    StringError, StringErrors, UnicodeError,
  },
};
// GraphQL-only: the token/error types themselves, plus the GraphQL-flavored
// `SimdSyntacticLexer` used as the `<str>` reference lexer in the matrix
// tests below.
#[cfg(feature = "graphql")]
use smear_lexer::graphql::{
  error::{DecimalError, FloatError},
  syntactic::SimdSyntacticLexer,
  syntactic::{SyntacticLexerError, SyntacticLexerErrorData, SyntacticLexerErrors, SyntacticToken},
};
// `AsBytes` is dialect-agnostic (it's `crate::simd_common::AsBytes` under the
// hood) but only re-exported publicly through each dialect's `simd` module;
// `same_inline_str`/`same_block_str` below need it regardless of which
// dialect is active, so pick it up from whichever is available, preferring
// `graphql` when both are on (the two paths name the same type).
#[cfg(feature = "graphql")]
use smear_lexer::graphql::syntactic::AsBytes;
#[cfg(all(feature = "graphqlx", not(feature = "graphql")))]
use smear_lexer::graphqlx::syntactic::AsBytes;
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

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
fn same_lexer_error(byte: &SyntacticLexerError<u8>, str_: &SyntacticLexerError<char>) -> bool {
  byte.span() == str_.span() && same_error_data(byte.data(), str_.data())
}

#[cfg(feature = "graphql")]
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
#[cfg(feature = "graphql")]
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
#[cfg(feature = "graphql")]
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

#[cfg(feature = "graphql")]
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

#[cfg(all(feature = "graphql", feature = "bytes"))]
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

#[cfg(all(feature = "graphql", feature = "bstr"))]
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

#[cfg(all(feature = "graphql", feature = "hipstr"))]
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

#[cfg(all(feature = "graphql", feature = "hipstr"))]
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

#[cfg(feature = "graphql")]
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

/// Fixtures added for the SIMD lexer to widen coverage past `GRAPHQLX_EXTRA`:
/// a multi-segment path feeding a multi-argument generic, doubly-nested angle
/// brackets behind a fat arrow, and negative radix literals (the `-`-as-sign
/// dispatch that no `GRAPHQLX_EXTRA` fixture reaches — every `-` there is
/// space-separated `Minus`). Their goldens are new; the existing goldens are
/// never re-blessed.
#[cfg(feature = "graphqlx")]
const GRAPHQLX_SIMD_EXTRA: &[(&str, &str)] = &[
  ("gx_nested_path_generic", "type T { f: a::b::C<D, E> }"),
  ("gx_nested_map", "type T { m: <String => List<Int>> }"),
  (
    "gx_negative_literals",
    "type T { f(x: Int = -5, y: Float = -2.5, z: Int = -0xFF): Int }",
  ),
  // Block strings: GRAPHQLX_EXTRA only reaches the single-line Plain path
  // (`gx_string_and_block`). These add the Complex path (common indent →
  // every normalization fact), the empty-body edge, and the unterminated
  // (Logos-delegate) path, so the GraphQLx SIMD block-string hook is proven
  // across all three outcomes.
  (
    "gx_blk_common_indent",
    "type T { f: String = \"\"\"foo\n    bar\n    baz\"\"\" }",
  ),
  ("gx_blk_empty", "type T { f: String = \"\"\"\"\"\" }"),
  ("gx_blk_unterminated", "type T { f: String = \"\"\"oops }"),
  // Unknown byte: `?` is claimed by no GraphQLx token, so the SIMD lexer's
  // hand-rolled `_` arm emits an `UnknownLexeme` error byte-identical to the
  // full grammar's Logos `default_error`. (The GraphQL twin, `unknown_byte`,
  // lives in `MALFORMED`; GraphQLx has no equivalent malformed set, so this
  // SIMD-added fixture is its home.)
  ("gx_unknown_byte", "{ a ? b }"),
];

// `SimdSyntacticLexer::<str>` produces `Char = char` tokens/errors -- exactly
// what these golden files were originally captured from via a live
// Logos-backed lexer over the full `SyntacticToken` grammar (that
// differential test, `graphqlx_syntactic_oracle`, was deleted in Task 4 of
// the Logos-slimming plan once this SIMD-vs-golden test below had proven the
// same thing more directly; see git history for the deleted test) -- so its
// render is byte-for-byte identical to the golden files, no conversion
// needed.
#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_syntactic_simd_oracle() {
  use smear_lexer::graphqlx::syntactic::SimdSyntacticLexer;
  let mut mismatches = Vec::new();
  for (name, src) in GRAPHQLX_EXTRA.iter().chain(GRAPHQLX_SIMD_EXTRA) {
    let rendered = render_stream!(SimdSyntacticLexer::<str>::new(src));
    check("graphqlx-syntactic", name, &rendered, &mut mismatches);
  }
  assert_no_mismatches(&mismatches);
}

// Low-recursion-limit SIMD parity for GraphQLx (see the GraphQL counterpart
// above for the rationale). Angle brackets `<`/`>` count toward the
// recursion budget here, so the inputs nest them; `::` and `=>` additionally
// cover the two-byte-punct arm, which GraphQL has no analog for.
//
// This used to compare the SIMD render directly against a live Logos-backed
// lexer (both `Char = char`, so no golden file was needed). Task 4 of the
// Logos-slimming plan severed that live comparator — the full `SyntacticToken`
// grammar it depended on is deleted in Task 5 — so each expected render below
// is frozen from that comparator's output, captured immediately before
// deletion (see `docs/superpowers/plans/2026-07-06-logos-slimming-phase2.md`,
// Task 4).
#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_syntactic_simd_low_recursion_parity() {
  use smear_lexer::graphqlx::syntactic::SimdSyntacticLexer;
  use tokit::state::recursion_tracker::RecursionLimiter;

  const CASES: &[(&str, &str)] = &[
    (
      "Box<Inner<T>>",
      "OK  0..3  Identifier(\"Box\")\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 3, end: 4 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 9, end: 10 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  12..13  RAngle\n",
    ),
    (
      "<a::b>",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  5..6  RAngle\n",
    ),
    (
      "<x => y>",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  7..8  RAngle\n",
    ),
    (
      "<a + b>",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  6..7  RAngle\n",
    ),
    (
      "{a}",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  2..3  RBrace\n",
    ),
    (
      "((x))",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 1, end: 2 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 2 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  4..5  RParen\n",
    ),
    (
      r#"<v: "s">"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  7..8  RAngle\n",
    ),
    (
      r#"<e: "">"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  6..7  RAngle\n",
    ),
    // block strings on the SIMD fast path must re-check the limiter through
    // `finish!` too: plain, empty, and complex (common-indent) bodies.
    (
      r#"<v: """x""">"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  11..12  RAngle\n",
    ),
    (
      r#"<e: """""">"#,
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  10..11  RAngle\n",
    ),
    (
      "<v: \"\"\"foo\n  bar\"\"\">",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  19..20  RAngle\n",
    ),
    (
      "<...a>",
      "ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 1 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       ERR LexerErrors([LexerError { span: SimpleSpan { start: 0, end: 0 }, data: State(RecursionLimitExceeded(RecursionLimiter { max: 0, current: 1 })) }])\n\
       OK  5..6  RAngle\n",
    ),
  ];

  for (src, expected) in CASES {
    let simd = render_stream!(SimdSyntacticLexer::<str>::with_state(
      src,
      RecursionLimiter::with_limitation(0),
    ));
    assert_eq!(&simd, expected, "low-limit stream mismatch for {src:?}");
  }
}

// ─── GraphQLx SIMD source matrix: `<str>` vs `<[u8]>`/`<Bytes>` ─────────────
//
// The GraphQLx analog of the GraphQL source matrix above. `graphqlx_syntactic_
// simd_oracle` proves `<str>` matches the golden byte-for-byte; this section
// proves the byte-flavored sources (`Char = u8`) carry the same kinds, spans,
// and decoded content, transitively matching the golden without ever comparing
// `Debug` text across `Char` types. The dialect-agnostic leaf comparators built
// for the GraphQL matrix (`same_lexeme`, `same_string_errors`, `same_inline_str`,
// `same_block_str`, ...) are reused verbatim; only the GraphQLx-typed walkers
// are new, because GraphQLx has radix-number error enums and enum-shaped
// `LitInt`/`LitFloat` tokens the GraphQL comparators don't cover.

#[cfg(feature = "graphqlx")]
use smear_lexer::graphqlx::{
  LitFloat, LitInt,
  error::{
    BinaryError, DecimalError as GxDecimalError, FloatError as GxFloatError, HexError,
    HexFloatError, LexerError as GxLexerError, LexerErrorData as GxErrorData,
    LexerErrors as GxLexerErrors, OctalError,
  },
  syntactic::SimdSyntacticLexer as GxSimd,
  syntactic::SyntacticToken as GxToken,
};
#[cfg(feature = "graphqlx")]
use tokit::state::recursion_tracker::RecursionLimitExceeded;

// Each number-error enum reuses the shared `same_lexeme` for its `Char`-bearing
// suffix leaf; its `UnexpectedEnd`/span-only leaves carry no `Char`, so both
// sides are the same concrete type and compare by plain `==`.
#[cfg(feature = "graphqlx")]
fn same_gx_float_error(byte: &GxFloatError<u8>, str_: &GxFloatError<char>) -> bool {
  match (byte, str_) {
    (GxFloatError::UnexpectedSuffix(b), GxFloatError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (GxFloatError::UnexpectedLexeme(b), GxFloatError::UnexpectedLexeme(s)) => {
      same_unexpected_lexeme(b, s)
    }
    (GxFloatError::UnexpectedEnd(b), GxFloatError::UnexpectedEnd(s)) => b == s,
    (GxFloatError::MissingIntegerPart(b), GxFloatError::MissingIntegerPart(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_hex_float_error(byte: &HexFloatError<u8>, str_: &HexFloatError<char>) -> bool {
  match (byte, str_) {
    (HexFloatError::UnexpectedSuffix(b), HexFloatError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (HexFloatError::UnexpectedLexeme(b), HexFloatError::UnexpectedLexeme(s)) => {
      same_unexpected_lexeme(b, s)
    }
    (HexFloatError::UnexpectedEnd(b), HexFloatError::UnexpectedEnd(s)) => b == s,
    (HexFloatError::MissingIntegerPart(b), HexFloatError::MissingIntegerPart(s)) => b == s,
    (HexFloatError::MissingExponent(b), HexFloatError::MissingExponent(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_decimal_error(byte: &GxDecimalError<u8>, str_: &GxDecimalError<char>) -> bool {
  match (byte, str_) {
    (GxDecimalError::UnexpectedSuffix(b), GxDecimalError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (GxDecimalError::UnexpectedEnd(b), GxDecimalError::UnexpectedEnd(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_hex_error(byte: &HexError<u8>, str_: &HexError<char>) -> bool {
  match (byte, str_) {
    (HexError::UnexpectedSuffix(b), HexError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (HexError::UnexpectedEnd(b), HexError::UnexpectedEnd(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_octal_error(byte: &OctalError<u8>, str_: &OctalError<char>) -> bool {
  match (byte, str_) {
    (OctalError::UnexpectedSuffix(b), OctalError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (OctalError::UnexpectedEnd(b), OctalError::UnexpectedEnd(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_binary_error(byte: &BinaryError<u8>, str_: &BinaryError<char>) -> bool {
  match (byte, str_) {
    (BinaryError::UnexpectedSuffix(b), BinaryError::UnexpectedSuffix(s)) => same_lexeme(b, s),
    (BinaryError::UnexpectedEnd(b), BinaryError::UnexpectedEnd(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_error_data(
  byte: &GxErrorData<u8, RecursionLimitExceeded>,
  str_: &GxErrorData<char, RecursionLimitExceeded>,
) -> bool {
  match (byte, str_) {
    (GxErrorData::Float(b), GxErrorData::Float(s)) => same_gx_float_error(b, s),
    (GxErrorData::HexFloat(b), GxErrorData::HexFloat(s)) => same_gx_hex_float_error(b, s),
    (GxErrorData::Decimal(b), GxErrorData::Decimal(s)) => same_gx_decimal_error(b, s),
    (GxErrorData::Hex(b), GxErrorData::Hex(s)) => same_gx_hex_error(b, s),
    (GxErrorData::Octal(b), GxErrorData::Octal(s)) => same_gx_octal_error(b, s),
    (GxErrorData::Binary(b), GxErrorData::Binary(s)) => same_gx_binary_error(b, s),
    (GxErrorData::String(b), GxErrorData::String(s)) => same_string_errors(b, s),
    (GxErrorData::UnexpectedLexeme(b), GxErrorData::UnexpectedLexeme(s)) => same_lexeme(b, s),
    (GxErrorData::UnknownLexeme(b), GxErrorData::UnknownLexeme(s)) => same_lexeme(b, s),
    (GxErrorData::UnexpectedEndOfInput, GxErrorData::UnexpectedEndOfInput) => true,
    (GxErrorData::UnterminatedSpreadOperator, GxErrorData::UnterminatedSpreadOperator) => true,
    (GxErrorData::State(b), GxErrorData::State(s)) => b == s,
    (GxErrorData::InvalidUtf8(b), GxErrorData::InvalidUtf8(s)) => b == s,
    (GxErrorData::Other(b), GxErrorData::Other(s)) => b == s,
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_lexer_error(
  byte: &GxLexerError<u8, RecursionLimitExceeded>,
  str_: &GxLexerError<char, RecursionLimitExceeded>,
) -> bool {
  byte.span() == str_.span() && same_gx_error_data(byte.data(), str_.data())
}

#[cfg(feature = "graphqlx")]
fn same_gx_lexer_errors(
  byte: &GxLexerErrors<u8, RecursionLimitExceeded>,
  str_: &GxLexerErrors<char, RecursionLimitExceeded>,
) -> bool {
  byte.len() == str_.len()
    && byte
      .iter()
      .zip(str_.iter())
      .all(|(b, s)| same_gx_lexer_error(b, s))
}

// `LitInt`/`LitFloat` are radix-tagged enums; a matching radix variant plus
// equal raw bytes proves the byte and str lexers classified the literal the
// same way (radix is a pure function of the bytes, but the variant match makes
// any divergence a hard mismatch rather than a silent pass).
#[cfg(feature = "graphqlx")]
fn same_gx_lit_int<B: AsBytes>(byte: &LitInt<B>, str_: &LitInt<&str>) -> bool {
  match (byte, str_) {
    (LitInt::Decimal(b), LitInt::Decimal(s))
    | (LitInt::Hex(b), LitInt::Hex(s))
    | (LitInt::Binary(b), LitInt::Binary(s))
    | (LitInt::Octal(b), LitInt::Octal(s)) => b.as_bytes() == s.as_bytes(),
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_lit_float<B: AsBytes>(byte: &LitFloat<B>, str_: &LitFloat<&str>) -> bool {
  match (byte, str_) {
    (LitFloat::Decimal(b), LitFloat::Decimal(s)) | (LitFloat::Hex(b), LitFloat::Hex(s)) => {
      b.as_bytes() == s.as_bytes()
    }
    _ => false,
  }
}

#[cfg(feature = "graphqlx")]
fn same_gx_token<B: AsBytes>(byte: &GxToken<B>, str_: &GxToken<&str>) -> bool {
  use GxToken as Tok;
  if byte.kind() != str_.kind() {
    return false;
  }
  match (byte, str_) {
    (Tok::Identifier(b), Tok::Identifier(s)) => b.as_bytes() == s.as_bytes(),
    (Tok::LitInt(b), Tok::LitInt(s)) => same_gx_lit_int(b, s),
    (Tok::LitFloat(b), Tok::LitFloat(s)) => same_gx_lit_float(b, s),
    (Tok::LitInlineStr(b), Tok::LitInlineStr(s)) => same_inline_str(b, s),
    (Tok::LitBlockStr(b), Tok::LitBlockStr(s)) => same_block_str(b, s),
    // Every other variant carries no payload; the `kind()` check above already
    // confirmed they're the same punctuation/spread token.
    _ => true,
  }
}

/// Drives a byte-sourced GraphQLx `SimdSyntacticLexer` and the reference
/// `<str>`-flavored one over the same fixture text, in lockstep, asserting
/// every token/error pair is source-agnostically equivalent.
#[cfg(feature = "graphqlx")]
macro_rules! assert_gx_matches_str_stream {
  ($label:expr, $fixture:expr, $byte_lexer:expr, $str_src:expr) => {{
    let mut byte_lex = $byte_lexer;
    let mut str_lex = GxSimd::<str>::new($str_src);
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
            same_gx_token(&bt, &st),
            "{}/{}#{idx}: content mismatch: byte={bt:?} str={st:?}",
            $label,
            $fixture
          );
        }
        (Some(Err(be)), Some(Err(se))) => {
          assert!(
            same_gx_lexer_errors(&be, &se),
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

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_syntactic_simd_source_matrix_u8() {
  for (name, src) in GRAPHQLX_EXTRA.iter().chain(GRAPHQLX_SIMD_EXTRA) {
    assert_gx_matches_str_stream!("[u8]", name, GxSimd::<[u8]>::new(src.as_bytes()), src);
  }
}

#[cfg(all(feature = "graphqlx", feature = "bytes"))]
#[test]
fn graphqlx_syntactic_simd_source_matrix_bytes() {
  for (name, src) in GRAPHQLX_EXTRA.iter().chain(GRAPHQLX_SIMD_EXTRA) {
    let owned = bytes::Bytes::copy_from_slice(src.as_bytes());
    assert_gx_matches_str_stream!("Bytes", name, GxSimd::<bytes::Bytes>::new(&owned), src);
  }
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
