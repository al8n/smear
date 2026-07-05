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
