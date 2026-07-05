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
fn check(group: &str, name: &str, rendered: &str) {
  let path = oracle_path(group, name);
  if std::env::var_os("BLESS").is_some() {
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(&path, rendered).unwrap();
    return;
  }
  let expected = fs::read_to_string(&path).unwrap_or_else(|_| {
    panic!("missing golden file {path:?}; run: BLESS=1 cargo test -p smear-lexer --test oracle")
  });
  assert_eq!(
    rendered, expected,
    "\noracle mismatch for {group}/{name}\n(if this change is intended: BLESS=1 cargo test -p smear-lexer --test oracle)\n"
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

#[test]
fn graphql_syntactic_oracle() {
  use smear_lexer::graphql::syntactic::SyntacticLexer;
  for (name, src) in CORPUS.iter().chain(MALFORMED) {
    let rendered = render_stream!(SyntacticLexer::<&str>::new(src));
    check("graphql-syntactic", name, &rendered);
  }
}

#[test]
fn graphql_lossless_oracle() {
  use smear_lexer::graphql::lossless::LosslessLexer;
  for (name, src) in CORPUS.iter().chain(MALFORMED) {
    let rendered = render_stream!(LosslessLexer::<&str>::new(src));
    check("graphql-lossless", name, &rendered);
  }
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
];

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_syntactic_oracle() {
  use smear_lexer::graphqlx::syntactic::SyntacticLexer;
  for (name, src) in GRAPHQLX_EXTRA.iter() {
    let rendered = render_stream!(SyntacticLexer::<&str>::new(src));
    check("graphqlx-syntactic", name, &rendered);
  }
}

#[cfg(feature = "graphqlx")]
#[test]
fn graphqlx_lossless_oracle() {
  use smear_lexer::graphqlx::lossless::LosslessLexer;
  for (name, src) in GRAPHQLX_EXTRA.iter() {
    let rendered = render_stream!(LosslessLexer::<&str>::new(src));
    check("graphqlx-lossless", name, &rendered);
  }
}
