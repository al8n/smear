//! Token-mix profiler: walks the lossless GraphQL lexer over an input file
//! and reports what fraction of bytes belongs to each lexical category.
//!
//! This tells us the *upper bound* on what a SIMD-accelerated layer can
//! buy us. If 80 % of bytes are trivia + identifiers and we can scan those
//! at SIMD rates, the analytical ceiling is roughly `1 / (1 - 0.8) = 5×`
//! over the current lexer (Amdahl).
//!
//! Usage:
//!     cargo run --release --example token_mix -- <path-to-graphql-file>
//!     cargo run --release --example token_mix -- ../smear/tests/fixtures/schemas/gmx_schema.graphql

use std::{env, fs, process::ExitCode};

use smear_lexer::{
  graphql::lossless::{LosslessLexer, LosslessTokenKind},
  tokora::lexer::Lexer as _,
};

#[derive(Default, Debug, Clone, Copy)]
struct ByteCounts {
  /// Whitespace, commas, BOM, comments — everything the syntactic lexer skips.
  trivia: usize,
  /// `[a-zA-Z_][a-zA-Z0-9_]*`.
  identifier: usize,
  /// Single-byte structural tokens (`{ } [ ] ( ) : ; @ $ = ! & |`).
  punct: usize,
  /// `...`. Three bytes; lexsimd-shape SIMD won't naturally help here.
  spread: usize,
  /// Int + Float literals.
  number: usize,
  /// Inline strings (quoted, may contain escapes).
  inline_string: usize,
  /// Triple-quoted block strings.
  block_string: usize,
  /// Ranges that produced a lex error and that we skipped.
  errored: usize,
  /// Total bytes seen.
  total: usize,
}

fn classify(kind: LosslessTokenKind) -> Bucket {
  use LosslessTokenKind::*;
  match kind {
    // Trivia — bytes the syntactic lexer never emits.
    Space | Tab | Newline | CarriageReturn | CarriageReturnAndNewline | Comma | Bom | Comment => {
      Bucket::Trivia
    }

    // Single-byte structural punctuation.
    Ampersand | At | RBrace | LBrace | RBracket | LBracket | RParen | LParen | Bang | Colon
    | Dollar | Equal | Pipe => Bucket::Punct,

    Spread => Bucket::Spread,
    Identifier => Bucket::Identifier,
    Int | Float | Boolean => Bucket::Number,
    InlineString => Bucket::InlineString,
    BlockString => Bucket::BlockString,
  }
}

#[derive(Copy, Clone)]
enum Bucket {
  Trivia,
  Identifier,
  Punct,
  Spread,
  Number,
  InlineString,
  BlockString,
}

fn analyze(input: &str) -> ByteCounts {
  let mut c = ByteCounts {
    total: input.len(),
    ..Default::default()
  };

  let mut lexer = LosslessLexer::<&str>::new(input);
  loop {
    let result = lexer.lex();
    let span = lexer.span();
    let len = span.end().saturating_sub(span.start());
    match result {
      Some(Ok(tok)) => match classify(tok.kind()) {
        Bucket::Trivia => c.trivia += len,
        Bucket::Identifier => c.identifier += len,
        Bucket::Punct => c.punct += len,
        Bucket::Spread => c.spread += len,
        Bucket::Number => c.number += len,
        Bucket::InlineString => c.inline_string += len,
        Bucket::BlockString => c.block_string += len,
      },
      Some(Err(_)) => c.errored += len,
      None => break,
    }
  }
  c
}

fn pct(part: usize, total: usize) -> f64 {
  if total == 0 {
    0.0
  } else {
    (part as f64 / total as f64) * 100.0
  }
}

fn print_report(label: &str, c: &ByteCounts) {
  let covered = c.trivia
    + c.identifier
    + c.punct
    + c.spread
    + c.number
    + c.inline_string
    + c.block_string
    + c.errored;

  println!("=== Token mix: {label} ===");
  println!("Total bytes: {}", c.total);
  println!("Covered:     {} ({:.1}%)", covered, pct(covered, c.total));
  println!();
  println!("By byte category:");
  println!(
    "  trivia       {:>10}  {:>5.1}%",
    c.trivia,
    pct(c.trivia, c.total)
  );
  println!(
    "  identifier   {:>10}  {:>5.1}%",
    c.identifier,
    pct(c.identifier, c.total)
  );
  println!(
    "  punctuation  {:>10}  {:>5.1}%",
    c.punct,
    pct(c.punct, c.total)
  );
  println!(
    "  spread (..)  {:>10}  {:>5.1}%",
    c.spread,
    pct(c.spread, c.total)
  );
  println!(
    "  number       {:>10}  {:>5.1}%",
    c.number,
    pct(c.number, c.total)
  );
  println!(
    "  inline str   {:>10}  {:>5.1}%",
    c.inline_string,
    pct(c.inline_string, c.total)
  );
  println!(
    "  block str    {:>10}  {:>5.1}%",
    c.block_string,
    pct(c.block_string, c.total)
  );
  if c.errored > 0 {
    println!(
      "  errored      {:>10}  {:>5.1}%",
      c.errored,
      pct(c.errored, c.total)
    );
  }
  println!();

  let simd_eligible = c.trivia + c.identifier;
  let logos_only = c.number + c.inline_string + c.block_string + c.spread + c.punct + c.errored;
  println!(
    "SIMD-eligible (trivia + identifier): {} bytes  ({:.1}%)",
    simd_eligible,
    pct(simd_eligible, c.total)
  );
  println!(
    "Logos-only (numbers, strings, punct, spread, errors): {} bytes  ({:.1}%)",
    logos_only,
    pct(logos_only, c.total)
  );

  // Amdahl: if SIMD makes its share negligible, the speedup is
  // 1 / (1 - simd_share). Reality will be much less because the SIMD path
  // isn't free — but this is the analytical ceiling.
  let simd_share = simd_eligible as f64 / c.total.max(1) as f64;
  let amdahl = if simd_share >= 1.0 {
    f64::INFINITY
  } else {
    1.0 / (1.0 - simd_share)
  };
  println!(
    "Amdahl ceiling (perfect SIMD on the eligible bytes): {:.2}× the current lexer",
    amdahl
  );
  println!();
}

fn main() -> ExitCode {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    eprintln!("usage: {} <path-to-graphql-file> [more files...]", args[0]);
    return ExitCode::from(2);
  }

  for path in &args[1..] {
    let input = match fs::read_to_string(path) {
      Ok(s) => s,
      Err(err) => {
        eprintln!("failed to read {path}: {err}");
        return ExitCode::from(1);
      }
    };
    let counts = analyze(&input);
    print_report(path, &counts);
  }

  ExitCode::SUCCESS
}
