//! Trivia-span profiler: walks the lossless GraphQL lexer and reports the
//! length distribution of *contiguous* trivia runs (a run = one or more
//! adjacent trivia tokens between two non-trivia tokens).
//!
//! This is the second half of the SIMD-feasibility question. Even if 30 %
//! of bytes are trivia, SIMD can only amortize when each individual run is
//! long enough. If most runs are 1-byte (single space between tokens),
//! SIMD will be tied with scalar — there's nothing to vectorize over. If
//! most runs are 4–32 bytes (typical pretty-printed indent), SIMD wins.
//!
//! Usage:
//!     cargo run --release --example trivia_spans -- <path-to-graphql-file> [...]

use std::{env, fs, process::ExitCode};

use smear_lexer::{graphql::lossless::LosslessLexer, tokit::lexer::Lexer as _};

fn collect_trivia_runs(input: &str) -> Vec<usize> {
  let mut runs: Vec<usize> = Vec::new();
  let mut current: Option<usize> = None;

  let mut lexer = LosslessLexer::<&str>::new(input);
  loop {
    let result = lexer.lex();
    let span = lexer.span();
    let len = span.end().saturating_sub(span.start());
    let is_trivia = match result {
      Some(Ok(tok)) => tok.is_trivia(),
      Some(Err(_)) => false,
      None => {
        // EOF — flush any in-progress run and stop.
        if let Some(run) = current.take() {
          runs.push(run);
        }
        return runs;
      }
    };

    if is_trivia {
      *current.get_or_insert(0) += len;
    } else if let Some(run) = current.take() {
      runs.push(run);
    }
  }
}

/// Powers-of-two-ish histogram buckets, matching what we'd care about in a
/// SIMD design where the chunk size is 16 bytes.
const BUCKETS: &[(&str, usize, usize)] = &[
  // (label,                lo, hi)  — inclusive bounds in bytes
  ("1 byte", 1, 1),
  ("2-3 bytes", 2, 3),
  ("4-7 bytes", 4, 7),
  ("8-15 bytes", 8, 15),
  ("16-31 bytes", 16, 31),
  ("32-63 bytes", 32, 63),
  ("64-127 bytes", 64, 127),
  ("128-255 bytes", 128, 255),
  ("256+ bytes", 256, usize::MAX),
];

fn print_report(label: &str, runs: &[usize], total_input_bytes: usize) {
  println!("=== Trivia spans: {label} ===");

  if runs.is_empty() {
    println!("(no trivia runs)");
    println!();
    return;
  }

  let n = runs.len();
  let total_trivia: usize = runs.iter().sum();
  let mean = total_trivia as f64 / n as f64;
  let mut sorted = runs.to_vec();
  sorted.sort_unstable();
  let median = sorted[n / 2];
  let max = *sorted.last().unwrap();
  let min = *sorted.first().unwrap();

  println!("Total trivia runs:        {n}");
  println!(
    "Total trivia bytes:       {total_trivia}  ({:.1}% of input)",
    (total_trivia as f64 / total_input_bytes.max(1) as f64) * 100.0
  );
  println!("Mean run length:          {mean:.1} bytes");
  println!("Median run length:        {median} bytes");
  println!("Min / Max run length:     {min} / {max} bytes");
  println!();

  println!("Run length distribution:");
  println!(
    "  {:<14}  {:>8}  {:>6}  {:>10}  {:>6}",
    "bucket", "runs", "% runs", "bytes", "% bytes"
  );
  for (lbl, lo, hi) in BUCKETS {
    let bucket_runs: Vec<usize> = sorted
      .iter()
      .copied()
      .filter(|&r| r >= *lo && r <= *hi)
      .collect();
    let count = bucket_runs.len();
    let bytes: usize = bucket_runs.iter().sum();
    if count == 0 {
      continue;
    }
    println!(
      "  {:<14}  {:>8}  {:>5.1}%  {:>10}  {:>5.1}%",
      lbl,
      count,
      (count as f64 / n as f64) * 100.0,
      bytes,
      (bytes as f64 / total_trivia as f64) * 100.0
    );
  }
  println!();

  // SIMD amortization indicator: what fraction of trivia *bytes* live in
  // runs that are at least one SIMD chunk (16 B) long. That's the share
  // SIMD can actually accelerate; sub-chunk runs run at scalar speed
  // regardless.
  let simd_amortizable: usize = sorted.iter().copied().filter(|&r| r >= 16).sum();
  let simd_share = (simd_amortizable as f64 / total_trivia.max(1) as f64) * 100.0;
  println!(
    "Bytes in runs ≥ 16 B (SIMD-amortizable): {simd_amortizable}  ({simd_share:.1}% of trivia)"
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
    let runs = collect_trivia_runs(&input);
    print_report(path, &runs, input.len());
  }

  ExitCode::SUCCESS
}
