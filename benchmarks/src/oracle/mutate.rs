//! Mechanical mutation of a valid document into a family of near-miss documents.
//!
//! # Why mutants, and why blind ones
//!
//! A hand-written corpus tests the cases somebody thought of, and the rules somebody thought of
//! are the rules that already have fixtures. Mutation covers the other direction: take a document
//! both implementations accept, break it one token at a time, and compare the verdicts. The
//! operators below are deliberately **not** keyed to rules. A differential test does not need to
//! know which rule a mutant should trip — that is what having a second implementation is for —
//! and an operator written to produce a specific rule's failure can only ever confirm what its
//! author already believed.
//!
//! # Mutants that stop parsing are not waste
//!
//! Roughly half of these break the grammar. That is not a defect of the generator: a document
//! neither parser accepts is scored as
//! [`Outcome::BothRejectedSyntax`](super::Outcome::BothRejectedSyntax) and counted, and a document
//! **one** parser accepts is a
//! [`Divergence::ParseDivergence`](super::Divergence::ParseDivergence) — a real finding, reported
//! under its own name so it cannot be mistaken for a validator disagreement. The family therefore
//! exercises the two parsers' agreement for free while it is exercising the two validators'.

use std::ops::Range;

/// A generated document and the operator that produced it.
#[derive(Debug, Clone)]
pub struct Mutant {
  /// `operator@offset`, unique within a seed and stable across runs.
  pub name: String,
  /// The mutated source.
  pub source: String,
}

/// Every mutant of one seed document, in a deterministic order.
///
/// Deterministic because a differential corpus that changes between runs cannot be bisected: a
/// red has to name a document somebody can paste into a scratch file.
pub fn mutants(seed: &str) -> Vec<Mutant> {
  let scan = Scan::of(seed);
  let mut out = Vec::new();

  for span in &scan.identifiers {
    out.push(Mutant {
      name: format!("mangle@{}", span.start),
      source: splice(seed, span.clone(), &format!("zz{}", &seed[span.clone()])),
    });
  }

  for &offset in &scan.bangs {
    out.push(Mutant {
      name: format!("drop-bang@{offset}"),
      source: splice(seed, offset..offset + 1, ""),
    });
  }

  // Non-null is the other half of 5.8.5 and of every required-argument rule, so the family has to
  // be able to *add* one as well as remove one. Only after `:` or `]`, which is where a type
  // reference can end; anywhere else it is a guaranteed syntax error and buys nothing.
  for &offset in &scan.type_ends {
    out.push(Mutant {
      name: format!("add-bang@{offset}"),
      source: splice(seed, offset..offset, "!"),
    });
  }

  for (index, line) in line_spans(seed).into_iter().enumerate() {
    let text = &seed[line.clone()];
    if text.trim().is_empty() {
      continue;
    }
    out.push(Mutant {
      name: format!("dup-line@{index}"),
      source: splice(seed, line.end..line.end, &format!("\n{text}")),
    });
    out.push(Mutant {
      name: format!("drop-line@{index}"),
      source: splice(seed, line.clone(), ""),
    });
  }

  out
}

fn splice(source: &str, range: Range<usize>, replacement: &str) -> String {
  let mut out = String::with_capacity(source.len() + replacement.len());
  out.push_str(&source[..range.start]);
  out.push_str(replacement);
  out.push_str(&source[range.end..]);
  out
}

fn line_spans(source: &str) -> Vec<Range<usize>> {
  let mut spans = Vec::new();
  let mut start = 0;
  for (offset, byte) in source.bytes().enumerate() {
    if byte == b'\n' {
      spans.push(start..offset);
      start = offset + 1;
    }
  }
  if start < source.len() {
    spans.push(start..source.len());
  }
  spans
}

/// Byte offsets of the things the operators need, found by a scan that knows about strings and
/// comments.
///
/// Without that knowledge every operator would also edit the inside of string literals and block
/// strings, where a mangled word is not a name and a `!` is not non-null — mutants that cannot
/// distinguish the two implementations because neither one is looking at that text.
struct Scan {
  identifiers: Vec<Range<usize>>,
  bangs: Vec<usize>,
  type_ends: Vec<usize>,
}

impl Scan {
  fn of(source: &str) -> Self {
    let bytes = source.as_bytes();
    let mut scan = Self {
      identifiers: Vec::new(),
      bangs: Vec::new(),
      type_ends: Vec::new(),
    };
    // Set when the previous significant byte was `:` or `[` — the two places a type *name* can
    // begin, and therefore the only two after which the following identifier is a position `!` may
    // be appended to. `]` is handled separately: it ends a list wrapper, so `!` goes after the
    // bracket itself rather than after an identifier.
    //
    // Getting `[` wrong here is not cosmetic. It was originally absent, which cleared the flag on
    // every list type and meant the `add-bang` operator never once fired inside `[Int]` — the
    // exact shape draft 5.8.5 and whitelist class W2 are about. The unit tests below pin the
    // offsets rather than asserting the vector is non-empty, which is what let the omission
    // survive its first review.
    let mut after_type_open = false;
    let mut index = 0;

    while index < bytes.len() {
      match bytes[index] {
        b'#' => {
          while index < bytes.len() && bytes[index] != b'\n' {
            index += 1;
          }
        }
        b'"' if bytes[index..].starts_with(b"\"\"\"") => {
          index += 3;
          while index + 2 < bytes.len() && !bytes[index..].starts_with(b"\"\"\"") {
            index += 1;
          }
          index = (index + 3).min(bytes.len());
        }
        b'"' => {
          index += 1;
          while index < bytes.len() && bytes[index] != b'"' {
            index += if bytes[index] == b'\\' { 2 } else { 1 };
          }
          index += 1;
        }
        b'!' => {
          scan.bangs.push(index);
          index += 1;
          after_type_open = false;
        }
        b':' | b'[' => {
          after_type_open = true;
          index += 1;
        }
        b']' => {
          // `[Int]` → a `!` may follow the bracket. In a list *value* it may not, and the mutant is
          // then a syntax error both parsers reject — which the report counts and nobody has to
          // filter.
          index += 1;
          scan.type_ends.push(index);
          after_type_open = false;
        }
        byte if byte.is_ascii_whitespace() || byte == b',' => index += 1,
        byte if byte == b'_' || byte.is_ascii_alphabetic() => {
          let start = index;
          while index < bytes.len()
            && (bytes[index] == b'_' || bytes[index].is_ascii_alphanumeric())
          {
            index += 1;
          }
          scan.identifiers.push(start..index);
          if after_type_open {
            scan.type_ends.push(index);
          }
          after_type_open = false;
        }
        _ => {
          index += 1;
          after_type_open = false;
        }
      }
    }

    scan
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn the_scan_ignores_strings_and_comments() {
    let source = "{ f(a: \"not an ident\") # nor this\n  b }";
    let scan = Scan::of(source);
    let names: Vec<_> = scan
      .identifiers
      .iter()
      .map(|span| &source[span.clone()])
      .collect();
    assert_eq!(names, ["f", "a", "b"]);
  }

  #[test]
  fn the_scan_ignores_block_strings() {
    let source = "{ f(a: \"\"\"ghost field\"\"\") }";
    let scan = Scan::of(source);
    let names: Vec<_> = scan
      .identifiers
      .iter()
      .map(|span| &source[span.clone()])
      .collect();
    assert_eq!(names, ["f", "a"]);
  }

  /// Offsets, not emptiness. Asserting `!type_ends.is_empty()` is what let `[` go missing from the
  /// opener set, because a scan that finds the wrong positions still finds *some*.
  #[test]
  fn bangs_and_type_ends_are_found_where_a_type_can_end() {
    //              0         1         2
    //              0123456789012345678901234567890
    let source = "query ($v: [Int!]) { f(a: $v) }";
    assert_eq!(&source[12..15], "Int");
    assert_eq!(&source[15..16], "!");
    assert_eq!(&source[16..17], "]");

    let scan = Scan::of(source);
    assert_eq!(scan.bangs, [15], "the one `!` in the source");
    assert_eq!(
      scan.type_ends,
      [15, 17],
      "after `Int` (making it `[Int!!]`) and after `]` (making it `[Int!]!`)"
    );
  }

  /// A variable *use* is not a type position, so no `!` may be appended after it.
  #[test]
  fn a_variable_use_is_not_a_type_position() {
    let source = "{ f(a: $v) }";
    let scan = Scan::of(source);
    assert!(
      scan.type_ends.is_empty(),
      "recorded {:?} as type positions",
      scan.type_ends
    );
  }

  #[test]
  fn every_mutant_differs_from_its_seed_and_from_the_others() {
    let seed = "query Q($v: Int!) { f(a: $v) { g } }";
    let mutants = mutants(seed);
    assert!(mutants.len() > 10, "only {} mutants", mutants.len());
    for mutant in &mutants {
      assert_ne!(mutant.source, seed, "{} changed nothing", mutant.name);
    }
    let mut names: Vec<_> = mutants.iter().map(|mutant| mutant.name.clone()).collect();
    names.sort();
    let before = names.len();
    names.dedup();
    assert_eq!(before, names.len(), "mutant names collide");
  }
}
