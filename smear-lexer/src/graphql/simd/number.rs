//! Validity-only fast scan for GraphQL numeric literals (`Int` / `Float`).
//!
//! [`scan_number`] answers exactly one question: "starting at `src[0]`, is
//! this a clean, valid GraphQL number, and how many bytes does it span?" It
//! is pure validity + measurement — it never constructs an error. The
//! instant the grammar is violated, or the number is followed by a byte
//! GraphQL treats as an illegal suffix, it returns `None` so the caller can
//! delegate the *entire* token to the existing Logos lexer, which already
//! knows how to build the exact error (leading zeros, empty
//! fraction/exponent, decimal/ident suffix, ...). SIMD only ever
//! accelerates the hot, unambiguous path; Logos remains the permanent
//! source of truth for every anomaly.
//!
//! Grammar mirrored from `graphql/syntactic/token.rs`'s Logos subpatterns:
//! ```text
//! digit          = [0-9]
//! non_zero_digit = [1-9]
//! int            = -? (0 | non_zero_digit digit*)
//! frac           = . digit+
//! exp            = [eE] [+-]? digit+
//! Int            = int
//! Float          = int (frac exp | frac | exp)
//! ```
//!
//! [`scan_number`] is wired into the dispatch `match` in `graphql/simd.rs`:
//! `Some((kind, len))` becomes an inline `SyntacticToken::LitInt`/`LitFloat`
//! (no Logos involved), and `None` delegates the whole token to Logos via
//! `SimdSyntacticLexer::delegate_to_logos`.

/// The two GraphQL numeric literal shapes [`scan_number`] can recognize.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NumberKind {
  /// `-?(0|[1-9][0-9]*)` — no fraction, no exponent.
  Int,
  /// An [`NumberKind::Int`]-shaped integer part with a fraction and/or
  /// exponent suffix.
  Float,
}

/// Below this many remaining bytes, a tight scalar loop beats dispatching
/// into `memspan`'s SIMD digit scanner — mirrors the short/long split in
/// `lex_identifier` (`graphql/simd.rs`). GraphQL numbers are almost always
/// a handful of digits, so this is the overwhelmingly common branch; the
/// SIMD path exists for pathological long digit runs (huge synthetic IDs,
/// timestamps, etc. encoded as decimal).
const LONG_DIGIT_RUN: usize = 32;

/// Length of the maximal leading run of ASCII digits (`0..=9`) in `bytes`.
#[cfg_attr(not(tarpaulin), inline(always))]
fn digit_run_len(bytes: &[u8]) -> usize {
  if bytes.len() >= LONG_DIGIT_RUN {
    memspan::skip::skip_digits(bytes)
  } else {
    let mut n = 0;
    while n < bytes.len() && bytes[n].is_ascii_digit() {
      n += 1;
    }
    n
  }
}

/// Is `b` a byte that may legally terminate a GraphQL number?
///
/// This mirrors Logos's own suffix check byte-for-byte — see
/// `ValidateNumberChar<GraphQLNumber> for u8::is_first_invalid_char` in
/// `graphql/handlers.rs` and its use from `handle_number_suffix` in
/// `handlers.rs`: a well-formed number is illegally suffixed iff the very
/// next byte is an ASCII ident-start byte (`[a-zA-Z_]`) or `.` — nothing
/// else. In particular a plain digit can never reach this check (any digit
/// run is already maximally consumed by the int/frac/exp scan below), and
/// every other punctuation byte, EOF, and any non-ASCII (UTF-8
/// continuation/lead) byte legally ends the number.
#[cfg_attr(not(tarpaulin), inline(always))]
const fn is_legal_terminator(b: u8) -> bool {
  !matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
}

/// Scan a numeric literal starting at `src[0]`, which the caller has
/// already established is an ASCII digit or `-` (a `.`-led input like `.5`
/// is not this function's concern — GraphQL floats always require an
/// integer part, so `.5` stays in the `.` dispatch arm and delegates to
/// Logos regardless of this scanner).
///
/// Returns `Some((kind, len))` only for a fully valid, unambiguous GraphQL
/// `Int`/`Float` literal: `len` bytes were consumed and whatever comes next
/// (another byte, or end of input) is a legal delimiter. Returns `None` for
/// any anomaly — a lone `-`, leading zeros, an empty fraction/exponent, or
/// a number immediately followed by a digit/`.`/ident byte — in every case
/// the caller must delegate the token to Logos, which produces the exact
/// GraphQL error.
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn scan_number(src: &[u8]) -> Option<(NumberKind, usize)> {
  let mut pos = 0;

  // Optional leading `-`. A leading `+` is never valid GraphQL and is not
  // handled here — by contract this function is only invoked at a digit or
  // `-` byte, so a stray `+` simply falls through to the "no digits at
  // all" check below and safely returns `None`.
  if src.first() == Some(&b'-') {
    pos += 1;
  }

  // Integer part: `0 | [1-9][0-9]*`. Scan the maximal digit run first, then
  // classify it — a lone `-`/empty run and a leading-zero run (`00`,
  // `007`, ...) are both grammar violations that must delegate.
  let int_len = digit_run_len(&src[pos..]);
  if int_len == 0 {
    return None;
  }
  if src[pos] == b'0' && int_len > 1 {
    return None;
  }
  pos += int_len;

  let mut kind = NumberKind::Int;

  // Optional fraction: `.` digit+. An empty fraction (`1.`, `1.e5`) is a
  // grammar violation — Logos reports the missing-digits-after-`.` error.
  if matches!(src.get(pos), Some(&b'.')) {
    let frac_len = digit_run_len(&src[pos + 1..]);
    if frac_len == 0 {
      return None;
    }
    pos += 1 + frac_len;
    kind = NumberKind::Float;
  }

  // Optional exponent: `[eE] [+-]?` digit+. An empty exponent (`1e`,
  // `1e+`) is a grammar violation — Logos reports the missing-digits-
  // after-exponent error.
  if matches!(src.get(pos), Some(&b'e') | Some(&b'E')) {
    let mut exp_start = pos + 1;
    if matches!(src.get(exp_start), Some(&b'+') | Some(&b'-')) {
      exp_start += 1;
    }
    let exp_len = digit_run_len(&src[exp_start..]);
    if exp_len == 0 {
      return None;
    }
    pos = exp_start + exp_len;
    kind = NumberKind::Float;
  }

  // The literal is grammatically complete. It is only an unambiguous token
  // if what follows is a legal delimiter — see `is_legal_terminator`. When
  // unsure this always says "no" (delegate): wrongly returning `Some` here
  // would desync from Logos, whereas an unnecessary delegation is merely a
  // missed fast path, never a correctness bug.
  match src.get(pos) {
    None => Some((kind, pos)),
    Some(&b) if is_legal_terminator(b) => Some((kind, pos)),
    Some(_) => None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn scan(src: &[u8]) -> Option<(NumberKind, usize)> {
    scan_number(src)
  }

  #[test]
  fn valid_ints() {
    assert_eq!(scan(b"0"), Some((NumberKind::Int, 1)));
    assert_eq!(scan(b"123"), Some((NumberKind::Int, 3)));
    assert_eq!(scan(b"-5"), Some((NumberKind::Int, 2)));
    assert_eq!(scan(b"10)"), Some((NumberKind::Int, 2))); // stops at ')'
    assert_eq!(scan(b"7, "), Some((NumberKind::Int, 1))); // stops at ','
  }

  #[test]
  fn valid_floats() {
    assert_eq!(scan(b"3.14"), Some((NumberKind::Float, 4)));
    assert_eq!(scan(b"1.5e10"), Some((NumberKind::Float, 6)));
    assert_eq!(scan(b"1E3"), Some((NumberKind::Float, 3)));
    assert_eq!(scan(b"-2.5"), Some((NumberKind::Float, 4)));
    assert_eq!(scan(b"1e-9]"), Some((NumberKind::Float, 4))); // stops at ']'
  }

  #[test]
  fn anomalies_delegate() {
    // all None -> Logos handles them
    for s in [
      b"007" as &[u8],
      b"-00",
      b"1.",
      b"1e",
      b"1e+",
      b"123abc",
      b"1.5x",
      b"-",
      b"00.5",
      b"1.2.3",
      b"0x",
    ] {
      assert_eq!(scan(s), None, "{s:?} should delegate");
    }
  }

  /// Below `LONG_DIGIT_RUN` (32 bytes), `digit_run_len` uses the scalar
  /// loop; Task 1 left the `memspan::skip::skip_digits` branch (a digit run
  /// at/above that threshold) untested. Exercise it directly for the
  /// integer part, the fractional part, and with a legal delimiter
  /// following the long run.
  #[test]
  fn long_digit_runs_use_the_simd_scan() {
    let forty_nines = "9".repeat(40);
    assert_eq!(scan(forty_nines.as_bytes()), Some((NumberKind::Int, 40)));

    // Long integer part immediately followed by a legal delimiter -- the
    // SIMD scan must stop exactly at the digit run's end, not consume (or
    // under-consume) into/around the terminator.
    let forty_nines_then_paren = format!("{forty_nines})");
    assert_eq!(
      scan(forty_nines_then_paren.as_bytes()),
      Some((NumberKind::Int, 40))
    );

    // Long fraction: the SIMD path is exercised on the run *after* the
    // `.`, not just the integer part.
    let long_frac = format!("1.{}", "3".repeat(40));
    assert_eq!(
      scan(long_frac.as_bytes()),
      Some((NumberKind::Float, long_frac.len()))
    );

    // Long exponent digit run.
    let long_exp = format!("1e{}", "2".repeat(40));
    assert_eq!(
      scan(long_exp.as_bytes()),
      Some((NumberKind::Float, long_exp.len()))
    );

    // A leading-zero violation is still caught even when the (illegal)
    // digit run is long -- the SIMD scan only measures length; the
    // leading-zero classification happens after, on `int_len`.
    let long_leading_zero = format!("0{}", "1".repeat(40));
    assert_eq!(scan(long_leading_zero.as_bytes()), None);
  }
}
