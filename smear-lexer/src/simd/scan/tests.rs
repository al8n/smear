use std::format;

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

/// Below `LONG_DIGIT_RUN` (32 bytes), `digit_run_len` uses the scalar loop,
/// leaving the `memspan::skip::skip_digits` branch (a digit run at/above
/// that threshold) untested. Exercise it directly for the integer part, the
/// fractional part, and with a legal delimiter following the long run.
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
