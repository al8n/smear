//! A decimal float literal read for `core`'s parser: its spellings, its zeros and overflows, the
//! spellings it refuses, and literals whose exponent the parser alone would misread. Every literal
//! is built on the stack: the crate builds with no allocator.

use super::{DecimalFloat, NORMALIZED_FLOAT_LEN, normalize_decimal_float};

/// The zeros of the long literals: enough that a written exponent past 655 359 is offset back into
/// range, which is what the parser alone misreads; fewer under Miri, where the verdict is the same
/// and only the parser's misreading is not exercised.
const ZEROS: usize = if cfg!(miri) { 997 } else { 99_998 };

/// The room the long literals are built in.
const ROOM: usize = ZEROS + 32;

/// What `spelling` reads as.
fn read(spelling: &[u8]) -> Option<DecimalFloat<'static>> {
  // Each reading leaks nothing: the buffer is a fresh array, and a spelling is copied out into a
  // `'static` table of the cases that answer one.
  let mut buffer = [0; NORMALIZED_FLOAT_LEN];
  match normalize_decimal_float(spelling, &mut buffer)? {
    DecimalFloat::Zero { negative } => Some(DecimalFloat::Zero { negative }),
    DecimalFloat::Overflow { negative } => Some(DecimalFloat::Overflow { negative }),
    DecimalFloat::Spelled(spelled) => Some(DecimalFloat::Spelled(known(spelled))),
  }
}

/// `spelled`, as the `'static` row of the table of spellings the tests expect.
fn known(spelled: &str) -> &'static str {
  const TABLE: [&str; 14] = [
    "1.5e0",
    "-1.5e3",
    "1e-3",
    "1.2345e0",
    "1.0005e3",
    "1e308",
    "1.7976931348623157e308",
    "5e-324",
    "1e-400",
    "1.2e1",
    "-1.0e308",
    "1e0",
    "3.4028235e38",
    "1e-45",
  ];
  TABLE
    .iter()
    .find(|row: &&&str| **row == spelled)
    .copied()
    .unwrap_or("<a spelling no test expects>")
}

/// The bits of the `f64` and of the `f32` `spelling` reads as.
fn floats(spelling: &[u8]) -> (u64, u32) {
  let mut buffer = [0; NORMALIZED_FLOAT_LEN];
  match normalize_decimal_float(spelling, &mut buffer) {
    Some(DecimalFloat::Spelled(spelled)) => (
      spelled.parse::<f64>().map_or(0, f64::to_bits),
      spelled.parse::<f32>().map_or(0, f32::to_bits),
    ),
    other => panic!("no spelling: {other:?}"),
  }
}

/// `head`, then `count` zeros, then `tail`, in `room`: the literal's bytes.
fn literal<'r>(room: &'r mut [u8; ROOM], head: &[u8], count: usize, tail: &[u8]) -> &'r [u8] {
  let end = head.len() + count + tail.len();
  for (at, byte) in room.iter_mut().enumerate().take(end) {
    *byte = if at < head.len() {
      head.get(at).copied().unwrap_or(b'0')
    } else if at < head.len() + count {
      b'0'
    } else {
      tail.get(at - head.len() - count).copied().unwrap_or(b'0')
    };
  }
  let room: &'r [u8; ROOM] = room;
  room.get(..end).unwrap_or(&[])
}

/// The decimal digits of `value`, in `out`.
fn digits(mut value: usize, out: &mut [u8; 24]) -> &[u8] {
  let mut start = out.len();
  loop {
    start -= 1;
    if let Some(slot) = out.get_mut(start) {
      *slot = b'0' + u8::try_from(value % 10).unwrap_or(0);
    }
    value /= 10;
    if value == 0 {
      break;
    }
  }
  out.get(start..).unwrap_or(&[])
}

#[test]
fn a_value_is_spelled_with_its_digits_and_a_short_exponent() {
  let cases: [(&str, &str); 13] = [
    ("1.5", "1.5e0"),
    ("-1.5e3", "-1.5e3"),
    ("0.001", "1e-3"),
    ("123.45e-2", "1.2345e0"),
    ("1_000.5", "1.0005e3"),
    ("1e308", "1e308"),
    ("1.7976931348623157e308", "1.7976931348623157e308"),
    ("5e-324", "5e-324"),
    ("1e-400", "1e-400"),
    ("12", "1.2e1"),
    ("00012", "1.2e1"),
    ("0.1e309", "1e308"),
    ("-10E3_07", "-1.0e308"),
  ];
  for (written, spelled) in cases {
    assert_eq!(
      read(written.as_bytes()),
      Some(DecimalFloat::Spelled(spelled)),
      "{written}"
    );
  }
}

#[test]
fn zero_and_overflow_are_read_from_the_first_significant_digit() {
  let cases: [(&str, DecimalFloat<'static>); 9] = [
    ("0", DecimalFloat::Zero { negative: false }),
    ("-0.0", DecimalFloat::Zero { negative: true }),
    ("0e999999", DecimalFloat::Zero { negative: false }),
    ("1e-401", DecimalFloat::Zero { negative: false }),
    ("1e-999999", DecimalFloat::Zero { negative: false }),
    ("1e309", DecimalFloat::Overflow { negative: false }),
    ("-1e309", DecimalFloat::Overflow { negative: true }),
    ("10e308", DecimalFloat::Overflow { negative: false }),
    (
      "1e99999999999999999999999",
      DecimalFloat::Overflow { negative: false },
    ),
  ];
  for (written, expected) in cases {
    assert_eq!(read(written.as_bytes()), Some(expected), "{written}");
  }
}

#[test]
fn what_is_no_decimal_float_is_refused() {
  for written in [
    "", "-", ".", "e5", "1e", "1e+", "1..2", "1.2.3", "nan", "inf", "1x", "1e5x", "+1",
  ] {
    assert_eq!(read(written.as_bytes()), None, "`{written}`");
  }
}

#[test]
fn the_edges_of_each_type_are_read_exactly() {
  // The greatest finite and the least subnormal of each.
  assert_eq!(floats(b"1.7976931348623157e308").0, f64::MAX.to_bits());
  assert_eq!(floats(b"5e-324").0, 1);
  assert_eq!(floats(b"3.4028235e38").1, f32::MAX.to_bits());
  assert_eq!(floats(b"1e-45").1, 1);
  // A first significant digit at each edge: 10^308 is finite in `f64`, 10^38 in `f32`, and 10^39
  // is past `f32`; 10^-325 is below half of `f64`'s least subnormal and 10^-46 below half of
  // `f32`'s, so each is zero there; 10^-400 is zero in both.
  assert_eq!(floats(b"1e308").0, 1e308f64.to_bits());
  assert_eq!(floats(b"1e38").1, 1e38f32.to_bits());
  assert_eq!(floats(b"1e39").1, f32::INFINITY.to_bits());
  assert_eq!(floats(b"1e-325").0, 0);
  assert_eq!(floats(b"1e-46").1, 0);
  assert_eq!(floats(b"1e-400"), (0, 0));
}

#[test]
fn an_exponent_past_what_the_parser_reads_is_read_exactly() {
  let mut room = [0; ROOM];
  // `0.`, the zeros, `1e999999`: 10^900000 when unshrunk, past every float.
  let codex = literal(&mut room, b"0.", ZEROS, b"1e999999");
  assert_eq!(
    read(codex),
    Some(DecimalFloat::Overflow { negative: false })
  );
  // `1`, the zeros and one more, `e-999999`: as far below every float.
  let reverse = literal(&mut room, b"1", ZEROS + 1, b"e-999999");
  assert_eq!(read(reverse), Some(DecimalFloat::Zero { negative: false }));
  // The zeros offset by an exponent of their count, each way: 1.0.
  let mut place = [0; 24];
  let count = digits(ZEROS + 1, &mut place);
  let mut tail = [0; 32];
  let tail = joined(&mut tail, b"1e", count);
  let cancelling = literal(&mut room, b"0.", ZEROS, tail);
  assert_eq!(read(cancelling), Some(DecimalFloat::Spelled("1e0")));
  let mut tail = [0; 32];
  let tail = joined(&mut tail, b"e-", count);
  let reversed = literal(&mut room, b"1", ZEROS + 1, tail);
  assert_eq!(floats(reversed), (1f64.to_bits(), 1f32.to_bits()));
}

/// `head` then `rest`, in `out`.
fn joined<'o>(out: &'o mut [u8; 32], head: &[u8], rest: &[u8]) -> &'o [u8] {
  let end = head.len() + rest.len();
  for (at, byte) in out.iter_mut().enumerate().take(end) {
    *byte = if at < head.len() {
      head.get(at).copied().unwrap_or(b'0')
    } else {
      rest.get(at - head.len()).copied().unwrap_or(b'0')
    };
  }
  let out: &'o [u8; 32] = out;
  out.get(..end).unwrap_or(&[])
}

#[test]
fn a_digit_past_the_kept_ones_keeps_its_side_of_a_halfway_point() {
  // 1 + 2^-53, the halfway point between 1 and the next `f64`, which rounds to even, 1.0; then a
  // `1` past the 800 digits kept, which puts it above the halfway point: 1 + 2^-52.
  const HALF: &[u8] = b"1.00000000000000011102230246251565404236316680908203125";
  let mut room = [0; ROOM];
  let exact = literal(&mut room, HALF, 0, b"");
  assert_eq!(floats(exact).0, 1f64.to_bits());
  let above = literal(&mut room, HALF, 900, b"1");
  assert_eq!(floats(above).0, (1f64 + f64::EPSILON).to_bits());
  // Zeros past the kept digits keep no final `1`: the value is the halfway point.
  let zeros = literal(&mut room, HALF, 900, b"");
  assert_eq!(floats(zeros).0, 1f64.to_bits());
}
