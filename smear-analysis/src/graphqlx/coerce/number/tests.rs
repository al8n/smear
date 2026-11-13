//! Reading numbers: integers of every radix against widths at their edges, and floats rounded bit
//! by bit, each against values written by hand.

use std::{
  string::{String, ToString},
  vec::Vec,
};

use super::{
  Float, U128_DIGITS, bit_length, decimal_cost, decimal_float, fits, hex_float, is_power_of_two,
  magnitude, magnitude_float, max_digits, scan,
};
use crate::graphqlx::raw::Radix;

/// The magnitude of the integer `text` of `radix`, as coercion converts it.
fn converted(text: &str, radix: Radix) -> Vec<u8> {
  let scanned = scan(text.as_bytes(), radix).unwrap_or_else(|| panic!("`{text}` scans"));
  let mut out: Vec<u8> = Vec::new();
  let mut limbs: Vec<u32> = Vec::new();
  magnitude(
    text.as_bytes(),
    radix,
    scanned.significant,
    &mut out,
    &mut limbs,
  );
  out
}

/// Whether the integer `text` of `radix` fits the integer `width` bits wide, signed when `signed`,
/// read as coercion reads it: refused by its length first, then by its magnitude.
fn fits_width(text: &str, radix: Radix, signed: bool, width: u32) -> bool {
  let Some(scanned) = scan(text.as_bytes(), radix) else {
    return false;
  };
  if scanned.significant > max_digits(radix, width) {
    return false;
  }
  let out = converted(text, radix);
  fits(
    scanned.negative,
    bit_length(&out),
    is_power_of_two(&out),
    signed,
    width,
  )
}

/// `head` followed by `tail`.
fn joined(head: &str, tail: &str) -> String {
  let mut text = String::from(head);
  text.push_str(tail);
  text
}

/// The prefix of `radix`'s integers.
fn prefix(radix: Radix) -> &'static str {
  match radix {
    Radix::Decimal => "",
    Radix::Hex => "0x",
    Radix::Octal => "0o",
    Radix::Binary => "0b",
  }
}

/// The digits of `2^width - 1` in a radix of `bits` bits a digit, whose greatest digit is `top`: a
/// leading digit for the bits left over, then `top` for each whole digit.
fn all_ones(width: u32, bits: u32, top: &str) -> String {
  let whole = usize::try_from(width / bits).unwrap_or(0);
  let left = width % bits;
  let mut text = String::new();
  if left > 0 {
    text.push(char::from_digit((1u32 << left) - 1, 16).unwrap_or('0'));
  }
  text.push_str(&top.repeat(whole));
  text
}

/// The bits of what `read` reads as in `f64`, when it is finite.
fn bits(read: Option<Float>) -> Option<u64> {
  read.and_then(|float: Float| float.value).map(f64::to_bits)
}

#[test]
fn every_radix_reads_its_digits() {
  let cases: [(&str, Radix, u64, &[u8]); 16] = [
    ("255", Radix::Decimal, 3, &[255]),
    ("2_5_5", Radix::Decimal, 3, &[255]),
    ("0xff", Radix::Hex, 2, &[255]),
    ("0xFF", Radix::Hex, 2, &[255]),
    ("0x_f_f", Radix::Hex, 2, &[255]),
    ("0o377", Radix::Octal, 3, &[255]),
    ("0b1111_1111", Radix::Binary, 8, &[255]),
    ("0x000000000000000000ff", Radix::Hex, 2, &[255]),
    ("-0x80", Radix::Hex, 2, &[128]),
    ("0", Radix::Decimal, 0, &[]),
    ("-0", Radix::Decimal, 0, &[]),
    ("0x0_0", Radix::Hex, 0, &[]),
    ("0x1_0000", Radix::Hex, 5, &[0, 0, 1]),
    ("0o1777", Radix::Octal, 4, &[255, 3]),
    ("1000000000", Radix::Decimal, 10, &[0x00, 0xca, 0x9a, 0x3b]),
    ("0b0001", Radix::Binary, 1, &[1]),
  ];
  for (text, radix, significant, expected) in cases {
    let scanned = scan(text.as_bytes(), radix).unwrap_or_else(|| panic!("`{text}` scans"));
    assert_eq!(scanned.significant, significant, "`{text}`'s digits");
    assert_eq!(converted(text, radix), expected, "`{text}`'s magnitude");
  }
  // A byte that is no digit of the radix, a missing prefix and no digit at all are no integer.
  for (text, radix) in [
    ("0o8", Radix::Octal),
    ("0b2", Radix::Binary),
    ("ff", Radix::Hex),
    ("0x", Radix::Hex),
    ("1a", Radix::Decimal),
  ] {
    assert!(scan(text.as_bytes(), radix).is_none(), "`{text}`");
  }
}

#[test]
fn a_decimal_integer_carries_into_a_new_limb() {
  // Up to 38 digits, `u128` arithmetic reads the value: 2^32, 2^32 - 1 and 2^64, each at the edge
  // of a limb.
  assert_eq!(converted("4294967296", Radix::Decimal), [0, 0, 0, 0, 1]);
  assert_eq!(
    converted("4294967295", Radix::Decimal),
    [255, 255, 255, 255]
  );
  assert_eq!(
    converted("18446744073709551616", Radix::Decimal),
    [0, 0, 0, 0, 0, 0, 0, 0, 1]
  );
  // Past 38, nine digits at a time multiply the limbs: `u128::MAX` fills four, and 2^128, one
  // more, carries into a fifth.
  assert_eq!(
    converted("340282366920938463463374607431768211455", Radix::Decimal),
    [255; 16]
  );
  assert_eq!(
    converted("340282366920938463463374607431768211456", Radix::Decimal),
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
  );
}

#[test]
fn a_decimal_integer_longer_than_u128_holds_costs_its_limb_updates() {
  // `ceil(d / 9) × (d × 3322 / 32000 + 2)` past 38 digits, and nothing up to them.
  let costs: [(u64, u32); 6] = [
    (0, 0),
    (38, 0),
    (39, 30),
    (78, 90),
    (19_730, 4_495_650),
    (u64::MAX, u32::MAX),
  ];
  for (digits, cost) in costs {
    assert_eq!(decimal_cost(digits), cost, "{digits} digits");
  }
  // The most digits `u128` arithmetic reads, and what they spell; leading zeros are no digits.
  let nines = "9".repeat(usize::try_from(U128_DIGITS).unwrap_or(0));
  let mut expected: Vec<u8> = (10u128.pow(38) - 1).to_le_bytes().to_vec();
  while expected.last() == Some(&0) {
    expected.pop();
  }
  assert_eq!(converted(&nines, Radix::Decimal), expected);
  assert_eq!(
    converted(
      "0000000000000000000000000000000000000000001",
      Radix::Decimal
    ),
    [1]
  );
}

#[test]
fn the_length_rule_refuses_only_what_cannot_fit() {
  let bounds: [(Radix, u32, u64); 12] = [
    (Radix::Binary, 8, 8),
    (Radix::Octal, 8, 3),
    (Radix::Hex, 8, 3),
    (Radix::Decimal, 8, 4),
    (Radix::Binary, 64, 64),
    (Radix::Octal, 64, 22),
    (Radix::Hex, 64, 17),
    (Radix::Decimal, 64, 21),
    (Radix::Decimal, 0, 2),
    (Radix::Hex, 1024, 257),
    (Radix::Octal, 1024, 342),
    (Radix::Decimal, 65_536, 19_730),
  ];
  for (radix, width, bound) in bounds {
    assert_eq!(max_digits(radix, width), bound, "width {width}");
  }
  // The greatest value of each width, written in each radix, is within the bound and fits; the
  // least of one digit more than the bound is out of the width's range, signed or not.
  let greatest: [(u32, &str); 5] = [
    (1, "1"),
    (8, "255"),
    (64, "18446744073709551615"),
    (128, "340282366920938463463374607431768211455"),
    (129, "680564733841876926926749214863536422911"),
  ];
  for (width, decimal) in greatest {
    let ones = usize::try_from(width).unwrap_or(0);
    let written = [
      (String::from(decimal), Radix::Decimal),
      (joined("0b", &"1".repeat(ones)), Radix::Binary),
      (joined("0o", &all_ones(width, 3, "7")), Radix::Octal),
      (joined("0x", &all_ones(width, 4, "f")), Radix::Hex),
    ];
    for (text, radix) in written {
      assert!(
        fits_width(&text, radix, false, width),
        "`{text}` fits u{width}"
      );
      let bound = usize::try_from(max_digits(radix, width)).unwrap_or(0);
      let longer = joined(prefix(radix), &joined("1", &"0".repeat(bound)));
      assert!(
        !fits_width(&longer, radix, false, width),
        "`{longer}` fits u{width}"
      );
      assert!(
        !fits_width(&longer, radix, true, width),
        "`{longer}` fits i{width}"
      );
    }
  }
}

#[test]
fn integers_are_checked_at_the_edges_of_their_width() {
  let cases: [(&str, Radix, bool, u32, bool); 36] = [
    // `u0` and `i0` hold zero alone.
    ("0", Radix::Decimal, false, 0, true),
    ("1", Radix::Decimal, false, 0, false),
    ("0", Radix::Decimal, true, 0, true),
    ("-1", Radix::Decimal, true, 0, false),
    ("1", Radix::Decimal, true, 0, false),
    // `u1` holds 0 and 1, `i1` -1 and 0.
    ("1", Radix::Decimal, false, 1, true),
    ("2", Radix::Decimal, false, 1, false),
    ("-1", Radix::Decimal, false, 1, false),
    ("-1", Radix::Decimal, true, 1, true),
    ("1", Radix::Decimal, true, 1, false),
    ("-2", Radix::Decimal, true, 1, false),
    // `i8` and `u8`.
    ("127", Radix::Decimal, true, 8, true),
    ("128", Radix::Decimal, true, 8, false),
    ("-128", Radix::Decimal, true, 8, true),
    ("-129", Radix::Decimal, true, 8, false),
    ("-0x80", Radix::Hex, true, 8, true),
    ("255", Radix::Decimal, false, 8, true),
    ("256", Radix::Decimal, false, 8, false),
    ("-1", Radix::Decimal, false, 8, false),
    ("-0", Radix::Decimal, false, 8, true),
    // `i64` and `u64`.
    ("0x7fffffffffffffff", Radix::Hex, true, 64, true),
    ("0x8000000000000000", Radix::Hex, true, 64, false),
    ("-0x8000000000000000", Radix::Hex, true, 64, true),
    ("-0x8000000000000001", Radix::Hex, true, 64, false),
    ("18446744073709551615", Radix::Decimal, false, 64, true),
    ("18446744073709551616", Radix::Decimal, false, 64, false),
    // `i128` and `u128`.
    (
      "170141183460469231731687303715884105727",
      Radix::Decimal,
      true,
      128,
      true,
    ),
    (
      "170141183460469231731687303715884105728",
      Radix::Decimal,
      true,
      128,
      false,
    ),
    (
      "-170141183460469231731687303715884105728",
      Radix::Decimal,
      true,
      128,
      true,
    ),
    (
      "340282366920938463463374607431768211456",
      Radix::Decimal,
      false,
      128,
      false,
    ),
    // `i129` and `u129`, one bit past what `i128` and `u128` hold.
    (
      "0xffffffffffffffffffffffffffffffff",
      Radix::Hex,
      true,
      129,
      true,
    ),
    (
      "0x100000000000000000000000000000000",
      Radix::Hex,
      true,
      129,
      false,
    ),
    (
      "-0x100000000000000000000000000000000",
      Radix::Hex,
      true,
      129,
      true,
    ),
    (
      "-0x100000000000000000000000000000001",
      Radix::Hex,
      true,
      129,
      false,
    ),
    (
      "680564733841876926926749214863536422911",
      Radix::Decimal,
      false,
      129,
      true,
    ),
    (
      "680564733841876926926749214863536422912",
      Radix::Decimal,
      false,
      129,
      false,
    ),
  ];
  for (text, radix, signed, width, expected) in cases {
    let sign = if signed { "i" } else { "u" };
    assert_eq!(
      fits_width(text, radix, signed, width),
      expected,
      "`{text}` against {sign}{width}"
    );
  }
}

#[test]
fn a_hexadecimal_float_is_rounded_to_nearest_and_to_even() {
  let cases: [(&str, Option<u64>); 16] = [
    ("0x1p0", Some(0x3ff0_0000_0000_0000)),
    ("0x1.8p1", Some(0x4008_0000_0000_0000)),
    ("0x0.1p4", Some(0x3ff0_0000_0000_0000)),
    ("0x10p-4", Some(0x3ff0_0000_0000_0000)),
    ("-0x1p0", Some(0xbff0_0000_0000_0000)),
    ("-0x0p0", Some(0x8000_0000_0000_0000)),
    // The least subnormal, and the tie below it, which goes to zero.
    ("0x1p-1074", Some(1)),
    ("0x1p-1075", Some(0)),
    ("0x1.0000000000001p-1075", Some(1)),
    // The tie at the edge of the subnormals rounds up out of them: `f64::MIN_POSITIVE`.
    ("0x1.fffffffffffffp-1023", Some(0x0010_0000_0000_0000)),
    // A tie to even, and a tie broken by a digit past the sixteenth, through the sticky bit.
    ("0x1.00000000000008p0", Some(0x3ff0_0000_0000_0000)),
    ("0x1.000000000000080000001p0", Some(0x3ff0_0000_0000_0001)),
    // The greatest, a tie above it whose carry passes it, and a value just below that tie.
    ("0x1.fffffffffffffp1023", Some(0x7fef_ffff_ffff_ffff)),
    ("0x1.fffffffffffff8p1023", None),
    ("0x1.fffffffffffff7p1023", Some(0x7fef_ffff_ffff_ffff)),
    ("0x1p1024", None),
  ];
  for (text, expected) in cases {
    assert_eq!(bits(hex_float(text.as_bytes())), expected, "`{text}`");
  }
  assert_eq!(f64::MIN_POSITIVE.to_bits(), 0x0010_0000_0000_0000);
  assert_eq!(f64::MAX.to_bits(), 0x7fef_ffff_ffff_ffff);
}

#[test]
fn a_hexadecimal_floats_exponent_is_decimal() {
  for text in ["0x1p10", "0x1p1_0", "0x1p+10", "0x1P10"] {
    assert_eq!(
      bits(hex_float(text.as_bytes())),
      Some(0x4090_0000_0000_0000),
      "`{text}` is 1024"
    );
  }
  // A letter after `p` is no decimal digit, and an exponent needs a digit.
  assert!(hex_float(b"0x1p1f").is_none());
  assert!(hex_float(b"0x1p").is_none());
  // Thirty digits saturate: up, no `f64` and out of `f32`; down, zero.
  let up = hex_float(joined("0x1p", &"9".repeat(30)).as_bytes()).expect("a hexadecimal float");
  assert!(up.value.is_none() && !up.single);
  assert_eq!(
    bits(hex_float(joined("0x1p-", &"9".repeat(30)).as_bytes())),
    Some(0)
  );
  // Each digit of the fraction part takes four from the exponent, a leading zero among them.
  let long = joined("0x0.", &joined(&"0".repeat(29), "1p120"));
  assert_eq!(
    bits(hex_float(long.as_bytes())),
    Some(0x3ff0_0000_0000_0000)
  );
}

#[test]
fn a_floats_range_in_f32_is_judged_on_its_exact_value() {
  let cases: [(&str, bool); 7] = [
    ("0x1.fffffep127", true),
    ("0x1.fffffefffffffp127", true),
    // Below the tie, though its `f64` rounds up onto it.
    ("0x1.fffffeffffffffp127", true),
    ("0x1.ffffffp127", false),
    ("-0x1.ffffffp127", false),
    ("0x1p128", false),
    ("0x1p-200", true),
  ];
  for (text, single) in cases {
    let read = hex_float(text.as_bytes()).expect("a hexadecimal float");
    assert_eq!(read.single, single, "`{text}`");
  }
  // The `f64` of the third is `0x1.ffffffp127`, which a verdict on the `f64` would refuse.
  assert_eq!(
    bits(hex_float(b"0x1.fffffeffffffffp127")),
    Some(0x47ef_ffff_f000_0000)
  );
}

#[test]
fn an_integer_where_a_float_is_expected_is_rounded_as_a_float_is() {
  let cases: [(&str, Radix, u64); 5] = [
    ("0x20000000000001", Radix::Hex, 0x4340_0000_0000_0000),
    ("0x20000000000003", Radix::Hex, 0x4340_0000_0000_0002),
    ("0o17", Radix::Octal, 0x402e_0000_0000_0000),
    ("0b101", Radix::Binary, 0x4014_0000_0000_0000),
    ("0x10", Radix::Hex, 0x4030_0000_0000_0000),
  ];
  for (text, radix, expected) in cases {
    let read = magnitude_float(false, &converted(text, radix));
    assert_eq!(read.value.map(f64::to_bits), Some(expected), "`{text}`");
    assert!(read.single, "`{text}` fits `f32`");
  }
  // `2^64 + 2^11` is a tie its top 53 bits break to even; a byte below the top eight that is not
  // zero is the sticky bit, which breaks it up: `2^64 + 2^11 + 1` is `2^64 + 2^12`.
  let tie = magnitude_float(false, &[0, 0x08, 0, 0, 0, 0, 0, 0, 1]);
  assert_eq!(tie.value.map(f64::to_bits), Some(0x43f0_0000_0000_0000));
  let above = magnitude_float(false, &[1, 0x08, 0, 0, 0, 0, 0, 0, 1]);
  assert_eq!(above.value.map(f64::to_bits), Some(0x43f0_0000_0000_0001));
  let negative = magnitude_float(true, &[1]);
  assert_eq!(
    negative.value.map(f64::to_bits),
    Some(0xbff0_0000_0000_0000)
  );
}

#[test]
fn a_decimal_float_is_read_by_the_standard_parser() {
  // Values an `f64` holds exactly, `_` skipped.
  let exact: [(&str, u64); 5] = [
    ("1.5", 0x3ff8_0000_0000_0000),
    ("1_000.5", 0x408f_4400_0000_0000),
    ("1e-400", 0),
    ("16", 0x4030_0000_0000_0000),
    // One less than the least value `f32` rounds to infinity: its `f64` is that value.
    (
      "340282356779733661637539395458142568447.0",
      0x47ef_ffff_f000_0000,
    ),
  ];
  for (text, expected) in exact {
    let read = decimal_float(text.as_bytes()).expect("a decimal float");
    assert_eq!(read.value.map(f64::to_bits), Some(expected), "`{text}`");
  }
  // Whether each is finite as an `f64`, and in `f32`'s range, each read by its own parse.
  let ranges: [(&str, bool, bool); 6] = [
    ("1e308", true, false),
    ("1e309", false, false),
    ("3.4028234e38", true, true),
    ("3.5e38", true, false),
    ("340282356779733661637539395458142568447.0", true, true),
    ("340282356779733661637539395458142568448.0", true, false),
  ];
  for (text, finite, single) in ranges {
    let read = decimal_float(text.as_bytes()).expect("a decimal float");
    assert_eq!(read.value.is_some(), finite, "`{text}` as `f64`");
    assert_eq!(read.single, single, "`{text}` in `f32`");
  }
}

#[test]
fn a_decimal_floats_exponent_is_read_whatever_its_digits_offset() {
  // Enough zeros that an exponent past 655 359, the most the standard parser reads exactly, is
  // offset back into range; fewer under Miri, where each verdict is the same.
  let zeros = "0".repeat(if cfg!(miri) { 997 } else { 99_998 });
  let count = (zeros.len() + 1).to_string();
  // `0.`, the zeros, `1e999999` is 10^900000 when unshrunk, past every `f64` and `f32`; the
  // standard parser alone reads it as 1.0.
  let codex = joined(&joined("0.", &zeros), "1e999999");
  let read = decimal_float(codex.as_bytes()).expect("a decimal float");
  assert!(read.value.is_none(), "past every `f64`");
  assert!(!read.single, "past every `f32`");
  // `1`, the zeros and one more, `e-999999` is as far below every float, and zero in each; the
  // standard parser alone reads it as 1.0 too.
  let reverse = joined(&joined("1", &zeros), "0e-999999");
  let read = decimal_float(reverse.as_bytes()).expect("a decimal float");
  assert_eq!(read.value.map(f64::to_bits), Some(0));
  assert!(read.single, "zero is in `f32`'s range");
  // The same digits under an exponent that offsets them: 1.0 each way, in both.
  for text in [
    joined(&joined(&joined("0.", &zeros), "1e"), &count),
    joined(&joined(&joined("1", &zeros), "0e-"), &count),
  ] {
    let read = decimal_float(text.as_bytes()).expect("a decimal float");
    assert_eq!(read.value.map(f64::to_bits), Some(1f64.to_bits()));
    assert!(read.single);
  }
  // A first significant digit at each edge: 10^308 and 10^-400 are spelled, 10^309 is past every
  // float and 10^-401 is zero; `f32`'s range ends between 10^38 and 10^39.
  let edges: [(&str, Option<u64>, bool); 7] = [
    ("1e308", Some(1e308f64.to_bits()), false),
    ("1e309", None, false),
    ("1e-400", Some(0), true),
    ("1e-401", Some(0), true),
    ("1e38", Some(1e38f64.to_bits()), true),
    ("1e39", Some(1e39f64.to_bits()), false),
    ("-0.0e999999", Some((-0f64).to_bits()), true),
  ];
  for (text, wide, single) in edges {
    let read = decimal_float(text.as_bytes()).expect("a decimal float");
    assert_eq!(read.value.map(f64::to_bits), wide, "`{text}` as `f64`");
    assert_eq!(read.single, single, "`{text}` in `f32`");
  }
  // The greatest finite and the least subnormal `f64` read exactly.
  let max = decimal_float(b"1.7976931348623157e308").expect("a decimal float");
  assert_eq!(max.value.map(f64::to_bits), Some(f64::MAX.to_bits()));
  let least = decimal_float(b"5e-324").expect("a decimal float");
  assert_eq!(least.value.map(f64::to_bits), Some(1));
}
