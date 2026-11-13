//! Reading a number literal: an integer of any length against a width, and a float, decimal or
//! hexadecimal, against `f64` and `f32`.
//!
//! Each function reads a literal's text as the lexer spells it, and answers `None` for a text the
//! lexer never makes. An integer is read in three steps, so that a literal that cannot fit costs
//! one scan: its significant digits are counted; a count past what the width expected can hold
//! refuses it whatever the digits are; and only then is its magnitude computed, little-endian,
//! from the last digit back for a radix that is a power of two and nine decimal digits at a time
//! otherwise. A float is rounded to nearest, a tie to even, and its range in `f32` is judged on
//! its exact value, never through its `f64`.

use std::{string::String, vec::Vec};

use crate::graphqlx::raw::Radix;

/// The bits of `f32`'s greatest exponent that make a value round to infinity: the 25 leading bits
/// of `0x1.ffffffp127`, the least value out of `f32`'s range, all set.
const SINGLE_EDGE: u64 = (1 << 25) - 1;

/// An integer literal as scanned, before it is converted.
#[derive(Clone, Copy)]
pub(super) struct Scanned {
  /// Whether it is written with `-`.
  pub(super) negative: bool,
  /// How many digits it has after its leading zeros: `_` is no digit.
  pub(super) significant: u64,
}

/// What a literal reads as where a float is expected.
#[derive(Clone, Copy)]
pub(super) struct Float {
  /// The nearest `f64`, when it is finite.
  pub(super) value: Option<f64>,
  /// Whether its exact value is below `0x1.ffffffp127` in magnitude, the least that `f32` rounds
  /// to infinity: whether it is in `f32`'s range.
  pub(super) single: bool,
}

/// How many values a digit of `radix` takes.
fn base(radix: Radix) -> u32 {
  match radix {
    Radix::Decimal => 10,
    Radix::Hex => 16,
    Radix::Octal => 8,
    Radix::Binary => 2,
  }
}

/// How many bits a digit of `radix` holds, for a radix that is a power of two.
fn digit_bits(radix: Radix) -> Option<u32> {
  match radix {
    Radix::Decimal => None,
    Radix::Hex => Some(4),
    Radix::Octal => Some(3),
    Radix::Binary => Some(1),
  }
}

/// The value of `byte` as a digit of `radix`: `None` for a byte that is no digit of it, `_` among
/// them.
fn digit(byte: u8, radix: Radix) -> Option<u32> {
  let value = match byte {
    b'0'..=b'9' => u32::from(byte - b'0'),
    b'a'..=b'f' => u32::from(byte - b'a') + 10,
    b'A'..=b'F' => u32::from(byte - b'A') + 10,
    _ => return None,
  };
  (value < base(radix)).then_some(value)
}

/// `text` without its sign: whether it is written with `-`, and the rest.
fn sign(text: &[u8]) -> (bool, &[u8]) {
  match text.split_first() {
    Some((b'-', rest)) => (true, rest),
    _ => (false, text),
  }
}

/// The digits of `text`, an integer literal of `radix`: its sign, and what follows its radix's
/// prefix, `0x`, `0o` or `0b`, none for a decimal one. `None` when the prefix is not there.
fn body(text: &[u8], radix: Radix) -> Option<(bool, &[u8])> {
  let (negative, rest) = sign(text);
  let prefix: &[u8] = match radix {
    Radix::Decimal => b"",
    Radix::Hex => b"0x",
    Radix::Octal => b"0o",
    Radix::Binary => b"0b",
  };
  Some((negative, rest.strip_prefix(prefix)?))
}

/// Scans `text`, an integer literal of `radix` as the lexer spells it: an optional `-`, its
/// radix's prefix, then digits and `_`. `None` when the prefix is missing, when no digit is
/// written, or when a byte is neither a digit of the radix nor `_`.
pub(super) fn scan(text: &[u8], radix: Radix) -> Option<Scanned> {
  let (negative, digits) = body(text, radix)?;
  let mut any = false;
  let mut significant: u64 = 0;
  for &byte in digits {
    if byte == b'_' {
      continue;
    }
    let value = digit(byte, radix)?;
    any = true;
    if significant > 0 || value != 0 {
      significant = significant.saturating_add(1);
    }
  }
  any.then_some(Scanned {
    negative,
    significant,
  })
}

/// The most significant digits of `radix` an integer of `width` bits can be written with: one with
/// more is out of the width's range, whatever its digits are. The decimal count is
/// `width × 30103 / 100000 + 2`, `30103 / 100000` being just above `log10(2)`.
pub(super) fn max_digits(radix: Radix, width: u32) -> u64 {
  let width = u64::from(width);
  match radix {
    Radix::Binary => width,
    Radix::Octal => width / 3 + 1,
    Radix::Hex => width / 4 + 1,
    Radix::Decimal => width * 30_103 / 100_000 + 2,
  }
}

/// Appends to `out` the magnitude of `text`, an integer literal of `radix` that [`scan`] read with
/// `significant` significant digits: little-endian, with no trailing zero byte, so zero is no byte.
/// A radix that is a power of two packs its digits' bits from the last digit back; a decimal one
/// is read nine digits at a time into little-endian `u32` limbs, `limbs` being their scratch.
pub(super) fn magnitude(
  text: &[u8],
  radix: Radix,
  significant: u64,
  out: &mut Vec<u8>,
  limbs: &mut Vec<u32>,
) {
  let Some((_, digits)) = body(text, radix) else {
    return;
  };
  let start = out.len();
  match digit_bits(radix) {
    Some(bits) => {
      // The significant digits are the last `significant` digits written.
      let mut held: u32 = 0;
      let mut count: u32 = 0;
      let mut left = significant;
      for &byte in digits.iter().rev() {
        if left == 0 {
          break;
        }
        // `scan` read every byte, so a byte that is no digit is `_`.
        let Some(value) = digit(byte, radix) else {
          continue;
        };
        held |= value << count;
        count += bits;
        left -= 1;
        while count >= 8 {
          let [low, ..] = held.to_le_bytes();
          out.push(low);
          held >>= 8;
          count -= 8;
        }
      }
      if count > 0 {
        let [low, ..] = held.to_le_bytes();
        out.push(low);
      }
    }
    None => {
      decimal(digits, limbs);
      for limb in limbs.iter() {
        out.extend_from_slice(&limb.to_le_bytes());
      }
    }
  }
  while out.len() > start && out.last() == Some(&0) {
    out.pop();
  }
}

/// The value of the decimal `digits`, each `_` skipped, as little-endian `u32` limbs, into
/// `limbs`: nine digits at a time after the leading zeros, every limb multiplied by `10^9`, or by
/// the power of ten of the last, shorter chunk, and the chunk added, in `u64`.
fn decimal(digits: &[u8], limbs: &mut Vec<u32>) {
  limbs.clear();
  let mut chunk: u64 = 0;
  let mut len: u32 = 0;
  let mut leading = true;
  for &byte in digits {
    let Some(value) = digit(byte, Radix::Decimal) else {
      continue;
    };
    if leading && value == 0 {
      continue;
    }
    leading = false;
    chunk = chunk * 10 + u64::from(value);
    len += 1;
    if len == 9 {
      multiply_add(limbs, 1_000_000_000, chunk);
      chunk = 0;
      len = 0;
    }
  }
  if len > 0 {
    multiply_add(limbs, 10u64.pow(len), chunk);
  }
}

/// `limbs`, little-endian `u32` limbs, times `factor`, plus `addend`, each step in `u64`: a limb is
/// the low 32 bits of a product, and the carry the rest. `factor` is at most `10^9` and `addend`
/// below it, so no step overflows.
fn multiply_add(limbs: &mut Vec<u32>, factor: u64, addend: u64) {
  let mut carry = addend;
  for limb in limbs.iter_mut() {
    let product = u64::from(*limb) * factor + carry;
    *limb = low_limb(product);
    carry = product >> 32;
  }
  while carry != 0 {
    limbs.push(low_limb(carry));
    carry >>= 32;
  }
}

/// The low 32 bits of `value`.
fn low_limb(value: u64) -> u32 {
  u32::try_from(value & u64::from(u32::MAX)).unwrap_or(0)
}

/// How many bits `magnitude`, little-endian with no trailing zero byte, is long: 0 for zero.
pub(super) fn bit_length(magnitude: &[u8]) -> u64 {
  match magnitude.split_last() {
    Some((&top, rest)) => (rest.len() as u64)
      .saturating_mul(8)
      .saturating_add(u64::from(8 - top.leading_zeros())),
    None => 0,
  }
}

/// Whether `magnitude`, little-endian with no trailing zero byte, is a power of two.
pub(super) fn is_power_of_two(magnitude: &[u8]) -> bool {
  match magnitude.split_last() {
    Some((&top, rest)) => top.is_power_of_two() && rest.iter().all(|byte: &u8| *byte == 0),
    None => false,
  }
}

/// Whether an integer, below zero when `negative`, whose magnitude is `bits` long and a power of two
/// when `power`, is in the range of the integer `width` bits wide, signed when `signed`. Zero is in
/// every range. Any other value fits `uN` when it is not negative and at most `N` bits long, and
/// `iN` when `N` is 1 or more and it is at most `N - 1` bits long, or it is negative, `N` bits
/// long and a power of two: `-128` fits `i8`.
pub(super) fn fits(negative: bool, bits: u64, power: bool, signed: bool, width: u32) -> bool {
  if bits == 0 {
    return true;
  }
  let width = u64::from(width);
  if signed {
    width >= 1 && (bits < width || (negative && bits == width && power))
  } else {
    !negative && bits <= width
  }
}

/// What `text`, a decimal float or integer literal, reads as where a float is expected: the text
/// without its `_`, which `scratch` holds, read by `str::parse` as an `f64` and, apart, as an
/// `f32`. `None` for a text `str::parse` refuses.
pub(super) fn decimal_float(text: &[u8], scratch: &mut String) -> Option<Float> {
  scratch.clear();
  for &byte in text {
    if byte == b'_' {
      continue;
    }
    if !byte.is_ascii() {
      return None;
    }
    scratch.push(char::from(byte));
  }
  let wide = scratch.parse::<f64>().ok()?;
  let single = scratch.parse::<f32>().ok()?;
  Some(Float {
    value: wide.is_finite().then_some(wide),
    single: single.is_finite(),
  })
}

/// What `text`, a hexadecimal float literal, `-?0x…(.…)?p±…`, reads as. Its mantissa is the first
/// 16 hexadecimal digits after its leading zeros, of the integer and the fraction parts together;
/// each digit after them is dropped, a sticky bit saying whether one was not zero. Its exponent of
/// two is the number after `p`, read in decimal, each `_` skipped, plus four for each digit dropped,
/// less four for each digit of the fraction part, all in saturating `i64`. `None` for a text with
/// no `p`, or with a byte after it that is no decimal digit, `_` or a sign.
pub(super) fn hex_float(text: &[u8]) -> Option<Float> {
  let (negative, rest) = sign(text);
  let rest = rest.strip_prefix(b"0x".as_slice())?;
  let at = rest
    .iter()
    .position(|byte: &u8| *byte == b'p' || *byte == b'P')?;
  let (written, exponent) = rest.split_at_checked(at)?;
  let mut mantissa: u64 = 0;
  let mut kept: u32 = 0;
  let mut dropped: i64 = 0;
  let mut fraction: i64 = 0;
  let mut sticky = false;
  let mut in_fraction = false;
  for &byte in written {
    if byte == b'_' {
      continue;
    }
    if byte == b'.' {
      in_fraction = true;
      continue;
    }
    let value = digit(byte, Radix::Hex)?;
    if in_fraction {
      fraction = fraction.saturating_add(1);
    }
    if kept == 0 && value == 0 {
      continue;
    }
    if kept < 16 {
      mantissa = (mantissa << 4) | u64::from(value);
      kept += 1;
    } else {
      dropped = dropped.saturating_add(1);
      sticky |= value != 0;
    }
  }
  let (_, exponent) = exponent.split_first()?;
  let (below, digits) = match exponent.split_first() {
    Some((b'-', rest)) => (true, rest),
    Some((b'+', rest)) => (false, rest),
    _ => (false, exponent),
  };
  let mut power: i64 = 0;
  let mut any = false;
  for &byte in digits {
    if byte == b'_' {
      continue;
    }
    let value = digit(byte, Radix::Decimal)?;
    power = power.saturating_mul(10).saturating_add(i64::from(value));
    any = true;
  }
  if !any {
    return None;
  }
  if below {
    power = power.saturating_neg();
  }
  let exp2 = power
    .saturating_add(dropped.saturating_mul(4))
    .saturating_sub(fraction.saturating_mul(4));
  Some(round(negative, mantissa, exp2, sticky))
}

/// What an integer, below zero when `negative`, of `magnitude`, little-endian with no trailing zero
/// byte, reads as where a float is expected: its top eight bytes the mantissa, eight times the
/// count of the bytes below them the exponent of two, and the sticky bit whether one of those is
/// not zero.
pub(super) fn magnitude_float(negative: bool, magnitude: &[u8]) -> Float {
  let below = magnitude.len().saturating_sub(8);
  let (low, top) = magnitude
    .split_at_checked(below)
    .unwrap_or((&[], magnitude));
  let mut mantissa: u64 = 0;
  for &byte in top.iter().rev() {
    mantissa = (mantissa << 8) | u64::from(byte);
  }
  let sticky = low.iter().any(|byte: &u8| *byte != 0);
  let exp2 = i64::try_from(below).unwrap_or(i64::MAX).saturating_mul(8);
  round(negative, mantissa, exp2, sticky)
}

/// The float `mantissa × 2^exp2`, below zero when `negative`, more than that by a little when
/// `sticky`: the mantissa is shifted left until its top bit is set, so that the value's leading
/// bit stands for `2^e`, `e` being the exponent plus 63. Its range in `f32` is judged then, on that
/// exact value; its `f64` is rounded after.
fn round(negative: bool, mantissa: u64, exp2: i64, sticky: bool) -> Float {
  let sign: u64 = if negative { 1 << 63 } else { 0 };
  if mantissa == 0 {
    return Float {
      value: Some(f64::from_bits(sign)),
      single: true,
    };
  }
  let shift = mantissa.leading_zeros();
  let normal = mantissa << shift;
  let e = exp2.saturating_sub(i64::from(shift)).saturating_add(63);
  let single = e < 127 || (e == 127 && normal >> 39 != SINGLE_EDGE);
  Float {
    value: wide(sign, normal, e, sticky),
    single,
  }
}

/// The `f64` of sign `sign` whose value is `normal`, which has its top bit set, its leading bit
/// standing for `2^e`, and a little more when `sticky`: `None` when it is not finite.
///
/// At `e` of -1022 or more it is normal: the top 53 bits, rounded by the 11 below them and the
/// sticky bit, a carry out of them adding one to `e`. Below, it is subnormal: shifted right by
/// `-1022 - e` more, rounded over everything shifted out. A shift of 64 leaves only the rounding,
/// and a longer one is zero; both are taken apart before any shift, since a `u64` shifted by 64
/// is no value.
fn wide(sign: u64, normal: u64, e: i64, sticky: bool) -> Option<f64> {
  if e > 1023 {
    return None;
  }
  if e >= -1022 {
    let mut q = nearest(normal, 11, sticky);
    let mut e = e;
    if q == 1 << 53 {
      q >>= 1;
      e += 1;
    }
    if e > 1023 {
      return None;
    }
    let field = u64::try_from(e + 1023).ok()?;
    return Some(f64::from_bits(sign | (field << 52) | (q & ((1 << 52) - 1))));
  }
  let shift = (-1011i64).saturating_sub(e);
  let q = match shift {
    ..=63 => nearest(normal, u32::try_from(shift).ok()?, sticky),
    // All of it is shifted out: it rounds up to the least subnormal when it is above half of
    // it, and a tie goes to zero, which is even.
    64 => u64::from(normal > 1 << 63 || (normal == 1 << 63 && sticky)),
    _ => 0,
  };
  // A carry out of a subnormal's 52 bits is the least normal number: `q` is then `1 << 52`, whose
  // set bit is the exponent field's 1.
  Some(f64::from_bits(sign | q))
}

/// `value` shifted right by `shift`, 1 to 63 bits, rounded to nearest by the bits shifted out and
/// `sticky`, a tie to even.
fn nearest(value: u64, shift: u32, sticky: bool) -> u64 {
  let q = value >> shift;
  let rest = value & ((1 << shift) - 1);
  let half = 1 << (shift - 1);
  let up = rest > half || (rest == half && (sticky || q & 1 == 1));
  q + u64::from(up)
}

#[cfg(test)]
mod tests;
