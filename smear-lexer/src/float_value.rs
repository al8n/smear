//! A decimal float literal's value, read for `core`'s float parser, as `string_value_pieces`
//! reads a string literal's value from its spelling: [`normalize_decimal_float`].

/// The significant digits a spelling keeps: more than the 767 any halfway point between two `f64`s
/// has.
const KEPT: usize = 800;

/// The bound in magnitude of a written exponent as it is read.
const EXPONENT_BOUND: i64 = 1 << 40;

/// The least exponent of a first significant digit past `f64::MAX`.
const OVERFLOW: i64 = 309;

/// The least exponent of a first significant digit that is written into a spelling.
const UNDERFLOW: i64 = -400;

/// How long a buffer [`normalize_decimal_float`] writes into is: a sign, the 800 digits it keeps
/// and a final one, a point, an `e` and an exponent of at most four characters.
pub const NORMALIZED_FLOAT_LEN: usize = 808;

const _: () = assert!(1 + KEPT + 1 + 1 + 1 + 4 <= NORMALIZED_FLOAT_LEN);

/// What [`normalize_decimal_float`] reads a decimal float literal as.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum DecimalFloat<'a> {
  /// Zero, or a value no binary float tells from zero: no digit of the literal is significant, or
  /// the first that is stands below 10^-400.
  Zero {
    /// Whether the literal is written with `-`.
    negative: bool,
  },
  /// A value past every finite `f64`, and so past every finite `f32`: the literal's first
  /// significant digit stands at 10^309 or higher.
  Overflow {
    /// Whether the literal is written with `-`.
    negative: bool,
  },
  /// Any other value, spelled as its significant digits with the exponent of its first, from -400
  /// to 308, as `d.ddd…e<E>`: `str::parse::<f64>` and `::<f32>` read it as they would read the
  /// literal if they read every exponent exactly.
  Spelled(&'a str),
}

/// Where a literal's first significant digit stands.
#[derive(Clone, Copy)]
enum Start {
  /// In the integer part.
  Integer,
  /// In the fraction, its digit at this place, 1 for the tenths.
  Fraction(i64),
}

/// The bytes of a spelling being written.
struct Writer<'b> {
  buffer: &'b mut [u8; NORMALIZED_FLOAT_LEN],
  len: usize,
}

impl Writer<'_> {
  /// Appends `byte`, unless the buffer is full, which the spelling's bound rules out.
  fn push(&mut self, byte: u8) {
    if let Some(slot) = self.buffer.get_mut(self.len) {
      *slot = byte;
      self.len += 1;
    }
  }
}

/// Reads `spelling`, a decimal float literal as a lexer spells it, into what it is worth to a
/// binary float: an optional `-`, digits with an optional fraction after a `.`, and an optional
/// exponent after `e` or `E` with an optional sign, each `_` skipped wherever it stands. A decimal
/// integer is one too. A spelling it answers is written into `buffer`, so it needs no allocator.
///
/// # Why the literal is rewritten
///
/// `str::parse::<f64>` and `::<f32>` round a decimal literal of any length correctly, but for its
/// exponent: they stop reading the exponent's digits once the exponent read so far reaches 65 536.
/// A literal whose written exponent is larger, and whose digits offset it, is then misread: `0.`
/// and 99 998 zeros then `1e999999` is 10^900000 and reads as 1.0, and `1` and 99 999 zeros then
/// `e-999999` is 10^-900000 and reads as 1.0 too. So one scan of the literal reads, in saturating
/// arithmetic, whether any digit is significant, the place of the first that is, and the written
/// exponent, its magnitude saturated at 2^40, past any count of digits a source can hold to offset
/// it. Their sum is the exponent `E` of the first significant digit:
/// - `E` of 309 or more is past `f64::MAX`, below 1.8 × 10^308, and so past every finite `f32`:
///   [`DecimalFloat::Overflow`];
/// - `E` below -400 is below 10^-400, far below half of `f64`'s least subnormal, 2^-1075, which is
///   above 10^-324, so the value rounds to zero in `f64` and in `f32`, whose range lies inside:
///   [`DecimalFloat::Zero`], as is a literal with no significant digit;
/// - any other `E`, from -400 to 308, is written into a spelling the parser reads exactly:
///   [`DecimalFloat::Spelled`].
///
/// # Why a fixed buffer is exact
///
/// The spelling keeps the literal's first 800 significant digits and, when a later digit is not
/// zero, a final `1` after them. No halfway point between two `f64`s, nor between two `f32`s, has
/// more than 767 significant digits, so the value and the spelling lie on the same side of each,
/// and round alike; `core`'s parser keeps 768 digits and a flag for a later digit that is not zero
/// for the same reason. So the buffer bounds no literal's length, and changes no value the parser
/// reads right.
///
/// `None` for a spelling that is no decimal float: an unexpected byte, a second point, no digit,
/// or an exponent with no digit.
pub fn normalize_decimal_float<'b>(
  spelling: &[u8],
  buffer: &'b mut [u8; NORMALIZED_FLOAT_LEN],
) -> Option<DecimalFloat<'b>> {
  let (negative, rest) = match spelling.split_first() {
    Some((b'-', rest)) => (true, rest),
    _ => (false, spelling),
  };
  let (mantissa, exponent) = match rest
    .iter()
    .position(|byte: &u8| matches!(byte, b'e' | b'E'))
  {
    Some(at) => {
      let (mantissa, marked) = rest.split_at_checked(at)?;
      (mantissa, Some(marked.get(1..)?))
    }
    None => (rest, None),
  };
  let mut written = Writer { buffer, len: 0 };
  if negative {
    written.push(b'-');
  }
  let mut any = false;
  let mut start: Option<Start> = None;
  let mut integer: i64 = 0;
  let mut fraction: i64 = 0;
  let mut in_fraction = false;
  let mut kept: usize = 0;
  let mut sticky = false;
  for &byte in mantissa {
    match byte {
      b'_' => continue,
      b'.' if !in_fraction => {
        in_fraction = true;
        continue;
      }
      b'0'..=b'9' => {}
      _ => return None,
    }
    any = true;
    if in_fraction {
      fraction = fraction.saturating_add(1);
    }
    if start.is_none() {
      // A leading zero is no significant digit.
      if byte == b'0' {
        continue;
      }
      start = Some(if in_fraction {
        Start::Fraction(fraction)
      } else {
        Start::Integer
      });
    }
    if !in_fraction {
      integer = integer.saturating_add(1);
    }
    if kept < KEPT {
      if kept == 1 {
        written.push(b'.');
      }
      written.push(byte);
      kept += 1;
    } else if byte != b'0' {
      sticky = true;
    }
  }
  if !any {
    return None;
  }
  let power = match exponent {
    Some(exponent) => read_exponent(exponent)?,
    None => 0,
  };
  let Some(start) = start else {
    return Some(DecimalFloat::Zero { negative });
  };
  let lead = match start {
    Start::Integer => integer.saturating_sub(1),
    Start::Fraction(place) => place.saturating_neg(),
  };
  let e = lead.saturating_add(power);
  if e >= OVERFLOW {
    return Some(DecimalFloat::Overflow { negative });
  }
  if e < UNDERFLOW {
    return Some(DecimalFloat::Zero { negative });
  }
  // A digit past the kept ones that is not zero: a final `1` after them keeps the value on its
  // side of every halfway point.
  if sticky {
    written.push(b'1');
  }
  written.push(b'e');
  if e < 0 {
    written.push(b'-');
  }
  let magnitude = e.unsigned_abs();
  let (hundreds, tens, ones) = (magnitude / 100, magnitude / 10 % 10, magnitude % 10);
  if hundreds > 0 {
    written.push(ascii(hundreds));
  }
  if hundreds > 0 || tens > 0 {
    written.push(ascii(tens));
  }
  written.push(ascii(ones));
  let Writer { buffer, len } = written;
  let buffer: &'b [u8; NORMALIZED_FLOAT_LEN] = buffer;
  core::str::from_utf8(buffer.get(..len)?)
    .ok()
    .map(DecimalFloat::Spelled)
}

/// The digits after a literal's `e` or `E`, with an optional sign, each `_` skipped, as a number:
/// its magnitude saturated at [`EXPONENT_BOUND`]. `None` for no digit, or a byte that is none.
fn read_exponent(written: &[u8]) -> Option<i64> {
  let (negative, digits) = match written.split_first() {
    Some((b'-', rest)) => (true, rest),
    Some((b'+', rest)) => (false, rest),
    _ => (false, written),
  };
  let mut any = false;
  let mut value: i64 = 0;
  for &byte in digits {
    match byte {
      b'_' => continue,
      b'0'..=b'9' => {
        value = (value * 10 + i64::from(byte - b'0')).min(EXPONENT_BOUND);
        any = true;
      }
      _ => return None,
    }
  }
  if !any {
    return None;
  }
  Some(if negative { -value } else { value })
}

/// The ASCII digit of `digit`, which is below ten.
fn ascii(digit: u64) -> u8 {
  u8::try_from(digit).map_or(b'0', |digit: u8| b'0' + digit)
}

#[cfg(test)]
mod tests;
