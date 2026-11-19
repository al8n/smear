//! Draft §2.9.1's escapes, read in one place.
//!
//! [`escape`] is the workspace's one reader of an inline string's escapes. The decoder's inline
//! walk calls it, and through that walk so do the `Cow` conversion on a lexed literal and
//! `smear`'s JSON writer. It reads bytes, as a source holds them, and answers the character an
//! escape names and how many bytes after the backslash the escape spans, or that the bytes begin
//! no escape at all. What a backslash that begins none means is its caller's to say: the decoder
//! refuses the spelling, and the `Cow` conversion copies the backslash through.
//!
//! The lexers' side of a malformed fixed escape is here too, so the workspace has one notion of a
//! fixed escape's digit: [`fixed_digits`] measures what a bare `\u` takes as its digits, and
//! [`ends_escape`] is the set of bytes every Unicode escape stops before.

use tokora::{
  SimpleSpan,
  utils::{Lexeme, PositionedChar},
};

use crate::error::{InvalidUnicodeHexDigits, UnicodeError};

/// One escape draft §2.9.1 defines, read off the bytes after its backslash.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Escape {
  /// The character the escape names.
  pub(crate) ch: char,
  /// How many bytes after the backslash the escape spans: 1 for `\n`, 5 for `A`, 11 for a
  /// surrogate pair, and the braces included for `\u{…}`.
  pub(crate) len: usize,
}

/// Reads the escape that `after`, the bytes after a backslash, begins.
///
/// `\"` `\\` `\/` `\b` `\f` `\n` `\r` `\t`; the fixed-width `\uXXXX`, where a leading surrogate is
/// a character only with its trailing half spelled as a second `\uXXXX` right after it; and the
/// braced `\u{X…}` of one to six hex digits, the lexer's own bound. `None` when `after` begins none
/// of them: another character or none, a digit that is not hex, too few or too many of them, a lone
/// surrogate in either spelling, or a value past U+10FFFF.
///
/// It reads at most the eleven bytes a surrogate pair spans, so it costs the same however long
/// the literal is.
pub(crate) fn escape(after: &[u8]) -> Option<Escape> {
  let ch = match *after.first()? {
    b'"' => '"',
    b'\\' => '\\',
    b'/' => '/',
    b'b' => '\u{8}',
    b'f' => '\u{c}',
    b'n' => '\n',
    b'r' => '\r',
    b't' => '\t',
    // `+ 1` for the `u`.
    b'u' => return unicode(&after[1..]).map(|Escape { ch, len }| Escape { ch, len: len + 1 }),
    _ => return None,
  };
  Some(Escape { ch, len: 1 })
}

/// Reads a `\u` escape's body, everything after the `u`, in either of draft §2.9.1's spellings.
fn unicode(after: &[u8]) -> Option<Escape> {
  if let [b'{', braced @ ..] = after {
    // One to six digits, the lexer's bound: the braced form names a scalar value, and six hex
    // digits already reach past U+10FFFF. So the `}` is among the seven bytes after the `{`, or
    // the escape is none, and nothing past them is read.
    let close = braced.iter().take(7).position(|&byte| byte == b'}')?;
    if close == 0 {
      return None;
    }
    // `+ 2` for the two braces.
    return char::from_u32(hex(&braced[..close])?).map(|ch| Escape { ch, len: close + 2 });
  }

  let leading = hex(after.get(..4)?)?;
  // A leading surrogate is a character only with its trailing half, which the grammar spells as a
  // second `\u` escape immediately after this one. Anything else is not a character at all.
  if (0xD800..0xDC00).contains(&leading) {
    let trailing = match after.get(4..)? {
      [b'\\', b'u', tail @ ..] => hex(tail.get(..4)?)?,
      _ => return None,
    };
    if !(0xDC00..0xE000).contains(&trailing) {
      return None;
    }
    let combined = 0x1_0000 + ((leading - 0xD800) << 10) + (trailing - 0xDC00);
    return char::from_u32(combined).map(|ch| Escape { ch, len: 10 });
  }
  // A lone trailing surrogate is refused here, by `from_u32`.
  char::from_u32(leading).map(|ch| Escape { ch, len: 4 })
}

/// Reads hex digits as a number, or `None` when one of them is not a hex digit.
///
/// At most six reach here, so the value fits.
fn hex(digits: &[u8]) -> Option<u32> {
  digits
    .iter()
    .try_fold(0u32, |value, &digit| Some(value * 16 + hex_digit(digit)?))
}

/// A hex digit's value: draft §2.9.1's `0-9`, `A-F` and `a-f`, and nothing else.
const fn hex_digit(byte: u8) -> Option<u32> {
  match byte {
    b'0'..=b'9' => Some((byte - b'0') as u32),
    b'A'..=b'F' => Some((byte - b'A' + 10) as u32),
    b'a'..=b'f' => Some((byte - b'a' + 10) as u32),
    _ => None,
  }
}

/// Whether `byte` ends a Unicode escape's digits: the literal's closing quote, the next escape's
/// backslash, or a line end. The braced form adds its own `}`.
///
/// All four are ASCII, so the one byte test serves a text source and a byte source alike, and a
/// cut before one never splits a character or an ill-formed sequence, neither of which holds an
/// ASCII byte. The other spelling of the set is the sub-lexers' partial-brace class
/// `[^0-9A-Fa-f}"\\\r\n]`, which leaves out these four beside the `}` and the hex digits: a Logos
/// rule cannot call a function.
pub(crate) const fn ends_escape(byte: u8) -> bool {
  matches!(byte, b'"' | b'\\' | b'\r' | b'\n')
}

/// How many bytes after a bare `\u` its measurement reads. Four units of at most four bytes each:
/// the fourth starts at byte 12 or earlier and ends by 16, and a maximal ill-formed subpart is at
/// most three bytes. So the window holds every unit the rule can take, each of them whole.
const FIXED_WINDOW: usize = 16;

/// One unit a bare `\u` takes as a digit: a UTF-8 scalar, or in a byte source a maximal ill-formed
/// subpart.
#[derive(Clone, Copy, Default)]
struct FixedDigit {
  /// Its byte offset after the `u`.
  offset: usize,
  /// Its first byte: a character's lead byte, or an ill-formed subpart's first.
  lead: u8,
  /// The scalar, for a well-formed unit.
  ch: Option<char>,
}

impl FixedDigit {
  /// Whether the unit is a hex digit. A hex digit is one ASCII byte, and no other unit starts with
  /// an ASCII byte, so the first byte decides.
  const fn is_hex(&self) -> bool {
    hex_digit(self.lead).is_some()
  }
}

/// What a bare `\u` takes as its digits, when the four hex digits the fixed form needs are not all
/// there: up to four units after the `u`, stopping before the first `"`, `\`, CR or LF. A unit is a
/// UTF-8 scalar, or in a byte source a maximal ill-formed subpart.
pub(crate) struct FixedDigits {
  /// The units taken, in order; the first `taken` of them.
  units: [FixedDigit; 4],
  /// How many units the escape took, at most four.
  taken: u8,
  /// Bytes taken after the `u`: the end of the last unit taken.
  len: usize,
}

/// Measures what a bare `\u` takes as its digits, from `after_u`, the bytes after its `u`.
///
/// It reads at most the window's sixteen bytes, cut before the first `"`, `\`, CR or LF, so it
/// costs the same however long the literal is: a measurement of the rest of the document would cost
/// its length for every bare `\u`, since one step of `utf8_chunks` validates up to the first
/// ill-formed sequence or the end of what it is handed.
pub(crate) fn fixed_digits(after_u: &[u8]) -> FixedDigits {
  let window = &after_u[..after_u.len().min(FIXED_WINDOW)];
  let window = match window.iter().position(|&byte| ends_escape(byte)) {
    Some(stop) => &window[..stop],
    None => window,
  };

  let units = window.utf8_chunks().flat_map(|chunk| {
    let invalid = chunk.invalid();
    chunk
      .valid()
      .chars()
      .map(|ch| (Some(ch), ch.len_utf8()))
      .chain((!invalid.is_empty()).then_some((None, invalid.len())))
  });

  let mut digits = FixedDigits {
    units: [FixedDigit::default(); 4],
    taken: 0,
    len: 0,
  };
  for (slot, (ch, width)) in digits.units.iter_mut().zip(units) {
    *slot = FixedDigit {
      offset: digits.len,
      lead: window[digits.len],
      ch,
    };
    digits.taken += 1;
    digits.len += width;
  }
  digits
}

impl FixedDigits {
  /// Bytes taken after the `u`, which the escape's token covers.
  pub(crate) const fn len(&self) -> usize {
    self.len
  }

  /// A text source's error for the escape whose `\` is at `backslash`.
  pub(crate) fn text_error(&self, backslash: usize) -> UnicodeError<char> {
    self.error(backslash, |unit| {
      unit
        .ch
        .expect("a text source's window holds its first four units whole, as characters")
    })
  }

  /// A byte source's error for the escape whose `\` is at `backslash`, reporting a digit that is
  /// not hex by its first byte.
  pub(crate) fn byte_error(&self, backslash: usize) -> UnicodeError<u8> {
    self.error(backslash, |unit| unit.lead)
  }

  /// The error over `\u` and the units taken: an incomplete escape when every unit taken is hex,
  /// none taken included, and an invalid sequence of the units that are not hex otherwise.
  fn error<Char>(
    &self,
    backslash: usize,
    digit: impl Fn(&FixedDigit) -> Char,
  ) -> UnicodeError<Char> {
    let span = SimpleSpan::new(backslash, backslash + 2 + self.len);
    let units = &self.units[..usize::from(self.taken)];
    if units.iter().all(FixedDigit::is_hex) {
      return UnicodeError::incomplete_fixed_unicode_escape(Lexeme::Range(span));
    }
    let mut digits = InvalidUnicodeHexDigits::default();
    for unit in units.iter().filter(|unit| !unit.is_hex()) {
      digits.push_fixed(PositionedChar::with_position(
        digit(unit),
        backslash + 2 + unit.offset,
      ));
    }
    UnicodeError::invalid_fixed_unicode_escape_sequence(digits, span, self.taken)
  }
}
