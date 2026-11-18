//! Draft §2.9.1's escapes, read in one place.
//!
//! [`escape`] is the workspace's one reader of an inline string's escapes. The decoder's inline
//! walk calls it, and through that walk so do the `Cow` conversion on a lexed literal and
//! `smear`'s JSON writer. It reads bytes, as a source holds them, and answers the character an
//! escape names and how many bytes after the backslash the escape spans, or that the bytes begin
//! no escape at all. What a backslash that begins none means is its caller's to say: the decoder
//! refuses the spelling, and the `Cow` conversion copies the backslash through.

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
