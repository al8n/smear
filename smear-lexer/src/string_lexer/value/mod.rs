//! Draft §2.9's `StringValue` static semantics, read off the bytes of a spelling.
//!
//! # Why this reads a spelling and not a literal
//!
//! The carriers above keep their source, and their `Cow` conversions cook it — over `&str`, from
//! the line facts the lexer measured on the way past. Neither reaches a caller that holds the
//! spelling alone, and the callers that matter do: a parsed `StringValue` publishes its source and
//! nothing the lexer measured, `smear-schema`'s builder is generic over `S: AsRef<[u8]>`, and an
//! executor answering `__type(name:)` meets the literal in a document of any source type. So the
//! three functions here take the spelling — delimiters included — and apply §2.9.1's escapes or
//! §2.9.4's `BlockStringValue()` themselves.
//!
//! # Bytes in, text out
//!
//! The spelling is bytes because a source is; the value is text because draft §2.9 says a string is
//! a sequence of Unicode scalar values. The two meet once, where the delimiters come off: the
//! content is checked for UTF-8 there, and every escape produces a scalar value, so every piece
//! handed out afterwards is a `&str` and an out-buffer is a `String` with no check left to make.
//! The parsers' byte lexer refuses a string literal that is not UTF-8 (smear#246), so a spelling
//! they lexed passes that check; [`StringValueError::NotUtf8`] answers a spelling from anywhere
//! else.
//!
//! # Pieces, and the two callers that want them
//!
//! [`string_value_pieces`] hands the value out as consecutive `&str` pieces — runs of the
//! spelling's own bytes, one escape's character, a line feed between two block lines — and never
//! splits a character across two of them. [`string_value_len`] adds them up and
//! [`decode_string_value`] appends them, which is how an arena is reserved from the decoded length
//! before a byte of it is written. A printer that re-encodes the value walks the pieces too and
//! needs no buffer of its own.

use std::string::String;

use crate::error::StringValueError;

/// Walks the value a GraphQL `StringValue` spelling denotes, one piece at a time.
///
/// `spelling` is the literal as a document wrote it, delimiters included: `"…"` takes draft
/// §2.9.1's escapes — `\"` `\\` `\/` `\b` `\f` `\n` `\r` `\t`, `\uXXXX` with surrogate pairs, and
/// `\u{X…}` of one to six digits, the lexer's own bound — and `"""…"""` takes §2.9.4's
/// `BlockStringValue()`: common indentation off every line but the first, blank lines off both
/// ends, lines joined by a line feed whatever terminator the source spelled, and `\"""` read as
/// `"""`.
///
/// The pieces concatenate to the value and never split a character, so each is valid UTF-8 on its
/// own. Nothing allocates.
///
/// # Errors
///
/// [`StringValueError`]: a spelling with no delimiters, content that is not UTF-8, or a backslash
/// that begins no escape. A refusal can come after some pieces were handed out; a caller that
/// writes them somewhere sizes the value with [`string_value_len`] first, as [`decode_string_value`]
/// does, and so meets every refusal before it writes.
pub fn string_value_pieces(
  spelling: &[u8],
  mut piece: impl FnMut(&str),
) -> Result<(), StringValueError> {
  let (content, block) = match spelling {
    [b'"', b'"', b'"', content @ .., b'"', b'"', b'"'] => (content, true),
    // `"""` opens a block string and nothing else, so a spelling that starts with one and does not
    // end with one is no string at all rather than an inline one holding quotes.
    [b'"', b'"', b'"', ..] => return Err(StringValueError::Unquoted),
    [b'"', content @ .., b'"'] => (content, false),
    _ => return Err(StringValueError::Unquoted),
  };
  let content = core::str::from_utf8(content).map_err(|_| StringValueError::NotUtf8)?;
  if block {
    block_value(content, &mut piece);
    Ok(())
  } else {
    inline_value(content, &mut piece)
  }
}

/// Returns the length in bytes of the value a `StringValue` spelling denotes.
///
/// The sizing half of [`decode_string_value`]: an exact count, not a bound, because it is the same
/// walk with the bytes added up instead of written.
///
/// # Errors
///
/// As [`string_value_pieces`].
pub fn string_value_len(spelling: &[u8]) -> Result<usize, StringValueError> {
  let mut len = 0usize;
  string_value_pieces(spelling, |piece| len += piece.len())?;
  Ok(len)
}

/// Appends the value a `StringValue` spelling denotes to `out`, reserving exactly its length
/// first.
///
/// Two walks over the spelling — [`string_value_len`], then the copy — so `out` grows by one
/// reservation of the decoded length at most, and a refusal leaves it as it was.
///
/// # Errors
///
/// As [`string_value_pieces`], with nothing appended.
pub fn decode_string_value(spelling: &[u8], out: &mut String) -> Result<(), StringValueError> {
  let len = string_value_len(spelling)?;
  out.reserve(len);
  let start = out.len();
  let decoded = string_value_pieces(spelling, |piece| out.push_str(piece));
  // Unreachable: the sizing walk above read the same spelling and accepted it. Undone rather than
  // assumed, so a caller is never handed half a value beside an error.
  if decoded.is_err() {
    out.truncate(start);
  }
  decoded
}

/// An inline string's content, its escapes applied.
fn inline_value<F: FnMut(&str)>(content: &str, piece: &mut F) -> Result<(), StringValueError> {
  let bytes = content.as_bytes();
  let mut copied = 0;
  // `str::find` with a `char` takes the memchr route; `copied` is always a character boundary, at
  // the start or just past an escape, which is ASCII to its last byte.
  while let Some(offset) = content[copied..].find('\\') {
    let slash = copied + offset;
    if slash > copied {
      piece(&content[copied..slash]);
    }
    // `+ 1` for the opening quote, so the offset names the backslash in the spelling.
    let (ch, consumed) =
      escape(&bytes[slash + 1..]).ok_or(StringValueError::InvalidEscape(slash + 1))?;
    piece(ch.encode_utf8(&mut [0u8; 4]));
    copied = slash + 1 + consumed;
  }
  if copied < content.len() {
    piece(&content[copied..]);
  }
  Ok(())
}

/// Reads one escape body — everything after the backslash — as its character and the bytes it
/// spans.
fn escape(after: &[u8]) -> Option<(char, usize)> {
  let simple = match *after.first()? {
    b'"' => '"',
    b'\\' => '\\',
    b'/' => '/',
    b'b' => '\u{8}',
    b'f' => '\u{c}',
    b'n' => '\n',
    b'r' => '\r',
    b't' => '\t',
    b'u' => return unicode(&after[1..]).map(|(ch, consumed)| (ch, consumed + 1)),
    _ => return None,
  };
  Some((simple, 1))
}

/// Reads a `\u` escape body — everything after the `u` — in either of draft §2.9.1's spellings.
fn unicode(after: &[u8]) -> Option<(char, usize)> {
  if let [b'{', braced @ ..] = after {
    let close = braced.iter().position(|&byte| byte == b'}')?;
    let digits = &braced[..close];
    // One to six, the lexer's bound: the braced form names a scalar value, and six hex digits are
    // already past U+10FFFF.
    if !(1..=6).contains(&digits.len()) {
      return None;
    }
    // `+ 2` for the braces the offset does not span.
    return char::from_u32(hex(digits)?).map(|ch| (ch, close + 2));
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
    return char::from_u32(combined).map(|ch| (ch, 10));
  }
  // A lone trailing surrogate is refused here, by `from_u32`.
  char::from_u32(leading).map(|ch| (ch, 4))
}

/// Reads hex digits — draft §2.9.1's `0-9`, `A-F` and `a-f` and nothing else — as a number.
///
/// At most six of them reach here, so the value fits.
fn hex(digits: &[u8]) -> Option<u32> {
  digits.iter().try_fold(0u32, |value, &digit| {
    Some(value * 16 + char::from(digit).to_digit(16)?)
  })
}

/// A block string's content, as draft §2.9.4's `BlockStringValue()` reads it.
///
/// Two walks over the lines: the first finds the common indentation of every line after the first
/// that is not blank, and the first and last lines that are not blank; the second hands out the
/// lines between them, each but the raw first one dedented, with a line feed between each pair.
/// Every byte the algorithm removes is a space, a tab or a line terminator, so a cut never lands
/// inside a character.
fn block_value<F: FnMut(&str)>(content: &str, piece: &mut F) {
  let mut common: Option<usize> = None;
  let mut kept: Option<(usize, usize)> = None;
  for (index, line) in Lines::new(content).enumerate() {
    let width = indent(line);
    if width == line.len() {
      continue;
    }
    kept = Some(match kept {
      None => (index, index),
      Some((first, _)) => (first, index),
    });
    if index > 0 && common.is_none_or(|common| width < common) {
      common = Some(width);
    }
  }
  // Every line blank: steps 5 and 6 remove them all, and the value is empty.
  let Some((first, last)) = kept else {
    return;
  };
  let common = common.unwrap_or(0);
  for (index, line) in Lines::new(content).enumerate().take(last + 1).skip(first) {
    if index > first {
      piece("\n");
    }
    // Step 4 exempts the first line of the raw split, and that line only: not a blank line, and not
    // the first line steps 5 and 6 leave, which is a different line whenever the block opens with a
    // terminator. A blank line shorter than the common indentation loses all of it.
    let line = if index == 0 {
      line
    } else {
      &line[indent(line).min(common)..]
    };
    unescape_triple_quotes(line, piece);
  }
}

/// The leading spaces and tabs of a line — draft §2.9.4's `WhiteSpace`, both one byte.
fn indent(line: &str) -> usize {
  line
    .bytes()
    .take_while(|&byte| byte == b' ' || byte == b'\t')
    .count()
}

/// One block line, with §2.9.5's only escape read: `\"""` is `"""`.
fn unescape_triple_quotes<F: FnMut(&str)>(line: &str, piece: &mut F) {
  let mut rest = line;
  while let Some(at) = rest.find("\\\"\"\"") {
    if at > 0 {
      piece(&rest[..at]);
    }
    piece("\"\"\"");
    rest = &rest[at + 4..];
  }
  if !rest.is_empty() {
    piece(rest);
  }
}

/// A block string's content, split on draft §2.9.4's `LineTerminator`s: `\r\n`, `\n` and `\r`.
///
/// An empty content is one empty line, and a trailing terminator opens one more — the split the
/// algorithm's steps are written against.
struct Lines<'a> {
  rest: Option<&'a str>,
}

impl<'a> Lines<'a> {
  fn new(content: &'a str) -> Self {
    Self {
      rest: Some(content),
    }
  }
}

impl<'a> Iterator for Lines<'a> {
  type Item = &'a str;

  fn next(&mut self) -> Option<&'a str> {
    let rest = self.rest?;
    let Some(end) = rest.bytes().position(|byte| byte == b'\n' || byte == b'\r') else {
      self.rest = None;
      return Some(rest);
    };
    let terminator = if rest.as_bytes()[end..].starts_with(b"\r\n") {
      2
    } else {
      1
    };
    self.rest = Some(&rest[end + terminator..]);
    Some(&rest[..end])
  }
}

#[cfg(test)]
mod tests;
