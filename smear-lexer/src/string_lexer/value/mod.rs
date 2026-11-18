//! Draft §2.9's `StringValue` static semantics, read off the bytes of a spelling: the one place
//! this workspace applies them.
//!
//! # Why this reads a spelling and not a literal
//!
//! The carriers above keep their source, and a caller that holds one cooks it with a `Cow`
//! conversion. That door does not reach a caller that holds the spelling alone, and the callers
//! that matter do: a parsed `StringValue` publishes its source and nothing the lexer measured,
//! `smear-schema`'s builder is generic over `S: AsRef<[u8]>`, and an executor answering
//! `__type(name:)` meets the literal in a document of any source type. So the three public
//! functions here take the spelling — delimiters included — and apply §2.9.1's escapes or
//! §2.9.4's `BlockStringValue()` themselves.
//!
//! # One reading behind every door
//!
//! The walks here are the only ones. The `Cow` conversions run them over a lexed carrier's body,
//! and `smear`'s JSON writer runs them through [`string_value_pieces`]. The inline walk reads every
//! escape with the one escape reader, `escape`, and its caller says what a backslash that begins
//! no escape is: a refusal here, and content in the `Cow` conversion, which answers on a literal
//! the lexer already accepted and so must neither panic nor refuse.
//!
//! # Bytes in, text out
//!
//! The spelling is bytes because a source is; the value is text because draft §2.9 says a string is
//! a sequence of Unicode scalar values. The two meet once, where the delimiters come off: the
//! content is checked for UTF-8 there, and every escape produces a scalar value, so every piece
//! handed out afterwards is a `&str`, which any [`core::fmt::Write`] takes with no check left to
//! make. The parsers' byte lexer refuses a string literal that is not UTF-8 (smear#246), so a
//! spelling they lexed passes that check; [`StringValueError::NotUtf8`] answers a spelling from
//! anywhere else.
//!
//! # Pieces, and the callers that want them
//!
//! [`string_value_pieces`] hands the value out as consecutive `&str` pieces — runs of the
//! spelling's own bytes, one escape's character, a line feed between two block lines — and never
//! splits a character across two of them. [`string_value_len`] adds them up, which is how an arena
//! is reserved from the decoded length before a byte of it is written, and [`decode_string_value`]
//! writes them to any `fmt::Write`. A printer that re-encodes the value walks the pieces too and
//! needs no buffer of its own.

use core::fmt;

use super::escape::{Escape, escape};
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
/// must write nothing on a refusal walks the spelling with [`string_value_len`] first, as
/// [`decode_string_value`] does, and so meets every refusal before it writes.
pub fn string_value_pieces(
  spelling: &[u8],
  mut piece: impl FnMut(&str),
) -> Result<(), StringValueError> {
  walk(spelling, &mut |text: &str| -> fmt::Result {
    piece(text);
    Ok(())
  })
}

/// Returns the length in bytes of the value a `StringValue` spelling denotes.
///
/// An exact count, not a bound, because it is the same walk with the bytes added up instead of
/// written. A `String` that reserves it before [`decode_string_value`] grows once.
///
/// # Errors
///
/// As [`string_value_pieces`].
pub fn string_value_len(spelling: &[u8]) -> Result<usize, StringValueError> {
  let mut len = 0usize;
  string_value_pieces(spelling, |piece| len += piece.len())?;
  Ok(len)
}

/// Appends the value a `StringValue` spelling denotes to `out`.
///
/// The spelling is checked whole before a byte is written, so a refused spelling writes nothing. A
/// writer that fails midway has received a prefix of the value, and the answer is
/// [`StringValueError::Sink`]. A `String` caller that wants one allocation reserves
/// [`string_value_len`] first.
///
/// Any [`core::fmt::Write`] takes the value: a `String`, a `&mut dyn fmt::Write`, or the
/// [`fmt::Formatter`] a `Display` implementation is handed.
///
/// # Errors
///
/// As [`string_value_pieces`], with nothing written; and [`StringValueError::Sink`] when `out`
/// refuses a write, after which nothing more is written to it.
pub fn decode_string_value<W>(spelling: &[u8], out: &mut W) -> Result<(), StringValueError>
where
  W: fmt::Write + ?Sized,
{
  // The checking walk: it meets every refusal of the spelling before a byte reaches `out`.
  string_value_len(spelling)?;
  walk(spelling, &mut |piece: &str| out.write_str(piece))
}

/// The walk behind every door here: the delimiters come off, the content is checked for UTF-8, and
/// the inline or the block reading hands the value to `piece`, which can refuse a piece.
///
/// A refused piece stops the walk with [`StringValueError::Sink`].
fn walk<F>(spelling: &[u8], piece: &mut F) -> Result<(), StringValueError>
where
  F: FnMut(&str) -> fmt::Result,
{
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
    block_value(content, piece).map_err(|fmt::Error| StringValueError::Sink)
  } else {
    inline_value(content, Invalid::Refuse, piece)
  }
}

/// What the inline walk makes of a backslash that begins no escape draft §2.9.1 defines.
#[derive(Clone, Copy)]
pub(crate) enum Invalid {
  /// The spelling is refused, with [`StringValueError::InvalidEscape`], once the run before the
  /// backslash is handed out: the decoder's answer, and through [`string_value_pieces`] the JSON
  /// writer's.
  Refuse,
  /// The backslash is content, and the walk reads on from the byte after it: the `Cow`
  /// conversion's answer. That conversion runs on a literal the lexer already accepted, so it is
  /// allowed neither a panic nor a refusal. The lexer hands it no such backslash; only a carrier
  /// built inside this crate, past the lexer, can hold one.
  CopyThrough,
}

/// An inline string's content, its escapes applied, handed to `piece` a run or a character at a
/// time.
///
/// `invalid` says what a backslash that begins no escape is. A piece `piece` refuses stops the walk
/// with [`StringValueError::Sink`], and nothing more is handed to it.
pub(crate) fn inline_value<F>(
  content: &str,
  invalid: Invalid,
  piece: &mut F,
) -> Result<(), StringValueError>
where
  F: FnMut(&str) -> fmt::Result,
{
  let bytes = content.as_bytes();
  // The run not yet handed out starts at `copied`, and the next backslash is looked for from
  // `from`. The two part only past a backslash copied through, which stays in the run.
  let mut copied = 0;
  let mut from = 0;
  // `str::find` with a `char` takes the memchr route. Both offsets are always character
  // boundaries: the start, or just past a backslash or an escape, which are ASCII to their last
  // byte.
  while let Some(offset) = content[from..].find('\\') {
    let slash = from + offset;
    let read = escape(&bytes[slash + 1..]);
    if read.is_none() && matches!(invalid, Invalid::CopyThrough) {
      from = slash + 1;
      continue;
    }
    hand_out(piece, &content[copied..slash])?;
    // `+ 1` for the opening quote, so the offset names the backslash in the spelling.
    let Escape { ch, len } = read.ok_or(StringValueError::InvalidEscape(slash + 1))?;
    hand_out(piece, ch.encode_utf8(&mut [0u8; 4]))?;
    copied = slash + 1 + len;
    from = copied;
  }
  hand_out(piece, &content[copied..])
}

/// Hands `text` to `piece` unless it is empty, which no piece is, and answers a refusal as
/// [`StringValueError::Sink`].
fn hand_out<F>(piece: &mut F, text: &str) -> Result<(), StringValueError>
where
  F: FnMut(&str) -> fmt::Result,
{
  if text.is_empty() {
    return Ok(());
  }
  piece(text).map_err(|fmt::Error| StringValueError::Sink)
}

/// A block string's content, as draft §2.9.4's `BlockStringValue()` reads it.
///
/// Two walks over the lines: the first finds the common indentation of every line after the first
/// that is not blank, and the first and last lines that are not blank; the second hands out the
/// lines between them, each but the raw first one dedented, with a line feed between each pair.
/// Every byte the algorithm removes is a space, a tab or a line terminator, so a cut never lands
/// inside a character.
///
/// A piece `piece` refuses stops the walk, and nothing more is handed to it.
pub(crate) fn block_value<F>(content: &str, piece: &mut F) -> fmt::Result
where
  F: FnMut(&str) -> fmt::Result,
{
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
    return Ok(());
  };
  let common = common.unwrap_or(0);
  for (index, line) in Lines::new(content).enumerate().take(last + 1).skip(first) {
    if index > first {
      piece("\n")?;
    }
    // Step 4 exempts the first line of the raw split, and that line only: not a blank line, and not
    // the first line steps 5 and 6 leave, which is a different line whenever the block opens with a
    // terminator. A blank line shorter than the common indentation loses all of it.
    let line = if index == 0 {
      line
    } else {
      &line[indent(line).min(common)..]
    };
    unescape_triple_quotes(line, piece)?;
  }
  Ok(())
}

/// The leading spaces and tabs of a line — draft §2.9.4's `WhiteSpace`, both one byte.
fn indent(line: &str) -> usize {
  line
    .bytes()
    .take_while(|&byte| byte == b' ' || byte == b'\t')
    .count()
}

/// One block line, with §2.9.5's only escape read: `\"""` is `"""`.
fn unescape_triple_quotes<F>(line: &str, piece: &mut F) -> fmt::Result
where
  F: FnMut(&str) -> fmt::Result,
{
  let mut rest = line;
  while let Some(at) = rest.find("\\\"\"\"") {
    if at > 0 {
      piece(&rest[..at])?;
    }
    piece("\"\"\"")?;
    rest = &rest[at + 4..];
  }
  if !rest.is_empty() {
    piece(rest)?;
  }
  Ok(())
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
