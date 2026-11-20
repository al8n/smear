//! GraphQLx's `char` literal, read over bytes: the one reader both of the dialect's lexers call.
//!
//! A literal is `'`, one unit of an inline string's body, and `'`. A unit is one character, or one
//! escape: GraphQL's, read by [`escape`], and `\'`. `"` may stand as it is. The literal is read to
//! its closing `'`, or to the line terminator or the end of input that cuts it first, so a refused
//! literal is one token over everything it holds.

use super::escape::{Escape, escape};

/// What a `char` literal at the start of a source's bytes is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CharLiteral {
  /// A literal that spells one Unicode scalar value, `len` bytes long with both quotes.
  Char {
    /// The character the literal spells, its escape applied.
    value: char,
    /// The literal's length in bytes.
    len: usize,
  },
  /// A refused literal, `len` bytes long: to its closing `'`, or to the line terminator or the
  /// end of input that cuts it, which the literal does not hold.
  Refused {
    /// Why the literal is refused.
    fault: CharFault,
    /// The refused literal's length in bytes.
    len: usize,
  },
}

/// Why a `char` literal is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CharFault {
  /// Nothing between the quotes.
  Empty,
  /// More than one unit between the quotes.
  Long,
  /// A line terminator or the end of input before the closing quote.
  Unterminated,
  /// A backslash that begins no escape. The backslash is always the literal's second byte: a
  /// first unit that is no escape is a character, and the fault of a later unit is
  /// [`Long`](Self::Long).
  Escape,
  /// A body that is not UTF-8, which only a byte source can hold.
  Utf8(core::str::Utf8Error),
}

/// Reads the `char` literal whose opening `'` comes just before `after`. Each length it answers
/// counts that opening `'`.
///
/// Its cost is the literal's own length, which ends at the first quote that no backslash takes,
/// or at the first line terminator.
pub(crate) fn read_char_literal(after: &[u8]) -> CharLiteral {
  // The closing quote is the first `'` no backslash takes. A backslash takes the `'` or the `\`
  // after it, and nothing else: no other escape holds either, and a line terminator after a
  // backslash still ends the line.
  let mut at = 0usize;
  let close = loop {
    match after.get(at) {
      None | Some(b'\n' | b'\r') => {
        return CharLiteral::Refused {
          fault: CharFault::Unterminated,
          len: at.saturating_add(1),
        };
      }
      Some(b'\'') => break at,
      Some(b'\\') if matches!(after.get(at.saturating_add(1)), Some(b'\'' | b'\\')) => {
        at = at.saturating_add(2);
      }
      Some(_) => at = at.saturating_add(1),
    }
  };
  let len = close.saturating_add(2);
  let body: &[u8] = after.get(..close).unwrap_or_default();
  let text = match core::str::from_utf8(body) {
    Ok(text) => text,
    Err(error) => {
      return CharLiteral::Refused {
        fault: CharFault::Utf8(error),
        len,
      };
    }
  };

  let (value, unit) = match text.chars().next() {
    None => {
      return CharLiteral::Refused {
        fault: CharFault::Empty,
        len,
      };
    }
    Some('\\') => {
      let escaped: &[u8] = body.get(1..).unwrap_or_default();
      match escaped.first() {
        Some(b'\'') => ('\'', 2),
        _ => match escape(escaped) {
          Some(Escape { ch, len: spans }) => (ch, spans.saturating_add(1)),
          None => {
            return CharLiteral::Refused {
              fault: CharFault::Escape,
              len,
            };
          }
        },
      }
    }
    Some(ch) => (ch, ch.len_utf8()),
  };
  if unit < body.len() {
    return CharLiteral::Refused {
      fault: CharFault::Long,
      len,
    };
  }
  CharLiteral::Char { value, len }
}
