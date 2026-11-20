//! GraphQLx's `char` literal: the one reader both of the dialect's lexers call.
//!
//! A literal is `'`, one unit of an inline string's body, and `'`. The body is read by the inline
//! string's own sub-lexer, a unit at a time: `StringToken` over a text source and
//! `BytesStringToken` over a byte source, as a string literal is. So it accepts and refuses exactly
//! what a string's body does, and a unit it refuses is reported with the string's own error, at the
//! offsets the same spelling has in a string, since both literals open with one quote.
//!
//! What is the literal's own stands around that reading. Its first `'` that no backslash takes
//! closes it, so `"` is a character; `\'` is one more escape; and it spells one Unicode scalar
//! value. It is refused when it is empty, when it holds more than one unit, or when a line
//! terminator or the end of input cuts it first. It is read to its closing `'` or to that cut, so a
//! refused literal is one token over everything it holds.

use tokora::{
  SimpleSpan,
  logos::{Lexer, Logos, Source},
  utils::Lexeme,
};

use crate::error::{LexemeErrors, StringError, StringErrors};

use super::{
  escape::{Escape, escape},
  inline::{BytesStringToken, StringToken},
};

/// A `char` literal, read from its opening `'`.
pub(crate) struct CharLiteral<Char> {
  /// The literal's length in bytes, its opening `'` counted: through its closing `'`, or up to the
  /// line terminator or the end of input that cuts it, which it does not hold.
  pub(crate) len: usize,
  /// The character the literal spells, its escape applied, or why it is refused.
  pub(crate) read: Result<char, CharRefusal<Char>>,
}

/// Why a `char` literal is refused: its body's faults, by an inline string's rules, and its own.
/// There is always one or the other.
pub(crate) struct CharRefusal<Char> {
  /// The body's faults, as a string reports the same units: in the order found, the first sixteen,
  /// and [`StringError::Omitted`] counting the rest. Empty when a string accepts every unit.
  pub(crate) string: StringErrors<Char>,
  /// The literal's own fault, when it has one. It always has one when `string` is empty.
  pub(crate) own: Option<CharFault>,
}

/// A `char` literal's own fault.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CharFault {
  /// Nothing between the quotes.
  Empty,
  /// More than one unit between the quotes.
  Long,
  /// A line terminator or the end of input before the closing quote.
  Unterminated,
}

/// The `char` literal's reader, by the character type a source's errors carry: `char` for a text
/// source and `u8` for a byte source. Each reads the body with its alphabet's inline string
/// sub-lexer, and the byte one holds the body to UTF-8 as it holds a string's.
pub(crate) trait CharBody: Sized {
  /// Reads the `char` literal whose opening `'` is at `start`, from `after`, the source's bytes
  /// after that quote. Its cost is the literal's own length.
  fn read_char_literal(after: &[u8], start: usize) -> CharLiteral<Self>;
}

impl CharBody for char {
  fn read_char_literal(after: &[u8], start: usize) -> CharLiteral<char> {
    let extent = Extent::of(after);
    match core::str::from_utf8(extent.body(after)) {
      Ok(body) => extent.read(StringToken::lexer(body), start),
      // Not reached: a text source's bytes are UTF-8, and the body ends before an ASCII byte or at
      // their end. It answers all the same, with the string's variant at the first sequence that
      // is not UTF-8.
      Err(error) => {
        let at = start + 1 + error.valid_up_to();
        let end = error
          .error_len()
          .map_or(start + 1 + extent.body, |len| at + len);
        extent.refuse(CharRefusal {
          string: StringError::UnsupportedCharacter(Lexeme::Range(SimpleSpan::new(at, end))).into(),
          own: (!extent.closed).then_some(CharFault::Unterminated),
        })
      }
    }
  }
}

impl CharBody for u8 {
  fn read_char_literal(after: &[u8], start: usize) -> CharLiteral<u8> {
    let extent = Extent::of(after);
    extent.read(BytesStringToken::lexer(extent.body(after)), start)
  }
}

/// Where a `char` literal's body ends, and what ends it.
#[derive(Clone, Copy)]
struct Extent {
  /// The body's length in bytes.
  body: usize,
  /// Whether the closing `'` ends the body, rather than a line terminator or the end of input.
  closed: bool,
}

impl Extent {
  /// The extent of the body that `after` begins with.
  ///
  /// The closing quote is the first `'` that is not the second byte of an escape. Only `\'` holds a
  /// `'`, and `\\` is the one escape whose second byte is a backslash, so a backslash takes the `'`
  /// or the `\` after it and nothing else. A line terminator after a backslash still ends the line.
  /// Each cut is before an ASCII byte or at the end, so none splits a character.
  fn of(after: &[u8]) -> Self {
    let mut at = 0;
    loop {
      match after.get(at) {
        None | Some(b'\n' | b'\r') => {
          return Self {
            body: at,
            closed: false,
          };
        }
        Some(b'\'') => {
          return Self {
            body: at,
            closed: true,
          };
        }
        Some(b'\\') if matches!(after.get(at + 1), Some(b'\'' | b'\\')) => at += 2,
        Some(_) => at += 1,
      }
    }
  }

  /// The body, out of the bytes after the opening quote.
  fn body(self, after: &[u8]) -> &[u8] {
    after.get(..self.body).unwrap_or_default()
  }

  /// The literal's length: its opening quote, its body, and its closing quote when it has one.
  fn len(self) -> usize {
    1 + self.body + usize::from(self.closed)
  }

  /// The literal, refused.
  fn refuse<Char>(self, refusal: CharRefusal<Char>) -> CharLiteral<Char> {
    CharLiteral {
      len: self.len(),
      read: Err(refusal),
    }
  }

  /// Reads the body with `lexer`, its alphabet's inline string sub-lexer over it, for the literal
  /// whose opening quote is at `start`.
  fn read<'s, T, Char>(self, mut lexer: Lexer<'s, T>, start: usize) -> CharLiteral<Char>
  where
    T: Logos<'s, Error = StringError<Char>>,
    <T::Source as Source>::Slice<'s>: AsRef<[u8]>,
  {
    // The sub-lexer counts from the body, which starts after the opening quote, as a string's
    // counts from after its own.
    let base = start + 1;
    let mut errors = LexemeErrors::new();
    let mut units = Units::default();
    loop {
      // `\'`, the literal's own escape, which the string's grammar does not know. Every other
      // backslash in the body begins one of the grammar's units, so it stands where a unit does.
      if lexer.remainder().as_ref().starts_with(b"\\'") {
        lexer.bump(2);
        units.take(Some('\''));
        continue;
      }
      match lexer.next() {
        None => break,
        Some(Ok(_)) => match lexer.slice().as_ref() {
          // An escape the grammar accepts, whose character `escape` reads, as a string's decoder
          // does. Were the two ever to disagree, the backslash would be the character, as the
          // decoder's `CopyThrough` reads one.
          [b'\\', escaped @ ..] => {
            units.take(Some(escape(escaped).map_or('\\', |Escape { ch, .. }| ch)));
          }
          // A run of characters, each one a unit, or a `"`, which closes only a string.
          characters => characters
            .utf8_chunks()
            .flat_map(|chunk| chunk.valid().chars())
            .for_each(|ch| units.take(Some(ch))),
        },
        Some(Err(mut error)) => {
          error.bump(base);
          errors.push(error);
          units.take(None);
        }
      }
    }

    let own = match (self.closed, units.count) {
      (false, _) => Some(CharFault::Unterminated),
      (true, 0) => Some(CharFault::Empty),
      (true, 1) => None,
      (true, _) => Some(CharFault::Long),
    };
    match (own, units.first) {
      // One unit, which the string accepts: its character is the literal's.
      (None, Some(value)) if errors.is_empty() => CharLiteral {
        len: self.len(),
        read: Ok(value),
      },
      (own, _) => self.refuse(CharRefusal {
        string: errors.into_string_errors(),
        own,
      }),
    }
  }
}

/// The units of a body read so far.
#[derive(Default)]
struct Units {
  /// How many there are.
  count: usize,
  /// The character the first one spells, when a string accepts it.
  first: Option<char>,
}

impl Units {
  /// Counts one more unit, with its character when a string accepts it.
  fn take(&mut self, ch: Option<char>) {
    if self.count == 0 {
      self.first = ch;
    }
    self.count += 1;
  }
}
