//! The body of GraphQLx's `b"…"`, read over bytes: the one reader both of the dialect's lexers
//! call, and `LitPrefixedStr::literal` calls again.
//!
//! The body is an inline string's, read by the inline string's own sub-lexer a unit at a time, as
//! a `char` literal's is, so it accepts and refuses what a string's body does. It has one more
//! escape: `\x` and two hexadecimal digits, any one byte. A `\x` that two do not follow is refused
//! with [`StringError::InvalidByteEscape`], over the `\x` and the digits that do. The literal is
//! read to its closing `"`, or to the line terminator or the end of input that cuts it, and is
//! then refused as an unterminated string is.

use tokora::{
  SimpleSpan,
  logos::{Lexer, Logos, Source},
  utils::Lexeme,
};

use crate::error::{LexemeErrors, StringError, StringErrors};

use super::{
  char_literal::Extent,
  inline::{BytesStringToken, StringToken},
};

/// A `b"…"` literal, read from its opening `"`.
pub(crate) struct ByteStr<Char> {
  /// The literal's length in bytes, its opening `"` counted: through its closing `"`, or up to the
  /// line terminator or the end of input that cuts it, which it does not hold.
  pub(crate) len: usize,
  /// What its value needs, or the errors it is refused with, as a string reports them: `None` for
  /// a body that holds no escape, whose value is its spelling, and otherwise how many bytes the
  /// value takes, one for each `\x` escape.
  pub(crate) read: Result<Option<usize>, StringErrors<Char>>,
}

/// The `b"…"` body's reader, by the character type a source's errors carry: `char` for a text
/// source and `u8` for a byte source, each with its alphabet's inline string sub-lexer.
pub(crate) trait ByteStrBody: Sized {
  /// Reads the `b"…"` literal whose opening `"` is at `start`, from `after`, the source's bytes
  /// after that quote. Its cost is the literal's own length.
  fn read_byte_str(after: &[u8], start: usize) -> ByteStr<Self>;
}

impl ByteStrBody for char {
  fn read_byte_str(after: &[u8], start: usize) -> ByteStr<char> {
    let extent = Extent::of(after, b'"');
    match core::str::from_utf8(extent.body(after)) {
      Ok(body) => read(StringToken::lexer(body), start, extent),
      // Not reached: a text source's bytes are UTF-8, and the body ends before an ASCII byte or at
      // their end. It answers all the same, with the string's variant at the first sequence that
      // is not UTF-8.
      Err(error) => {
        let at = start + 1 + error.valid_up_to();
        let end = error
          .error_len()
          .map_or(start + 1 + extent.body, |len| at + len);
        let mut errors = LexemeErrors::new();
        errors.push(StringError::UnsupportedCharacter(Lexeme::Range(
          SimpleSpan::new(at, end),
        )));
        ByteStr {
          len: extent.len(),
          read: Err(refusal(errors, extent)),
        }
      }
    }
  }
}

impl ByteStrBody for u8 {
  fn read_byte_str(after: &[u8], start: usize) -> ByteStr<u8> {
    let extent = Extent::of(after, b'"');
    read(BytesStringToken::lexer(extent.body(after)), start, extent)
  }
}

/// A refused literal's record: the errors found, and after them, for a literal its line or the
/// input cuts, the unterminated string's.
fn refusal<Char>(errors: LexemeErrors<StringError<Char>>, extent: Extent) -> StringErrors<Char> {
  if extent.closed {
    errors.into_string_errors()
  } else {
    errors.into_unterminated(StringError::unterminated_inline_string())
  }
}

/// Reads the body with `lexer`, its alphabet's inline string sub-lexer over it, for the literal
/// whose opening quote is at `start`. The sub-lexer's `extras` counts the bytes the value takes,
/// as a string's does.
fn read<'s, T, Char>(mut lexer: Lexer<'s, T>, start: usize, extent: Extent) -> ByteStr<Char>
where
  T: Logos<'s, Extras = usize, Error = StringError<Char>>,
  <T::Source as Source>::Slice<'s>: AsRef<[u8]>,
{
  // The sub-lexer counts from the body, which starts after the opening quote, as a string's
  // counts from after its own.
  let base = start + 1;
  let mut errors = LexemeErrors::new();
  let mut escaped = false;
  loop {
    // `\x`, the escape the string's grammar does not know. Every other backslash in the body
    // begins one of the grammar's units, so it stands where a unit does.
    if let [b'\\', b'x', digits @ ..] = lexer.remainder().as_ref() {
      let hex = digits
        .iter()
        .take(2)
        .take_while(|byte: &&u8| byte.is_ascii_hexdigit())
        .count();
      let at = base + lexer.span().end;
      lexer.bump(2 + hex);
      if hex == 2 {
        lexer.extras += 1;
        escaped = true;
      } else {
        errors.push(StringError::InvalidByteEscape(SimpleSpan::new(
          at,
          at + 2 + hex,
        )));
      }
      continue;
    }
    match lexer.next() {
      None => break,
      Some(Ok(_)) => match lexer.slice().as_ref() {
        // A Unicode escape's callback counted the bytes of the character it names.
        [b'\\', b'u', ..] => escaped = true,
        // Each of the other escapes names a character of one byte.
        [b'\\', ..] => {
          lexer.extras += 1;
          escaped = true;
        }
        // Characters normalize byte for byte.
        characters => lexer.extras += characters.len(),
      },
      Some(Err(mut error)) => {
        error.bump(base);
        errors.push(error);
      }
    }
  }

  let read = if extent.closed && errors.is_empty() {
    Ok(escaped.then_some(lexer.extras))
  } else {
    Err(refusal(errors, extent))
  };
  ByteStr {
    len: extent.len(),
    read,
  }
}
