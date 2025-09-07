use core::ops::Range;

use logos::{Lexer, Logos};

pub use float::*;


mod float;

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Digits {
  /// Ok indicates that a valid sequence of digits was found.
  #[regex(r"[0-9]+", handle_digits)]
  #[regex(r"[a-zA-Z_]", handle_invalid)]
  Ok,
  /// Whitespace
  #[regex(r"[ \t\r\n,\ufeff]")]
  WhiteSpace,
  /// Invalid stores the span of the invalid sequence, e.g. "12abc34" -> the span will be '2..5'
  Invalid(Range<usize>),
}

#[inline]
fn handle_invalid<'a>(lexer: &mut Lexer<'a, Digits>) -> Digits {
  let remainder = lexer.remainder();

  // store the current lexer's span
  let span = lexer.span();

  for (idx, ch) in remainder.char_indices() {
    if matches!(ch, 'a'..='z' | 'A'..='Z' | '_' ) {
      continue;
    }

    // bump the lexer to the end of the invalid sequence
    lexer.bump(idx);
    // return the range of the invalid sequence
    return Digits::Invalid(span.start..(span.end + idx));
  }

  let len = remainder.len();
  // bump the lexer to the end of the invalid sequence
  lexer.bump(len);
  // return the range of the invalid sequence
  Digits::Invalid(span.start..(span.end + len))
}

#[inline]
fn handle_digits<'a>(lexer: &mut Lexer<'a, Digits>) -> Digits {
  let remainder = lexer.remainder();

  let mut iter = remainder.char_indices();
  match iter.next() {
    None => Digits::Ok,
    // if next char is a letter or underscore, we need try to find the longest invalid sequence
    Some((_, 'a'..='z' | 'A'..='Z' | '_' )) => {
      for (idx, ch) in iter {
        if matches!(ch, 'a'..='z' | 'A'..='Z' | '_' ) {
          continue;
        }

        // store the current span
        let span = lexer.span();
        // bump the lexer to the end of the invalid sequence
        lexer.bump(idx);
        // return the range of the invalid sequence
        return Digits::Invalid(span.end..(span.end + idx));
      }

      let len = remainder.len();
      // store the current span
      let span = lexer.span();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      // return the range of the invalid sequence
      Digits::Invalid(span.end..(span.end + len))
    },
    Some(_) => Digits::Ok,
  }
}

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(source = [u8])]
pub enum DigitsBytes {
  /// Ok indicates that a valid sequence of digits was found.
  #[regex(r"[0-9]+", handle_digits_bytes)]
  #[regex(r"[a-zA-Z_]", handle_invalid_digits_bytes)]
  Ok,
  /// Whitespace
  #[regex(r"[ \t\r\n,\ufeff]")]
  WhiteSpace,
  /// Invalid stores the span of the invalid sequence, e.g. "12abc34" -> the span will be '2..5'
  Invalid(Range<usize>),
}

#[inline]
fn handle_invalid_digits_bytes<'a>(lexer: &mut Lexer<'a, DigitsBytes>) -> DigitsBytes {
  let remainder = lexer.remainder();

  // store the current lexer's span
  let span = lexer.span();

  for (idx, ch) in remainder.iter().copied().enumerate() {
    if matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' ) {
      continue;
    }

    // bump the lexer to the end of the invalid sequence
    lexer.bump(idx);
    // return the range of the invalid sequence
    return DigitsBytes::Invalid(span.start..(span.end + idx));
  }

  let len = remainder.len();
  // bump the lexer to the end of the invalid sequence
  lexer.bump(len);
  // return the range of the invalid sequence
  DigitsBytes::Invalid(span.start..(span.end + len))
}

#[inline]
fn handle_digits_bytes<'a>(lexer: &mut Lexer<'a, DigitsBytes>) -> DigitsBytes {
  let remainder = lexer.remainder();

  let mut iter = remainder.iter().copied().enumerate();
  match iter.next() {
    None => DigitsBytes::Ok,
    // if next char is a letter or underscore, we need try to find the longest invalid sequence
    Some((_, b'a'..=b'z' | b'A'..=b'Z' | b'_' )) => {
      for (idx, ch) in iter {
        if matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' ) {
          continue;
        }

        // store the current span
        let span = lexer.span();
        // bump the lexer to the end of the invalid sequence
        lexer.bump(idx);
        // return the range of the invalid sequence
        return DigitsBytes::Invalid(span.end..(span.end + idx));
      }

      let len = remainder.len();
      // store the current span
      let span = lexer.span();
      // bump the lexer to the end of the invalid sequence
      lexer.bump(len);
      // return the range of the invalid sequence
      DigitsBytes::Invalid(span.end..(span.end + len))
    },
    Some(_) => DigitsBytes::Ok,
  }
}