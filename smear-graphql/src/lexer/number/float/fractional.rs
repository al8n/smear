use logos::{Lexer, Logos, Source};
use derive_more::Display;
use logosky::IsAsciiChar;

use crate::lexer::token::error::FloatError;

/// The hint about what is expected for the next character
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum FractionalHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
  /// Expect the next character to be a fractional identifier '.'.
  #[display("'.'")]
  Dot,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FractionalError<C> {
  found: Option<C>,
  hint: FractionalHint,
}

impl<C> FractionalError<C> {
  #[inline]
  const fn new(hint: FractionalHint) -> Self {
    Self::maybe_found(None, hint)
  }

  #[inline]
  const fn with_found(found: C, hint: FractionalHint) -> Self {
    Self::maybe_found(Some(found), hint)
  }

  #[inline]
  const fn maybe_found(found: Option<C>, hint: FractionalHint) -> Self {
    Self {
      found,
      hint,
    }
  }

  #[inline]
  pub const fn found(&self) -> Option<&C> {
    self.found.as_ref()
  }

  #[inline]
  pub const fn hint(&self) -> FractionalHint {
    self.hint
  }

  #[inline]
  pub fn into_components(self) -> (Option<C>, FractionalHint) {
    (self.found, self.hint)
  }
}

impl<C> core::default::Default for FractionalError<C> {
  #[inline(always)]
  fn default() -> Self {
    Self {
      found: None,
      hint: FractionalHint::Dot,
    }
  }
}

impl<C: core::fmt::Display> core::fmt::Display for FractionalError<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match &self.found {
      Some(ch) => write!(
        f,
        "unexpected character '{}' in fractional, expected {}",
        ch, self.hint
      ),
      None => write!(f, "unexpected end of input in fractional, expected {}", self.hint),
    }
  }
}

impl<C: core::fmt::Display + core::fmt::Debug> core::error::Error for FractionalError<C> {}


#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(FractionalError<u8>, |lexer| FractionalError::maybe_found(lexer.slice().first().copied(), FractionalHint::Dot)))]
#[logos(source = [u8])]
pub enum FractionalBytesToken {
  #[regex(r"[0-9]+")] 
  #[regex(r"(\D)?", |lexer| unexpected_end_of_fractional(lexer.slice().iter().copied()))]
  Ok,
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(FractionalError<char>, |lexer| FractionalError::maybe_found(lexer.slice().chars().next(), FractionalHint::Dot)))]
pub enum FractionalStrToken {
  #[regex(r"[0-9]+")]
  #[regex(r"(\D)?", |lexer| unexpected_end_of_fractional(lexer.slice().chars()))]
  Ok,
}

fn unexpected_end_of_fractional<S, C>(remaining: S) -> Result<(), FractionalError<C>>
where
  S: IntoIterator<Item = C>,
  C: IsAsciiChar,
{
  let mut iter = remaining.into_iter();
  Err(FractionalError::maybe_found(iter.next(), FractionalHint::Digit))
}

pub fn lex_fractional<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), FloatError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
where
  T: Logos<'a>,
  <T::Source as Source>::Slice<'a>: FractionalInput<'a>,
{
  use sealed::Sealed;

  let (end, res) = lexer.remainder().lex();
  lexer.bump(end);
  res
}

pub fn lex_fractional_with_dot<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), FloatError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
where
  T: Logos<'a>,
  <T::Source as Source>::Slice<'a>: FractionalInput<'a>,
{
  use sealed::Sealed;

  let (end, res) = lexer.remainder().lex_with_dot();
  lexer.bump(end);
  res
}

pub trait FractionalInput<'c>: sealed::Sealed<'c> {}

mod sealed {
  use crate::lexer::number::{lex_exponent, lex_exponent_with_identifier};

  use super::*;
  use logosky::source::CustomSource;

  pub trait Sealed<'c> {
    type Char: 'c;
    type Slice: 'c;
    type Token<'t>: Logos<'t> where Self: 't;

    /// Returns a lexer for the exponent suffix.
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>);

    /// Returns a lexer for the exponent suffix.
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>);

    /// Returns an iterator over the characters in the exponent suffix.
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c;
  }

  impl<'a, T: Sealed<'a> + ?Sized> FractionalInput<'a> for T {}

  impl<'c> Sealed<'c> for [u8] {
    type Char = u8;
    type Slice = &'c [u8];
    type Token<'t> = FractionalBytesToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      const BOM: &[u8] = b"\xef\xbb\xbf";

      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        None => Err(FractionalError::new(FractionalHint::Digit).into()),
        Some(tok) => match tok {
          Err(e) => Err(e.into()),
          Ok(_) => {
            let remainder = lexer.remainder();
            let mut iter = remainder.iter();

            match iter.next() {
              None => Ok(()),
              Some(&ch) => {
                match ch {
                  b' ' | b'\t' | b'\r' | b'\n' => Ok(()),
                  b'e' | b'E' => {
                    match lex_exponent_with_identifier(&mut lexer) {
                      Ok(_) => Ok(()),
                      Err(e) => Err(e.into()),
                    }
                  },
                  _ if remainder.starts_with(BOM) => Ok(()),
                  _ => Err(FloatError::UnexpectedSuffix(ch)),
                }
              },
            }
          },
        },
      };
      (lexer.span().end, res)
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      if self.is_empty() {
        return (0, Err(FractionalError::new(FractionalHint::Dot).into()));
      }

      let first = self[0];
      if first != b'.' {
        return (0, Err(FractionalError::with_found(first, FractionalHint::Dot).into()));
      }

      let (end, res) = self[1..].lex();
      (end + 1, res)
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c {
      slice.iter().copied()
    }
  }

  impl<'c> Sealed<'c> for str {
    type Char = char;
    type Slice = &'c str;
    type Token<'t> = FractionalStrToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        None => Err(FractionalError::new(FractionalHint::Digit).into()),
        Some(tok) => match tok {
          Err(e) => Err(e.into()),
          Ok(_) => {
            let remainder = lexer.remainder();
            let mut iter = remainder.chars();

            match iter.next() {
              None => Ok(()),
              Some(ch) => {
                match ch {
                  ' ' | '\t' | '\r' | '\n' | '\u{feff}' => Ok(()),
                  'e' | 'E' => {
                    match lex_exponent_with_identifier(&mut lexer) {
                      Ok(_) => Ok(()),
                      Err(e) => Err(e.into()),
                    }
                  },
                  _ => Err(FloatError::UnexpectedSuffix(ch)),
                }
              },
            }
          },
        },
      };
      (lexer.span().end, res)
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      match self.chars().next() {
        None => (0, Err(FractionalError::new(FractionalHint::Dot).into())),
        Some('.') => {
          let (end, res) = self[1..].lex();
          (end + 1, res)
        },
        Some(ch) => (1, Err(FractionalError::with_found(ch, FractionalHint::Dot).into())),
      }
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c {
      slice.chars()
    }
  }

  impl<'l, T: Sealed<'l> + ?Sized> Sealed<'l> for &'l T {
    type Char = T::Char;
    type Slice = T::Slice;
    type Token<'t> = T::Token<'t> where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex_with_dot()
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
      T::iter(slice)
    }
  }

  impl<'l, T: Sealed<'l> + ?Sized> Sealed<'l> for CustomSource<T> {
    type Char = T::Char;
    type Slice = T::Slice;
    type Token<'t> = T::Token<'t> where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      self.as_ref().lex()
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      self.as_ref().lex_with_dot()
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
      T::iter(slice)
    }
  }

  #[cfg(feature = "bstr")]
  impl<'l> Sealed<'l> for bstr::BStr {
    type Char = u8;
    type Slice = &'l [u8];
    type Token<'t> = FractionalBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex_with_dot()
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
      slice.iter().copied()
    }
  }

  #[cfg(feature = "bytes")]
  impl<'l> Sealed<'l> for bytes::Bytes {
    type Char = u8;
    type Slice = &'l [u8];
    type Token<'t> = FractionalBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_dot(&self) -> (usize, Result<(), FloatError<Self::Char>>) {
      (**self).lex_with_dot()
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
      slice.iter().copied()
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::lexer::{number::ExponentHint, token::error::FloatHint};

  use super::*;

  #[test]
  fn test_expected_dot() {
    let mut lexer = Lexer::<FractionalStrToken>::new("");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Dot));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new("a");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Dot));
    assert_eq!(err.found(), Some(&'a'));
  }

  #[test]
  fn test_bytes_expected_dot() {
    let mut lexer = Lexer::<FractionalBytesToken>::new(b"");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Dot));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalBytesToken>::new(b"a");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Dot));
    assert_eq!(err.found(), Some(&b'a'));
  }

  #[test]
  fn test_unexpected_eof() {
    let mut lexer = Lexer::<FractionalStrToken>::new(".");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new(".a");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), Some(&'a'));

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e+");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::Digit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e+a");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::Digit));
    assert_eq!(err.found(), Some(&'a'));
  }

  #[test]
  fn test_bytes_unexpected_eof() {
    let mut lexer = Lexer::<FractionalBytesToken>::new(b".");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".a");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Fractional(FractionalHint::Digit));
    assert_eq!(err.found(), Some(&b'a'));

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::SignOrDigit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e+");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::Digit));
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e+a");
    let err = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), FloatHint::Exponent(ExponentHint::Digit));
    assert_eq!(err.found(), Some(&b'a'));
  }

  #[test]
  fn test_unexpected_suffix() {
    let mut lexer = Lexer::<FractionalStrToken>::new(".1a");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, 'a');

    let mut lexer = Lexer::<FractionalStrToken>::new(".1.");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, '.');

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e1a");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, 'a');

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e1.");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, '.');

    let mut lexer = Lexer::<FractionalStrToken>::new(".1e+1a");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, 'a');
  
    let mut lexer = Lexer::<FractionalStrToken>::new(".1e+1.");
    let ch = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, '.');
  }

  #[test]
  fn test_bytes_unexpected_suffix() {
    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1a");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'a');

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1.");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'.');

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e1a");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'a');

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e1.");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'.');

    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e+1a");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'a');
  
    let mut lexer = Lexer::<FractionalBytesToken>::new(b".1e+1.");
    let ch = lex_fractional::<FractionalBytesToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, b'.');
  }

  #[test]
  fn test_lex_str_ok() {
    const INPUT: &[&str] = &[
      ".123",
      ".1",
      ".0000000",
      ".0000000001",
      ".123 x",
      ".123\tx",
      ".123\rx",
      ".123\nx",
      ".123\u{feff}x",
      ".123e10",
      ".123E10",
      ".123e+10",
      ".123e-10",
      ".123E+10",
      ".123E-10",
    ];

    for &input in INPUT {
      let mut lexer = Lexer::<FractionalStrToken>::new(input);
      assert!(lex_fractional::<FractionalStrToken>(&mut lexer).is_ok(), "fail on input: {input}");
    }
  }

  #[test]
  fn test_lex_bytes_ok() {
    const INPUT: &[&[u8]] = &[
      b".123",
      b".1",
      b".0000000",
      b".0000000001",
      b".123 x",
      b".123\tx",
      b".123\rx",
      b".123\nx",
      &[b'.', b'1', b'2', b'3', 239, 187, 191, b'x'], // ".123\u{feff}x"
      b".123e10",
      b".123E10",
      b".123e+10",
      b".123e-10",
      b".123E+10",
      b".123E-10",
    ];

    for &input in INPUT {
      let mut lexer = Lexer::<FractionalBytesToken>::new(input);
      assert!(lex_fractional::<FractionalBytesToken>(&mut lexer).is_ok(), "fail on input: {input:?}");
    }
  }
}