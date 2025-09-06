use logos::{Lexer, Logos, Source};
use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use logosky::IsAsciiChar;
use smear_parser::source::AsciiChar;

/// The hint about what is expected for the next character
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
pub enum ExponentHint {
  /// Expect the next character to be digit.
  #[display("digit")]
  Digit,
  /// Expect the next character to be a sign or a digit.
  #[display("'+', '-' or digit")]
  SignOrDigit,
  /// Expect the next character to be an exponent identifier 'e' or 'E'.
  #[display("'e' or 'E'")]
  Identifier,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnexpectedExponentCharacter<C> {
  found: C,
  hint: ExponentHint,
}

impl<C> UnexpectedExponentCharacter<C> {
  #[inline]
  const fn new(found: C, hint: ExponentHint) -> Self {
    Self {
      found,
      hint,
    }
  }

  #[inline]
  pub const fn found(&self) -> &C {
    &self.found
  }

  #[inline]
  pub const fn hint(&self) -> ExponentHint {
    self.hint
  }

  #[inline]
  pub fn into_components(self) -> (C, ExponentHint) {
    (self.found, self.hint)
  }
}

impl<C: core::fmt::Display> core::fmt::Display for UnexpectedExponentCharacter<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(
      f,
      "unexpected character '{}' in exponent, expected {}",
      self.found, self.hint
    )
  }
}

impl<C: core::fmt::Display + core::fmt::Debug> core::error::Error for UnexpectedExponentCharacter<C> {}

/// Exponent parsing error
#[derive(Debug, Clone, Copy, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum ExponentError<C> {
  /// The exponent has an unexpected suffix, e.g. `e+1x`, `ex`
  #[from(skip)]
  UnexpectedSuffix(C),
  /// The exponent found an unexpected character.
  UnexpectedCharacter(UnexpectedExponentCharacter<C>),
  /// The exponent expects one or more characters
  UnexpectedEof(ExponentHint),
}

impl<C: core::fmt::Display> core::fmt::Display for ExponentError<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(ch) => write!(f, "unexpected character '{ch}' as exponent suffix"),
      Self::UnexpectedCharacter(e) => e.fmt(f),
      Self::UnexpectedEof(hint) => write!(f, "unexpected end of input in exponent, expected {hint}"),
    }
  }
}

impl<C> Default for ExponentError<C> {
  fn default() -> Self {
    Self::UnexpectedEof(ExponentHint::SignOrDigit)
  }
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(ExponentError<u8>, |lexer| default_error(lexer.slice().first().copied())))]
#[logos(source = [u8])]
pub enum ExponentBytesToken {
  #[regex(r"[+-]?[0-9]+")]
  #[regex(r"[+-]?(\D)?", |lexer| unexpected_eoe(lexer.slice().iter().copied()))]
  #[regex(r"[+-]?[0-9]+[^\d \t\r\n,\ufeff]", |lexer| unexpected_suffix(lexer.slice().last().copied().expect("there is at least one char")))]
  Ok,
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(ExponentError<char>, |lexer| default_error(lexer.slice().chars().next())))]
pub enum ExponentStrToken {
  #[regex(r"[+-]?[0-9]+")]
  #[regex(r"[+-]?(\D)?", |lexer| unexpected_eoe(lexer.slice().chars()))]
  #[regex(r"[+-]?[0-9]+[^\d \t\r\n,\ufeff]", |lexer| unexpected_suffix(lexer.slice().chars().last().expect("there is at least one char")))]
  Ok,
}

#[inline]
fn unexpected_eoe<S, C>(remaining: S) -> Result<(), ExponentError<C>>
where
  S: IntoIterator<Item = C>,
  C: IsAsciiChar,
{
  let mut iter = remaining.into_iter();

  Err(match iter.next() {
    Some(ch) if ch.one_of(&[AsciiChar::Plus, AsciiChar::Minus]) => {
      match iter.next() {
        Some(ch) => UnexpectedExponentCharacter::new(ch, ExponentHint::Digit).into(),
        None => ExponentHint::Digit.into(),
      }
    },
    Some(ch) => {
      UnexpectedExponentCharacter::new(ch, ExponentHint::SignOrDigit).into()
    },
    None => ExponentHint::SignOrDigit.into(),
  })
}

#[inline(always)]
fn unexpected_suffix<C>(ch: C) -> Result<(), ExponentError<C>> {
  Err(ExponentError::UnexpectedSuffix(ch))
}

#[inline(always)]
fn default_error<C>(ch: Option<C>) -> ExponentError<C> {
  match ch {
    None => ExponentError::UnexpectedEof(ExponentHint::SignOrDigit),
    Some(ch) => ExponentError::UnexpectedCharacter(UnexpectedExponentCharacter::new(ch, ExponentHint::Digit)),
  }
}

pub fn lex_exponent<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), ExponentError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
where
  T: Logos<'a>,
  <T::Source as Source>::Slice<'a>: ExponentInput<'a>,
{
  use sealed::Sealed;

  let (end, res) = lexer.remainder().lex();
  lexer.bump(end);
  res
}

pub fn lex_exponent_with_identifier<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), ExponentError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
where
  T: Logos<'a>,
  <T::Source as Source>::Slice<'a>: ExponentInput<'a>,
{
  use sealed::Sealed;

  let (end, res) = lexer.remainder().lex_with_identifier();
  lexer.bump(end);
  res
}

pub trait ExponentInput<'c>: sealed::Sealed<'c> {
}

mod sealed {
  use super::*;
  use logosky::source::CustomSource;

  pub trait Sealed<'c> {
    type Char: 'c;
    type Slice: 'c;
    type Token<'t>: Logos<'t> where Self: 't;

    /// Returns a lexer for the exponent suffix.
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>);

    /// Returns a lexer for the exponent suffix.
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>);

    /// Returns an iterator over the characters in the exponent suffix.
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c;
  }

  impl<'a, T: Sealed<'a> + ?Sized> ExponentInput<'a> for T {}

  impl<'c> Sealed<'c> for [u8] {
    type Char = u8;
    type Slice = &'c [u8];
    type Token<'t> = ExponentBytesToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        None => Err(ExponentHint::SignOrDigit.into()),
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
      };
      (lexer.span().end, res)
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      if self.is_empty() {
        return (0, Err(ExponentHint::Identifier.into()));
      }

      let first = self[0];
      match first {
        b'e' | b'E' => {},
        ch => return (1, Err(UnexpectedExponentCharacter::new(ch, ExponentHint::Identifier).into())),
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
    type Token<'t> = ExponentStrToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        None => Err(ExponentHint::SignOrDigit.into()),
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
      };
      (lexer.span().end, res)
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      match self.chars().next() {
        None => {
          (0, Err(ExponentHint::Identifier.into()))
        },
        Some('e') | Some('E') => {
          let (end, res) = self[1..].lex();
          (end + 1, res)
        },
        Some(ch) => {
          (1, Err(UnexpectedExponentCharacter::new(ch, ExponentHint::Identifier).into()))
        },
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
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex_with_identifier()
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
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      self.as_ref().lex()
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      self.as_ref().lex_with_identifier()
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
    type Token<'t> = ExponentBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex_with_identifier()
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
    type Token<'t> = ExponentBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex()
    }

    #[inline]
    fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
      (**self).lex_with_identifier()
    }

    #[inline(always)]
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
      slice.iter().copied()
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_expected_identifier() {
    let mut lexer = Lexer::<ExponentStrToken>::new("");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err, ExponentHint::Identifier);

    let mut lexer = Lexer::<ExponentStrToken>::new("a");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.hint(), ExponentHint::Identifier);
    assert_eq!(err.found(), &'a');
  }

  #[test]
  fn test_expected_sign_or_digit() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err, ExponentHint::SignOrDigit);

    let mut lexer = Lexer::<ExponentStrToken>::new("eX");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.hint(), ExponentHint::SignOrDigit);
    assert_eq!(err.found(), &'X');
  }

  #[test]
  fn test_expected_digit() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e+");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err, ExponentHint::Digit);

    let mut lexer = Lexer::<ExponentStrToken>::new("e-");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err, ExponentHint::Digit);

    let mut lexer = Lexer::<ExponentStrToken>::new("e+X");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert_eq!(err.found(), &'X');

    let mut lexer = Lexer::<ExponentStrToken>::new("e-X");
    let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert_eq!(err.found(), &'X');
  }

  #[test]
  fn test_unexpected_suffix() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e+123x");
    let ch = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
    assert_eq!(ch, 'x');
  }

  #[test]
  fn test_ok() {
    const INPUT: &[&str] = &[
      "e0",
      "e1",
      "e123",
      "e+0",
      "e+1",
      "e+123",
      "e-0",
      "e-1",
      "e-123",
      "e+123 x", // this works because the lexer should yield the first token as "e+123"
      "e+123\tx", // this works because the lexer should yield the first token as "e+123"
      "e+123\rx", // this works because the lexer should yield the first token as "e+123"
      "e+123\nx", // this works because the lexer should yield the first token as "e+123"
      "e+123\u{feff}x", // this works because the lexer should yield the first token as "e+123"
      "e+123,x", // this works because the lexer should yield the first token as "e+123"
    ];

    for &input in INPUT {
      let mut lexer = Lexer::<ExponentStrToken>::new(input);
      assert!(lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).is_ok());
    }
  }
}