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
pub struct UnexpectedEndOfExponent<C> {
  found: Option<C>,
  hint: ExponentHint,
}

impl<C> UnexpectedEndOfExponent<C> {
  #[inline]
  const fn new(hint: ExponentHint) -> Self {
    Self::maybe_found(None, hint)
  }

  #[inline]
  const fn maybe_found(found: Option<C>, hint: ExponentHint) -> Self {
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
  pub const fn hint(&self) -> ExponentHint {
    self.hint
  }
}

impl<C> core::default::Default for UnexpectedEndOfExponent<C> {
  #[inline(always)]
  fn default() -> Self {
    Self {
      found: None,
      hint: ExponentHint::Identifier,
    }
  }
}

impl<C: core::fmt::Display> core::fmt::Display for UnexpectedEndOfExponent<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match &self.found {
      Some(ch) => write!(
        f,
        "unexpected character '{}' in exponent, expected {}",
        ch, self.hint
      ),
      None => write!(f, "unexpected end of input in exponent, expected {}", self.hint),
    }
  }
}

impl<C: core::fmt::Display + core::fmt::Debug> core::error::Error for UnexpectedEndOfExponent<C> {}

/// Exponent parsing error
#[derive(Debug, Clone, Copy, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum ExponentError<C> {
  /// The exponent has an unexpected suffix, e.g. `e+1x`, `ex`
  #[from(skip)]
  UnexpectedSuffix(C),
  /// The exponent expects one or more characters
  UnexpectedEof(UnexpectedEndOfExponent<C>),
}

impl<C: core::fmt::Display> core::fmt::Display for ExponentError<C> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::UnexpectedSuffix(ch) => write!(f, "unexpected character '{ch}' as exponent suffix"),
      Self::UnexpectedEof(eof) => eof.fmt(f),
    }
  }
}

impl<C> Default for ExponentError<C> {
  fn default() -> Self {
    Self::UnexpectedEof(Default::default())
  }
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(ExponentError<u8>, |lexer| UnexpectedEndOfExponent::maybe_found(lexer.slice().first().copied(), ExponentHint::Identifier).into()))]
#[logos(source = [u8])]
pub enum ExponentBytesToken {
  #[regex(r"[eE][+-]?[0-9]+")]
  #[regex(r"[eE][+-]?(\D)?", |lexer| unexpected_eoe(lexer.slice().iter().copied()))]
  #[regex(r"[eE][+-]?[0-9]+[^0-9 \t\r\n,\ufeff]", |lexer| unexpected_suffix(lexer.slice().iter().copied()))]
  Ok,
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(ExponentError<char>, |lexer| UnexpectedEndOfExponent::maybe_found(lexer.slice().chars().next(), ExponentHint::Identifier).into()))]
pub enum ExponentStrToken {
  #[regex(r"[eE][+-]?[0-9]+")]
  #[regex(r"[eE][+-]?(\D)?", |lexer| unexpected_eoe(lexer.slice().chars()))]
  #[regex(r"[eE][+-]?[0-9]+[^0-9 \t\r\n,\ufeff]", |lexer| unexpected_suffix(lexer.slice().chars()))]
  Ok,
}

#[inline]
fn unexpected_eoe<S, C>(remaining: S) -> Result<(), ExponentError<C>>
where
  S: IntoIterator<Item = C>,
  C: IsAsciiChar,
{
  let mut iter = remaining.into_iter();
  assert!(iter.next().is_some(), "there must be an exponent identifier");

  Err(match iter.next() {
    Some(ch) if ch.one_of(&[AsciiChar::Plus, AsciiChar::Minus]) => {
      UnexpectedEndOfExponent::maybe_found(iter.next(), ExponentHint::Digit)
    },
    ch => {
      UnexpectedEndOfExponent::maybe_found(ch, ExponentHint::SignOrDigit)
    }
  }.into())
}

#[inline]
fn unexpected_suffix<S, C>(remaining: S) -> Result<(), ExponentError<C>>
where
  S: IntoIterator<Item = C>,
  C: IsAsciiChar,
{
  let mut iter = remaining.into_iter();
  assert!(iter.next().is_some(), "there must be an exponent identifier");

  match iter.next() {
    // eat the exponent sign
    None => {},
    Some(ch) if ch.one_of(&[AsciiChar::Plus, AsciiChar::Minus]) => {},
    Some(ch) => {
      if !ch.is_ascii_digit() {
        return Err(ExponentError::UnexpectedSuffix(ch)); 
      }
    }
  }
  
  // consumes the valid digits
  for ch in iter {
    if !ch.is_ascii_digit() {
      return Err(ExponentError::UnexpectedSuffix(ch));
    }
  }

  unreachable!("there must be an invalid character")
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

pub trait ExponentInput<'c>: sealed::Sealed<'c> {}

mod sealed {
  use super::*;
  use logosky::source::CustomSource;

  pub trait Sealed<'c> {
    type Char: 'c;
    type Slice: 'c;
    type Token<'t>: Logos<'t> where Self: 't;

    /// Returns a lexer for the exponent suffix.
    fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>);

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
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
        None => Err(UnexpectedEndOfExponent::new(ExponentHint::Identifier).into()),
      };
      (lexer.span().end, res)
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
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
        None => Err(UnexpectedEndOfExponent::new(ExponentHint::Identifier).into()),
      };
      (lexer.span().end, res)
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
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Identifier);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<ExponentStrToken>::new("a");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Identifier);
    assert_eq!(err.found(), Some(&'a'));
  }

  #[test]
  fn test_expected_sign_or_digit() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::SignOrDigit);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<ExponentStrToken>::new("eX");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::SignOrDigit);
    assert_eq!(err.found(), Some(&'X'));
  }

  #[test]
  fn test_expected_digit() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e+");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<ExponentStrToken>::new("e-");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<ExponentStrToken>::new("e+X");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert_eq!(err.found(), Some(&'X'));

    let mut lexer = Lexer::<ExponentStrToken>::new("e-X");
    let err = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
    assert_eq!(err.hint(), ExponentHint::Digit);
    assert_eq!(err.found(), Some(&'X'));
  }

  #[test]
  fn test_unexpected_suffix() {
    let mut lexer = Lexer::<ExponentStrToken>::new("e+123x");
    let ch = lex_exponent::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
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
      assert!(lex_exponent::<ExponentStrToken>(&mut lexer).is_ok());
    }
  }
}