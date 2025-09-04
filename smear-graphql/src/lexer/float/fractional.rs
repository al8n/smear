use logos::{Lexer, Logos, Source};
use derive_more::Display;
use logosky::IsAsciiChar;

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
  #[regex(r"\.[0-9]+")] 
  #[regex(r"\.(\D)?", |lexer| unexpected_end_of_fractional(lexer.slice().iter().copied()))]
  Ok,
}

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[logos(error(FractionalError<char>, |lexer| FractionalError::maybe_found(lexer.slice().chars().next(), FractionalHint::Dot)))]
pub enum FractionalStrToken {
  #[regex(r"\.[0-9]+")]
  #[regex(r"\.(\D)?", |lexer| unexpected_end_of_fractional(lexer.slice().chars()))]
  Ok,
}

fn unexpected_end_of_fractional<S, C>(remaining: S) -> Result<(), FractionalError<C>>
where
  S: IntoIterator<Item = C>,
  C: IsAsciiChar,
{
  let mut iter = remaining.into_iter();
  let first = iter.next();
  assert!(first.is_some(), "there must be a '.'");

  Err(FractionalError::maybe_found(iter.next(), FractionalHint::Digit))
}

pub fn lex_fractional<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), FractionalError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
where
  T: Logos<'a>,
  <T::Source as Source>::Slice<'a>: FractionalInput<'a>,
{
  use sealed::Sealed;

  let (end, res) = lexer.remainder().lex();
  lexer.bump(end);
  res
}

pub trait FractionalInput<'c>: sealed::Sealed<'c> {
}

mod sealed {
  use super::*;
  use logosky::source::CustomSource;

  pub trait Sealed<'c> {
    type Char: 'c;
    type Slice: 'c;
    type Token<'t>: Logos<'t> where Self: 't;

    /// Returns a lexer for the exponent suffix.
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>);

    /// Returns an iterator over the characters in the exponent suffix.
    fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c;
  }

  impl<'a, T: Sealed<'a> + ?Sized> FractionalInput<'a> for T {}

  impl<'c> Sealed<'c> for [u8] {
    type Char = u8;
    type Slice = &'c [u8];
    type Token<'t> = FractionalBytesToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
        None => Err(FractionalError::new(FractionalHint::Dot)),
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
    type Token<'t> = FractionalStrToken;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
      let mut lexer = Lexer::<Self::Token<'_>>::new(self);
      let res = match lexer.next() {
        Some(tok) => match tok {
          Ok(_) => Ok(()),
          Err(e) => Err(e),
        },
        None => Err(FractionalError::new(FractionalHint::Dot)),
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
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
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
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
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
    type Token<'t> = FractionalBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
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
    type Token<'t> = FractionalBytesToken where Self: 't;

    #[inline]
    fn lex(&self) -> (usize, Result<(), FractionalError<Self::Char>>) {
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
  fn test_expected_dot() {
    let mut lexer = Lexer::<FractionalStrToken>::new("");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err();
    assert_eq!(err.hint(), FractionalHint::Dot);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new("a");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err();
    assert_eq!(err.hint(), FractionalHint::Dot);
    assert_eq!(err.found(), Some(&'a'));
  }

  #[test]
  fn test_expected_digit() {
    let mut lexer = Lexer::<FractionalStrToken>::new(".");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err();
    assert_eq!(err.hint(), FractionalHint::Digit);
    assert!(err.found().is_none());

    let mut lexer = Lexer::<FractionalStrToken>::new(".a");
    let err = lex_fractional::<FractionalStrToken>(&mut lexer).unwrap_err();
    assert_eq!(err.hint(), FractionalHint::Digit);
    assert_eq!(err.found(), Some(&'a'));
  }

  #[test]
  fn test_ok() {
    const INPUT: &[&str] = &[
      ".123",
      ".1",
      ".0000000",
      ".0000000001",
      ".123x", // this works because the lexer should yield the first token as "e+123"
    ];

    for &input in INPUT {
      let mut lexer = Lexer::<FractionalStrToken>::new(input);
      assert!(lex_fractional::<FractionalStrToken>(&mut lexer).is_ok(), "fail on input: {input}");
    }
  }
}