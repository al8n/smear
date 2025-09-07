// use logos::{Lexer, Logos, Source};
// use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
// use logosky::{IsAsciiChar, utils::{Lexeme, PositionedChar, UnexpectedEnd, UnexpectedLexeme}};
// use smear_parser::source::AsciiChar;

// pub type UnexpectedEndOfExponent = UnexpectedEnd<ExponentHint>;

// /// The hint about what is expected for the next character
// #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
// pub enum ExponentHint {
//   /// Expect the next character to be digit.
//   #[display("digit")]
//   Digit,
//   /// Expect the next character to be a sign or a digit.
//   #[display("'+', '-' or digit")]
//   SignOrDigit,
//   /// Expect the next character to be an exponent identifier 'e' or 'E'.
//   #[display("'e' or 'E'")]
//   Identifier,
// }

// /// Exponent parsing error
// #[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
// #[unwrap(ref)]
// #[try_unwrap(ref)]
// pub enum ExponentError<C> {
//   /// The exponent has an unexpected suffix, e.g. `e+1x`
//   UnexpectedSuffix(Lexeme<C>),
//   /// The exponent found an unexpected character.
//   UnexpectedLexeme(UnexpectedLexeme<C, ExponentHint>),
//   /// The exponent expects one or more characters
//   UnexpectedEof(UnexpectedEndOfExponent),
// }

// impl<C> From<ExponentHint> for ExponentError<C> {
//   #[inline]
//   fn from(hint: ExponentHint) -> Self {
//     Self::UnexpectedEof(UnexpectedEndOfExponent::with_name("exponent".into(), hint))
//   }
// }

// impl<C> Default for ExponentError<C> {
//   fn default() -> Self {
//     Self::UnexpectedEof(UnexpectedEndOfExponent::with_name("exponent".into(), ExponentHint::SignOrDigit))
//   }
// }

// #[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
// #[logos(error(ExponentError<u8>, |lexer| default_error(lexer.slice().first().copied())))]
// #[logos(source = [u8])]
// pub enum ExponentBytesToken {
//   #[regex(r"[+-]?[0-9]+")]
//   #[regex(r"[+-]?(\D)?", unexpected_lexeme)]
//   #[regex(r"[+-]?[0-9]+\w", |lexer| unexpected_suffix(lexer.slice().last().copied().expect("there is at least one char")))]
//   Ok,
// }

// #[derive(Logos, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
// #[logos(error(ExponentError<char>, |lexer| default_error(lexer.slice().chars().next())))]
// pub enum ExponentStrToken {
//   #[regex(r"[+-]?[0-9]+")]
//   #[regex(r"[+-]?(\D)?", unexpected_lexeme)]
//   #[regex(r"[+-]?[0-9]+\w", |lexer| unexpected_suffix(lexer.slice().chars().last().expect("there is at least one char")))]
//   Ok,
// }

// #[inline]
// fn unexpected_lexeme<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), ExponentError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
// where
//   T: Logos<'a>,
//   <T::Source as Source>::Slice<'a>: ExponentInput<'a>,
//   <<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char: IsAsciiChar,
// {
//   <<T::Source as Source>::Slice<'a> as sealed::Sealed<'_>>::unexpected_lexeme::<T>(lexer)
//   // let slice = lexer.slice();
//   // let mut iter = <<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::iter(slice);

//   // // let mut ul = match iter.next() {
//   // //   Some(ch) if ch.one_of(&[AsciiChar::Plus, AsciiChar::Minus]) => {
//   // //     match iter.next() {
//   // //       Some(ch) => UnexpectedExponentCharacter::new(ch, ExponentHint::Digit).into(),
//   // //       None => ExponentHint::Digit.into(),
//   // //     }
//   // //   },
//   // //   Some(ch) => {
//   // //     UnexpectedLexeme::Char(PositionedChar::with_position(ch, lexer.span().start + 1))
//   // //     // UnexpectedExponentCharacter::new(ch, ExponentHint::SignOrDigit).into()
//   // //   },
//   // //   None => return Err(ExponentHint::SignOrDigit.into()),
//   // // };

//   // Err(match iter.next() {
//   //   Some(ch) if ch.one_of(&[AsciiChar::Plus, AsciiChar::Minus]) => {
//   //     match iter.next() {
//   //       Some(ch) => UnexpectedExponentCharacter::new(ch, ExponentHint::Digit).into(),
//   //       None => ExponentHint::Digit.into(),
//   //     }
//   //   },
//   //   Some(ch) => {
//   //     UnexpectedLexeme::from_char(PositionedChar::with_position(ch, lexer.span().start + 1), ExponentHint::SignOrDigit).into()

//   //     // UnexpectedExponentCharacter::new(ul, ExponentHint::SignOrDigit).into()
//   //   },
//   //   None => ExponentHint::SignOrDigit.into(),
//   // })
// }

// #[inline(always)]
// fn unexpected_suffix<C>(ch: C) -> Result<(), ExponentError<C>> {
//   Err(ExponentError::UnexpectedSuffix(ch))
// }

// #[inline(always)]
// fn default_error<C>(ch: Option<C>) -> ExponentError<C> {
//   match ch {
//     None => ExponentHint::SignOrDigit.into(),
//     Some(ch) => ExponentError::UnexpectedCharacter(UnexpectedExponentCharacter::new(ch, ExponentHint::Digit)),
//   }
// }

// pub fn lex_exponent<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), ExponentError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
// where
//   T: Logos<'a>,
//   <T::Source as Source>::Slice<'a>: ExponentInput<'a>,
// {
//   use sealed::Sealed;

//   let (end, res) = lexer.remainder().lex();
//   lexer.bump(end);
//   res
// }

// pub fn lex_exponent_with_identifier<'a, T>(lexer: &mut Lexer<'a, T>) -> Result<(), ExponentError<<<T::Source as Source>::Slice<'a> as sealed::Sealed<'a>>::Char>>
// where
//   T: Logos<'a>,
//   <T::Source as Source>::Slice<'a>: ExponentInput<'a>,
// {
//   use sealed::Sealed;

//   let (end, res) = lexer.remainder().lex_with_identifier();
//   lexer.bump(end);
//   res
// }

// pub trait ExponentInput<'c>: sealed::Sealed<'c> {
// }

// mod sealed {
//   use crate::lexer::BOM;

//   use super::{*, super::{FindLongestInvalidBytesSequence, FindLongestInvalidSequence, super::{Digits, DigitsBytes}}};
//   use logosky::source::CustomSource;
//   use core::ops::Range;

//   #[derive(Logos)]
//   enum Helper {
//     #[regex(r"[0-9]+", |lexer| lexer.span())]
//     Digits(Range<usize>),
//     #[regex(r"[^\d \t\r\n,\ufeff]", |lexer| {
//       let remainder = lexer.remainder();
//       let slice = lexer.slice();
//       let span = lexer.span();

//       match slice.chars().next().expect("there is at least one char") {
//         'a'..='z' | 'A'..='Z' | '_' => {
//           for (idx, ch) in remainder.char_indices() {
//             if matches!(ch, 'a'..='z' | 'A'..='Z' | '_' ) {
//               continue;
//             }

//             // bump the lexer to the end of the invalid sequence
//             lexer.bump(idx);
//             // return the range of the invalid sequence
//             return span.start..(span.end + idx);
//           }

//           let len = remainder.len();
//           // bump the lexer to the end of the invalid sequence
//           lexer.bump(len);
//           // return the range of the invalid sequence
//           span.start..(span.end + len)
//         },
//         _ => {
//           span
//         }
//       }
//     })]
//     Invalid(Range<usize>),
//     #[regex(r"[ \t\r\n,\ufeff]")]
//     Whitespace,
//   }

//   #[derive(Logos)]
//   #[logos(source = [u8])]
//   enum BytesHelper {
//     #[regex(r"[0-9]+", |lexer| lexer.span())]
//     Digits(Range<usize>),
//     #[regex(r"[^\d \t\r\n,\ufeff]", |lexer| {
//       let remainder = lexer.remainder();
//       let slice = lexer.slice();
//       let span = lexer.span();

//       match slice[0] {
//         b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
//           for (idx, &ch) in remainder.iter().enumerate() {
//             if matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' ) {
//               continue;
//             }

//             // bump the lexer to the end of the invalid sequence
//             lexer.bump(idx);
//             // return the range of the invalid sequence
//             return span.start..(span.end + idx);
//           }

//           let len = remainder.len();
//           // bump the lexer to the end of the invalid sequence
//           lexer.bump(len);
//           // return the range of the invalid sequence
//           span.start..(span.end + len)
//         },
//         _ => {
//           span
//         }
//       }
//     })]
//     Invalid(Range<usize>),
//     #[regex(r"[ \t\r\n,\ufeff]")]
//     Whitespace,
//   }

//   pub trait Sealed<'c> {
//     type Char: 'c;
//     type Slice: 'c;
//     type Token<'t>: Logos<'t> where Self: 't;

//     /// Returns a lexer for the exponent suffix.
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>);

//     /// Returns a lexer for the exponent suffix.
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>);
    
//     fn lex_exponent<T>(lexer: &mut Lexer<'c, T>) -> Result<(), ExponentError<Self::Char>>
//     where
//       T: Logos<'c>,
//       T::Source: Source<Slice<'c> = Self::Slice>;
    
//     fn lex_exponent_with_identifier<T>(lexer: &mut Lexer<'c, T>) -> Result<(), ExponentError<Self::Char>>
//     where
//       T: Logos<'c>,
//       T::Source: Source<Slice<'c> = Self::Slice>;

//     /// Returns an iterator over the characters in the exponent suffix.
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c;
//   }

//   impl<'a, T: Sealed<'a> + ?Sized> ExponentInput<'a> for T {}

//   impl<'c> Sealed<'c> for [u8] {
//     type Char = u8;
//     type Slice = &'c [u8];
//     type Token<'t> = ExponentBytesToken;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       let mut lexer = Lexer::<Self::Token<'_>>::new(self);
//       let res = match lexer.next() {
//         None => Err(ExponentHint::SignOrDigit.into()),
//         Some(tok) => match tok {
//           Ok(_) => Ok(()),
//           Err(e) => Err(e),
//         },
//       };
//       (lexer.span().end, res)
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       if self.is_empty() {
//         return (0, Err(ExponentHint::Identifier.into()));
//       }

//       let first = self[0];
//       match first {
//         b'e' | b'E' => {},
//         ch => return (1, Err(UnexpectedExponentCharacter::new(ch, ExponentHint::Identifier).into())),
//       }

//       let (end, res) = self[1..].lex();
//       (end + 1, res)
//     }

//     fn lex_exponent<T>(lexer: &mut Lexer<'c, T>) -> Result<(), ExponentError<Self::Char>>
//     where
//       T: Logos<'c>,
//       T::Source: Source<Slice<'c> = Self::Slice>
//     {
//       todo!()
//     }

//     fn lex_exponent_with_identifier<T>(lexer: &mut Lexer<'c, T>) -> Result<(), ExponentError<Self::Char>>
//     where
//       T: Logos<'c>,
//       T::Source: Source<Slice<'c> = Self::Slice>
//     {
//       todo!()
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c {
//       slice.iter().copied()
//     }
//   }

//   impl<'c> Sealed<'c> for str {
//     type Char = char;
//     type Slice = &'c str;
//     type Token<'t> = ExponentStrToken;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       let mut lexer = Lexer::<Self::Token<'_>>::new(self);
//       let res = match lexer.next() {
//         None => Err(ExponentHint::SignOrDigit.into()),
//         Some(tok) => match tok {
//           Ok(_) => Ok(()),
//           Err(e) => Err(e),
//         },
//       };
//       (lexer.span().end, res)
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       match self.chars().next() {
//         None => {
//           (0, Err(ExponentHint::Identifier.into()))
//         },
//         Some('e') | Some('E') => {
//           let (end, res) = self[1..].lex();
//           (end + 1, res)
//         },
//         Some(ch) => {
//           (1, Err(UnexpectedLexeme::from(ch, ExponentHint::Identifier).into()))
//         },
//       }
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'c {
//       slice.chars()
//     }
//   }

//   impl<'l, T: Sealed<'l> + ?Sized> Sealed<'l> for &'l T {
//     type Char = T::Char;
//     type Slice = T::Slice;
//     type Token<'t> = T::Token<'t> where Self: 't;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex()
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex_with_identifier()
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
//       T::iter(slice)
//     }
//   }

//   impl<'l, T: Sealed<'l> + ?Sized> Sealed<'l> for CustomSource<T> {
//     type Char = T::Char;
//     type Slice = T::Slice;
//     type Token<'t> = T::Token<'t> where Self: 't;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       self.as_ref().lex()
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       self.as_ref().lex_with_identifier()
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
//       T::iter(slice)
//     }
//   }

//   #[cfg(feature = "bstr")]
//   impl<'l> Sealed<'l> for bstr::BStr {
//     type Char = u8;
//     type Slice = &'l [u8];
//     type Token<'t> = ExponentBytesToken where Self: 't;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex()
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex_with_identifier()
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
//       slice.iter().copied()
//     }
//   }

//   #[cfg(feature = "bytes")]
//   impl<'l> Sealed<'l> for bytes::Bytes {
//     type Char = u8;
//     type Slice = &'l [u8];
//     type Token<'t> = ExponentBytesToken where Self: 't;

//     #[inline]
//     fn lex(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex()
//     }

//     #[inline]
//     fn lex_with_identifier(&self) -> (usize, Result<(), ExponentError<Self::Char>>) {
//       (**self).lex_with_identifier()
//     }

//     #[inline(always)]
//     fn iter(slice: Self::Slice) -> impl core::iter::Iterator<Item = Self::Char> + 'l {
//       slice.iter().copied()
//     }
//   }
// }

// #[cfg(test)]
// mod tests {
//   use super::*;

//   #[test]
//   fn test_expected_identifier() {
//     let mut lexer = Lexer::<ExponentStrToken>::new("");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
//     assert_eq!(err.hint(), &ExponentHint::Identifier);

//     let mut lexer = Lexer::<ExponentStrToken>::new("a");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
//     assert_eq!(err.hint(), ExponentHint::Identifier);
//     assert_eq!(err.found(), &'a');
//   }

//   #[test]
//   fn test_expected_sign_or_digit() {
//     let mut lexer = Lexer::<ExponentStrToken>::new("e");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
//     assert_eq!(err, ExponentHint::SignOrDigit);

//     let mut lexer = Lexer::<ExponentStrToken>::new("eX");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
//     assert_eq!(err.hint(), ExponentHint::SignOrDigit);
//     assert_eq!(err.found(), &'X');
//   }

//   #[test]
//   fn test_expected_digit() {
//     let mut lexer = Lexer::<ExponentStrToken>::new("e+");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
//     assert_eq!(err, ExponentHint::Digit);

//     let mut lexer = Lexer::<ExponentStrToken>::new("e-");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_eof();
//     assert_eq!(err, ExponentHint::Digit);

//     let mut lexer = Lexer::<ExponentStrToken>::new("e+X");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
//     assert_eq!(err.hint(), ExponentHint::Digit);
//     assert_eq!(err.found(), &'X');

//     let mut lexer = Lexer::<ExponentStrToken>::new("e-X");
//     let err = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_character();
//     assert_eq!(err.hint(), ExponentHint::Digit);
//     assert_eq!(err.found(), &'X');
//   }

//   #[test]
//   fn test_unexpected_suffix() {
//     let mut lexer = Lexer::<ExponentStrToken>::new("e+123x");
//     let ch = lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).unwrap_err().unwrap_unexpected_suffix();
//     assert_eq!(ch, 'x');
//   }

//   #[test]
//   fn test_ok() {
//     const INPUT: &[&str] = &[
//       "e0",
//       "e1",
//       "e123",
//       "e+0",
//       "e+1",
//       "e+123",
//       "e-0",
//       "e-1",
//       "e-123",
//       "e+123 x", // this works because the lexer should yield the first token as "e+123"
//       "e+123\tx", // this works because the lexer should yield the first token as "e+123"
//       "e+123\rx", // this works because the lexer should yield the first token as "e+123"
//       "e+123\nx", // this works because the lexer should yield the first token as "e+123"
//       "e+123\u{feff}x", // this works because the lexer should yield the first token as "e+123"
//       "e+123,x", // this works because the lexer should yield the first token as "e+123"
//     ];

//     for &input in INPUT {
//       let mut lexer = Lexer::<ExponentStrToken>::new(input);
//       assert!(lex_exponent_with_identifier::<ExponentStrToken>(&mut lexer).is_ok());
//     }
//   }
// }