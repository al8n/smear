use logos::{Lexer, Logos, Source};
use logosky::utils::Span;

use crate::error::{
  ExponentHint, FloatError, FloatHint, IntError, LexerErrorData, LexerErrors, LineTerminatorHint,
  UnpairedSurrogateHint, UnterminatedHint,
};

fn assert_token<'a, Token, StateError>(source: &'a str, kind: Token, length: usize)
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug + Eq,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer(source);
  assert_eq!(
    lexer.next(),
    Some(Ok(kind)),
    "Testing the lexing of string '{source}'"
  );
  assert_eq!(
    lexer.span(),
    0..length,
    "Testing the lexing of string '{source}'"
  );
}

pub(super) trait TestToken<'a>: Logos<'a> + Eq + Copy + core::fmt::Debug {
  fn is_ignored(&self) -> bool;

  fn block_string_literal(&self) -> Option<&'a str>;

  fn inline_string_literal(&self) -> Option<&'a str>;

  fn from_inline_string_literal(s: &'a str) -> Self;

  fn from_float_literal(s: &'a str) -> Self;

  fn from_integer_literal(s: &'a str) -> Self;

  fn test_lexer(source: &'a Self::Source) -> TestLexer<'a, Self>
  where
    Self::Extras: Default,
  {
    TestLexer::new(source)
  }

  fn test_lexer_with_extras(source: &'a Self::Source, extras: Self::Extras) -> TestLexer<'a, Self> {
    TestLexer::with_extras(source, extras)
  }
}

pub(super) struct TestLexer<'a, T: Logos<'a>> {
  inner: Lexer<'a, T>,
}

impl<'a, T: TestToken<'a>> Iterator for TestLexer<'a, T> {
  type Item = Result<T, T::Error>;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match self.inner.next() {
        None => return None,
        Some(Ok(tok)) => {
          if tok.is_ignored() {
            // continue lexing
            continue;
          } else {
            return Some(Ok(tok));
          }
        }
        Some(Err(e)) => return Some(Err(e)),
      }
    }
  }
}

impl<'a, T: Logos<'a>> TestLexer<'a, T> {
  pub fn new(source: &'a T::Source) -> Self
  where
    T::Extras: Default,
  {
    Self {
      inner: Lexer::new(source),
    }
  }

  pub fn with_extras(source: &'a T::Source, extras: T::Extras) -> Self {
    Self {
      inner: Lexer::with_extras(source, extras),
    }
  }

  pub fn span(&self) -> core::ops::Range<usize> {
    self.inner.span()
  }

  pub fn slice(&self) -> <T::Source as Source>::Slice<'a> {
    self.inner.slice()
  }
}

pub(super) fn test_unexpected_character<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("+1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unexpected_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'+');
  assert_eq!(err.position(), 0);

  let mut lexer = Token::test_lexer("-A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unexpected_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'-');
  assert_eq!(err.position(), 0);
}

pub(super) fn test_unknown_character<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("<");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_unknown_lexeme()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'<');
  assert_eq!(err.position(), 0);

  const INPUT: &str = r#"
# U+FEFF
# U+000B
# U+000C
# U+0085
 # U+00A0
‎# U+200E
‏# U+200F
 # U+2028
 # U+2029
  "#;

  let mut lexer = Token::test_lexer(INPUT);
  for expected_char in [
    '\u{000B}', '\u{000C}', '\u{0085}', '\u{00A0}', '\u{200E}', '\u{200F}', '\u{2028}', '\u{2029}',
  ] {
    let err = lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data()
      .unwrap_unknown_lexeme()
      .unwrap_char();
    assert_eq!(err.char_ref(), &expected_char);
  }
}

pub(super) fn test_number_leading_zero<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("00");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Int(IntError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("-01");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Int(IntError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("01.23");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("-01.23");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("01e3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("-01E3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("01e+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("-01E+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("01e-3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));

  let mut lexer = Token::test_lexer("-01E-3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::LeadingZeros(_))
  ));
}

pub(super) fn test_int_leading_zeros_and_suffix<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("00abc");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_int_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(0..2));
  let err2 = errs[1]
    .data()
    .unwrap_int_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(2..5));

  let mut lexer = Token::test_lexer("-00abc");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_int_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(1..3));
  let err2 = errs[1]
    .data()
    .unwrap_int_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(3..6));
}

pub(super) fn test_float_leading_zeros_and_other<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("01.");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_char_ref();
  assert_eq!(err1.char_ref(), &'0');
  assert_eq!(err1.position(), 0);
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_end_ref();
  assert_eq!(err2.hint(), &FloatHint::Fractional);
  assert_eq!(errs[1].span(), Span::from(0..3));

  let mut lexer = Token::test_lexer("-01.");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_char_ref();
  assert_eq!(err1.char_ref(), &'0');
  assert_eq!(err1.position(), 1);
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_end_ref();
  assert_eq!(err2.hint(), &FloatHint::Fractional);
  assert_eq!(errs[1].span(), Span::from(0..4));

  let mut lexer = Token::test_lexer("00001.23abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  let err1 = errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_leading_zeros_ref()
    .unwrap_span_ref();
  assert_eq!(err1, &Span::from(0..4));
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span_ref();
  assert_eq!(err2, &Span::from(8..12));
}

pub(super) fn test_invalid_number_suffix<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("0abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, (1..4).into());

  let mut lexer = Token::test_lexer("0a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 1);

  let mut lexer = Token::test_lexer("-0abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(2..5));

  let mut lexer = Token::test_lexer("-0a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 2);

  let mut lexer = Token::test_lexer("123abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(3..6));

  let mut lexer = Token::test_lexer("123a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 3);

  let mut lexer = Token::test_lexer("-123abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(4..7));

  let mut lexer = Token::test_lexer("-123a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_int()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 4);

  let mut lexer = Token::test_lexer("123.45a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::test_lexer("-123.45a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::test_lexer("123e3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 5);

  let mut lexer = Token::test_lexer("-123E3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::test_lexer("123e+3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::test_lexer("-123E+3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::test_lexer("123e-3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 6);

  let mut lexer = Token::test_lexer("-123E-3a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'a');
  assert_eq!(err.position(), 7);

  let mut lexer = Token::test_lexer("1.23.4");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(4..6));

  let mut lexer = Token::test_lexer("-1.23.4 ");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_span();
  assert_eq!(err, Span::from(5..7));
  assert_eq!(lexer.span(), 0..7);

  // check that we don't consume trailing valid items
  let mut lexer = Token::test_lexer("1.23.{}");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::test_lexer("1.23. {}");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::test_lexer("1.23. []");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::test_lexer("1.23. foo");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);

  let mut lexer = Token::test_lexer("1.23. $foo");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_suffix()
    .unwrap_char();
  assert_eq!(err.char_ref(), &'.');
  assert_eq!(err.position(), 4);
  assert_eq!(lexer.span(), 0..5);
}

pub(super) fn test_missing_integer_part<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer(".123");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::test_lexer("-.123");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::test_lexer(".123e3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));
  let mut lexer = Token::test_lexer("-.123E3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));

  let mut lexer = Token::test_lexer(".123e+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));
  let mut lexer = Token::test_lexer("-.123E+3");
  assert!(matches!(
    lexer
      .next()
      .unwrap()
      .unwrap_err()
      .pop()
      .unwrap()
      .into_data(),
    LexerErrorData::Float(FloatError::MissingIntegerPart)
  ));
}

pub(super) fn test_missing_integer_part_and_invalid_suffix<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer(".123abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);

  errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_missing_integer_part_ref();
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span();
  assert_eq!(err2, Span::from(4..8));

  let mut lexer = Token::test_lexer("-.123abcd");
  let errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 2);
  errs[0]
    .data()
    .unwrap_float_ref()
    .unwrap_missing_integer_part_ref();
  let err2 = errs[1]
    .data()
    .unwrap_float_ref()
    .unwrap_unexpected_suffix_ref()
    .unwrap_span();
  assert_eq!(err2, Span::from(5..9));
}

pub(super) fn test_unexpected_float_eof<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("1.");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Fractional);

  let mut lexer = Token::test_lexer("-1.");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Fractional);

  let mut lexer = Token::test_lexer("1e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::test_lexer("-1e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::test_lexer("1e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("-1e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("1e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("-1e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("1.0e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::test_lexer("-1.0e");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));

  let mut lexer = Token::test_lexer("1.0e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("-1.0e-");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("1.0e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));

  let mut lexer = Token::test_lexer("-1.0e+");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_end();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::Digit));
}

pub(super) fn test_unexpected_number_lexme<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let mut lexer = Token::test_lexer("1.a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'a');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::test_lexer("-1.a");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'a');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::test_lexer("1.A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::test_lexer("-1.A");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::test_lexer("1.abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (2..5).into());
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::test_lexer("-1.abc");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (3..6).into());
  assert_eq!(lexer.span(), (0..6));

  let mut lexer = Token::test_lexer("1.e1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (2..4).into());
  assert_eq!(lexer.span(), (0..4));

  let mut lexer = Token::test_lexer("-1.e1");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Fractional);
  assert_eq!(err.lexeme().unwrap_span(), (3..5).into());
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::test_lexer("1.0eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 4);
  assert_eq!(lexer.span(), (0..5));

  let mut lexer = Token::test_lexer("-1.0eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 5);
  assert_eq!(lexer.span(), 0..6);

  let mut lexer = Token::test_lexer("1.0eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (4..12).into());
  assert_eq!(lexer.span(), 0..12);

  let mut lexer = Token::test_lexer("-1.0eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (5..13).into());
  assert_eq!(lexer.span(), 0..13);

  let mut lexer = Token::test_lexer("1eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 2);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::test_lexer("-1eA");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_char_ref().char_ref(), &'A');
  assert_eq!(err.lexeme().unwrap_char_ref().position(), 3);
  assert_eq!(lexer.span(), 0..4);

  let mut lexer = Token::test_lexer("1eA123.456");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (2..10).into());
  assert_eq!(lexer.span(), 0..10);

  let mut lexer = Token::test_lexer("-1eA123.456");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (3..11).into());
  assert_eq!(lexer.span(), (0..11));

  let mut lexer = Token::test_lexer("1eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (2..10).into());
  assert_eq!(lexer.span(), (0..10));

  let mut lexer = Token::test_lexer("-1eA123.456 some_name");
  let err = lexer
    .next()
    .unwrap()
    .unwrap_err()
    .pop()
    .unwrap()
    .into_data()
    .unwrap_float()
    .unwrap_unexpected_lexeme();
  assert_eq!(err.hint(), &FloatHint::Exponent(ExponentHint::SignOrDigit));
  assert_eq!(err.lexeme().unwrap_span(), (3..11).into());
  assert_eq!(lexer.span(), (0..11));
}

pub(super) fn test_integer_ok<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let input: &[(&str, Token, usize)] = &[
    ("4", Token::from_integer_literal("4"), 1),
    ("-4", Token::from_integer_literal("-4"), 2),
    ("9", Token::from_integer_literal("9"), 1),
    ("0", Token::from_integer_literal("0"), 1),
    ("-0", Token::from_integer_literal("-0"), 2),
  ];

  for (source, kind, length) in input {
    assert_token(source, *kind, *length);
  }
}

pub(super) fn test_float_ok<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let input: &[(&str, Token, usize)] = &[
    ("4.123", Token::from_float_literal("4.123"), 5),
    ("-4.123", Token::from_float_literal("-4.123"), 6),
    ("0.123", Token::from_float_literal("0.123"), 5),
    ("123e4", Token::from_float_literal("123e4"), 5),
    ("123E4", Token::from_float_literal("123E4"), 5),
    ("123e-4", Token::from_float_literal("123e-4"), 6),
    ("123e+4", Token::from_float_literal("123e+4"), 6),
    ("-1.123e4", Token::from_float_literal("-1.123e4"), 8),
    ("-1.123E4", Token::from_float_literal("-1.123E4"), 8),
    ("-1.123e-4", Token::from_float_literal("-1.123e-4"), 9),
    ("-1.123e+4", Token::from_float_literal("-1.123e+4"), 9),
    ("-1.123e4567", Token::from_float_literal("-1.123e4567"), 11),
  ];

  for (source, kind, length) in input {
    assert_token(source, *kind, *length);
  }
}

pub(super) fn test_inline_string_ok<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug + Eq + 'a,
{
  let input: &[(&str, Token, usize)] = &[
    (r#""""#, Token::from_inline_string_literal(r#""""#), 2),
    {
      const CASE: &str = r#""hello""#;
      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""hello world""#;
      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""hello✨""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \" quote""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \\ backslash""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \n new line""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \r carriage return""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \t tab""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
    {
      const CASE: &str = r#""escaped \u1234 unicode\"""#;

      (CASE, Token::from_inline_string_literal(CASE), CASE.len())
    },
  ];

  for (source, kind, length) in input {
    assert_token(source, *kind, *length);
  }
}

pub(super) fn test_unterminated_inline_string<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  let mut lexer = Token::test_lexer(r#"""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..1));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..1);

  let mut lexer = Token::test_lexer(r#""unterminated"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);

  let err1 = errs.pop().unwrap();
  assert_eq!(err1.span(), Span::from(0..13));

  let err1 = err1
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..13);

  let mut lexer = Token::test_lexer(r#""escaped \" quote"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..17));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);

  let mut lexer = Token::test_lexer(
    r#""escaped 
  new line""#,
  );
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..21));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unexpected_line_terminator();
  match err1.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 9);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 9);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err1.lexeme().unwrap_span(), Span::from(9..11));
    }
  }

  let mut lexer = Token::test_lexer(r#""hello✨"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..r#""hello✨"#.len()));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);

  let mut lexer = Token::test_lexer(r#""\n\n\\u{c}\nPSK\\u{1}\\0\\0\\0י"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(
    err.span(),
    Span::from(0..r#""\n\n\\u{c}\nPSK\\u{1}\\0\\0\\0י"#.len())
  );

  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::Quote);
}

pub(super) fn test_incomplete_unicode_and_eof<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  let mut lexer = Token::test_lexer(r#""\u222"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();

  assert_eq!(err.span(), Span::from(0..6));

  let err = err.into_data().unwrap_string();
  assert_eq!(err.len(), 2, "Expected 2 errors, got {err:?}");
  let err1 = err[0].unwrap_unicode_ref().unwrap_incomplete_ref();
  assert_eq!(err1.span(), Span::from(1..6));

  let err2 = err[1].unwrap_unterminated_ref();
  assert_eq!(err2.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..6);

  let mut lexer = Token::test_lexer(r#""\u"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();

  assert_eq!(err.span(), Span::from(0..3));

  let err = err.into_data().unwrap_string();
  assert_eq!(err.len(), 2, "Expected 2 errors, got {err:?}");
  let err1 = err[0].unwrap_unicode_ref().unwrap_incomplete_ref();
  assert_eq!(err1.span(), Span::from(1..3));

  let err2 = err[1].unwrap_unterminated_ref();
  assert_eq!(err2.hint(), &UnterminatedHint::Quote);
  assert_eq!(lexer.span(), 0..3);
}

pub(super) fn test_unexpected_line_terminator<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  let mut lexer = Token::test_lexer(
    r#""
hello
""#,
  );

  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");

  let err1 = errs[0].unwrap_unexpected_line_terminator_ref();
  match err1.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 1);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err1.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err1.lexeme().unwrap_char_ref().position(), 1);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err1.lexeme().unwrap_span(), Span::from(1..3));
    }
  }
  assert_eq!(lexer.span(), 0..9);

  let err2 = errs[1].unwrap_unexpected_line_terminator_ref();
  match err2.hint() {
    LineTerminatorHint::NewLine => {
      assert_eq!(err2.lexeme().unwrap_char_ref().char_ref(), &'\n');
      assert_eq!(err2.lexeme().unwrap_char_ref().position(), 7);
    }
    LineTerminatorHint::CarriageReturn => {
      assert_eq!(err2.lexeme().unwrap_char_ref().char_ref(), &'\r');
      assert_eq!(err2.lexeme().unwrap_char_ref().position(), 7);
    }
    LineTerminatorHint::CarriageReturnNewLine => {
      assert_eq!(err2.lexeme().unwrap_span(), Span::from(7..9));
    }
  }
  assert_eq!(lexer.span(), 0..9);
}

pub(super) fn test_unexpected_escaped<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  // "This is \"\"a test \a\d\q description"
  let mut lexer = Token::test_lexer(r#""This is \"\"a test \a\d\q description""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err1 = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(err1.len(), 3, "Expected 3 errors, got {err1:?}");
  assert!(
    errs.is_empty(),
    "Expected only one error, remaining {errs:?}"
  );

  let unexpected = err1[0].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'a');
  assert_eq!(unexpected.position(), 21);

  let unexpected = err1[1].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'd');
  assert_eq!(unexpected.position(), 23);

  let unexpected = err1[2].unwrap_unexpected_escaped_character_ref();
  assert_eq!(unexpected.char(), 'q');
  assert_eq!(unexpected.position(), 25);
}

pub(super) fn test_surrogate_pair<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default + core::fmt::Debug,
  StateError: core::fmt::Debug + Eq,
{
  const CASES: &[&str] = &[
    r#""string with unicode surrogate pair escape \uD83D\uDE00""#,
    r#""string with minimal surrogate pair escape \uD800\uDC00""#,
    r#""string with maximal surrogate pair escape \uDBFF\uDFFF""#,
  ];

  for case in CASES {
    let mut lexer = Token::test_lexer(case);
    let token = lexer.next().unwrap().expect(case).inline_string_literal();
    assert_eq!(token, Some(*case), "case: {case}");

    assert_eq!(lexer.slice(), *case, "case: {case}");
    assert_eq!(lexer.next(), None, "case: {case}");
  }
}

pub(super) fn test_invalid_surrogate_pair<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
{
  let mut lexer = Token::test_lexer(r#""Backwards pair \uDE00\uD83D""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");
  let err1 = &errs[0];
  let err = err1.unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::Low);
  assert_eq!(err.span(), Span::from(16..22));

  let err2 = &errs[1];
  let err = err2.unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::High);
  assert_eq!(err.span(), Span::from(22..28));

  let mut lexer = Token::test_lexer(r#""split pair \uD83D \uDE00""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 2, "Expected 2 errors, got {errs:?}");
  let err1 = &errs[0];
  let err = err1.unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::High);
  assert_eq!(err.span(), Span::from(12..18));

  let err2 = &errs[1];
  let err = err2.unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::Low);
  assert_eq!(err.span(), Span::from(19..25));

  let mut lexer = Token::test_lexer(r#""Lone lead surrogate \uD83E""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 1, "Expected 1 error, got {errs:?}");
  let err = errs[0].unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::High);
  assert_eq!(err.span(), Span::from(21..27));

  let mut lexer = Token::test_lexer(r#""Lone trail surrogate \uDD80""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let errs = errs.pop().unwrap().into_data().unwrap_string();
  assert_eq!(errs.len(), 1, "Expected 1 error, got {errs:?}");
  let err = errs[0].unwrap_unicode_ref().unwrap_unpaired_surrogate_ref();
  assert_eq!(err.hint(), &UnpairedSurrogateHint::Low);
  assert_eq!(err.span(), Span::from(22..28));
}

pub(super) fn test_unterminated_block_string<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
{
  let mut lexer = Token::test_lexer(r#"""""#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..3));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::TripleQuote);
  assert_eq!(lexer.span(), 0..3);

  let mut lexer = Token::test_lexer(r#""""\"#);
  let mut errs = lexer.next().unwrap().unwrap_err();
  assert_eq!(errs.len(), 1);
  let err = errs.pop().unwrap();
  assert_eq!(err.span(), Span::from(0..4));
  let err1 = err
    .into_data()
    .unwrap_string()
    .pop()
    .unwrap()
    .unwrap_unterminated();
  assert_eq!(err1.hint(), &UnterminatedHint::TripleQuote);
  assert_eq!(lexer.span(), 0..4);
}

pub(super) fn test_surrogate_pair_in_block_string<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  let mut lexer =
    Token::test_lexer(r#""""string with unicode surrogate pair escape \uD83D\uDE00""""#);
  let token = lexer.next().unwrap().expect("Should lex successfully");
  let block_string = token.block_string_literal();
  assert_eq!(
    block_string,
    Some(r#""""string with unicode surrogate pair escape \uD83D\uDE00""""#),
    "Should match"
  );
}

pub(super) fn test_escape_triple_quote_block_string<'a, Token, StateError>()
where
  Token: TestToken<'a, Source = str, Error = LexerErrors<char, StateError>> + core::fmt::Debug,
  Token::Extras: Default,
  StateError: core::fmt::Debug,
{
  let mut lexer = Token::test_lexer(
    r#""""

      block string uses \"""

""""#,
  );
  let token = lexer.next().unwrap().expect("Should lex successfully");
  let block_string = token.block_string_literal();
  assert_eq!(
    block_string,
    Some(
      r#""""

      block string uses \"""

""""#
    ),
    "Should match"
  );
}
