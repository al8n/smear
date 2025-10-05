use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{
    Lexeme, UnexpectedEnd, UnexpectedLexeme,
    recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
  },
};

use crate::{
  hints::{FloatHint, HexFloatHint},
  lexer::handlers::{self, ValidateNumberChar, handle_number_suffix},
};

use super::error;

pub(super) mod slice;
pub(super) mod str;

#[inline(always)]
pub(super) fn increase_recursion_depth<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, RecursionLimitExceeded>>
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| error::LexerError::new(lexer.span(), error::LexerErrorData::State(e)))
}

#[inline(always)]
fn handle_hex_float_missing_exponent_then_check_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
) -> Result<S::Slice<'a>, error::LexerErrors<Char, E>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  Char: Copy + ValidateNumberChar<GraphQLxHexNumber>,
{
  let mut errs = error::LexerErrors::default();
  errs.push(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::HexFloat(error::HexFloatError::MissingExponent(lexer.span().into())),
  ));

  match handle_valid_hex_suffix(
    lexer,
    remainder_len,
    remainder,
    error::HexFloatError::UnexpectedSuffix,
  ) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

#[inline]
fn fractional_error<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
) -> E
where
  Char: Copy + ValidateNumberChar<GraphQLxNumber>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<FloatHint>> + From<UnexpectedLexeme<Char, FloatHint>>,
{
  handlers::lit_float_suffix_error(lexer, remainder_len, remainder, is_ignored_char, || {
    FloatHint::Fractional
  })
}

#[inline]
fn hex_fractional_error<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
) -> E
where
  Char: Copy + ValidateNumberChar<GraphQLxNumber>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<HexFloatHint>> + From<UnexpectedLexeme<Char, HexFloatHint>>,
{
  handlers::lit_float_suffix_error(lexer, remainder_len, remainder, is_ignored_char, || {
    HexFloatHint::Fractional
  })
}

#[allow(clippy::result_large_err)]
#[inline]
fn handle_float_missing_integer_part_error_then_check_suffix<'a, Char, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
) -> Result<S::Slice<'a>, error::LexerErrors<Char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  Char: Copy + ValidateNumberChar<GraphQLxNumber>,
{
  let mut errs = error::LexerErrors::default();
  errs.push(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::Float(error::FloatError::MissingIntegerPart(lexer.span().into())),
  ));

  match handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
    error::FloatError::UnexpectedSuffix,
  ) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

#[allow(clippy::result_large_err)]
#[inline]
fn handle_hex_float_missing_integer_part_error_then_check_suffix<'a, Char, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
) -> Result<S::Slice<'a>, error::LexerErrors<Char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  Char: Copy + ValidateNumberChar<GraphQLxHexExponent>,
{
  let mut errs = error::LexerErrors::default();
  errs.push(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::HexFloat(error::HexFloatError::MissingIntegerPart(
      lexer.span().into(),
    )),
  ));

  match handle_hex_float_suffix(
    lexer,
    remainder_len,
    remainder,
    error::HexFloatError::UnexpectedSuffix,
  ) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

#[inline]
fn handle_decimal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLxNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

#[inline]
fn handle_valid_binary_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLxBinaryNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

#[inline]
fn handle_valid_octal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLxOctalNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

#[inline]
fn handle_valid_hex_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLxHexNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

#[inline]
fn handle_hex_float_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<GraphQLxHexExponent>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_number_suffix(lexer, remainder_len, remainder, unexpected_suffix)
}

struct GraphQLxNumber;
struct GraphQLxHexNumber;
struct GraphQLxHexExponent;
struct GraphQLxOctalNumber;
struct GraphQLxBinaryNumber;

impl ValidateNumberChar<GraphQLxNumber> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'a'..='z' | 'A'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLxHexNumber> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'g'..='z' | 'G'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLxHexExponent> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'a'..='z' | 'A'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLxNumber> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}

impl ValidateNumberChar<GraphQLxHexNumber> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'g'..=b'z' | b'G'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}

impl ValidateNumberChar<GraphQLxHexExponent> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}

impl ValidateNumberChar<GraphQLxOctalNumber> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, '8'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLxOctalNumber> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'8'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}

impl ValidateNumberChar<GraphQLxBinaryNumber> for char {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, '2'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLxBinaryNumber> for u8 {
  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'2'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}
