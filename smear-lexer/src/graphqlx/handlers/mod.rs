use tokora::{
  error::{UnexpectedEnd, UnexpectedLexeme},
  logos::{Lexer, Logos, Source},
  state::LimitExceeded,
  utils::Lexeme,
};

use crate::{
  graphqlx::lossless::LosslessState,
  handlers::{self, ValidateNumberChar, handle_number_suffix},
  hints::{FloatHint, HexFloatHint},
};

use super::error;

pub(super) mod slice;
pub(super) mod str;

#[inline(always)]
pub(super) fn increase_recursion_depth_and_token<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::increase_recursion_depth_and_token(lexer)
}

#[inline(always)]
pub(super) fn open_brace<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::open_brace(lexer)
}

#[inline(always)]
pub(super) fn tt_hook_and_then<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, error::LexerError<C, LimitExceeded>>,
) -> Result<O, error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::tt_hook_and_then(lexer, f)
}

#[allow(clippy::result_large_err)]
#[inline(always)]
pub(super) fn tt_hook_and_then_into_errors<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, error::LexerErrors<C, LimitExceeded>>,
) -> Result<O, error::LexerErrors<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::tt_hook_and_then_into_errors(lexer, f)
}

#[inline(always)]
pub(super) fn tt_hook_map<'a, C, T, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> O,
) -> Result<O, error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::tt_hook_map(lexer, f)
}

#[inline(always)]
pub(super) fn tt_hook<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  handlers::tt_hook(lexer)
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
  handlers::lit_float_suffix_error(
    "float",
    lexer,
    remainder_len,
    remainder,
    is_ignored_char,
    || FloatHint::Fractional,
  )
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
  handlers::lit_float_suffix_error(
    "hex float",
    lexer,
    remainder_len,
    remainder,
    is_ignored_char,
    || HexFloatHint::Fractional,
  )
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

/// Which suffix a typed number rule matched.
#[derive(Clone, Copy)]
pub(super) enum Suffix {
  /// `i` or `u`, and a width.
  Int,
  /// `f32` or `f64`.
  Float,
}

/// Where a typed number's suffix is refused: the offset of its letter in the rule's match, and
/// how many bytes past the match the refused run takes.
pub(super) struct SuffixRefusal {
  pub(super) letter: usize,
  pub(super) past: usize,
}

/// Whether the suffix a typed number rule matched stands; and when it does not, where the refused
/// run begins and how far past the match it reaches.
///
/// A suffix stands when the byte after it is none of a letter, a digit, `.` and `_`, and, for an
/// integer's, when its width fits a `u32`. Otherwise the number is refused as a letter after it is
/// today, by [`handle_number_suffix`]'s rule: over the run that begins at the suffix's letter and
/// takes letters, digits and `.`. `slice` is the rule's match, and `remainder` what follows it.
pub(super) fn suffix_refusal(
  slice: &[u8],
  remainder: &[u8],
  suffix: Suffix,
) -> Option<SuffixRefusal> {
  // The rule's pattern ends in the suffix, so its letter is the last `i` or `u` of an integer's
  // match, before the width's digits, and the last `f` of a float's.
  let letter = match suffix {
    Suffix::Int => slice
      .iter()
      .rposition(|byte: &u8| matches!(byte, b'i' | b'u'))?,
    Suffix::Float => slice.iter().rposition(|byte: &u8| *byte == b'f')?,
  };
  let fits = match suffix {
    Suffix::Int => slice
      .get(letter.saturating_add(1)..)
      .unwrap_or_default()
      .iter()
      .try_fold(0u32, |width: u32, digit: &u8| {
        width
          .checked_mul(10)?
          .checked_add(u32::from(digit.wrapping_sub(b'0')))
      })
      .is_some(),
    Suffix::Float => true,
  };
  let touched = remainder
    .first()
    .is_some_and(|byte: &u8| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_'));
  if fits && !touched {
    return None;
  }
  let past = remainder
    .iter()
    .take_while(|byte: &&u8| byte.is_ascii_alphanumeric() || **byte == b'.')
    .count();
  Some(SuffixRefusal { letter, past })
}
