use logosky::{
  Logos, Source, logos::Lexer, utils::{Lexeme, PositionedChar, Span, recursion_tracker::RecursionLimiter}
};

use crate::error::UnterminatedSpreadOperatorError;

#[inline(always)]
pub(super) fn unterminated_spread_operator_error<'a, T, E>(
  lexer: &mut Lexer<'a, T>,
) -> E
where
  T: Logos<'a>,
  E: UnterminatedSpreadOperatorError,
{
  E::unterminated_spread_operator(lexer.span().into())
}

#[inline(always)]
pub(super) fn decrease_recursion_depth<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.decrease();
}

#[inline]
pub(super) fn handle_graphql_decimal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + NumberCharValidator<GraphQLNumber, Char = Char>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
    Char::is_first_invalid_char,
    Char::is_following_invalid_char,
    unexpected_suffix,
  )
}

#[inline]
pub(super) fn handle_graphqlx_decimal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + NumberCharValidator<GraphQLxNumber, Char = Char>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
    Char::is_first_invalid_char,
    Char::is_following_invalid_char,
    unexpected_suffix,
  )
}

#[inline]
pub(super) fn handle_decimal_suffix<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  mut remainder: impl Iterator<Item = Char>,
  first_invalid_char: impl FnOnce(Char) -> bool,
  following_invalid_char: impl Fn(Char) -> bool,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  let mut curr = 0;

  match remainder.next() {
    // we have a following character after the float literal, need to report the error
    Some(item) if first_invalid_char(item) => {
      // the first char is already consumed and it cannot be a digit,
      curr += 1;

      let span = lexer.span();
      // try to consume the longest invalid sequence,
      // the first char is already consumed and it cannot be a digit,
      // but the following chars can be digits as well
      for ch in remainder {
        if following_invalid_char(ch) {
          curr += 1;
          continue;
        }

        // bump the lexer to the end of the invalid sequence
        lexer.bump(curr);

        let l = if curr == 1 {
          // only one invalid char
          let pc = PositionedChar::with_position(item, span.end);
          Lexeme::Char(pc)
        } else {
          Lexeme::Span(Span::from(span.end..(span.end + curr)))
        };
        return Err(unexpected_suffix(l));
      }

      // we reached the end of remainder
      // bump the lexer to the end of the invalid sequence
      lexer.bump(remainder_len);

      let l = if remainder_len == 1 {
        let pc = PositionedChar::with_position(item, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + remainder_len)))
      };

      // return the range of the invalid sequence
      Err(unexpected_suffix(l))
    }
    // For other characters, just return the float literal
    Some(_) | None => Ok(lexer.slice()),
  }
}

trait NumberCharValidator<Language> {
  type Char;

  fn is_first_invalid_char(ch: Self::Char) -> bool;
  fn is_following_invalid_char(ch: Self::Char) -> bool;
}

struct GraphQLNumber;
struct GraphQLxNumber;

impl NumberCharValidator<GraphQLNumber> for char {
  type Char = char;

  #[inline(always)]
  fn is_first_invalid_char(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(ch: char) -> bool {
    matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.')
  }
}

impl NumberCharValidator<GraphQLxNumber> for char {
  type Char = char;

  #[inline(always)]
  fn is_first_invalid_char(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(ch: char) -> bool {
    matches!(ch, '0'..='9' | 'a'..='z' | 'A'..='Z' | '.')
  }
}

impl NumberCharValidator<GraphQLNumber> for u8 {
  type Char = u8;

  #[inline(always)]
  fn is_first_invalid_char(ch: u8) -> bool {
    matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(ch: u8) -> bool {
    matches!(ch, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }
}

impl NumberCharValidator<GraphQLxNumber> for u8 {
  type Char = u8;

  #[inline(always)]
  fn is_first_invalid_char(ch: u8) -> bool {
    matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(ch: u8) -> bool {
    matches!(ch, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.')
  }
}

