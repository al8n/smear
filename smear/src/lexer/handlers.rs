use logosky::{
  Logos, Source, logos::Lexer, utils::{CharSize, Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme, recursion_tracker::RecursionLimiter}
};

use crate::{error::UnterminatedSpreadOperatorError, hints::FloatHint};

pub(super) mod str;
pub(super) mod slice;

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
  Char: Copy + ValidateNumberChar<GraphQLNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
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
  Char: Copy + ValidateNumberChar<GraphQLxNumber>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder,
    unexpected_suffix,
  )
}

#[inline]
pub(super) fn handle_decimal_suffix<'a, Char, Language, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  mut remainder: impl Iterator<Item = Char>,
  unexpected_suffix: impl FnOnce(Lexeme<Char>) -> E,
) -> Result<S::Slice<'a>, E>
where
  Char: Copy + ValidateNumberChar<Language>,
  S: ?Sized + Source,
  T: Logos<'a, Source = S>,
{
  let mut curr = 0;

  match remainder.next() {
    // we have a following character after the float literal, need to report the error
    Some(item) if item.is_first_invalid_char() => {
      // the first char is already consumed and it cannot be a digit,
      curr += 1;

      let span = lexer.span();
      // try to consume the longest invalid sequence,
      // the first char is already consumed and it cannot be a digit,
      // but the following chars can be digits as well
      for ch in remainder {
        if ch.is_following_invalid_char() {
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

#[inline]
pub(super) fn graphql_fractional_error<'a, Char, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
) -> E
where
  Char: Copy + ValidateNumberChar<GraphQLNumber>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<FloatHint>> + From<UnexpectedLexeme<Char, FloatHint>>,
{
  fractional_error(lexer, remainder_len, remainder, is_ignored_char)
}

#[inline]
pub(super) fn fractional_error<'a, Char, Language, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  mut remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
) -> E
where
  Char: Copy + ValidateNumberChar<Language>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<FloatHint>> + From<UnexpectedLexeme<Char, FloatHint>>,
{
  let err = match remainder.next() {
    None => {
      UnexpectedEnd::with_name("float".into(), FloatHint::Fractional).into()
    }
    Some(ch) if is_ignored_char(&ch) => {
      UnexpectedEnd::with_name("float".into(), FloatHint::Fractional).into()
    }
    Some(ch) if ch.is_first_invalid_char() => {
      // The first char is already consumed.
      let mut curr = 1;
      let span = lexer.span();

      for ch in remainder {
        if ch.is_following_invalid_char() {
          curr += 1;
          continue;
        }

        // bump the lexer to the end of the invalid sequence
        lexer.bump(curr);

        let l = if curr == 1 {
          let pc = PositionedChar::with_position(ch, span.end);
          Lexeme::Char(pc)
        } else {
          Lexeme::Span(Span::from(span.end..(span.end + curr)))
        };

        return UnexpectedLexeme::new(l, FloatHint::Fractional).into();
      }

      // we reached the end of remainder

      // bump the lexer to the end of the invalid sequence
      lexer.bump(remainder_len);
      let l = if remainder_len == 1 {
        let pc = PositionedChar::with_position(ch, span.end);
        Lexeme::Char(pc)
      } else {
        Lexeme::Span(Span::from(span.end..(span.end + remainder_len)))
      };

      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    }
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(ch.char_size());

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, FloatHint::Fractional).into()
    }
  };

  err
}

pub(super) trait ValidateNumberChar<Language>: CharSize {
  fn is_ignored_char(&self) -> bool;
  fn is_first_invalid_char(&self) -> bool;
  fn is_following_invalid_char(&self) -> bool;
}

pub(super) struct GraphQLNumber;
pub(super) struct GraphQLxNumber;
pub(super) struct GraphQLxHexNumber;

impl ValidateNumberChar<GraphQLNumber> for char {
  #[inline(always)]
  fn is_ignored_char(&self) -> bool {
    matches!(*self, ' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',')
  }

  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'a'..='z' | 'A'..='Z' | '_' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.')
  }
}

impl ValidateNumberChar<GraphQLxNumber> for char {
  #[inline(always)]
  fn is_ignored_char(&self) -> bool {
    matches!(*self, ' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',')
  }

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
  fn is_ignored_char(&self) -> bool {
    matches!(*self, ' ' | '\t' | '\r' | '\n' | '\u{feff}' | ',')
  }

  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, 'g'..='z' | 'G'..='Z' | '.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, '0'..='9' | 'g'..='z' | 'G'..='Z' | '.')
  }
}

impl ValidateNumberChar<GraphQLNumber> for u8 {
  #[inline(always)]
  fn is_ignored_char(&self) -> bool {
    matches!(*self, b' ' | b'\t' | b'\r' | b'\n' | b',')
  }

  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.')
  }
}

impl ValidateNumberChar<GraphQLxNumber> for u8 {
  #[inline(always)]
  fn is_ignored_char(&self) -> bool {
    matches!(*self, b' ' | b'\t' | b'\r' | b'\n' | b',')
  }

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
  fn is_ignored_char(&self) -> bool {
    matches!(*self, b' ' | b'\t' | b'\r' | b'\n' | b',')
  }

  #[inline(always)]
  fn is_first_invalid_char(&self) -> bool {
    matches!(*self, b'g'..=b'z' | b'G'..=b'Z' | b'.')
  }

  #[inline(always)]
  fn is_following_invalid_char(&self) -> bool {
    matches!(*self, b'0'..=b'9' | b'g'..=b'z' | b'G'..=b'Z' | b'.')
  }
}
