use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{
    CharSize, Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme,
    recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
    tracker::{LimitExceeded, Tracker},
  },
};

use crate::error::{BadStateError, UnterminatedSpreadOperatorError};

#[cfg_attr(not(tarpaulin), inline(always))]
fn increase_token<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = Tracker>,
{
  lexer.extras.increase_token();
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn increase_recursion_depth<'a, T, E>(lexer: &mut Lexer<'a, T>) -> Result<(), E>
where
  T: Logos<'a, Extras = RecursionLimiter>,
  E: BadStateError<StateError = RecursionLimitExceeded>,
{
  lexer.extras.increase();

  lexer
    .extras
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn increase_recursion_depth_and_token<'a, T, E>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), E>
where
  T: Logos<'a, Extras = Tracker>,
  E: BadStateError<StateError = LimitExceeded>,
{
  lexer.extras.increase_recursion();
  lexer.extras.increase_token();
  lexer
    .extras
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e))
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn tt_hook_and_then<'a, T, E, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, E>,
) -> Result<O, E>
where
  T: Logos<'a, Extras = Tracker>,
  E: BadStateError<StateError = LimitExceeded>,
{
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e.into()))
    .and_then(|_| {
      f(lexer).inspect(|_| {
        increase_token(lexer);
      })
    })
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn tt_hook_and_then_into_errors<'a, T, E, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> Result<O, E>,
) -> Result<O, E>
where
  T: Logos<'a, Extras = Tracker>,
  E: BadStateError<StateError = LimitExceeded>,
{
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e.into()))
    .and_then(|_| {
      f(lexer).inspect(|_| {
        increase_token(lexer);
      })
    })
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn tt_hook_map<'a, T, E, O>(
  lexer: &mut Lexer<'a, T>,
  f: impl FnOnce(&mut Lexer<'a, T>) -> O,
) -> Result<O, E>
where
  T: Logos<'a, Extras = Tracker>,
  E: BadStateError<StateError = LimitExceeded>,
{
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e.into()))
    .map(|_| {
      increase_token(lexer);
      f(lexer)
    })
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn tt_hook<'a, T, E>(lexer: &mut Lexer<'a, T>) -> Result<(), E>
where
  T: Logos<'a, Extras = Tracker>,
  E: BadStateError<StateError = LimitExceeded>,
{
  lexer
    .extras
    .token()
    .check()
    .map_err(|e| E::bad_state(lexer.span().into(), e.into()))
    .inspect(|_| {
      increase_token(lexer);
    })
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn unterminated_spread_operator_error<'a, T, E>(lexer: &mut Lexer<'a, T>) -> E
where
  T: Logos<'a>,
  E: UnterminatedSpreadOperatorError,
{
  E::unterminated_spread_operator(lexer.span().into())
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn decrease_recursion_depth_and_increase_token<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = Tracker>,
{
  lexer.extras.decrease_recursion();
  // right punctuation also increases the token count
  lexer.extras.increase_token();
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) fn decrease_recursion_depth<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = RecursionLimiter>,
{
  lexer.extras.decrease();
}

#[inline]
pub(super) fn handle_number_suffix<'a, Char, Language, S, T, E>(
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
pub(super) fn lit_float_suffix_error<'a, Char, Language, H, S, T, E>(
  name: &'static str,
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  mut remainder: impl Iterator<Item = Char>,
  is_ignored_char: impl FnOnce(&Char) -> bool,
  hint: impl Fn() -> H,
) -> E
where
  Char: Copy + ValidateNumberChar<Language>,
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<H>> + From<UnexpectedLexeme<Char, H>>,
{
  match remainder.next() {
    None => UnexpectedEnd::with_name(name.into(), hint()).into(),
    Some(ch) if is_ignored_char(&ch) => UnexpectedEnd::with_name(name.into(), hint()).into(),
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

        return UnexpectedLexeme::new(l, hint()).into();
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

      UnexpectedLexeme::new(l, hint()).into()
    }
    Some(ch) => {
      let span = lexer.span();
      lexer.bump(ch.char_size());

      let l = Lexeme::Char(PositionedChar::with_position(ch, span.end));
      UnexpectedLexeme::new(l, hint()).into()
    }
  }
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) const fn is_ignored_char(ch: &char) -> bool {
  matches!(ch, ' ' | '\t' | '\r' | '\n' | '\u{FEFF}' | ',' | '#')
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(super) const fn is_ignored_byte(slice: &[u8], b: &u8) -> bool {
  match b {
    b' ' | b'\t' | b'\r' | b'\n' | b',' | b'#' => true,
    0xEF => {
      // Bom
      slice.len() >= 3 && slice[1] == 0xBB && slice[2] == 0xBF
    }
    _ => false,
  }
}

pub(super) trait ValidateNumberChar<Language>: CharSize {
  fn is_first_invalid_char(&self) -> bool;
  fn is_following_invalid_char(&self) -> bool;
}
