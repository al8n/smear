use logosky::{
  Logos, Source,
  error::{UnexpectedEot, UnknownLexeme},
  logos::Lexer,
};

use crate::{Lxr, error::StringError};

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_string_error<'a, S, T>(lexer: &mut Lexer<'a, T>) -> Option<StringError<char>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  lexer.slice().as_ref().chars().next().map(|ch| {
    StringError::UnsupportedCharacter(UnknownLexeme::unknown_character(lexer.span().start, ch))
  })
}

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, L, E>(lexer: &mut Lexer<'a, T>) -> E
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: From<UnknownLexeme<char, L>> + From<UnexpectedEot>,
  L: Lxr,
{
  let span = lexer.span();

  let slice = lexer.slice();
  let slice_str = slice.as_ref();

  let mut chars = slice_str.char_indices();

  match chars.next() {
    None => E::from(UnexpectedEot::eot(span.into())),
    Some((idx, first)) => {
      match first {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {}
        _ => {
          return UnknownLexeme::from_char(lexer.span().start + idx, first, L::INIT).into();
        }
      }

      match chars.next() {
        Some((_, _)) => UnknownLexeme::from_range(span, L::INIT).into(),
        None => {
          let start = span.start;
          let mut cur = 0;
          let remaining = lexer.remainder();
          let remaining_str = remaining.as_ref();
          let rem_chars = remaining_str.char_indices();

          for (r_idx, ch) in rem_chars {
            match ch {
              '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                cur = r_idx + 1;
                continue;
              }
              _ => break,
            }
          }

          if cur == 0 {
            return UnknownLexeme::from_char(span.end, first, L::INIT).into();
          }

          lexer.bump(cur);

          UnknownLexeme::from_range(start..span.end + cur, L::INIT).into()
        }
      }
    }
  }
}
