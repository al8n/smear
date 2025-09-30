use logosky::{Logos, logos::Lexer};

use crate::error;

pub(super) mod slice;
pub(super) mod str;

#[inline(always)]
pub(super) fn unterminated_spread_operator<'a, C, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), error::LexerError<C, Extras>>
where
  T: Logos<'a>,
{
  Err(error::LexerError::new(
    lexer.span(),
    error::LexerErrorData::UnterminatedSpreadOperator,
  ))
}
