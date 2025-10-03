use super::{GraphQLNumber, ValidateNumberChar, Lexer, Logos, Source, UnexpectedEnd, UnexpectedLexeme, FloatHint};


#[inline]
pub(in crate::lexer) fn graphql_fractional_error<'a, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  remainder_len: usize,
  remainder: impl Iterator<Item = char>,
) -> E
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  E: From<UnexpectedEnd<FloatHint>> + From<UnexpectedLexeme<char, FloatHint>>,
{
  super::graphql_fractional_error(lexer, remainder_len, remainder, <char as ValidateNumberChar<GraphQLNumber>>::is_ignored_char)
}
