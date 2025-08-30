use crate::{
  lexer::{Lexer, LexerError, State, Text, Token, TokenKind},
  source::*,
};

use chumsky::{extra::ParserExtra, prelude::*};

/// LineTerminator  ::  <LF> | <CR> [<LF>]
/// Treat CR and CRLF as a single terminator.
pub fn line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  choice((
    just(I::Token::LINE_FEED).ignored(),
    just(I::Token::CARRIAGE_RETURN)
      .then(just(I::Token::LINE_FEED).or_not())
      .ignored(),
  ))
}

///a
pub fn line_terminator1<'src, I, T, S, E>() -> impl Parser<'src, Lexer<'src, I, T, S>, T, E> + Clone
where
  I: Text<'src>,
  T: Token<'src, I, S>,
  S: State + 'src,
  E: ParserExtra<'src, Lexer<'src, I, T, S>>,
  E::Error: From<LexerError<'src, I, T, S>>,
{
  any()
    .try_map(|res, _| {
      match res {
        Ok(tok) => LexerError::check_token_kind(tok, <T::Kind as TokenKind>::line_terminator()).map_err(Into::into),
        Err(e) => Err(E::Error::from(e)),
      }
    })
}

/// Comment  ::  '#' CommentChar*
/// CommentChar :: SourceCharacter but not LineTerminator
/// (U+0000 is excluded from SourceCharacter)
pub fn comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  just(I::Token::HASH)
    .ignore_then(
      // consume until CR/LF/NULL or end of input
      any()
        .filter(|t: &I::Token| {
          *t != I::Token::LINE_FEED && *t != I::Token::CARRIAGE_RETURN && *t != I::Token::NULL
        })
        .repeated(),
    )
    .ignored()
}

/// j
pub fn comment2<'src, I, E>() -> impl Parser<'src, I, I::Slice, E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  just(I::Token::HASH)
    .ignore_then(
      // consume until CR/LF/NULL or end of input
      any()
        .filter(|t: &I::Token| {
          *t != I::Token::LINE_FEED && *t != I::Token::CARRIAGE_RETURN && *t != I::Token::NULL
        })
        .repeated(),
    )
    .to_slice()
}

/// Comment  ::  '#' CommentChar*
/// CommentChar :: SourceCharacter but not LineTerminator
/// (U+0000 is excluded from SourceCharacter)
pub fn comment1<'src, I, T, S, E>() -> impl Parser<'src, Lexer<'src, I, T, S>, T, E> + Clone
where
  I: Text<'src>,
  T: Token<'src, I, S>,
  S: State + 'src,
  E: ParserExtra<'src, Lexer<'src, I, T, S>>,
  E::Error: From<LexerError<'src, I, T, S>>,
{
  any()
    .try_map(|res, _| {
      match res {
        Ok(tok) => LexerError::check_token_kind(tok, <T::Kind as TokenKind>::comment()).map_err(Into::into),
        Err(e) => Err(E::Error::from(e)),
      }
    }) 
}

/// WhiteSpace  :: U+0009 (TAB) | U+0020 (SPACE)
pub fn white_space<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  choice((just(I::Token::SPACE), just(I::Token::TAB))).ignored()
}

/// Comma is insignificant in GraphQL (treat like whitespace).
pub fn comma<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  just(I::Token::COMMA).ignored()
}

/// Unicode BOM — may appear *at the start of the source*.
/// Keep this separate; don't include in general padding.
pub fn bom<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  just(I::Token::bom()).ignored()
}

/// Ignored tokens *between* meaningful tokens (no BOM here).
/// Spec: WhiteSpace | LineTerminator | Comment | Comma
pub fn ignored<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
{
  choice((bom(), white_space(), line_terminator(), comment(), comma()))
    .repeated()
    .ignored()
}

/// Ignored tokens *between* meaningful tokens (no BOM here).
/// Spec: WhiteSpace | LineTerminator | Comment | Comma
pub fn ignored1<'src, I, T, S, E>() -> impl Parser<'src, Lexer<'src, I, T, S>, (), E> + Clone
where
  I: Text<'src>,
  T: Token<'src, I, S>,
  S: State + 'src,
  E: ParserExtra<'src, Lexer<'src, I, T, S>>,
  E::Error: From<LexerError<'src, I, T, S>>,
{
  any()
    .try_map(|res, _| {
      match res {
        Ok(tok) => LexerError::check_token_kind(tok, <T::Kind as TokenKind>::ignored()).map_err(Into::into),
        Err(e) => Err(E::Error::from(e)),
      }
    })
    .ignored()
}
