use super::super::char::Char;

use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
};

/// LineTerminator  ::  <LF> | <CR> [<LF>]
/// Treat CR and CRLF as a single terminator.
pub fn line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((
    just(I::Token::LINE_FEED).ignored(),
    just(I::Token::CARRIAGE_RETURN)
      .then(just(I::Token::LINE_FEED).or_not())
      .ignored(),
  ))
}

/// Comment  ::  '#' CommentChar*
/// CommentChar :: SourceCharacter but not LineTerminator
/// (U+0000 is excluded from SourceCharacter)
pub fn comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
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

/// WhiteSpace  :: U+0009 (TAB) | U+0020 (SPACE)
pub fn white_space<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((just(I::Token::SPACE), just(I::Token::TAB))).ignored()
}

/// Comma is insignificant in GraphQL (treat like whitespace).
pub fn comma<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::COMMA).ignored()
}

/// Unicode BOM — may appear *at the start of the source*.
/// Keep this separate; don't include in general padding.
pub fn bom<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::bom()).ignored()
}

/// Ignored tokens *between* meaningful tokens (no BOM here).
/// Spec: WhiteSpace | LineTerminator | Comment | Comma
pub fn ignored<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((bom(), white_space(), line_terminator(), comment(), comma()))
    .repeated()
    .ignored()
}
