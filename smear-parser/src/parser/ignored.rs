use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
};

use crate::parser::SmearChar;

/// A parser which consumes line terminators
///
/// Spec: [LineTerminator](https://spec.graphql.org/draft/#LineTerminator)
pub fn line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::CARRIAGE_RETURN)
    .then(just(I::Token::LINE_FEED).or_not())
    .ignored()
    .or(just(I::Token::LINE_FEED).ignored())
}

/// Returns a parser which consumes comments
///
/// Spec: [Comment](https://spec.graphql.org/draft/#sec-Comments)
pub fn comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::HASH)
    // CommentChar*: any SourceCharacter except LineTerminator; SourceCharacter excludes U+0000.
    .ignore_then(any().filter(|c: &I::Token| I::Token::LINE_FEED.ne(c) && I::Token::CARRIAGE_RETURN.ne(c) && I::Token::NULL.ne(c)).repeated())
    .ignored()
}

/// Returns a parser which consumes white spaces.
///
/// Spec: [Whitespace](https://spec.graphql.org/draft/#WhiteSpace)
pub fn white_space<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((
    just(I::Token::SPACE),
    just(I::Token::TAB),
    just(I::Token::COMMA),
  ))
  .ignored()
}

/// Returns a parser which consumes byte order marks (BOM)
///
/// Spec: `\u{FEFF}`
pub fn bom<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::bom()).ignored()
}

/// Returns a parser which consumes all ignored tokens
///
/// Spec: [IgnoreTokens](https://spec.graphql.org/draft/#Ignored)
pub fn ignored<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((white_space(), line_terminator(), comment(), bom()))
    .repeated()
    .ignored()
}
