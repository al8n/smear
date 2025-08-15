use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
};

use crate::parser::SmearChar;

/// Spec: [LineTerminator](https://spec.graphql.org/draft/#LineTerminator)
pub(super) fn line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
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

/// Spec: [Comment](https://spec.graphql.org/draft/#sec-Comments)
pub(super) fn comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
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

/// Spec: [Whitespace](https://spec.graphql.org/draft/#WhiteSpace)
pub(super) fn white_space<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
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

pub(super) fn bom<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  just(I::Token::bom()).ignored()
}

/// Spec: [IgnoreTokens](https://spec.graphql.org/draft/#Ignored)
pub(super) fn ignored<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
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

#[test]
fn t() {
  let input = "\u{feff} # Hello, world! \t\n";
  let parser = ignored::<_, super::Error>();
  parser.parse(input).into_result().unwrap();
}
