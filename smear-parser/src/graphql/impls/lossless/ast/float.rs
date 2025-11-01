use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::float::FloatValue};

use super::*;

impl<'a> Parseable<'a, LosslessTokenizer<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for FloatValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenizer<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenizer<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          Token::Float(val) => Ok(Self::new(span, val)),
          tok => Err(Error::unexpected_token(tok, TokenKind::Float, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::lossless::LosslessParserExtra;

  use super::*;

  #[test]
  fn test_float_parser() {
    let parser = FloatValue::parser::<LosslessParserExtra<&str>>();
    let input = r#"1.3"#;
    let parsed = parser.parse(LosslessTokenizer::new(input)).unwrap();
    assert_eq!(*parsed.source(), "1.3");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
