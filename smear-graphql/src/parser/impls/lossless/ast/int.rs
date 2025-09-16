use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::int::IntValue};

use super::*;

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>> for IntValue<&'a str> {
  type Error = LosslessTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = Self::Error> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Int(val) => Ok(Self::new(span, val)),
        tok => Err(Error::unexpected_token(tok, TokenKind::Int, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::lossless::LosslessParserExtra;

  use super::*;

  #[test]
  fn test_int_parser() {
    let parser = IntValue::parser::<LosslessParserExtra>();
    let input = r#"42"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "42");
    assert_eq!(parsed.span(), Span::new(0, 2));
  }
}
