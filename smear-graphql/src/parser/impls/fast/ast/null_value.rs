use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::null::NullValue};

use super::*;

impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> for NullValue<&'a str> {
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Identifier(name) => match name {
          "null" => Ok(NullValue::new(span, name)),
          val => Err(Error::invalid_null_value(val, span).into()),
        },
        tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::fast::FastParserExtra;

  use super::*;

  #[test]
  fn test_null_value_parser() {
    let parser = NullValue::parser::<FastParserExtra<&str>>();
    let input = r#"null"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "null");
    assert_eq!(parsed.span(), Span::new(0, 4));
  }
}
