use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{
  error::{Error, Errors},
  parser::float::FloatValue,
};

use super::*;

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>> for FloatValue<&'a str> {
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = Self::Error> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Float(val) => Ok(Self::new(span, val)),
        tok => Err(Error::unexpected_token(tok, TokenKind::Float, span).into()),
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
  fn test_float_parser() {
    let parser = FloatValue::parser::<LosslessParserExtra>();
    let input = r#"1.3"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "1.3");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
