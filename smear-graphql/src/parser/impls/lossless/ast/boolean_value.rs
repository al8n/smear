use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, Tokenizer};

use crate::{error::Error, parser::ast::BooleanValue};

use super::*;

impl<'a> Parseable<'a, LosslessTokenStream<'a>> for BooleanValue<&'a str> {
  type Token = Token<'a>;
  type Error = LosslessTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    LosslessTokenStream<'a>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = Self::Error> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Identifier(ident) => Ok(match ident {
          "true" => BooleanValue::new(span, ident, true),
          "false" => BooleanValue::new(span, ident, false),
          val => return Err(Error::invalid_boolean_value(val, span).into()),
        }),
        tok => Err(Error::unexpected_token(tok, TokenKind::Boolean, span).into()),
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
  fn test_enum_value_parser() {
    let parser = BooleanValue::parser::<LosslessParserExtra>();
    let input = r#"foo"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
