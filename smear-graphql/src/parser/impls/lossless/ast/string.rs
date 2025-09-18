use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{
  error::Error,
  parser::string::{Kind, StringValue},
};

use super::*;

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>> for StringValue<&'a str> {
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => Ok(match tok {
        Token::StringLiteral(raw) => {
          StringValue::new(span, raw, raw.trim_matches('"'), Kind::Inline)
        }
        Token::BlockStringLiteral(raw) => {
          StringValue::new(span, raw, raw.trim_matches('"'), Kind::Block)
        }
        tok => return Err(Error::unexpected_token(tok, TokenKind::String, span).into()),
      }),
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::lossless::LosslessParserExtra;

  use super::*;

  #[test]
  fn test_string_value_parser() {
    let parser = StringValue::parser::<LosslessParserExtra<&str>>();
    let input = r#""Hello, World!""#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.content(), "Hello, World!");
    assert_eq!(parsed.kind, Kind::Inline);
    assert_eq!(parsed.span(), Span::new(0, 15));
    assert_eq!(*parsed.raw(), r#""Hello, World!""#);
  }

  #[test]
  fn test_block_string_value_parser() {
    let parser = StringValue::parser::<LosslessParserExtra<&str>>();
    let input = r#""""Hello,
World!""""#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.content(), "Hello,\nWorld!");
    assert_eq!(parsed.kind, Kind::Block);
    assert_eq!(parsed.span(), Span::new(0, 19));
    assert_eq!(
      *parsed.raw(),
      r#""""Hello,
World!""""#
    );
  }
}
