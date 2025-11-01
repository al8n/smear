use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::enum_value::EnumValue};

use super::*;

impl<'a> Parseable<'a, LosslessTokenizer<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for EnumValue<&'a str>
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
          Token::Identifier(name) => match name {
            "true" | "false" | "null" => Err(Error::invalid_enum_value(name, span).into()),
            _ => Ok(EnumValue::new(span, name)),
          },
          tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
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
  fn test_enum_value_parser() {
    let parser = EnumValue::parser::<LosslessParserExtra<&str>>();
    let input = r#"foo"#;
    let parsed = parser.parse(LosslessTokenizer::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), &Span::new(0, 3));
  }
}
