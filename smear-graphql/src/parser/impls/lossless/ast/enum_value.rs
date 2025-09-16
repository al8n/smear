use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, TokenStream, Tokenizer};

use crate::{
  error::{Error, Errors},
  parser::enum_value::EnumValue,
};

use super::*;

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for EnumValue<&'a str> {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E> + Clone
  where
    Self: Sized,
    TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Identifier(name) => match name {
          "true" | "false" | "null" => Err(Error::invalid_enum_value(name, span).into()),
          _ => Ok(EnumValue::new(span, name)),
        },
        tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_enum_value_parser() {
    let parser = EnumValue::parser::<LosslessParserExtra>();
    let input = r#"foo"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
