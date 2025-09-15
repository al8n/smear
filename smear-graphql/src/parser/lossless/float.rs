use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, TokenStream, Tokenizer};

use crate::{
  error::{Error, Errors},
  parser::float::Float,
};

use super::*;

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for Float<'a> {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    TokenStream<'a, Token<'a>>: Tokenizer<'a, Self::Token>,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Float(val) => Ok(Float::new(span, val)),
        tok => Err(Error::unexpected_token(tok, TokenKind::Float, span).into()),
      },
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_float_parser() {
    let parser = Float::parser::<LosslessParserExtra>();
    let input = r#"1.3"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(parsed.as_str(), "1.3");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
