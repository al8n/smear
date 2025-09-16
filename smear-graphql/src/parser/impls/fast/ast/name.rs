use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};

use crate::{error::Error, parser::name::Name};

use super::*;

impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>> for Name<&'a str> {
  type Error = FastTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = Self::Error> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Identifier(name) => Ok(Name::new(span, name)),
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
  fn test_name_parser() {
    let parser = Name::parser::<FastParserExtra>();
    let input = r#"foo"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 3));
  }
}
