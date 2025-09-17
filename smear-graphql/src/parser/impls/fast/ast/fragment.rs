use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::lang::v2::{self, FragmentName, Name};

use crate::{error::Error, parser::ast};

use super::*;

pub type TypeCondition<S> = v2::TypeCondition<Name<S>>;
pub type FragmentSpread<S, Container = Vec<Directive<S>>> = ast::FragmentSpread<Directives<S, Container>, S>;
// pub type InlineFragment<S, Container = Vec<Directive<S>>> = v2::InlineFragment<TypeCondition<S>, Directives<S, Container>, S>;



impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a>> for FragmentName<&'a str> {
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => match tok {
        Token::Identifier(name) => if name.eq("on") {
          Err(Error::invalid_fragment_name(name, span).into())
        } else {
          Ok(FragmentName::new(span, name))
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
  fn test_name_parser() {
    let parser = FragmentName::parser::<FastParserExtra>();
    let input = r#"foo"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), &Span::new(0, 3));
  }
}
