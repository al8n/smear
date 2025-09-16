use chumsky::{Parser, extra::ParserExtra};
use logosky::{Parseable, TokenStream};

use crate::{
  error::{Error, Errors, VariableValueHint},
  parser::{name::Name, punctuator::Dollar, variable::Variable},
};

use super::*;

impl<'a> Parseable<'a, TokenStream<'a, Token<'a>>> for Variable<&'a str> {
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    <Dollar as Parseable<'a, TokenStream<'a, Token<'a>>>>::parser()
      .or_not()
      .then(<Name<&'a str> as Parseable<'a, TokenStream<'a, Token<'a>>>>::parser().or_not())
      .try_map_with(|(dollar, name), exa| {
        let span = exa.span();
        let slice = exa.slice();
        match (dollar, name) {
          (None, None) => {
            Err(Error::unexpected_end_of_variable_value(VariableValueHint::Dollar, span).into())
          }
          (Some(_), None) => {
            Err(Error::unexpected_end_of_variable_value(VariableValueHint::Name, span).into())
          }
          (None, Some(name)) => Err(
            Error::unexpected_token(Token::Identifier(name.source()), TokenKind::Dollar, span)
              .into(),
          ),
          (Some(dollar), Some(name)) => Ok(Variable::new(span, slice, dollar, name)),
        }
      })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_variable_parser() {
    let parser = Variable::parser::<FastParserExtra>();
    let input = r#"$foo"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.slice(), "$foo");
    assert_eq!(*parsed.name().source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 4));
  }
}
