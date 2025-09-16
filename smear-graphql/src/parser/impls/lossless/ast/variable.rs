use chumsky::{Parser, extra::ParserExtra};
use logosky::Parseable;

use crate::{
  error::{Error, VariableValueHint},
  parser::ast::{Dollar, Name, Variable},
};

use super::*;

impl<'a> Parseable<'a, LosslessTokenStream<'a>, Token<'a>> for Variable<&'a str> {
  type Error = LosslessTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = Self::Error> + 'a,
  {
    <Dollar as Parseable<'a, LosslessTokenStream<'a>, Token<'a>>>::parser()
      .or_not()
      .then(<Name<&'a str> as Parseable<'a, LosslessTokenStream<'a>, Token<'a>>>::parser().or_not())
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
  use crate::parser::lossless::LosslessParserExtra;

  use super::*;

  #[test]
  fn test_variable_parser() {
    let parser = Variable::parser::<LosslessParserExtra>();
    let input = r#"$foo"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.slice(), "$foo");
    assert_eq!(*parsed.name().source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 4));
  }
}
