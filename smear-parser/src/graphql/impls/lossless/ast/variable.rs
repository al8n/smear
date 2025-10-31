use chumsky::{Parser, extra::ParserExtra};
use logosky::Parseable;

use crate::{
  error::{Error, VariableValueHint},
  parser::ast::{Dollar, Name, Variable},
};

use super::*;

impl<'a> Parseable<'a, LosslessTokenizer<'a>, Token<'a>, LosslessTokenErrors<'a, &'a str>>
  for Variable<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenizer<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenizer<'a>, Error = LosslessTokenErrors<'a, &'a str>> + 'a,
  {
    <Dollar as Parseable<
      'a,
      LosslessTokenizer<'a>,
      Token<'a>,
      LosslessTokenErrors<'a, &'a str>,
    >>::parser()
    .or_not()
    .then(
      <Name<&'a str> as Parseable<
        'a,
        LosslessTokenizer<'a>,
        Token<'a>,
        LosslessTokenErrors<'a, &'a str>,
      >>::parser()
      .or_not(),
    )
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
          Error::unexpected_token(Token::Identifier(name.source()), TokenKind::Dollar, span).into(),
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
    let parser = Variable::parser::<LosslessParserExtra<&str>>();
    let input = r#"$foo"#;
    let parsed = parser.parse(LosslessTokenizer::new(input)).unwrap();
    assert_eq!(*parsed.slice(), "$foo");
    assert_eq!(*parsed.name().source(), "foo");
    assert_eq!(parsed.span(), Span::new(0, 4));
  }
}
