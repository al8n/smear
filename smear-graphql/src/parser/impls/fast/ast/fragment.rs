use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::lang::minized::{self, FragmentName, Name};

use crate::error::Error;

use super::*;

pub type TypeCondition<S> = minized::TypeCondition<Name<S>>;

pub type FragmentSpread<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  Container = Vec<Directive<S, ArgumentContainer>>,
> = minized::FragmentSpread<FragmentName<S>, Directives<S, ArgumentContainer, Container>>;

pub type InlineFragment<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = minized::InlineFragment<
  TypeCondition<S>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
  SelectionSet<S, ArgumentContainer, DirectiveContainer>,
>;

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for FragmentName<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          FastToken::Identifier(name) => {
            if name.eq("on") {
              Err(Error::invalid_fragment_name(name, span).into())
            } else {
              Ok(FragmentName::new(span, name))
            }
          }
          tok => Err(Error::unexpected_token(tok, FastTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::fast::FastParserExtra;

  use super::*;

  #[test]
  fn test_fragment_name_parser() {
    let parser = FragmentName::parser::<FastParserExtra<&str>>();
    let input = r#"foo"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.source(), "foo");
    assert_eq!(parsed.span(), &Span::new(0, 3));
  }

  #[test]
  fn test_type_condition_parser() {
    let parser = TypeCondition::parser::<FastParserExtra<&str>>();
    let input = r#"on foo"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "foo");
    assert_eq!(parsed.span(), &Span::new(0, 6));
  }
}
