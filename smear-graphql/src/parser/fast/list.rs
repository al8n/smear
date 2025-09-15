use chumsky::{IterParser as _, Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable, TokenStream, Tokenizer};

use crate::{
  error::{Error, Errors},
  parser::{
    ast::{LBracket, RBracket},
  },
};

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct List<Value, Container = Vec<Value>> {
  values: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Container> List<Value, Container> {
  #[inline]
  pub const fn values(&self) -> &Container {
    &self.values
  }

  #[inline]
  pub fn into_values(self) -> Container {
    self.values
  }
}

impl<'a, V, Container> Parseable<'a, TokenStream<'a, Token<'a>>> for List<V, Container>
where
  V: Parseable<
      'a,
      TokenStream<'a, Token<'a>>,
      Token = Token<'a>,
      Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>,
    >,
  Container: chumsky::container::Container<V>,
{
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    V::parser::<E>()
      .repeated()
      .collect::<Container>()
      .map(|values| Self {
        values,
        _value: core::marker::PhantomData,
      })
      .delimited_by(LBracket::parser(), RBracket::parser())
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::ast::StringValue;

  use super::*;

  #[test]
  fn test_list_parser() {
    let parser = List::<StringValue>::parser::<FastParserExtra>();
    let input = r#"["a", "b", "c"]"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.values().len(), 3);
    assert_eq!(parsed.values()[0].as_str(), "a");
    assert_eq!(parsed.values()[1].as_str(), "b");
    assert_eq!(parsed.values()[2].as_str(), "c");
  }
}
