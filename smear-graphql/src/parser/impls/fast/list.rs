use chumsky::{IterParser as _, Parser, extra::ParserExtra};
use logosky::{Parseable, TokenStream};

use crate::{
  error::{Error, Errors},
  parser::ast::{LBracket, List, RBracket},
};

use super::*;

impl<'a, V> Parseable<'a, TokenStream<'a, Token<'a>>> for List<V>
where
  V: Parseable<
      'a,
      TokenStream<'a, Token<'a>>,
      Token = Token<'a>,
      Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>,
    >,
{
  type Token = Token<'a>;
  type Error = Errors<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, TokenStream<'a, Token<'a>>, Self, E>
  where
    Self: Sized,
    E: ParserExtra<'a, TokenStream<'a, Token<'a>>, Error = Self::Error>,
  {
    <LBracket as Parseable<'a, TokenStream<'a, Token<'a>>>>::parser()
      .then(V::parser().repeated().collect())
      .then(<RBracket as Parseable<'a, TokenStream<'a, Token<'a>>>>::parser().or_not())
      .try_map(|((l, values), r), span| match r {
        Some(r) => Ok(Self::new(span, l, r, values)),
        None => Err(Error::unclosed_list(span).into()),
      })
  }
}

#[cfg(test)]
mod tests {
  use crate::{error::ErrorData, parser::ast::StringValue};

  use super::*;

  #[test]
  fn test_list_parser() {
    let parser = List::<StringValue<&str>>::parser::<FastParserExtra>();
    let input = r#"["a", "b", "c"]"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.values().len(), 3);
    assert_eq!(*parsed.values()[0].content(), "a");
    assert_eq!(*parsed.values()[1].content(), "b");
    assert_eq!(*parsed.values()[2].content(), "c");
    assert_eq!(parsed.span(), &Span::new(0, 15));
  }

  #[test]
  fn test_unclosed_list_parser() {
    let parser = List::<StringValue<&str>>::parser::<FastParserExtra>();
    let input = r#"["a", "b", "c""#;
    let mut parsed = parser
      .parse(FastTokenStream::new(input))
      .into_result()
      .unwrap_err();
    assert_eq!(parsed.len(), 1);
    let mut err = parsed.pop().unwrap();
    assert_eq!(err.len(), 1);
    let err = err.pop().unwrap();
    let data = err.data();
    assert!(matches!(data, ErrorData::UnclosedList));
  }
}
