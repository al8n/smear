use chumsky::{Parser, extra::ParserExtra};
use logosky::Parseable;
use smear_parser::lang::minized::List;

use crate::error::Error;

use super::*;

impl<'a, V, Container>
  Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for List<V, Container>
where
  V: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> + 'a,
  Container: chumsky::container::Container<V> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    Self::parser_with(V::parser(), |span| Error::unclosed_list(span).into())
  }
}

// pub fn list_parser<'src, I, T, Error, V, Container, VP, E>(
//   value_parser: VP,
//   on_missing_rbracket: impl Fn(Span) -> Error + Clone + 'src,
// ) -> impl Parser<'src, I, List<V, Container>, E> + Clone
// where
//   T: Token<'src>,
//   I: Tokenizer<'src, T, Slice = <T::Source as Source>::Slice<'src>>,
//   Error: 'src,
//   E: ParserExtra<'src, I, Error = Error> + 'src,
//   VP: Parser<'src, I, V, E> + Clone + 'src,
//   Container: chumsky::container::Container<V>,
//   LBracket: Parseable<I, T, Error>,
//   RBracket: Parseable<I, T, Error>,
//   V: 'src,
// {
//   <LBracket as Parseable<I, T, Error>>::parser()
//     .ignore_then(value_parser.repeated().collect())
//     .then(<RBracket as Parseable<I, T, Error>>::parser().or_not())
//     .try_map(move |(values, r), span| match r {
//       Some(_) => Ok(List::new(span, values)),
//       None => Err(on_missing_rbracket(span)),
//     })
// }

#[cfg(test)]
mod tests {
  use logosky::utils::Span;

  use crate::{
    error::{ErrorData, Unclosed},
    parser::fast::FastParserExtra,
  };

  use super::*;

  #[test]
  fn test_list_parser() {
    let parser = List::<StringValue<&str>>::parser::<FastParserExtra<&str>>();
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
    let parser = List::<StringValue<&str>>::parser::<FastParserExtra<&str>>();
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
    assert!(matches!(data, ErrorData::Unclosed(Unclosed::List)));
  }
}
