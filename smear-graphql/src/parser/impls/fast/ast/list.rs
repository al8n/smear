use chumsky::{IterParser as _, Parser, extra::ParserExtra};
use logosky::Parseable;

use crate::{
  error::Error,
  parser::ast::{LBracket, List, RBracket},
};

use super::*;

impl<'a, V> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> for List<V>
where
  V: Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    list_parser(V::parser())
  }
}

pub fn list_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, FastTokenStream<'a>, List<V>, E> + Clone
where
  E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  VP: Parser<'a, FastTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <LBracket as Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>>>::parser()
    .then(value_parser.repeated().collect())
    .then(<RBracket as Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>>>::parser().or_not())
    .try_map(|((l, values), r), span| match r {
      Some(r) => Ok(List::new(span, l, r, values)),
      None => Err(Error::unclosed_list(span).into()),
    })
}

#[cfg(test)]
mod tests {
  use crate::{
    error::{ErrorData, Unclosed},
    parser::{ast::StringValue, fast::FastParserExtra},
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
