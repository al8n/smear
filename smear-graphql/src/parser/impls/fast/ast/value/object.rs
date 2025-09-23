use chumsky::{Parser, extra::ParserExtra};
use logosky::Parseable;
use smear_parser::lang::minized::{Object, ObjectField};

use crate::{
  error::Error,
  parser::ast::{Colon, Name},
};

use super::*;

// impl<'a, V> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
//   for ObjectField<Name<&'a str>, V>
// where
//   V: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> + 'a,
//   Colon: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
//   where
//     Self: Sized,
//     E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
//   {
//     ObjectField::parser_with(Name::parser(), V::parser())
//   }
// }

impl<'a, V> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for Object<Name<&'a str>, V>
where
  V: Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    Object::parser_with(Name::parser(), V::parser(), |span| {
      Error::unclosed_object(span).into()
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::{
    error::{ErrorData, Unclosed},
    parser::fast::FastParserExtra,
  };

  use super::*;

  #[test]
  fn test_object_field_parser() {
    let parser = ObjectField::<Name<&str>, StringValue<&str>>::parser::<FastParserExtra<&str>>();
    let input = r#"name: "Jane""#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "name");
    assert_eq!(*parsed.value().content(), "Jane");
  }

  #[test]
  fn test_object_parser() {
    let parser = Object::<Name<&str>, StringValue<&str>>::parser::<FastParserExtra<&str>>();
    let input = r#"{a: "a", b: "b", c: "c"}"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.fields().len(), 3);
    assert_eq!(*parsed.fields()[0].value().content(), "a");
    assert_eq!(*parsed.fields()[1].value().content(), "b");
    assert_eq!(*parsed.fields()[2].value().content(), "c");
  }

  #[test]
  fn test_unclosed_object_parser() {
    let parser = Object::<Name<&str>, StringValue<&str>>::parser::<FastParserExtra<&str>>();
    let input = r#"{a: "a", b: "b", c: "c""#;
    let mut parsed = parser
      .parse(FastTokenStream::new(input))
      .into_result()
      .unwrap_err();
    assert_eq!(parsed.len(), 1);
    let mut err = parsed.pop().unwrap();
    assert_eq!(err.len(), 1);
    let err = err.pop().unwrap();
    let data = err.data();
    assert!(matches!(data, ErrorData::Unclosed(Unclosed::Object)));
  }
}
