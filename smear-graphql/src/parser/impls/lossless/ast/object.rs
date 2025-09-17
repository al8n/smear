use chumsky::{IterParser as _, Parser, extra::ParserExtra};
use logosky::Parseable;
use smear_parser::source::{IntoComponents, IntoSpan};

use crate::{
  error::Error,
  parser::{
    ast::{Colon, LBrace, Name, Object, RBrace},
    lossless::LosslessTokenErrors,
  },
};

use super::{padded::Padded, *};

/// A single field within a GraphQL input object literal.
///
/// Represents a name-value pair within an object literal, following the
/// GraphQL specification for input object fields. Each field consists of
/// a field name, a colon separator, and a value, with optional whitespace
/// and comments allowed around each component.
///
/// ## Grammar
///
/// ```text
/// ObjectField ::= Name ':' Value
/// ```
///
/// ## Examples
///
/// ```text
/// name: "John"              // String field
/// age: 25                   // Integer field  
/// active: true              // Boolean field
/// tags: ["user", "admin"]   // List field
/// profile: { bio: "..." }   // Nested object field
/// settings: null            // Null field
/// ```
///
/// ## Component Structure
///
/// Each field contains:
/// - **Overall span**: Covers from field name through the value
/// - **Field name**: A GraphQL name identifier
/// - **Colon separator**: The `:` token with its position
/// - **Field value**: The value assigned to this field
#[derive(Debug, Clone)]
pub struct ObjectField<InputValue, S> {
  span: Span,
  name: PaddedRight<Name<S>, S>,
  colon: Colon,
  value: PaddedLeft<InputValue, S>,
}

impl<InputValue, S> AsRef<Span> for ObjectField<InputValue, S> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<InputValue, S> IntoSpan<Span> for ObjectField<InputValue, S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<InputValue, S> IntoComponents for ObjectField<InputValue, S> {
  type Components = (Span, PaddedRight<Name<S>, S>, Colon, PaddedLeft<InputValue, S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, S> ObjectField<InputValue, S> {
  #[inline]
  pub(crate) const fn new(
    span: Span,
    name: PaddedRight<Name<S>, S>,
    colon: Colon,
    value: PaddedLeft<InputValue, S>,
  ) -> Self {
    Self {
      span,
      name,
      colon,
      value,
    }
  }

  /// Returns the source span of the entire field.
  ///
  /// This span covers from the first character of the field name through
  /// the last character of the field value, providing the complete source
  /// location for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the colon separator token.
  ///
  /// This provides access to the `:` character that separates the field
  /// name from its value, including its exact source position. Useful
  /// for syntax highlighting and precise error reporting.
  #[inline]
  pub const fn colon(&self) -> &Colon {
    &self.colon
  }

  /// Returns the field name.
  ///
  /// This provides access to the GraphQL name that identifies this field
  /// within the object. The name follows standard GraphQL identifier rules
  /// and cannot be a reserved keyword.
  #[inline]
  pub const fn name(&self) -> &PaddedRight<Name<S>, S> {
    &self.name
  }

  /// Returns the field value.
  ///
  /// This provides access to the value assigned to this field. The value
  /// can be any valid GraphQL input value type including scalars, enums,
  /// lists, nested objects, or null.
  #[inline]
  pub const fn value(&self) -> &PaddedLeft<InputValue, S> {
    &self.value
  }
}

impl<'a, V> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>> for ObjectField<V, &'a str>
where
  V: Parseable<
      'a,
      LosslessTokenStream<'a>,
      Token<'a>,
      LosslessTokenErrors<'a>,
    > + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a>> + 'a,
  {
    <PaddedRight<Name<&'a str>, &'a str> as Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>>>::parser()
      .then(<Colon as Parseable<
        'a,
        LosslessTokenStream<'a>,
        Token<'a>,
        LosslessTokenErrors<'a>,
      >>::parser())
      .then(padded_left_parser(V::parser()))
      .map_with(|((name, colon), value), exa| Self::new(exa.span(), name, colon, value))
  }
}

impl<'a, V> Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>>
  for Object<Padded<ObjectField<V, &'a str>, &'a str>>
where
  V: Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>> + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, LosslessTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a>> + 'a,
  {
    object_parser(V::parser())
  }
}

pub fn object_field_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, LosslessTokenStream<'a>, ObjectField<V, &'a str>, E> + Clone
where
  E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a>> + 'a,
  VP: Parser<'a, LosslessTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <PaddedRight<Name<&'a str>, &'a str> as Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>>>::parser()
    .then(<Colon as Parseable<
      'a,
      LosslessTokenStream<'a>,
      Token<'a>,
      LosslessTokenErrors<'a>,
    >>::parser())
    .then(padded_left_parser(value_parser))
    .map_with(|((name, colon), value), exa| ObjectField::new(exa.span(), name, colon, value))
}

pub fn object_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, LosslessTokenStream<'a>, Object<Padded<ObjectField<V, &'a str>, &'a str>>, E> + Clone
where
  E: ParserExtra<'a, LosslessTokenStream<'a>, Error = LosslessTokenErrors<'a>> + 'a,
  VP: Parser<'a, LosslessTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <LBrace as Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>>>::parser()
    .then(
      padded::padded_parser(object_field_parser::<V, VP, E>(value_parser))
        .repeated()
        .collect(),
    )
    .then(<RBrace as Parseable<'a, LosslessTokenStream<'a>, Token<'a>, LosslessTokenErrors<'a>>>::parser().or_not())
    .try_map(|((l, values), r), span| match r {
      Some(r) => Ok(Object::new(span, l, values, r)),
      None => Err(Error::unclosed_object(span).into()),
    })
}

#[cfg(test)]
mod tests {
  use crate::{
    error::{ErrorData, Unclosed},
    parser::{ast::StringValue, lossless::LosslessParserExtra},
  };

  use super::*;

  #[test]
  fn test_object_field_parser() {
    let parser = ObjectField::<StringValue<&str>, &str>::parser::<LosslessParserExtra>();
    let input = r#"name: "Jane""#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "name");
    assert_eq!(*parsed.value().content(), "Jane");
  }

  #[test]
  fn test_object_parser() {
    let parser =
      Object::<Padded<ObjectField<StringValue<&str>, &str>, &str>>::parser::<LosslessParserExtra>();
    let input = r#"{
      # A comment
      a: "a",
      # Another comment
      b: "b",
      c: "c", # Trailing comment
    }"#;
    let parsed = parser.parse(LosslessTokenStream::new(input)).unwrap();
    assert_eq!(parsed.fields().len(), 3);
    assert_eq!(*parsed.fields()[0].value().value().content(), "a");
    assert_eq!(*parsed.fields()[1].value().value().content(), "b");
    assert_eq!(*parsed.fields()[2].value().value().content(), "c");
  }

  #[test]
  fn test_unclosed_object_parser() {
    let parser =
      Object::<Padded<ObjectField<StringValue<&str>, &str>, &str>>::parser::<LosslessParserExtra>();
    let input = r#"{a: "a", b: "b", c: "c""#;
    let mut parsed = parser
      .parse(LosslessTokenStream::new(input))
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
