use chumsky::{IterParser as _, Parser, extra::ParserExtra};
use logosky::Parseable;
use smear_parser::source::{IntoComponents, IntoSpan};

use crate::{
  error::Error,
  parser::ast::{Colon, LBrace, Name, Object, RBrace},
};

use super::*;

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
#[derive(Debug, Clone, Copy)]
pub struct ObjectField<InputValue, S> {
  span: Span,
  name: Name<S>,
  colon: Colon,
  value: InputValue,
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
  type Components = (Span, Name<S>, Colon, InputValue);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, S> ObjectField<InputValue, S> {
  #[inline]
  pub(crate) const fn new(span: Span, name: Name<S>, colon: Colon, value: InputValue) -> Self {
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
  pub const fn name(&self) -> &Name<S> {
    &self.name
  }

  /// Returns the field value.
  ///
  /// This provides access to the value assigned to this field. The value
  /// can be any valid GraphQL input value type including scalars, enums,
  /// lists, nested objects, or null.
  #[inline]
  pub const fn value(&self) -> &InputValue {
    &self.value
  }
}

impl<'a, V> Parseable<'a, FastTokenStream<'a>> for ObjectField<V, &'a str>
where
  V: Parseable<'a, FastTokenStream<'a>, Token = Token<'a>, Error = FastTokenErrors<'a>> + 'a,
{
  type Token = Token<'a>;
  type Error = FastTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = Self::Error> + 'a,
  {
    object_field_parser(V::parser())
  }
}

impl<'a, V> Parseable<'a, FastTokenStream<'a>> for Object<ObjectField<V, &'a str>>
where
  V: Parseable<'a, FastTokenStream<'a>, Token = Token<'a>, Error = FastTokenErrors<'a>> + 'a,
{
  type Token = Token<'a>;
  type Error = FastTokenErrors<'a>;

  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = Self::Error> + 'a,
  {
    object_parser(V::parser())
  }
}

pub fn object_field_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, FastTokenStream<'a>, ObjectField<V, &'a str>, E> + Clone
where
  E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a>> + 'a,
  VP: Parser<'a, FastTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <Name<&'a str> as Parseable<'a, FastTokenStream<'a>>>::parser()
    .then(<Colon as Parseable<'a, FastTokenStream<'a>>>::parser())
    .then(value_parser)
    .map_with(|((name, colon), value), exa| ObjectField::new(exa.span(), name, colon, value))
}

pub fn object_parser<'a, V, VP, E>(
  value_parser: VP,
) -> impl Parser<'a, FastTokenStream<'a>, Object<ObjectField<V, &'a str>>, E> + Clone
where
  E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a>> + 'a,
  VP: Parser<'a, FastTokenStream<'a>, V, E> + Clone + 'a,
  V: 'a,
{
  <LBrace as Parseable<'a, FastTokenStream<'a>>>::parser()
    .then(
      object_field_parser::<V, VP, E>(value_parser)
        .repeated()
        .collect(),
    )
    .then(<RBrace as Parseable<'a, FastTokenStream<'a>>>::parser().or_not())
    .try_map(|((l, values), r), span| match r {
      Some(r) => Ok(Object::new(span, l, values, r)),
      None => Err(Error::unclosed_object(span).into()),
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
  fn test_object_field_parser() {
    let parser = ObjectField::<StringValue<&str>, &str>::parser::<FastParserExtra>();
    let input = r#"name: "Jane""#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "name");
    assert_eq!(*parsed.value().content(), "Jane");
  }

  #[test]
  fn test_object_parser() {
    let parser = Object::<ObjectField<StringValue<&str>, &str>>::parser::<FastParserExtra>();
    let input = r#"{a: "a", b: "b", c: "c"}"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.fields().len(), 3);
    assert_eq!(*parsed.fields()[0].value().content(), "a");
    assert_eq!(*parsed.fields()[1].value().content(), "b");
    assert_eq!(*parsed.fields()[2].value().content(), "c");
  }

  #[test]
  fn test_unclosed_object_parser() {
    let parser = Object::<ObjectField<StringValue<&str>, &str>>::parser::<FastParserExtra>();
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
