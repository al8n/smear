use chumsky::{
  extra::ParserExtra,
  input::StrInput,
  label::LabelError,
  prelude::*,
  span::Span,
  text::{Char, TextExpected},
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBrace, LBracket, Quote, RBrace, RBracket, TripleQuote},
  Name, SmearChar, Spanned,
};

use super::*;

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone, derive_more::Unwrap, derive_more::TryUnwrap, derive_more::IsVariant)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum InputValue<Src, Span> {
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  Variable(Variable<Src, Span>),
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(Int<Src, Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(Float<Src, Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(Boolean<Src, Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(String<Src, Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(Null<Src, Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(Enum<Src, Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(List<Self, Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(Object<Self, Src, Span>),
}

impl<Src, Span> InputValue<Src, Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Variable(value) => value.span(),
      Self::Int(value) => value.span(),
      Self::Float(value) => value.span(),
      Self::Boolean(value) => value.span(),
      Self::String(value) => value.span(),
      Self::Null(value) => value.span(),
      Self::Enum(value) => value.span(),
      Self::List(value) => value.span(),
      Self::Object(value) => value.span(),
    }
  }

  /// Returns a parser for the input value.
  ///
  /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    recursive(|value| {
      let boolean_value_parser = Boolean::parser::<I, E>().map(|v| Self::Boolean(v));
      let null_value_parser = Null::parser::<I, E>().map(|v| Self::Null(v));
      let int_value_parser = Int::parser::<I, E>().map(|v| Self::Int(v));
      let float_value_parser = Float::parser::<I, E>().map(|v| Self::Float(v));
      let string_value_parser = String::parser::<I, E>().map(|v| Self::String(v));
      let enum_value_parser = Enum::parser::<I, E>().map(|v| Self::Enum(v));
      let variable_value_parser = Variable::parser::<I, E>().map(|v| Self::Variable(v));
      let object_value_parser =
        Object::parser_with::<I, E, _>(value.clone()).map(|v| Self::Object(v));
      let list_value_parser = List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v));

      choice((
        boolean_value_parser,
        null_value_parser,
        enum_value_parser,
        variable_value_parser,
        string_value_parser,
        float_value_parser,
        int_value_parser,
        list_value_parser,
        object_value_parser,
      ))
      .padded_by(super::ignored::padded())
    })
  }
}

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone, derive_more::Unwrap, derive_more::TryUnwrap, derive_more::IsVariant)]
#[unwrap(ref)]
#[try_unwrap(ref)]
#[non_exhaustive]
pub enum ConstInputValue<Src, Span> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(Int<Src, Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(Float<Src, Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(Boolean<Src, Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(String<Src, Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(Null<Src, Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(Enum<Src, Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(List<Self, Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(Object<Self, Src, Span>),
}

impl<Src, Span> ConstInputValue<Src, Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Int(value) => value.span(),
      Self::Float(value) => value.span(),
      Self::Boolean(value) => value.span(),
      Self::String(value) => value.span(),
      Self::Null(value) => value.span(),
      Self::Enum(value) => value.span(),
      Self::List(value) => value.span(),
      Self::Object(value) => value.span(),
    }
  }

  /// Returns a parser for the input value.
  ///
  /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    recursive(|value| {
      // scalars (whatever you already have)
      let boolean_value_parser = Boolean::parser::<I, E>().map(|v| Self::Boolean(v));
      let null_value_parser = Null::parser::<I, E>().map(|v| Self::Null(v));
      let int_value_parser = Int::parser::<I, E>().map(|v| Self::Int(v));
      let float_value_parser = Float::parser::<I, E>().map(|v| Self::Float(v));
      let string_value_parser = String::parser::<I, E>().map(|v| Self::String(v));
      let enum_value_parser = Enum::parser::<I, E>().map(|v| Self::Enum(v));

      let object_value_parser =
        Object::parser_with::<I, E, _>(value.clone()).map(|v| Self::Object(v));
      let list_value_parser = List::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v));

      choice((
        boolean_value_parser,
        null_value_parser,
        float_value_parser,
        int_value_parser,
        string_value_parser,
        enum_value_parser,
        list_value_parser,
        object_value_parser,
      ))
      .padded_by(super::ignored::padded())
    })
  }
}

#[test]
fn test_boolean_value() {
  let input = "true";
  let InputValue::Boolean(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.value());

  let input = "false";
  let InputValue::Boolean(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(!result.value());
}

#[test]
fn test_null_value() {
  let input = "null";
  let InputValue::Null(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.span().source().eq(&"null"));
}

#[test]
fn test_int_value() {
  let input = "42";
  let InputValue::Int(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.sign().is_none());
  println!("{}", result.span().source());

  let input = "-42";
  let InputValue::Int(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.sign().is_some());
}

#[test]
fn test_float_value() {
  let input = r"
4.123
-4.123
0.123
123e4
123E4
123e-4
123e+4
-1.123e4
-1.123E4
-1.123e-4
-1.123e+4
-1.123e4567  
";

  let expected = input.trim().lines().collect::<Vec<_>>();
  let parser = InputValue::parser::<&str, extra::Err<Simple<char>>>()
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()                                   // <- prevents `()`
    .then_ignore(super::ignored::padded()); // eat trailing ws/comments

  let values = parser.parse(input).into_result().unwrap();
  assert_eq!(values.len(), 12);

  // All should be Float (given our inputs)
  for (v, exp) in values.iter().zip(expected.iter()) {
    match v {
      InputValue::Float(f) => {
        // sanity: float must have fractional and/or exponent
        assert!(f.fractional().is_some() || f.exponent().is_some());
        assert!(f.span().source().eq(exp));
      }
      other => panic!("expected Float, got {:?}", other),
    }
  }
}

#[test]
fn test_string_value() {
  let input = r###"
""
"simple"
" white space "
"unicode \u1234\u5678\u90AB\uCDEF"
"string with \"escaped\" characters"
"string with multiple languages котя, 猫, ねこ, قطة"
"""
block string with unusual whitespaces
a b  c
d

e	f
g  h
ijk﻿l‎‏m
"""
"###;

  let parser = InputValue::parser::<&str, super::super::Error>()
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .then_ignore(super::ignored::padded());

  let values = parser.parse(input).into_result().unwrap();
  assert_eq!(values.len(), 7);

  // All should be String (given our inputs)
  for v in values {
    match v {
      InputValue::String(s) => {
        println!("{}", s.span().source());
        println!("{}", s.content().source());
      }
      other => panic!("expected String, got {:?}", other),
    }
  }
}

#[test]
fn test_enum_value() {
  let input = "SomeEnum";
  let InputValue::Enum(result) = InputValue::parser::<_, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert_eq!(*result.span().source(), "SomeEnum");
}

#[test]
fn test_object_value() {
  let input = "{ a: 1, b: 2 }";
  let InputValue::Object(result) = InputValue::parser::<&str, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("Expected Object");
  };
  assert_eq!(result.fields().len(), 2);

  let ConstInputValue::Object(result) = ConstInputValue::parser::<&str, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("Expected Object");
  };
  assert_eq!(result.fields().len(), 2);

  let input = "{ a: 1, b: { c: \"foo\", d: true } }";
  let InputValue::Object(result) = InputValue::parser::<&str, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("Expected Object");
  };
  assert_eq!(result.fields().len(), 2);
  assert!(result.fields()[0].name().source().eq(&"a"));
  let InputValue::Int(int) = result.fields()[0].value() else {
    panic!("Expected Int");
  };
  assert_eq!(int.digits().source(), &"1");
  assert!(result.fields()[1].name().source().eq(&"b"));
  let InputValue::Object(inner) = result.fields()[1].value() else {
    panic!("Expected Object");
  };
  assert_eq!(inner.fields().len(), 2);
  assert!(inner.fields()[0].name().source().eq(&"c"));
  let InputValue::String(string) = inner.fields()[0].value() else {
    panic!("Expected String");
  };
  assert_eq!(string.content().source(), &"foo");
  assert!(inner.fields()[1].name().source().eq(&"d"));
  let InputValue::Boolean(boolean) = inner.fields()[1].value() else {
    panic!("Expected BoolValue");
  };
  assert!(boolean.value());

  let ConstInputValue::Object(result) = ConstInputValue::parser::<&str, super::super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("Expected Object");
  };
  assert_eq!(result.fields().len(), 2);
  assert!(result.fields()[0].name().source().eq(&"a"));
  let ConstInputValue::Int(int) = result.fields()[0].value() else {
    panic!("Expected Int");
  };
  assert_eq!(int.digits().source(), &"1");
  assert!(result.fields()[1].name().source().eq(&"b"));
  let ConstInputValue::Object(inner) = result.fields()[1].value() else {
    panic!("Expected Object");
  };
  assert_eq!(inner.fields().len(), 2);
  assert!(inner.fields()[0].name().source().eq(&"c"));
  let ConstInputValue::String(string) = inner.fields()[0].value() else {
    panic!("Expected String");
  };
  assert_eq!(string.content().source(), &"foo");
  assert!(inner.fields()[1].name().source().eq(&"d"));
  let ConstInputValue::Boolean(boolean) = inner.fields()[1].value() else {
    panic!("Expected BoolValue");
  };
  assert!(boolean.value());
}
