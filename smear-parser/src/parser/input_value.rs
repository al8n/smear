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

use super::ignored;

pub use boolean::*;
pub use enum_::*;
pub use float::*;
pub use int::*;
pub use list::*;
pub use null::*;
pub use object::*;
pub use string::*;
pub use variable::*;

mod boolean;
mod enum_;
mod float;
mod int;
mod list;
mod null;
mod object;
mod string;
mod variable;

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum InputValue<Src, Span> {
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  Variable(VariableValue<Src, Span>),
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Src, Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Src, Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Src, Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Src, Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Src, Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Src, Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(ListValue<Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(ObjectValue<Src, Span>),
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
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let boolean_value_parser = BooleanValue::parser::<I, E>().map(|v| Self::Boolean(v));
    let null_value_parser = NullValue::parser::<I, E>().map(|v| Self::Null(v));
    let int_value_parser = IntValue::parser::<I, E>().map(|v| Self::Int(v));
    let float_value_parser = FloatValue::parser::<I, E>().map(|v| Self::Float(v));
    let string_value_parser = StringValue::parser::<I, E>().map(|v| Self::String(v));
    let enum_value_parser = EnumValue::parser::<I, E>().map(|v| Self::Enum(v));
    let variable_value_parser = VariableValue::parser::<I, E>().map(|v| Self::Variable(v));

    choice((
      boolean_value_parser,
      null_value_parser,
      enum_value_parser,
      variable_value_parser,
      string_value_parser,
      float_value_parser,
      int_value_parser,
    ))
    .padded_by(super::ignored::ignored())
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
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ConstInputValue<Src, Span> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Src, Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Src, Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Src, Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Src, Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Src, Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Src, Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(ConstListValue<Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(ConstObjectValue<Src, Span>),
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
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let boolean_value_parser = BooleanValue::parser::<I, E>().map(|v| Self::Boolean(v));
    let null_value_parser = NullValue::parser::<I, E>().map(|v| Self::Null(v));
    let int_value_parser = IntValue::parser::<I, E>().map(|v| Self::Int(v));
    let float_value_parser = FloatValue::parser::<I, E>().map(|v| Self::Float(v));
    let string_value_parser = StringValue::parser::<I, E>().map(|v| Self::String(v));
    let enum_value_parser = EnumValue::parser::<I, E>().map(|v| Self::Enum(v));

    choice((
      boolean_value_parser,
      null_value_parser,
      enum_value_parser,
      string_value_parser,
      float_value_parser,
      int_value_parser,
    ))
    .padded_by(super::ignored::ignored())
  }
}

#[test]
fn test_boolean_value() {
  let input = "true";
  let InputValue::Boolean(result) = InputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.value());

  let input = "false";
  let InputValue::Boolean(result) = InputValue::parser::<_, super::Error>()
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
  let InputValue::Null(result) = InputValue::parser::<_, super::Error>()
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
  let InputValue::Int(result) = InputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert!(result.sign().is_none());
  println!("{}", result.span().source());

  let input = "-42";
  let InputValue::Int(result) = InputValue::parser::<_, super::Error>()
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
    .then_ignore(super::ignored::ignored()); // eat trailing ws/comments

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

  let parser = InputValue::parser::<&str, super::Error>()
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .then_ignore(super::ignored::ignored());

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
  let InputValue::Enum(result) = InputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap()
  else {
    panic!("unexpected value");
  };
  assert_eq!(*result.span().source(), "SomeEnum");
}
