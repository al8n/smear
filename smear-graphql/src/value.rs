use chumsky::{extra::ParserExtra, prelude::*, text::TextExpected, util::MaybeRef};

use super::{
  lang::{
    ignored,
    input_value::{self, *},
    punct::Equal,
  },
  Char, Span,
};

pub type ListValue<Span> = input_value::ListValue<InputValue<Span>, Span>;
pub type ConstListValue<Span> = input_value::ListValue<ConstInputValue<Span>, Span>;

pub type ObjectValue<Span> = input_value::ObjectValue<InputValue<Span>, Span>;
pub type ConstObjectValue<Span> = input_value::ObjectValue<ConstInputValue<Span>, Span>;

/// Input value
///
/// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
///
/// About the generics
/// - `Src`: the corresponding slice of the [`StrInput`](chumsky::input::StrInput)
/// - `Span`: the span type of the input [`StrInput`](chumsky::input::StrInput)
/// - `Container`: the container type for storing the nested type, e.g. list, object, map, set.
#[derive(Debug, Clone, derive_more::Unwrap, derive_more::TryUnwrap, derive_more::IsVariant)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum InputValue<Span> {
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  Variable(Variable<Span>),
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(ListValue<Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(Object<Span>),
}

impl<Span> InputValue<Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Span {
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
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
      .padded_by(ignored())
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
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ConstInputValue<Span> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(IntValue<Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(FloatValue<Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(BooleanValue<Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(StringValue<Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(NullValue<Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(EnumValue<Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(ConstListValue<Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(ConstObject<Span>),
}

impl<Span> ConstInputValue<Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Span {
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
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
        ConstObject::parser_with::<I, E, _>(value.clone()).map(|v| Self::Object(v));
      let list_value_parser =
        ConstList::parser_with::<I, E, _>(value.clone()).map(|v| Self::List(v));

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
      .padded_by(ignored())
    })
  }
}

/// Default input value
#[derive(Debug, Clone, Copy)]
pub struct DefaultInputValue<Span>(input_value::DefaultInputValue<ConstInputValue<Span>, Span>);

impl<Span> DefaultInputValue<Span> {
  /// Returns the span of the default input value
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns a reference to the equal token
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    self.0.eq()
  }

  /// Returns a reference to the value of the default input value.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Span> {
    self.0.value()
  }

  /// Returns a parser of default input value.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    input_value::DefaultInputValue::parser_with(ConstInputValue::parser()).map(Self)
  }
}
