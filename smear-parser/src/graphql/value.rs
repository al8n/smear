use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::{
    ignored::ignored,
    input_value::{self, *},
    punct::Equal,
  },
  SmearChar, Spanned,
};

pub type List<Src, Span> = input_value::List<InputValue<Src, Span>, Src, Span>;
pub type ConstList<Src, Span> = input_value::List<ConstInputValue<Src, Span>, Src, Span>;

pub type Object<Src, Span> = input_value::Object<InputValue<Src, Span>, Src, Span>;
pub type ConstObject<Src, Span> = input_value::Object<ConstInputValue<Src, Span>, Src, Span>;

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
  List(List<Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(Object<Src, Span>),
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
  List(ConstList<Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(ConstObject<Src, Span>),
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
#[derive(Debug, Clone)]
pub struct DefaultInputValue<Src, Span>(
  input_value::DefaultInputValue<ConstInputValue<Src, Span>, Src, Span>,
);

impl<Src, Span> DefaultInputValue<Src, Span> {
  /// Returns the span of the default input value
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  /// Returns a reference to the equal token
  #[inline]
  pub const fn eq(&self) -> &Equal<Src, Span> {
    self.0.eq()
  }

  /// Returns a reference to the value of the default input value.
  #[inline]
  pub const fn value(&self) -> &ConstInputValue<Src, Span> {
    self.0.value()
  }

  /// Returns a parser of default input value.
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
    input_value::DefaultInputValue::parser_with(ConstInputValue::parser()).map(Self)
  }
}
