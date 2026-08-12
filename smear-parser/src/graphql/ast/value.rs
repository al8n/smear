use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{DefaultVec, Name};
use crate::graphql::GraphQL;

/// A GraphQL boolean literal.
pub type BooleanValue<S, Span = SimpleSpan> = crate::value::BooleanValue<S, Span, GraphQL>;

/// A GraphQL enum literal.
pub type EnumValue<S, Span = SimpleSpan> = crate::value::EnumValue<S, Span, GraphQL>;

/// A GraphQL floating-point literal.
pub type FloatValue<S, Span = SimpleSpan> = crate::value::FloatValue<S, Span, GraphQL>;

/// A GraphQL integer literal.
pub type IntValue<S, Span = SimpleSpan> = crate::value::IntValue<S, Span, GraphQL>;

/// The GraphQL `null` literal.
pub type NullValue<S, Span = SimpleSpan> = crate::value::NullValue<S, Span, GraphQL>;

/// A GraphQL string literal.
pub type StringValue<S, Span = SimpleSpan> = crate::value::StringValue<S, Span, GraphQL>;

/// A GraphQL inline string literal.
pub type InlineStringValue<S, Span = SimpleSpan> =
  crate::value::InlineStringValue<S, Span, GraphQL>;

/// A GraphQL block string literal.
pub type BlockStringValue<S, Span = SimpleSpan> = crate::value::BlockStringValue<S, Span, GraphQL>;

/// A GraphQL variable value that can appear in queries and mutations.
pub type VariableValue<S, Span = SimpleSpan> = crate::value::VariableValue<Name<S>, Span>;

/// List value in GraphQL (can contain variables).
pub type List<S, I = S, F = S, Container = DefaultVec<InputValue<S, I, F>>> =
  crate::value::List<InputValue<S, I, F>, SimpleSpan, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, I = S, F = S, Container = DefaultVec<ObjectField<S, I, F>>> =
  crate::value::Object<Name<S>, InputValue<S, I, F>, SimpleSpan, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S, I = S, F = S> = crate::value::ObjectField<Name<S>, InputValue<S, I, F>>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, I = S, F = S, Container = DefaultVec<ConstInputValue<S, I, F>>> =
  crate::value::List<ConstInputValue<S, I, F>, SimpleSpan, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, I = S, F = S, Container = DefaultVec<ConstObjectField<S, I, F>>> =
  crate::value::Object<Name<S>, ConstInputValue<S, I, F>, SimpleSpan, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S, I = S, F = S> =
  crate::value::ObjectField<Name<S>, ConstInputValue<S, I, F>>;

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`). Copied type-only from the frozen `graphql/ast/default.rs`.
pub type DefaultInputValue<S, I = S, F = S> =
  crate::value::DefaultInputValue<ConstInputValue<S, I, F>>;

/// GraphQL input value (executable context).
///
/// # The `I` and `F` parameters
///
/// `S` is the source slice every text-bearing leaf keeps. `I` and `F` are the payloads of the
/// `Int` and `Float` leaves, and they **default to `S`** — so `InputValue<S>` is exactly the
/// source-slice value this parser has always produced, spelled the way it has always been
/// spelled.
///
/// They exist because materialisation varies the payload and nothing else:
/// `InputValue<S, i64, f64>` is the same tree with its two numeric leaves converted, which is
/// what the `materialized-numbers` feature's alias set names. Splitting them into two parameters
/// rather than one is a decision made representable: were a narrower integer ever wanted it would
/// be `i32` + `f64`, never `i32` + `f32`, because GraphQL's `Float` is IEEE 754 **double**.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S, I = S, F = S> {
  /// Variable reference (e.g., `$userId`).
  Variable(VariableValue<S>),
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue<S>),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<F>),
  /// Integer number.
  Int(IntValue<I>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of values.
  List(List<S, I, F>),
  /// Object value with named fields.
  Object(Object<S, I, F>),
}

impl<S, I, F> AsSpan<SimpleSpan> for InputValue<S, I, F> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Variable(v) => v.as_span(),
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S, I, F> IntoSpan<SimpleSpan> for InputValue<S, I, F> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Variable(v) => v.into_span(),
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// GraphQL constant input value (schema context).
///
/// `I` and `F` are the `Int` and `Float` payloads and default to `S`, exactly as on
/// [`InputValue`].
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S, I = S, F = S> {
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue<S>),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<F>),
  /// Integer number.
  Int(IntValue<I>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of constant values.
  List(ConstList<S, I, F>),
  /// Object value with named fields (all values must be constant).
  Object(ConstObject<S, I, F>),
}

impl<S, I, F> AsSpan<SimpleSpan> for ConstInputValue<S, I, F> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S, I, F> IntoSpan<SimpleSpan> for ConstInputValue<S, I, F> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}
