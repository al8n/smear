use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_scaffold::ast as scaffold;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::{DefaultVec, Name};

pub use crate::value::{
  BlockStringValue, BooleanValue, EnumValue, FloatValue, InlineStringValue, IntValue, NullValue,
  StringValue,
};

/// A GraphQL variable value that can appear in queries and mutations.
pub type VariableValue<S> = crate::value::VariableValue<Name<S>>;

/// List value in GraphQL (can contain variables).
pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Name<S>, InputValue<S>, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S> = scaffold::ObjectField<Name<S>, InputValue<S>>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Name<S>, ConstInputValue<S>, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S> = scaffold::ObjectField<Name<S>, ConstInputValue<S>>;

/// GraphQL input value (executable context).
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// Variable reference (e.g., `$userId`).
  Variable(VariableValue<S>),
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<S>),
  /// Integer number.
  Int(IntValue<S>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of values.
  List(scaffold::List<InputValue<S>>),
  /// Object value with named fields.
  Object(scaffold::Object<Name<S>, InputValue<S>>),
}

impl<S> AsSpan<Span> for InputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
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

impl<S> IntoSpan<Span> for InputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
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
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue),
  /// String value (inline or block string).
  String(StringValue<S>),
  /// Floating-point number.
  Float(FloatValue<S>),
  /// Integer number.
  Int(IntValue<S>),
  /// Enum value name.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of constant values.
  List(scaffold::List<ConstInputValue<S>>),
  /// Object value with named fields (all values must be constant).
  Object(scaffold::Object<Name<S>, ConstInputValue<S>>),
}

impl<S> AsSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
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

impl<S> IntoSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
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
