use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_scaffold::ast as scaffold;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{DefaultVec, Name};
use crate::graphql::GraphQL;

/// A GraphQL boolean literal.
pub type BooleanValue<Span = SimpleSpan> = crate::value::BooleanValue<Span, GraphQL>;

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

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`). Copied type-only from the frozen `graphql/ast/default.rs`.
pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;

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

impl<S> AsSpan<SimpleSpan> for InputValue<S> {
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

impl<S> IntoSpan<SimpleSpan> for InputValue<S> {
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

impl<S> AsSpan<SimpleSpan> for ConstInputValue<S> {
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

impl<S> IntoSpan<SimpleSpan> for ConstInputValue<S> {
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
