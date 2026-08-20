use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::Name;
use crate::{
  graphql::GraphQL,
  value::{Nestable, Nested, Sealed},
};

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
pub type List<S, Container = Nested<InputValue<S>>> =
  crate::value::List<InputValue<S>, SimpleSpan, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, Container = Nested<ObjectField<S>>> =
  crate::value::Object<Name<S>, InputValue<S>, SimpleSpan, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S> = crate::value::ObjectField<Name<S>, InputValue<S>>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, Container = Nested<ConstInputValue<S>>> =
  crate::value::List<ConstInputValue<S>, SimpleSpan, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, Container = Nested<ConstObjectField<S>>> =
  crate::value::Object<Name<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S> = crate::value::ObjectField<Name<S>, ConstInputValue<S>>;

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`). Copied type-only from the frozen `graphql/ast/default.rs`.
pub type DefaultInputValue<S> = crate::value::DefaultInputValue<ConstInputValue<S>>;

/// GraphQL input value (executable context).
///
/// # This enum declares no `Drop`, and that is load-bearing
///
/// Releasing a deeply nested one used to abort the process, one native frame per level. The repair
/// is [`Nested`], the container the `List` and `Object` arms hold their
/// children in — **not** a `Drop` on this enum, which would have cost every by-value `unwrap_*` and
/// `try_unwrap_*` to `E0509`. Everything `derive_more` generated before is generated now.
///
/// [`Nestable`] below is how the release reaches this enum's children.
///
/// **This tree carries the repair even though the issue that prompted it was raised against the
/// materialised twin**, and the reason is that the defect is a property of the shape rather than of
/// the payload: this enum nests through the same `List` and `Object` carriers, holds them the same
/// way, and is just as available to a `graphql_proto::Values` driver — nothing anywhere selects the
/// materialised tree for that job.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// Variable reference (e.g., `$userId`).
  Variable(VariableValue<S>),
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue<S>),
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
  List(List<S>),
  /// Object value with named fields.
  Object(Object<S>),
}

impl<S> Sealed for InputValue<S> {}

impl<S> Nestable for InputValue<S> {
  type Node = Self;

  #[inline]
  fn into_children(self, pending: &mut std::vec::Vec<Self>) {
    match self {
      // A leaf owns no value, so it is released here rather than put on the worklist.
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => pending.extend(list.into_values()),
      // A field is `(span, name, value)` and only the value can nest; the name is a leaf and is
      // released here.
      Self::Object(object) => pending.extend(
        object
          .into_fields()
          .into_iter()
          .map(|field| field.into_components().2),
      ),
    }
  }
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
///
/// The `From` conversions are how the shared value productions build this tree: a production
/// names the leaf it just read and converts, so one body serves both this tree and its
/// materialised twin. [`InputValue`] has derived them since it was written, and this enum gaining
/// them is the **only** change the materialisation axis makes to this file. It is additive.
///
/// Like [`InputValue`], it declares no `Drop`; the release is [`Nested`]'s.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// Boolean value (`true` or `false`).
  Boolean(BooleanValue<S>),
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
  List(ConstList<S>),
  /// Object value with named fields (all values must be constant).
  Object(ConstObject<S>),
}

impl<S> Sealed for ConstInputValue<S> {}

impl<S> Nestable for ConstInputValue<S> {
  type Node = Self;

  #[inline]
  fn into_children(self, pending: &mut std::vec::Vec<Self>) {
    match self {
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => pending.extend(list.into_values()),
      Self::Object(object) => pending.extend(
        object
          .into_fields()
          .into_iter()
          .map(|field| field.into_components().2),
      ),
    }
  }
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
