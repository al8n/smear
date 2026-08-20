use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{DefaultVec, Name};
use crate::{
  graphql::GraphQL,
  value::{Unnest, push_nesting, release},
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
pub type List<S, Container = DefaultVec<InputValue<S>>> =
  crate::value::List<InputValue<S>, SimpleSpan, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, Container = DefaultVec<ObjectField<S>>> =
  crate::value::Object<Name<S>, InputValue<S>, SimpleSpan, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S> = crate::value::ObjectField<Name<S>, InputValue<S>>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  crate::value::List<ConstInputValue<S>, SimpleSpan, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  crate::value::Object<Name<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S> = crate::value::ObjectField<Name<S>, ConstInputValue<S>>;

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`). Copied type-only from the frozen `graphql/ast/default.rs`.
pub type DefaultInputValue<S> = crate::value::DefaultInputValue<ConstInputValue<S>>;

/// GraphQL input value (executable context).
///
/// # The by-value `unwrap_*` forms are gone, and a nested value is why
///
/// `#[unwrap(ref, ref_mut)]` and `#[try_unwrap(ref, ref_mut)]` are repeated on **every variant**
/// rather than only on the enum, which is how `derive_more` is told to stop generating the owned
/// `unwrap_list(self) -> …` and `try_unwrap_list(self) -> Result<…, Self>` pairs. Nothing else
/// about the variant set or the shape changed, and `unwrap_list_ref`, `unwrap_list_mut`,
/// `try_unwrap_list_ref` and `is_list` are generated exactly as before.
///
/// This enum has a hand-written [`Drop`] — see [`nesting`](crate::value::nesting) for the process
/// abort it removes — and Rust does not let a payload be moved out of a type that implements
/// `Drop` (`E0509`).
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
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Variable(VariableValue<S>),
  /// Boolean value (`true` or `false`).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(BooleanValue<S>),
  /// String value (inline or block string).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(StringValue<S>),
  /// Floating-point number.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue<S>),
  /// Integer number.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<S>),
  /// Enum value name.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(EnumValue<S>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(NullValue<S>),
  /// List of values.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(List<S>),
  /// Object value with named fields.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Object(Object<S>),
}

impl<S> Unnest for InputValue<S> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(self, Self::List(_) | Self::Object(_))
  }

  fn unnest(&mut self, pending: &mut std::vec::Vec<Self>) {
    match self {
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Object(object) => push_nesting(
        pending,
        object
          .fields_mut()
          .drain(..)
          .map(|field| field.into_components().2),
      ),
    }
  }
}

impl<S> Drop for InputValue<S> {
  #[inline]
  fn drop(&mut self) {
    release(self);
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
  /// The same span the borrowing accessor answers, copied out.
  ///
  /// It used to reach the span by matching `self` by value and moving each payload out, which
  /// `E0509` forbids now that this enum has a [`Drop`]. [`SimpleSpan`] is [`Copy`], so the answer
  /// is identical and the value is released on the way out exactly as it was before.
  #[inline]
  fn into_span(self) -> SimpleSpan {
    *self.as_span()
  }
}

/// GraphQL constant input value (schema context).
///
/// The `From` conversions are how the shared value productions build this tree: a production
/// names the leaf it just read and converts, so one body serves both this tree and its
/// materialised twin. [`InputValue`] has derived them since it was written, and this enum gaining
/// them is the **only** change the materialisation axis makes to this file. It is additive.
///
/// The per-variant `unwrap` attributes and the [`Drop`] below are [`InputValue`]'s, for the reason
/// stated there.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// Boolean value (`true` or `false`).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(BooleanValue<S>),
  /// String value (inline or block string).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(StringValue<S>),
  /// Floating-point number.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue<S>),
  /// Integer number.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<S>),
  /// Enum value name.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(EnumValue<S>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(NullValue<S>),
  /// List of constant values.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(ConstList<S>),
  /// Object value with named fields (all values must be constant).
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Object(ConstObject<S>),
}

impl<S> Unnest for ConstInputValue<S> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(self, Self::List(_) | Self::Object(_))
  }

  fn unnest(&mut self, pending: &mut std::vec::Vec<Self>) {
    match self {
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Object(object) => push_nesting(
        pending,
        object
          .fields_mut()
          .drain(..)
          .map(|field| field.into_components().2),
      ),
    }
  }
}

impl<S> Drop for ConstInputValue<S> {
  #[inline]
  fn drop(&mut self) {
    release(self);
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
  /// [`InputValue`]'s body, for the same reason.
  #[inline]
  fn into_span(self) -> SimpleSpan {
    *self.as_span()
  }
}
