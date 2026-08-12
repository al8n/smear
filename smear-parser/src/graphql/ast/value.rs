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

// ── The carrier: one value tree, generic over what its two numeric leaves carry ───────────────
//
// Everything below the carrier is an alias over it. There are two alias sets — the slice set in
// this module and the `i64`/`f64` set in `materialized` — and adding a third would declare no
// type either. That is what "materialisation varies the payload and nothing else" means at the
// level of the AST.

/// The GraphQL input-value tree (executable context), generic over its two numeric payloads.
///
/// `S` is the source slice every text-bearing leaf keeps. `I` and `F` are the payloads of the
/// `Int` and `Float` leaves. The two instantiations that exist are named by two alias sets, and
/// neither declares a type: [`InputValue<S>`] is `InputValueOf<S, S, S>`, the source-slice value
/// this parser has always produced, and `materialized::InputValue<S>` is
/// `InputValueOf<S, i64, f64>`, the same tree with its two numeric leaves converted.
///
/// # Why the payloads live on a separate name
///
/// Because defaulting them onto [`InputValue`] itself is not source-compatible, in two ways this
/// workspace has no caller to notice:
///
/// * a default does **not** constrain an otherwise-unbound parameter during *variant
///   construction*, so `InputValue::String(parsed)` written with no annotation stops compiling
///   — the payloads of the `String` variant say nothing about `I` or `F`;
/// * inserting `I` and `F` ahead of [`List`]'s `Container` argument leaves `List<S, MyContainer>`
///   compiling and **meaning something else**, which is a reinterpretation no consumer gets to
///   see.
///
/// Carrying all three parameters here keeps both unwritable: the alias sets below hold the arity
/// and the argument positions they have always published, and `value_parameters_are_source_compatible`
/// in `smear-smoke` compiles both shapes across a real dependency edge.
///
/// # Why two payload parameters and not one
///
/// A decision made representable: were a narrower integer ever wanted it would be `i32` + `f64`,
/// never `i32` + `f32`, because GraphQL's `Float` is IEEE 754 **double**.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValueOf<S, I, F> {
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
  List(ListOf<S, I, F>),
  /// Object value with named fields.
  Object(ObjectOf<S, I, F>),
}

impl<S, I, F> AsSpan<SimpleSpan> for InputValueOf<S, I, F> {
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

impl<S, I, F> IntoSpan<SimpleSpan> for InputValueOf<S, I, F> {
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

/// The GraphQL constant input-value tree (schema context), generic over its two numeric payloads.
///
/// `I` and `F` are the `Int` and `Float` payloads, exactly as on [`InputValueOf`], and the same
/// two alias sets name the same two instantiations.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValueOf<S, I, F> {
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
  List(ConstListOf<S, I, F>),
  /// Object value with named fields (all values must be constant).
  Object(ConstObjectOf<S, I, F>),
}

impl<S, I, F> AsSpan<SimpleSpan> for ConstInputValueOf<S, I, F> {
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

impl<S, I, F> IntoSpan<SimpleSpan> for ConstInputValueOf<S, I, F> {
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

/// A list value over [`InputValueOf`], with the numeric payloads it carries.
pub type ListOf<S, I, F, Container = DefaultVec<InputValueOf<S, I, F>>> =
  crate::value::List<InputValueOf<S, I, F>, SimpleSpan, Container>;

/// An object value over [`InputValueOf`], with the numeric payloads it carries.
pub type ObjectOf<S, I, F, Container = DefaultVec<ObjectFieldOf<S, I, F>>> =
  crate::value::Object<Name<S>, InputValueOf<S, I, F>, SimpleSpan, Container>;

/// One object field over [`InputValueOf`], with the numeric payloads it carries.
pub type ObjectFieldOf<S, I, F> = crate::value::ObjectField<Name<S>, InputValueOf<S, I, F>>;

/// A constant list value over [`ConstInputValueOf`], with the numeric payloads it carries.
pub type ConstListOf<S, I, F, Container = DefaultVec<ConstInputValueOf<S, I, F>>> =
  crate::value::List<ConstInputValueOf<S, I, F>, SimpleSpan, Container>;

/// A constant object value over [`ConstInputValueOf`], with the numeric payloads it carries.
pub type ConstObjectOf<S, I, F, Container = DefaultVec<ConstObjectFieldOf<S, I, F>>> =
  crate::value::Object<Name<S>, ConstInputValueOf<S, I, F>, SimpleSpan, Container>;

/// One constant object field over [`ConstInputValueOf`], with the numeric payloads it carries.
pub type ConstObjectFieldOf<S, I, F> =
  crate::value::ObjectField<Name<S>, ConstInputValueOf<S, I, F>>;

/// A default value over [`ConstInputValueOf`], with the numeric payloads it carries.
pub type DefaultInputValueOf<S, I, F> = crate::value::DefaultInputValue<ConstInputValueOf<S, I, F>>;

// ── The slice alias set: the arity and the argument positions this parser has always published ─

/// GraphQL input value (executable context).
///
/// The source-slice instantiation of [`InputValueOf`]: every leaf, numeric ones included, holds
/// the literal's own bytes.
pub type InputValue<S> = InputValueOf<S, S, S>;

/// GraphQL constant input value (schema context).
///
/// The source-slice instantiation of [`ConstInputValueOf`].
pub type ConstInputValue<S> = ConstInputValueOf<S, S, S>;

/// List value in GraphQL (can contain variables).
pub type List<S, Container = DefaultVec<InputValue<S>>> = ListOf<S, S, S, Container>;

/// Object value in GraphQL (can contain variables).
pub type Object<S, Container = DefaultVec<ObjectField<S>>> = ObjectOf<S, S, S, Container>;

/// Object field in GraphQL (can contain variables).
pub type ObjectField<S> = ObjectFieldOf<S, S, S>;

/// Constant list value in GraphQL (no variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> = ConstListOf<S, S, S, Container>;

/// Constant object value in GraphQL (no variables).
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  ConstObjectOf<S, S, S, Container>;

/// Constant object field in GraphQL (no variables).
pub type ConstObjectField<S> = ConstObjectFieldOf<S, S, S>;

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`). Copied type-only from the frozen `graphql/ast/default.rs`.
pub type DefaultInputValue<S> = DefaultInputValueOf<S, S, S>;
