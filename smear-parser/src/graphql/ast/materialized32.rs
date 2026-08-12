//! The specification's own reading of `Int`: **the same productions again, at [`i32`].**
//!
//! [`ast::materialized`](crate::graphql::ast::materialized) laid out two enums and seven aliases
//! with `Int` carrying [`i64`]. This module is that module at the width GraphQL specifies, and
//! everything its header argues — two nominal enums rather than one carrier at two
//! instantiations, `Int` and `Float` materialised and nothing else, no `OnceCell`, the
//! parse-error bound accepted rather than engineered around — holds here unchanged and is not
//! repeated. What follows is only what is different.
//!
//! # `i32` is the exact reading, not the narrow one
//!
//! Draft §3.5.1 defines `Int` as a **signed 32-bit integer**. A `2147483648` in a document is a
//! well-formed `IntValue` under draft §2.9.1's grammar and is not a value any conformant `Int`
//! can hold, so the two trees answer two different questions:
//!
//! * this one — *what does this document mean under the specification?* A literal outside `i32`
//!   is refused here, which is the specification's own answer arriving at parse time instead of
//!   at coercion time.
//! * [`ast::materialized`](crate::graphql::ast::materialized) — *what did the author write?* It
//!   accepts every literal an `i64` can hold, including the ones §3.5.1 does not admit.
//!
//! Neither is a subset of "correct". A consumer that has to tell the two apart reads
//! [`IntWidth`](crate::graphql::error::IntWidth) off the error, which is why that error names the
//! width rather than saying only that an integer overflowed.
//!
//! # The `32` is the integer and nothing else
//!
//! [`FloatValue`] is [`f64`] here exactly as it is next door. GraphQL's `Float` **is** IEEE 754
//! double precision (draft §3.5.2), so an `f32` leaf would be non-conformant rather than
//! narrower — the sibling module's header says `f32` must not follow `i32` down, and this module
//! is where that sentence is kept.
//!
//! # Why a second module and not a payload parameter
//!
//! The obvious economy — one `InputValue<S, I>` with `I` at [`i32`] or [`i64`], four enums
//! collapsing to two — is the shape this axis has already been through once. **Both spellings of
//! it were applied to this tree and compiled, and each failed differently**; neither reading below
//! is an argument from taste:
//!
//! * **without a default**, the parameter breaks every site that names the tree by its published
//!   arity: `cargo check -p smear-parser --all-features` answers `E0107` twelve times — four at
//!   the enums, eight at the aliases — before a consumer is reached at all. `smear-smoke`'s
//!   `arities` probe is written against precisely that shape one level further out.
//! * **with a default**, `smear-parser` still compiles and the damage moves to the dependent:
//!   `smear-smoke` fails with `E0308` eight times, all four container-alias probes, because
//!   `List<S, Own<InputValue<S>>>` now binds the dependent's own container type to the *payload*
//!   parameter — `expected Vec<InputValue<&str, Own<_>>>, found Own<_>`. Nothing warns at the
//!   declaration; the reinterpretation is silent until somebody writes the second argument.
//! * **as an alias over one carrier** — the version that would collapse the enums entirely — a
//!   variant import stops resolving: `use InputValue::{Int, String}` is `E0432` against a type
//!   alias and compiles against an `enum`. That is the property `smear-smoke`'s
//!   `value_variant_namespace` module exists to hold, and it is why the trees are nominal.
//!
//! Two more enums cost a variant list written twice more, and
//! `every_value_tree_declares_the_same_variants` charges that cost to a census rather than to the
//! next reader.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  BooleanValue as AstBooleanValue, DefaultVec, EnumValue as AstEnumValue, Name,
  NullValue as AstNullValue, StringValue as AstStringValue, VariableValue as AstVariableValue,
};

/// A GraphQL integer literal, materialised as [`i32`] — draft §3.5.1's own width.
pub type IntValue<Span = SimpleSpan> = super::IntValue<i32, Span>;

/// A GraphQL floating-point literal, materialised as [`f64`].
///
/// The same type [`materialized::FloatValue`](super::materialized::FloatValue) is, and named
/// again here so this module publishes a whole tree rather than most of one.
pub type FloatValue<Span = SimpleSpan> = super::FloatValue<f64, Span>;

/// A GraphQL input value (executable context) whose numeric leaves are materialised at `i32`.
///
/// Variant for variant [`ast::InputValue`](crate::graphql::ast::InputValue), with `Int` and
/// `Float` carrying [`i32`] and [`f64`]. Every other leaf is the *same type* the slice tree
/// holds, not a copy of it.
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// Variable reference (e.g., `$userId`).
  Variable(AstVariableValue<S>),
  /// Boolean value (`true` or `false`).
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  Float(FloatValue),
  /// Integer number, materialised at the specified width.
  Int(IntValue),
  /// Enum value name.
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  Null(AstNullValue<S>),
  /// List of values.
  List(List<S>),
  /// Object value with named fields.
  Object(Object<S>),
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

/// A GraphQL constant input value (schema context) whose numeric leaves are materialised at
/// `i32`.
///
/// Variant for variant [`ast::ConstInputValue`](crate::graphql::ast::ConstInputValue).
#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// Boolean value (`true` or `false`).
  Boolean(AstBooleanValue<S>),
  /// String value (inline or block string).
  String(AstStringValue<S>),
  /// Floating-point number, materialised.
  Float(FloatValue),
  /// Integer number, materialised at the specified width.
  Int(IntValue),
  /// Enum value name.
  Enum(AstEnumValue<S>),
  /// The `null` literal.
  Null(AstNullValue<S>),
  /// List of constant values.
  List(ConstList<S>),
  /// Object value with named fields (all values must be constant).
  Object(ConstObject<S>),
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

/// A list value whose numeric leaves are materialised at `i32`.
pub type List<S, Container = DefaultVec<InputValue<S>>> =
  crate::value::List<InputValue<S>, SimpleSpan, Container>;

/// An object value whose numeric leaves are materialised at `i32`.
pub type Object<S, Container = DefaultVec<ObjectField<S>>> =
  crate::value::Object<Name<S>, InputValue<S>, SimpleSpan, Container>;

/// An object field whose value's numeric leaves are materialised at `i32`.
pub type ObjectField<S> = crate::value::ObjectField<Name<S>, InputValue<S>>;

/// A constant list value whose numeric leaves are materialised at `i32`.
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  crate::value::List<ConstInputValue<S>, SimpleSpan, Container>;

/// A constant object value whose numeric leaves are materialised at `i32`.
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  crate::value::Object<Name<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// A constant object field whose value's numeric leaves are materialised at `i32`.
pub type ConstObjectField<S> = crate::value::ObjectField<Name<S>, ConstInputValue<S>>;

/// A default value whose numeric leaves are materialised at `i32`.
pub type DefaultInputValue<S> = crate::value::DefaultInputValue<ConstInputValue<S>>;
