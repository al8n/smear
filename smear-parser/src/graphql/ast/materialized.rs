//! The materialised-number view of the GraphQL value AST: **the same productions, a second tree.**
//!
//! Two enums and seven aliases. The enums are [`InputValue`] and [`ConstInputValue`], variant for
//! variant the shape of their slice twins in [`ast`](crate::graphql::ast) with `Int` and `Float`
//! carrying [`i64`] and [`f64`]; the aliases bind the shared list, object and default-value
//! carriers to them, at the arity and argument positions their slice twins publish. Every leaf
//! type is the *same* type on both sides, and every production is the same production — see
//! [`syntactic::materialized`](crate::graphql::syntactic::value::materialized), whose entries are
//! [`super`]'s bodies at this tree.
//!
//! # Why a second tree and not a second instantiation of one
//!
//! Because a Rust type alias is not a module. Making the slice `InputValue<S>` an alias of a
//! three-parameter carrier — one tree, two instantiations, no second enum — costs
//! `use ast::InputValue::{Int, String}`, which is `E0432` against an alias and compiles against an
//! enum. It also has to be got exactly right in three other ways at once: a parameter default
//! does not constrain an unmentioned parameter during variant construction, and inserting the
//! payloads ahead of `List`'s `Container` argument silently reinterprets `List<S, MyContainer>`.
//!
//! Two nominal enums have none of those problems and one cost — the variant lists are written
//! twice. `the_two_value_trees_have_the_same_variants` pays for that cost with a wildcard-free
//! census on both, rather than with a comment asking the next reader to keep them in step.
//! `graphql/ast/value.rs` is then **byte-identical to the revision before this axis existed**,
//! which is a stronger compatibility statement than any test.
//!
//! # What is materialised, and what is not
//!
//! **`Int` and `Float`. Nothing else.** An [`ast::StringValue`](crate::graphql::ast::StringValue)
//! still holds the source slice, escapes and all; an
//! [`ast::EnumValue`](crate::graphql::ast::EnumValue), an
//! [`ast::NullValue`](crate::graphql::ast::NullValue) and every
//! [`ast::Name`](crate::graphql::ast::Name) do too. Unescaping a string
//! would mean an owned buffer per node, and the property this view exists to keep is that
//! materialisation allocates **nothing** the slice parser did not already allocate — the two
//! parsers make the same allocations, node for node, and `materialization_allocates_nothing`
//! in the production module's tests is the measurement rather than the claim.
//!
//! There is no `OnceCell` either, and that is the same decision seen from the other side.
//! [`IntValue`] and [`FloatValue`] are `Copy`, so a lazily-filled cell would cost 8–16 bytes on
//! every numeric node whether or not anything ever read it, and it would make the tree `!Sync` —
//! which a server sharing one parsed persisted-query document across request threads cannot
//! accept.
//!
//! # Why `i64` and `f64`
//!
//! `i64` because GraphQL's `IntValue` grammar carries an optional leading `-`
//! (draft §2.9.1), so no unsigned type can hold `-5`. `f64` because GraphQL's `Float` **is** IEEE
//! 754 double precision (draft §3.5.2). Were a narrower integer ever wanted the pair would be
//! `i32` + `f64`; `f32` is non-conformant and must not follow `i32` down.
//!
//! # The bound this view accepts, stated rather than engineered around
//!
//! A 26-digit integer literal is *syntactically valid GraphQL*. In this view it becomes a
//! **parse** error where the specification would make it a **coercion** error, because the
//! conversion happens where the literal is read. The slice parser remains available for the full
//! grammar and is unchanged. See
//! [`syntactic::materialized`](crate::graphql::syntactic::value::materialized) for the
//! productions and the exact error.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use super::{
  BooleanValue as AstBooleanValue, DefaultVec, EnumValue as AstEnumValue, Name,
  NullValue as AstNullValue, StringValue as AstStringValue, VariableValue as AstVariableValue,
};

/// A GraphQL integer literal, materialised as [`i64`].
pub type IntValue<Span = SimpleSpan> = super::IntValue<i64, Span>;

/// A GraphQL floating-point literal, materialised as [`f64`].
pub type FloatValue<Span = SimpleSpan> = super::FloatValue<f64, Span>;

/// A GraphQL input value (executable context) whose numeric leaves are materialised.
///
/// Variant for variant [`ast::InputValue`](crate::graphql::ast::InputValue), with `Int` and
/// `Float` carrying [`i64`] and [`f64`]. Every other leaf is the *same type* the slice tree
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
  /// Integer number, materialised.
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

/// A GraphQL constant input value (schema context) whose numeric leaves are materialised.
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
  /// Integer number, materialised.
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

/// A list value whose numeric leaves are materialised.
pub type List<S, Container = DefaultVec<InputValue<S>>> =
  crate::value::List<InputValue<S>, SimpleSpan, Container>;

/// An object value whose numeric leaves are materialised.
pub type Object<S, Container = DefaultVec<ObjectField<S>>> =
  crate::value::Object<Name<S>, InputValue<S>, SimpleSpan, Container>;

/// An object field whose value's numeric leaves are materialised.
pub type ObjectField<S> = crate::value::ObjectField<Name<S>, InputValue<S>>;

/// A constant list value whose numeric leaves are materialised.
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  crate::value::List<ConstInputValue<S>, SimpleSpan, Container>;

/// A constant object value whose numeric leaves are materialised.
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  crate::value::Object<Name<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// A constant object field whose value's numeric leaves are materialised.
pub type ConstObjectField<S> = crate::value::ObjectField<Name<S>, ConstInputValue<S>>;

/// A default value whose numeric leaves are materialised.
pub type DefaultInputValue<S> = crate::value::DefaultInputValue<ConstInputValue<S>>;

#[cfg(test)]
mod tests;
