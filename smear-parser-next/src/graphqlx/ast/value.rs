//! GraphQLx value AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/value.rs`): the value carriers, the `set`/`map` composites, and
//! the [`InputValue`] / [`ConstInputValue`] sum types. The scalar carriers reuse
//! the shared [`crate::value`] nodes, except the radix-preserving [`IntValue`] /
//! [`FloatValue`] and the `::`-path [`EnumValue`], which wrap them in GraphQLx
//! newtypes (see the `int` / `float` / `enum_value` submodules).

// The `pub(crate)` `new` constructors on the value newtypes are the substrate the
// value productions (a later wave) mint these nodes with; until those land they
// have no in-crate caller.
#![allow(dead_code)]

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_scaffold::ast as scaffold;
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::DefaultVec;
use crate::ident::Ident;

pub use crate::value::{BlockStringValue, BooleanValue, InlineStringValue, NullValue, StringValue};
pub use enum_value::*;
pub use float::*;
pub use int::*;

mod enum_value;
mod float;
mod int;

/// A variable value reference in GraphQLx.
pub type VariableValue<S> = crate::value::VariableValue<Ident<S>>;

/// A list value in GraphQLx that can contain any input value.
pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
/// A set value in GraphQLx containing input values.
pub type Set<S, Container = DefaultVec<InputValue<S>>> = scaffold::Set<InputValue<S>, Container>;
/// A key-value entry in a map value.
pub type MapEntry<S> = scaffold::MapEntry<InputValue<S>, InputValue<S>>;
/// A map value in GraphQLx containing key-value pairs.
pub type Map<S, Container = DefaultVec<(InputValue<S>, InputValue<S>)>> =
  scaffold::Map<InputValue<S>, InputValue<S>, Container>;
/// An object value in GraphQLx containing named fields.
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Ident<S>, InputValue<S>, Container>;
/// A field in an object value.
pub type ObjectField<S> = scaffold::ObjectField<Ident<S>, InputValue<S>>;

/// A constant list value in GraphQLx (cannot contain variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
/// A constant set value in GraphQLx (cannot contain variables).
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Set<ConstInputValue<S>, Container>;
/// A constant key-value entry in a map value.
pub type ConstMapEntry<S> = scaffold::MapEntry<ConstInputValue<S>, ConstInputValue<S>>;
/// A constant map value in GraphQLx (cannot contain variables).
pub type ConstMap<S, Container = DefaultVec<(ConstInputValue<S>, ConstInputValue<S>)>> =
  scaffold::Map<ConstInputValue<S>, ConstInputValue<S>, Container>;
/// A constant object value in GraphQLx (cannot contain variables).
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Ident<S>, ConstInputValue<S>, Container>;
/// A field in a constant object value.
pub type ConstObjectField<S> = scaffold::ObjectField<Ident<S>, ConstInputValue<S>>;

/// Default value for input fields and arguments, using constant expressions
/// (`= ConstValue`).
pub type DefaultInputValue<S> = scaffold::DefaultInputValue<ConstInputValue<S>>;

/// GraphQLx input value (executable context).
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
  /// Enum value (a `::`-path).
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of values.
  List(scaffold::List<InputValue<S>>),
  /// Set of values (`set { … }`).
  Set(scaffold::Set<InputValue<S>>),
  /// Map of key-value pairs (`map { … }`).
  Map(scaffold::Map<InputValue<S>, InputValue<S>>),
  /// Object value with named fields.
  Object(scaffold::Object<Ident<S>, InputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

/// GraphQLx constant input value (schema context).
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
  /// Enum value (a `::`-path).
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// List of constant values.
  List(scaffold::List<ConstInputValue<S>>),
  /// Set of constant values (`set { … }`).
  Set(scaffold::Set<ConstInputValue<S>>),
  /// Map of constant key-value pairs (`map { … }`).
  Map(scaffold::Map<ConstInputValue<S>, ConstInputValue<S>>),
  /// Object value with named fields (all values must be constant).
  Object(scaffold::Object<Ident<S>, ConstInputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}
