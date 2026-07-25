//! GraphQLx AST node types and aliases over the shared parser-next carriers.
//!
//! The aliases bind shared nodes to the [`GraphQLx`]
//! marker while preserving the source slice type supplied by the concrete lexer.

use std::{boxed::Box, vec::Vec};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
};

use crate::graphqlx::GraphQLx;

pub use import::*;

mod import;

/// The default collection container used by GraphQLx AST collections.
pub type DefaultVec<T> = Vec<T>;

/// A GraphQLx name.
#[allow(type_alias_bounds)]
pub type Name<S: ?Sized, Span = SimpleSpan> = crate::name::Name<S, Span, GraphQLx>;

/// A `::`-separated GraphQLx path.
pub type Path<S, Span = SimpleSpan, Container = DefaultVec<Name<S, Span>>> =
  crate::path::Path<Name<S, Span>, Span, Container>;

/// A GraphQLx boolean literal.
pub type BooleanValue<S, Span = SimpleSpan> = crate::value::BooleanValue<S, Span, GraphQLx>;

/// A GraphQLx string literal.
pub type StringValue<S, Span = SimpleSpan> = crate::value::StringValue<S, Span, GraphQLx>;

/// A GraphQLx inline string literal.
pub type InlineStringValue<S, Span = SimpleSpan> =
  crate::value::InlineStringValue<S, Span, GraphQLx>;

/// A GraphQLx block string literal.
pub type BlockStringValue<S, Span = SimpleSpan> = crate::value::BlockStringValue<S, Span, GraphQLx>;

/// A GraphQLx integer literal preserving its decimal, hexadecimal, binary, or
/// octal lexer representation while exposing the source slice as `S`.
pub type IntValue<S, Span = SimpleSpan> =
  crate::value::IntValue<smear_lexer::graphqlx::LitInt<S>, Span, GraphQLx>;

/// A GraphQLx floating-point literal preserving its decimal or hexadecimal
/// lexer representation while exposing the source slice as `S`.
pub type FloatValue<S, Span = SimpleSpan> =
  crate::value::FloatValue<smear_lexer::graphqlx::LitFloat<S>, Span, GraphQLx>;

/// The GraphQLx `null` literal.
pub type NullValue<S, Span = SimpleSpan> = crate::value::NullValue<S, Span, GraphQLx>;

/// A GraphQLx enum value represented by a complete path.
pub type EnumValue<S, Span = SimpleSpan> = crate::value::EnumValue<Path<S, Span>, Span, GraphQLx>;

/// A GraphQLx variable value.
pub type VariableValue<S, Span = SimpleSpan> = crate::value::VariableValue<Name<S, Span>, Span>;

/// A GraphQLx list value.
pub type List<S, Container = DefaultVec<InputValue<S>>> =
  crate::value::List<InputValue<S>, SimpleSpan, Container>;

/// A GraphQLx set value.
pub type Set<S, Container = DefaultVec<InputValue<S>>> =
  crate::value::Set<InputValue<S>, SimpleSpan, Container>;

/// A GraphQLx map entry.
pub type MapEntry<S> = crate::value::MapEntry<InputValue<S>, InputValue<S>>;

/// A GraphQLx map value.
pub type Map<S, Container = DefaultVec<MapEntry<S>>> =
  crate::value::Map<InputValue<S>, InputValue<S>, SimpleSpan, Container>;

/// A GraphQLx object field.
pub type ObjectField<S> = crate::value::ObjectField<Name<S>, InputValue<S>>;

/// A GraphQLx object value.
pub type Object<S, Container = DefaultVec<ObjectField<S>>> =
  crate::value::Object<Name<S>, InputValue<S>, SimpleSpan, Container>;

/// A constant GraphQLx list value.
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  crate::value::List<ConstInputValue<S>, SimpleSpan, Container>;

/// A constant GraphQLx set value.
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  crate::value::Set<ConstInputValue<S>, SimpleSpan, Container>;

/// A constant GraphQLx map entry.
pub type ConstMapEntry<S> = crate::value::MapEntry<ConstInputValue<S>, ConstInputValue<S>>;

/// A constant GraphQLx map value.
pub type ConstMap<S, Container = DefaultVec<ConstMapEntry<S>>> =
  crate::value::Map<ConstInputValue<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// A constant GraphQLx object field.
pub type ConstObjectField<S> = crate::value::ObjectField<Name<S>, ConstInputValue<S>>;

/// A constant GraphQLx object value.
pub type ConstObject<S, Container = DefaultVec<ConstObjectField<S>>> =
  crate::value::Object<Name<S>, ConstInputValue<S>, SimpleSpan, Container>;

/// A GraphQLx input value, including variables and extended collections.
#[derive(
  Debug,
  Clone,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::TryUnwrap,
  derive_more::Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// A variable reference (`$name`).
  Variable(VariableValue<S>),
  /// A boolean literal.
  Boolean(BooleanValue<S>),
  /// A string literal.
  String(StringValue<S>),
  /// A floating-point literal.
  Float(FloatValue<S>),
  /// An integer literal.
  Int(IntValue<S>),
  /// An enum path.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// A list literal.
  List(List<S>),
  /// A `set { ... }` literal.
  Set(Set<S>),
  /// A `map { key => value ... }` literal.
  Map(Map<S>),
  /// An object literal.
  Object(Object<S>),
}

impl<S> AsSpan<SimpleSpan> for InputValue<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Variable(value) => value.as_span(),
      Self::Boolean(value) => value.as_span(),
      Self::String(value) => value.as_span(),
      Self::Float(value) => value.as_span(),
      Self::Int(value) => value.as_span(),
      Self::Enum(value) => value.as_span(),
      Self::Null(value) => value.as_span(),
      Self::List(value) => value.as_span(),
      Self::Set(value) => value.as_span(),
      Self::Map(value) => value.as_span(),
      Self::Object(value) => value.as_span(),
    }
  }
}

impl<S> IntoSpan<SimpleSpan> for InputValue<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Variable(value) => value.into_span(),
      Self::Boolean(value) => value.into_span(),
      Self::String(value) => value.into_span(),
      Self::Float(value) => value.into_span(),
      Self::Int(value) => value.into_span(),
      Self::Enum(value) => value.into_span(),
      Self::Null(value) => value.into_span(),
      Self::List(value) => value.into_span(),
      Self::Set(value) => value.into_span(),
      Self::Map(value) => value.into_span(),
      Self::Object(value) => value.into_span(),
    }
  }
}

/// A GraphQLx constant input value, which cannot contain a variable.
#[derive(Debug, Clone, derive_more::IsVariant, derive_more::TryUnwrap, derive_more::Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// A boolean literal.
  Boolean(BooleanValue<S>),
  /// A string literal.
  String(StringValue<S>),
  /// A floating-point literal.
  Float(FloatValue<S>),
  /// An integer literal.
  Int(IntValue<S>),
  /// An enum path.
  Enum(EnumValue<S>),
  /// The `null` literal.
  Null(NullValue<S>),
  /// A constant list literal.
  List(ConstList<S>),
  /// A constant set literal.
  Set(ConstSet<S>),
  /// A constant map literal.
  Map(ConstMap<S>),
  /// A constant object literal.
  Object(ConstObject<S>),
}

impl<S> AsSpan<SimpleSpan> for ConstInputValue<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    match self {
      Self::Boolean(value) => value.as_span(),
      Self::String(value) => value.as_span(),
      Self::Float(value) => value.as_span(),
      Self::Int(value) => value.as_span(),
      Self::Enum(value) => value.as_span(),
      Self::Null(value) => value.as_span(),
      Self::List(value) => value.as_span(),
      Self::Set(value) => value.as_span(),
      Self::Map(value) => value.as_span(),
      Self::Object(value) => value.as_span(),
    }
  }
}

impl<S> IntoSpan<SimpleSpan> for ConstInputValue<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Boolean(value) => value.into_span(),
      Self::String(value) => value.into_span(),
      Self::Float(value) => value.into_span(),
      Self::Int(value) => value.into_span(),
      Self::Enum(value) => value.into_span(),
      Self::Null(value) => value.into_span(),
      Self::List(value) => value.into_span(),
      Self::Set(value) => value.into_span(),
      Self::Map(value) => value.into_span(),
      Self::Object(value) => value.into_span(),
    }
  }
}

/// Generic type arguments used by a GraphQLx type path.
pub type TypeGenerics<S, Container = DefaultVec<Type<S>>> =
  crate::ty::TypeGenerics<Type<S>, SimpleSpan, Container>;

/// A path type, its optional generic type arguments, and a non-null modifier.
pub type DefinitionTypePath<
  S,
  PathContainer = DefaultVec<Name<S>>,
  TypeContainer = DefaultVec<Type<S>>,
> = crate::ty::DefinitionTypePath<Name<S>, Type<S>, SimpleSpan, PathContainer, TypeContainer>;

/// A recursive GraphQLx type reference.
#[derive(
  Debug,
  Clone,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::TryUnwrap,
  derive_more::Unwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Type<S> {
  /// A namespaced path with optional generic arguments.
  Path(DefinitionTypePath<S>),
  /// A list type (`[T]`).
  List(Box<crate::ty::ListType<Self>>),
  /// A set type (`<T>`).
  Set(Box<crate::ty::SetType<Self>>),
  /// A map type (`<K => V>`).
  Map(Box<crate::ty::MapType<Self, Self>>),
}

impl<S> Type<S> {
  /// Returns the span covering this complete type reference.
  #[inline]
  pub fn span(&self) -> &SimpleSpan {
    match self {
      Self::Path(value) => value.span(),
      Self::List(value) => value.span(),
      Self::Set(value) => value.span(),
      Self::Map(value) => value.span(),
    }
  }

  /// Returns whether this type reference is non-null.
  #[inline]
  pub fn required(&self) -> bool {
    match self {
      Self::Path(value) => value.required(),
      Self::List(value) => value.required(),
      Self::Set(value) => value.required(),
      Self::Map(value) => value.required(),
    }
  }
}

impl<S> AsSpan<SimpleSpan> for Type<S> {
  #[inline]
  fn as_span(&self) -> &SimpleSpan {
    self.span()
  }
}

impl<S> IntoSpan<SimpleSpan> for Type<S> {
  #[inline]
  fn into_span(self) -> SimpleSpan {
    match self {
      Self::Path(value) => value.into_span(),
      Self::List(value) => value.into_span(),
      Self::Set(value) => value.into_span(),
      Self::Map(value) => value.into_span(),
    }
  }
}
