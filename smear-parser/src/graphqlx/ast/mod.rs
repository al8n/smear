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

/// GraphQLx argument AST aliases.
pub mod argument;
/// GraphQLx directive AST aliases.
pub mod directive;
/// GraphQLx document AST aliases and top-level variants.
pub mod document;
/// GraphQLx executable-definition AST aliases and enums.
pub mod executable;
/// GraphQLx generic-definition AST aliases.
pub mod generic;
/// GraphQLx import AST nodes.
pub mod import;
/// GraphQLx selection and field AST aliases.
pub mod selection;
/// GraphQLx type-system AST aliases and enums.
pub mod type_system;

pub use argument::*;
pub use directive::*;
pub use document::*;
pub use executable::*;
pub use generic::*;
pub use import::*;
pub use selection::*;
pub use type_system::*;

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
pub type List<S, Span = SimpleSpan, Container = DefaultVec<InputValue<S, Span>>> =
  crate::value::List<InputValue<S, Span>, Span, Container>;

/// A GraphQLx set value.
pub type Set<S, Span = SimpleSpan, Container = DefaultVec<InputValue<S, Span>>> =
  crate::value::Set<InputValue<S, Span>, Span, Container>;

/// A GraphQLx map entry.
pub type MapEntry<S, Span = SimpleSpan> =
  crate::value::MapEntry<InputValue<S, Span>, InputValue<S, Span>, Span>;

/// A GraphQLx map value.
pub type Map<S, Span = SimpleSpan, Container = DefaultVec<MapEntry<S, Span>>> =
  crate::value::Map<InputValue<S, Span>, InputValue<S, Span>, Span, Container>;

/// A GraphQLx object field.
pub type ObjectField<S, Span = SimpleSpan> =
  crate::value::ObjectField<Name<S, Span>, InputValue<S, Span>, Span>;

/// A GraphQLx object value.
pub type Object<S, Span = SimpleSpan, Container = DefaultVec<ObjectField<S, Span>>> =
  crate::value::Object<Name<S, Span>, InputValue<S, Span>, Span, Container>;

/// A constant GraphQLx list value.
pub type ConstList<S, Span = SimpleSpan, Container = DefaultVec<ConstInputValue<S, Span>>> =
  crate::value::List<ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx set value.
pub type ConstSet<S, Span = SimpleSpan, Container = DefaultVec<ConstInputValue<S, Span>>> =
  crate::value::Set<ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx map entry.
pub type ConstMapEntry<S, Span = SimpleSpan> =
  crate::value::MapEntry<ConstInputValue<S, Span>, ConstInputValue<S, Span>, Span>;

/// A constant GraphQLx map value.
pub type ConstMap<S, Span = SimpleSpan, Container = DefaultVec<ConstMapEntry<S, Span>>> =
  crate::value::Map<ConstInputValue<S, Span>, ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx object field.
pub type ConstObjectField<S, Span = SimpleSpan> =
  crate::value::ObjectField<Name<S, Span>, ConstInputValue<S, Span>, Span>;

/// A constant GraphQLx object value.
pub type ConstObject<S, Span = SimpleSpan, Container = DefaultVec<ConstObjectField<S, Span>>> =
  crate::value::Object<Name<S, Span>, ConstInputValue<S, Span>, Span, Container>;

/// A GraphQLx default input value assignment.
pub type DefaultInputValue<S, Span = SimpleSpan> =
  crate::value::DefaultInputValue<ConstInputValue<S, Span>, Span>;

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
pub enum InputValue<S, Span = SimpleSpan> {
  /// A variable reference (`$name`).
  Variable(VariableValue<S, Span>),
  /// A boolean literal.
  Boolean(BooleanValue<S, Span>),
  /// A string literal.
  String(StringValue<S, Span>),
  /// A floating-point literal.
  Float(FloatValue<S, Span>),
  /// An integer literal.
  Int(IntValue<S, Span>),
  /// An enum path.
  Enum(EnumValue<S, Span>),
  /// The `null` literal.
  Null(NullValue<S, Span>),
  /// A list literal.
  List(List<S, Span>),
  /// A `set { ... }` literal.
  Set(Set<S, Span>),
  /// A `map { key => value ... }` literal.
  Map(Map<S, Span>),
  /// An object literal.
  Object(Object<S, Span>),
}

impl<S, Span> AsSpan<Span> for InputValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
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

impl<S, Span> IntoSpan<Span> for InputValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
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
pub enum ConstInputValue<S, Span = SimpleSpan> {
  /// A boolean literal.
  Boolean(BooleanValue<S, Span>),
  /// A string literal.
  String(StringValue<S, Span>),
  /// A floating-point literal.
  Float(FloatValue<S, Span>),
  /// An integer literal.
  Int(IntValue<S, Span>),
  /// An enum path.
  Enum(EnumValue<S, Span>),
  /// The `null` literal.
  Null(NullValue<S, Span>),
  /// A constant list literal.
  List(ConstList<S, Span>),
  /// A constant set literal.
  Set(ConstSet<S, Span>),
  /// A constant map literal.
  Map(ConstMap<S, Span>),
  /// A constant object literal.
  Object(ConstObject<S, Span>),
}

impl<S, Span> AsSpan<Span> for ConstInputValue<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
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

impl<S, Span> IntoSpan<Span> for ConstInputValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
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
pub type TypeGenerics<S, Span = SimpleSpan, Container = DefaultVec<Type<S, Span>>> =
  crate::ty::TypeGenerics<Type<S, Span>, Span, Container>;

/// A path type, its optional generic type arguments, and a non-null modifier.
pub type DefinitionTypePath<
  S,
  Span = SimpleSpan,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = DefaultVec<Type<S, Span>>,
> = crate::ty::DefinitionTypePath<Name<S, Span>, Type<S, Span>, Span, PathContainer, TypeContainer>;

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
pub enum Type<S, Span = SimpleSpan> {
  /// A namespaced path with optional generic arguments.
  Path(DefinitionTypePath<S, Span>),
  /// A list type (`[T]`).
  List(Box<crate::ty::ListType<Self, Span>>),
  /// A set type (`<T>`).
  Set(Box<crate::ty::SetType<Self, Span>>),
  /// A map type (`<K => V>`).
  Map(Box<crate::ty::MapType<Self, Self, Span>>),
}

impl<S, Span> Type<S, Span> {
  /// Returns the span covering this complete type reference.
  #[inline]
  pub fn span(&self) -> &Span {
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

impl<S, Span> AsSpan<Span> for Type<S, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span> IntoSpan<Span> for Type<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Path(value) => value.into_span(),
      Self::List(value) => value.into_span(),
      Self::Set(value) => value.into_span(),
      Self::Map(value) => value.into_span(),
    }
  }
}

#[cfg(test)]
mod tests;
