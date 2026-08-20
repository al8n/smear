//! GraphQLx AST node types and aliases over the shared parser-next carriers.
//!
//! The aliases bind shared nodes to the [`GraphQLx`]
//! marker while preserving the source slice type supplied by the concrete lexer.

use std::{boxed::Box, vec::Vec};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use crate::{
  graphqlx::GraphQLx,
  value::{Unnest, push_nesting, release},
};

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
///
/// # The by-value `unwrap_*` forms are gone, and a nested value is why
///
/// `#[unwrap(ref, ref_mut)]` and `#[try_unwrap(ref, ref_mut)]` are repeated on **every variant**
/// rather than only on the enum, which is how `derive_more` is told to stop generating the owned
/// `unwrap_list(self) -> …` and `try_unwrap_list(self) -> Result<…, Self>` pairs. The variant set
/// and the shape are untouched, and `unwrap_list_ref`, `unwrap_list_mut`, `try_unwrap_list_ref`
/// and `is_list` are generated exactly as before.
///
/// This enum has a hand-written [`Drop`] — see [`nesting`](crate::value::nesting) for the process
/// abort it removes — and Rust does not let a payload be moved out of a type that implements
/// `Drop` (`E0509`). This dialect has **four** nesting variants rather than GraphQL's two, so it is
/// the one where the defect had the most ways in.
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
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Variable(VariableValue<S, Span>),
  /// A boolean literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(BooleanValue<S, Span>),
  /// A string literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(StringValue<S, Span>),
  /// A floating-point literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue<S, Span>),
  /// An integer literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<S, Span>),
  /// An enum path.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(EnumValue<S, Span>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(NullValue<S, Span>),
  /// A list literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(List<S, Span>),
  /// A `set { ... }` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Set(Set<S, Span>),
  /// A `map { key => value ... }` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Map(Map<S, Span>),
  /// An object literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
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

/// # `Span: Clone`, and it is a narrowing rather than a convenience
///
/// This impl used to reach the span by matching `self` by value and moving each payload out, which
/// `E0509` forbids now that the enum has a [`Drop`]. The only remaining door onto the span is
/// [`AsSpan`], which lends one, so producing an owned `Span` needs the bound — and no bound can be
/// added to a trait *method*, so it goes on the impl.
///
/// What that costs is an `IntoSpan` for a span type that cannot be duplicated. Every span in this
/// workspace is [`SimpleSpan`], which is [`Copy`]; `Clone` rather than `Copy` is chosen so the impl
/// stays available to the widest set of span types that can satisfy it at all.
impl<S, Span: Clone> IntoSpan<Span> for InputValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.as_span().clone()
  }
}

impl<S, Span> Unnest for InputValue<S, Span> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(
      self,
      Self::List(_) | Self::Set(_) | Self::Map(_) | Self::Object(_)
    )
  }

  fn unnest(&mut self, pending: &mut Vec<Self>) {
    match self {
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Set(set) => push_nesting(pending, set.values_mut().drain(..)),
      // A map entry's KEY is an input value too, and it nests exactly as the value does — the one
      // place in either dialect where a single child slot yields two subtrees.
      Self::Map(map) => push_nesting(
        pending,
        map.entries_mut().drain(..).flat_map(|entry| {
          let (_, key, value) = entry.into_components();
          [key, value]
        }),
      ),
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

impl<S, Span> Drop for InputValue<S, Span> {
  #[inline]
  fn drop(&mut self) {
    release(self);
  }
}

/// A GraphQLx constant input value, which cannot contain a variable.
///
/// The per-variant `unwrap` attributes and the [`Drop`] below are [`InputValue`]'s, for the reason
/// stated there.
#[derive(Debug, Clone, derive_more::IsVariant, derive_more::TryUnwrap, derive_more::Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S, Span = SimpleSpan> {
  /// A boolean literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Boolean(BooleanValue<S, Span>),
  /// A string literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  String(StringValue<S, Span>),
  /// A floating-point literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Float(FloatValue<S, Span>),
  /// An integer literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Int(IntValue<S, Span>),
  /// An enum path.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Enum(EnumValue<S, Span>),
  /// The `null` literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Null(NullValue<S, Span>),
  /// A constant list literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  List(ConstList<S, Span>),
  /// A constant set literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Set(ConstSet<S, Span>),
  /// A constant map literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
  Map(ConstMap<S, Span>),
  /// A constant object literal.
  #[unwrap(ref, ref_mut)]
  #[try_unwrap(ref, ref_mut)]
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

/// [`InputValue`]'s body and [`InputValue`]'s bound, for the reasons stated there.
impl<S, Span: Clone> IntoSpan<Span> for ConstInputValue<S, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.as_span().clone()
  }
}

impl<S, Span> Unnest for ConstInputValue<S, Span> {
  #[inline]
  fn nests(&self) -> bool {
    matches!(
      self,
      Self::List(_) | Self::Set(_) | Self::Map(_) | Self::Object(_)
    )
  }

  fn unnest(&mut self, pending: &mut Vec<Self>) {
    match self {
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => push_nesting(pending, list.values_mut().drain(..)),
      Self::Set(set) => push_nesting(pending, set.values_mut().drain(..)),
      Self::Map(map) => push_nesting(
        pending,
        map.entries_mut().drain(..).flat_map(|entry| {
          let (_, key, value) = entry.into_components();
          [key, value]
        }),
      ),
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

impl<S, Span> Drop for ConstInputValue<S, Span> {
  #[inline]
  fn drop(&mut self) {
    release(self);
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
