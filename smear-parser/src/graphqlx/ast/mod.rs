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

use crate::{graphqlx::GraphQLx, value::Sealed};

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

/// The two shapes the carriers hold their nesting children in, and the traits that decide what may
/// stand in each.
///
/// Re-exported here because both reach a consumer's signatures whether or not they name them.
/// [`Nested`] is the default `Container` argument of every value alias below, of
/// [`SelectionSet`] and of [`DefinitionTypePath`]'s type arguments;
/// [`Nest`] is the payload type of three public [`Type`] variants, so it reaches a consumer's
/// `match` as well. [`Nested`] is a `Vec` in every respect a consumer can observe and [`Nest`]
/// answers as the pointer it wraps does; what each adds is the iterative release that keeps a
/// value, a selection or a type nested through these carriers from aborting the process on the way
/// out, however deep it is. That ranges over every recursive position the grammar forms, and not
/// over a node a caller stored in `S` or in `Span` — see [`Nested`]'s own documentation, which
/// states the difference. [`Nestable`], [`NestPtr`] and [`SoleNestPtr`] are all sealed, which fixes
/// who may implement them and says nothing about what a payload may be.
///
/// All eight are exported, and the list is not decorative: a consumer who builds this crate with
/// `graphqlx` and without `graphql` has this module as their only door onto them. The last three
/// are in [`Nestable`]'s own signature — it hands its children to a [`Worklist`], its `Node` is
/// bounded by [`NestNode`], and a node whose grammar has no object or map carrier fills those two
/// lanes with [`Absent`].
pub use crate::value::{Absent, Nest, NestNode, NestPtr, Nestable, Nested, SoleNestPtr, Worklist};

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
pub type List<S, Span = SimpleSpan, Container = Nested<InputValue<S, Span>>> =
  crate::value::List<InputValue<S, Span>, Span, Container>;

/// A GraphQLx set value.
pub type Set<S, Span = SimpleSpan, Container = Nested<InputValue<S, Span>>> =
  crate::value::Set<InputValue<S, Span>, Span, Container>;

/// A GraphQLx map entry.
pub type MapEntry<S, Span = SimpleSpan> =
  crate::value::MapEntry<InputValue<S, Span>, InputValue<S, Span>, Span>;

/// A GraphQLx map value.
pub type Map<S, Span = SimpleSpan, Container = Nested<MapEntry<S, Span>>> =
  crate::value::Map<InputValue<S, Span>, InputValue<S, Span>, Span, Container>;

/// A GraphQLx object field.
pub type ObjectField<S, Span = SimpleSpan> =
  crate::value::ObjectField<Name<S, Span>, InputValue<S, Span>, Span>;

/// A GraphQLx object value.
pub type Object<S, Span = SimpleSpan, Container = Nested<ObjectField<S, Span>>> =
  crate::value::Object<Name<S, Span>, InputValue<S, Span>, Span, Container>;

/// A constant GraphQLx list value.
pub type ConstList<S, Span = SimpleSpan, Container = Nested<ConstInputValue<S, Span>>> =
  crate::value::List<ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx set value.
pub type ConstSet<S, Span = SimpleSpan, Container = Nested<ConstInputValue<S, Span>>> =
  crate::value::Set<ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx map entry.
pub type ConstMapEntry<S, Span = SimpleSpan> =
  crate::value::MapEntry<ConstInputValue<S, Span>, ConstInputValue<S, Span>, Span>;

/// A constant GraphQLx map value.
pub type ConstMap<S, Span = SimpleSpan, Container = Nested<ConstMapEntry<S, Span>>> =
  crate::value::Map<ConstInputValue<S, Span>, ConstInputValue<S, Span>, Span, Container>;

/// A constant GraphQLx object field.
pub type ConstObjectField<S, Span = SimpleSpan> =
  crate::value::ObjectField<Name<S, Span>, ConstInputValue<S, Span>, Span>;

/// A constant GraphQLx object value.
pub type ConstObject<S, Span = SimpleSpan, Container = Nested<ConstObjectField<S, Span>>> =
  crate::value::Object<Name<S, Span>, ConstInputValue<S, Span>, Span, Container>;

/// A GraphQLx default input value assignment.
pub type DefaultInputValue<S, Span = SimpleSpan> =
  crate::value::DefaultInputValue<ConstInputValue<S, Span>, Span>;

/// A GraphQLx input value, including variables and extended collections.
///
/// # This enum declares no `Drop`, and that is load-bearing
///
/// Releasing a deeply nested one used to abort the process, one native frame per level. The repair
/// is [`Nested`], the container the four nesting arms hold their children in
/// — **not** a `Drop` on this enum, which would have cost every by-value `unwrap_*` and
/// `try_unwrap_*` to `E0509`.
///
/// This dialect has **four** nesting variants rather than GraphQL's two, and a map entry nests
/// through both halves, so it is the one where the defect had the most ways in.
///
/// The repair covers every recursive position the *grammar* forms. It does not cover a node a
/// caller stored in `S` or in `Span` — this pair is the only one that leaves `Span` a parameter,
/// so it is also the one with the widest exposure. See [`Nested`]'s own documentation and
/// `al8n/smear#176`.
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

impl<S, Span> Sealed for InputValue<S, Span> {}

impl<S, Span> NestNode for InputValue<S, Span> {
  type Field = ObjectField<S, Span>;
  type Entry = MapEntry<S, Span>;
}

impl<S, Span> Nestable for InputValue<S, Span> {
  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      // These arms hold no value of this enum, so they are released here. They do hold `S` and
      // `Span`, and at a caller's arguments either can own a node this loop cannot reach
      // (al8n/smear#176).
      Self::Variable(_)
      | Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => worklist.adopt(list.into_values().into_vec()),
      Self::Set(set) => worklist.adopt(set.into_values().into_vec()),
      // A map entry's KEY is an input value too, and it nests exactly as the value does — the one
      // place in either dialect where a single child slot yields two subtrees. The container of
      // entries is handed over as it stands and each entry forwards its own two halves when the
      // walk reaches it, in place of flattening all 2N of them into the worklist up front.
      Self::Map(map) => worklist.adopt_entries(map.into_entries().into_vec()),
      Self::Object(object) => worklist.adopt_fields(object.into_fields().into_vec()),
    }
  }
}

/// A GraphQLx constant input value, which cannot contain a variable.
///
/// Like [`InputValue`], it declares no `Drop`; the release is [`Nested`]'s.
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

impl<S, Span> Sealed for ConstInputValue<S, Span> {}

impl<S, Span> NestNode for ConstInputValue<S, Span> {
  type Field = ConstObjectField<S, Span>;
  type Entry = ConstMapEntry<S, Span>;
}

impl<S, Span> Nestable for ConstInputValue<S, Span> {
  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      // These arms hold no value of this enum, so they are released here. They do hold `S` and
      // `Span`, and at a caller's arguments either can own a node this loop cannot reach
      // (al8n/smear#176).
      Self::Boolean(_)
      | Self::String(_)
      | Self::Float(_)
      | Self::Int(_)
      | Self::Enum(_)
      | Self::Null(_) => {}
      Self::List(list) => worklist.adopt(list.into_values().into_vec()),
      Self::Set(set) => worklist.adopt(set.into_values().into_vec()),
      // A map entry's KEY is an input value too, and it nests exactly as the value does — the one
      // place in either dialect where a single child slot yields two subtrees.
      Self::Map(map) => worklist.adopt_entries(map.into_entries().into_vec()),
      Self::Object(object) => worklist.adopt_fields(object.into_fields().into_vec()),
    }
  }
}

/// Generic type arguments used by a GraphQLx type path.
///
/// `Container` is a plain `Vec` here and [`Nested`] inside [`DefinitionTypePath`], which is the
/// one of the two that closes a cycle — see that alias.
pub type TypeGenerics<S, Span = SimpleSpan, Container = DefaultVec<Type<S, Span>>> =
  crate::ty::TypeGenerics<Type<S, Span>, Span, Container>;

/// A path type, its optional generic type arguments, and a non-null modifier.
///
/// `TypeContainer` is [`Nested`], and this is this dialect's *fourth* route into [`Type`]: a path's
/// arguments are types, so `A<B<C<…>>>` nests without passing through any of the three pointer
/// arms. [`Nest`] cannot see that cycle — there is no owned pointer in it — so the container is
/// what stands in it, exactly as it does for the value collections.
pub type DefinitionTypePath<
  S,
  Span = SimpleSpan,
  PathContainer = DefaultVec<Name<S, Span>>,
  TypeContainer = Nested<Type<S, Span>>,
> = crate::ty::DefinitionTypePath<Name<S, Span>, Type<S, Span>, Span, PathContainer, TypeContainer>;

/// A recursive GraphQLx type reference.
///
/// # This enum declares no `Drop`, and that is load-bearing
///
/// Releasing a deeply nested one used to abort the process, one native frame per level, and no
/// parse was needed to build one: every carrier's constructor is public, so a caller can grow the
/// nesting in a loop and merely leaving scope was enough. The repair is [`Nest`] in the three
/// pointer arms and [`Nested`] in [`TypeGenerics`] — **not** a `Drop` on this enum, which `E0509`
/// would have charged every by-value `unwrap_*` and `try_unwrap_*` for, and which for a
/// single-owned-child cycle could not have been written at all. [`Nest`]'s own documentation
/// derives why.
///
/// This dialect nests four ways where vanilla GraphQL nests one, and all four are covered:
/// [`Nestable`] below is the walk, and it matches without a wildcard arm so a fifth is a compile
/// error here rather than a silent return to recursing.
///
/// `Drop` is one of three generated impls that descend one frame per level on this enum — there is
/// no derived `PartialEq` here, unlike the vanilla dialect's — and the derived `Debug` and `Clone`
/// still do. This removes the only one of the three that fires without a call being made. What
/// standing a [`Nest`] in an arm costs the other two was measured on `graphql::ast::Type`, whose
/// arm has the same shape; `value/nesting.rs`'s header has the table, and the short version is
/// nothing in a release build and a stated charge in a debug one.
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
  List(Nest<Box<crate::ty::ListType<Self, Span>>>),
  /// A set type (`<T>`).
  Set(Nest<Box<crate::ty::SetType<Self, Span>>>),
  /// A map type (`<K => V>`).
  Map(Nest<Box<crate::ty::MapType<Self, Self, Span>>>),
}

impl<S, Span> Sealed for Type<S, Span> {}

/// A type's children are types, so neither carrier lane exists on this enum: an object field and
/// a map entry are *value* carriers, and a map **type**'s key and value are types handed over one
/// at a time through [`Worklist::push`].
impl<S, Span> NestNode for Type<S, Span> {
  type Field = Absent<Self>;
  type Entry = Absent<Self>;
}

impl<S, Span> Nestable for Type<S, Span> {
  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    match self {
      // A path holds no type EXCEPT through its generic arguments, which is the arm a reading of
      // the three pointer variants alone would miss.
      Self::Path(path) => {
        let (_, _, generics, _) = path.into_components();
        if let Some(generics) = generics {
          worklist.adopt(generics.into_params().into_vec());
        }
      }
      // Nothing is handed over only when a pointer is shared and another owner remains, which is
      // the one case with nothing below it to unlink yet. A chain of these arms runs through the
      // worklist's register, so releasing one allocates nothing at any depth.
      Self::List(nest) => {
        if let Some(list) = nest.into_inner() {
          worklist.push(list.into_components().1);
        }
      }
      Self::Set(nest) => {
        if let Some(set) = nest.into_inner() {
          worklist.push(set.into_components().1);
        }
      }
      // A map's KEY is a type too, and it nests exactly as the value does.
      Self::Map(nest) => {
        if let Some(map) = nest.into_inner() {
          let (_, key, value, _) = map.into_components();
          worklist.push(key);
          worklist.push(value);
        }
      }
    }
  }
}

impl<S, Span> Sealed for crate::ty::ListType<Type<S, Span>, Span> {}

/// The pointee side of the walk: a list carrier is `(span, element, required)`.
impl<S, Span> Nestable for crate::ty::ListType<Type<S, Span>, Span> {
  type Node = Type<S, Span>;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Type<S, Span>>) {
    worklist.push(self.into_components().1);
  }
}

impl<S, Span> Sealed for crate::ty::SetType<Type<S, Span>, Span> {}

/// The pointee side of the walk: a set carrier is `(span, element, required)`.
impl<S, Span> Nestable for crate::ty::SetType<Type<S, Span>, Span> {
  type Node = Type<S, Span>;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Type<S, Span>>) {
    worklist.push(self.into_components().1);
  }
}

impl<S, Span> Sealed for crate::ty::MapType<Type<S, Span>, Type<S, Span>, Span> {}

/// The pointee side of the walk: a map carrier is `(span, key, value, required)`, and both the key
/// and the value are types.
impl<S, Span> Nestable for crate::ty::MapType<Type<S, Span>, Type<S, Span>, Span> {
  type Node = Type<S, Span>;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Type<S, Span>>) {
    let (_, key, value, _) = self.into_components();
    worklist.push(key);
    worklist.push(value);
  }
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
      // `into_sole` rather than a deref: the span is owned and `Span` carries no `Copy` bound in
      // this dialect, so the pointee has to come back out. `Box` is the sole owner, which is what
      // makes that infallible.
      Self::List(value) => value.into_sole().into_span(),
      Self::Set(value) => value.into_sole().into_span(),
      Self::Map(value) => value.into_sole().into_span(),
    }
  }
}

#[cfg(test)]
mod tests;
