//! GraphQLx document AST aliases, the top-level entries `use` items stand among, and the inline
//! modules a type-system entry may be.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use super::{
  ConstInputValue, DefaultVec, Described, DescribedExecutableDefinition,
  DescribedTypeSystemDefinition, ExecutableDefinition, Leaf, Name, Nested, StringValue, Type,
  TypeSystemDefinition, TypeSystemExtension, UseDefinition, UseTree,
};
use crate::value::{Absent, NestNode, Nestable, Sealed, Worklist};

/// A GraphQLx definition: either type-system or executable syntax.
pub type Definition<S, Span = SimpleSpan, Ty = Type<S, Span>> = crate::executable::Definition<
  TypeSystemDefinition<S, Span, Ty>,
  ExecutableDefinition<S, Span, Ty>,
>;

/// A described GraphQLx definition.
pub type DescribedDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  Described<Definition<S, Span, Ty>, S, Span>;

/// A described GraphQLx definition or a type-system extension.
pub type DefinitionOrExtension<S, Span = SimpleSpan, Ty = Type<S, Span>> =
  crate::executable::DefinitionOrExtension<
    DescribedDefinition<S, Span, Ty>,
    TypeSystemExtension<S, Span, Ty>,
  >;

/// A GraphQLx type-system entry: a `use` item, an inline module, a definition or an extension.
///
/// The one entry a type-system document and a module's body both hold, so a module may hold
/// another module.
///
/// # This enum declares no `Drop`, and that is load-bearing
///
/// A [`ModDefinition`] holds these in a [`Nested`] container, and every constructor involved is
/// public, so a caller can nest modules in a loop that no parser sees. The release is that
/// container's, fed by this enum's [`Nestable`] impl, and walks the modules off the native stack. A
/// `Drop` here would cost every by-value `unwrap_*` and `try_unwrap_*` to `E0509`.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemItem<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// An inline module, `mod name { … }`.
  Mod(ModDefinition<S, Span, Ty>),
  /// A described type-system definition.
  Definition(DescribedTypeSystemDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> TypeSystemItem<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Mod(module) => module.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for TypeSystemItem<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for TypeSystemItem<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Mod(module) => module.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

impl<S, Span, Ty> Sealed for TypeSystemItem<S, Span, Ty> {}

/// An item's children are items, and a module holds them directly. Neither carrier lane exists
/// here: an object field and a map entry are *value* carriers.
impl<S, Span, Ty> NestNode for TypeSystemItem<S, Span, Ty> {
  type Field = Absent<Self>;
  type Entry = Absent<Self>;
}

// ── THE RANK EDGES, ASSERTED ────────────────────────────────────────────────────────────────────
//
// An item hands over only a module's items. Everything else it holds is dropped **in place**: a
// definition's directives and default values hold `ConstInputValue` nodes, its fields' and
// arguments' types hold `Type` nodes, and a `use` item holds a `UseTree`. Each of those is a node of
// a lower-ranked tree with an iterative release of its own, so dropping one here costs this release
// O(1) native frames however deep it is — the invariant on `Nestable::into_children`.
//
// One concrete instantiation each is enough: `RANK` does not depend on the payload parameters.
const _: () = assert!(
  <ConstInputValue<&'static str> as Nestable>::RANK
    < <TypeSystemItem<&'static str> as Nestable>::RANK,
  "an item drops constant values in place, so the value tree must rank below the item tree",
);

const _: () = assert!(
  <Type<&'static str> as Nestable>::RANK < <TypeSystemItem<&'static str> as Nestable>::RANK,
  "an item drops a field's type in place, so the type tree must rank below the item tree",
);

const _: () = assert!(
  <UseTree<&'static str> as Nestable>::RANK < <TypeSystemItem<&'static str> as Nestable>::RANK,
  "an item drops a `use` item's tree in place, so the use tree must rank below the item tree",
);

impl<S, Span, Ty> Nestable for TypeSystemItem<S, Span, Ty> {
  /// The module item tree: rank 2. It drops value, type and use nodes in place — see the
  /// assertions above — and holds no selection.
  const RANK: u8 = 2;

  type Node = Self;

  #[inline]
  fn into_children(self, worklist: &mut Worklist<Self>) {
    // The match has no wildcard arm although the enum is `#[non_exhaustive]`, which it can do
    // because the impl is inside the defining crate: a fifth variant is a compile error here
    // rather than a silent return to recursing.
    match self {
      // These arms hold no item, so they are released here. What they hold is a leaf or a node of
      // a lower-ranked tree, which is what the assertions above check.
      Self::Use(_) | Self::Definition(_) | Self::Extension(_) => {}
      // span, description, name — dropped in place, all three leaves.
      Self::Mod(module) => worklist.adopt(module.into_items().into_vec()),
    }
  }
}

/// An inline module, `Description? mod Name { ModItem* }`.
///
/// A namespace: its items are a type-system document's — `use` items, modules, definitions and
/// extensions — and an empty one is a module with nothing in it. No operation and no fragment may
/// stand in one. A description, when one is written, is the module's own, and the module's span
/// starts at it.
///
/// # This type declares no `Drop`, and that is load-bearing
///
/// A module holds items, an item may be a module, and every constructor here is public, so a caller
/// can nest one in a loop that no parser sees. The items sit in a [`Nested`] container and are
/// released by its loop rather than by one native frame per level; a `Drop` here would cost the
/// by-value [`IntoComponents`] destructuring to `E0509`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  span: Span,
  description: Option<StringValue<S, Span>>,
  name: Name<S, Span>,
  items: Nested<TypeSystemItem<S, Span, Ty>>,
}

impl<S, Span, Ty> ModDefinition<S, Span, Ty> {
  /// Creates a module from its complete span, its description, its name and its items.
  ///
  /// `S` and `Span` are [`Leaf`]: a module's items are released by a loop that drops every name,
  /// description and definition in place, so neither may own a node of its own
  /// (`al8n/smear#176`). Every item shares the module's `S` and `Span`, so the bound here reaches
  /// all of them.
  #[inline]
  pub const fn new(
    span: Span,
    description: Option<StringValue<S, Span>>,
    name: Name<S, Span>,
    items: Nested<TypeSystemItem<S, Span, Ty>>,
  ) -> Self
  where
    S: Leaf,
    Span: Leaf,
  {
    Self {
      span,
      description,
      name,
      items,
    }
  }

  /// Returns the complete module span, from its description or its `mod` to its `}`.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the module's description, if one was written.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<S, Span>> {
    self.description.as_ref()
  }

  /// Returns the module's name.
  #[inline]
  pub const fn name(&self) -> &Name<S, Span> {
    &self.name
  }

  /// Returns the module's items, in source order.
  #[inline]
  pub fn items(&self) -> &[TypeSystemItem<S, Span, Ty>] {
    self.items.as_slice()
  }

  /// Consumes this module and returns its items.
  #[inline]
  pub fn into_items(self) -> Nested<TypeSystemItem<S, Span, Ty>> {
    self.items
  }
}

impl<S, Span, Ty> AsSpan<Span> for ModDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for ModDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S, Span, Ty> IntoComponents for ModDefinition<S, Span, Ty> {
  type Components = (
    Span,
    Option<StringValue<S, Span>>,
    Name<S, Span>,
    Nested<TypeSystemItem<S, Span, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.description, self.name, self.items)
  }
}

/// A top-level GraphQLx executable entry: a `use` item or an executable definition.
///
/// An executable document holds no module: `mod` is not one of its heads.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum UseOrExecutableDefinition<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// A described executable definition.
  Definition(DescribedExecutableDefinition<S, Span, Ty>),
}

impl<S, Span, Ty> UseOrExecutableDefinition<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Definition(definition) => definition.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for UseOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for UseOrExecutableDefinition<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Definition(definition) => definition.into_span(),
    }
  }
}

/// A complete top-level GraphQLx entry: a `use` item, an inline module, an executable or
/// type-system definition, or a type-system extension.
#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum DocumentItem<S, Span = SimpleSpan, Ty = Type<S, Span>> {
  /// A `use` item.
  Use(UseDefinition<S, Span>),
  /// An inline module, `mod name { … }`, whose body holds type-system items only.
  Mod(ModDefinition<S, Span, Ty>),
  /// A described executable or type-system definition.
  Definition(DescribedDefinition<S, Span, Ty>),
  /// A type-system extension.
  Extension(TypeSystemExtension<S, Span, Ty>),
}

impl<S, Span, Ty> DocumentItem<S, Span, Ty> {
  /// Returns the complete top-level entry span.
  #[inline]
  pub fn span(&self) -> &Span {
    match self {
      Self::Use(item) => item.span(),
      Self::Mod(module) => module.span(),
      Self::Definition(definition) => definition.span(),
      Self::Extension(extension) => extension.span(),
    }
  }
}

impl<S, Span, Ty> AsSpan<Span> for DocumentItem<S, Span, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Span, Ty> IntoSpan<Span> for DocumentItem<S, Span, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Use(item) => item.into_span(),
      Self::Mod(module) => module.into_span(),
      Self::Definition(definition) => definition.into_span(),
      Self::Extension(extension) => extension.into_span(),
    }
  }
}

/// A GraphQLx type-system document, whose entries are [`TypeSystemItem`]s.
pub type TypeSystemDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<TypeSystemItem<S, Span, Ty>>,
> = crate::executable::Document<TypeSystemItem<S, Span, Ty>, Span, Container>;

/// A GraphQLx executable document, whose entries are `use` items and executable definitions.
pub type ExecutableDocument<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<UseOrExecutableDefinition<S, Span, Ty>>,
> = crate::executable::Document<UseOrExecutableDefinition<S, Span, Ty>, Span, Container>;

/// A complete GraphQLx document, whose entries may be `use` items, inline modules, executable
/// definitions, type-system definitions and type-system extensions.
pub type Document<
  S,
  Span = SimpleSpan,
  Ty = Type<S, Span>,
  Container = DefaultVec<DocumentItem<S, Span, Ty>>,
> = crate::executable::Document<DocumentItem<S, Span, Ty>, Span, Container>;
