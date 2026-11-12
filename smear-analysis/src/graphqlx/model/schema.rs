//! The schema, and the built-in definitions a consumer keys on.

use crate::graphqlx::id::{ApplicationId, DefId, TypeExprId};

use super::{
  Model,
  def::{Contributions, Primitive},
  name::Description,
  table::{BUILTIN_DIRECTIVES, BUILTIN_TYPES},
};

/// One of the three root operations a schema has a type for.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum RootOperation {
  /// `query`.
  Query,
  /// `mutation`.
  Mutation,
  /// `subscription`.
  Subscription,
}

impl RootOperation {
  /// Its slot among the schema's roots.
  pub(super) const fn index(self) -> usize {
    match self {
      Self::Query => 0,
      Self::Mutation => 1,
      Self::Subscription => 2,
    }
  }
}

/// A root operation's type, and how the schema came to have it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Root {
  /// Written in a schema definition or extension, as `query: Q` is: the type occurrence, which is
  /// that occurrence's `Unresolved` when it did not resolve. It may be an instantiation.
  Written(TypeExprId),
  /// Presumed, with no schema definition, from the definition GraphQL's default root names.
  Presumed(DefId),
}

impl Model {
  /// The schema: the root package's, whether or not it writes one.
  #[inline]
  pub fn schema(&self) -> Schema<'_> {
    Schema { model: self }
  }

  /// The built-in definitions a consumer keys on instead of spellings.
  #[inline]
  pub fn builtins(&self) -> Builtins<'_> {
    Builtins { model: self }
  }
}

/// The schema of the model: its roots, its applications, its description and its contributions.
///
/// `Copy`: it holds the model, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Schema<'a> {
  model: &'a Model,
}

impl<'a> Schema<'a> {
  /// The type of the root `operation`, or `None` when the schema has none for it.
  #[inline]
  pub fn root(&self, operation: RootOperation) -> Option<Root> {
    self.model.schema.roots[operation.index()]
  }

  /// Its directive applications, its schema definition's and then each extension's.
  #[inline]
  pub fn applications(&self) -> &'a [ApplicationId] {
    let model: &'a Model = self.model;
    model.schema.applications.of(&model.listed_applications)
  }

  /// Its description: the one the root package's first schema definition writes, if it writes one.
  #[inline]
  pub fn description(&self) -> Option<Description<'a>> {
    Description::of(self.model, self.model.schema.description)
  }

  /// What each schema item any package writes contributed to it: every schema definition, by
  /// source id and position, then every schema extension, likewise; none when no package writes
  /// one. Each says whether it is an extension. The root package's first schema definition gives
  /// the schema its description, roots and applications, and the root package's schema extensions
  /// give it roots and applications; any other schema item is a contribution and nothing else,
  /// there for the rules that refuse it.
  #[inline]
  pub fn contributions(&self) -> Contributions<'a> {
    Contributions::of(self.model, self.model.schema.contributions)
  }
}

impl core::fmt::Debug for Schema<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Schema")
      .field("query", &self.root(RootOperation::Query))
      .field("mutation", &self.root(RootOperation::Mutation))
      .field("subscription", &self.root(RootOperation::Subscription))
      .field("applications", &self.applications())
      .finish()
  }
}

/// The built-in definitions of the model that a consumer keys on instead of matching spellings.
///
/// Each is a definition of `core`, `alloc` or `std`, found once, when the model is built, so that
/// nothing after it matches a spelling. A GraphQL built-in the root package replaces keeps its
/// built-in's id, so it is still the one answered here.
///
/// # Panics
///
/// Each accessor but [`primitive`](Self::primitive) panics when the model holds no such
/// definition, which no model an analysis answers does: smear's built-in library defines every one,
/// and every analysis reads it.
///
/// `Copy`: it holds the model.
#[derive(Clone, Copy)]
pub struct Builtins<'a> {
  model: &'a Model,
}

impl Builtins<'_> {
  /// `@marker`.
  #[inline]
  pub fn marker(&self) -> DefId {
    self.directive(0)
  }

  /// `@set`.
  #[inline]
  pub fn set(&self) -> DefId {
    self.directive(1)
  }

  /// `@map`.
  #[inline]
  pub fn map(&self) -> DefId {
    self.directive(2)
  }

  /// `@extern`.
  #[inline]
  pub fn r#extern(&self) -> DefId {
    self.directive(3)
  }

  /// `@repr`.
  #[inline]
  pub fn repr(&self) -> DefId {
    self.directive(4)
  }

  /// `@heap`.
  #[inline]
  pub fn heap(&self) -> DefId {
    self.directive(5)
  }

  /// `@deprecated`.
  #[inline]
  pub fn deprecated(&self) -> DefId {
    self.directive(6)
  }

  /// `@specifiedBy`.
  #[inline]
  pub fn specified_by(&self) -> DefId {
    self.directive(7)
  }

  /// `@oneOf`.
  #[inline]
  pub fn one_of(&self) -> DefId {
    self.directive(8)
  }

  /// `@skip`.
  #[inline]
  pub fn skip(&self) -> DefId {
    self.directive(9)
  }

  /// `@include`.
  #[inline]
  pub fn include(&self) -> DefId {
    self.directive(10)
  }

  /// `TypeRef`, the type of a value that names a definition.
  #[inline]
  pub fn type_ref(&self) -> DefId {
    self.ty(0)
  }

  /// `MapKey`, the marker a map's key type must satisfy.
  #[inline]
  pub fn map_key(&self) -> DefId {
    self.ty(1)
  }

  /// `NonZero`.
  #[inline]
  pub fn non_zero(&self) -> DefId {
    self.ty(2)
  }

  /// `string`, GraphQL's `String`.
  #[inline]
  pub fn string(&self) -> DefId {
    self.ty(3)
  }

  /// `bytes`.
  #[inline]
  pub fn bytes(&self) -> DefId {
    self.ty(4)
  }

  /// `id`, GraphQL's `ID`.
  #[inline]
  pub fn id(&self) -> DefId {
    self.ty(5)
  }

  /// The definition `core` materialises for `primitive`, or `None` when no package writes it
  /// where it may have it: `core` holds a primitive's definition only when a program names it, and
  /// a width beyond `core`'s own only when a package whose integer provider gives it names it.
  #[inline]
  pub fn primitive(&self, primitive: Primitive) -> Option<DefId> {
    self
      .model
      .primitives
      .iter()
      .find(|entry: &&(Primitive, DefId)| entry.0 == primitive)
      .map(|entry: &(Primitive, DefId)| entry.1)
  }

  /// The built-in directive in `slot` of the model's built-in table.
  fn directive(&self, slot: usize) -> DefId {
    match self.model.builtins.directives[slot] {
      Some(found) => found,
      None => panic!(
        "the model holds no built-in directive `@{}`",
        BUILTIN_DIRECTIVES[slot]
      ),
    }
  }

  /// The built-in type in `slot` of the model's built-in table.
  fn ty(&self, slot: usize) -> DefId {
    match self.model.builtins.types[slot] {
      Some(found) => found,
      None => panic!("the model holds no built-in type `{}`", BUILTIN_TYPES[slot]),
    }
  }
}

impl core::fmt::Debug for Builtins<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Builtins")
      .field("directives", &self.model.builtins.directives)
      .field("types", &self.model.builtins.types)
      .field("primitives", &self.model.primitives)
      .finish()
  }
}
