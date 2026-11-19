//! Selection-node carriers shared by the GraphQL-family dialect ASTs.
//!
//! The structures contain only grammar data; dialect AST modules bind names,
//! values, directives, and language markers through public aliases.

use core::{
  marker::PhantomData,
  ops::{Deref, DerefMut},
};

use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A field alias (`Name :`).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Alias<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
}

impl<Name, Span> Alias<Name, Span> {
  /// Creates a field alias.
  #[inline]
  pub const fn new(span: Span, name: Name) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self { span, name }
  }

  /// Returns the span covering the alias and its colon.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the alias name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name, Span> Deref for Alias<Name, Span> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name, Span> DerefMut for Alias<Name, Span> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.name
  }
}

impl<Name, Span> AsSpan<Span> for Alias<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for Alias<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `Alias` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AliasComponents<Name, Span> {
  /// The span covering the alias and its colon.
  pub span: Span,
  /// The alias name.
  pub name: Name,
}

impl<Name, Span> IntoComponents for Alias<Name, Span> {
  type Components = AliasComponents<Name, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    AliasComponents {
      span: self.span,
      name: self.name,
    }
  }
}

/// A fragment type condition (`on NamedType`).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeCondition<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
}

impl<Name, Span> TypeCondition<Name, Span> {
  /// Creates a fragment type condition.
  #[inline]
  pub const fn new(span: Span, name: Name) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self { span, name }
  }

  /// Returns the span covering `on` and the type name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the condition's type name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name, Span> AsSpan<Span> for TypeCondition<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for TypeCondition<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `TypeCondition` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeConditionComponents<Name, Span> {
  /// The span covering `on` and the type name.
  pub span: Span,
  /// The condition's type name.
  pub name: Name,
}

impl<Name, Span> IntoComponents for TypeCondition<Name, Span> {
  type Components = TypeConditionComponents<Name, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    TypeConditionComponents {
      span: self.span,
      name: self.name,
    }
  }
}

/// A named fragment spread (`... FragmentName Directives?`).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct FragmentSpread<FragmentName, Directives, Span = SimpleSpan> {
  span: Span,
  name: FragmentName,
  directives: Option<Directives>,
}

impl<FragmentName, Directives, Span> FragmentSpread<FragmentName, Directives, Span> {
  /// Creates a named fragment spread.
  #[inline]
  pub const fn new(span: Span, name: FragmentName, directives: Option<Directives>) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      name,
      directives,
    }
  }

  /// Returns the span covering the entire spread.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the spread fragment name.
  #[inline]
  pub const fn name(&self) -> &FragmentName {
    &self.name
  }

  /// Returns the nonempty directive collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<FragmentName, Directives, Span> AsSpan<Span>
  for FragmentSpread<FragmentName, Directives, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FragmentName, Directives, Span> IntoSpan<Span>
  for FragmentSpread<FragmentName, Directives, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `FragmentSpread` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FragmentSpreadComponents<FragmentName, Directives, Span> {
  /// The span covering the entire spread.
  pub span: Span,
  /// The spread fragment name.
  pub name: FragmentName,
  /// The nonempty directive collection, if present.
  pub directives: Option<Directives>,
}

impl<FragmentName, Directives, Span> IntoComponents
  for FragmentSpread<FragmentName, Directives, Span>
{
  type Components = FragmentSpreadComponents<FragmentName, Directives, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    FragmentSpreadComponents {
      span: self.span,
      name: self.name,
      directives: self.directives,
    }
  }
}

/// An inline fragment (`... TypeCondition? Directives? SelectionSet`).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct InlineFragment<TypeCondition, Directives, SelectionSet, Span = SimpleSpan> {
  span: Span,
  type_condition: Option<TypeCondition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<TypeCondition, Directives, SelectionSet, Span>
  InlineFragment<TypeCondition, Directives, SelectionSet, Span>
{
  /// Creates an inline fragment.
  #[inline]
  pub const fn new(
    span: Span,
    type_condition: Option<TypeCondition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      type_condition,
      directives,
      selection_set,
    }
  }

  /// Returns the span covering the entire inline fragment.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional type condition.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition> {
    self.type_condition.as_ref()
  }

  /// Returns the nonempty directive collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the required nested selection set.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<TypeCondition, Directives, SelectionSet, Span> AsSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<TypeCondition, Directives, SelectionSet, Span> IntoSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `InlineFragment` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InlineFragmentComponents<TypeCondition, Directives, SelectionSet, Span> {
  /// The span covering the entire inline fragment.
  pub span: Span,
  /// The optional type condition.
  pub type_condition: Option<TypeCondition>,
  /// The nonempty directive collection, if present.
  pub directives: Option<Directives>,
  /// The required nested selection set.
  pub selection_set: SelectionSet,
}

impl<TypeCondition, Directives, SelectionSet, Span> IntoComponents
  for InlineFragment<TypeCondition, Directives, SelectionSet, Span>
{
  type Components = InlineFragmentComponents<TypeCondition, Directives, SelectionSet, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    InlineFragmentComponents {
      span: self.span,
      type_condition: self.type_condition,
      directives: self.directives,
      selection_set: self.selection_set,
    }
  }
}

/// A nonempty GraphQL selection-set collection.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct SelectionSet<Selection, Span = SimpleSpan, Container = Vec<Selection>> {
  span: Span,
  selections: Container,
  _selection: PhantomData<Selection>,
}

impl<Selection, Span, Container> SelectionSet<Selection, Span, Container> {
  /// Creates a selection set from its complete delimiter span and selections.
  #[inline]
  pub const fn new(span: Span, selections: Container) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      selections,
      _selection: PhantomData,
    }
  }

  /// Returns the span including the surrounding braces.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the contained selections.
  #[inline]
  pub fn selections(&self) -> &[Selection]
  where
    Container: AsRef<[Selection]>,
  {
    self.selections.as_ref()
  }

  /// Consumes this node and returns its selections.
  #[inline]
  pub fn into_selections(self) -> Container {
    self.selections
  }
}

impl<Selection, Span, Container> AsSpan<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Selection, Span, Container> IntoSpan<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `SelectionSet` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SelectionSetComponents<Span, Container> {
  /// The span including the surrounding braces.
  pub span: Span,
  /// The contained selections.
  pub selections: Container,
}

impl<Selection, Span, Container> IntoComponents for SelectionSet<Selection, Span, Container> {
  type Components = SelectionSetComponents<Span, Container>;

  #[inline]
  fn into_components(self) -> Self::Components {
    SelectionSetComponents {
      span: self.span,
      selections: self.selections,
    }
  }
}

/// A field (`Alias? Name Arguments? Directives? SelectionSet?`).
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Field<Alias, Name, Arguments, Directives, SelectionSet, Span = SimpleSpan> {
  span: Span,
  alias: Option<Alias>,
  name: Name,
  arguments: Option<Arguments>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Span>
  Field<Alias, Name, Arguments, Directives, SelectionSet, Span>
{
  /// Creates a field from its parsed components.
  #[inline]
  pub const fn new(
    span: Span,
    alias: Option<Alias>,
    name: Name,
    arguments: Option<Arguments>,
    directives: Option<Directives>,
    selection_set: Option<SelectionSet>,
  ) -> Self
  where
    Span: crate::value::Leaf,
  {
    Self {
      span,
      alias,
      name,
      arguments,
      directives,
      selection_set,
    }
  }

  /// Returns the span covering the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional field alias.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias> {
    self.alias.as_ref()
  }

  /// Returns the required field name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the nonempty arguments collection, if present.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments> {
    self.arguments.as_ref()
  }

  /// Returns the nonempty directives collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the optional nested selection set.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Span> AsSpan<Span>
  for Field<Alias, Name, Arguments, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Span> IntoSpan<Span>
  for Field<Alias, Name, Arguments, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

/// `Field` taken apart: every field, in the order it is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldComponents<Alias, Name, Arguments, Directives, SelectionSet, Span> {
  /// The span covering the entire field.
  pub span: Span,
  /// The optional field alias.
  pub alias: Option<Alias>,
  /// The required field name.
  pub name: Name,
  /// The nonempty arguments collection, if present.
  pub arguments: Option<Arguments>,
  /// The nonempty directives collection, if present.
  pub directives: Option<Directives>,
  /// The optional nested selection set.
  pub selection_set: Option<SelectionSet>,
}

impl<Alias, Name, Arguments, Directives, SelectionSet, Span> IntoComponents
  for Field<Alias, Name, Arguments, Directives, SelectionSet, Span>
{
  type Components = FieldComponents<Alias, Name, Arguments, Directives, SelectionSet, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    FieldComponents {
      span: self.span,
      alias: self.alias,
      name: self.name,
      arguments: self.arguments,
      directives: self.directives,
      selection_set: self.selection_set,
    }
  }
}

#[cfg(test)]
mod tests;
