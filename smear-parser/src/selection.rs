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

impl<Name, Span> IntoComponents for Alias<Name, Span> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
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

impl<Name, Span> IntoComponents for TypeCondition<Name, Span> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
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

impl<FragmentName, Directives, Span> IntoComponents
  for FragmentSpread<FragmentName, Directives, Span>
{
  type Components = (Span, FragmentName, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives)
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

impl<TypeCondition, Directives, SelectionSet, Span> IntoComponents
  for InlineFragment<TypeCondition, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<TypeCondition>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
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

impl<Selection, Span, Container> IntoComponents for SelectionSet<Selection, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.selections)
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

impl<Alias, Name, Arguments, Directives, SelectionSet, Span> IntoComponents
  for Field<Alias, Name, Arguments, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<Alias>,
    Name,
    Option<Arguments>,
    Option<Directives>,
    Option<SelectionSet>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.alias,
      self.name,
      self.arguments,
      self.directives,
      self.selection_set,
    )
  }
}

#[cfg(test)]
mod tests {
  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{Alias, Field, FragmentSpread, InlineFragment, SelectionSet, TypeCondition};

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  /// A span of the test's own, taking the obligation in the one line a consumer would write. Every
  /// selection carrier owns its span by value and releases it where it finds it, so the span
  /// answers the same question the value and type carriers' does (`al8n/smear#176`).
  impl crate::value::Leaf for CustomSpan {}

  struct ArrayBacked<T, const N: usize>([T; N]);

  impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
    fn as_ref(&self) -> &[T] {
      &self.0
    }
  }

  #[test]
  fn carriers_support_custom_spans() {
    let alias = Alias::<_, CustomSpan>::new(CustomSpan(1), "alias");
    assert_eq!(alias.as_span(), &CustomSpan(1));
    assert_eq!(alias.into_components(), (CustomSpan(1), "alias"));

    let condition = TypeCondition::<_, CustomSpan>::new(CustomSpan(2), "User");
    assert_eq!(condition.into_span(), CustomSpan(2));

    let set = SelectionSet::<&str, CustomSpan, _>::new(CustomSpan(3), ["field"]);
    assert_eq!(set.selections(), &["field"]);

    let spread = FragmentSpread::<_, _, CustomSpan>::new(CustomSpan(4), "part", Some("@skip"));
    assert_eq!(spread.directives(), Some(&"@skip"));

    let inline = InlineFragment::<_, _, _, CustomSpan>::new(
      CustomSpan(5),
      Some("on User"),
      None::<&str>,
      "{ field }",
    );
    assert_eq!(inline.type_condition(), Some(&"on User"));

    let field = Field::<_, _, _, _, _, CustomSpan>::new(
      CustomSpan(6),
      Some("alias"),
      "field",
      Some("(arg: 1)"),
      None::<&str>,
      Some("{ child }"),
    );
    assert_eq!(field.name(), &"field");
    assert_eq!(field.into_span(), CustomSpan(6));
  }

  #[test]
  fn selections_project_array_backed_containers_to_slices() {
    let selections = SelectionSet::<u8, CustomSpan, _>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
    let projected: &[u8] = selections.selections();
    assert_eq!(projected, &[1, 2]);
    assert_eq!(selections.into_selections().0, [1, 2]);
  }
}
