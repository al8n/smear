//! Executable-document carriers shared by the GraphQL-family dialect ASTs.
//!
//! Dialect AST modules specialize these data-only structures to their own names,
//! values, directives, and keyword node types.

use core::{
  marker::PhantomData,
  ops::{Deref, DerefMut},
};

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use std::vec::Vec;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A node with an optional leading description.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Described<T, Description, Span = SimpleSpan> {
  span: Span,
  description: Option<Description>,
  node: T,
}

impl<T, Description, Span> Described<T, Description, Span> {
  /// Creates a described node.
  #[inline]
  pub const fn new(span: Span, description: Option<Description>, node: T) -> Self {
    Self {
      span,
      description,
      node,
    }
  }

  /// Returns the span covering the description and node.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional description.
  #[inline]
  pub const fn description(&self) -> Option<&Description> {
    self.description.as_ref()
  }

  /// Returns the described node.
  #[inline]
  pub const fn node(&self) -> &T {
    &self.node
  }
}

impl<T, Description, Span> Deref for Described<T, Description, Span> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.node()
  }
}

impl<T, Description, Span> DerefMut for Described<T, Description, Span> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.node
  }
}

impl<T, Description, Span> AsSpan<Span> for Described<T, Description, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<T, Description, Span> IntoSpan<Span> for Described<T, Description, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<T, Description, Span> IntoComponents for Described<T, Description, Span> {
  type Components = (Span, Option<Description>, T);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.description, self.node)
  }
}

/// A variable definition (`Variable : Type DefaultValue? Directives?`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableDefinition<Variable, Type, DefaultValue, Directives, Span = SimpleSpan> {
  span: Span,
  variable: Variable,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Variable, Type, DefaultValue, Directives, Span>
  VariableDefinition<Variable, Type, DefaultValue, Directives, Span>
{
  /// Creates a variable definition in source order.
  #[inline]
  pub const fn new(
    span: Span,
    variable: Variable,
    ty: Type,
    default_value: Option<DefaultValue>,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      variable,
      ty,
      default_value,
      directives,
    }
  }

  /// Returns the span covering the whole definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the variable.
  #[inline]
  pub const fn variable(&self) -> &Variable {
    &self.variable
  }

  /// Returns the declared type.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns the optional constant default value.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// Returns the nonempty constant directive collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

impl<Variable, Type, DefaultValue, Directives, Span> AsSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Variable, Type, DefaultValue, Directives, Span> IntoSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Variable, Type, DefaultValue, Directives, Span> IntoComponents
  for VariableDefinition<Variable, Type, DefaultValue, Directives, Span>
{
  type Components = (
    Span,
    Variable,
    Type,
    Option<DefaultValue>,
    Option<Directives>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.variable,
      self.ty,
      self.default_value,
      self.directives,
    )
  }
}

/// A variable-definition collection.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct VariablesDefinition<
  VariableDefinition,
  Span = SimpleSpan,
  Container = Vec<VariableDefinition>,
> {
  span: Span,
  variable_definitions: Container,
  _definition: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Span, Container> VariablesDefinition<VariableDefinition, Span, Container> {
  /// Creates a variable-definition collection.
  #[inline]
  pub const fn new(span: Span, variable_definitions: Container) -> Self {
    Self {
      span,
      variable_definitions,
      _definition: PhantomData,
    }
  }

  /// Returns the span including surrounding parentheses when present.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed variable definitions.
  #[inline]
  pub fn variable_definitions(&self) -> &[VariableDefinition]
  where
    Container: AsRef<[VariableDefinition]>,
  {
    self.variable_definitions.as_ref()
  }

  /// Consumes this collection and returns its variable definitions.
  #[inline]
  pub fn into_variable_definitions(self) -> Container {
    self.variable_definitions
  }
}

impl<VariableDefinition, Span, Container> AsSpan<Span>
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<VariableDefinition, Span, Container> IntoSpan<Span>
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<VariableDefinition, Span, Container> IntoComponents
  for VariablesDefinition<VariableDefinition, Span, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.variable_definitions)
  }
}

/// A named fragment definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FragmentDefinition<
  FragmentName,
  TypeCondition,
  Directives,
  SelectionSet,
  Span = SimpleSpan,
> {
  span: Span,
  name: FragmentName,
  type_condition: TypeCondition,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Span>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Span>
{
  /// Creates a fragment definition.
  #[inline]
  pub const fn new(
    span: Span,
    name: FragmentName,
    type_condition: TypeCondition,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      span,
      name,
      type_condition,
      directives,
      selection_set,
    }
  }

  /// Returns the span covering the entire definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the fragment name.
  #[inline]
  pub const fn name(&self) -> &FragmentName {
    &self.name
  }

  /// Returns the required type condition.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition {
    &self.type_condition
  }

  /// Returns the nonempty directive collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the required selection set.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Span> AsSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Span> IntoSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, Span> IntoComponents
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    FragmentName,
    TypeCondition,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

/// A named operation definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NamedOperationDefinition<
  Name,
  OperationType,
  VariablesDefinition,
  Directives,
  SelectionSet,
  Span = SimpleSpan,
> {
  span: Span,
  operation_type: OperationType,
  name: Option<Name>,
  variable_definitions: Option<VariablesDefinition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Span>
  NamedOperationDefinition<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Span>
{
  /// Creates a named operation definition.
  #[inline]
  pub const fn new(
    span: Span,
    operation_type: OperationType,
    name: Option<Name>,
    variable_definitions: Option<VariablesDefinition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      span,
      operation_type,
      name,
      variable_definitions,
      directives,
      selection_set,
    }
  }

  /// Returns the span covering the entire operation.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the operation keyword node.
  #[inline]
  pub const fn operation_type(&self) -> &OperationType {
    &self.operation_type
  }

  /// Returns the optional operation name.
  #[inline]
  pub const fn name(&self) -> Option<&Name> {
    self.name.as_ref()
  }

  /// Returns the nonempty variables collection, if present.
  #[inline]
  pub const fn variable_definitions(&self) -> Option<&VariablesDefinition> {
    self.variable_definitions.as_ref()
  }

  /// Returns the nonempty directive collection, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the required operation selection set.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Span> AsSpan<Span>
  for NamedOperationDefinition<
    Name,
    OperationType,
    VariablesDefinition,
    Directives,
    SelectionSet,
    Span,
  >
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Span> IntoSpan<Span>
  for NamedOperationDefinition<
    Name,
    OperationType,
    VariablesDefinition,
    Directives,
    SelectionSet,
    Span,
  >
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, OperationType, VariablesDefinition, Directives, SelectionSet, Span> IntoComponents
  for NamedOperationDefinition<
    Name,
    OperationType,
    VariablesDefinition,
    Directives,
    SelectionSet,
    Span,
  >
{
  type Components = (
    Span,
    OperationType,
    Option<Name>,
    Option<VariablesDefinition>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.operation_type,
      self.name,
      self.variable_definitions,
      self.directives,
      self.selection_set,
    )
  }
}

/// A GraphQL-family document.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Document<Definition, Span = SimpleSpan, Container = Vec<Definition>> {
  span: Span,
  definitions: Container,
  _definition: PhantomData<Definition>,
}

impl<Definition, Span, Container> Document<Definition, Span, Container> {
  /// Creates a document from its span and definitions.
  #[inline]
  pub const fn new(span: Span, definitions: Container) -> Self {
    Self {
      span,
      definitions,
      _definition: PhantomData,
    }
  }

  /// Returns the complete document span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the parsed definitions.
  #[inline]
  pub fn definitions(&self) -> &[Definition]
  where
    Container: AsRef<[Definition]>,
  {
    self.definitions.as_ref()
  }

  /// Consumes this document and returns its parsed definitions.
  #[inline]
  pub fn into_definitions(self) -> Container {
    self.definitions
  }
}

impl<Definition, Span, Container> AsSpan<Span> for Document<Definition, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Definition, Span, Container> IntoSpan<Span> for Document<Definition, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Definition, Span, Container> IntoComponents for Document<Definition, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.definitions)
  }
}

/// A GraphQL-family definition: either type-system or executable syntax.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Definition<TypeSystem, Executable> {
  /// A type-system definition.
  TypeSystem(TypeSystem),
  /// An executable definition.
  Executable(Executable),
}

impl<TypeSystem, Executable, Span> AsSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: AsSpan<Span>,
  Executable: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::TypeSystem(value) => value.as_span(),
      Self::Executable(value) => value.as_span(),
    }
  }
}

impl<TypeSystem, Executable, Span> IntoSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: IntoSpan<Span>,
  Executable: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::TypeSystem(value) => value.into_span(),
      Self::Executable(value) => value.into_span(),
    }
  }
}

impl<TypeSystem, Executable> Definition<TypeSystem, Executable> {
  /// Returns the span of the selected definition arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    TypeSystem: AsSpan<Span>,
    Executable: AsSpan<Span>,
  {
    self.as_span()
  }
}

/// A GraphQL-family definition with its optional description, or a type-system
/// extension that cannot carry one.
#[derive(Debug, Clone, PartialEq, Eq, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum DefinitionOrExtension<Definition, Extension> {
  /// A definition, including its optional description.
  Definition(Definition),
  /// A type-system extension.
  Extension(Extension),
}

impl<Definition, Extension, Span> AsSpan<Span> for DefinitionOrExtension<Definition, Extension>
where
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(value) => value.as_span(),
      Self::Extension(value) => value.as_span(),
    }
  }
}

impl<Definition, Extension, Span> IntoSpan<Span> for DefinitionOrExtension<Definition, Extension>
where
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(value) => value.into_span(),
      Self::Extension(value) => value.into_span(),
    }
  }
}

impl<Definition, Extension> DefinitionOrExtension<Definition, Extension> {
  /// Returns the span of the selected document-entry arm.
  #[inline]
  pub fn span<Span>(&self) -> &Span
  where
    Definition: AsSpan<Span>,
    Extension: AsSpan<Span>,
  {
    self.as_span()
  }
}

#[cfg(test)]
mod tests {
  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{
    Described, Document, FragmentDefinition, NamedOperationDefinition, VariableDefinition,
    VariablesDefinition,
  };

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct CustomSpan(u8);

  struct ArrayBacked<T, const N: usize>([T; N]);

  impl<T, const N: usize> AsRef<[T]> for ArrayBacked<T, N> {
    fn as_ref(&self) -> &[T] {
      &self.0
    }
  }

  #[test]
  fn carriers_support_custom_spans() {
    let variable = VariableDefinition::<_, _, _, _, CustomSpan>::new(
      CustomSpan(1),
      "$id",
      "ID!",
      Some("= 1"),
      Some("@skip"),
    );
    assert_eq!(variable.as_span(), &CustomSpan(1));
    assert_eq!(
      variable.into_components(),
      (CustomSpan(1), "$id", "ID!", Some("= 1"), Some("@skip"))
    );

    let described = Described::<_, _, CustomSpan>::new(CustomSpan(2), Some("desc"), "node");
    assert_eq!(described.description(), Some(&"desc"));
    assert_eq!(described.into_span(), CustomSpan(2));

    let variables = VariablesDefinition::<&str, CustomSpan, _>::new(CustomSpan(3), ["$id"]);
    assert_eq!(variables.variable_definitions(), &["$id"]);

    let fragment = FragmentDefinition::<_, _, _, _, CustomSpan>::new(
      CustomSpan(4),
      "Part",
      "on User",
      None::<&str>,
      "{ id }",
    );
    assert_eq!(fragment.selection_set(), &"{ id }");

    let named = NamedOperationDefinition::<_, _, _, _, _, CustomSpan>::new(
      CustomSpan(5),
      "query",
      Some("Q"),
      None::<&str>,
      Some("@cache"),
      "{ id }",
    );
    assert_eq!(named.name(), Some(&"Q"));

    let document = Document::<&str, CustomSpan, _>::new(CustomSpan(6), ["query { id }"]);
    assert_eq!(document.as_span(), &CustomSpan(6));
    assert_eq!(
      document.into_components(),
      (CustomSpan(6), ["query { id }"])
    );
  }

  #[test]
  fn collection_accessors_project_array_backed_containers_to_slices() {
    let variables =
      VariablesDefinition::<u8, CustomSpan, _>::new(CustomSpan(1), ArrayBacked([1_u8, 2]));
    let projected: &[u8] = variables.variable_definitions();
    assert_eq!(projected, &[1, 2]);
    assert_eq!(variables.into_variable_definitions().0, [1, 2]);

    let document = Document::<u8, CustomSpan, _>::new(CustomSpan(2), ArrayBacked([3_u8, 4]));
    let projected: &[u8] = document.definitions();
    assert_eq!(projected, &[3, 4]);
    assert_eq!(document.into_definitions().0, [3, 4]);
  }
}
