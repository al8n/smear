//! Generic-definition carriers shared by extended GraphQL-family ASTs.
//!
//! These nodes deliberately contain no GraphQLx marker or lexer-specific
//! source type. Dialect assemblies bind names, paths, and recursive type
//! references through aliases, keeping the generic grammar reusable and every
//! span independently configurable.

use core::marker::PhantomData;

use std::vec::Vec;
use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

/// A declared generic type parameter with an optional default type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeParam<Name, Type, Span = SimpleSpan> {
  span: Span,
  name: Name,
  default: Option<Type>,
}

impl<Name, Type, Span> DefinitionTypeParam<Name, Type, Span> {
  /// Creates a declared generic parameter.
  #[inline]
  pub const fn new(span: Span, name: Name, default: Option<Type>) -> Self {
    Self {
      span,
      name,
      default,
    }
  }

  /// Returns the complete parameter span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the declared parameter name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional default type.
  #[inline]
  pub const fn default(&self) -> Option<&Type> {
    self.default.as_ref()
  }
}

impl<Name, Type, Span> AsSpan<Span> for DefinitionTypeParam<Name, Type, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Type, Span> IntoSpan<Span> for DefinitionTypeParam<Name, Type, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Type, Span> IntoComponents for DefinitionTypeParam<Name, Type, Span> {
  type Components = (Span, Name, Option<Type>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.default)
  }
}

/// An angle-delimited list of declared generic type parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionTypeGenerics<Parameter, Span = SimpleSpan, Container = Vec<Parameter>> {
  span: Span,
  params: Container,
  _parameter: PhantomData<Parameter>,
}

impl<Parameter, Span, Container> DefinitionTypeGenerics<Parameter, Span, Container> {
  /// Creates a declared generic-parameter list.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _parameter: PhantomData,
    }
  }

  /// Returns the span covering the enclosing angle brackets.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the declared parameters as a slice.
  #[inline]
  pub fn params(&self) -> &[Parameter]
  where
    Container: AsRef<[Parameter]>,
  {
    self.params.as_ref()
  }

  /// Consumes the list and returns its parameter container.
  #[inline]
  pub fn into_params(self) -> Container {
    self.params
  }
}

impl<Parameter, Span, Container> AsSpan<Span>
  for DefinitionTypeGenerics<Parameter, Span, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Parameter, Span, Container> IntoSpan<Span>
  for DefinitionTypeGenerics<Parameter, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Parameter, Span, Container> IntoComponents
  for DefinitionTypeGenerics<Parameter, Span, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

/// A generic argument written on a type-system extension.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeParam<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
}

impl<Name, Span> ExtensionTypeParam<Name, Span> {
  /// Creates an extension generic argument.
  #[inline]
  pub const fn new(span: Span, name: Name) -> Self {
    Self { span, name }
  }

  /// Returns the complete argument span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the argument name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name, Span> AsSpan<Span> for ExtensionTypeParam<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for ExtensionTypeParam<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span> IntoComponents for ExtensionTypeParam<Name, Span> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

/// An angle-delimited list of generic arguments on a type-system extension.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionTypeGenerics<Parameter, Span = SimpleSpan, Container = Vec<Parameter>> {
  span: Span,
  params: Container,
  _parameter: PhantomData<Parameter>,
}

impl<Parameter, Span, Container> ExtensionTypeGenerics<Parameter, Span, Container> {
  /// Creates an extension generic-argument list.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _parameter: PhantomData,
    }
  }

  /// Returns the span covering the enclosing angle brackets.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extension generic arguments as a slice.
  #[inline]
  pub fn params(&self) -> &[Parameter]
  where
    Container: AsRef<[Parameter]>,
  {
    self.params.as_ref()
  }

  /// Consumes the list and returns its parameter container.
  #[inline]
  pub fn into_params(self) -> Container {
    self.params
  }
}

impl<Parameter, Span, Container> AsSpan<Span>
  for ExtensionTypeGenerics<Parameter, Span, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Parameter, Span, Container> IntoSpan<Span>
  for ExtensionTypeGenerics<Parameter, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Parameter, Span, Container> IntoComponents
  for ExtensionTypeGenerics<Parameter, Span, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

/// An angle-delimited list of generic names in executable syntax.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionTypeGenerics<Name, Span = SimpleSpan, Container = Vec<Name>> {
  span: Span,
  params: Container,
  _name: PhantomData<Name>,
}

impl<Name, Span, Container> ExecutableDefinitionTypeGenerics<Name, Span, Container> {
  /// Creates an executable generic-name list.
  #[inline]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _name: PhantomData,
    }
  }

  /// Returns the span covering the enclosing angle brackets.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the executable generic names as a slice.
  #[inline]
  pub fn params(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.params.as_ref()
  }

  /// Consumes the list and returns its name container.
  #[inline]
  pub fn into_params(self) -> Container {
    self.params
  }
}

impl<Name, Span, Container> AsSpan<Span>
  for ExecutableDefinitionTypeGenerics<Name, Span, Container>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span, Container> IntoSpan<Span>
  for ExecutableDefinitionTypeGenerics<Name, Span, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span, Container> IntoComponents
  for ExecutableDefinitionTypeGenerics<Name, Span, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.params)
  }
}

/// A type-system definition name with optional declared generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefinitionName<Name, Generics, Span = SimpleSpan> {
  span: Span,
  name: Name,
  generics: Option<Generics>,
}

impl<Name, Generics, Span> DefinitionName<Name, Generics, Span> {
  /// Creates a definition name.
  #[inline]
  pub const fn new(span: Span, name: Name, generics: Option<Generics>) -> Self {
    Self {
      span,
      name,
      generics,
    }
  }

  /// Returns the complete name span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the definition name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional declared generic parameters.
  #[inline]
  pub const fn generics(&self) -> Option<&Generics> {
    self.generics.as_ref()
  }
}

impl<Name, Generics, Span> AsSpan<Span> for DefinitionName<Name, Generics, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Generics, Span> IntoSpan<Span> for DefinitionName<Name, Generics, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Generics, Span> IntoComponents for DefinitionName<Name, Generics, Span> {
  type Components = (Span, Name, Option<Generics>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.generics)
  }
}

/// A type-system extension path with optional generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionName<Path, Generics, Span = SimpleSpan> {
  span: Span,
  path: Path,
  generics: Option<Generics>,
}

impl<Path, Generics, Span> ExtensionName<Path, Generics, Span> {
  /// Creates an extension name.
  #[inline]
  pub const fn new(span: Span, path: Path, generics: Option<Generics>) -> Self {
    Self {
      span,
      path,
      generics,
    }
  }

  /// Returns the complete name span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the extension path.
  #[inline]
  pub const fn path(&self) -> &Path {
    &self.path
  }

  /// Returns the optional generic arguments.
  #[inline]
  pub const fn generics(&self) -> Option<&Generics> {
    self.generics.as_ref()
  }
}

impl<Path, Generics, Span> AsSpan<Span> for ExtensionName<Path, Generics, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Path, Generics, Span> IntoSpan<Span> for ExtensionName<Path, Generics, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Path, Generics, Span> IntoComponents for ExtensionName<Path, Generics, Span> {
  type Components = (Span, Path, Option<Generics>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.path, self.generics)
  }
}

/// An executable definition name with optional generic names.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionName<Name, Generics, Span = SimpleSpan> {
  span: Span,
  name: Name,
  generics: Option<Generics>,
}

impl<Name, Generics, Span> ExecutableDefinitionName<Name, Generics, Span> {
  /// Creates an executable definition name.
  #[inline]
  pub const fn new(span: Span, name: Name, generics: Option<Generics>) -> Self {
    Self {
      span,
      name,
      generics,
    }
  }

  /// Returns the complete name span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the executable definition name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the optional generic names.
  #[inline]
  pub const fn generics(&self) -> Option<&Generics> {
    self.generics.as_ref()
  }
}

impl<Name, Generics, Span> AsSpan<Span> for ExecutableDefinitionName<Name, Generics, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Generics, Span> IntoSpan<Span> for ExecutableDefinitionName<Name, Generics, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Generics, Span> IntoComponents for ExecutableDefinitionName<Name, Generics, Span> {
  type Components = (Span, Name, Option<Generics>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.generics)
  }
}

/// The leading generic declaration and name of an executable definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableDefinitionHeader<ImplementationGenerics, Name, Span = SimpleSpan> {
  span: Span,
  implementation_generics: Option<ImplementationGenerics>,
  name: Name,
}

impl<ImplementationGenerics, Name, Span>
  ExecutableDefinitionHeader<ImplementationGenerics, Name, Span>
{
  /// Creates an executable-definition header.
  #[inline]
  pub const fn new(
    span: Span,
    implementation_generics: Option<ImplementationGenerics>,
    name: Name,
  ) -> Self {
    Self {
      span,
      implementation_generics,
      name,
    }
  }

  /// Returns the complete header span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional leading generic declaration.
  #[inline]
  pub const fn implementation_generics(&self) -> Option<&ImplementationGenerics> {
    self.implementation_generics.as_ref()
  }

  /// Returns the executable definition name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<ImplementationGenerics, Name, Span> AsSpan<Span>
  for ExecutableDefinitionHeader<ImplementationGenerics, Name, Span>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<ImplementationGenerics, Name, Span> IntoSpan<Span>
  for ExecutableDefinitionHeader<ImplementationGenerics, Name, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<ImplementationGenerics, Name, Span> IntoComponents
  for ExecutableDefinitionHeader<ImplementationGenerics, Name, Span>
{
  type Components = (Span, Option<ImplementationGenerics>, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.implementation_generics, self.name)
  }
}

/// A path with optional recursive generic type arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePath<Path, Generics, Span = SimpleSpan> {
  span: Span,
  path: Path,
  generics: Option<Generics>,
}

impl<Path, Generics, Span> TypePath<Path, Generics, Span> {
  /// Creates a generic-capable type path.
  #[inline]
  pub const fn new(span: Span, path: Path, generics: Option<Generics>) -> Self {
    Self {
      span,
      path,
      generics,
    }
  }

  /// Returns the complete type-path span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the namespaced path.
  #[inline]
  pub const fn path(&self) -> &Path {
    &self.path
  }

  /// Returns the optional generic type arguments.
  #[inline]
  pub const fn type_generics(&self) -> Option<&Generics> {
    self.generics.as_ref()
  }
}

impl<Path, Generics, Span> AsSpan<Span> for TypePath<Path, Generics, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Path, Generics, Span> IntoSpan<Span> for TypePath<Path, Generics, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Path, Generics, Span> IntoComponents for TypePath<Path, Generics, Span> {
  type Components = (Span, Path, Option<Generics>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.path, self.generics)
  }
}

/// A predicate that constrains one type path to satisfy one or more bounds.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<TypePath, Span = SimpleSpan, Container = Vec<TypePath>> {
  span: Span,
  bounded_type: TypePath,
  bounds: Container,
}

impl<TypePath, Span, Container> WherePredicate<TypePath, Span, Container> {
  /// Creates a type-bound predicate.
  #[inline]
  pub const fn new(span: Span, bounded_type: TypePath, bounds: Container) -> Self {
    Self {
      span,
      bounded_type,
      bounds,
    }
  }

  /// Returns the complete predicate span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the path being constrained.
  #[inline]
  pub const fn bounded_type(&self) -> &TypePath {
    &self.bounded_type
  }

  /// Returns the bound paths as a slice.
  #[inline]
  pub fn bounds(&self) -> &[TypePath]
  where
    Container: AsRef<[TypePath]>,
  {
    self.bounds.as_ref()
  }

  /// Consumes the predicate and returns its bound container.
  #[inline]
  pub fn into_bounds(self) -> Container {
    self.bounds
  }
}

impl<TypePath, Span, Container> AsSpan<Span> for WherePredicate<TypePath, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<TypePath, Span, Container> IntoSpan<Span> for WherePredicate<TypePath, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<TypePath, Span, Container> IntoComponents for WherePredicate<TypePath, Span, Container> {
  type Components = (Span, TypePath, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.bounded_type, self.bounds)
  }
}

/// A `where` clause containing one or more type-bound predicates.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause<Predicate, Span = SimpleSpan, Container = Vec<Predicate>> {
  span: Span,
  predicates: Container,
  _predicate: PhantomData<Predicate>,
}

impl<Predicate, Span, Container> WhereClause<Predicate, Span, Container> {
  /// Creates a `where` clause.
  #[inline]
  pub const fn new(span: Span, predicates: Container) -> Self {
    Self {
      span,
      predicates,
      _predicate: PhantomData,
    }
  }

  /// Returns the complete clause span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the predicates as a slice.
  #[inline]
  pub fn predicates(&self) -> &[Predicate]
  where
    Container: AsRef<[Predicate]>,
  {
    self.predicates.as_ref()
  }

  /// Consumes the clause and returns its predicate container.
  #[inline]
  pub fn into_predicates(self) -> Container {
    self.predicates
  }
}

impl<Predicate, Span, Container> AsSpan<Span> for WhereClause<Predicate, Span, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Predicate, Span, Container> IntoSpan<Span> for WhereClause<Predicate, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Predicate, Span, Container> IntoComponents for WhereClause<Predicate, Span, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.predicates)
  }
}

/// A grammar node associated with an optional `where` clause.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constrained<Target, WhereClause, Span = SimpleSpan> {
  span: Span,
  where_clause: Option<WhereClause>,
  target: Target,
}

impl<Target, WhereClause, Span> Constrained<Target, WhereClause, Span> {
  /// Creates a constrained grammar node.
  #[inline]
  pub const fn new(span: Span, where_clause: Option<WhereClause>, target: Target) -> Self {
    Self {
      span,
      where_clause,
      target,
    }
  }

  /// Returns the complete constrained-node span.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional `where` clause.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause> {
    self.where_clause.as_ref()
  }

  /// Returns the constrained target.
  #[inline]
  pub const fn target(&self) -> &Target {
    &self.target
  }
}

impl<Target, WhereClause, Span> AsSpan<Span> for Constrained<Target, WhereClause, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Target, WhereClause, Span> IntoSpan<Span> for Constrained<Target, WhereClause, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Target, WhereClause, Span> IntoComponents for Constrained<Target, WhereClause, Span> {
  type Components = (Span, Option<WhereClause>, Target);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.where_clause, self.target)
  }
}

#[cfg(test)]
mod tests;
