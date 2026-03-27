use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents},
};
use core::marker::PhantomData;
use std::vec::Vec;

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};


use super::Path;

/// A GraphQLx imported item, potentially with an alias.
///
/// ## Grammar
///
/// ```graphqlx
/// namedSpecifier
///     : identifier ("as" path)?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedSpecifier<Ident, Container = Vec<Ident>> {
  span: Span,
  name: Ident,
  alias: Option<Path<Ident, Container>>,
}

impl<Ident, Container> AsSpan<Span> for NamedSpecifier<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for NamedSpecifier<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for NamedSpecifier<Ident, Container> {
  type Components = (Span, Ident, Option<Path<Ident, Container>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.alias)
  }
}

impl<Ident, Container> NamedSpecifier<Ident, Container> {
  /// Creates a new named item with the given span, name, and optional alias.
  #[inline]
  pub const fn new(span: Span, name: Ident, alias: Option<Path<Ident, Container>>) -> Self {
    Self { span, name, alias }
  }

  /// Returns the span of the entire named item.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name of the imported item.
  #[inline]
  pub const fn name(&self) -> &Ident {
    &self.name
  }

  /// Returns the optional alias for the imported item.
  #[inline]
  pub const fn alias(&self) -> Option<&Path<Ident, Container>> {
    self.alias.as_ref()
  }
}

/// A glob import item, representing a wildcard import.
///
/// ## Grammar
///
/// ```graphqlx
/// wildcardSpecifier
///     : "*" ("as" path)?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildcardSpecifier<Ident, Container = Vec<Ident>> {
  span: Span,
  alias: Option<Path<Ident, Container>>,
}

impl<Ident, Container> AsSpan<Span> for WildcardSpecifier<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for WildcardSpecifier<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, Container> IntoComponents for WildcardSpecifier<Ident, Container> {
  type Components = (Span, Option<Path<Ident, Container>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.alias)
  }
}

impl<Ident, Container> WildcardSpecifier<Ident, Container> {
  /// Creates a new wildcard item with the given span and optional alias.
  #[inline]
  pub const fn new(span: Span, alias: Option<Path<Ident, Container>>) -> Self {
    Self { span, alias }
  }

  /// Returns the span of the entire wildcard item.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional alias for the wildcard import.
  #[inline]
  pub const fn alias(&self) -> Option<&Path<Ident, Container>> {
    self.alias.as_ref()
  }
}

/// An imported item, which can be either a named item or a wildcard item.
///
/// ## Grammar
///
/// ```graphqlx
/// importedMember
///    : namedSpecifier | wildcardSpecifier
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Unwrap, TryUnwrap, IsVariant)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportMember<Ident, Container = Vec<Ident>> {
  /// A named import specifier, potentially with an alias.
  Named(NamedSpecifier<Ident, Container>),
  /// A wildcard import specifier, potentially with an alias.
  Wildcard(WildcardSpecifier<Ident, Container>),
}

impl<Ident, Container> AsSpan<Span> for ImportMember<Ident, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, Container> IntoSpan<Span> for ImportMember<Ident, Container> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Named(item) => item.into_span(),
      Self::Wildcard(item) => item.into_span(),
    }
  }
}

impl<Ident, Container> ImportMember<Ident, Container> {
  /// Returns the span of the imported item.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Named(item) => item.span(),
      Self::Wildcard(item) => item.span(),
    }
  }

  /// Returns the optional alias for the imported item.
  #[inline]
  pub const fn alias(&self) -> Option<&Path<Ident, Container>> {
    match self {
      Self::Named(item) => item.alias(),
      Self::Wildcard(item) => item.alias(),
    }
  }
}

/// A GraphQLx brace import statement.
///
/// ## Grammar
///
/// ```graphqlx
/// importList
///    : "{" importedMember* "}"
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportList<
  Ident,
  PathContainer = Vec<Ident>,
  Container = Vec<ImportMember<Ident, PathContainer>>,
> {
  span: Span,
  items: Container,
  _m: PhantomData<Ident>,
  _p: PhantomData<PathContainer>,
}

impl<Ident, PathContainer, Container> core::ops::Deref
  for ImportList<Ident, PathContainer, Container>
{
  type Target = Container;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.items
  }
}

impl<Ident, PathContainer, Container> AsSpan<Span> for ImportList<Ident, PathContainer, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, PathContainer, Container> IntoSpan<Span>
  for ImportList<Ident, PathContainer, Container>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, PathContainer, Container> IntoComponents
  for ImportList<Ident, PathContainer, Container>
{
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.items)
  }
}

impl<Ident, PathContainer, Container> ImportList<Ident, PathContainer, Container> {
  /// Creates a new brace items with the given span and items.
  #[inline]
  pub const fn new(span: Span, items: Container) -> Self {
    Self {
      span,
      items,
      _m: PhantomData,
      _p: PhantomData,
    }
  }

  /// Returns the span of the brace items.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the items contained within the braces.
  #[inline]
  pub const fn items(&self) -> &Container {
    &self.items
  }

  /// Returns the slice of items contained within the braces.
  #[inline]
  pub fn items_slice(&self) -> &[ImportMember<Ident>]
  where
    Container: AsRef<[ImportMember<Ident>]>,
  {
    self.items().as_ref()
  }
}

/// An import specifier, which can be either a brace import or a wildcard import.
///
/// ## Grammar
///
/// ```graphqlx
/// importClause
///   : importList | wildcardItem
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Unwrap, TryUnwrap, IsVariant)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ImportClause<
  Ident,
  PathContainer = Vec<Ident>,
  ItemContainer = Vec<ImportMember<Ident, PathContainer>>,
> {
  /// A brace import containing multiple named or wildcard specifiers.
  List(ImportList<Ident, PathContainer, ItemContainer>),
  /// A wildcard import specifier.
  Wildcard(WildcardSpecifier<Ident, PathContainer>),
}

impl<Ident, PathContainer, ItemContainer> AsSpan<Span>
  for ImportClause<Ident, PathContainer, ItemContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, PathContainer, ItemContainer> IntoSpan<Span>
  for ImportClause<Ident, PathContainer, ItemContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::List(b) => b.into_span(),
      Self::Wildcard(w) => w.into_span(),
    }
  }
}

impl<Ident, PathContainer, ItemContainer> ImportClause<Ident, PathContainer, ItemContainer> {
  /// Returns the span of the import specifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::List(b) => b.span(),
      Self::Wildcard(w) => w.span(),
    }
  }
}

/// A complete import definition in GraphQLx.
///
/// ## Grammar
///
/// ```graphqlx
/// importDefinition
///  : "import" importClause "from" filePath
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportDefinition<
  Ident,
  FilePath,
  PathContainer = Vec<Ident>,
  ItemContainer = Vec<ImportMember<Ident, PathContainer>>,
> {
  span: Span,
  file: FilePath,
  clause: ImportClause<Ident, PathContainer, ItemContainer>,
}

impl<Ident, FilePath, PathContainer, ItemContainer> AsSpan<Span>
  for ImportDefinition<Ident, FilePath, PathContainer, ItemContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Ident, FilePath, PathContainer, ItemContainer> IntoSpan<Span>
  for ImportDefinition<Ident, FilePath, PathContainer, ItemContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Ident, FilePath, PathContainer, ItemContainer> IntoComponents
  for ImportDefinition<Ident, FilePath, PathContainer, ItemContainer>
{
  type Components = (
    Span,
    FilePath,
    ImportClause<Ident, PathContainer, ItemContainer>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.file, self.clause)
  }
}

impl<Ident, FilePath, PathContainer, ItemContainer>
  ImportDefinition<Ident, FilePath, PathContainer, ItemContainer>
{
  /// Creates a new import definition with the given span, file path, and clause.
  #[inline]
  pub const fn new(
    span: Span,
    file: FilePath,
    clause: ImportClause<Ident, PathContainer, ItemContainer>,
  ) -> Self {
    Self { span, file, clause }
  }

  /// Returns the span of the entire import definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the file path being imported.
  #[inline]
  pub const fn file_path(&self) -> &FilePath {
    &self.file
  }

  /// Returns the import clause (either list or wildcard).
  #[inline]
  pub const fn clause(&self) -> &ImportClause<Ident, PathContainer, ItemContainer> {
    &self.clause
  }
}
