use super::{fields_definition::FieldsDefinition, ty::Path, *};
use derive_more::{From, Into};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

type InterfaceTypeDefinitionAlias<S, Ty = Type<S>> = scaffold::InterfaceTypeDefinition<
  DefinitionName<S, Ty>,
  ImplementInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

type InterfaceTypeExtensionAlias<S, Ty = Type<S>> = scaffold::InterfaceTypeExtension<
  ExtensionName<S>,
  ImplementInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

/// An interface type definition with an optional description.
pub type DescribedInterfaceTypeDefinition<S, Ty = Type<S>> =
  Described<InterfaceTypeDefinition<S, Ty>, S>;

/// A GraphQLx interface type definition.
#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeDefinition<S, Ty = Type<S>>(InterfaceTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InterfaceTypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span { self.0.as_span() }
}

impl<S, Ty> IntoSpan<Span> for InterfaceTypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span { self.0.into_span() }
}

impl<S, Ty> IntoComponents for InterfaceTypeDefinition<S, Ty> {
  type Components = (Span, Ident<S>, Option<DefinitionTypeGenerics<S, Ty>>, Option<ImplementInterfaces<S, Ty>>, Option<ConstDirectives<S, Ty>>, Option<WhereClause<S, Ty>>, Option<super::FieldsDefinition<S, Ty>>);
  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, interfaces, directives, fields) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    let (where_clause, fields) = match fields {
      Some(fields_def) => { let (_, wc, f) = fields_def.into_components(); (wc, Some(f)) }
      None => (None, None),
    };
    (span, name, generics, interfaces, directives, where_clause, fields)
  }
}

impl<S, Ty> InterfaceTypeDefinition<S, Ty> {
  /// Returns the span.
  #[inline] pub const fn span(&self) -> &Span { self.0.span() }
  /// Returns the name.
  #[inline] pub const fn name(&self) -> &Ident<S> { self.0.name().name() }
  /// Returns the type generics, if any.
  #[inline] pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> { self.0.name().generics() }
  /// Returns the implemented interfaces, if any.
  #[inline] pub const fn implements(&self) -> Option<&ImplementInterfaces<S, Ty>> { self.0.implements() }
  /// Returns the directives, if any.
  #[inline] pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> { self.0.directives() }
  /// Returns the where clause, if any.
  #[inline] pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> { match self.0.fields_definition() { Some(fd) => fd.where_clause(), None => None } }
  /// Returns the fields definition, if any.
  #[inline] pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> { match self.0.fields_definition() { Some(fd) => Some(fd.fields()), None => None } }
}

/// A GraphQLx interface type extension.
#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeExtension<S, Ty = Type<S>>(InterfaceTypeExtensionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InterfaceTypeExtension<S, Ty> {
  #[inline] fn as_span(&self) -> &Span { self.0.as_span() }
}
impl<S, Ty> IntoSpan<Span> for InterfaceTypeExtension<S, Ty> {
  #[inline] fn into_span(self) -> Span { self.0.into_span() }
}

impl<S, Ty> IntoComponents for InterfaceTypeExtension<S, Ty> {
  type Components = (Span, Path<S>, Option<ExtensionTypeGenerics<S>>, Option<ImplementInterfaces<S, Ty>>, Option<ConstDirectives<S, Ty>>, Option<WhereClause<S, Ty>>, Option<super::FieldsDefinition<S, Ty>>);
  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, data) = self.0.into_components();
    let (_, path, generics) = name.into_components();
    match data {
      scaffold::InterfaceTypeExtensionData::Directives { implements, directives } => (span, path, generics, implements, Some(directives), None, None),
      scaffold::InterfaceTypeExtensionData::Fields { implements, directives, fields } => {
        let (_, wc, f) = fields.into_components();
        (span, path, generics, implements, directives, wc, Some(f))
      }
      scaffold::InterfaceTypeExtensionData::Implements(impls) => (span, path, generics, Some(impls), None, None, None),
    }
  }
}

impl<S, Ty> InterfaceTypeExtension<S, Ty> {
  /// Returns the span.
  #[inline] pub const fn span(&self) -> &Span { self.0.span() }
  /// Returns the path.
  #[inline] pub const fn path(&self) -> &Path<S> { self.0.name().path() }
  /// Returns the type generics, if any.
  #[inline] pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> { self.0.name().generics() }
  /// Returns the implemented interfaces, if any.
  #[inline] pub const fn implements(&self) -> Option<&ImplementInterfaces<S, Ty>> { self.0.implements() }
  /// Returns the directives, if any.
  #[inline] pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> { self.0.directives() }
  /// Returns the where clause, if any.
  #[inline] pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> { match self.0.fields_definition() { Some(fd) => fd.where_clause(), None => None } }
  /// Returns the fields definition, if any.
  #[inline] pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> { match self.0.fields_definition() { Some(fd) => Some(fd.fields()), None => None } }
}
