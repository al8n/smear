use super::{ty::Path, *};
use derive_more::{From, Into};
use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

type UnionTypeDefinitionAlias<S, Ty = Type<S>> = scaffold::UnionTypeDefinition<
  DefinitionName<S, Ty>,
  ConstDirectives<S, Ty>,
  UnionMemberTypesInner<S, Ty>,
>;

type UnionTypeExtensionAlias<S, Ty = Type<S>> =
  scaffold::UnionTypeExtension<ExtensionName<S>, ConstDirectives<S, Ty>, UnionMemberTypesInner<S, Ty>>;

/// A union type definition with an optional description.
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Union member types with an optional where clause.
#[derive(Debug, Clone, From, Into)]
pub(super) struct UnionMemberTypesInner<S, Ty = Type<S>> {
  where_clause: Option<WhereClause<S, Ty>>,
  types: super::UnionMemberTypes<S, Ty>,
}

impl<S, Ty> IntoComponents for UnionMemberTypesInner<S, Ty> {
  type Components = (Option<WhereClause<S, Ty>>, super::UnionMemberTypes<S, Ty>);
  #[inline]
  fn into_components(self) -> Self::Components {
    (self.where_clause, self.types)
  }
}

impl<S, Ty> UnionMemberTypesInner<S, Ty> {
  /// Returns the optional where clause.
  #[inline] pub(super) const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> { self.where_clause.as_ref() }
  /// Returns the union member types.
  #[inline] pub(super) const fn types(&self) -> &super::UnionMemberTypes<S, Ty> { &self.types }
}

/// A GraphQLx union type definition.
#[derive(Debug, Clone, From, Into)]
pub struct UnionTypeDefinition<S, Ty = Type<S>>(UnionTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for UnionTypeDefinition<S, Ty> {
  #[inline] fn as_span(&self) -> &Span { self.0.as_span() }
}
impl<S, Ty> IntoSpan<Span> for UnionTypeDefinition<S, Ty> {
  #[inline] fn into_span(self) -> Span { self.0.into_span() }
}

impl<S, Ty> IntoComponents for UnionTypeDefinition<S, Ty> {
  type Components = (Span, Ident<S>, Option<DefinitionTypeGenerics<S, Ty>>, Option<ConstDirectives<S, Ty>>, Option<super::UnionMemberTypes<S, Ty>>, Option<WhereClause<S, Ty>>);
  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives, types) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    let (where_clause, types) = match types {
      Some(t) => { let (wc, t) = t.into_components(); (wc, Some(t)) }
      None => (None, None),
    };
    (span, name, generics, directives, types, where_clause)
  }
}

impl<S, Ty> UnionTypeDefinition<S, Ty> {
  /// Returns the name.
  #[inline] pub const fn name(&self) -> &Ident<S> { self.0.name().name() }
  /// Returns the directives, if any.
  #[inline] pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> { self.0.directives() }
  /// Returns the union member types, if any.
  #[inline] pub const fn members(&self) -> Option<&super::UnionMemberTypes<S, Ty>> { match self.0.member_types() { Some(m) => Some(m.types()), None => None } }
  /// Returns the type generics, if any.
  #[inline] pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> { self.0.name().generics() }
  /// Returns the where clause, if any.
  #[inline] pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> { match self.0.member_types() { Some(m) => m.where_clause(), None => None } }
}

/// A GraphQLx union type extension.
#[derive(Debug, Clone, From, Into)]
pub struct UnionTypeExtension<S, Ty>(UnionTypeExtensionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for UnionTypeExtension<S, Ty> {
  #[inline] fn as_span(&self) -> &Span { self.0.as_span() }
}
impl<S, Ty> IntoSpan<Span> for UnionTypeExtension<S, Ty> {
  #[inline] fn into_span(self) -> Span { self.0.into_span() }
}

impl<S, Ty> IntoComponents for UnionTypeExtension<S, Ty> {
  type Components = (Span, Path<S>, Option<ExtensionTypeGenerics<S>>, Option<ConstDirectives<S, Ty>>, Option<super::UnionMemberTypes<S, Ty>>, Option<WhereClause<S, Ty>>);
  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, data) = self.0.into_components();
    let (_, path, generics) = name.into_components();
    match data {
      scaffold::UnionTypeExtensionData::Directives(d) => (span, path, generics, Some(d), None, None),
      scaffold::UnionTypeExtensionData::Members { directives, members } => {
        let (wc, m) = members.into_components();
        (span, path, generics, directives, Some(m), wc)
      }
    }
  }
}

impl<S, Ty> UnionTypeExtension<S, Ty> {
  /// Returns the path.
  #[inline] pub const fn path(&self) -> &Path<S> { self.0.name().path() }
  /// Returns the type generics, if any.
  #[inline] pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> { self.0.name().generics() }
  /// Returns the directives, if any.
  #[inline] pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> { self.0.directives() }
  /// Returns the union member types, if any.
  #[inline] pub const fn members(&self) -> Option<&super::UnionMemberTypes<S, Ty>> { match self.0.member_types() { Some(m) => Some(m.types()), None => None } }
  /// Returns the where clause, if any.
  #[inline] pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> { match self.0.member_types() { Some(m) => m.where_clause(), None => None } }
}
