use super::{ty::Path, *};
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type UnionTypeDefinitionAlias<S> =
  scaffold::UnionTypeDefinition<DefinitionName<S>, ConstDirectives<S>, UnionMemberTypes<S>>;

type UnionTypeExtensionAlias<S> =
  scaffold::UnionTypeExtension<ExtensionName<S>, ConstDirectives<S>, UnionMemberTypes<S>>;

/// A union type definition with an optional description.
///
/// ## Grammar
///
/// ```text
/// DescribedUnionTypeDefinition : Description? UnionTypeDefinition
/// ```
pub type DescribedUnionTypeDefinition<S> = Described<UnionTypeDefinition<S>, S>;

/// Union member types with an optional where clause.
///
/// Groups the union member types with their associated generic constraints.
#[derive(Debug, Clone, From, Into)]
pub(super) struct UnionMemberTypes<S> {
  where_clause: Option<WhereClause<S>>,
  types: super::UnionMemberTypes<S>,
}

impl<S> IntoComponents for UnionMemberTypes<S> {
  type Components = (Option<WhereClause<S>>, super::UnionMemberTypes<S>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.where_clause, self.types)
  }
}

impl<S> UnionMemberTypes<S> {
  /// Returns the optional where clause of the union member types.
  #[inline]
  pub(super) const fn where_clause(&self) -> Option<&WhereClause<S>> {
    self.where_clause.as_ref()
  }

  /// Returns the union member types.
  #[inline]
  pub(super) const fn types(&self) -> &super::UnionMemberTypes<S> {
    &self.types
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for UnionMemberTypes<S>
where
  WhereClause<S>: Parseable<'a, I, T, Error>,
  super::UnionMemberTypes<S>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    super::UnionMemberTypes::parser()
      .then(WhereClause::parser().or_not())
      .map(|(types, where_clause)| Self {
        where_clause,
        types,
      })
  }
}

/// A GraphQLx union type definition.
///
/// Represents a union of multiple object types, where a field can return
/// any one of the union's member types. Supports generic type parameters
/// and where clauses for type constraints.
///
/// ## Grammar
///
/// ```text
/// UnionTypeDefinition :
///   union Name TypeGenerics? Directives? UnionMemberTypes? WhereClause?
///
/// UnionMemberTypes :
///   = |? TypePath (| TypePath)*
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct UnionTypeDefinition<S>(UnionTypeDefinitionAlias<S>);

impl<S> AsSpan<Span> for UnionTypeDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for UnionTypeDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for UnionTypeDefinition<S> {
  type Components = (
    Span,
    Ident<S>,
    Option<DefinitionTypeGenerics<S>>,
    Option<ConstDirectives<S>>,
    Option<super::UnionMemberTypes<S>>,
    Option<WhereClause<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives, types) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    let (where_clause, types) = match types {
      Some(types) => {
        let (where_clause, types) = types.into_components();
        (where_clause, Some(types))
      }
      None => (None, None),
    };
    (span, name, generics, directives, types, where_clause)
  }
}

impl<S> UnionTypeDefinition<S> {
  /// Returns the name of the union type definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().name()
  }

  /// Returns the optional directives of the union type definition.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S>> {
    self.0.directives()
  }

  /// Returns the optional union member types of the union type definition.
  #[inline]
  pub const fn members(&self) -> Option<&super::UnionMemberTypes<S>> {
    match self.0.member_types() {
      Some(members) => Some(members.types()),
      None => None,
    }
  }

  /// Returns the optional generics of the union type definition.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the optional where clause of the union type definition.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    match self.0.member_types() {
      Some(members) => members.where_clause(),
      None => None,
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for UnionTypeDefinition<S>
where
  UnionTypeDefinitionAlias<S>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    UnionTypeDefinitionAlias::parser::<E>().map(Self)
  }
}

/// A GraphQLx union type extension.
///
/// Extends an existing union type by adding directives or additional
/// member types. Supports generic type parameters.
///
/// ## Grammar
///
/// ```text
/// UnionTypeExtension :
///   extend union Path TypeGenerics? Directives WhereClause?
///   extend union Path TypeGenerics? UnionMemberTypes WhereClause?
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct UnionTypeExtension<S>(UnionTypeExtensionAlias<S>);

impl<S> AsSpan<Span> for UnionTypeExtension<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for UnionTypeExtension<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for UnionTypeExtension<S> {
  type Components = (
    Span,
    Path<S>,
    Option<ExtensionTypeGenerics<S>>,
    Option<ConstDirectives<S>>,
    Option<super::UnionMemberTypes<S>>,
    Option<WhereClause<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, data) = self.0.into_components();
    let (_, path, generics) = name.into_components();
    match data {
      scaffold::UnionTypeExtensionData::Directives(directives) => {
        (span, path, generics, Some(directives), None, None)
      }
      scaffold::UnionTypeExtensionData::Members {
        directives,
        members,
      } => {
        let (where_clause, members) = members.into_components();
        (
          span,
          path,
          generics,
          directives,
          Some(members),
          where_clause,
        )
      }
    }
  }
}

impl<S> UnionTypeExtension<S> {
  /// Returns the path of the union type extension.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the optional generics of the union type extension.
  #[inline]
  pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the optional directives of the union type extension.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S>> {
    self.0.directives()
  }

  /// Returns the optional union member types of the union type extension.
  #[inline]
  pub const fn members(&self) -> Option<&super::UnionMemberTypes<S>> {
    match self.0.member_types() {
      Some(members) => Some(members.types()),
      None => None,
    }
  }

  /// Returns the optional where clause of the union type extension.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    match self.0.member_types() {
      Some(members) => members.where_clause(),
      None => None,
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for UnionTypeExtension<S>
where
  UnionTypeExtensionAlias<S>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    UnionTypeExtensionAlias::parser::<E>().map(Self)
  }
}
