use super::{fields_definition::FieldsDefinition, ty::Path, *};
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type ObjectTypeDefinitionAlias<S, Ty = Type<S>> = scaffold::ObjectTypeDefinition<
  DefinitionName<S, Ty>,
  ImplementsInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

type ObjectTypeExtensionAlias<S, Ty = Type<S>> = scaffold::ObjectTypeExtension<
  ExtensionName<S>,
  ImplementsInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

/// An object type definition with an optional description.
///
/// ## Grammar
///
/// ```text
/// DescribedObjectTypeDefinition : Description? ObjectTypeDefinition
/// ```
pub type DescribedObjectTypeDefinition<S, Ty = Type<S>> = Described<ObjectTypeDefinition<S, Ty>, S>;

/// A GraphQLx object type definition.
///
/// Represents a concrete type that defines a set of named fields and can
/// implement one or more interfaces. Supports generic type parameters and
/// where clauses for type constraints.
///
/// ## Grammar
///
/// ```text
/// ObjectTypeDefinition :
///   type Name TypeGenerics? ImplementsInterfaces? Directives? WhereClause? FieldsDefinition?
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct ObjectTypeDefinition<S, Ty = Type<S>>(ObjectTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for ObjectTypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for ObjectTypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for ObjectTypeDefinition<S, Ty> {
  type Components = (
    Span,
    Ident<S>,
    Option<DefinitionTypeGenerics<S, Ty>>,
    Option<ImplementsInterfaces<S, Ty>>,
    Option<ConstDirectives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    Option<super::FieldsDefinition<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, interfaces, directives, fields) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    let (where_clause, fields) = match fields {
      Some(fields_def) => {
        let (_, where_clause, fields) = fields_def.into_components();
        (where_clause, Some(fields))
      }
      None => (None, None),
    };
    (
      span,
      name,
      generics,
      interfaces,
      directives,
      where_clause,
      fields,
    )
  }
}

impl<S, Ty> ObjectTypeDefinition<S, Ty> {
  /// Returns the span of the object type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the object type definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().name()
  }

  /// Returns the type generics of the object type definition, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> {
    self.0.name().generics()
  }

  /// Returns the implemented interfaces of the object type definition, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces<S, Ty>> {
    self.0.implements()
  }

  /// Returns the directives of the object type definition, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the where clause of the object type definition, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the object type definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for ObjectTypeDefinition<S, Ty>
where
  ObjectTypeDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    ObjectTypeDefinitionAlias::parser::<E>().map(Self)
  }
}

/// A GraphQLx object type extension.
///
/// Extends an existing object type by adding directives, implementing
/// additional interfaces, or adding fields. Supports generic type parameters.
///
/// ## Grammar
///
/// ```text
/// ObjectTypeExtension :
///   extend type Path TypeGenerics? ImplementsInterfaces? Directives
///   extend type Path TypeGenerics? ImplementsInterfaces? WhereClause? FieldsDefinition
///   extend type Path TypeGenerics? ImplementsInterfaces
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct ObjectTypeExtension<S, Ty = Type<S>>(ObjectTypeExtensionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for ObjectTypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for ObjectTypeExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for ObjectTypeExtension<S, Ty> {
  type Components = (
    Span,
    Path<S>,
    Option<ExtensionTypeGenerics<S>>,
    Option<ImplementsInterfaces<S, Ty>>,
    Option<ConstDirectives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    Option<super::FieldsDefinition<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, data) = self.0.into_components();

    let (_, path, generics) = name.into_components();
    match data {
      scaffold::ObjectTypeExtensionData::Directives {
        implements,
        directives,
      } => (
        span,
        path,
        generics,
        implements,
        Some(directives),
        None,
        None,
      ),
      scaffold::ObjectTypeExtensionData::Fields {
        implements,
        directives,
        fields,
      } => {
        let (_, where_clause, fields) = fields.into_components();
        (
          span,
          path,
          generics,
          implements,
          directives,
          where_clause,
          Some(fields),
        )
      }
      scaffold::ObjectTypeExtensionData::Implements(impls) => {
        (span, path, generics, Some(impls), None, None, None)
      }
    }
  }
}

impl<S, Ty> ObjectTypeExtension<S, Ty> {
  /// Returns the span of the object type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the object type extension.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the generics of the object type extension, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the implemented interfaces of the object type extension, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces<S, Ty>> {
    self.0.implements()
  }

  /// Returns the directives of the object type extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the where clause of the object type extension, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the object type extension, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for ObjectTypeExtension<S, Ty>
where
  ObjectTypeExtensionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    ObjectTypeExtensionAlias::parser::<E>().map(Self)
  }
}
