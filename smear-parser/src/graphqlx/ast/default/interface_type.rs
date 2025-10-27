use super::{fields_definition::FieldsDefinition, ty::Path, *};
use derive_more::{From, Into};
use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type InterfaceTypeDefinitionAlias<S, Ty = Type<S>> = scaffold::InterfaceTypeDefinition<
  DefinitionName<S, Ty>,
  ImplementsInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

type InterfaceTypeExtensionAlias<S, Ty = Type<S>> = scaffold::InterfaceTypeExtension<
  ExtensionName<S>,
  ImplementsInterfaces<S, Ty>,
  ConstDirectives<S, Ty>,
  FieldsDefinition<S, Ty>,
>;

/// An interface type definition with an optional description.
///
/// ## Grammar
///
/// ```text
/// DescribedInterfaceTypeDefinition : Description? InterfaceTypeDefinition
/// ```
pub type DescribedInterfaceTypeDefinition<S, Ty = Type<S>> =
  Described<InterfaceTypeDefinition<S, Ty>, S>;

/// A GraphQLx interface type definition.
///
/// Represents an abstract type that defines a set of fields which object
/// types can implement. Supports generic type parameters, interface
/// inheritance, and where clauses.
///
/// ## Grammar
///
/// ```text
/// InterfaceTypeDefinition :
///   interface Name TypeGenerics? ImplementsInterfaces? Directives? WhereClause? FieldsDefinition?
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeDefinition<S, Ty = Type<S>>(InterfaceTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InterfaceTypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InterfaceTypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InterfaceTypeDefinition<S, Ty> {
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

impl<S, Ty> InterfaceTypeDefinition<S, Ty> {
  /// Returns the span of the interface type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the interface type definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().name()
  }

  /// Returns the type generics of the interface type definition, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> {
    self.0.name().generics()
  }

  /// Returns the implemented interfaces of the interface type definition, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces<S, Ty>> {
    self.0.implements()
  }

  /// Returns the directives of the interface type definition, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the where clause of the interface type definition, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the interface type definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InterfaceTypeDefinition<S, Ty>
where
  InterfaceTypeDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InterfaceTypeDefinitionAlias::parser::<E>().map(Self)
  }
}

/// A GraphQLx interface type extension.
///
/// Extends an existing interface type by adding directives, implementing
/// additional interfaces, or adding fields. Supports generic type parameters.
///
/// ## Grammar
///
/// ```text
/// InterfaceTypeExtension :
///   extend interface Path TypeGenerics? ImplementsInterfaces? Directives
///   extend interface Path TypeGenerics? ImplementsInterfaces? WhereClause? FieldsDefinition
///   extend interface Path TypeGenerics? ImplementsInterfaces
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeExtension<S, Ty = Type<S>>(InterfaceTypeExtensionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InterfaceTypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InterfaceTypeExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InterfaceTypeExtension<S, Ty> {
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
      scaffold::InterfaceTypeExtensionData::Directives {
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
      scaffold::InterfaceTypeExtensionData::Fields {
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
      scaffold::InterfaceTypeExtensionData::Implements(impls) => {
        (span, path, generics, Some(impls), None, None, None)
      }
    }
  }
}

impl<S, Ty> InterfaceTypeExtension<S, Ty> {
  /// Returns the span of the interface type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the interface type extension.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the generics of the interface type extension, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the implemented interfaces of the interface type extension, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementsInterfaces<S, Ty>> {
    self.0.implements()
  }

  /// Returns the directives of the interface type extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the where clause of the interface type extension, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the interface type extension, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InterfaceTypeExtension<S, Ty>
where
  InterfaceTypeExtensionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InterfaceTypeExtensionAlias::parser::<E>().map(Self)
  }
}
