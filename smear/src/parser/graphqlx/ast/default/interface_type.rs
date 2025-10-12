use super::{fields_definition::FieldsDefinition, ty::Path, *};
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type InterfaceTypeDefinitionAlias<S> = scaffold::InterfaceTypeDefinition<
  DefinitionName<S>,
  ImplementInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

type InterfaceTypeExtensionAlias<S> = scaffold::InterfaceTypeExtension<
  ExtensionName<S>,
  ImplementInterfaces<S>,
  ConstDirectives<S>,
  FieldsDefinition<S>,
>;

pub type DescribedInterfaceTypeDefinition<S> = Described<InterfaceTypeDefinition<S>, S>;

#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeDefinition<S>(InterfaceTypeDefinitionAlias<S>);

impl<S> AsSpan<Span> for InterfaceTypeDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for InterfaceTypeDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for InterfaceTypeDefinition<S> {
  type Components = (
    Span,
    Ident<S>,
    Option<DefinitionTypeGenerics<S>>,
    Option<ImplementInterfaces<S>>,
    Option<ConstDirectives<S>>,
    Option<WhereClause<S>>,
    Option<super::FieldsDefinition<S>>,
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

impl<S> InterfaceTypeDefinition<S> {
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
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the implemented interfaces of the object type definition, if any.
  #[inline]
  pub const fn implements(&self) -> Option<&ImplementInterfaces<S>> {
    self.0.implements()
  }

  /// Returns the directives of the object type definition, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S>> {
    self.0.directives()
  }

  /// Returns the where clause of the object type definition, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the object type definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for InterfaceTypeDefinition<S>
where
  InterfaceTypeDefinitionAlias<S>: Parseable<'a, I, T, Error>,
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

#[derive(Debug, Clone, From, Into)]
pub struct InterfaceTypeExtension<S>(InterfaceTypeExtensionAlias<S>);

impl<S> AsSpan<Span> for InterfaceTypeExtension<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for InterfaceTypeExtension<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for InterfaceTypeExtension<S> {
  type Components = (
    Span,
    Path<S>,
    Option<ExtensionTypeGenerics<S>>,
    Option<ImplementInterfaces<S>>,
    Option<ConstDirectives<S>>,
    Option<WhereClause<S>>,
    Option<super::FieldsDefinition<S>>,
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

impl<S> InterfaceTypeExtension<S> {
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
  pub const fn implements(&self) -> Option<&ImplementInterfaces<S>> {
    self.0.implements()
  }

  /// Returns the directives of the object type extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S>> {
    self.0.directives()
  }

  /// Returns the where clause of the object type extension, if any.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the fields definition of the object type extension, if any.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::FieldsDefinition<S>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for InterfaceTypeExtension<S>
where
  InterfaceTypeExtensionAlias<S>: Parseable<'a, I, T, Error>,
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
