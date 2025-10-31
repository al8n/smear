use super::{input_fields_definition::InputFieldsDefinition, ty::Path, *};
use derive_more::{From, Into};
use logosky::{
  LogoStream, Logos, Source, Token,
  chumsky::{Parseable, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type InputObjectTypeDefinitionAlias<S, Ty = Type<S>> = scaffold::InputObjectTypeDefinition<
  DefinitionName<S, Ty>,
  ConstDirectives<S, Ty>,
  InputFieldsDefinition<S, Ty>,
>;

type InputObjectTypeExtensionAlias<S, Ty = Type<S>> = scaffold::InputObjectTypeExtension<
  ExtensionName<S>,
  ConstDirectives<S, Ty>,
  InputFieldsDefinition<S, Ty>,
>;

/// An input object type definition with an optional description.
///
/// ## Grammar
///
/// ```text
/// DescribedInputObjectTypeDefinition : Description? InputObjectTypeDefinition
/// ```
pub type DescribedInputObjectTypeDefinition<S, Ty = Type<S>> =
  Described<InputObjectTypeDefinition<S, Ty>, S>;

/// A GraphQLx input object type definition.
///
/// Represents an input object type used for complex input values in mutations
/// and queries. Supports generic type parameters and where clauses.
///
/// ## Grammar
///
/// ```text
/// InputObjectTypeDefinition :
///   input Name TypeGenerics? Directives? WhereClause? InputFieldsDefinition?
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct InputObjectTypeDefinition<S, Ty = Type<S>>(InputObjectTypeDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InputObjectTypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InputObjectTypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InputObjectTypeDefinition<S, Ty> {
  type Components = (
    Span,
    Ident<S>,
    Option<DefinitionTypeGenerics<S, Ty>>,
    Option<ConstDirectives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    Option<super::InputFieldsDefinition<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives, fields) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    let (where_clause, fields) = match fields {
      Some(fields_def) => {
        let (_, where_clause, fields) = fields_def.into_components();
        (where_clause, Some(fields))
      }
      None => (None, None),
    };
    (span, name, generics, directives, where_clause, fields)
  }
}

impl<S, Ty> InputObjectTypeDefinition<S, Ty> {
  /// Returns the span of the input object type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the name of the input object type definition.
  #[inline]
  pub const fn name(&self) -> &Ident<S> {
    self.0.name().name()
  }

  /// Returns the optional generics of the input object type definition.
  #[inline]
  pub const fn type_generics(&self) -> Option<&DefinitionTypeGenerics<S, Ty>> {
    self.0.name().generics()
  }

  /// Returns the optional directives of the input object type definition.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the optional where clause of the input object type definition.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the optional input fields definition of the input object type definition.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::InputFieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InputObjectTypeDefinition<S, Ty>
where
  InputObjectTypeDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InputObjectTypeDefinitionAlias::parser().map(Self)
  }
}

/// A GraphQLx input object type extension.
///
/// Extends an existing input object type by adding directives or additional
/// input fields. Supports generic type parameters.
///
/// ## Grammar
///
/// ```text
/// InputObjectTypeExtension :
///   extend input Path TypeGenerics? Directives
///   extend input Path TypeGenerics? WhereClause? InputFieldsDefinition
/// ```
#[derive(Debug, Clone, From, Into)]
pub struct InputObjectTypeExtension<S, Ty = Type<S>>(InputObjectTypeExtensionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InputObjectTypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InputObjectTypeExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InputObjectTypeExtension<S, Ty> {
  type Components = (
    Span,
    Path<S>,
    Option<ExtensionTypeGenerics<S>>,
    Option<ConstDirectives<S, Ty>>,
    Option<WhereClause<S, Ty>>,
    Option<super::InputFieldsDefinition<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, data) = self.0.into_components();
    let (_, name, generics) = name.into_components();
    match data {
      scaffold::InputObjectTypeExtensionData::Directives(directive) => {
        (span, name, generics, Some(directive), None, None)
      }
      scaffold::InputObjectTypeExtensionData::Fields { directives, fields } => {
        let (_, where_clause, fields) = fields.into_components();
        (span, name, generics, directives, where_clause, Some(fields))
      }
    }
  }
}

impl<S, Ty> InputObjectTypeExtension<S, Ty> {
  /// Returns the span of the input object type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the input object type extension.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the optional generics of the input object type extension.
  #[inline]
  pub const fn type_generics(&self) -> Option<&ExtensionTypeGenerics<S>> {
    self.0.name().generics()
  }

  /// Returns the optional directives of the input object type extension.
  #[inline]
  pub const fn directives(&self) -> Option<&ConstDirectives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the optional where clause of the input object type extension.
  #[inline]
  pub const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => fields_def.where_clause(),
      None => None,
    }
  }

  /// Returns the optional input fields definition of the input object type extension.
  #[inline]
  pub const fn fields_definition(&self) -> Option<&super::InputFieldsDefinition<S, Ty>> {
    match self.0.fields_definition() {
      Some(fields_def) => Some(fields_def.fields()),
      None => None,
    }
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InputObjectTypeExtension<S, Ty>
where
  InputObjectTypeExtensionAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InputObjectTypeExtensionAlias::parser().map(Self)
  }
}
