use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{
    AsSpan, IntoSpan, Span, cmp::Equivalent, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use crate::{
  keywords::{
    self, ArgumentDefinitionLocation, EnumLocation, EnumValueLocation, FieldDefinitionLocation,
    FieldLocation, FragmentDefinitionLocation, FragmentSpreadLocation, InlineFragmentLocation,
    InputFieldDefinitionLocation, InputObjectLocation, InterfaceLocation, MutationLocation,
    ObjectLocation, QueryLocation, ScalarLocation, SchemaLocation, SubscriptionLocation,
    UnionLocation, VariableDefinitionLocation,
  },
  lexer::graphql::ast::AstLexerErrors,
};

use super::*;

/// Represents locations where directives can be applied during GraphQL execution.
///
/// Executable directive locations specify where directives can be used in GraphQL
/// operations (queries, mutations, subscriptions) and their components. These
/// directives are processed during query execution and can control field selection,
/// fragment inclusion, and operation behavior.
///
/// ## Examples
///
/// ```text
/// # Query operation directive
/// query @auth { user { name } }
///
/// # Field directive
/// { user { name @deprecated email } }
///
/// # Fragment spread directive
/// { ...UserFragment @include(if: $showUser) }
///
/// # Inline fragment directive
/// { ... on User @skip(if: $hideUser) { name } }
///
/// # Variable definition directive
/// query($id: ID! @validate(pattern: "^[0-9]+$")) { user(id: $id) { name } }
/// ```
///
/// Spec: [ExecutableDirectiveLocation](https://spec.graphql.org/draft/#ExecutableDirectiveLocation)
#[derive(Debug, Clone, Copy, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDirectiveLocation {
  /// `QUERY` - directive can be applied to query operations
  Query(QueryLocation),
  /// `MUTATION` - directive can be applied to mutation operations
  Mutation(MutationLocation),
  /// `SUBSCRIPTION` - directive can be applied to subscription operations
  Subscription(SubscriptionLocation),
  /// `FIELD` - directive can be applied to field selections
  Field(FieldLocation),
  /// `FRAGMENT_DEFINITION` - directive can be applied to fragment definitions
  FragmentDefinition(FragmentDefinitionLocation),
  /// `FRAGMENT_SPREAD` - directive can be applied to fragment spreads
  FragmentSpread(FragmentSpreadLocation),
  /// `INLINE_FRAGMENT` - directive can be applied to inline fragments
  InlineFragment(InlineFragmentLocation),
  /// `VARIABLE_DEFINITION` - directive can be applied to variable definitions
  VariableDefinition(VariableDefinitionLocation),
}

impl AsSpan<Span> for ExecutableDirectiveLocation {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for ExecutableDirectiveLocation {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Query(loc) => loc.into_span(),
      Self::Mutation(loc) => loc.into_span(),
      Self::Subscription(loc) => loc.into_span(),
      Self::Field(loc) => loc.into_span(),
      Self::FragmentDefinition(loc) => loc.into_span(),
      Self::FragmentSpread(loc) => loc.into_span(),
      Self::InlineFragment(loc) => loc.into_span(),
      Self::VariableDefinition(loc) => loc.into_span(),
    }
  }
}

impl AsRef<str> for ExecutableDirectiveLocation {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl core::borrow::Borrow<str> for ExecutableDirectiveLocation {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl DisplaySDL for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayHuman for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl DisplaySyntaxTree for ExecutableDirectiveLocation {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    let span = self.span();
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- {}@{}..{}",
      self.syntax_tree_name(),
      span.start(),
      span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      span.start(),
      span.end(),
      self.as_str(),
    )
  }
}

impl ExecutableDirectiveLocation {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Query(loc) => loc.span(),
      Self::Mutation(loc) => loc.span(),
      Self::Subscription(loc) => loc.span(),
      Self::Field(loc) => loc.span(),
      Self::FragmentDefinition(loc) => loc.span(),
      Self::FragmentSpread(loc) => loc.span(),
      Self::InlineFragment(loc) => loc.span(),
      Self::VariableDefinition(loc) => loc.span(),
    }
  }

  /// Returns the string representation of the directive location.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query(_) => QueryLocation::raw(),
      Self::Mutation(_) => MutationLocation::raw(),
      Self::Subscription(_) => SubscriptionLocation::raw(),
      Self::Field(_) => FieldLocation::raw(),
      Self::FragmentDefinition(_) => FragmentDefinitionLocation::raw(),
      Self::FragmentSpread(_) => FragmentSpreadLocation::raw(),
      Self::InlineFragment(_) => InlineFragmentLocation::raw(),
      Self::VariableDefinition(_) => VariableDefinitionLocation::raw(),
    }
  }

  #[inline]
  fn syntax_tree_name(&self) -> &'static str {
    match self {
      Self::Query(_) => "QUERY_KW",
      Self::Mutation(_) => "MUTATION_KW",
      Self::Subscription(_) => "SUBSCRIPTION_KW",
      Self::Field(_) => "FIELD_KW",
      Self::FragmentDefinition(_) => "FRAGMENT_DEFINITION_KW",
      Self::FragmentSpread(_) => "FRAGMENT_SPREAD_KW",
      Self::InlineFragment(_) => "INLINE_FRAGMENT_KW",
      Self::VariableDefinition(_) => "VARIABLE_DEFINITION_KW",
    }
  }
}

/// Represents locations where directives can be applied in GraphQL schema definitions.
///
/// Type system directive locations specify where directives can be used in GraphQL
/// schemas to provide metadata, validation, transformation, or other behaviors
/// for types, fields, and other schema elements.
///
/// ## Examples
///
/// ```text
/// # Schema directive
/// schema @link(url: "https://specs.apollo.dev/federation/v2.0") { query: Query }
///
/// # Object type directive
/// type User @key(fields: "id") { id: ID! name: String }
///
/// # Field definition directive
/// type User { name: String @deprecated(reason: "Use fullName") }
///
/// # Scalar directive
/// scalar DateTime @specifiedBy(url: "https://scalars.graphql.org/andimarek/datetime")
///
/// # Enum value directive
/// enum Role { USER @deprecated ADMIN MODERATOR }
/// ```
///
/// Spec: [TypeSystemDirectiveLocation](https://spec.graphql.org/draft/#TypeSystemDirectiveLocation)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDirectiveLocation {
  /// `SCHEMA` - directive can be applied to schema definitions
  Schema(SchemaLocation),
  /// `SCALAR` - directive can be applied to scalar type definitions
  Scalar(ScalarLocation),
  /// `OBJECT` - directive can be applied to object type definitions
  Object(ObjectLocation),
  /// `FIELD_DEFINITION` - directive can be applied to field definitions
  FieldDefinition(FieldDefinitionLocation),
  /// `ARGUMENT_DEFINITION` - directive can be applied to argument definitions
  ArgumentDefinition(ArgumentDefinitionLocation),
  /// `INTERFACE` - directive can be applied to interface type definitions
  Interface(InterfaceLocation),
  /// `UNION` - directive can be applied to union type definitions
  Union(UnionLocation),
  /// `ENUM` - directive can be applied to enum type definitions
  Enum(EnumLocation),
  /// `ENUM_VALUE` - directive can be applied to enum value definitions
  EnumValue(EnumValueLocation),
  /// `INPUT_OBJECT` - directive can be applied to input object type definitions
  InputObject(InputObjectLocation),
  /// `INPUT_FIELD_DEFINITION` - directive can be applied to input field definitions
  InputFieldDefinition(InputFieldDefinitionLocation),
}

impl AsSpan<Span> for TypeSystemDirectiveLocation {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for TypeSystemDirectiveLocation {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Schema(loc) => loc.into_span(),
      Self::Scalar(loc) => loc.into_span(),
      Self::Object(loc) => loc.into_span(),
      Self::FieldDefinition(loc) => loc.into_span(),
      Self::ArgumentDefinition(loc) => loc.into_span(),
      Self::Interface(loc) => loc.into_span(),
      Self::Union(loc) => loc.into_span(),
      Self::Enum(loc) => loc.into_span(),
      Self::EnumValue(loc) => loc.into_span(),
      Self::InputObject(loc) => loc.into_span(),
      Self::InputFieldDefinition(loc) => loc.into_span(),
    }
  }
}

impl AsRef<str> for TypeSystemDirectiveLocation {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl core::borrow::Borrow<str> for TypeSystemDirectiveLocation {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl DisplaySDL for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

impl DisplayHuman for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl DisplaySyntaxTree for TypeSystemDirectiveLocation {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    let span = self.span();
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- {}@{}..{}",
      self.syntax_tree_name(),
      span.start(),
      span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      span.start(),
      span.end(),
      self.as_str(),
    )
  }
}

impl TypeSystemDirectiveLocation {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Schema(loc) => loc.span(),
      Self::Scalar(loc) => loc.span(),
      Self::Object(loc) => loc.span(),
      Self::FieldDefinition(loc) => loc.span(),
      Self::ArgumentDefinition(loc) => loc.span(),
      Self::Interface(loc) => loc.span(),
      Self::Union(loc) => loc.span(),
      Self::Enum(loc) => loc.span(),
      Self::EnumValue(loc) => loc.span(),
      Self::InputObject(loc) => loc.span(),
      Self::InputFieldDefinition(loc) => loc.span(),
    }
  }

  /// Returns the string representation of the directive location.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Schema(_) => SchemaLocation::raw(),
      Self::Scalar(_) => ScalarLocation::raw(),
      Self::Object(_) => ObjectLocation::raw(),
      Self::FieldDefinition(_) => FieldDefinitionLocation::raw(),
      Self::ArgumentDefinition(_) => ArgumentDefinitionLocation::raw(),
      Self::Interface(_) => InterfaceLocation::raw(),
      Self::Union(_) => UnionLocation::raw(),
      Self::Enum(_) => EnumLocation::raw(),
      Self::EnumValue(_) => EnumValueLocation::raw(),
      Self::InputObject(_) => InputObjectLocation::raw(),
      Self::InputFieldDefinition(_) => InputFieldDefinitionLocation::raw(),
    }
  }

  #[inline]
  fn syntax_tree_name(&self) -> &'static str {
    match self {
      Self::Schema(_) => "SCHEMA_KW",
      Self::Scalar(_) => "SCALAR_KW",
      Self::Object(_) => "OBJECT_KW",
      Self::FieldDefinition(_) => "FIELD_DEFINITION_KW",
      Self::ArgumentDefinition(_) => "ARGUMENT_DEFINITION_KW",
      Self::Interface(_) => "INTERFACE_KW",
      Self::Union(_) => "UNION_KW",
      Self::Enum(_) => "ENUM_KW",
      Self::EnumValue(_) => "ENUM_VALUE_KW",
      Self::InputObject(_) => "INPUT_OBJECT_KW",
      Self::InputFieldDefinition(_) => "INPUT_FIELD_DEFINITION_KW",
    }
  }
}

/// Represents any location where a directive can be applied in GraphQL.
///
/// This is the top-level enum that encompasses both executable and type system
/// directive locations. It provides a unified interface for working with all
/// possible directive placement locations in GraphQL documents.
///
/// Spec: [DirectiveLocation](https://spec.graphql.org/draft/#DirectiveLocation)
#[derive(Debug, Clone, Copy, IsVariant, From, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Location {
  /// Executable directive location
  Executable(ExecutableDirectiveLocation),
  /// Type system directive location
  TypeSystem(TypeSystemDirectiveLocation),
}

impl AsSpan<Span> for Location {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl IntoSpan<Span> for Location {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Executable(loc) => loc.into_span(),
      Self::TypeSystem(loc) => loc.into_span(),
    }
  }
}

impl DisplaySDL for Location {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplaySDL::fmt(loc, f),
      Self::TypeSystem(loc) => DisplaySDL::fmt(loc, f),
    }
  }
}

impl DisplayHuman for Location {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplayHuman::fmt(loc, f),
      Self::TypeSystem(loc) => DisplayHuman::fmt(loc, f),
    }
  }
}

impl DisplaySyntaxTree for Location {
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    match self {
      Self::Executable(loc) => DisplaySyntaxTree::fmt(loc, level, indent, f),
      Self::TypeSystem(loc) => DisplaySyntaxTree::fmt(loc, level, indent, f),
    }
  }
}

impl Location {
  /// Returns a reference to the span covering the directive location token.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Executable(loc) => loc.span(),
      Self::TypeSystem(loc) => loc.span(),
    }
  }
}

macro_rules! from_location {
  ($($variant:ident: [$($sub_variant:ident),+$(,)?]), +$(,)?) => {
    $(
      $(
        impl From<$sub_variant> for Location {
          fn from(location: $sub_variant) -> Self {
            Self::$variant(location.into())
          }
        }
      )*
    )*
  };
}

from_location!(
  Executable: [
    QueryLocation,
    MutationLocation,
    SubscriptionLocation,
    FieldLocation,
    FragmentDefinitionLocation,
    FragmentSpreadLocation,
    InlineFragmentLocation,
    VariableDefinitionLocation
  ],
  TypeSystem: [
    SchemaLocation,
    ScalarLocation,
    ObjectLocation,
    FieldDefinitionLocation,
    ArgumentDefinitionLocation,
    InterfaceLocation,
    UnionLocation,
    EnumLocation,
    EnumValueLocation,
    InputObjectLocation,
    InputFieldDefinitionLocation,
  ],
);

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> for Location
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => Ok({
            match () {
              () if "QUERY".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Query(keywords::QueryLocation::new(span)),
              ),
              () if "MUTATION".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Mutation(keywords::MutationLocation::new(span)),
              ),
              () if "SUBSCRIPTION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::Subscription(
                  keywords::SubscriptionLocation::new(span),
                ))
              }
              () if "FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::FieldDefinition(
                  keywords::FieldDefinitionLocation::new(span),
                ))
              }
              () if "FIELD".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Field(keywords::FieldLocation::new(span)),
              ),
              () if "FRAGMENT_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::FragmentDefinition(
                  keywords::FragmentDefinitionLocation::new(span),
                ))
              }
              () if "FRAGMENT_SPREAD".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::FragmentSpread(
                  keywords::FragmentSpreadLocation::new(span),
                ))
              }
              () if "INLINE_FRAGMENT".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::InlineFragment(
                  keywords::InlineFragmentLocation::new(span),
                ))
              }
              () if "VARIABLE_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::VariableDefinition(
                  keywords::VariableDefinitionLocation::new(span),
                ))
              }
              () if "SCHEMA".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Schema(keywords::SchemaLocation::new(span)),
              ),
              () if "SCALAR".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Scalar(keywords::ScalarLocation::new(span)),
              ),
              () if "OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Object(keywords::ObjectLocation::new(span)),
              ),
              () if "ARGUMENT_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::ArgumentDefinition(
                  keywords::ArgumentDefinitionLocation::new(span),
                ))
              }
              () if "INTERFACE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Interface(keywords::InterfaceLocation::new(span)),
              ),
              () if "UNION".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Union(keywords::UnionLocation::new(span)),
              ),
              () if "ENUM_VALUE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::EnumValue(keywords::EnumValueLocation::new(span)),
              ),
              () if "ENUM".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Enum(keywords::EnumLocation::new(span)),
              ),
              () if "INPUT_OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::InputObject(keywords::InputObjectLocation::new(span)),
              ),
              () if "INPUT_FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::InputFieldDefinition(
                  keywords::InputFieldDefinitionLocation::new(span),
                ))
              }
              _ => return Err(Error::unknown_directive_location(name, span).into()),
            }
          }),
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
