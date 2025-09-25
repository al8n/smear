use chumsky::{Parser, extra::ParserExtra, prelude::choice};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{Parseable, Source, Token, Tokenizer, utils::Span};
use smear_utils::IntoSpan;

/// Type definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> {
  /// A scalar type definition.
  Scalar(
    ScalarTypeDefinition,
  ),
  /// An object type definition.
  Object(
    ObjectTypeDefinition,
  ),
  /// An interface type definition.
  Interface(
    InterfaceTypeDefinition,
  ),
  /// A union type definition.
  Union(  
    UnionTypeDefinition,
  ),
  /// An enum type definition.
  Enum(
    EnumTypeDefinition,
  ),
  /// An input object type definition.
  InputObject(
    InputObjectTypeDefinition,
  ),
}

impl<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> AsRef<Span>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: AsRef<Span>,
  ObjectTypeDefinition: AsRef<Span>,
  InterfaceTypeDefinition: AsRef<Span>,
  UnionTypeDefinition: AsRef<Span>,
  EnumTypeDefinition: AsRef<Span>,
  InputObjectTypeDefinition: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_ref(),
      Self::InputObject(i) => i.as_ref(),
      Self::Object(o) => o.as_ref(),
      Self::Interface(i) => i.as_ref(),
      Self::Union(u) => u.as_ref(),
      Self::Enum(e) => e.as_ref(),
    }
  }
}

impl<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> IntoSpan<Span>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: IntoSpan<Span>,
  ObjectTypeDefinition: IntoSpan<Span>,
  InterfaceTypeDefinition: IntoSpan<Span>,
  UnionTypeDefinition: IntoSpan<Span>,
  EnumTypeDefinition: IntoSpan<Span>,
  InputObjectTypeDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
      Self::Union(u) => u.into_span(),
      Self::Enum(e) => e.into_span(),
    }
  }
}

/// Type extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeExtension<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> {
  /// A scalar type extension.
  Scalar(
    ScalarTypeExtension,
  ),
  /// An object type extension.
  Object(
    ObjectTypeExtension,
  ),
  /// An interface type extension.
  Interface(
    InterfaceTypeExtension,
  ),
  /// A union type extension.
  Union(
    UnionTypeExtension,
  ),
  /// An enum type extension.
  Enum(
    EnumTypeExtension,
  ),
  /// An input object type extension.
  InputObject(
    InputObjectTypeExtension,
  ),
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> AsRef<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: AsRef<Span>,
  ObjectTypeExtension: AsRef<Span>,
  InterfaceTypeExtension: AsRef<Span>,
  UnionTypeExtension: AsRef<Span>,
  EnumTypeExtension: AsRef<Span>,
  InputObjectTypeExtension: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_ref(),
      Self::InputObject(i) => i.as_ref(),
      Self::Object(o) => o.as_ref(),
      Self::Interface(i) => i.as_ref(),
      Self::Union(u) => u.as_ref(),
      Self::Enum(e) => e.as_ref(),
    }
  }
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> IntoSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: IntoSpan<Span>,
  ObjectTypeExtension: IntoSpan<Span>,
  InterfaceTypeExtension: IntoSpan<Span>,
  UnionTypeExtension: IntoSpan<Span>,
  EnumTypeExtension: IntoSpan<Span>,
  InputObjectTypeExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
      Self::Union(u) => u.into_span(),
      Self::Enum(e) => e.into_span(),
    }
  }
}

/// Type system definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemDefinition<
  TypeDefinition,
  DirectiveDefinition,
  SchemaDefinition,
> {
  /// A type definition.
  Type(
    TypeDefinition,
  ),
  /// A directive definition.
  Directive(
    DirectiveDefinition,
  ),
  /// A schema definition.
  Schema(SchemaDefinition),
}

impl<
  TypeDefinition,
  DirectiveDefinition,
  SchemaDefinition,
> AsRef<Span>
  for TypeSystemDefinition<
    TypeDefinition,
    DirectiveDefinition,
    SchemaDefinition,
  >
where
  TypeDefinition: AsRef<Span>,
  DirectiveDefinition: AsRef<Span>,
  SchemaDefinition: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_ref(),
      Self::Directive(d) => d.as_ref(),
      Self::Schema(s) => s.as_ref(),
    }
  }
}

impl<
  TypeDefinition,
  DirectiveDefinition,
  SchemaDefinition,
> IntoSpan<Span>
  for TypeSystemDefinition<
    TypeDefinition,
    DirectiveDefinition,
    SchemaDefinition,
  >
where
  TypeDefinition: IntoSpan<Span>,
  DirectiveDefinition: IntoSpan<Span>,
  SchemaDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Directive(d) => d.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<
  'a,
  TypeDefinition,
  DirectiveDefinition,
  SchemaDefinition,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemDefinition<
    TypeDefinition,
    DirectiveDefinition,
    SchemaDefinition,
  >
where
  TypeDefinition: Parseable<'a, I, T, Error>,
  DirectiveDefinition: Parseable<'a, I, T, Error>,
  SchemaDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      TypeDefinition::parser::<E>().map(Self::Type),
      DirectiveDefinition::parser::<E>().map(Self::Directive),
      SchemaDefinition::parser::<E>().map(Self::Schema),
    ))
  }
}

/// Type system extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<
  TypeExtension,
  SchemaExtension,
> {
  /// A type extension.
  Type(
    TypeExtension,
  ),
  /// A schema extension.
  Schema(
    SchemaExtension,
  ),
}

impl<
  TypeExtension,
  SchemaExtension,
> AsRef<Span>
  for TypeSystemExtension<
    TypeExtension,
    SchemaExtension,
  >
where
  TypeExtension: AsRef<Span>,
  SchemaExtension: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_ref(),
      Self::Schema(s) => s.as_ref(),
    }
  }
}

impl<
  TypeExtension,
  SchemaExtension,
> IntoSpan<Span>
  for TypeSystemExtension<
    TypeExtension,
    SchemaExtension,
  >
where
  TypeExtension: IntoSpan<Span>,
  SchemaExtension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {  
    match self {
      Self::Type(t) => t.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<
  'a,
  TypeExtension,
  SchemaExtension,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemExtension<
    TypeExtension,
    SchemaExtension,
  >
where
  TypeExtension: Parseable<'a, I, T, Error>,
  SchemaExtension: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      TypeExtension::parser::<E>().map(Self::Type),
      SchemaExtension::parser::<E>().map(Self::Schema),
    ))
  }
}

/// Type system definition or extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinitionOrExtension<
  Definition,
  Extension,
> {
  /// A type system definition.
  Definition(
    Definition,
  ),
  /// A type system extension.
  Extension(
    Extension,
  ),
}

impl<
  Definition,
  Extension,
> AsRef<Span>
  for TypeSystemDefinitionOrExtension<
    Definition,
    Extension,
  >
where
  Definition: AsRef<Span>,
  Extension: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_ref(),
      Self::Extension(e) => e.as_ref(),
    }
  }
}

impl<
  Definition,
  Extension,
> IntoSpan<Span>
  for TypeSystemDefinitionOrExtension<
    Definition,
    Extension,
  >
where
  Definition: IntoSpan<Span>,
  Extension: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

impl<
  'a,
  Definition,
  Extension,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeSystemDefinitionOrExtension<
    Definition,
    Extension,
  >
where
  Definition: Parseable<'a, I, T, Error>,
  Extension: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      Definition::parser::<E>().map(Self::Definition),
      Extension::parser::<E>().map(Self::Extension),
    ))
  }
}

/// Executable definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDefinition<
  OperationDefinition,
  FragmentDefinition,
> {
  /// An operation definition.
  Operation(
    OperationDefinition,
  ),
  /// A fragment definition.
  Fragment(
    FragmentDefinition,
  ),
}

impl<
  OperationDefinition,
  FragmentDefinition,
> AsRef<Span>
  for ExecutableDefinition<
    OperationDefinition,
    FragmentDefinition,
  >
where
  OperationDefinition: AsRef<Span>,
  FragmentDefinition: AsRef<Span>,
{
  #[inline]
  fn as_ref(&self) -> &Span {
    match self {
      Self::Operation(o) => o.as_ref(),
      Self::Fragment(f) => f.as_ref(),
    }
  }
}

impl<
  OperationDefinition,
  FragmentDefinition,
> IntoSpan<Span>
  for ExecutableDefinition<
    OperationDefinition,
    FragmentDefinition,
  >
where
  OperationDefinition: IntoSpan<Span>,
  FragmentDefinition: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Operation(o) => o.into_span(),
      Self::Fragment(f) => f.into_span(),
    }
  }
}

impl<
  'a,
  OperationDefinition,
  FragmentDefinition,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for ExecutableDefinition<
    OperationDefinition,
    FragmentDefinition,
  >
where
  OperationDefinition: Parseable<'a, I, T, Error>,
  FragmentDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      OperationDefinition::parser::<E>().map(Self::Operation),
      FragmentDefinition::parser::<E>().map(Self::Fragment),
    ))
  }
}
