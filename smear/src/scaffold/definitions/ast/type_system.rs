use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::choice},
  utils::{AsSpan, IntoSpan, Span},
};

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
  Scalar(ScalarTypeDefinition),
  /// An object type definition.
  Object(ObjectTypeDefinition),
  /// An interface type definition.
  Interface(InterfaceTypeDefinition),
  /// A union type definition.
  Union(UnionTypeDefinition),
  /// An enum type definition.
  Enum(EnumTypeDefinition),
  /// An input object type definition.
  InputObject(InputObjectTypeDefinition),
}

impl<
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
> AsSpan<Span>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: AsSpan<Span>,
  ObjectTypeDefinition: AsSpan<Span>,
  InterfaceTypeDefinition: AsSpan<Span>,
  UnionTypeDefinition: AsSpan<Span>,
  EnumTypeDefinition: AsSpan<Span>,
  InputObjectTypeDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_span(),
      Self::InputObject(i) => i.as_span(),
      Self::Object(o) => o.as_span(),
      Self::Interface(i) => i.as_span(),
      Self::Union(u) => u.as_span(),
      Self::Enum(e) => e.as_span(),
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

impl<
  'a,
  ScalarTypeDefinition,
  ObjectTypeDefinition,
  InterfaceTypeDefinition,
  UnionTypeDefinition,
  EnumTypeDefinition,
  InputObjectTypeDefinition,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeDefinition<
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition,
  >
where
  ScalarTypeDefinition: Parseable<'a, I, T, Error>,
  ObjectTypeDefinition: Parseable<'a, I, T, Error>,
  InterfaceTypeDefinition: Parseable<'a, I, T, Error>,
  UnionTypeDefinition: Parseable<'a, I, T, Error>,
  EnumTypeDefinition: Parseable<'a, I, T, Error>,
  InputObjectTypeDefinition: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      ScalarTypeDefinition::parser::<E>().map(Self::Scalar),
      ObjectTypeDefinition::parser::<E>().map(Self::Object),
      InterfaceTypeDefinition::parser::<E>().map(Self::Interface),
      UnionTypeDefinition::parser::<E>().map(Self::Union),
      EnumTypeDefinition::parser::<E>().map(Self::Enum),
      InputObjectTypeDefinition::parser::<E>().map(Self::InputObject),
    ))
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
  Scalar(ScalarTypeExtension),
  /// An object type extension.
  Object(ObjectTypeExtension),
  /// An interface type extension.
  Interface(InterfaceTypeExtension),
  /// A union type extension.
  Union(UnionTypeExtension),
  /// An enum type extension.
  Enum(EnumTypeExtension),
  /// An input object type extension.
  InputObject(InputObjectTypeExtension),
}

impl<
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
> AsSpan<Span>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: AsSpan<Span>,
  ObjectTypeExtension: AsSpan<Span>,
  InterfaceTypeExtension: AsSpan<Span>,
  UnionTypeExtension: AsSpan<Span>,
  EnumTypeExtension: AsSpan<Span>,
  InputObjectTypeExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.as_span(),
      Self::InputObject(i) => i.as_span(),
      Self::Object(o) => o.as_span(),
      Self::Interface(i) => i.as_span(),
      Self::Union(u) => u.as_span(),
      Self::Enum(e) => e.as_span(),
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

impl<
  'a,
  ScalarTypeExtension,
  ObjectTypeExtension,
  InterfaceTypeExtension,
  UnionTypeExtension,
  EnumTypeExtension,
  InputObjectTypeExtension,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for TypeExtension<
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension,
  >
where
  ScalarTypeExtension: Parseable<'a, I, T, Error>,
  ObjectTypeExtension: Parseable<'a, I, T, Error>,
  InterfaceTypeExtension: Parseable<'a, I, T, Error>,
  UnionTypeExtension: Parseable<'a, I, T, Error>,
  EnumTypeExtension: Parseable<'a, I, T, Error>,
  InputObjectTypeExtension: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    choice((
      ScalarTypeExtension::parser::<E>().map(Self::Scalar),
      ObjectTypeExtension::parser::<E>().map(Self::Object),
      InterfaceTypeExtension::parser::<E>().map(Self::Interface),
      UnionTypeExtension::parser::<E>().map(Self::Union),
      EnumTypeExtension::parser::<E>().map(Self::Enum),
      InputObjectTypeExtension::parser::<E>().map(Self::InputObject),
    ))
  }
}

/// Type system definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition> {
  /// A type definition.
  Type(TypeDefinition),
  /// A directive definition.
  Directive(DirectiveDefinition),
  /// A schema definition.
  Schema(SchemaDefinition),
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition> AsSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
where
  TypeDefinition: AsSpan<Span>,
  DirectiveDefinition: AsSpan<Span>,
  SchemaDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_span(),
      Self::Directive(d) => d.as_span(),
      Self::Schema(s) => s.as_span(),
    }
  }
}

impl<TypeDefinition, DirectiveDefinition, SchemaDefinition> IntoSpan<Span>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
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

impl<'a, TypeDefinition, DirectiveDefinition, SchemaDefinition, I, T, Error>
  Parseable<'a, I, T, Error>
  for TypeSystemDefinition<TypeDefinition, DirectiveDefinition, SchemaDefinition>
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
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
pub enum TypeSystemExtension<TypeExtension, SchemaExtension> {
  /// A type extension.
  Type(TypeExtension),
  /// A schema extension.
  Schema(SchemaExtension),
}

impl<TypeExtension, SchemaExtension> AsSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
where
  TypeExtension: AsSpan<Span>,
  SchemaExtension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Type(t) => t.as_span(),
      Self::Schema(s) => s.as_span(),
    }
  }
}

impl<TypeExtension, SchemaExtension> IntoSpan<Span>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
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

impl<'a, TypeExtension, SchemaExtension, I, T, Error> Parseable<'a, I, T, Error>
  for TypeSystemExtension<TypeExtension, SchemaExtension>
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    TypeExtension::parser::<E>()
      .map(Self::Type)
      .or(SchemaExtension::parser::<E>().map(Self::Schema))
  }
}

/// Type system definition or extension for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinitionOrExtension<Definition, Extension> {
  /// A type system definition.
  Definition(Definition),
  /// A type system extension.
  Extension(Extension),
}

impl<Definition, Extension> AsSpan<Span> for TypeSystemDefinitionOrExtension<Definition, Extension>
where
  Definition: AsSpan<Span>,
  Extension: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
    }
  }
}

impl<Definition, Extension> IntoSpan<Span>
  for TypeSystemDefinitionOrExtension<Definition, Extension>
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

impl<'a, Definition, Extension, I, T, Error> Parseable<'a, I, T, Error>
  for TypeSystemDefinitionOrExtension<Definition, Extension>
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Definition::parser::<E>()
      .map(Self::Definition)
      .or(Extension::parser::<E>().map(Self::Extension))
  }
}

/// Executable definition for GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDefinition<OperationDefinition, FragmentDefinition> {
  /// An operation definition.
  Operation(OperationDefinition),
  /// A fragment definition.
  Fragment(FragmentDefinition),
}

impl<OperationDefinition, FragmentDefinition> AsSpan<Span>
  for ExecutableDefinition<OperationDefinition, FragmentDefinition>
where
  OperationDefinition: AsSpan<Span>,
  FragmentDefinition: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Operation(o) => o.as_span(),
      Self::Fragment(f) => f.as_span(),
    }
  }
}

impl<OperationDefinition, FragmentDefinition> IntoSpan<Span>
  for ExecutableDefinition<OperationDefinition, FragmentDefinition>
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

impl<'a, OperationDefinition, FragmentDefinition, I, T, Error> Parseable<'a, I, T, Error>
  for ExecutableDefinition<OperationDefinition, FragmentDefinition>
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
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    OperationDefinition::parser::<E>()
      .map(Self::Operation)
      .or(FragmentDefinition::parser::<E>().map(Self::Fragment))
  }
}

/// A definition of a GraphQL specification.
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Definition<TypeSystem, Executable> {
  /// A type system definition or extension.
  TypeSystem(TypeSystem),
  /// An executable definition.
  Executable(Executable),
}

impl<TypeSystem, Executable> AsSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: AsSpan<Span>,
  Executable: AsSpan<Span>,
{
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::TypeSystem(t) => t.as_span(),
      Self::Executable(e) => e.as_span(),
    }
  }
}

impl<TypeSystem, Executable> IntoSpan<Span> for Definition<TypeSystem, Executable>
where
  TypeSystem: IntoSpan<Span>,
  Executable: IntoSpan<Span>,
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::TypeSystem(t) => t.into_span(),
      Self::Executable(e) => e.into_span(),
    }
  }
}

impl<'a, TypeSystem, Executable, I, T, Error> Parseable<'a, I, T, Error>
  for Definition<TypeSystem, Executable>
where
  TypeSystem: Parseable<'a, I, T, Error>,
  Executable: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    TypeSystem::parser::<E>()
      .map(Self::TypeSystem)
      .or(Executable::parser::<E>().map(Self::Executable))
  }
}
