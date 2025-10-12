use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoSpan, Span},
};

use super::*;

/// A GraphQL type definition.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeDefinition<S> {
  /// The scalar type definition.
  Scalar(ScalarTypeDefinition<S>),
  /// The object type definition.
  Object(ObjectTypeDefinition<S>),
  /// The interface type definition.
  Interface(InterfaceTypeDefinition<S>),
  /// The union type definition.
  Union(UnionTypeDefinition<S>),
  /// The enum type definition.
  Enum(EnumTypeDefinition<S>),
  /// The input object type definition.
  InputObject(InputObjectTypeDefinition<S>),
}

impl<S> AsSpan<Span> for TypeDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for TypeDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
      Self::Interface(v) => v.into_span(),
      Self::Union(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::InputObject(v) => v.into_span(),
    }
  }
}

impl<S> TypeDefinition<S> {
  /// Returns the span of the type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(v) => v.span(),
      Self::Object(v) => v.span(),
      Self::Interface(v) => v.span(),
      Self::Union(v) => v.span(),
      Self::Enum(v) => v.span(),
      Self::InputObject(v) => v.span(),
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for TypeDefinition<S>
where
  ScalarTypeDefinition<S>: Parseable<'a, I, T, Error>,
  ObjectTypeDefinition<S>: Parseable<'a, I, T, Error>,
  InterfaceTypeDefinition<S>: Parseable<'a, I, T, Error>,
  UnionTypeDefinition<S>: Parseable<'a, I, T, Error>,
  EnumTypeDefinition<S>: Parseable<'a, I, T, Error>,
  InputObjectTypeDefinition<S>: Parseable<'a, I, T, Error>,
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

/// A GraphQL type extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<S> {
  /// The scalar type extension.
  Scalar(ScalarTypeExtension<S>),
  /// The object type extension.
  Object(ObjectTypeExtension<S>),
  /// The interface type extension.
  Interface(InterfaceTypeExtension<S>),
  /// The union type extension.
  Union(UnionTypeExtension<S>),
  /// The enum type extension.
  Enum(EnumTypeExtension<S>),
  /// The input object type extension.
  InputObject(InputObjectTypeExtension<S>),
}

impl<S> AsSpan<Span> for TypeExtension<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for TypeExtension<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
      Self::Interface(v) => v.into_span(),
      Self::Union(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::InputObject(v) => v.into_span(),
    }
  }
}

impl<S> TypeExtension<S> {
  /// Returns the span of the type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(v) => v.span(),
      Self::Object(v) => v.span(),
      Self::Interface(v) => v.span(),
      Self::Union(v) => v.span(),
      Self::Enum(v) => v.span(),
      Self::InputObject(v) => v.span(),
    }
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for TypeExtension<S>
where
  ScalarTypeExtension<S>: Parseable<'a, I, T, Error>,
  ObjectTypeExtension<S>: Parseable<'a, I, T, Error>,
  InterfaceTypeExtension<S>: Parseable<'a, I, T, Error>,
  UnionTypeExtension<S>: Parseable<'a, I, T, Error>,
  EnumTypeExtension<S>: Parseable<'a, I, T, Error>,
  InputObjectTypeExtension<S>: Parseable<'a, I, T, Error>,
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
