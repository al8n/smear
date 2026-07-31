use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use super::*;

/// A GraphQL type definition.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeDefinition<S, Ty = Type<Name<S>>> {
  /// The scalar type definition.
  Scalar(ScalarTypeDefinition<S>),
  /// The object type definition.
  Object(ObjectTypeDefinition<S, Ty>),
  /// The interface type definition.
  Interface(InterfaceTypeDefinition<S, Ty>),
  /// The union type definition.
  Union(UnionTypeDefinition<S>),
  /// The enum type definition.
  Enum(EnumTypeDefinition<S>),
  /// The input object type definition.
  InputObject(InputObjectTypeDefinition<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeDefinition<S, Ty> {
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

impl<S, Ty> TypeDefinition<S, Ty> {
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

/// A GraphQL type extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<S, Ty = Type<Name<S>>> {
  /// The scalar type extension.
  Scalar(ScalarTypeExtension<S>),
  /// The object type extension.
  Object(ObjectTypeExtension<S, Ty>),
  /// The interface type extension.
  Interface(InterfaceTypeExtension<S, Ty>),
  /// The union type extension.
  Union(UnionTypeExtension<S>),
  /// The enum type extension.
  Enum(EnumTypeExtension<S>),
  /// The input object type extension.
  InputObject(InputObjectTypeExtension<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeExtension<S, Ty> {
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

impl<S, Ty> TypeExtension<S, Ty> {
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
