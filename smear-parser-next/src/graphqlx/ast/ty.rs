//! GraphQLx type-reference AST node types.
//!
//! Copied type-only from the frozen `smear-parser` crate
//! (`graphqlx/ast/ty.rs`): the path / type-path aliases onto the
//! [`smear_scaffold`](crate::scaffold) generic vocabulary and the recursive
//! [`Type`] family (`Box`/`Rc`/`Arc` flavors). Spans stay
//! [`SimpleSpan`](tokora::SimpleSpan); the parser fns that lived beside them in the
//! frozen crate are rebuilt on the atom layer in a later wave.

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use smear_scaffold::ast::{self as scaffold, ListType, MapType, SetType};
use std::{boxed::Box, rc::Rc, sync::Arc, vec::Vec};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

use crate::ident::Ident;

/// A path referencing a type, module, or schema element.
pub type Path<S> = scaffold::Path<Ident<S>>;
/// A type path with optional generic type parameters.
pub type TypePath<S, Ty = Type<S>> = scaffold::generic::TypePath<Ident<S>, Ty>;
/// A definition-level type path with generic type parameters.
pub type DefinitionTypePath<S, Ty = Type<S>> = scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;

/// A definition-level type path using `Arc` for thread-safe reference counting.
pub type ArcDefinitionTypePath<S, Ty = ArcType<S>> =
  scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;
/// A definition-level type path using `Rc` for single-threaded reference counting.
pub type RcDefinitionTypePath<S, Ty = RcType<S>> =
  scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;

impl<S> From<Ident<S>> for Path<S> {
  #[inline]
  fn from(ident: Ident<S>) -> Self {
    Self::new(*ident.span(), Vec::from_iter([ident]), false)
  }
}

macro_rules! ty {
  ($(
    $(#[$meta:meta])*
    $ty:ident<$name:ident>), +$(,)?
  ) => {
    paste::paste! {
      $(
        ///
        /// Represents a complete GraphQLx type that can be either named, a list, a set or a map.
        ///
        $(#[$meta])*
        #[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
        #[unwrap(ref, ref_mut)]
        #[try_unwrap(ref, ref_mut)]
        pub enum $name<S> {
          /// A path type referencing a schema-defined type.
          Path(scaffold::generic::DefinitionTypePath<Ident<S>, Self>),

          /// A list type containing elements of another type.
          List($ty<ListType<Self>>),

          /// A set type containing elements of another type.
          Set($ty<SetType<Self>>),

          /// A map type containing key-value pairs.
          Map($ty<MapType<Self, Self>>),
        }

        impl<S> From<ListType<Self>> for $name<S> {
          #[inline]
          fn from(ty: ListType<Self>) -> Self {
            Self::List(<$ty<ListType<Self>>>::new(ty))
          }
        }

        impl<S> From<SetType<Self>> for $name<S> {
          #[inline]
          fn from(ty: SetType<Self>) -> Self {
            Self::Set(<$ty<SetType<Self>>>::new(ty))
          }
        }

        impl<S> From<MapType<Self, Self>> for $name<S> {
          #[inline]
          fn from(ty: MapType<Self, Self>) -> Self {
            Self::Map(<$ty<MapType<Self, Self>>>::new(ty))
          }
        }

        impl<S> AsSpan<Span> for $name<S> {
          #[inline]
          fn as_span(&self) -> &Span {
            self.span()
          }
        }

        impl<S> IntoSpan<Span> for $name<S> {
          #[inline]
          fn into_span(self) -> Span {
            match self {
              Self::Path(ty) => ty.into_span(),
              Self::List(ty) => *ty.span(),
              Self::Map(ty) => *ty.span(),
              Self::Set(ty) => *ty.span(),
            }
          }
        }

        impl<S> $name<S> {
          /// Returns the span of the type.
          #[inline]
          pub fn span(&self) -> &Span {
            match self {
              Self::Path(ty) => ty.span(),
              Self::List(ty) => ty.span(),
              Self::Set(ty) => ty.span(),
              Self::Map(ty) => ty.span(),
            }
          }
        }
      )*
    }
  };
}

ty!(
  /// GraphQLx type using `Box` for recursive types.
  Box<Type>,
  /// GraphQLx type using `Rc` for recursive types with reference counting.
  Rc<RcType>,
  /// GraphQLx type using `Arc` for recursive types with atomic reference counting.
  Arc<ArcType>,
);
