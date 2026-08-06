use std::{boxed::Box, rc::Rc, sync::Arc};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
};

pub use crate::parser::ty::{ListType, NamedType};

macro_rules! ty {
  ($(
    $(#[$meta:meta])*
    $ty:ident<$name:ident>), +$(,)?
  ) => {
    paste::paste! {
      $(
        $(#[$meta])*
        #[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
        #[unwrap(ref, ref_mut)]
        #[try_unwrap(ref, ref_mut)]
        pub enum $name<Name> {
          /// A named type referencing a schema-defined type.
          Name(NamedType<Name>),

          /// A list type containing elements of another type.
          List($ty<ListType<Self>>),
        }

        impl<Name> From<ListType<Self>> for $name<Name> {
          #[inline]
          fn from(ty: ListType<Self>) -> Self {
            Self::List(<$ty<ListType<Self>>>::new(ty))
          }
        }

        impl<Name> AsSpan<Span> for $name<Name> {
          #[inline]
          fn as_span(&self) -> &Span {
            match self {
              Self::Name(ty) => ty.span(),
              Self::List(ty) => ty.span(),
            }
          }
        }

        impl<Name> IntoSpan<Span> for $name<Name> {
          #[inline]
          fn into_span(self) -> Span {
            match self {
              Self::Name(ty) => ty.into_span(),
              Self::List(ty) => *ty.span(),
            }
          }
        }

        impl<Name> $name<Name> {
          /// Returns whether this type reference is non-null.
          #[inline]
          pub fn required(&self) -> bool {
            match self {
              Self::Name(ty) => ty.required(),
              Self::List(ty) => ty.required(),
            }
          }
        }
      )*
    }
  };
}

ty!(
  /// GraphQL type using `Box` for recursive list types.
  Box<Type>,
  /// GraphQL type using `Rc` for recursive list types with reference counting.
  Rc<RcType>,
  /// GraphQL type using `Arc` for recursive list types with atomic reference counting.
  Arc<ArcType>,
);
