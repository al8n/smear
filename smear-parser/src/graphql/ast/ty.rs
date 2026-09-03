use std::{boxed::Box, rc::Rc, sync::Arc};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokora::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
};

use crate::value::{Absent, NestNode, Nestable, Sealed, Worklist};

pub use crate::ty::{ListType, NamedType};

/// The pointer the `List` arm holds its element behind, and the two traits that decide what may
/// stand in it.
///
/// Re-exported here because [`Nest`] is the payload type of a public variant, so it reaches a
/// consumer's `match` whether or not they name it. It derefs to the [`ListType`] the arm used to
/// hold directly, which is why nothing that reads one had to change; what it adds is the iterative
/// release. Both traits are sealed.
pub use crate::value::{Nest, NestPtr, SoleNestPtr};

macro_rules! ty {
  ($(
    $(#[$meta:meta])*
    $ty:ident<$name:ident>), +$(,)?
  ) => {
    paste::paste! {
      $(
        $(#[$meta])*
        ///
        /// # This enum declares no `Drop`, and that is load-bearing
        ///
        /// Releasing a deeply nested one used to abort the process, one native frame per level, and
        /// no parse was needed to build one: `From<ListType<Self>>` is public, so a caller can
        /// grow the chain in a loop and merely leaving scope was enough. The repair is [`Nest`],
        /// the pointer the `List` arm holds its element behind — **not** a `Drop` on this enum,
        /// which `E0509` would have charged every by-value `unwrap_*` and `try_unwrap_*` for, and
        /// which for this shape could not have been written at all. `Nest`'s own documentation
        /// derives why. Everything `derive_more` generated before is generated now.
        ///
        /// [`Nestable`] below is how the release reaches this enum's child. It reaches every
        /// child the *grammar* can put in a recursive position; it does not reach a node a caller
        /// stored in `Name`, for which see [`Nested`](crate::value::Nested)'s own documentation and
        /// `al8n/smear#176`.
        ///
        /// # What it does not repair
        ///
        /// `Drop` is one of four generated impls that descend one frame per level; the derived
        /// `Debug`, `Clone` and `PartialEq` still do. **This removes the only one of the four that
        /// fires without a call being made** — release happens to whoever holds the value, on scope
        /// exit, on unwind, in a caller's teardown of a collection, and it can be neither caught nor
        /// refused. The other three are chosen calls, and repairing them would land on this same
        /// pointer rather than on this enum.
        ///
        /// What standing [`Nest`] in the arm cost those three was measured on the [`Box`] member of
        /// this family in both profiles — the other two clone by refcount and so have no deep
        /// `Clone` to charge — and `value/nesting.rs`'s header has the table: **nothing in
        /// release**, where the forwarding impls inline away and `==` is not bounded by a stack at
        /// all, and in a debug build an unchanged `{:?}` ceiling against about 6% off `clone` and
        /// 17% off `==`.
        #[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]
        #[unwrap(ref, ref_mut)]
        #[try_unwrap(ref, ref_mut)]
        pub enum $name<Name> {
          /// A named type referencing a schema-defined type.
          Name(NamedType<Name>),

          /// A list type containing elements of another type.
          List(Nest<$ty<ListType<Self>>>),
        }

        impl<Name> Sealed for $name<Name> {}

        /// A type's children are types, so neither carrier lane exists on this enum: an object
        /// field and a map entry are *value* carriers, and this dialect's type grammar has no map
        /// at all.
        impl<Name> NestNode for $name<Name> {
          type Field = Absent<Self>;
          type Entry = Absent<Self>;
        }

        impl<Name> Nestable for $name<Name> {
          /// The type tree: rank 1. It holds no value node and no selection.
          const RANK: u8 = 1;

          type Node = Self;

          #[inline]
          fn into_children(self, worklist: &mut Worklist<Self>) {
            match self {
              // Holds no type of this crate's own. What it does hold is `Name` — at the crate's own
              // instantiation a source-slice name, and at a caller's whatever the caller chose,
              // including a node this loop cannot reach (al8n/smear#176).
              Self::Name(_) => {}
              // Nothing is handed over only when the pointer is shared and another owner remains,
              // which is the one case with nothing below it to unlink yet. A chain of these arms
              // runs through the worklist's register, so releasing one allocates nothing at any
              // depth.
              Self::List(nest) => {
                if let Some(list) = nest.into_inner() {
                  worklist.push(list.into_components().1);
                }
              }
            }
          }
        }

        impl<Name> Sealed for ListType<$name<Name>> {}

        /// The pointee side of the same walk: a list carrier is `(span, element, required)` and
        /// only the element holds a type.
        impl<Name> Nestable for ListType<$name<Name>> {
          /// The type tree: rank 1. It holds no value node and no selection.
          const RANK: u8 = 1;

          type Node = $name<Name>;

          #[inline]
          fn into_children(self, worklist: &mut Worklist<$name<Name>>) {
            worklist.push(self.into_components().1);
          }
        }

        impl<Name> From<ListType<Self>> for $name<Name> {
          #[inline]
          fn from(ty: ListType<Self>) -> Self {
            Self::List(Nest::new(ty))
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
