#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs, warnings)]
#![allow(clippy::type_complexity)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

pub use chumsky;

macro_rules! word {
  ($(
    $(#[$meta:meta])*
    $label:literal:$name:ident: $expr:expr
  ),+$(,)?) => {
    paste::paste! {
      $(
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $name<Span>(Span);

        impl<Span> ::core::convert::AsRef<Span> for $name<Span> {
          #[inline]
          fn as_ref(&self) -> &Span {
            self.span()
          }
        }

        impl<Span> $crate::__private::IntoSpan<Span> for $name<Span> {
          #[inline]
          fn into_span(self) -> Span {
            self.0
          }
        }

        impl<Span> $crate::__private::IntoComponents for $name<Span> {
          type Components = Span;

          #[inline]
          fn into_components(self) -> Self::Components {
            <$name<Span> as $crate::__private::IntoSpan<Span>>::into_span(self)
          }
        }

        impl<Span> $name<Span> {
          #[doc = "Returns the span of the `" $label "`."]
          #[inline]
          pub const fn span(&self) -> &Span {
            &self.0
          }

          #[doc = "Returns the parser for the `" $label "`."]
          pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
          where
            I: $crate::__private::Source<'src>,
            I::Token: $crate::__private::Char + 'src,
            I::Slice: $crate::__private::Slice<Token = I::Token>,
            E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
            Span: $crate::__private::FromMapExtra<'src, I, E>,
          {
            use $crate::__private::{Char as _, chumsky::Parser as _};

            $crate::__private::chumsky::prelude::just($expr)
              .map_with(|_, sp| Self($crate::__private::FromMapExtra::from_map_extra(sp)))
          }
        }
      )*
    }
  };
}

/// Language related parsers.
///
/// This module implements all content belongs to the GraphQL language specification.
///
/// Spec: [Language spec](https://spec.graphql.org/draft/#sec-Language)
pub mod lang;

/// Definations parsers
pub mod definitions;

/// Source trait and implementations
pub mod source {
  pub use smear_utils::*;
}

#[doc(hidden)]
pub mod __private {
  pub use chumsky;

  pub use super::source::*;
}

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
