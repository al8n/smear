#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs)]
#![allow(clippy::type_complexity)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

pub use chumsky;

/// On some platforms, the linker fails to compile complex types, the reference issue is
/// [rust-lang/rust/issues#130729](https://github.com/rust-lang/rust/issues/130729).
///
/// This macro conditionally boxes parsers to reduce type complexity on platforms
/// where the issue occurs.
#[macro_export]
macro_rules! boxed {
  ($expr:expr) => {{
    #[cfg(not(target_os = "linux"))]
    {
      #[allow(unused_imports)]
      use $crate::__private::chumsky::prelude::Parser as _;
      $expr.boxed()
    }
    #[cfg(target_os = "linux")]
    $expr
  }};
}

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

        impl<Span> $crate::__private::parse::Parsable<Span> for $name<Span> {
          fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
          where
            I: $crate::__private::Source<'src>,
            I::Token: $crate::__private::Char + 'src,
            I::Slice: $crate::__private::Slice<Token = I::Token>,
            E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
            Span: $crate::__private::FromMapExtra<'src, I, E>,
          {
            Self::parser()
          }
        }

        #[cfg(test)]
        mod [< __ $name:snake _tests >] {
          use super::*;

          fn keyword_parser<'a>() -> impl $crate::__private::chumsky::Parser<
            'a,
            &'a ::core::primitive::str,
            $name<$crate::__private::WithSource<&'a ::core::primitive::str, $crate::__private::chumsky::prelude::SimpleSpan>>,
            $crate::__private::chumsky::extra::Err<$crate::__private::chumsky::prelude::Simple<'a, ::core::primitive::char>>,
          > + Clone {
            $name::<$crate::__private::WithSource<&::core::primitive::str, $crate::__private::chumsky::prelude::SimpleSpan>>::parser::<&::core::primitive::str, $crate::__private::chumsky::extra::Err<$crate::__private::chumsky::prelude::Simple<::core::primitive::char>>>(
            )
          }

          #[test]
          fn [< test_ $name:snake >]() {
            use $crate::__private::chumsky::prelude::Parser as _;

            let parser = keyword_parser();
            let result = parser.parse($label).into_result();
            assert!(result.is_ok());
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

/// Parsing related traits
pub mod parse;

pub use smear_utils::lexer;

/// Source trait and implementations
pub mod source {
  pub use smear_utils::*;
}

#[doc(hidden)]
pub mod __private {
  pub use chumsky;

  pub use super::{parse, source::*, lexer};
}

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
