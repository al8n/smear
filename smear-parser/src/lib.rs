#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_docs, warnings)]

#[cfg(not(feature = "std"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

macro_rules! word {
  ($(
    $(#[$meta:meta])*
    $label:literal:$name:ident: $expr:expr
  ),+$(,)?) => {
    paste::paste! {
      $(
        $(#[$meta])*
        #[derive(Debug, Clone, Copy)]
        pub struct $name<Src, Span>($crate::__private::Spanned<Src, Span>);

        impl<Src, Span> $crate::__private::AsSpanned<Src, Span> for $name<Src, Span> {
          #[inline]
          fn as_spanned(&self) -> &$crate::__private::Spanned<Src, Span> {
            self.span()
          }
        }

        impl<Src, Span> $crate::__private::IntoSpanned<Src, Span> for $name<Src, Span> {
          #[inline]
          fn into_spanned(self) -> $crate::__private::Spanned<Src, Span> {
            self.0
          }
        }

        impl<Src, Span> $crate::__private::IntoComponents for $name<Src, Span> {
          type Components = $crate::__private::Spanned<Src, Span>;

          #[inline]
          fn into_components(self) -> Self::Components {
            self.0
          }
        }

        impl<Src, Span> $name<Src, Span> {
          #[doc = "Returns the span of the `" $label "`."]
          #[inline]
          pub const fn span(&self) -> &$crate::__private::Spanned<Src, Span> {
            &self.0
          }

          #[doc = "Returns the parser for the `" $label "`."]
          pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
          where
            I: $crate::__private::Source<'src, Slice = Src, Span = Span>,
            I::Token: $crate::__private::Char + 'src,
            E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
            E::Error:
              $crate::__private::chumsky::label::LabelError<'src, I, &'static ::core::primitive::str>
          {
            use ::core::convert::From;
            use $crate::__private::{Char as _, chumsky::Parser as _};

            $crate::__private::chumsky::prelude::just($expr)
              .map_with(|_, sp| Self($crate::__private::Spanned::from(sp)))
              .labelled($label)
          }
        }
      )*
    }
  };
}

/// Language constructs
pub mod language;

/// Definations parsers
pub mod definitions;

/// The common keywords
pub mod keywords;

/// The name
pub mod name;

/// The digits
pub mod digits;

/// The parser
pub mod parser;

/// The spanned
pub mod spanned;

/// Character trait and implementations
pub mod char;

/// Conversion related traits
pub mod convert;

/// Source trait and implementations
pub mod source;

mod utils;

#[doc(hidden)]
pub mod __private {
  pub use chumsky;

  pub use super::{char::Char, source::Source, spanned::Spanned, convert::*};
}

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
