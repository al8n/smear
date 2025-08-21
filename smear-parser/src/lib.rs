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
    $name:ident: $expr:expr
  ),+$(,)?) => {
    $(
      $(#[$meta])*
      #[derive(Debug, Clone, Copy)]
      pub struct $name<Src, Span>($crate::__private::Spanned<Src, Span>);

      impl<Src, Span> $name<Src, Span> {
        /// Returns the span
        #[inline]
        pub const fn span(&self) -> &$crate::__private::Spanned<Src, Span> {
          &self.0
        }

        /// Returns the parser.
        pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
        where
          I: $crate::__private::chumsky::input::StrInput<'src, Slice = Src, Span = Span>,
          I::Token: $crate::__private::Char + 'src,
          E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
          E::Error:
            $crate::__private::chumsky::label::LabelError<'src, I, $crate::__private::chumsky::text::TextExpected<'src, I>>
            + $crate::__private::chumsky::label::LabelError<'src, I, $crate::__private::chumsky::util::MaybeRef<'src, I::Token>>,
        {
          use ::core::convert::From;
          use $crate::__private::{Char as _, chumsky::Parser as _};

          $crate::__private::chumsky::prelude::just($expr)
            .map_with(|_, sp| Self($crate::__private::Spanned::from(sp)))
        }
      }
    )*
  };
}

/// Language constructs
pub mod language;

/// Definations parsers
pub mod definitions;

/// The common keywords
pub mod keywords;

/// The name of the schema
pub mod name;

/// The parser
pub mod parser;

/// The spanned
pub mod spanned;

/// Character trait and implementations
pub mod char;

mod utils;

#[doc(hidden)]
pub mod __private {
  pub use chumsky;

  pub use super::{char::Char, spanned::Spanned};
}

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
