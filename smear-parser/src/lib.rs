#![doc = include_str!("../../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_docs, warnings)]

use chumsky::{combinator::Repeated, container::Seq, prelude::*};

#[cfg(all(feature = "alloc", not(feature = "std")))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The parser
pub mod parser;
mod utils;

#[derive(Debug, Clone, Copy)]
pub struct Spanned<D, S = SimpleSpan<usize, ()>> {
  data: D,
  span: S,
}

impl<D, S> Spanned<D, S> {
  /// Create a new `Spanned` value.
  #[inline]
  pub const fn new(data: D, span: S) -> Self {
    Self { data, span }
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }

  /// Returns the data holds by this span
  #[inline]
  pub const fn data(&self) -> &D {
    &self.data
  }
}

// type Spanned<T> = (T, SimpleSpan);

// pub enum Document {

// }

// fn parser<'a, E>() -> impl Parser<'a, &'a str, Document, E> {
//   recursive(|value| {
//     let ws = utils::whitespace!();
//     let line_terminator = utils::line_terminator!();

//     choice((
//       ws,
//       line_terminator,
//     ))
//   })
// }

#[cfg(all(feature = "std", test))]
mod tests;

#[test]
fn t() {}
