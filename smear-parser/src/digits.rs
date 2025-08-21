use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{char::Char, source::Source, spanned::Spanned};

/// A sequence of decimal digits.
///
/// By design this parser accepts **one or more** digits (it can succeed empty),
/// which makes it handy for optional components (e.g., fraction/exponent tails).
#[derive(Debug, Clone, Copy)]
pub struct Digits<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> Digits<Src, Span> {
  /// Source span of the non-empty digit sequence.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }

  /// Parser for **one or more** decimal digits.
  ///
  /// Useful for optional components (e.g., fraction/exponent tails).
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    one_of(I::DIGITS)
      .repeated()
      .at_least(1)
      .ignored()
      .map_with(|_, sp| Self(Spanned::from(sp)))
  }
}
