use chumsky::{extra::ParserExtra, prelude::*};

use crate::{char::Char, convert::*, source::Source, spanned::Spanned};

/// A sequence of decimal digits (`0-9`).
///
/// Represents one or more consecutive decimal digits as they appear in various
/// numeric literals within GraphQL. This is a fundamental building block used
/// in parsing integers, float fractional parts, and exponent values.
///
/// ## Specification
///
/// - Accepts only decimal digits: `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`
/// - Requires at least one digit (cannot be empty)
/// - No limit on maximum length
/// - Does not validate numeric range or overflow
/// - Does not handle signs, decimal points, or exponent notation
///
/// ## Format
///
/// ```text
/// Digits ::= [0-9]+
/// ```
///
/// ## Examples
///
/// **Valid digit sequences:**
/// ```text
/// 0              // Single digit
/// 1              // Single digit
/// 42             // Multiple digits
/// 123456789      // Long sequence
/// 007            // Leading zeros allowed
/// 999999999999   // Very long sequence
/// ```
///
/// **Invalid sequences:**
/// ```text
/// ""             // Empty string
/// abc            // Letters not allowed
/// 1.23           // Decimal point not handled here
/// -42            // Negative sign not handled here
/// 1e10           // Exponent notation not handled here
/// 1,000          // Commas not allowed
/// ```
///
/// ## Usage in GraphQL
///
/// This parser is used as a component in parsing:
/// - **Integer literals**: `42`, `0`, `123456`
/// - **Float fractional parts**: `.123` (digits = `123`)
/// - **Float exponent values**: `e42` (digits = `42`)
/// - **Any numeric component requiring digit sequences**
///
/// ## Design Notes
///
/// This parser is intentionally simple and focused solely on recognizing
/// sequences of decimal digits. It does not:
/// - Validate numeric ranges or prevent overflow
/// - Handle leading zeros (validation is caller's responsibility)
/// - Parse signs, decimal points, or scientific notation
/// - Perform any numeric conversion
///
/// Higher-level parsers combine this with other components to build complete
/// numeric literals with proper validation and semantic meaning.
#[derive(Debug, Clone, Copy)]
pub struct Digits<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> Digits<Src, Span> {
  /// Returns the source span of the digit sequence.
  ///
  /// This provides access to the original source location and text of the
  /// digits, useful for error reporting, source mapping, extracting the
  /// actual numeric string, and building higher-level numeric types.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }

  /// Creates a parser for sequences of decimal digits.
  ///
  /// This parser matches one or more consecutive decimal digits (`0-9`).
  /// It is designed to be a reusable component for building more complex
  /// numeric parsers in GraphQL literals.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
  {
    one_of(I::DIGITS)
      .repeated()
      .at_least(1)
      .ignored()
      .map_with(|_, sp| Self(Spanned::from(sp)))
  }
}

impl<Src, Span> AsSpanned<Src, Span> for Digits<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for Digits<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.0
  }
}

impl<Src, Span> IntoComponents for Digits<Src, Span> {
  type Components = Spanned<Src, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_spanned()
  }
}
