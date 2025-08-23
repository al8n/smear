use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  source::{Char, Slice, Source},
};

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
/// ## Grammar
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
pub struct Digits<Span>(Span);

impl<Span> Digits<Span> {
  /// Returns the source span of the digit sequence.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser for sequences of decimal digits.
  ///
  /// This parser matches one or more consecutive decimal digits (`0-9`).
  /// It is designed to be a reusable component for building more complex
  /// numeric parsers in GraphQL literals.
  /// 
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the digits.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    one_of(I::DIGITS)
      .repeated()
      .at_least(1)
      .ignored()
      .map_with(|_, sp| Self(Span::from_map_extra(sp)))
  }
}

impl<Span> AsRef<Span> for Digits<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Digits<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for Digits<Span> {
  type Components = Span;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_span()
  }
}
