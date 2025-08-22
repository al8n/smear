use chumsky::{extra::ParserExtra, prelude::*};

use crate::{char::Char, convert::*, source::Source, spanned::Spanned};

/// An unsigned decimal integer component for GraphQL numeric literals.
///
/// This parser handles the unsigned integer portion that appears in GraphQL
/// integer and float literals. It enforces GraphQL's strict rules about numeric
/// formatting, particularly around leading zeros and sign handling.
///
/// ## Important Note
///
/// This is **not** a complete GraphQL literal parser by itself — it's a building
/// block used by higher-level parsers for integers (`IntValue`) and floats
/// (`FloatValue`). Those parsers handle signs and combine this with other
/// components as needed.
///
/// ## Specification Rules
///
/// GraphQL unsigned integers must follow these strict formatting rules:
/// - **Zero**: The single digit `0` is valid
/// - **Non-zero integers**: Must start with `1-9` followed by any number of `0-9`
/// - **No leading zeros**: Values like `01`, `007`, `00` are forbidden
/// - **No signs**: Values like `+1`, `-1` are handled at a higher level
/// - **Decimal digits only**: Base-10 representation required
///
/// ## Format
///
/// ```text
/// UintValue ::= '0' | [1-9][0-9]*
/// ```
///
/// ## Examples
///
/// **Valid unsigned integers:**
/// ```text
/// 0              // Zero is always valid
/// 1              // Single non-zero digit
/// 42             // Multiple digits starting with non-zero
/// 123456789      // Long sequence
/// 999999999999   // Very long sequence
/// ```
///
/// **Invalid formats:**
/// ```text
/// 01             // Leading zero forbidden
/// 007            // Leading zeros forbidden  
/// 00             // Multiple zeros forbidden
/// +1             // Plus sign not handled here
/// -1             // Minus sign not handled here
/// 1.0            // Decimal notation not handled here
/// 1e10           // Scientific notation not handled here
/// ```
///
/// ## Usage in GraphQL
///
/// This component appears in:
/// - **Integer literals**: `42`, `0`, `123456` (combined with optional minus sign)
/// - **Float integer parts**: In `3.14`, the `3` is parsed as a `UintValue`
/// - **Float exponent values**: In `1e42`, the `42` is parsed as a `UintValue`
///
/// ## Design Philosophy
///
/// This parser implements GraphQL's "no leading zeros" rule, which prevents
/// ambiguity about numeric base and ensures consistent formatting across
/// different GraphQL implementations. The rule exists because:
/// - Leading zeros might suggest octal notation in some languages
/// - Consistent formatting improves readability and prevents confusion
/// - It matches common JSON numeric formatting expectations
///
/// ## Error Prevention
///
/// Common mistakes this parser prevents:
/// - Using octal-style notation (`077`)
/// - Copy-pasting zero-padded numbers (`001`, `042`)
/// - Including unnecessary leading zeros from other systems
#[derive(Debug, Clone, Copy)]
pub struct UintValue<Span>(Span);

impl<Span> UintValue<Span> {
  /// Returns the source span of the unsigned integer.
  ///
  /// This provides access to the original source location and text of the
  /// unsigned integer, useful for error reporting, source mapping, extracting
  /// the actual numeric string for conversion, and building higher-level
  /// numeric AST nodes.
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser for unsigned decimal integers.
  ///
  /// This parser implements GraphQL's unsigned integer specification with
  /// strict validation of leading zero rules. It uses an alternation strategy
  /// to handle the two valid cases: zero and non-zero integers.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    one_of(I::NON_ZERO_DIGITS)
      .then(one_of(I::DIGITS).repeated().ignored())
      .ignored()
      .map_with(|_, sp| Self(Spanned::from_map_extra(sp)))
      .or(just(I::Token::ZERO).map_with(|_, sp| Self(Spanned::from_map_extra(sp))))
  }
}

impl<Span> AsRef<Span> for UintValue<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpanned<Span> for UintValue<Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for UintValue<Span> {
  type Components = Span;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_spanned()
  }
}

#[cfg(test)]
mod tests {
  use crate::spanned::WithSource;

  use super::*;

  fn uint_parser<'a>(
  ) -> impl Parser<'a, &'a str, UintValue<WithSource<&'a str, SimpleSpan>>, extra::Err<Simple<'a, char>>>
       + Clone {
    UintValue::<WithSource<&str, SimpleSpan>>::parser::<&str, extra::Err<Simple<char>>>()
      .then_ignore(end())
  }

  #[test]
  fn valid_zero() {
    let iv = uint_parser().parse("0").into_result().unwrap();
    assert_eq!(iv.span().source(), &"0");
  }

  #[test]
  fn valid_single_digit() {
    let iv = uint_parser().parse("7").into_result().unwrap();

    assert_eq!(iv.span().source(), &"7");
  }

  #[test]
  fn valid_multi_digit() {
    let iv = uint_parser().parse("1234567890").into_result().unwrap();

    assert_eq!(iv.span().source(), &"1234567890");
  }

  #[test]
  fn invalid_leading_zero() {
    assert!(uint_parser().parse("01").into_result().is_err());
    assert!(uint_parser().parse("-01").into_result().is_err());
    assert!(uint_parser().parse("00").into_result().is_err());
  }

  #[test]
  fn invalid_plus_sign() {
    assert!(uint_parser().parse("+1").into_result().is_err());
  }

  #[test]
  fn invalid_minus_values() {
    assert!(uint_parser().parse("-1").into_result().is_err());
    assert!(uint_parser().parse("-1234567890").into_result().is_err());
  }

  #[test]
  fn invalid_dangling_minus() {
    assert!(uint_parser().parse("-").into_result().is_err());
  }

  #[test]
  fn invalid_empty() {
    assert!(uint_parser().parse("").into_result().is_err());
  }

  #[test]
  fn invalid_trailing_garbage() {
    assert!(uint_parser().parse("1x").into_result().is_err());
    assert!(uint_parser().parse("0x10").into_result().is_err());
  }
}
