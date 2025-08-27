use chumsky::{extra::ParserExtra, prelude::*};

use crate::source::*;

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
/// ## Grammar
///
/// ```text
/// UintValue ::= '0' | [1-9][0-9]*
/// ```
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
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the uint value.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
  {
    one_of(I::NON_ZERO_DIGITS)
      .then(one_of(I::DIGITS).repeated().ignored())
      .ignored()
      .map_with(|_, sp| Self(Span::from_map_extra(sp)))
      .or(just(I::Token::ZERO).map_with(|_, sp| Self(Span::from_map_extra(sp))))
  }
}

impl<Span> AsRef<Span> for UintValue<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for UintValue<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for UintValue<Span> {
  type Components = Span;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into_span()
  }
}

#[cfg(test)]
mod tests {
  use crate::source::WithSource;

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
