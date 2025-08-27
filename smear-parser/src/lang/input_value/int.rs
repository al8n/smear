use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::{punct::Minus, UintValue},
  source::*,
};

/// A GraphQL integer literal value.
///
/// Represents a complete integer literal as defined by the GraphQL specification.
/// Integer literals are signed decimal numbers that follow strict formatting rules
/// to ensure consistent parsing and prevent ambiguity about numeric representation.
///
/// ## Specification Rules
///
/// GraphQL integer literals must follow these formatting constraints:
/// - **Optional leading minus**: `-` is allowed for negative values
/// - **No plus sign**: `+` is explicitly forbidden (unlike some languages)
/// - **No leading zeros**: Prevents confusion with octal notation (`01`, `007` rejected)
/// - **Decimal only**: No hexadecimal (`0x10`), octal (`077`), or binary (`0b101`) notation
/// - **Integer only**: No decimal points (`.`) or exponents (`e`) - those create floats
/// - **Zero handling**: Both `0` and `-0` are syntactically valid
///
/// ## Grammar
///
/// ```text
/// IntValue ::= '-'? ('0' | [1-9][0-9]*)
/// ```
///
/// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
#[derive(Debug, Clone, Copy)]
pub struct IntValue<Span> {
  span: Span,
  sign: Option<Minus<Span>>,
  digits: UintValue<Span>,
}

impl<Span> AsRef<Span> for IntValue<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for IntValue<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for IntValue<Span> {
  type Components = (Span, Option<Minus<Span>>, UintValue<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.sign, self.digits)
  }
}

impl<Span> IntValue<Span> {
  /// Returns the source span of the entire integer literal.
  ///
  /// This span covers from the first character (sign or first digit) through
  /// the last digit, providing the complete source location for error reporting,
  /// source mapping, and extracting the full literal text.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the optional minus sign component.
  ///
  /// This provides access to the leading minus sign if the integer is negative.
  /// Returns `Some(Minus)` for negative literals, `None` for positive literals.
  /// Note that positive literals never have an explicit `+` sign in GraphQL.
  ///
  /// # Examples
  ///
  /// ```text
  /// 42      // Returns None
  /// -42     // Returns Some(Minus)
  /// 0       // Returns None  
  /// -0      // Returns Some(Minus)
  /// ```
  pub const fn sign(&self) -> Option<&Minus<Span>> {
    self.sign.as_ref()
  }

  /// Returns the unsigned integer component (digits only).
  ///
  /// This provides access to the numeric digits without any sign information.
  /// The digits follow GraphQL's unsigned integer rules (no leading zeros
  /// except for the value `0` itself).
  ///
  /// # Examples
  ///
  /// ```text
  /// 42      // digits = UintValue("42")
  /// -42     // digits = UintValue("42")
  /// 0       // digits = UintValue("0")
  /// -0      // digits = UintValue("0")
  /// ```
  pub const fn digits(&self) -> &UintValue<Span> {
    &self.digits
  }

  /// Creates a parser for GraphQL integer literals.
  ///
  /// This parser implements the complete GraphQL integer literal specification,
  /// handling optional signs and enforcing all formatting constraints. It combines
  /// an optional minus sign with unsigned integer parsing to build complete
  /// signed integer literals.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the int value.
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
    Minus::parser()
      .or_not()
      .then(UintValue::parser())
      .map_with(|(sign, digits), sp| Self {
        span: Span::from_map_extra(sp),
        sign,
        digits,
      })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::source::WithSource;

  fn int_parser<'a>(
  ) -> impl Parser<'a, &'a str, IntValue<WithSource<&'a str, SimpleSpan>>, extra::Err<Simple<'a, char>>>
       + Clone {
    IntValue::<WithSource<&str, SimpleSpan>>::parser::<&str, extra::Err<Simple<char>>>()
      .then_ignore(end())
  }

  #[test]
  fn valid_zero() {
    let iv = int_parser().parse("0").into_result().unwrap();
    assert!(iv.sign().is_none());
    assert_eq!(iv.digits().span().source(), &"0");
  }

  #[test]
  fn valid_negative_zero() {
    let iv = int_parser().parse("-0").into_result().unwrap();
    assert!(iv.sign().is_some());
    assert_eq!(iv.digits().span().source(), &"0");
  }

  #[test]
  fn valid_single_digit() {
    let iv = int_parser().parse("7").into_result().unwrap();
    assert!(iv.sign().is_none());
    assert_eq!(iv.digits().span().source(), &"7");
  }

  #[test]
  fn valid_multi_digit() {
    let iv = int_parser().parse("1234567890").into_result().unwrap();
    assert!(iv.sign().is_none());
    assert_eq!(iv.digits().span().source(), &"1234567890");
  }

  #[test]
  fn valid_negative_multi_digit() {
    let iv = int_parser().parse("-987654321").into_result().unwrap();
    assert!(iv.sign().is_some());
    assert_eq!(iv.digits().span().source(), &"987654321");
  }

  #[test]
  fn invalid_leading_zero() {
    assert!(int_parser().parse("01").into_result().is_err());
    assert!(int_parser().parse("-01").into_result().is_err());
    assert!(int_parser().parse("00").into_result().is_err());
  }

  #[test]
  fn invalid_plus_sign() {
    assert!(int_parser().parse("+1").into_result().is_err());
  }

  #[test]
  fn invalid_dangling_minus() {
    assert!(int_parser().parse("-").into_result().is_err());
  }

  #[test]
  fn invalid_empty() {
    assert!(int_parser().parse("").into_result().is_err());
  }

  #[test]
  fn invalid_trailing_garbage() {
    assert!(int_parser().parse("1x").into_result().is_err());
    assert!(int_parser().parse("0x10").into_result().is_err());
  }
}
