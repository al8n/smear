use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  char::Char,
  convert::*,
  language::{input_value::UintValue, punct::Minus},
  source::Source,
  spanned::Spanned,
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
/// ## Format
///
/// ```text
/// IntValue ::= '-'? ('0' | [1-9][0-9]*)
/// ```
///
/// ## Examples
///
/// **Valid integer literals:**
/// ```text
/// 0              // Zero
/// -0             // Negative zero (syntactically valid)
/// 7              // Single digit
/// -7             // Negative single digit
/// 42             // Multiple digits
/// -123           // Negative multiple digits
/// 1234567890     // Large number
/// -2147483648    // Large negative number
/// ```
///
/// **Invalid integer literals:**
/// ```text
/// +1             // Plus sign forbidden
/// +0             // Plus sign forbidden
/// 01             // Leading zero forbidden
/// -01            // Leading zero with sign forbidden
/// 00             // Multiple zeros forbidden
/// 007            // Leading zeros forbidden
/// 0x10           // Hexadecimal notation forbidden
/// 1.0            // Decimal point makes it a float
/// 1e10           // Exponent makes it a float
/// -              // Dangling minus sign
/// 1,000          // Thousands separators forbidden
/// ```
///
/// ## Component Structure
///
/// Each integer literal consists of:
/// - **Overall span**: Covers the entire literal including sign
/// - **Optional sign**: The `-` character if present (no `+` allowed)
/// - **Digits**: The unsigned integer portion (handled by [`UintValue`])
///
/// ## Lexical vs Semantic Validation
///
/// This parser performs **lexical validation only**:
/// - ✅ Validates syntax and formatting rules
/// - ✅ Ensures no leading zeros or forbidden characters
/// - ❌ Does **not** validate 32-bit integer range limits
/// - ❌ Does **not** handle overflow/underflow detection
/// - ❌ Does **not** normalize `-0` to `0`
///
/// Range validation and numeric conversion should be performed at a higher
/// level when converting to actual integer values for GraphQL execution.
///
/// ## Design Notes
///
/// - The parser accepts `-0` as syntactically valid per the specification
/// - Leading zero prevention avoids octal interpretation ambiguity
/// - No plus sign support maintains consistency with JSON numeric format
/// - Whitespace and comment handling is the caller's responsibility
///
/// ## Usage in GraphQL
///
/// Integer literals appear throughout GraphQL:
/// - **Query arguments**: `user(id: 123)`
/// - **Variable values**: `{ "count": 42 }`
/// - **Default values**: `field(limit: Int = 10)`
/// - **List elements**: `ids: [1, 2, 3]`
/// - **Input object fields**: `{ age: 25, score: -5 }`
///
/// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
#[derive(Debug, Clone, Copy)]
pub struct IntValue<Src, Span> {
  span: Spanned<Src, Span>,
  sign: Option<Minus<Src, Span>>,
  digits: UintValue<Src, Span>,
}

impl<Src, Span> AsSpanned<Src, Span> for IntValue<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for IntValue<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<Src, Span> IntoComponents for IntValue<Src, Span> {
  type Components = (
    Spanned<Src, Span>,
    Option<Minus<Src, Span>>,
    UintValue<Src, Span>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.sign, self.digits)
  }
}

impl<Src, Span> IntValue<Src, Span> {
  /// Returns the source span of the entire integer literal.
  ///
  /// This span covers from the first character (sign or first digit) through
  /// the last digit, providing the complete source location for error reporting,
  /// source mapping, and extracting the full literal text.
  pub const fn span(&self) -> &Spanned<Src, Span> {
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
  pub const fn sign(&self) -> Option<&Minus<Src, Span>> {
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
  pub const fn digits(&self) -> &UintValue<Src, Span> {
    &self.digits
  }

  /// Creates a parser for GraphQL integer literals.
  ///
  /// This parser implements the complete GraphQL integer literal specification,
  /// handling optional signs and enforcing all formatting constraints. It combines
  /// an optional minus sign with unsigned integer parsing to build complete
  /// signed integer literals.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
  {
    Minus::parser()
      .or_not()
      .then(UintValue::parser())
      .map_with(|(sign, digits), sp| Self {
        span: Spanned::from(sp),
        sign,
        digits,
      })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn int_parser<'a>(
  ) -> impl Parser<'a, &'a str, IntValue<&'a str, SimpleSpan>, extra::Err<Simple<'a, char>>> + Clone
  {
    IntValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end())
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
