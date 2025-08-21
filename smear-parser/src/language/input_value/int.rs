use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{
  char::Char,
  language::{input_value::UintValue, punct::Minus},
  source::Source,
  spanned::Spanned,
};

/// A GraphQL **Int** literal (base-10).
///
/// This parser recognizes integer *literals* exactly as specified by the GraphQL
/// draft spec’s **Int Value** grammar: <https://spec.graphql.org/draft/#sec-Int-Value>.
///
/// ## Note
///
/// Surrounding whitespace/comments are not handled here; please compose at a higher level.
///
/// ## Lexical rules enforced
/// - **Optional leading minus only**: `-` is allowed; **`+` is not**.
/// - **No leading zeros** for non-zero values:
///   - `0` is allowed.
///   - `01`, `-01`, `00` are **rejected**.
/// - **Digits are decimal** only (no hex, octal, or binary).
/// - **Integer only**: no decimal points or exponents (those are `Float` literals).
///
/// ## Semantics and validation
/// - This parser performs **lexical** validation only. It does **not** coerce or
///   clamp to GraphQL’s 32-bit runtime `Int`; do range checks in a higher layer.
/// - The grammar technically allows **`-0`**. This parser accepts `-0`. If your
///   project prefers to forbid or normalize it, enforce that in semantic checks.
///
/// ## Spans
/// - `span`: covers the entire literal (sign + digits if a sign is present).
/// - `sign`: the span for the leading minus, if present.
/// - `digits`: the span covering the digit sequence **only** (no sign).
///
/// ## Examples
///
/// Valid: `0`, `-0`, `7`, `-123`, `1234567890`  
/// Invalid: `01`, `-01`, `00`, `+1`, `-`, `1x`, `0x10`, `3.14`, `1e9`
#[derive(Debug, Clone, Copy)]
pub struct IntValue<Src, Span> {
  span: Spanned<Src, Span>,
  sign: Option<Minus<Src, Span>>,
  digits: UintValue<Src, Span>,
}

impl<Src, Span> IntValue<Src, Span> {
  /// Returns the span of the entire integer literal (sign + digits if present).
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the the leading minus sign, if present.
  pub const fn sign(&self) -> Option<&Minus<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the span of the unsigned integer part (without any sign).
  pub const fn digits(&self) -> &UintValue<Src, Span> {
    &self.digits
  }

  /// Parser for a GraphQL **Int** literal.
  ///
  /// Accepts an optional leading minus and a base-10 digit sequence with the
  /// “no leading zeros” rule described above.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Minus::parser()
      .or_not()
      .then(UintValue::parser())
      .map_with(|(sign, digits), sp| Self {
        span: Spanned::from(sp),
        sign,
        digits,
      })
      .labelled("int value")
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
