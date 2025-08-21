use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{char::Char, source::Source, spanned::Spanned};

/// An unsigned, base-10 integer token used as a building block for GraphQL numbers.
///
/// This is **not** a standalone GraphQL literal — it’s a helper that parses:
/// - `0`, or
/// - a non-zero digit followed by zero or more digits (`[1-9][0-9]*`)
///
/// ## Rules
/// - **No sign**: `+1`/`-1` are rejected.
/// - **No leading zeros**: `01`, `00` are rejected.
/// - **Base-10 only**.
///
/// The span returned by [`span`](UintValue::span) covers the full digit sequence.
#[derive(Debug, Clone, Copy)]
pub struct UintValue<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> UintValue<Src, Span> {
  /// Source span of the entire unsigned digit sequence.
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }

  /// Parser for an **unsigned**, base-10 integer token.
  ///
  /// Accepts either:
  /// - a lone `0`, or
  /// - a non-zero digit followed by zero or more digits (`[1-9][0-9]*`).
  ///
  /// **Rejects** any sign (`+`/`-`) and any leading zeros (`01`, `00`).
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    one_of(I::NON_ZERO_DIGITS)
      .then(one_of(I::DIGITS).repeated().ignored())
      .ignored()
      .map_with(|_, sp| Self(Spanned::from(sp)))
      .or(just(I::Token::ZERO).map_with(|_, sp| Self(Spanned::from(sp))))
      .labelled("uint value")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn uint_parser<'a>(
  ) -> impl Parser<'a, &'a str, UintValue<&'a str, SimpleSpan>, extra::Err<Simple<'a, char>>> + Clone
  {
    UintValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end())
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
