use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{
  char::Char,
  digits::Digits,
  language::punct::{Dot, Minus, Plus},
  source::Source,
  spanned::Spanned,
};

use super::IntValue;

/// Represents the sign of exponent.
///
/// This enum captures either a positive (`+`) or negative (`-`) sign token
/// with its associated source span information. Used primarily for parsing
/// exponent signs in float literals.
///
/// # Examples
///
/// ```text
/// 1e+5  // Positive sign in exponent
/// 1e-3  // Negative sign in exponent
/// ```
///
/// Spec: [Sign](https://spec.graphql.org/draft/#Sign)
#[derive(
  Debug,
  Clone,
  Copy,
  PartialEq,
  Eq,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExponentSign<Src, Span> {
  /// The positive sign `+`
  Positive(Plus<Src, Span>),
  /// The negative sign `-`
  Negative(Minus<Src, Span>),
}

impl<Src, Span> ExponentSign<Src, Span> {
  /// Returns the source span of the exponent sign token.
  ///
  /// This provides access to the original source location where the sign
  /// token was found, useful for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Positive(p) => p.span(),
      Self::Negative(n) => n.span(),
    }
  }

  #[inline]
  pub fn into_span(self) -> Spanned<Src, Span> {
    match self {
      Self::Positive(plus) => plus.into_span(),
      Self::Negative(minus) => minus.into_span(),
    }
  }

  /// Creates a parser for exponent sign (`+` or `-`).
  ///
  /// This parser will successfully match either a plus or minus character,
  /// returning the appropriate `ExponentSign` variant. Typically used when parsing
  /// exponent parts of float literals where a sign is optional.
  ///
  /// ```text
  /// +  // -> ExponentSign::Positive
  /// -  // -> ExponentSign::Negative
  /// ```
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Minus::parser()
      .map(Self::Negative)
      .or(Plus::parser().map(Self::Positive))
      .labelled("sign")
  }
}

/// Represents the exponent indicator (`e` or `E`) in a float literal.
///
/// This struct captures the source location of the exponent marker character
/// that separates the base number from the exponent part in scientific notation.
///
/// ```text
/// 1.23e4   // 'e' is the exponent identifier
/// 4.56E-2  // 'E' is the exponent identifier
/// ```
///
/// Spec: [ExponentIndicator](https://spec.graphql.org/draft/#ExponentIndicator)
#[derive(Debug, Clone, Copy)]
pub struct ExponentIdentifier<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> ExponentIdentifier<Src, Span> {
  /// Returns the source span of the exponent identifier character.
  ///
  /// This provides the exact location in the source where the `e` or `E`
  /// character was found, useful for error reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }

  pub fn into_span(self) -> Spanned<Src, Span> {
    self.0
  }

  /// Creates a parser for exponent identifier characters (`e` or `E`).
  ///
  /// This parser matches either lowercase `e` or uppercase `E`, which are
  /// both valid exponent indicators in GraphQL float literals according to
  /// the specification.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    just(I::Token::e)
      .or(just(I::Token::E))
      .map_with(|_, span| Self(Spanned::from(span)))
      .labelled("exponent identifier")
  }
}

/// Represents the complete exponent part of a float literal.
///
/// An exponent consists of an exponent identifier (`e` or `E`), an optional
/// sign (`+` or `-`), and one or more digits. This struct captures all these
/// components along with their source spans.
///
/// ## Structure
///
/// ```text
/// e[sign][digits]
/// ```
///
/// ## Examples
///
/// ```text
/// e10    // identifier='e', sign=None, digits='10'
/// E+5    // identifier='E', sign=Some(+), digits='5'
/// e-23   // identifier='e', sign=Some(-), digits='23'
/// ```
///
/// Spec: [ExponentPart](https://spec.graphql.org/draft/#ExponentPart)
#[derive(Debug, Clone, Copy)]
pub struct Exponent<Src, Span> {
  /// The span of the whole exponent part.
  span: Spanned<Src, Span>,
  /// The span of the exponent char, e.g. `e` or `E`
  e: ExponentIdentifier<Src, Span>,
  /// The span of the sign char, e.g. `+` or `-`
  sign: Option<ExponentSign<Src, Span>>,
  /// The span of the digits part, e.g. `123`
  digits: Digits<Src, Span>,
}

impl<Src, Span> Exponent<Src, Span> {
  /// Returns the source span of the entire exponent part.
  ///
  /// This span covers from the exponent identifier through the last digit,
  /// including any sign character that may be present.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the exponent identifier (`e` or `E`) part.
  ///
  /// This provides access to the specific character used to indicate the
  /// start of the exponent, along with its source location.
  #[inline]
  pub const fn identifier(&self) -> &ExponentIdentifier<Src, Span> {
    &self.e
  }

  /// Returns the optional sign of the exponent.
  ///
  /// The sign is optional in exponent notation. If no sign is present,
  /// the exponent is considered positive by default.
  #[inline]
  pub const fn sign(&self) -> Option<&ExponentSign<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the digits part of the exponent.
  ///
  /// This is the numeric value of the exponent, which must contain at least
  /// one digit according to the GraphQL specification.
  #[inline]
  pub const fn digits(&self) -> &Digits<Src, Span> {
    &self.digits
  }

  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    ExponentIdentifier<Src, Span>,
    Option<ExponentSign<Src, Span>>,
    Digits<Src, Span>,
  ) {
    (self.span, self.e, self.sign, self.digits)
  }

  /// Creates a parser for exponent parts.
  ///
  /// This parser expects the complete exponent structure: an identifier (`e` or `E`),
  /// an optional sign (`+` or `-`), followed by at least one digit. The parser will
  /// fail if any required component is missing.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    ExponentIdentifier::parser()
      .then(Sign::parser().or_not())
      .then(Digits::parser())
      .map_with(|((e, sign), digits), span| Exponent {
        span: Spanned::from(span),
        e,
        sign,
        digits,
      })
      .labelled("exponent")
  }
}

/// Represents the fractional part of a float literal.
///
/// A fractional part consists of a decimal point (`.`) followed by one or more
/// digits. This is the portion that represents the decimal places in a floating
/// point number.
///
/// ## Structure
///
/// ```text
/// .[digits]
/// ```
///
/// ## Examples
///
/// ```text
/// .123    // dot='.', digits='123'
/// .0      // dot='.', digits='0'
/// .999    // dot='.', digits='999'
/// ```
///
/// Spec: [FractionalPart](https://spec.graphql.org/draft/#FractionalPart)
#[derive(Debug, Clone, Copy)]
pub struct Fractional<Src, Span> {
  /// The span of the whole fractional part.
  span: Spanned<Src, Span>,
  /// The span of the dot character, e.g. `.`
  dot: Dot<Src, Span>,
  /// The span of the digits part, e.g. `123`
  digits: Digits<Src, Span>,
}

impl<Src, Span> Fractional<Src, Span> {
  /// Returns the source span of the entire fractional part.
  ///
  /// This span covers from the decimal point through the last fractional digit.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the decimal point character.
  ///
  /// This provides access to the dot character that separates the integer
  /// part from the fractional part, along with its source location.
  #[inline]
  pub const fn dot(&self) -> &Dot<Src, Span> {
    &self.dot
  }

  /// Returns the digits part of the fractional component.
  ///
  /// These are the digits that appear after the decimal point, representing
  /// the fractional portion of the number. At least one digit is required
  /// by the GraphQL specification.
  #[inline]
  pub const fn digits(&self) -> &Digits<Src, Span> {
    &self.digits
  }

  pub fn into_components(self) -> (Spanned<Src, Span>, Dot<Src, Span>, Digits<Src, Span>) {
    (self.span, self.dot, self.digits)
  }

  /// Creates a parser for fractional parts.
  ///
  /// This parser expects a decimal point (`.`) immediately followed by one or
  /// more digits. The parser will fail if the decimal point is not followed
  /// by at least one digit.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Dot::parser()
      .then(Digits::parser())
      .map_with(|(dot, digits), span| Fractional {
        span: Spanned::from(span),
        dot,
        digits,
      })
      .labelled("fractional")
  }
}

/// A GraphQL float literal value.
///
/// Represents a complete float literal as defined by the GraphQL specification.
/// A float literal must be one of two forms:
///
/// 1. **Integer with fractional part** (optionally with exponent):
///    - `IntValue` `.` `Digits` `Exponent?`
///    - Examples: `1.0`, `3.14`, `2.5e10`, `-0.123e-4`
///
/// 2. **Integer with exponent** (no fractional part):
///    - `IntValue` `Exponent`
///    - Examples: `1e10`, `5E-3`, `-2e+8`
///
/// ## Constraints
///
/// - Must have either a fractional part, an exponent, or both
/// - Cannot have a leading `+` sign on the integer part
/// - Integer part cannot have leading zeros (except for `0` itself)
/// - Fractional part must have at least one digit after the decimal point
/// - Exponent must have at least one digit after the identifier and optional sign
///
/// ## Examples
///
/// **Valid float literals:**
/// ```text
/// 0.0          // Integer: 0, Fractional: .0
/// -0.1         // Integer: -0, Fractional: .1
/// 1e10         // Integer: 1, Exponent: e10
/// 1E+10        // Integer: 1, Exponent: E+10
/// 2E-3         // Integer: 2, Exponent: E-3
/// 3.14e-2      // Integer: 3, Fractional: .14, Exponent: e-2
/// -0.0         // Integer: -0, Fractional: .0
/// ```
///
/// **Invalid float literals:**
/// ```text
/// 1.           // Missing digits after decimal point
/// .1           // Missing integer part
/// 1e           // Missing exponent digits
/// 1E+          // Missing exponent digits
/// +1.0         // Leading + not allowed
/// 01.2         // Leading zeros not allowed
/// ```
///
/// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
#[derive(Debug, Clone, Copy)]
pub struct FloatValue<Src, Span> {
  /// The span of the float value
  span: Spanned<Src, Span>,
  /// The integer section of the float value
  int: IntValue<Src, Span>,
  /// The fractional section of the float value
  fractional: Option<Fractional<Src, Span>>,
  /// The exponent section of the float value
  exponent: Option<Exponent<Src, Span>>,
}

impl<Src, Span> FloatValue<Src, Span> {
  /// Returns the source span of the entire float literal.
  ///
  /// This span covers from the first character of the integer part through
  /// the last character of the exponent (if present), or through the last
  /// digit of the fractional part (if no exponent is present).
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the integer part of the float literal.
  ///
  /// Every float literal begins with an integer part, which follows the same
  /// rules as GraphQL integer literals (optional minus sign, no leading zeros
  /// except for the value `0`).
  pub const fn int(&self) -> &IntValue<Src, Span> {
    &self.int
  }

  /// Returns the fractional part of the float literal, if present.
  ///
  /// The fractional part consists of a decimal point followed by one or more
  /// digits. Not all float literals have a fractional part (those with only
  /// an exponent don't).
  pub const fn fractional(&self) -> Option<&Fractional<Src, Span>> {
    self.fractional.as_ref()
  }

  /// Returns the exponent part of the float literal, if present.
  ///
  /// The exponent part consists of an exponent identifier (`e` or `E`),
  /// an optional sign, and one or more digits. Not all float literals
  /// have an exponent part (those with only a fractional part don't).
  pub const fn exponent(&self) -> Option<&Exponent<Src, Span>> {
    self.exponent.as_ref()
  }

  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    IntValue<Src, Span>,
    Option<Fractional<Src, Span>>,
    Option<Exponent<Src, Span>>,
  ) {
    (self.span, self.int, self.fractional, self.exponent)
  }

  /// Creates a parser for GraphQL float literals.
  ///
  /// This parser implements the complete GraphQL float literal specification,
  /// handling both required forms:
  ///
  /// 1. Integer + fractional + optional exponent
  /// 2. Integer + required exponent (no fractional)
  ///
  /// The parser enforces all GraphQL constraints including proper integer format,
  /// required digits after decimal points and exponent indicators, and proper
  /// sign usage.
  ///
  /// ## Parsing Strategy
  ///
  /// The parser uses an alternation between two patterns:
  /// 1. First tries to parse: `IntValue` + `Fractional` + optional `Exponent`
  /// 2. Falls back to: `IntValue` + required `Exponent`
  ///
  /// This ensures that literals like `1e10` are parsed correctly (as integer + exponent)
  /// rather than failing when looking for a fractional part.
  ///
  /// ## Examples
  ///
  /// ```text
  /// // Successful parses:
  /// "3.14"     -> FloatValue { int: 3, fractional: Some(.14), exponent: None }
  /// "1e10"     -> FloatValue { int: 1, fractional: None, exponent: Some(e10) }
  /// "2.5e-3"   -> FloatValue { int: 2, fractional: Some(.5), exponent: Some(e-3) }
  ///
  /// // Parse failures:
  /// "1."       -> Error: missing digits after decimal
  /// ".5"       -> Error: missing integer part  
  /// "1e"       -> Error: missing exponent digits
  /// ```
  ///
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    IntValue::<Src, Span>::parser()
      .then(Fractional::parser().then(Exponent::parser().or_not()))
      .map_with(|(int, (frac, exp)), span| Self {
        span: Spanned::from(span),
        int,
        fractional: Some(frac),
        exponent: exp,
      })
      .or(
        IntValue::<Src, Span>::parser()
          .then(Exponent::parser())
          .map_with(|(int, exp), span| Self {
            span: Spanned::from(span),
            int,
            fractional: None,
            exponent: Some(exp),
          }),
      )
      .labelled("float value")
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use chumsky::{error::Simple, extra};

  fn float_parser<'a>(
  ) -> impl Parser<'a, &'a str, FloatValue<&'a str, SimpleSpan>, extra::Err<Simple<'a, char>>> + Clone
  {
    FloatValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end())
  }

  #[test]
  fn valid_float_values_components() {
    // Reuse your helper if it's already defined; otherwise inline like:
    // let float_parser = || FloatValue::<&str, SimpleSpan>::parser::<&str, extra::Err<Simple<char>>>().then_ignore(end());
    let p = float_parser();

    #[derive(Clone, Copy)]
    struct Case {
      s: &'static str,
      int_sign: Option<char>,   // '-' or None (GraphQL doesn't allow '+' here)
      int_digits: &'static str, // digits of the IntValue (no sign)
      frac_digits: Option<&'static str>,
      exp_marker: Option<char>,         // 'e' | 'E'
      exp_sign: Option<char>,           // '+' | '-' | None
      exp_digits: Option<&'static str>, // digits after the exponent (1+)
    }

    let cases = [
      Case {
        s: "4.123",
        int_sign: None,
        int_digits: "4",
        frac_digits: Some("123"),
        exp_marker: None,
        exp_sign: None,
        exp_digits: None,
      },
      Case {
        s: "-4.123",
        int_sign: Some('-'),
        int_digits: "4",
        frac_digits: Some("123"),
        exp_marker: None,
        exp_sign: None,
        exp_digits: None,
      },
      Case {
        s: "0.123",
        int_sign: None,
        int_digits: "0",
        frac_digits: Some("123"),
        exp_marker: None,
        exp_sign: None,
        exp_digits: None,
      },
      Case {
        s: "123e4",
        int_sign: None,
        int_digits: "123",
        frac_digits: None,
        exp_marker: Some('e'),
        exp_sign: None,
        exp_digits: Some("4"),
      },
      Case {
        s: "123E4",
        int_sign: None,
        int_digits: "123",
        frac_digits: None,
        exp_marker: Some('E'),
        exp_sign: None,
        exp_digits: Some("4"),
      },
      Case {
        s: "123e-4",
        int_sign: None,
        int_digits: "123",
        frac_digits: None,
        exp_marker: Some('e'),
        exp_sign: Some('-'),
        exp_digits: Some("4"),
      },
      Case {
        s: "123e+4",
        int_sign: None,
        int_digits: "123",
        frac_digits: None,
        exp_marker: Some('e'),
        exp_sign: Some('+'),
        exp_digits: Some("4"),
      },
      Case {
        s: "-1.123e4",
        int_sign: Some('-'),
        int_digits: "1",
        frac_digits: Some("123"),
        exp_marker: Some('e'),
        exp_sign: None,
        exp_digits: Some("4"),
      },
      Case {
        s: "-1.123E4",
        int_sign: Some('-'),
        int_digits: "1",
        frac_digits: Some("123"),
        exp_marker: Some('E'),
        exp_sign: None,
        exp_digits: Some("4"),
      },
      Case {
        s: "-1.123e-4",
        int_sign: Some('-'),
        int_digits: "1",
        frac_digits: Some("123"),
        exp_marker: Some('e'),
        exp_sign: Some('-'),
        exp_digits: Some("4"),
      },
      Case {
        s: "-1.123e+4",
        int_sign: Some('-'),
        int_digits: "1",
        frac_digits: Some("123"),
        exp_marker: Some('e'),
        exp_sign: Some('+'),
        exp_digits: Some("4"),
      },
      Case {
        s: "-1.123e4567",
        int_sign: Some('-'),
        int_digits: "1",
        frac_digits: Some("123"),
        exp_marker: Some('e'),
        exp_sign: None,
        exp_digits: Some("4567"),
      },
    ];

    for c in cases {
      let fv = p.parse(c.s).into_result().expect(c.s);

      // Whole literal span matches input
      assert_eq!(
        fv.span().source(),
        &c.s,
        "span should cover entire literal: {}",
        c.s
      );

      // ---- Int part ----
      assert_eq!(
        fv.int().digits().span().source(),
        &c.int_digits,
        "int digits mismatch: {}",
        c.s
      );
      match (c.int_sign, fv.int().sign()) {
        (None, None) => {}
        (Some('-'), Some(_minus)) => {} // IntValue only permits '-', never '+'
        (expected, got) => panic!(
          "int sign mismatch for `{}`: expected {:?}, got {:?}",
          c.s,
          expected,
          got.map(|_| '-')
        ),
      }

      // ---- Fractional part ----
      match (c.frac_digits, fv.fractional()) {
        (None, None) => {}
        (Some(d), Some(fr)) => {
          assert_eq!(
            fr.dot().span().source(),
            &".",
            "dot missing/incorrect: {}",
            c.s
          );
          assert_eq!(
            fr.digits().span().source(),
            &d,
            "fractional digits mismatch: {}",
            c.s
          );
        }
        (expected, got) => panic!(
          "fractional presence mismatch for `{}`: expected {:?}, got {:?}",
          c.s,
          expected.is_some(),
          got.is_some()
        ),
      }

      // ---- Exponent part ----
      match (c.exp_marker, c.exp_sign, c.exp_digits, fv.exponent()) {
        (None, None, None, None) => {}
        (Some(m), sgn, Some(d), Some(exp)) => {
          let marker = if m == 'e' { "e" } else { "E" };
          assert_eq!(
            exp.e().span().source(),
            &marker,
            "exp marker mismatch: {}",
            c.s
          );

          match (sgn, exp.sign()) {
            (None, None) => {}
            (Some('+'), Some(super::Sign::Positive(_))) => {}
            (Some('-'), Some(super::Sign::Negative(_))) => {}
            (expected, got) => panic!(
              "exp sign mismatch for `{}`: expected {:?}, got {:?}",
              c.s,
              expected,
              got.map(|_| '?')
            ),
          }

          assert_eq!(
            exp.digits().span().source(),
            &d,
            "exp digits mismatch: {}",
            c.s
          );
        }
        _ => panic!("exponent presence/shape mismatch for `{}`", c.s),
      }
    }
  }

  #[test]
  fn valid_fraction_only() {
    for s in ["0.0", "-0.1", "7.5", "1234567890.987654321"] {
      assert!(
        float_parser().parse(s).into_result().is_ok(),
        "should accept `{s}`"
      );
    }
  }

  #[test]
  fn valid_exponent_only() {
    for s in ["0e0", "1e10", "1E+10", "2E-3", "-3e8", "-0e10"] {
      assert!(
        float_parser().parse(s).into_result().is_ok(),
        "should accept `{s}`"
      );
    }
  }

  #[test]
  fn valid_fraction_and_exponent() {
    for s in ["3.14e-2", "-1.0E+5", "6.022E23", "2.0e0"] {
      assert!(
        float_parser().parse(s).into_result().is_ok(),
        "should accept `{s}`"
      );
    }
  }

  #[test]
  fn invalid_missing_fraction_digits() {
    for s in ["1.", "-0."] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn invalid_missing_leading_int_with_dot() {
    for s in [".1", ".0e1"] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn invalid_exponent_digits() {
    for s in ["1e", "1E+", "0.0e", "2.e-"] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn invalid_plus_on_int_part() {
    for s in ["+1.0", "+1e2"] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn invalid_leading_zero_in_int_part() {
    for s in ["01.2", "00e1", "-01.0"] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn invalid_trailing_garbage() {
    for s in ["1.0x", "1e2x", "3.14.15"] {
      assert!(
        float_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  #[test]
  fn spans_and_sections_accessible() {
    let fv = float_parser().parse("3.14e2").into_result().unwrap();
    // Accessors exist and are wired
    let _ = fv.span();
    let _ = fv.int();
    let _ = fv.fractional().unwrap().dot();
    let _ = fv.fractional().unwrap().digits();
    let _ = fv.exponent().unwrap().e();
    let _ = fv.exponent().unwrap().sign();
    let _ = fv.exponent().unwrap().digits();
  }
}
