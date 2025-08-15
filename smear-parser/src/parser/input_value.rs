use chumsky::{
  extra::ParserExtra,
  input::StrInput,
  label::LabelError,
  prelude::*,
  span::Span,
  text::{Char, TextExpected},
  util::MaybeRef,
};

use crate::{
  parser::{
    punct::{LBrace, LBracket, Quote, RBrace, RBracket, TripleQuote},
    SmearChar, StrSpan,
  },
  Spanned,
};

/// Represents a raw integer value parsed from input.
///
/// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawIntValue<S> {
  /// The original raw string representation of the integer value.
  pub span: S,
  /// Returns the sign of the value.
  pub sign: Option<S>,
  /// The integer part of the value.
  pub digits: S,
}

/// The sign of the float value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign<S> {
  /// The positive sign `+`
  Positive(S),
  /// The negative sign `-`
  Negative(S),
}

/// Represents the exponent part of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Exponent<S> {
  /// The span of the whole exponent part.
  pub span: S,
  /// The span of the exponent char, e.g. `e` or `E`
  pub e: S,
  /// The span of the sign char, e.g. `+` or `-`
  pub sign: Option<Sign<S>>,
  /// The span of the digits part, e.g. `123`
  pub digits: S,
}

impl<S> Exponent<S> {
  /// Returns the span of the whole exponent part.
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }

  /// Returns the span of the exponent char, e.g. `e` or `E`
  #[inline]
  pub const fn e(&self) -> &S {
    &self.e
  }

  /// Returns the span of the sign char, e.g. `+` or `-`
  #[inline]
  pub const fn sign(&self) -> Option<&Sign<S>> {
    self.sign.as_ref()
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &S {
    &self.digits
  }
}

/// Represents the fractional of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Fractional<S> {
  /// The span of the whole fractional part.
  pub span: S,
  /// The span of the dot character, e.g. `.`
  pub dot: S,
  /// The span of the digits part, e.g. `123`
  pub digits: S,
}

impl<S> Fractional<S> {
  /// Returns the span of the whole fractional part.
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }

  /// Returns the span of the dot character, e.g. `.`
  #[inline]
  pub const fn dot(&self) -> &S {
    &self.dot
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &S {
    &self.digits
  }
}

/// Represents a raw float value parsed from input.
///
/// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawFloatValue<S> {
  /// The span of the float value
  pub span: S,
  /// The integer section of the float value
  pub int: RawIntValue<S>,
  /// The fractional section of the float value
  pub fractional: Option<Fractional<S>>,
  /// The exponent section of the float value
  pub exponent: Option<Exponent<S>>,
}

/// Delimiters used by a GraphQL string literal.
///
/// GraphQL has two forms of string values:
/// - **StringValue**: delimited by a single double quote (`"`).
/// - **BlockStringValue**: delimited by triple double quotes (`"""`).
///
/// We store both the opening and closing delimiter as *spanned* slices so you
/// can report exact locations, preserve trivia, or re-emit the original text.
#[derive(Debug, Clone, Copy)]
pub enum StringDelimiter<S> {
  /// Triple-quoted **block string**: `"""`
  ///
  /// Spec: <https://spec.graphql.org/draft/#BlockStringValue>
  TripleQuote {
    /// The opening `"""`.
    l_triple_quote: TripleQuote<S>,
    /// The closing `"""`.
    r_triple_quote: TripleQuote<S>,
  },
  /// Single-quoted **string**: `"`
  ///
  /// Spec: <https://spec.graphql.org/draft/#StringValue>
  Quote {
    /// The opening `"`.
    l_quote: Quote<S>,
    /// The closing `"`.
    r_quote: Quote<S>,
  },
}

/// A parsed GraphQL string literal (block or inline), stored as spans.
///
/// This struct is intentionally **zero-copy**:
/// - `raw` covers the entire literal (including delimiters).
/// - `content` covers only the content between the delimiters (no quotes).
/// - `delimiters` records which delimiter form was used and their exact spans.
///
/// Use `content` when you need the raw source slice; do unescaping/indent
/// dedentation in a separate step to keep parsing allocation-free.
///
/// Spec: [String Value](<https://spec.graphql.org/draft/#sec-String-Value>)
#[derive(Debug, Clone, Copy)]
pub struct RawStringValue<S> {
  /// Entire literal, including opening and closing delimiters.
  ///
  /// Example:
  /// - `"hello"` → covers 0..7
  /// - `"""hi"""` → covers 10..18
  pub span: S,

  /// The delimiter form (single quote vs triple quote) and their spans.
  pub delimiters: StringDelimiter<S>,

  /// Content between the delimiters, with no surrounding quotes.
  ///
  /// Example:
  /// - `"hello"` → `hello`
  /// - `"""hi"""` → `hi`
  ///
  /// Note: For block strings this is the raw slice; spec-defined block string
  /// processing (common indentation removal, newline normalization) is not
  /// applied here.
  pub content: S,
}

/// Represents a boolean value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct RawBooleanValue<S> {
  /// The original span of the boolean value
  pub span: S,
  /// The value of the boolean
  pub value: bool,
}

impl<S> RawBooleanValue<S> {
  /// Creates a raw boolean value
  #[inline]
  pub const fn new(span: S, value: bool) -> Self {
    Self { span, value }
  }

  /// Returns the value
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }
}

/// Represents a null value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct RawNullValue<S> {
  /// The original span of the null value
  pub span: S,
}

impl<S> RawNullValue<S> {
  /// Creates a new raw null value
  #[inline]
  pub const fn new(span: S) -> Self {
    Self { span }
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }
}

/// Represents an enum value parsed from input
///
/// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawEnumValue<S> {
  /// The original span of the enum value
  pub raw: S,
}

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawListValue<S> {
  /// The original span of the list value
  pub span: S,
  /// The left `[` token.
  pub l_bracket: LBracket<S>,
  /// The right `]` token.
  pub r_bracket: RBracket<S>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  pub content: S,
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawObjectValue<S> {
  /// The original span of the object value
  pub span: S,
  /// The left `{` token.
  pub l_brace: LBrace<S>,
  /// The right `}` token.
  pub r_brace: RBrace<S>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  pub content: S,
}

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum InputValue<S> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(RawIntValue<S>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(RawFloatValue<S>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(RawBooleanValue<S>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(RawStringValue<S>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(RawNullValue<S>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(RawEnumValue<S>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(RawListValue<S>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(RawObjectValue<S>),
}

/// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
pub(super) fn boolean_value<'src, I, E>(
) -> impl Parser<'src, I, RawBooleanValue<I::Span>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
{
  just([I::Token::t, I::Token::r, I::Token::u, I::Token::e])
    .to(true)
    .or(
      just([
        I::Token::f,
        I::Token::a,
        I::Token::l,
        I::Token::s,
        I::Token::e,
      ])
      .to(false),
    )
    .map_with(|data, span| RawBooleanValue::new(span.span(), data))
}

pub(super) fn null_value<'src, I, E>() -> impl Parser<'src, I, RawNullValue<I::Span>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
{
  just([I::Token::n, I::Token::u, I::Token::l, I::Token::l])
    .or_not()
    .map_with(|_, span| RawNullValue::new(span.span()))
}

// pub(super) fn string_value<'a, E>() -> impl Parser<'a, &'a str, RawStringValue<StrSpan<'a>>, E> + Clone
// where
//   E: ParserExtra<'a, &'a str>,
// {
//   let inlined_string = text::;
// }

pub(super) fn int_value<'src, I, E>() -> impl Parser<'src, I, RawIntValue<I::Span>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
{
  let sign = just(I::Token::MINUS)
    .map_with(|_, span| span.span())
    .or_not();
  let val = text::int(10).map_with(|int_s, int_sp| (int_s, int_sp.span()));

  sign
    .then(val)
    .map_with(|(sign, (_, digits)), full_sp| RawIntValue {
      span: full_sp.span(),
      sign,
      digits,
    })
}

/// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
pub(super) fn float_value<'src, I, E>() -> impl Parser<'src, I, RawFloatValue<I::Span>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
{
  let exp = || {
    just(I::Token::e)
      .or(just(I::Token::E))
      .map_with(|val, span| Spanned::new(val, span.span()))
      .then(
        just(I::Token::PLUS)
          .map_with(|_, span| Sign::Positive(span.span()))
          .or(just(I::Token::MINUS).map_with(|_, span| Sign::Negative(span.span())))
          .or_not(),
      )
      .then(text::digits::<I, E>(10).map_with(|_, span| span.span()))
      .map_with(|((e, sign), digits), span| Exponent {
        span: span.span(),
        e: e.span,
        sign,
        digits,
      })
  };

  let frac = || {
    just(I::Token::DOT)
      .map_with(|_, span| span.span())
      .then(text::digits::<I, E>(10).map_with(|_, span| span.span()))
      .map_with(|(dot, digits), span| Fractional {
        span: span.span(),
        dot,
        digits,
      })
  };

  int_value()
    .then(frac().then(exp().or_not()))
    .map_with(|(int, (frac, exp)), span| RawFloatValue {
      span: span.span(),
      int,
      fractional: Some(frac),
      exponent: exp,
    })
    .or(
      int_value()
        .then(exp())
        .map_with(|(int, exp), span| RawFloatValue {
          span: span.span(),
          int,
          fractional: None,
          exponent: Some(exp),
        }),
    )
}

pub(super) fn input_value<'src, I, E>() -> impl Parser<'src, I, InputValue<I::Span>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: SmearChar + 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
{
  let boolean_value_parser = boolean_value::<I, E>().map(|v| InputValue::Boolean(v));
  let null_value_parser = null_value::<I, E>().map(|v| InputValue::Null(v));
  let int_value_parser = int_value::<I, E>().map(|v| InputValue::Int(v));
  let float_value_parser = float_value::<I, E>().map(|v| InputValue::Float(v));

  choice((
    boolean_value_parser,
    null_value_parser,
    int_value_parser,
    float_value_parser,
  ))
  .padded_by(super::ignored::ignored())
}

#[test]
fn test_boolean_value() {
  let input = "true";
  let result = boolean_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert!(result.value());

  let input = "false";
  let result = boolean_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert!(!result.value());
}

#[test]
fn test_null_value() {
  let input = "null";
  let result = null_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert_eq!(result.span().start, 0);
  assert_eq!(result.span().end, 4);

  let input = "";
  let result = null_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert_eq!(result.span().start, 0);
  assert_eq!(result.span().end, 0);
}

#[test]
fn test_int_value() {
  let input = "42";
  let result = int_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert!(result.sign.is_none());

  let input = "-42";
  let result = int_value::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap();
  assert!(result.sign.is_some());
}
