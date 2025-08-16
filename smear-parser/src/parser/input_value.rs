use chumsky::{
  extra::ParserExtra,
  input::StrInput,
  label::LabelError,
  prelude::*,
  span::Span,
  text::{Char, TextExpected},
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBrace, LBracket, Quote, RBrace, RBracket, TripleQuote}, Name, SmearChar, Spanned
};

/// Represents a raw integer value parsed from input.
///
/// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawIntValue<Src, Span> {
  /// The original raw string representation of the integer value.
  pub span: Spanned<Src, Span>,
  /// Returns the sign of the value.
  pub sign: Option<Spanned<Src, Span>>,
  /// The integer part of the value.
  pub digits: Spanned<Src, Span>,
}

impl<Src, Span> RawIntValue<Src, Span> {
  /// Returns the span of the integer value.
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the sign
  pub const fn sign(&self) -> Option<&Spanned<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the span of the digits part.
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }

  /// Returns a parser for the integer value.
  ///
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value).
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let sign = just(I::Token::MINUS)
      .map_with(|_, span| Spanned::from(span))
      .or_not();
    let val = text::int(10).map_with(|_, sp| Spanned::from(sp));

    sign.then(val).map_with(|(sign, digits), sp| RawIntValue {
      span: Spanned::from(sp),
      sign,
      digits,
    })
    .padded_by(super::ignored::ignored())
  }
}

/// The sign of the float value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign<Src, Span> {
  /// The positive sign `+`
  Positive(Spanned<Src, Span>),
  /// The negative sign `-`
  Negative(Spanned<Src, Span>),
}

/// Represents the exponent part of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Exponent<Src, Span> {
  /// The span of the whole exponent part.
  pub span: Spanned<Src, Span>,
  /// The span of the exponent char, e.g. `e` or `E`
  pub e: Spanned<Src, Span>,
  /// The span of the sign char, e.g. `+` or `-`
  pub sign: Option<Sign<Src, Span>>,
  /// The span of the digits part, e.g. `123`
  pub digits: Spanned<Src, Span>,
}

impl<Src, Span> Exponent<Src, Span> {
  /// Returns the span of the whole exponent part.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the exponent char, e.g. `e` or `E`
  #[inline]
  pub const fn e(&self) -> &Spanned<Src, Span> {
    &self.e
  }

  /// Returns the span of the sign char, e.g. `+` or `-`
  #[inline]
  pub const fn sign(&self) -> Option<&Sign<Src, Span>> {
    self.sign.as_ref()
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }
}

/// Represents the fractional of a float value.
#[derive(Debug, Clone, Copy)]
pub struct Fractional<Src, Span> {
  /// The span of the whole fractional part.
  pub span: Spanned<Src, Span>,
  /// The span of the dot character, e.g. `.`
  pub dot: Spanned<Src, Span>,
  /// The span of the digits part, e.g. `123`
  pub digits: Spanned<Src, Span>,
}

impl<Src, Span> Fractional<Src, Span> {
  /// Returns the span of the whole fractional part.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the dot character, e.g. `.`
  #[inline]
  pub const fn dot(&self) -> &Spanned<Src, Span> {
    &self.dot
  }

  /// Returns the span of the digits part, e.g. `123`
  #[inline]
  pub const fn digits(&self) -> &Spanned<Src, Span> {
    &self.digits
  }
}

/// Represents a raw float value parsed from input.
///
/// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawFloatValue<Src, Span> {
  /// The span of the float value
  pub span: Spanned<Src, Span>,
  /// The integer section of the float value
  pub int: RawIntValue<Src, Span>,
  /// The fractional section of the float value
  pub fractional: Option<Fractional<Src, Span>>,
  /// The exponent section of the float value
  pub exponent: Option<Exponent<Src, Span>>,
}

impl<Src, Span> RawFloatValue<Src, Span> {
  /// Returns the span of the float value
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the integer section of the float value.
  pub const fn int(&self) -> &RawIntValue<Src, Span> {
    &self.int
  }

  /// Returns the fractional section of the float value.
  pub const fn fractional(&self) -> Option<&Fractional<Src, Span>> {
    self.fractional.as_ref()
  }

  /// Returns the exponent section of the float value.
  pub const fn exponent(&self) -> Option<&Exponent<Src, Span>> {
    self.exponent.as_ref()
  }

  /// Returns a parser of the float value
  ///
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let exp = || {
      just(I::Token::e)
        .or(just(I::Token::E))
        .map_with(|_, span| Spanned::from(span))
        .then(
          just(I::Token::PLUS)
            .map_with(|_, span| Sign::Positive(Spanned::from(span)))
            .or(just(I::Token::MINUS).map_with(|_, span| Sign::Negative(Spanned::from(span))))
            .or_not(),
        )
        .then(text::digits::<I, E>(10).map_with(|_, span| Spanned::from(span)))
        .map_with(|((e, sign), digits), span| Exponent {
          span: Spanned::from(span),
          e,
          sign,
          digits,
        })
    };

    let frac = || {
      just(I::Token::DOT)
        .map_with(|_, span| Spanned::from(span))
        .then(text::digits::<I, E>(10).map_with(|_, span| Spanned::from(span)))
        .map_with(|(dot, digits), span| Fractional {
          span: Spanned::from(span),
          dot,
          digits,
        })
    };

    RawIntValue::<Src, Span>::parser()
      .then(frac().then(exp().or_not()))
      .map_with(|(int, (frac, exp)), span| RawFloatValue {
        span: Spanned::from(span),
        int,
        fractional: Some(frac),
        exponent: exp,
      })
      .or(
        RawIntValue::<Src, Span>::parser()
          .then(exp())
          .map_with(|(int, exp), span| RawFloatValue {
            span: Spanned::from(span),
            int,
            fractional: None,
            exponent: Some(exp),
          }),
      )
      .padded_by(super::ignored::ignored())
  }
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
pub enum StringDelimiter<Src, Span> {
  /// Triple-quoted **block string**: `"""`
  ///
  /// Spec: <https://spec.graphql.org/draft/#BlockStringValue>
  TripleQuote {
    /// The opening `"""`.
    l_triple_quote: TripleQuote<Spanned<Src, Span>>,
    /// The closing `"""`.
    r_triple_quote: TripleQuote<Spanned<Src, Span>>,
  },
  /// Single-quoted **string**: `"`
  ///
  /// Spec: <https://spec.graphql.org/draft/#StringValue>
  Quote {
    /// The opening `"`.
    l_quote: Quote<Spanned<Src, Span>>,
    /// The closing `"`.
    r_quote: Quote<Spanned<Src, Span>>,
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
pub struct RawStringValue<Src, Span> {
  /// Entire literal, including opening and closing delimiters.
  ///
  /// Example:
  /// - `"hello"` → covers 0..7
  /// - `"""hi"""` → covers 10..18
  pub span: Spanned<Src, Span>,

  /// The delimiter form (single quote vs triple quote) and their spans.
  pub delimiters: StringDelimiter<Src, Span>,

  /// Content between the delimiters, with no surrounding quotes.
  ///
  /// Example:
  /// - `"hello"` → `hello`
  /// - `"""hi"""` → `hi`
  ///
  /// Note: For block strings this is the raw slice; spec-defined block string
  /// processing (common indentation removal, newline normalization) is not
  /// applied here.
  pub content: Spanned<Src, Span>,
}

impl<Src, Span> RawStringValue<Src, Span> {
  /// Returns the span of the string value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the content of the string value.
  #[inline]
  pub const fn content(&self) -> &Spanned<Src, Span> {
    &self.content
  }

  /// Returns the delimiters of the string value.
  #[inline]
  pub const fn delimiters(&self) -> &StringDelimiter<Src, Span> {
    &self.delimiters
  }

  /// Returns a parser for the string value.
  ///
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let quote = just(I::Token::QUOTATION).map_with(|_, sp| Quote::new(Spanned::from(sp)));

    // `"""` → TripleQuote<S>
    let triple_quote = just(I::Token::QUOTATION)
      .then(just(I::Token::QUOTATION))
      .then(just(I::Token::QUOTATION))
      .map_with(|_, sp| TripleQuote::new(Spanned::from(sp)));

    // \uXXXX (exactly 4 hex digits)
    let hex_digit = one_of([
      I::Token::ZERO,
      I::Token::ONE,
      I::Token::TWO,
      I::Token::THREE,
      I::Token::FOUR,
      I::Token::FIVE,
      I::Token::SIX,
      I::Token::SEVEN,
      I::Token::EIGHT,
      I::Token::NINE,
      I::Token::A,
      I::Token::B,
      I::Token::C,
      I::Token::D,
      I::Token::E,
      I::Token::F,
      I::Token::a,
      I::Token::b,
      I::Token::c,
      I::Token::d,
      I::Token::e,
      I::Token::f,
    ]);

    let esc_unicode = just(I::Token::BACKSLASH)
      .then(just(I::Token::u))
      .ignore_then(hex_digit.repeated().exactly(4));

    // \" \\ \/ \b \f \n \r \t
    let esc_char = just(I::Token::BACKSLASH)
      .ignore_then(one_of([
        I::Token::QUOTATION, // "
        I::Token::BACKSLASH, // \
        I::Token::SLASH,     // /
        I::Token::b,
        I::Token::f,
        I::Token::n,
        I::Token::r,
        I::Token::t,
      ]))
      .ignored();

    // Any char except " or \ or line terminator
    let unescaped_scalar = none_of([
      I::Token::QUOTATION,
      I::Token::BACKSLASH,
      I::Token::LINE_FEED,       // \n
      I::Token::CARRIAGE_RETURN, // \r
    ])
    .ignored();

    // Content of a normal "string"
    let inline_content_span = esc_unicode
      .or(esc_char)
      .or(unescaped_scalar)
      .repeated()
      .map_with(|_, sp| Spanned::from(sp));

    // Content of a block """string"""
    //
    // Spec allows any SourceCharacter except:
    //   - the sequence `"""`
    //   - the escaped triple quote `\"""` is permitted (because the backslash escapes the first ")
    //
    // We consume either:
    //  - an escaped triple quote `\"""` as 1 unit, or
    //  - any char that does not start `"""`
    //
    // To keep this simple and zero-copy, we implement:
    //    ( \"\"\" )  OR  ( any char not starting a raw `"""`
    //
    // Note: If your tokenization doesn’t have lookahead-friendly helpers,
    // this conservative version works: keep consuming until we *see* the
    // closing triple-quote sequence. That’s what `take_until_triple` below does.
    let escaped_triple = just(I::Token::BACKSLASH)
      .then(just(I::Token::QUOTATION))
      .then(just(I::Token::QUOTATION))
      .then(just(I::Token::QUOTATION))
      .ignored();

    // Or anything that does *not* start a raw `"""`
    // We cover three cases:
    //   - not a quote at all
    //   - a single quote followed by a non-quote
    //   - two quotes followed by a non-quote
    let not_triple_prefix = none_of([I::Token::QUOTATION])
      .ignored()
      .or(
        just(I::Token::QUOTATION)
          .then(none_of([I::Token::QUOTATION]))
          .ignored(),
      )
      .or(
        just(I::Token::QUOTATION)
          .then(just(I::Token::QUOTATION))
          .then(none_of([I::Token::QUOTATION]))
          .ignored(),
      );

    let block_piece = escaped_triple.or(not_triple_prefix);

    let block_content_span = block_piece.repeated().map_with(|_, sp| Spanned::from(sp));

    // " ... "
    let inline_string =
      quote
        .then(inline_content_span)
        .then(quote)
        .map_with(|((lq, content), rq), sp| RawStringValue {
          span: Spanned::from(sp),
          delimiters: StringDelimiter::Quote {
            l_quote: lq,
            r_quote: rq,
          },
          content,
        });

    // """ ... """
    let block_string = triple_quote
      .then(block_content_span)
      .then(triple_quote)
      .map_with(|((ltq, content), rtq), sp| RawStringValue {
        span: Spanned::from(sp),
        delimiters: StringDelimiter::TripleQuote {
          l_triple_quote: ltq,
          r_triple_quote: rtq,
        },
        content,
      });

    // Choose block first or inline first — both are unambiguous.
    block_string.or(inline_string)
      .padded_by(super::ignored::ignored())
  }
}

/// Represents a boolean value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct RawBooleanValue<Src, Span> {
  /// The original span of the boolean value
  pub span: Spanned<Src, Span>,
  /// The value of the boolean
  pub value: bool,
}

impl<Src, Span> RawBooleanValue<Src, Span> {
  /// Creates a raw boolean value
  #[inline]
  pub const fn new(span: Spanned<Src, Span>, value: bool) -> Self {
    Self { span, value }
  }

  /// Returns the value
  #[inline]
  pub const fn value(&self) -> bool {
    self.value
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a parser for the boolean value.
  ///
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
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
      .map_with(|data, span| RawBooleanValue::new(Spanned::from(span), data))
      .padded_by(super::ignored::ignored())
  }
}

/// Represents a null value parsed from input
#[derive(Debug, Clone, Copy)]
pub struct RawNullValue<Src, Span> {
  /// The original span of the null value
  pub span: Spanned<Src, Span>,
}

impl<Src, Span> RawNullValue<Src, Span> {
  /// Creates a new raw null value
  #[inline]
  pub const fn new(span: Spanned<Src, Span>) -> Self {
    Self { span }
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a parser of null value.
  ///
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    just([I::Token::n, I::Token::u, I::Token::l, I::Token::l])
      .map_with(|_, span| RawNullValue::new(Spanned::from(span)))
      .padded_by(super::ignored::ignored())
  }
}

/// Represents an enum value parsed from input
///
/// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawEnumValue<Src, Span> {
  /// The name of the enum value
  pub name: Name<Src, Span>,
}

impl<Src, Span> RawEnumValue<Src, Span> {
  /// Creates a new raw enum value
  #[inline]
  pub const fn new(name: Name<Src, Span>) -> Self {
    Self { name }
  }

  /// Returns the name
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Returns a parser for the enum value.
  ///
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    // Match exactly "true"/"false"/"null" when followed by a non-name char (or EOF)
    // so "trueValue"/"nullish" are NOT matched.
    choice((
      just([I::Token::t, I::Token::r, I::Token::u, I::Token::e]).not(),
      just([I::Token::f, I::Token::a, I::Token::l, I::Token::s, I::Token::e]).not(),
      just([I::Token::n, I::Token::u, I::Token::l, I::Token::l]).not(),
    ))
    .then(Name::<Src, Span>::parser())
    .map_with(|(_, name), _| RawEnumValue::new(name))
    .padded_by(super::ignored::ignored())
  }
}

/// Represents an list value parsed from input
///
/// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawListValue<Src, Span> {
  /// The original span of the list value
  pub span: Spanned<Src, Span>,
  /// The left `[` token.
  pub l_bracket: LBracket<Spanned<Src, Span>>,
  /// The right `]` token.
  pub r_bracket: RBracket<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  pub content: Spanned<Src, Span>,
}

impl<Src, Span> RawListValue<Src, Span> {
  /// Returns the span of the list value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the list value.
  #[inline]
  pub const fn l_bracket(&self) -> &LBracket<Spanned<Src, Span>> {
    &self.l_bracket
  }

  /// Returns the right bracket of the list value.
  #[inline]
  pub const fn r_bracket(&self) -> &RBracket<Spanned<Src, Span>> {
    &self.r_bracket
  }

  /// Returns the content of the list value.
  #[inline]
  pub const fn content(&self) -> &Spanned<Src, Span> {
    &self.content
  }
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
#[derive(Debug, Clone, Copy)]
pub struct RawObjectValue<Src, Span> {
  /// The original span of the object value
  pub span: Spanned<Src, Span>,
  /// The left `{` token.
  pub l_brace: LBrace<Spanned<Src, Span>>,
  /// The right `}` token.
  pub r_brace: RBrace<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the list entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  pub content: Spanned<Src, Span>,
}

impl<Src, Span> RawObjectValue<Src, Span> {
  /// Returns the span of the object value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left brace of the object value.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Spanned<Src, Span>> {
    &self.l_brace
  }

  /// Returns the right brace of the object value.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Spanned<Src, Span>> {
    &self.r_brace
  }

  /// Returns the content of the object value.
  #[inline]
  pub const fn content(&self) -> &Spanned<Src, Span> {
    &self.content
  }
}

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum RawInputValue<Src, Span> {
  /// Spec: [Int Value](https://spec.graphql.org/draft/#sec-Int-Value)
  ///
  /// Instead of giving a type of number, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Int(RawIntValue<Src, Span>),
  /// Spec: [Float Value](https://spec.graphql.org/draft/#sec-Float-Value)
  ///
  /// Instead of giving a type of float, keep the raw string representation, let the
  /// upper layers handle the conversion.
  Float(RawFloatValue<Src, Span>),
  /// Spec: [Boolean Value](https://spec.graphql.org/draft/#sec-Boolean-Value)
  Boolean(RawBooleanValue<Src, Span>),
  /// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
  String(RawStringValue<Src, Span>),
  /// Spec: [Null Value](https://spec.graphql.org/draft/#sec-Null-Value)
  Null(RawNullValue<Src, Span>),
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  Enum(RawEnumValue<Src, Span>),
  /// Spec: [List Value](https://spec.graphql.org/draft/#sec-List-Value)
  List(RawListValue<Src, Span>),
  /// Spec: [Input Object Value](https://spec.graphql.org/draft/#sec-Input-Object-Value)
  Object(RawObjectValue<Src, Span>),
}

impl<Src, Span> RawInputValue<Src, Span> {
  /// Returns the span of the input value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Int(value) => value.span(),
      Self::Float(value) => value.span(),
      Self::Boolean(value) => value.span(),
      Self::String(value) => value.span(),
      Self::Null(value) => value.span(),
      Self::Enum(value) => value.name().span(),
      Self::List(value) => value.span(),
      Self::Object(value) => value.span(),
    }
  }

  /// Returns a parser for the input value.
  ///
  /// Spec: [Input Value](https://spec.graphql.org/draft/#sec-Input-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    let boolean_value_parser = RawBooleanValue::parser::<I, E>().map(|v| RawInputValue::Boolean(v));
    let null_value_parser = RawNullValue::parser::<I, E>().map(|v| RawInputValue::Null(v));
    let int_value_parser = RawIntValue::parser::<I, E>().map(|v| RawInputValue::Int(v));
    let float_value_parser = RawFloatValue::parser::<I, E>().map(|v| RawInputValue::Float(v));
    let string_value_parser = RawStringValue::parser::<I, E>().map(|v| RawInputValue::String(v));
    let enum_value_parser = RawEnumValue::parser::<I, E>().map(|v| RawInputValue::Enum(v));

    choice((
      boolean_value_parser,
      null_value_parser,
      enum_value_parser,
      string_value_parser,
      float_value_parser,
      int_value_parser,
    ))
    .padded_by(super::ignored::ignored())
  }
}

#[test]
fn test_boolean_value() {
  let input = "true";
  let RawInputValue::Boolean(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert!(result.value());

  let input = "false";
  let RawInputValue::Boolean(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert!(!result.value());
}

#[test]
fn test_null_value() {
  let input = "null";
  let RawInputValue::Null(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert!(result.span().source().eq(&"null"));
}

#[test]
fn test_int_value() {
  let input = "42";
  let RawInputValue::Int(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert!(result.sign.is_none());
  println!("{}", result.span.source());

  let input = "-42";
  let RawInputValue::Int(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert!(result.sign.is_some());
}

#[test]
fn test_float_value() {
  let input = r"
4.123
-4.123
0.123
123e4
123E4
123e-4
123e+4
-1.123e4
-1.123E4
-1.123e-4
-1.123e+4
-1.123e4567  
";

  let expected = input.trim().lines().collect::<Vec<_>>();
  let parser = RawInputValue::parser::<&str, extra::Err<Simple<char>>>()
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()                                   // <- prevents `()`
    .then_ignore(super::ignored::ignored()); // eat trailing ws/comments

  let values = parser.parse(input).into_result().unwrap();
  assert_eq!(values.len(), 12);

  // All should be Float (given our inputs)
  for (v, exp) in values.iter().zip(expected.iter()) {
    match v {
      RawInputValue::Float(f) => {
        // sanity: float must have fractional and/or exponent
        assert!(f.fractional.is_some() || f.exponent.is_some());
        assert!(f.span.source().eq(exp));
      }
      other => panic!("expected Float, got {:?}", other),
    }
  }
}

#[test]
fn test_string_value() {
  let input = r###"
""
"simple"
" white space "
"unicode \u1234\u5678\u90AB\uCDEF"
"string with \"escaped\" characters"
"string with multiple languages котя, 猫, ねこ, قطة"
"""
block string with unusual whitespaces
a b  c
d

e	f
g  h
ijk﻿l‎‏m
"""
"###;

  let parser = RawInputValue::parser::<&str, super::Error>()
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .then_ignore(super::ignored::ignored());

  let values = parser.parse(input).into_result().unwrap();
  assert_eq!(values.len(), 7);

  // All should be String (given our inputs)
  for v in values {
    match v {
      RawInputValue::String(s) => {
        println!("{}", s.span.source());
        println!("{}", s.content.source());
      }
      other => panic!("expected String, got {:?}", other),
    }
  }
}

#[test]
fn test_enum_value() {
  let input = "SomeEnum";
  let RawInputValue::Enum(result) = RawInputValue::parser::<_, super::Error>()
    .parse(input)
    .into_result()
    .unwrap() else {
      panic!("unexpected value");
    };
  assert_eq!(*result.name().span().source(), "SomeEnum");
}
