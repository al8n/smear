use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  punct::{Quote, TripleQuote},
  SmearChar, Spanned,
};

/// Delimiters used by a GraphQL string literal.
///
/// GraphQL has two forms of string values:
/// - **String**: delimited by a single double quote (`"`).
/// - **BlockString**: delimited by triple double quotes (`"""`).
///
/// We store both the opening and closing delimiter as *spanned* slices so you
/// can report exact locations, preserve trivia, or re-emit the original text.
#[derive(Debug, Clone, Copy)]
pub enum StringDelimiter<Src, Span> {
  /// Triple-quoted **block string**: `"""`
  ///
  /// Spec: <https://spec.graphql.org/draft/#BlockString>
  TripleQuote {
    /// The opening `"""`.
    l_triple_quote: TripleQuote<Spanned<Src, Span>>,
    /// The closing `"""`.
    r_triple_quote: TripleQuote<Spanned<Src, Span>>,
  },
  /// Single-quoted **string**: `"`
  ///
  /// Spec: <https://spec.graphql.org/draft/#String>
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
pub struct String<Src, Span> {
  /// Entire literal, including opening and closing delimiters.
  ///
  /// Example:
  /// - `"hello"` → covers 0..7
  /// - `"""hi"""` → covers 10..18
  span: Spanned<Src, Span>,

  /// The delimiter form (single quote vs triple quote) and their spans.
  delimiters: StringDelimiter<Src, Span>,

  /// Content between the delimiters, with no surrounding quotes.
  ///
  /// Example:
  /// - `"hello"` → `hello`
  /// - `"""hi"""` → `hi`
  ///
  /// Note: For block strings this is the raw slice; spec-defined block string
  /// processing (common indentation removal, newline normalization) is not
  /// applied here.
  content: Spanned<Src, Span>,
}

impl<Src, Span> String<Src, Span> {
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
        .map_with(|((lq, content), rq), sp| String {
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
      .map_with(|((ltq, content), rtq), sp| String {
        span: Spanned::from(sp),
        delimiters: StringDelimiter::TripleQuote {
          l_triple_quote: ltq,
          r_triple_quote: rtq,
        },
        content,
      });

    // Choose block first or inline first — both are unambiguous.
    block_string
      .or(inline_string)
      .padded_by(super::ignored::padded())
  }
}
