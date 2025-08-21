use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::super::{
  super::{char::Char, convert::*, source::Source, spanned::Spanned},
  punct::{Quote, TripleQuote},
};

/// Represents the delimiter format used for a GraphQL string literal.
///
/// GraphQL supports two distinct string literal formats, each with different
/// delimiter styles and content rules. This enum captures which format was
/// used and provides access to the exact delimiter tokens for precise source
/// location tracking and re-emission.
///
/// ## String Format Types
///
/// ### Single-Quote Strings (`"..."`)
/// - **Purpose**: Standard string literals for most use cases
/// - **Delimiters**: Single double-quote characters (`"`)
/// - **Content rules**: Requires escape sequences for special characters
/// - **Line handling**: Must use `\n` escape for newlines
/// - **Use cases**: Field values, simple arguments, short text
///
/// ### Block Strings (`"""..."""`)
/// - **Purpose**: Multi-line strings with natural formatting
/// - **Delimiters**: Triple double-quote sequences (`"""`)
/// - **Content rules**: Most characters literal, minimal escaping needed
/// - **Line handling**: Natural newlines preserved with smart indentation
/// - **Use cases**: Documentation, descriptions, long text blocks
///
/// ## Examples
///
/// ```text
/// // Single-quote string
/// "Hello, world!"
/// "Line 1\nLine 2"
/// "Escaped \"quotes\" inside"
///
/// // Block string  
/// """
/// This is a multi-line
/// block string with
/// natural formatting
/// """
///
/// """Single line block string"""
/// ```
///
/// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
#[derive(Debug, Clone, Copy)]
pub enum StringDelimiter<Src, Span> {
  /// Triple-quoted block string delimiters: `"""`
  ///
  /// Block strings provide a more natural way to write multi-line text
  /// without requiring extensive escape sequences. They support:
  /// - Natural newlines without `\n` escaping
  /// - Common leading whitespace removal
  /// - Minimal character escaping requirements
  /// - Preservation of formatting and indentation
  ///
  /// Spec: [Block String](https://spec.graphql.org/draft/#BlockString)
  TripleQuote {
    /// The opening triple-quote delimiter (`"""`) with its source location.
    l_triple_quote: TripleQuote<Src, Span>,
    /// The closing triple-quote delimiter (`"""`) with its source location.
    r_triple_quote: TripleQuote<Src, Span>,
  },
  /// Single-quoted string delimiters: `"`
  ///
  /// Standard string literals that require escape sequences for special
  /// characters. These are the most common string format and follow
  /// JSON-like escaping conventions:
  /// - `\"` for literal quote characters
  /// - `\\` for literal backslashes  
  /// - `\n`, `\r`, `\t` for whitespace characters
  /// - `\uXXXX` for Unicode code points
  ///
  /// Spec: [String](https://spec.graphql.org/draft/#String)
  Quote {
    /// The opening quote delimiter (`"`) with its source location.
    l_quote: Quote<Src, Span>,
    /// The closing quote delimiter (`"`) with its source location.
    r_quote: Quote<Src, Span>,
  },
}

/// A GraphQL string literal content (the raw text between delimiters).
#[derive(Debug, Clone, Copy)]
pub struct StringContent<Src, Span>(Spanned<Src, Span>);

impl<Src, Span> StringContent<Src, Span> {
  /// Returns the underlying span of the string content.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.0
  }
}

impl<Src, Span> AsSpanned<Src, Span> for StringContent<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for StringContent<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.0
  }
}

impl<Src, Span> IntoComponents for StringContent<Src, Span> {
  type Components = Spanned<Src, Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0
  }
}

/// A GraphQL string literal value (single-quoted or block string).
///
/// Represents a complete string literal as defined by the GraphQL specification.
/// GraphQL supports two string formats: standard quoted strings for simple text
/// and triple-quoted block strings for multi-line content with natural formatting.
///
/// ## Design Philosophy
///
/// This parser is designed for **zero-copy parsing** efficiency:
/// - **No string allocation**: All content stored as source slices
/// - **Precise spans**: Every component retains exact source location
/// - **Deferred processing**: Escape sequence processing happens separately
/// - **Raw preservation**: Original source text fully preserved for re-emission
///
/// ## String Format Support
///
/// ### Standard Strings (`"..."`)
/// - JSON-compatible escape sequences
/// - Single-line content (newlines via `\n`)
/// - Compact representation for simple values
/// - Required escaping for quotes and backslashes
///
/// ### Block Strings (`"""..."""`)
/// - Multi-line content with natural line breaks
/// - Common indentation automatically removed
/// - Minimal escaping (only `\"""` for literal triple quotes)
/// - Ideal for documentation and formatted text
///
/// ## Component Structure
///
/// Each string literal contains:
/// - **Overall span**: Covers the entire literal including delimiters
/// - **Delimiters**: The opening and closing quote tokens with positions
/// - **Content span**: The raw text between delimiters (no processing applied)
///
/// ## Processing Stages
///
/// The parser handles lexical analysis only. Additional processing stages:
/// 1. **Lexical parsing** (this parser): Recognize delimiters and extract content
/// 2. **Escape processing**: Convert escape sequences to actual characters
/// 3. **Block string processing**: Remove common indentation, normalize newlines
/// 4. **Value conversion**: Convert to target language string representation
///
/// ## Examples
///
/// **Standard string literals:**
/// ```text
/// "hello"              // Simple text
/// "Hello, \"world\"!"  // Escaped quotes
/// "Line 1\nLine 2"     // Newline escape
/// "Unicode: \u0041"    // Unicode escape
/// ""                   // Empty string
/// ```
///
/// **Block string literals:**
/// ```text
/// """
/// This is a block string
/// with multiple lines
/// and natural formatting
/// """
///
/// """Single line block"""
///
/// """
///   Indented content
///     with nested indentation
///   is preserved relatively
/// """
/// ```
///
/// ## Escape Sequences (Standard Strings)
///
/// | Escape | Meaning | Unicode |
/// |--------|---------|---------|
/// | `\"` | Quote character | U+0022 |
/// | `\\` | Backslash | U+005C |
/// | `\/` | Forward slash | U+002F |
/// | `\b` | Backspace | U+0008 |
/// | `\f` | Form feed | U+000C |
/// | `\n` | Line feed | U+000A |
/// | `\r` | Carriage return | U+000D |
/// | `\t` | Tab | U+0009 |
/// | `\uXXXX` | Unicode code point | U+XXXX |
///
/// ## Block String Processing Rules
///
/// Block strings undergo automatic formatting:
/// 1. **Line termination**: Both `\r\n` and `\n` normalized to `\n`
/// 2. **Indentation removal**: Common leading whitespace stripped
/// 3. **Blank line handling**: Leading/trailing blank lines may be removed
/// 4. **Quote escaping**: Only `\"""` needs escaping (becomes `"""`)
///
/// ## Memory and Performance
///
/// - **Zero-copy**: Content stored as references to source input
/// - **Allocation-free parsing**: No strings allocated during lexical analysis
/// - **Streaming-friendly**: Can process large strings without loading entirely
/// - **Source preservation**: Original formatting preserved for tooling
///
/// ## Usage in GraphQL
///
/// String literals appear throughout GraphQL:
/// - **Field arguments**: `user(name: "John Doe")`
/// - **Variable values**: `{ "description": "A detailed explanation" }`
/// - **Default values**: `field(message: String = "Hello")`
/// - **Documentation**: `"""This field returns user information"""`
/// - **Descriptions**: Schema documentation and field descriptions
///
/// Spec: [String Value](https://spec.graphql.org/draft/#sec-String-Value)
#[derive(Debug, Clone, Copy)]
pub struct StringValue<Src, Span> {
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
  content: StringContent<Src, Span>,
}

impl<Src, Span> StringValue<Src, Span> {
  /// Returns the source span of the entire string literal.
  ///
  /// This span covers from the opening delimiter through the closing delimiter,
  /// including all content within. Useful for error reporting, source mapping,
  /// and extracting the complete literal text including quotes.
  ///
  /// # Examples
  ///
  /// ```text
  /// "hello"     // span covers all 7 characters
  /// """world""" // span covers all 9 characters
  /// ```
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the raw content span between the delimiters.
  ///
  /// This provides access to the string content without the surrounding
  /// quote delimiters. The content is raw and unprocessed - escape sequences
  /// are not converted and block string formatting rules are not applied.
  ///
  /// # Examples
  ///
  /// ```text
  /// "hello"           // content: "hello" (5 chars)
  /// "say \"hi\""      // content: "say \"hi\"" (raw escapes)
  /// """
  /// line 1
  /// line 2
  /// """               // content: "\nline 1\nline 2\n" (raw)
  /// ```
  #[inline]
  pub const fn content(&self) -> &StringContent<Src, Span> {
    &self.content
  }

  /// Returns the delimiter information for this string literal.
  ///
  /// This provides access to which delimiter format was used (single or triple
  /// quotes) and the exact source locations of both opening and closing
  /// delimiters. Useful for syntax highlighting, re-emission, and understanding
  /// the original format.
  #[inline]
  pub const fn delimiters(&self) -> &StringDelimiter<Src, Span> {
    &self.delimiters
  }

  /// Creates a parser for GraphQL string literals.
  ///
  /// This parser implements the complete GraphQL string literal specification,
  /// supporting both standard quoted strings and triple-quoted block strings.
  /// It handles all escape sequences, delimiter recognition, and content
  /// extraction while maintaining zero-copy efficiency.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    // let quote = Quote::<Src, Span>::parser();

    // // `"""` → TripleQuote<S>
    // let triple_quote = TripleQuote::<Src, Span>::parser();

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
      .ignore_then(hex_digit.repeated().exactly(4))
      .ignored();

    let esc_char = just(I::Token::BACKSLASH)
      .ignore_then(one_of([
        I::Token::QUOTATION,
        I::Token::BACKSLASH,
        I::Token::SLASH,
        I::Token::b,
        I::Token::f,
        I::Token::n,
        I::Token::r,
        I::Token::t,
      ]))
      .ignored();

    let unescaped_scalar = none_of([
      I::Token::QUOTATION,
      I::Token::BACKSLASH,
      I::Token::LINE_FEED,
      I::Token::CARRIAGE_RETURN,
    ])
    .ignored();

    let inline_content_parser = esc_unicode
      .or(esc_char)
      .or(unescaped_scalar)
      .repeated()
      .map_with(|_, sp| StringContent(Spanned::from(sp)));

    // Block content: consume either an escaped triple quote, or any single token
    // that is NOT the start of a raw closing `"""`.
    let escaped_triple = just(I::Token::BACKSLASH)
      .then(TripleQuote::parser())
      .ignored();

    let any_token = any().ignored();
    let not_closing_triple = TripleQuote::parser().not().ignore_then(any_token);

    let block_piece = escaped_triple.or(not_closing_triple);

    let block_content_parser = block_piece
      .repeated()
      .map_with(|_, sp| StringContent(Spanned::from(sp)));

    // " ... "
    let inline_string = Quote::parser()
      .then(inline_content_parser)
      .then(Quote::parser())
      .map_with(|((lq, content), rq), sp| Self {
        span: Spanned::from(sp),
        delimiters: StringDelimiter::Quote {
          l_quote: lq,
          r_quote: rq,
        },
        content,
      });

    // """ ... """
    let block_string = TripleQuote::parser()
      .then(block_content_parser)
      .then(TripleQuote::parser())
      .map_with(|((ltq, content), rtq), sp| Self {
        span: Spanned::from(sp),
        delimiters: StringDelimiter::TripleQuote {
          l_triple_quote: ltq,
          r_triple_quote: rtq,
        },
        content,
      });

    block_string.or(inline_string).labelled("string value")
  }
}

impl<Src, Span> AsSpanned<Src, Span> for StringValue<Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    self.span()
  }
}

impl<Src, Span> IntoSpanned<Src, Span> for StringValue<Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<Src, Span> IntoComponents for StringValue<Src, Span> {
  type Components = (
    Spanned<Src, Span>,
    StringDelimiter<Src, Span>,
    StringContent<Src, Span>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.delimiters, self.content)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use chumsky::{error::Simple, extra};

  type Err<'a> = extra::Err<Simple<'a, char>>;
  type Span = SimpleSpan;

  fn string_parser<'a>() -> impl Parser<'a, &'a str, StringValue<&'a str, Span>, Err<'a>> + Clone {
    StringValue::<&str, Span>::parser::<&str, Err>().then_ignore(end())
  }

  // ---------- Inline (") ----------

  #[test]
  fn inline_empty_and_basic() {
    let s = r#""""#;
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.span().source(), &s);
    assert_eq!(sv.content().span().source(), &"");

    let s = r#""hello""#;
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.content().span().source(), &"hello");
    match sv.delimiters() {
      StringDelimiter::Quote { l_quote, r_quote } => {
        assert_eq!(l_quote.span().source(), &"\"");
        assert_eq!(r_quote.span().source(), &"\"");
      }
      _ => panic!("expected quote delimiters"),
    }
  }

  #[test]
  fn inline_escapes_and_unicode() {
    // Escaped characters are accepted; content is raw (not unescaped here).
    let s = r#""line1\nline2\t\\\/\"""#;
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.content().span().source(), &r#"line1\nline2\t\\\/\""#);

    // Unicode escape: exactly four hex digits
    let s = r#""\u0041\u0062\u0030""#; // A b 0
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.content().span().source(), &r#"\u0041\u0062\u0030"#);
  }

  #[test]
  fn inline_invalid_newline_or_escape() {
    // Raw newline not allowed in inline string
    let s = "\"hello\nworld\"";
    assert!(string_parser().parse(s).into_result().is_err());

    // Bad unicode: non-hex or wrong length
    for s in [r#""\u004Z""#, r#""\u041""#, r#""\u0G10""#] {
      assert!(
        string_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }

    // Unknown short escape
    let s = r#""\q""#;
    assert!(string_parser().parse(s).into_result().is_err());

    // Unterminated / dangling backslash
    let s = r#""abc\"#;
    assert!(string_parser().parse(s).into_result().is_err());
  }

  #[test]
  fn inline_trailing_garbage_rejected() {
    for s in [r#""hi"x"#, r#""hi" "#] {
      assert!(
        string_parser().parse(s).into_result().is_err(),
        "should reject `{s}`"
      );
    }
  }

  // ---------- Block (""") ----------

  #[test]
  fn block_empty_and_basic() {
    let s = r#""""""""#; // """ """
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.span().source(), &s);
    assert_eq!(sv.content().span().source(), &"");

    let s = r#"""" """"#; // """ """
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.span().source(), &s);
    assert_eq!(sv.content().span().source(), &" ");

    let s = r#""""hello""""#;
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.content().span().source(), &"hello");
    match sv.delimiters() {
      StringDelimiter::TripleQuote {
        l_triple_quote,
        r_triple_quote,
      } => {
        assert_eq!(l_triple_quote.span().source(), &r#"""""#);
        assert_eq!(r_triple_quote.span().source(), &r#"""""#);
      }
      _ => panic!("expected triple-quote delimiters"),
    }
  }

  #[test]
  fn block_allows_newlines_and_quotes() {
    let s = "\"\"\"\nHe said \"Hi\".\nMultiline.\n\"\"\"";
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(
      sv.content().span().source(),
      &"\nHe said \"Hi\".\nMultiline.\n"
    );
  }

  #[test]
  fn block_escaped_triple_quote_inside() {
    // Content contains an escaped triple-quote sequence: \""
    let s = "\"\"\"x\\\"\"\"y\"\"\""; // runtime: """x\"""y"""
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.content().span().source(), &r#"x\"""y"#);
  }

  #[test]
  fn block_unterminated_or_trailing_garbage() {
    // Unterminated
    let s = "\"\"\"abc";
    assert!(string_parser().parse(s).into_result().is_err());

    // Trailing garbage after closing
    let s = "\"\"\"abc\"\"\"x";
    assert!(string_parser().parse(s).into_result().is_err());
  }

  // ---------- Spans & label ----------

  #[test]
  fn spans_cover_expected_regions() {
    let s = r#""abc""#;
    let sv = string_parser().parse(s).into_result().unwrap();
    assert_eq!(sv.span().source(), &s);
    assert_eq!(sv.content().span().source(), &"abc");
  }

  #[test]
  fn labelled_error_exists() {
    // Force an error by placing an illegal raw newline in inline string
    let errs = string_parser().parse("\"a\nb\"").into_result().unwrap_err();
    assert!(!errs.is_empty());
  }
}
