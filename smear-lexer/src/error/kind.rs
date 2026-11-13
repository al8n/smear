use tokora::diagnostic::{Code, Severity};

/// What a draft §2 syntax error refused, in one vocabulary for the lexer and the parser alike.
///
/// Draft §2 defines both layers as "the document is not GraphQL": a client cannot act differently
/// on which layer refused, and which layer did is an implementation detail that a rewording or a
/// moved lexer/parser boundary would change. So both layers answer through this one enum and one
/// code family, `smear::syntax`, beside `smear::schema` (draft §3), `smear::validation` (draft §5)
/// and `smear::introspection`.
///
/// It lives here, in the lowest crate both layers share. The lexer's errors map onto it through
/// each dialect's `LexerErrorData::kind`, and the parser's through its `Error::kind`, which keeps
/// a lexical refusal's kind when the syntactic path carries it up.
///
/// Every severity is [`Severity::Error`]: a syntax error leaves no document to work with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum SyntaxErrorKind {
  // -- the lexer: a character no token begins with ------------------------------------------
  /// A character that can begin a token, in a place where it begins none: a `+`, or a `-` that no
  /// digit follows (GraphQL only — GraphQLx lexes both as operators).
  ///
  /// It is a lexer error's `UnexpectedLexeme`, which only the scanner raises and always over the
  /// one character it refused, so the kind and its help hold by construction. A slice a whole-slice
  /// door refuses for not being exactly one token of the kind it asked for is
  /// [`UnexpectedToken`](Self::UnexpectedToken) instead.
  UnexpectedCharacter,
  /// A character that begins no token at all.
  ///
  /// An ill-formed UTF-8 sequence inside a comment in a byte source arrives here too (smear#251),
  /// because the lexer reports it as the character it could not read rather than as a string
  /// literal's [`InvalidUtf8`](Self::InvalidUtf8).
  UnknownCharacter,
  /// A `.` that is not the three-dot spread operator, or a `..` in GraphQL. GraphQLx lexes `..` as
  /// the parent-module sigil of a path, so there only a lone `.` is one.
  UnterminatedSpread,

  // -- the lexer: numbers ---------------------------------------------------------------------
  /// An integer part with a non-significant leading zero, such as `007` (GraphQL only — GraphQLx
  /// admits leading zeros).
  LeadingZero,
  /// A number the input ends inside: a `.`, an exponent marker or a radix prefix with nothing
  /// after it, or a hexadecimal float with no exponent.
  IncompleteNumber,
  /// A number followed by a character that cannot follow one, such as the `x` of `1.5x`, a
  /// fraction or exponent with no digit where one must be, or a float with no integer part.
  InvalidNumber,

  // -- the lexer: strings ---------------------------------------------------------------------
  /// A string literal the input ends inside, or a single-line one whose line ends before its
  /// closing `"`.
  UnterminatedString,
  /// A line feed or carriage return inside a single-line string literal.
  ///
  /// The lexer never reports it — a single-line string ends at its first line terminator, as
  /// [`UnterminatedString`](Self::UnterminatedString) — so only a `StringError` a caller builds
  /// reads as it.
  LineTerminatorInString,
  /// A string literal that does not open with `"` or `"""`.
  ///
  /// The lexer never meets one — it only starts a string at a quote — so the one door that
  /// raises it is `LitStr::try_from`, which re-reads a spelling a caller hands it.
  UnopenedString,
  /// A backslash followed by a character draft §2.9.4 defines no escape for.
  UnknownEscape,
  /// A backslash that no character follows — in a single-line string, none on its line.
  IncompleteEscape,
  /// A `\u` escape that is not four hex digits, or a `\u{…}` escape that is unclosed, empty, longer
  /// than six digits or not hexadecimal.
  InvalidUnicodeEscape,
  /// A `\u` escape that names no Unicode scalar value: an unpaired surrogate, or a `\u{…}` naming a
  /// surrogate or a value past `10FFFF`.
  InvalidUnicodeScalar,
  /// A string literal in a byte source that is not well-formed UTF-8, or a source that is not
  /// UTF-8 at all.
  InvalidUtf8,

  // -- the parser -----------------------------------------------------------------------------
  /// A token the grammar does not allow where it stands.
  ///
  /// The lexer's whole-slice doors — `identifier`, `int_literal`, `float_literal`, and GraphQLx's
  /// `LitInt` and `LitFloat` `TryFrom<&str>` — report it too, as a lexer error's `NotOneToken`,
  /// when the slice is not exactly one token of the kind the door asked for.
  UnexpectedToken,
  /// A document that ends where the grammar needs more.
  UnexpectedEndOfInput,
  /// A GraphQLx path that starts with `::`, which only ever joins two segments (GraphQLx only).
  ///
  /// A path's root is written before its first `::` — `~` for the package, `^` for a dependency,
  /// `..` for the parent module — so a `::` where a path begins names no root at all.
  LeadingPathSeparator,
  /// A `(`, `[` or `{` — or, in GraphQLx, a `<` — that is never closed.
  UnclosedDelimiter,
  /// An integer literal that does not fit the width a materialising parse reads it at.
  ///
  /// It arrives with the parser's `materialized-numbers` feature: the slice-keeping parse reads no
  /// number's value and so never refuses one.
  IntOverflow,
  /// A float literal that does not convert to a finite double.
  ///
  /// It arrives with the parser's `materialized-numbers` feature, as
  /// [`IntOverflow`](Self::IntOverflow) does.
  FloatOverflow,

  // -- resource limits ------------------------------------------------------------------------
  /// The lossless parse's parser-frame budget refused to descend one level deeper.
  NestingLimit,
  /// The lossless parse's durable token budget refused an item.
  TokenBudget,
  /// A limit tripped and the scan stopped: the lexer's nesting or token ceiling, or the end of
  /// input that stands in for a terminal scanner stop.
  ///
  /// Not [`UnexpectedEndOfInput`](Self::UnexpectedEndOfInput): the document did not end, the
  /// parse stopped reading it, and no further input would change that.
  ResourceLimit,

  // -- anything else ----------------------------------------------------------------------------
  /// A refusal no kind above names: an error built from a free-form message.
  InvalidSyntax,
}

impl SyntaxErrorKind {
  /// Every kind a syntax error can report.
  pub const ALL: &'static [Self] = &[
    Self::UnexpectedCharacter,
    Self::UnknownCharacter,
    Self::UnterminatedSpread,
    Self::LeadingZero,
    Self::IncompleteNumber,
    Self::InvalidNumber,
    Self::UnterminatedString,
    Self::LineTerminatorInString,
    Self::UnopenedString,
    Self::UnknownEscape,
    Self::IncompleteEscape,
    Self::InvalidUnicodeEscape,
    Self::InvalidUnicodeScalar,
    Self::InvalidUtf8,
    Self::UnexpectedToken,
    Self::UnexpectedEndOfInput,
    Self::LeadingPathSeparator,
    Self::UnclosedDelimiter,
    Self::IntOverflow,
    Self::FloatOverflow,
    Self::NestingLimit,
    Self::TokenBudget,
    Self::ResourceLimit,
    Self::InvalidSyntax,
  ];

  /// Returns the stable identifier for this kind.
  ///
  /// It outlives every rewording of [`message`](Self::message), which is the whole reason a
  /// consumer keys off this rather than off the text.
  #[inline]
  pub const fn code(&self) -> Code {
    match self {
      Self::UnexpectedCharacter => Code::new("smear::syntax::unexpected-character"),
      Self::UnknownCharacter => Code::new("smear::syntax::unknown-character"),
      Self::UnterminatedSpread => Code::new("smear::syntax::unterminated-spread"),
      Self::LeadingZero => Code::new("smear::syntax::leading-zero"),
      Self::IncompleteNumber => Code::new("smear::syntax::incomplete-number"),
      Self::InvalidNumber => Code::new("smear::syntax::invalid-number"),
      Self::UnterminatedString => Code::new("smear::syntax::unterminated-string"),
      Self::LineTerminatorInString => Code::new("smear::syntax::line-terminator-in-string"),
      Self::UnopenedString => Code::new("smear::syntax::unopened-string"),
      Self::UnknownEscape => Code::new("smear::syntax::unknown-escape"),
      Self::IncompleteEscape => Code::new("smear::syntax::incomplete-escape"),
      Self::InvalidUnicodeEscape => Code::new("smear::syntax::invalid-unicode-escape"),
      Self::InvalidUnicodeScalar => Code::new("smear::syntax::invalid-unicode-scalar"),
      Self::InvalidUtf8 => Code::new("smear::syntax::invalid-utf8"),
      Self::UnexpectedToken => Code::new("smear::syntax::unexpected-token"),
      Self::UnexpectedEndOfInput => Code::new("smear::syntax::unexpected-end-of-input"),
      Self::LeadingPathSeparator => Code::new("smear::syntax::leading-path-separator"),
      Self::UnclosedDelimiter => Code::new("smear::syntax::unclosed-delimiter"),
      Self::IntOverflow => Code::new("smear::syntax::int-overflow"),
      Self::FloatOverflow => Code::new("smear::syntax::float-overflow"),
      Self::NestingLimit => Code::new("smear::syntax::nesting-limit"),
      Self::TokenBudget => Code::new("smear::syntax::token-budget"),
      Self::ResourceLimit => Code::new("smear::syntax::resource-limit"),
      Self::InvalidSyntax => Code::new("smear::syntax::invalid-syntax"),
    }
  }

  /// Returns the phrase this kind renders as, with nothing about the instance attached.
  ///
  /// Lowercase, with no trailing period and no position: the position travels in the diagnostic's
  /// primary location, never in its text.
  #[inline]
  pub const fn message(&self) -> &'static str {
    match self {
      Self::UnexpectedCharacter => "unexpected character",
      Self::UnknownCharacter => "unknown character",
      Self::UnterminatedSpread => "unterminated spread operator",
      Self::LeadingZero => "leading zero in a number",
      Self::IncompleteNumber => "incomplete number",
      Self::InvalidNumber => "invalid number",
      Self::UnterminatedString => "unterminated string",
      Self::LineTerminatorInString => "line terminator in a string",
      Self::UnopenedString => "a string value must open with `\"` or `\"\"\"`",
      Self::UnknownEscape => "unknown escape sequence",
      Self::IncompleteEscape => "incomplete escape sequence",
      Self::InvalidUnicodeEscape => "invalid Unicode escape",
      Self::InvalidUnicodeScalar => "escape is not a Unicode scalar value",
      Self::InvalidUtf8 => "invalid UTF-8",
      Self::UnexpectedToken => "unexpected token",
      Self::UnexpectedEndOfInput => "unexpected end of input",
      Self::LeadingPathSeparator => "a path cannot start with `::`",
      Self::UnclosedDelimiter => "unclosed delimiter",
      Self::IntOverflow => "integer out of range",
      Self::FloatOverflow => "float out of range",
      Self::NestingLimit => "nesting limit exceeded",
      Self::TokenBudget => "token budget exhausted",
      Self::ResourceLimit => "a resource limit stopped the parse",
      Self::InvalidSyntax => "invalid syntax",
    }
  }

  /// Returns what the document's author can do about it, where the kind has something actionable
  /// to say beyond [`message`](Self::message).
  ///
  /// `None` is a real answer, not a gap: where the fix depends on the instance, the rendered
  /// message says it and a fixed line here would only repeat the message.
  #[inline]
  pub const fn help(&self) -> Option<&'static str> {
    match self {
      Self::UnexpectedCharacter => Some(
        "a number may start with `-` but not `+`, and the sign must come directly before a digit",
      ),
      Self::UnterminatedSpread => Some("a spread is written with exactly three dots: `...`"),
      Self::LeadingZero => Some("an integer part may start with `0` only when it is exactly `0`"),
      Self::UnterminatedString => Some(
        "close the string with the delimiter that opened it; an inline string ends with its \
         line, so text that spans lines needs a block string (`\"\"\"`)",
      ),
      Self::LineTerminatorInString => Some(
        "use a block string (`\"\"\"`) for text that spans lines, or write the line break as `\\n`",
      ),
      Self::UnknownEscape => {
        Some("the escapes are `\\\"`, `\\\\`, `\\/`, `\\b`, `\\f`, `\\n`, `\\r`, `\\t` and `\\u`")
      }
      Self::IncompleteEscape => {
        Some("a `\\` must be followed by an escape character; write `\\\\` for a backslash")
      }
      Self::InvalidUnicodeEscape => {
        Some("write `\\u` with four hex digits, or `\\u{…}` with one to six")
      }
      Self::InvalidUnicodeScalar => Some(
        "a surrogate is valid only as a `\\u` high-low pair, and no code point exceeds `10FFFF`",
      ),
      Self::InvalidUtf8 => Some("encode the document's text as well-formed UTF-8"),
      Self::LeadingPathSeparator => Some("a dependency path starts with `^::`"),
      Self::UnclosedDelimiter => Some("add the matching closing delimiter"),
      Self::IntOverflow => Some("a larger integer needs a `Float`, a `String` or a custom scalar"),
      Self::FloatOverflow => Some("a `Float` is a finite IEEE 754 double"),
      Self::NestingLimit => Some("raise the nesting-depth ceiling, or flatten the document"),
      Self::TokenBudget => Some("raise the token ceiling, or shorten the document"),
      Self::ResourceLimit => {
        Some("raise the nesting-depth or token ceiling, or shorten the document")
      }
      Self::UnknownCharacter
      | Self::IncompleteNumber
      | Self::InvalidNumber
      | Self::UnopenedString
      | Self::UnexpectedToken
      | Self::UnexpectedEndOfInput
      | Self::InvalidSyntax => None,
    }
  }

  /// Returns how much this kind asks of its reader: always [`Severity::Error`].
  ///
  /// Spelled variant by variant rather than as a wildcard, so a kind added for a class that is not
  /// an error has to say so here.
  #[inline]
  pub const fn severity(&self) -> Severity {
    match self {
      Self::UnexpectedCharacter
      | Self::UnknownCharacter
      | Self::UnterminatedSpread
      | Self::LeadingZero
      | Self::IncompleteNumber
      | Self::InvalidNumber
      | Self::UnterminatedString
      | Self::LineTerminatorInString
      | Self::UnopenedString
      | Self::UnknownEscape
      | Self::IncompleteEscape
      | Self::InvalidUnicodeEscape
      | Self::InvalidUnicodeScalar
      | Self::InvalidUtf8
      | Self::UnexpectedToken
      | Self::UnexpectedEndOfInput
      | Self::LeadingPathSeparator
      | Self::UnclosedDelimiter
      | Self::IntOverflow
      | Self::FloatOverflow
      | Self::NestingLimit
      | Self::TokenBudget
      | Self::ResourceLimit
      | Self::InvalidSyntax => Severity::Error,
    }
  }
}

impl core::fmt::Display for SyntaxErrorKind {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.message())
  }
}

#[cfg(test)]
mod tests;
