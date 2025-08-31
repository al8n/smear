use super::{AsciiChar, LexerError, State, Text, Token};

/// Require a token to be a comment token
pub trait RequireComment<'a, I, S>: Token<'a, I, S> {
  /// The comment token
  type Comment: 'a;

  /// Requires the token to be a comment, returning a lexer error if it is not.
  fn require_comment(self) -> Result<Self::Comment, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be a white space
pub trait RequireWhiteSpace<'a, I, S>: Token<'a, I, S> {
  /// The white space token
  type WhiteSpace: 'a;

  /// Requires the token to be a whitespace, returning a lexer error if it is not.
  fn require_whitespace(self) -> Result<Self::WhiteSpace, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be white spaces
pub trait RequireWhiteSpaces<'a, I, S>: Token<'a, I, S> {
  /// The white spaces token
  type WhiteSpaces: 'a;

  /// Requires the token to be white spaces, returning a lexer error if it is not.
  fn require_whitespaces(self) -> Result<Self::WhiteSpaces, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be ignore token
pub trait RequireIgnore<'a, I, S>: Token<'a, I, S> {
  /// The ignore token
  type Ignore: 'a;

  /// Requires the token to be ignore, returning a lexer error if it is not.
  fn require_ignore(self) -> Result<Self::Ignore, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be ignore tokens
pub trait RequireIgnores<'a, I, S>: Token<'a, I, S> {
  /// The ignore tokens
  type Ignores: 'a;

  /// Requires the token to be ignore tokens, returning a lexer error if it is not.
  fn require_ignores(self) -> Result<Self::Ignores, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be line terminator
pub trait RequireLineTerminator<'a, I, S>: Token<'a, I, S> {
  /// The line terminator token
  type LineTerminator: 'a;

  /// Requires the token to be line terminator, returning a lexer error if it is not.
  fn require_line_terminator(self) -> Result<Self::LineTerminator, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be a boolean token
pub trait RequireBoolean<'a, I, S>: Token<'a, I, S> {
  /// The boolean token
  type Boolean: 'a;

  /// Requires the token to be a boolean token, returning a lexer error if it is not.
  fn require_boolean(self) -> Result<Self::Boolean, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be a null token
pub trait RequireNull<'a, I, S>: Token<'a, I, S> {
  /// The null token
  type Null: 'a;

  /// Requires the token to be `null`, returning a lexer error if it is not.
  fn require_null(self) -> Result<Self::Null, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be int literal token
pub trait RequireInt<'a, I, S>: Token<'a, I, S> {
  /// The int token
  type Int: 'a;

  /// Requires the token to be an int, returning a lexer error if it is not.
  fn require_int(self) -> Result<Self::Int, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be float literal token
pub trait RequireFloat<'a, I, S>: Token<'a, I, S> {
  /// The float token
  type Float: 'a;

  /// Requires the token to be a float, returning a lexer error if it is not.
  fn require_float(self) -> Result<Self::Float, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be inline string token
pub trait RequireInlineString<'a, I, S>: Token<'a, I, S> {
  /// The inline string token
  type InlineString: 'a;

  /// Requires the token to be an inline string, returning a lexer error if it is not.
  fn require_inline_string(self) -> Result<Self::InlineString, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be block string token
pub trait RequireBlockString<'a, I, S>: Token<'a, I, S> {
  /// The block string token
  type BlockString: 'a;

  /// Requires the token to be a block string, returning a lexer error if it is not.
  fn require_block_string(self) -> Result<Self::BlockString, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be string token
pub trait RequireString<'a, I, S>: Token<'a, I, S> {
  /// The string token
  type String: 'a;

  /// Requires the token to be a string, returning a lexer error if it is not.
  fn require_string(self) -> Result<Self::String, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be an ident token
pub trait RequireIdent<'a, I, S>: Token<'a, I, S> {
  /// The ident token
  type Ident: 'a;

  /// Requires the token to be an ident, returning a lexer error if it is not.
  fn require_ident(self) -> Result<Self::Ident, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be `keyword` token
pub trait RequireKeyword<'a, I, S>: Token<'a, I, S> {
  /// The keyword token
  type Keyword: 'a;

  /// Requires the token to be `keyword`, returning a lexer error if it is not.
  fn require_keyword(self, kw: &str) -> Result<Self::Keyword, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be an ASCII character
pub trait RequireAscii<'a, I, S>: Token<'a, I, S> {
  /// The ascii token
  type Ascii: 'a;

  /// Requires the token to be an ASCII character, returning a lexer error if it is not.
  fn require_ascii(self, ch: AsciiChar) -> Result<Self::Ascii, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}

/// Require a token to be a utf-8 character
pub trait RequireChar<'a, I, S>: Token<'a, I, S> {
  /// The char token
  type Char: 'a;

  /// Requires the token to be an ASCII character, returning a lexer error if it is not.
  fn require_char(self, ch: char) -> Result<Self::Char, LexerError<'a, I, Self, S>>
  where
    I: Text<'a>,
    S: State;
}
