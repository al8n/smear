use core::num::NonZeroUsize;

use chumsky::{primitive::any, Parser};

use crate::{lexer::Require, require_token_parser_fn};

use super::{Span, State, Text};

// use crate::lexer::Require;

/// Common kinds of token
pub mod kind;

/// A structureu wraps a token and a span
#[derive(Debug, Clone, Copy)]
pub struct SpannedToken<T, S> {
  token: T,
  span: S,
}

impl<T, S> SpannedToken<T, S> {
  /// Creates a new `SpannedToken`
  #[inline]
  pub const fn new(token: T, span: S) -> Self {
    Self { token, span }
  }

  /// Returns the span
  #[inline]
  pub const fn span(&self) -> &S {
    &self.span
  }

  /// Returns the token
  #[inline]
  pub const fn token(&self) -> &T {
    &self.token
  }

  /// Consumes and returns the span and token
  #[inline]
  pub fn into_components(self) -> (T, S) {
    (self.token, self.span)
  }
}

/// The token error
pub trait TokenError<S>: core::error::Error + Clone {
  /// Returns the corresponding span for this error.
  fn span(&self) -> Span<S>;
}

/// The token trait.
///
/// The implementors of `Token` trait should be error-contained, which means
/// unknown tokens and invalid tokens should be represented as errors within
/// the token itself. The check method of `Token` trait will be invoked in parsers to
/// validate the token and return the `Self::Error` if it is invalid.
pub trait Token<'a, I, S>: Sized + core::fmt::Debug + core::fmt::Display + 'a {
  /// The error type for this token.
  type Error: TokenError<S>;

  /// Lex one token that *begins exactly at* `pos`.
  ///
  /// Returns the number of consumed from the position and the token. If there is
  /// no more token, returns `None`, means the end of input has been reached.
  fn peek_at(input: &I, pos: usize, state: &mut S) -> Option<(NonZeroUsize, Self)>
  where
    I: Text<'a>,
    S: State;

  /// Returns the span of this token.
  fn span(&self) -> &Span<S>;

  /// Creates a state error token.
  fn with_state_error(self, err: S::Error) -> Self
  where
    S: State;

  /// Check the token itself, if this is an error token, then return an error.
  fn check(self) -> Result<Self, Self::Error>
  where
    I: Text<'a>,
    S: State;
}

require_token_parser_fn! {
  /// Returns a parser which parses a token and requires the parsed token to be of a specific specification.
  pub fn require_token<'a, I, E, Spec>(spec: Spec) -> Spec {
    any().try_map(move |tok: I::Token, sp| {
      tok.require(spec)
        .map(|o| SpannedToken::new(o, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse a whitespace token
  pub fn whitespace<'src, I, E>() -> kind::WhiteSpace {
    require_token(kind::WhiteSpace)
  }

  /// Returns a parser which parse a whitespaces token
  pub fn whitespaces<'src, I, E>() -> kind::WhiteSpaces {
    require_token(kind::WhiteSpaces)
  }

  /// Returns a parser which parse a line terminator token
  pub fn line_terminator<'src, I, E>() -> kind::LineTerminator {
    require_token(kind::LineTerminator)
  }

  /// Returns a parser which parse a comment token
  pub fn comment<'src, I, E>() -> kind::Comment {
    require_token(kind::Comment)
  }

  /// Returns a parser which parses many ignore tokens
  pub fn ignores<'src, I, E>() -> kind::Ignores {
    require_token(kind::Ignores)
  }

  /// Returns a parser which parse one ignore token
  pub fn ignore<'src, I, E>() -> kind::Ignore {
    require_token(kind::Ignore)
  }

  /// Returns a parser which parse an int token
  pub fn int<'src, I, E>() -> kind::IntLiteral {
    require_token(kind::IntLiteral)
  }

  /// Returns a parser which parses a float token
  pub fn float<'src, I, E>() -> kind::FloatLiteral {
    require_token(kind::FloatLiteral)
  }

  /// Returns a parser which parses a boolean token
  pub fn boolean<'src, I, E>() -> kind::BooleanLiteral {
    require_token(kind::BooleanLiteral)
  }

  /// Returns a parser which parses a null token
  pub fn null<'src, I, E>() -> kind::NullLiteral {
    require_token(kind::NullLiteral)
  }

  /// Returns a parser which parses an inline string token
  pub fn inline_string<'src, I, E>() -> kind::InlineStringLiteral {
    require_token(kind::InlineStringLiteral)
  }

  /// Returns a parser which parses a block string token
  pub fn block_string<'src, I, E>() -> kind::BlockStringLiteral {
    require_token(kind::BlockStringLiteral)
  }

  /// Returns a parser which parses a string token
  pub fn string<'src, I, E>() -> kind::StringLiteral {
    require_token(kind::StringLiteral)
  }

  /// Returns a parser which parsers an identifier token
  pub fn ident<'src, I, E>() -> kind::Ident {
    require_token(kind::Ident)
  }

  /// Returns a parser which parses a keyword token
  pub fn keyword<'src, I, E>(kw: &'src str) -> kind::Keyword<'src> {
    require_token(kind::Keyword(kw))
  }

  /// Returns a parser which parses an ASCII character token
  pub fn ascii<'src, I, E>(ch: kind::AsciiChar) -> kind::AsciiChar {
    require_token(ch)
  }

  /// Returns a parser which parsers a UTF-8 character token
  pub fn char<'src, I, E>(ch: char) -> char {
    require_token(ch)
  }
}

// /// Returns a parser which skips a whitespace token
// pub fn skip_whitespace<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireWhiteSpace<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   whitespace::<I, E>().ignored()
// }

// /// Returns a parser which skips a whitespaces token
// pub fn skip_whitespaces<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireWhiteSpaces<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   whitespaces::<I, E>().ignored()
// }

// /// Returns a parser which skips a line terminator token
// pub fn skip_line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireLineTerminator<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   line_terminator::<I, E>().ignored()
// }

// /// Returns a parser which skips a comment token
// pub fn skip_comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireComment<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   comment::<I, E>().ignored()
// }

// /// Returns a parser which ignore many ignore tokens
// pub fn ignore_many<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireIgnores<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   ignores::<I, E>().ignored()
// }

// /// Returns a parser which ignore one ignore token
// pub fn ignore_one<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
// where
//   I: Tokenizer<'src>,
//   I::Token: RequireIgnore<'src, I::Text, I::State>,
//   E: ParserExtra<'src, I>,
//   E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
// {
//   ignore::<I, E>().ignored()
// }
