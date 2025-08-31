#![allow(clippy::type_complexity)]

pub use ascii::AsciiChar;
pub use require::*;

use super::{LexerError, Span, State, Text, Tokenizer};
use chumsky::{extra::ParserExtra, prelude::any, Parser};

mod require;

/// A macro to create a parser for a specific token type
#[macro_export]
macro_rules! require_token_parser {
  (
    $(
      $(#[$meta:meta])*
      $vis:vis fn $name:ident<$lt:lifetime, $t:ident, $e:ident>($($args:ident: $arg_ty:ty),*) -> <$leading:ident $(::$trait:ident)*>::$output:ident { $expr:expr }
    )*
  ) => {
    $(
      $(#[$meta])*
      $vis fn $name<$lt, $t, $e>($($args: $arg_ty),*) -> impl $crate::__private::chumsky::Parser<
        $lt,
        $t,
        $crate::__private::token::SpannedToken<
          <
            <$t as $crate::__private::chumsky::input::Input<$lt>>::Token as $leading $(::$trait)*<
              $lt,
              <$t as $crate::__private::Tokenizer<$lt>>::Text,
              <$t as $crate::__private::Tokenizer<$lt>>::State,
            >
          >::$output,
          <$t as $crate::__private::chumsky::input::Input<$lt>>::Span,
        >,
        $e,
      > + ::core::clone::Clone
      where
        $t: $crate::__private::Tokenizer<$lt>,
        $t::Token: $leading $(::$trait)* <
          $lt,
          <$t as $crate::__private::Tokenizer<$lt>>::Text,
          <$t as $crate::__private::Tokenizer<$lt>>::State,
        >,
        $e: $crate::__private::chumsky::extra::ParserExtra<$lt, $t>,
        <$e as $crate::__private::chumsky::extra::ParserExtra<$lt, $t>>::Error:
          ::core::convert::From<
            $crate::__private::lexer::LexerError<
              $lt,
              <$t as $crate::__private::Tokenizer<$lt>>::Text,
              <$t as $crate::__private::chumsky::input::Input<$lt>>::Token,
              <$t as $crate::__private::Tokenizer<$lt>>::State,
            >,
          >,
      {
        $expr
      }
    )*
  };
}

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

  /// Peeks a token from the input
  fn peek(input: I, state: &mut S) -> Option<Self>
  where
    I: Text<'a>,
    S: State;

  /// Returns the span of this token.
  fn span(&self) -> &Span<S>;

  /// Returns a mutable span of this token.
  fn span_mut(&mut self) -> &mut Span<S>;

  /// Creates a state error token.
  fn from_state_error(err: S::Error) -> Self where S: State;

  /// Check the token itself, if this is an error token, then return an error.
  fn check(self) -> Result<Self, Self::Error>
  where
    I: Text<'a>,
    S: State;
}

/// Returns a parser which skips a whitespace token
pub fn skip_whitespace<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireWhiteSpace<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  whitespace::<I, E>().ignored()
}

require_token_parser! {
  /// Returns a parser which parse a whitespace token
  pub fn whitespace<'src, I, E>() -> <RequireWhiteSpace>::WhiteSpace {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_whitespace()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse a whitespaces token
  pub fn whitespaces<'src, I, E>() -> <RequireWhiteSpaces>::WhiteSpaces {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_whitespaces()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse a line terminator token
  pub fn line_terminator<'src, I, E>() -> <RequireLineTerminator>::LineTerminator {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_line_terminator()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse a comment token
  pub fn comment<'src, I, E>() -> <RequireComment>::Comment {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_comment()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses many ignore tokens
  pub fn ignores<'src, I, E>() -> <RequireIgnores>::Ignores {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_ignores()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse one ignore token
  pub fn ignore<'src, I, E>() -> <RequireIgnore>::Ignore {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_ignore()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parse an int token
  pub fn int<'src, I, E>() -> <RequireInt>::Int {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_int()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a float token
  pub fn float<'src, I, E>() -> <RequireFloat>::Float {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_float()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a boolean token
  pub fn boolean<'src, I, E>() -> <RequireBoolean>::Boolean {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_boolean()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a null token
  pub fn null<'src, I, E>() -> <RequireNull>::Null {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_null()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses an inline string token
  pub fn inline_string<'src, I, E>() -> <RequireInlineString>::InlineString {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_inline_string()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a block string token
  pub fn block_string<'src, I, E>() -> <RequireBlockString>::BlockString {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_block_string()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a string token
  pub fn string<'src, I, E>() -> <RequireString>::String {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_string()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parsers an identifier token
  pub fn ident<'src, I, E>() -> <RequireIdent>::Ident {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_ident()
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses a keyword token
  pub fn keyword<'src, I, E>(kw: &'src str) -> <RequireKeyword>::Keyword {
    any().try_map(|tok: I::Token, sp| {
      tok
        .require_keyword(kw)
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parses an ASCII character token
  pub fn ascii<'src, I, E>(ch: AsciiChar) -> <RequireAscii>::Ascii {
    any().try_map(move |tok: I::Token, sp| {
      tok
        .require_ascii(ch)
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }

  /// Returns a parser which parsers a UTF-8 character token
  pub fn char<'src, I, E>(ch: char) -> <RequireChar>::Char {
    any().try_map(move |tok: I::Token, sp| {
      tok
        .require_char(ch)
        .map(|tok| SpannedToken::new(tok, sp))
        .map_err(Into::into)
    })
  }
}

/// Returns a parser which skips a whitespaces token
pub fn skip_whitespaces<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireWhiteSpaces<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  whitespaces::<I, E>().ignored()
}

/// Returns a parser which skips a line terminator token
pub fn skip_line_terminator<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireLineTerminator<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  line_terminator::<I, E>().ignored()
}

/// Returns a parser which skips a comment token
pub fn skip_comment<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireComment<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  comment::<I, E>().ignored()
}

/// Returns a parser which ignore many ignore tokens
pub fn ignore_many<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireIgnores<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  ignores::<I, E>().ignored()
}

/// Returns a parser which ignore one ignore token
pub fn ignore_one<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: Tokenizer<'src>,
  I::Token: RequireIgnore<'src, I::Text, I::State>,
  E: ParserExtra<'src, I>,
  E::Error: From<LexerError<'src, I::Text, I::Token, I::State>>,
{
  ignore::<I, E>().ignored()
}
