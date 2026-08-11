//! Focused GraphQL number sub-lexer.
//!
//! [`NumberToken`] is the classification returned by this number sub-lexer.
//! Its two primitive Logos grammars recognize ONLY numeric literals (`Int` /
//! `Float`) plus the bare `+`/`-` error tokens — the exact slow paths the SIMD
//! lexer's `scan_number` fast path refuses. They call the **same frozen number
//! handlers** (`graphql/handlers/{str,slice}.rs`), so every number error is
//! byte-identical to the pre-SIMD lexer's without re-deriving any diagnostic.
//!
//! The SIMD lexer delegates a malformed number to this grammar via
//! `SyntacticLexer::delegate_number_to_logos` (instantiating
//! `simd::delegate_to_logos::<NumberLexerToken<Char>>`) and maps the returned
//! classification with a slice from the original outer source. The adapter's
//! `Error` is the SAME `LexerErrors<Char, RecursionLimitExceeded>`
//! `SyntacticToken` uses, so the `Delegated::Error` arm needs zero conversion.
//!
//! The inner `Token` enums are unit-variant Logos grammars. Their `utf8`
//! setting selects a primitive `str` or `[u8]` source. Their callbacks retain
//! the frozen handlers but discard matched slices; `NumberLexerToken<Char>`
//! only adapts the classification to tokora's trait types, avoiding any
//! source-wrapper conversion.

use crate::lexer::graphql::syntactic::SyntacticTokenKind;

use super::SyntacticToken;

/// A classification-only focused numeric token. It only ever identifies a valid
/// `Int`/`Float`; every numeric anomaly is reported as an error by the reused
/// handlers, never as a variant here.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum NumberToken {
  /// Float literal token.
  Float,
  /// Int literal token.
  Int,
}

impl NumberToken {
  /// Returns the kind of the token.
  #[inline]
  const fn kind(&self) -> SyntacticTokenKind {
    match self {
      Self::Int => SyntacticTokenKind::Int,
      Self::Float => SyntacticTokenKind::Float,
    }
  }

  /// Combines this delegated classification with its zero-copy outer-source slice.
  #[inline(always)]
  pub(crate) fn into_syntactic_token<Slice>(self, slice: Slice) -> SyntacticToken<Slice> {
    match self {
      Self::Int => SyntacticToken::LitInt(slice),
      Self::Float => SyntacticToken::LitFloat(slice),
    }
  }
}

/// Tokora adapter for the primitive Logos number grammars.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct NumberLexerToken<Char> {
  number: NumberToken,
  _char: core::marker::PhantomData<fn() -> Char>,
}

impl<Char> NumberLexerToken<Char> {
  #[inline(always)]
  const fn new(number: NumberToken) -> Self {
    Self {
      number,
      _char: core::marker::PhantomData,
    }
  }

  #[inline(always)]
  pub(crate) const fn into_number_token(self) -> NumberToken {
    self.number
  }
}

/// Internal implementation macro for one primitive Logos number grammar.
macro_rules! number_token_impl {
  // One primitive `str` or `[u8]` Logos grammar.
  ($mod:ident ($char:ty, $handlers:ident, $utf8:tt)) => {
    mod $mod {
      use tokora::{
        logos::Logos,
        state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::lexer::graphql::{
        error::{DecimalError, FloatError, LexerErrors},
        handlers,
        syntactic::{
          SyntacticTokenKind,
          number::{NumberLexerToken, NumberToken},
        },
      };

      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;

      #[derive(Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
      #[logos(
        crate = tokora::logos,
        extras = RecursionLimiter,
        skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*?",
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::default_error)
      )]
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token {
        #[regex("-?0(?&digit)+((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix).map(|_| ()))]
        #[regex("(?&int)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix).map(|_| ()))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          |lexer| handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix(lexer).map(|_| ())
        )]
        #[regex("-?0(?&digit)+(?&frac)(?&esign)", |lexer| handlers::$handlers::handle_leading_zeros_and_exponent_error(lexer).map(|_| ()))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&frac)(?&esign)", |lexer| handlers::$handlers::handle_exponent_error(lexer).map(|_| ()))]
        #[regex("-?0(?&digit)+\\.", |lexer| handlers::$handlers::handle_leading_zeros_and_fractional_error(lexer).map(|_| ()))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)\\.", |lexer| handlers::$handlers::handle_fractional_error(lexer).map(|_| ()))]
        #[regex("-?0(?&digit)+(?&esign)", |lexer| handlers::$handlers::handle_leading_zeros_and_exponent_error(lexer).map(|_| ()))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&esign)", |lexer| handlers::$handlers::handle_exponent_error(lexer).map(|_| ()))]
        Float,

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix).map(|_| ()))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix).map(|_| ()))]
        #[token("-", |lexer| handlers::$handlers::unexpected_minus_token(lexer).map(|_| ()))]
        #[token("+", |lexer| handlers::$handlers::unexpected_plus_token(lexer).map(|_| ()))]
        Int,
      }

      impl From<Token> for NumberLexerToken<$char> {
        #[inline(always)]
        fn from(value: Token) -> Self {
          match value {
            Token::Float => Self::new(NumberToken::Float),
            Token::Int => Self::new(NumberToken::Int),
          }
        }
      }

      impl tokora::Token<'_> for NumberLexerToken<$char> {
        type Kind = SyntacticTokenKind;
        type Error = TokenErrors;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.number.kind()
        }

        #[inline(always)]
        fn is_trivia(&self) -> bool {
          false
        }
      }

      impl tokora::lexer::FromLogos<'_> for NumberLexerToken<$char> {
        type Logos = Token;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
}

number_token_impl!(str_number(char, str, true));
number_token_impl!(slice_number(u8, slice, false));
