//! Focused GraphQL number sub-lexer.
//!
//! [`NumberToken`] is a scoped Logos grammar that recognizes ONLY the numeric
//! literals (`Int` / `Float`) plus the bare `+`/`-` error tokens — the exact
//! slow paths the SIMD lexer's `scan_number` fast path refuses. It calls the
//! **same frozen number handlers** (`graphql/handlers/{str,slice}.rs`), so every
//! number error is byte-identical to the pre-SIMD lexer's without re-deriving
//! any diagnostic.
//!
//! The SIMD lexer delegates a malformed number to this grammar via
//! `SyntacticLexer::delegate_number_to_logos` (instantiating
//! `simd::delegate_to_logos::<NumberToken<S::Slice>>`) and maps the
//! result through [`SyntacticToken::from`]. `NumberToken`'s `Error` is the SAME
//! `LexerErrors<Char, RecursionLimitExceeded>` `SyntacticToken` uses, so the
//! `Delegated::Error` arm needs zero conversion.
//!
//! The enum needs a raw-inner / `S`-converted-outer split for each source
//! flavor (`&str`, `HipStr`, `&[u8]`, `Bytes`, `HipByt`): the inner `Token<'a>`
//! is the Logos-derived enum whose slices are always the primitive
//! `&'a str`/`&'a [u8]`, and the outer `NumberToken<S>` carries the source's own
//! slice type, converted via `IntoEquivalent` where they differ.

use crate::graphql::syntactic::SyntacticTokenKind;

use super::SyntacticToken;

/// A focused numeric token, parameterized over the slice type `S` exactly like
/// [`SyntacticToken<S>`]. It only ever holds a valid `Int`/`Float`; every
/// numeric anomaly is reported as an error by the reused handlers, never as a
/// variant here.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum NumberToken<S> {
  /// Float literal token.
  Float(S),
  /// Int literal token.
  Int(S),
}

impl<S> NumberToken<S> {
  /// Returns the kind of the token.
  #[inline]
  const fn kind(&self) -> SyntacticTokenKind {
    match self {
      Self::Int(_) => SyntacticTokenKind::Int,
      Self::Float(_) => SyntacticTokenKind::Float,
    }
  }
}

impl<S> From<NumberToken<S>> for SyntacticToken<S> {
  #[inline(always)]
  fn from(value: NumberToken<S>) -> Self {
    match value {
      NumberToken::Int(s) => SyntacticToken::LitInt(s),
      NumberToken::Float(s) => SyntacticToken::LitFloat(s),
    }
  }
}

/// Internal implementation macro for [`NumberToken`].
///
/// `$logos_lt` is the lifetime for the internal Logos `Token` enum (always
/// present). `$slice_lt` is empty (owned types) or the lifetime that binds the
/// outer slice to the Logos enum.
macro_rules! number_token_impl {
  // With slice lifetime (borrowed or borrowed-with-conversion).
  (
    $mod:ident [$logos_lt:lifetime] [$slice_lt:lifetime]
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokora::utils::IntoEquivalent;
      use tokora::{
        logos::Logos,
        state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::graphql::{
        error::{DecimalError, FloatError, LexerErrors},
        handlers,
        syntactic::{SyntacticTokenKind, number::NumberToken},
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
      pub enum Token<$logos_lt> {
        #[regex("-?0(?&digit)+((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
        #[regex("(?&int)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("-?0(?&digit)+(?&frac)(?&esign)", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("-?0(?&digit)+\\.", handlers::$handlers::handle_leading_zeros_and_fractional_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)\\.", handlers::$handlers::handle_fractional_error)]
        #[regex("-?0(?&digit)+(?&esign)", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($logos_slice),

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($logos_slice),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for NumberToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Float($val) => Self::Float($convert),
            Token::Int($val) => Self::Int($convert),
          }
        }
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokora::Token<'b> for NumberToken<$slice> {
        type Kind = SyntacticTokenKind;
        type Error = TokenErrors;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[inline(always)]
        fn is_trivia(&self) -> bool {
          false
        }
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokora::lexer::FromLogos<'b> for NumberToken<$slice> {
        type Logos = Token<$slice_lt>;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
  // Without slice lifetime (owned types like bytes::Bytes).
  (
    $mod:ident [$logos_lt:lifetime] []
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokora::utils::IntoEquivalent;
      use tokora::{
        logos::Logos,
        state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::graphql::{
        error::{DecimalError, FloatError, LexerErrors},
        handlers,
        syntactic::{SyntacticTokenKind, number::NumberToken},
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
      pub enum Token<$logos_lt> {
        #[regex("-?0(?&digit)+((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
        #[regex("(?&int)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("-?0(?&digit)+(?&frac)(?&esign)", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("-?0(?&digit)+\\.", handlers::$handlers::handle_leading_zeros_and_fractional_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)\\.", handlers::$handlers::handle_fractional_error)]
        #[regex("-?0(?&digit)+(?&esign)", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($logos_slice),

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($logos_slice),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for NumberToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Float($val) => Self::Float($convert),
            Token::Int($val) => Self::Int($convert),
          }
        }
      }

      impl tokora::Token<'_> for NumberToken<$slice> {
        type Kind = SyntacticTokenKind;
        type Error = TokenErrors;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[inline(always)]
        fn is_trivia(&self) -> bool {
          false
        }
      }

      impl<'b> tokora::lexer::FromLogos<'b> for NumberToken<$slice> {
        type Logos = Token<'b>;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
}

/// Dispatches each source flavor to [`number_token_impl!`] with the matching
/// raw-inner / converted-outer split.
macro_rules! number_token {
  // Borrowed slice: $slice uses lifetime $lt and IS the logos slice type (no conversion).
  ($mod:ident <$lt:lifetime>($slice:ty, $char:ty, $handlers:ident, $utf8:tt $(,)?)) => {
    number_token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $slice)
      {s => s}
    );
  };
  // Borrowed-with-conversion: $slice uses lifetime $lt but differs from the logos slice.
  ($mod:ident <$lt:lifetime>($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $logos_slice:ty $(,)?)) => {
    number_token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $logos_slice)
      {s => s.into_equivalent()}
    );
  };
  // Owned byte-slice: $slice is an owned type (no lifetime).
  ($mod:ident ($slice:ty, $char:ty, $handlers:ident, false $(,)?)) => {
    number_token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, false, &'s [u8])
      {s => s.into_equivalent()}
    );
  };
  // Owned str: $slice is an owned type (no lifetime).
  ($mod:ident ($slice:ty, $char:ty, $handlers:ident, true $(,)?)) => {
    number_token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, true, &'s str)
      {s => s.into_equivalent()}
    );
  };
}

number_token!(str_number<'a>(&'a str, char, str, true,));

#[cfg(feature = "hipstr")]
number_token!(hipstr_str_number<'a>(hipstr::HipStr<'a>, char, str, true, &'a str));

number_token!(slice_number<'a>(&'a [u8], u8, slice, false));

#[cfg(feature = "bytes")]
number_token!(bytes_number(bytes::Bytes, u8, slice, false));

#[cfg(feature = "hipstr")]
number_token!(hipstr_byt_number<'a>(hipstr::HipByt<'a>, u8, slice, false, &'a [u8]));
