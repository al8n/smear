//! Focused GraphQLx number sub-lexer.
//!
//! [`NumberToken`] is a scoped Logos grammar that recognizes ONLY the numeric
//! literals (decimal / hex / binary / octal ints, plus decimal and hex floats)
//! and the bare `-` (`Minus`) operator — the exact slow paths the SIMD lexer's
//! number / `-` / `.`-digit arms refuse to hand-roll. It calls the **same frozen
//! number handlers** (`graphqlx/handlers/{str,slice}.rs`), so every number error
//! is byte-identical to the pre-SIMD lexer's without re-deriving any diagnostic.
//!
//! Unlike GraphQL, GraphQLx has NO valid-number SIMD fast path: every number
//! delegates here. The bare `-` operator delegates too — a `-` directly
//! followed by a digit or `.` is the sign of a negative literal (`-5` ->
//! `Decimal("-5")`, `-.5` -> a missing-integer-part float), while a `-` before
//! any other byte is the valid `Minus` operator. Logos resolves that ambiguity
//! by longest match, so the `#[token("-")] Minus` arm and the `-?`-prefixed
//! number regexes coexist here exactly as they do in the full grammar.
//!
//! The SIMD lexer routes a malformed/ambiguous number opener here via
//! `SyntacticLexer::delegate_number_to_logos` (instantiating
//! `simd::delegate_to_logos::<NumberToken<S::Slice>>`) and maps the
//! result through [`SyntacticToken::from`]. `NumberToken`'s `Error` is the SAME
//! `LexerErrors<Char, RecursionLimitExceeded>` `SyntacticToken` uses, so the
//! `Delegated::Error` arm needs zero conversion.
//!
//! Like the full grammar, the enum needs a raw-inner / `S`-converted-outer
//! split for each source flavor (`&str`, `HipStr`, `&[u8]`, `Bytes`, `HipByt`):
//! the inner `Token<'a>` is the Logos-derived enum whose slices are always the
//! primitive `&'a str`/`&'a [u8]`, and the outer `NumberToken<S>` carries the
//! source's own slice type, converted via `IntoEquivalent` where they differ.

use crate::graphqlx::{
  LitFloat, LitInt,
  syntactic::{SyntacticToken, SyntacticTokenKind},
};

/// A focused numeric token, parameterized over the slice type `S` exactly like
/// [`SyntacticToken<S>`]. It only ever holds a valid radix literal / float, or
/// the bare `Minus` operator; every numeric anomaly is reported as an error by
/// the reused handlers, never as a variant here.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum NumberToken<S> {
  /// Decimal float literal token.
  Float(S),
  /// Hexadecimal float literal token.
  HexFloat(S),
  /// Decimal integer literal token.
  Decimal(S),
  /// Binary integer literal token.
  Binary(S),
  /// Octal integer literal token.
  Octal(S),
  /// Hexadecimal integer literal token.
  Hex(S),
  /// Bare `-` operator (valid whenever a non-digit follows).
  Minus,
}

impl<S> NumberToken<S> {
  /// Returns the kind of the token.
  #[inline]
  const fn kind(&self) -> SyntacticTokenKind {
    match self {
      Self::Decimal(_) | Self::Hex(_) | Self::Binary(_) | Self::Octal(_) => SyntacticTokenKind::Int,
      Self::Float(_) | Self::HexFloat(_) => SyntacticTokenKind::Float,
      Self::Minus => SyntacticTokenKind::Minus,
    }
  }
}

impl<S> From<NumberToken<S>> for SyntacticToken<S> {
  #[inline(always)]
  fn from(value: NumberToken<S>) -> Self {
    match value {
      NumberToken::Float(s) => SyntacticToken::LitFloat(LitFloat::Decimal(s)),
      NumberToken::HexFloat(s) => SyntacticToken::LitFloat(LitFloat::Hex(s)),
      NumberToken::Decimal(s) => SyntacticToken::LitInt(LitInt::Decimal(s)),
      NumberToken::Binary(s) => SyntacticToken::LitInt(LitInt::Binary(s)),
      NumberToken::Octal(s) => SyntacticToken::LitInt(LitInt::Octal(s)),
      NumberToken::Hex(s) => SyntacticToken::LitInt(LitInt::Hex(s)),
      NumberToken::Minus => SyntacticToken::Minus,
    }
  }
}

/// Internal implementation macro: emits, for one source flavor, the
/// Logos-derived `Token` enum and its conversions to [`NumberToken`].
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
      use crate::graphqlx::{
        error::{
          BinaryError, DecimalError, FloatError, HexError, HexFloatError, LexerErrors, OctalError,
        },
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
      #[logos(subpattern digit = "[0-9]")]
      #[logos(subpattern hex_digit = "[0-9a-fA-F]")]
      #[logos(subpattern octal_digit = "[0-7]")]
      #[logos(subpattern binary_digit = "[01]")]
      #[logos(subpattern digits_with_sep = "[0-9_]*[0-9][0-9_]*")]
      #[logos(subpattern hex_digits_with_sep = "[0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*")]
      #[logos(subpattern decimal = "-?(?&digit)[0-9_]*")]
      #[logos(subpattern hex_start = "-?0x_*")]
      #[logos(subpattern hex = "(?&hex_start)(?&hex_digit)[0-9a-fA-F_]*")]
      #[logos(subpattern octal_start = "-?0o_*")]
      #[logos(subpattern octal = "(?&octal_start)(?&octal_digit)[0-7_]*")]
      #[logos(subpattern binary_start = "-?0b_*")]
      #[logos(subpattern binary = "(?&binary_start)(?&binary_digit)[01_]*")]
      #[logos(subpattern frac = "\\.(?&digits_with_sep)")]
      #[logos(subpattern esign = "[eE][+-]?")]
      #[logos(subpattern exp = "(?&esign)(?&digits_with_sep)")]
      #[logos(subpattern psign = "[pP][+-]?")]
      #[logos(subpattern hex_exp = "(?&psign)(?&hex_digits_with_sep)")]
      #[logos(subpattern hex_frac = "\\.(?&hex_digits_with_sep)")]
      pub enum Token<$logos_lt> {
        #[token("-")]
        Minus,

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("(?&decimal)\\._*", handlers::$handlers::handle_fractional_error)]
        #[regex("(?&decimal)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($logos_slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexFloatError::UnexpectedSuffix))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix(lexer))]
        #[regex(
          "-?(?&hex_frac)(?&hex_exp)",
          handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        #[regex("(?&hex)\\._*", handlers::$handlers::handle_hex_fractional_error)]
        #[regex("(?&hex)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        HexFloat($logos_slice),

        #[regex("(?&decimal)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        Decimal($logos_slice),

        #[regex("(?&binary)", |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix))]
        #[regex("(?&binary_start)", |lexer| handlers::$handlers::handle_invalid_binary_suffix(lexer))]
        Binary($logos_slice),

        #[regex("(?&octal)", |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix))]
        #[regex("(?&octal_start)", |lexer| handlers::$handlers::handle_invalid_octal_suffix(lexer))]
        Octal($logos_slice),

        #[regex("(?&hex)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix))]
        #[regex("(?&hex_start)", handlers::$handlers::handle_invalid_hex_suffix)]
        Hex($logos_slice),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for NumberToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Float($val) => Self::Float($convert),
            Token::HexFloat($val) => Self::HexFloat($convert),
            Token::Decimal($val) => Self::Decimal($convert),
            Token::Binary($val) => Self::Binary($convert),
            Token::Octal($val) => Self::Octal($convert),
            Token::Hex($val) => Self::Hex($convert),
            Token::Minus => Self::Minus,
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
      use crate::graphqlx::{
        error::{
          BinaryError, DecimalError, FloatError, HexError, HexFloatError, LexerErrors, OctalError,
        },
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
      #[logos(subpattern digit = "[0-9]")]
      #[logos(subpattern hex_digit = "[0-9a-fA-F]")]
      #[logos(subpattern octal_digit = "[0-7]")]
      #[logos(subpattern binary_digit = "[01]")]
      #[logos(subpattern digits_with_sep = "[0-9_]*[0-9][0-9_]*")]
      #[logos(subpattern hex_digits_with_sep = "[0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*")]
      #[logos(subpattern decimal = "-?(?&digit)[0-9_]*")]
      #[logos(subpattern hex_start = "-?0x_*")]
      #[logos(subpattern hex = "(?&hex_start)(?&hex_digit)[0-9a-fA-F_]*")]
      #[logos(subpattern octal_start = "-?0o_*")]
      #[logos(subpattern octal = "(?&octal_start)(?&octal_digit)[0-7_]*")]
      #[logos(subpattern binary_start = "-?0b_*")]
      #[logos(subpattern binary = "(?&binary_start)(?&binary_digit)[01_]*")]
      #[logos(subpattern frac = "\\.(?&digits_with_sep)")]
      #[logos(subpattern esign = "[eE][+-]?")]
      #[logos(subpattern exp = "(?&esign)(?&digits_with_sep)")]
      #[logos(subpattern psign = "[pP][+-]?")]
      #[logos(subpattern hex_exp = "(?&psign)(?&hex_digits_with_sep)")]
      #[logos(subpattern hex_frac = "\\.(?&hex_digits_with_sep)")]
      pub enum Token<$logos_lt> {
        #[token("-")]
        Minus,

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("(?&decimal)\\._*", handlers::$handlers::handle_fractional_error)]
        #[regex("(?&decimal)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($logos_slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexFloatError::UnexpectedSuffix))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix(lexer))]
        #[regex(
          "-?(?&hex_frac)(?&hex_exp)",
          handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        #[regex("(?&hex)\\._*", handlers::$handlers::handle_hex_fractional_error)]
        #[regex("(?&hex)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        HexFloat($logos_slice),

        #[regex("(?&decimal)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        Decimal($logos_slice),

        #[regex("(?&binary)", |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix))]
        #[regex("(?&binary_start)", |lexer| handlers::$handlers::handle_invalid_binary_suffix(lexer))]
        Binary($logos_slice),

        #[regex("(?&octal)", |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix))]
        #[regex("(?&octal_start)", |lexer| handlers::$handlers::handle_invalid_octal_suffix(lexer))]
        Octal($logos_slice),

        #[regex("(?&hex)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix))]
        #[regex("(?&hex_start)", handlers::$handlers::handle_invalid_hex_suffix)]
        Hex($logos_slice),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for NumberToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Float($val) => Self::Float($convert),
            Token::HexFloat($val) => Self::HexFloat($convert),
            Token::Decimal($val) => Self::Decimal($convert),
            Token::Binary($val) => Self::Binary($convert),
            Token::Octal($val) => Self::Octal($convert),
            Token::Hex($val) => Self::Hex($convert),
            Token::Minus => Self::Minus,
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
