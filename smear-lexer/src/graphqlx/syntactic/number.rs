//! Focused GraphQLx number sub-lexer.
//!
//! [`NumberToken`] is the classification returned by this number sub-lexer. Its
//! two primitive Logos grammars recognize ONLY decimal / hexadecimal / binary /
//! octal integers, decimal and hexadecimal floats, and the bare `-` (`Minus`)
//! operator — the exact slow paths the SIMD lexer refuses to hand-roll. They
//! call the **same frozen number handlers** (`graphqlx/handlers/{str,slice}.rs`),
//! so every number error is byte-identical to the pre-SIMD lexer's without
//! re-deriving any diagnostic.
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

use crate::graphqlx::{
  LitFloat, LitInt,
  syntactic::{SyntacticToken, SyntacticTokenKind},
};

/// A classification-only focused numeric token. It only identifies a valid
/// radix literal / float or bare `Minus` operator; every numeric anomaly is
/// reported as an error by the reused handlers, never as a variant here.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum NumberToken {
  /// Decimal float literal token.
  Float,
  /// Hexadecimal float literal token.
  HexFloat,
  /// Decimal integer literal token.
  Decimal,
  /// Binary integer literal token.
  Binary,
  /// Octal integer literal token.
  Octal,
  /// Hexadecimal integer literal token.
  Hex,
  /// Bare `-` operator (valid whenever a non-digit follows).
  Minus,
}

impl NumberToken {
  /// Returns the kind of the token.
  #[inline]
  const fn kind(&self) -> SyntacticTokenKind {
    match self {
      Self::Decimal | Self::Hex | Self::Binary | Self::Octal => SyntacticTokenKind::Int,
      Self::Float | Self::HexFloat => SyntacticTokenKind::Float,
      Self::Minus => SyntacticTokenKind::Minus,
    }
  }

  /// Combines this delegated classification with its zero-copy outer-source slice.
  #[inline(always)]
  pub(crate) fn into_syntactic_token<Slice>(self, slice: Slice) -> SyntacticToken<Slice> {
    match self {
      Self::Float => SyntacticToken::LitFloat(LitFloat::Decimal(slice)),
      Self::HexFloat => SyntacticToken::LitFloat(LitFloat::Hex(slice)),
      Self::Decimal => SyntacticToken::LitInt(LitInt::Decimal(slice)),
      Self::Binary => SyntacticToken::LitInt(LitInt::Binary(slice)),
      Self::Octal => SyntacticToken::LitInt(LitInt::Octal(slice)),
      Self::Hex => SyntacticToken::LitInt(LitInt::Hex(slice)),
      Self::Minus => {
        let _ = slice;
        SyntacticToken::Minus
      }
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
        state::recursion_tracker::RecursionLimitExceeded,
      };
      use crate::graphqlx::{
        error::{
          BinaryError, DecimalError, FloatError, HexError, HexFloatError, LexerErrors, OctalError,
        },
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
        extras = crate::limits::SyntacticLimits,
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
      pub enum Token {
        #[token("-")]
        Minus,

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix).map(|_| ()))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          |lexer| handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix(lexer).map(|_| ())
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", |lexer| handlers::$handlers::handle_exponent_error(lexer).map(|_| ()))]
        #[regex("(?&decimal)\\._*", |lexer| handlers::$handlers::handle_fractional_error(lexer).map(|_| ()))]
        #[regex("(?&decimal)(?&esign)", |lexer| handlers::$handlers::handle_exponent_error(lexer).map(|_| ()))]
        Float,

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexFloatError::UnexpectedSuffix).map(|_| ()))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix(lexer).map(|_| ()))]
        #[regex(
          "-?(?&hex_frac)(?&hex_exp)",
          |lexer| handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix(lexer).map(|_| ())
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", |lexer| handlers::$handlers::handle_hex_exponent_error(lexer).map(|_| ()))]
        #[regex("(?&hex)\\._*", |lexer| handlers::$handlers::handle_hex_fractional_error(lexer).map(|_| ()))]
        #[regex("(?&hex)(?&psign)", |lexer| handlers::$handlers::handle_hex_exponent_error(lexer).map(|_| ()))]
        HexFloat,

        #[regex("(?&decimal)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix).map(|_| ()))]
        Decimal,

        #[regex("(?&binary)", |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix).map(|_| ()))]
        #[regex("(?&binary_start)", |lexer| handlers::$handlers::handle_invalid_binary_suffix(lexer).map(|_| ()))]
        Binary,

        #[regex("(?&octal)", |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix).map(|_| ()))]
        #[regex("(?&octal_start)", |lexer| handlers::$handlers::handle_invalid_octal_suffix(lexer).map(|_| ()))]
        Octal,

        #[regex("(?&hex)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix).map(|_| ()))]
        #[regex("(?&hex_start)", |lexer| handlers::$handlers::handle_invalid_hex_suffix(lexer).map(|_| ()))]
        Hex,
      }

      impl From<Token> for NumberLexerToken<$char> {
        #[inline(always)]
        fn from(value: Token) -> Self {
          match value {
            Token::Float => Self::new(NumberToken::Float),
            Token::HexFloat => Self::new(NumberToken::HexFloat),
            Token::Decimal => Self::new(NumberToken::Decimal),
            Token::Binary => Self::new(NumberToken::Binary),
            Token::Octal => Self::new(NumberToken::Octal),
            Token::Hex => Self::new(NumberToken::Hex),
            Token::Minus => Self::new(NumberToken::Minus),
          }
        }
      }

      impl tokora::Token<'_> for NumberLexerToken<$char> {
        type Kind = SyntacticTokenKind;
        type Error = TokenErrors;

        /// `Unbounded`, because `WithinSpan` is FALSIFIED for this vocabulary.
        ///
        /// The claim would be about the generated DFA, not about the callbacks, and `logos`
        /// backtracks to the last accepting prefix after probing past it. This grammar has a gap
        /// that backtrack falls into: over `-.5` truncated at `k = 2`, `-` is an accepting rule of
        /// its own, `-.` is accepted by nothing, and `-.5` is one float — so the prefix `-.`
        /// commits an item at `0..1` the complete parse does not have.
        /// `tokora::conformance::Harness::run_partial` reports it as `split k=2: a non-final
        /// prefix drain yielded 1 items but the complete parse has only 0 ending strictly before
        /// the cut`, in both dialects and at both doors.
        ///
        /// The exact frontier is not reachable either: a `logos` callback runs only at the leaf
        /// the DFA accepted, and that leaf is `-`, whose rule carries none — so `State::take_probe`
        /// has nothing to record. The price of `Unbounded` is paid by a `Partial` consumer alone,
        /// which buffers until the stream is sealed; every door this workspace ships is
        /// `Complete`, where there is no holdback at all.
        const SCAN_LOOKAHEAD: tokora::ScanLookahead = tokora::ScanLookahead::Unbounded;

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
