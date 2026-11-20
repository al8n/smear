//! Focused GraphQLx number grammars: the syntactic lexer's number sub-lexer, and the sign's
//! grammar the lossless lexer reads a `-` with.
//!
//! [`NumberToken`] is the classification returned by the number sub-lexer. Its
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
//! number regexes coexist here.
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
//!
//! `number_token_impl!` writes these rules once and renders them a second way:
//! the `sign` grammars, over the same two alphabets, where each rule answers its
//! own tag and runs no handler, no hook and no state. The lossless grammar
//! matches a `-` alone and reads the lexeme from it with one of them, and its
//! callback then runs the matched rule's hook and handler on the lossless lexer.
//! So the lossless grammar's longest match at a `-`, its charge and its errors
//! are the ones these rules make.

use tokora::state::{RecursionLimitExceeded, State};

use crate::{
  graphqlx::{
    LitFloat, LitInt,
    syntactic::{SyntacticToken, SyntacticTokenKind},
  },
  limits::SyntacticLimits,
};

/// The number sub-lexer's state: the limits the syntactic lexer lends it, and whether the number
/// is inside a `const { … }` block.
///
/// The block is a flag, never the lexer's word: the lexer folds back only the limits, so nothing
/// the sub-lexer answers can write the count of open `<`. Inside a block an incomplete number's
/// refusal ends before a brace or an angle bracket, as the lossless grammar's does there.
///
/// `pub` because the grammar's logos token, which is `pub`, names it as its `Extras`. This module
/// is crate-private and nothing re-exports the type, so no caller can name it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct NumberState {
  limits: SyntacticLimits,
  in_block: bool,
}

impl NumberState {
  /// The state a delegated number starts from: the lexer's `limits`, inside a block when
  /// `in_block`.
  #[inline(always)]
  pub(crate) const fn new(limits: SyntacticLimits, in_block: bool) -> Self {
    Self { limits, in_block }
  }

  /// The limits, as the sub-lexer left them.
  #[inline(always)]
  pub(crate) const fn limits(&self) -> SyntacticLimits {
    self.limits
  }

  /// Whether the number is inside a `const { … }` block.
  #[inline(always)]
  pub(crate) const fn in_block(&self) -> bool {
    self.in_block
  }
}

impl State for NumberState {
  type Error = RecursionLimitExceeded;

  /// The limits' own check.
  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    self.limits.check()
  }
}

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

/// The number grammars' one macro: the subpatterns and every rule a number or a `-` can
/// be, written once, and three ways to render them.
///
/// - `number_token_impl!(name(Char, handlers, utf8))` is one syntactic instance: a primitive `str`
///   or `[u8]` grammar whose rules run the frozen handlers, for `SyntacticLexer`'s delegation.
/// - `number_token_impl!(sign name(utf8))` is one instance of the sign's grammar: the same rules
///   over the same alphabets, each answering its tag and running nothing, for the lossless
///   grammar's `-` rule.
/// - `number_token_impl!(rules)` is `sign::Rule`, one tag for each rule.
macro_rules! number_token_impl {
  ($mod:ident ($char:ty, $handlers:ident, $utf8:tt)) => {
    number_token_impl! { @rules [syntactic $mod ($char, $handlers, $utf8)] }
  };
  (sign $mod:ident ($utf8:tt)) => {
    number_token_impl! { @rules [sign $mod ($utf8)] }
  };
  (rules) => {
    number_token_impl! { @rules [rules] }
  };
  // Each rule: the variant of the syntactic grammar it belongs to, its kind and pattern, its tag
  // and, for every rule but the bare `-`, the frozen handler a syntactic instance runs, with the
  // arguments that follow the lexer.
  (@rules [$($instance:tt)*]) => {
    number_token_impl! {
      @emit $($instance)*
      [
        digit = "[0-9]",
        hex_digit = "[0-9a-fA-F]",
        octal_digit = "[0-7]",
        binary_digit = "[01]",
        digits_with_sep = "[0-9_]*[0-9][0-9_]*",
        hex_digits_with_sep = "[0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*",
        decimal = "-?(?&digit)[0-9_]*",
        hex_start = "-?0x_*",
        hex = "(?&hex_start)(?&hex_digit)[0-9a-fA-F_]*",
        octal_start = "-?0o_*",
        octal = "(?&octal_start)(?&octal_digit)[0-7_]*",
        binary_start = "-?0b_*",
        binary = "(?&binary_start)(?&binary_digit)[01_]*",
        frac = "\\.(?&digits_with_sep)",
        esign = "[eE][+-]?",
        exp = "(?&esign)(?&digits_with_sep)",
        psign = "[pP][+-]?",
        hex_exp = "(?&psign)(?&digits_with_sep)",
        hex_frac = "\\.(?&hex_digits_with_sep)",
        int_suffix = "_?[iu](0|[1-9][0-9]*)",
        float_suffix = "_?f(32|64)",
      ]
      {
        Minus {
          token "-" => Minus,
        },
        Float {
          regex "(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))"
            => Float(handle_decimal_suffix[FloatError::UnexpectedSuffix]),
          regex "-?(?&frac)(?&exp)?"
            => FloatMissingInteger(handle_float_missing_integer_part_error_then_check_suffix[]),
          regex "(?&decimal)(?&frac)(?&esign)"
            => FloatFractionEmptyExponent(handle_exponent_error[]),
          regex "(?&decimal)\\._*" => FloatEmptyFraction(handle_fractional_error[]),
          regex "(?&decimal)(?&esign)" => FloatEmptyExponent(handle_exponent_error[]),
          regex "(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))(?&float_suffix)"
            => FloatSuffixed(handle_float_suffix[FloatError::UnexpectedSuffix]),
          regex "(?&decimal)(?&float_suffix)"
            => IntegerFloatSuffixed(handle_float_suffix[DecimalError::UnexpectedSuffix]),
        },
        HexFloat {
          regex "(?&hex)(?&hex_frac)?(?&hex_exp)"
            => HexFloat(handle_valid_hex_float_suffix[HexFloatError::UnexpectedSuffix]),
          regex "(?&hex)(?&hex_frac)"
            => HexFloatMissingExponent(handle_hex_float_missing_exponent_then_check_suffix[]),
          regex "-?(?&hex_frac)(?&hex_exp)"
            => HexFloatMissingInteger(
              handle_hex_float_missing_integer_part_error_then_check_suffix[]
            ),
          regex "(?&hex)(?&hex_frac)(?&psign)"
            => HexFloatFractionEmptyExponent(handle_hex_exponent_error[]),
          regex "(?&hex)\\._*" => HexFloatEmptyFraction(handle_hex_fractional_error[]),
          regex "(?&hex)(?&psign)" => HexFloatEmptyExponent(handle_hex_exponent_error[]),
          regex "(?&hex)(?&hex_frac)?(?&hex_exp)(?&float_suffix)"
            => HexFloatSuffixed(handle_float_suffix[HexFloatError::UnexpectedSuffix]),
        },
        Decimal {
          regex "(?&decimal)" => Decimal(handle_decimal_suffix[DecimalError::UnexpectedSuffix]),
          regex "(?&decimal)(?&int_suffix)"
            => DecimalSuffixed(handle_int_suffix[DecimalError::UnexpectedSuffix]),
        },
        Binary {
          regex "(?&binary)" => Binary(handle_valid_binary_suffix[BinaryError::UnexpectedSuffix]),
          regex "(?&binary_start)" => BinaryMissingDigits(handle_invalid_binary_suffix[]),
          regex "(?&binary)(?&int_suffix)"
            => BinarySuffixed(handle_int_suffix[BinaryError::UnexpectedSuffix]),
        },
        Octal {
          regex "(?&octal)" => Octal(handle_valid_octal_suffix[OctalError::UnexpectedSuffix]),
          regex "(?&octal_start)" => OctalMissingDigits(handle_invalid_octal_suffix[]),
          regex "(?&octal)(?&int_suffix)"
            => OctalSuffixed(handle_int_suffix[OctalError::UnexpectedSuffix]),
        },
        Hex {
          regex "(?&hex)" => Hex(handle_valid_hex_suffix[HexError::UnexpectedSuffix]),
          regex "(?&hex_start)" => HexMissingDigits(handle_invalid_hex_suffix[]),
          regex "(?&hex)(?&int_suffix)" => HexSuffixed(handle_int_suffix[HexError::UnexpectedSuffix]),
        },
      }
    }
  };
  // One syntactic instance.
  (@emit syntactic $mod:ident ($char:ty, $handlers:ident, $utf8:tt)
    [$($sub:ident = $sub_pattern:literal),* $(,)?]
    {$(
      $variant:ident {$(
        $kind:ident $pattern:literal => $tag:ident $(($handler:ident [$($arg:tt)*]))?
      ),* $(,)?}
    ),* $(,)?}
  ) => {
    mod $mod {
      use tokora::{
        logos::Logos,
        state::RecursionLimitExceeded,
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
        extras = crate::graphqlx::syntactic::number::NumberState,
        skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*?",
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::default_error)
      )]
      $(#[logos(subpattern $sub = $sub_pattern)])*
      pub enum Token {
        $(
          $(
            #[$kind($pattern $(, |lexer| handlers::$handlers::$handler(lexer, $($arg)*).map(|_| ()))?)]
          )*
          $variant,
        )*
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
  // One instance of the sign's grammar.
  (@emit sign $mod:ident ($utf8:tt)
    [$($sub:ident = $sub_pattern:literal),* $(,)?]
    {$(
      $variant:ident {$(
        $kind:ident $pattern:literal => $tag:ident $(($handler:ident [$($arg:tt)*]))?
      ),* $(,)?}
    ),* $(,)?}
  ) => {
    /// The sign's grammar over one alphabet.
    pub(crate) mod $mod {
      use tokora::logos::Logos;

      use super::Rule;

      /// A lexeme a `-` begins, by the rule that matched it. No rule has a handler, a hook or
      /// a state, so a scan charges nothing and refuses nothing.
      #[derive(Logos, Clone, Copy, Debug, Eq, PartialEq)]
      #[logos(crate = tokora::logos, utf8 = $utf8)]
      $(#[logos(subpattern $sub = $sub_pattern)])*
      pub(crate) enum Token {
        $(
          $(#[$kind($pattern, |_lexer| Rule::$tag)])*
          $variant(Rule),
        )*
      }

      impl Token {
        /// The rule that matched.
        #[inline(always)]
        pub(crate) const fn rule(self) -> Rule {
          match self {
            $(Self::$variant(rule))|* => rule,
          }
        }
      }
    }
  };
  // The tags.
  (@emit rules
    [$($sub:ident = $sub_pattern:literal),* $(,)?]
    {$(
      $variant:ident {$(
        $kind:ident $pattern:literal => $tag:ident $(($handler:ident [$($arg:tt)*]))?
      ),* $(,)?}
    ),* $(,)?}
  ) => {
    /// The rule of the sign's grammar that matched at a `-`, one for each rule.
    ///
    /// The lossless grammar's `-` callback maps each to the hook and the handler of the rule's
    /// unsigned twin. A class is not enough: `-1.` and `-1.5` are both a float, and their rules
    /// run different handlers.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub(crate) enum Rule {
      $($(
        #[doc = concat!("The rule `", $pattern, "`.")]
        $tag,
      )*)*
    }
  };
}

number_token_impl!(str_number(char, str, true));
number_token_impl!(slice_number(u8, slice, false));

/// The sign's grammar: the lossless grammar reads a `-` with it.
///
/// The lossless grammar matches a `-` alone, and its callback lexes from the `-` with this grammar,
/// which holds every rule a `-` can begin: the bare `-` and every signed number rule, the
/// syntactic instances' own. So its longest match at a `-` is the one the full grammar would make.
/// It answers the rule's tag and its extent, and the callback then runs that rule's hook and
/// handler on the lossless lexer itself.
pub(crate) mod sign {
  number_token_impl!(rules);
  number_token_impl!(sign str(true));
  number_token_impl!(sign slice(false));
}
