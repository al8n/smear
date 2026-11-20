macro_rules! token {
  // Borrowed slice: $slice uses lifetime $lt and IS the logos slice type (no conversion)
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt, $comment:tt $(,)?)) => {
    $crate::graphqlx::lossless::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $comment, $slice)
      {s => s}
    );
  };
  // Borrowed-with-conversion: $slice uses lifetime $lt but differs from logos slice
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt, $comment:tt, $logos_slice:ty $(,)?)) => {
    $crate::graphqlx::lossless::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $comment, $logos_slice)
      {s => s.into_equivalent()}
    );
  };
  // Owned byte-slice
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, false, $comment:tt $(,)?)) => {
    $crate::graphqlx::lossless::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, false, $comment, &'s [u8])
      {s => s.into_equivalent()}
    );
  };
  // Owned str
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, true, $comment:tt $(,)?)) => {
    $crate::graphqlx::lossless::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, true, $comment, &'s str)
      {s => s.into_equivalent()}
    );
  };
}

macro_rules! token_impl {
  // With slice lifetime
  (
    $mod:ident [$logos_lt:lifetime] [$slice_lt:lifetime]
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $comment:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokora::utils::IntoEquivalent;
      use tokora::{
        logos::Logos, lexer::Lexable, state::LimitExceeded,
      };
      use crate::{
        error::StringErrors,
        graphqlx::lossless::LosslessState,
        graphqlx::lossless::token::{
          AngleCloser, CommaRole, Named, Signed, close_angle, close_brace, comma, open_angle,
        },
        graphqlx::{
          error::{LexerErrors, LexerError, DecimalError, HexError, FloatError, HexFloatError, BinaryError, OctalError},
          handlers::{
            self,
            tt_hook, tt_hook_and_then, tt_hook_map, tt_hook_and_then_into_errors, increase_recursion_depth_and_token,
            open_brace,
          },
          lossless::{LosslessToken, LosslessTokenKind},
          LitChar, LitInt, LitFloat, LitPrefixedStr,
        },
        handlers::{
          decrease_recursion_depth_and_increase_token,
          unterminated_spread_operator_error,
        },
        LitBlockStr, LitInlineStr, SealedWrapper,
      };

      type TokenError = LexerError<$char, LimitExceeded>;
      type TokenErrors = LexerErrors<$char, LimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      /// The `-` rule's callback: the operator, or the sign of the number the `-` begins.
      ///
      /// This grammar's number patterns take no sign, so it matches a `-` alone. The sign's
      /// grammar holds every rule a `-` can begin and reads the lexeme from the `-`. This lexer
      /// bumps to that lexeme's end, and then runs exactly what the matched rule's unsigned twin
      /// in this grammar runs. So the ceiling's check, the one charge, the handler's reading of
      /// what follows and any refusal are all this lexer's, at the lexeme's extent.
      #[allow(clippy::result_large_err)]
      fn minus<$logos_lt>(
        lexer: &mut tokora::logos::Lexer<$logos_lt, Token<$logos_lt>>,
      ) -> Result<Signed<$logos_slice>, TokenErrors> {
        use crate::graphqlx::syntactic::number::sign::{$handlers::Token as Sign, Rule};

        // Inside a block no number carries a sign: the `-` is the operator.
        if lexer.extras.in_const_block() {
          return one(tt_hook(lexer)).map(|()| Signed::Operator);
        }

        let mut sign = <Sign as Logos>::lexer(lexer.source());
        sign.bump(lexer.span().start);
        // `-` alone is a rule of the sign's grammar, so the scan matches at least the `-`.
        let (rule, end) = match sign.next() {
          Some(Ok(token)) => (token.rule(), sign.span().end),
          Some(Err(())) | None => (Rule::Minus, lexer.span().end),
        };
        lexer.bump(end.saturating_sub(lexer.span().end));

        match rule {
          Rule::Minus => one(tt_hook(lexer)).map(|()| Signed::Operator),
          Rule::Float => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::FloatMissingInteger => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix,
          )
          .map(Signed::Float),
          Rule::FloatFractionEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))
              .map(Signed::Float)
          }
          Rule::FloatEmptyFraction => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))
              .map(Signed::Float)
          }
          Rule::FloatEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))
              .map(Signed::Float)
          }
          Rule::HexFloat => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_hex_float_suffix(lexer, HexFloatError::UnexpectedSuffix)
          }))
          .map(Signed::HexFloat),
          Rule::HexFloatMissingExponent => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix,
          )
          .map(Signed::HexFloat),
          Rule::HexFloatMissingInteger => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix,
          )
          .map(Signed::HexFloat),
          Rule::HexFloatFractionEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))
              .map(Signed::HexFloat)
          }
          Rule::HexFloatEmptyFraction => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_fractional_error))
              .map(Signed::HexFloat)
          }
          Rule::HexFloatEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))
              .map(Signed::HexFloat)
          }
          Rule::Decimal => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Decimal),
          Rule::Binary => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix)
          }))
          .map(Signed::Binary),
          Rule::BinaryMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_binary_suffix)
              .map(Signed::Binary)
          }
          Rule::Octal => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix)
          }))
          .map(Signed::Octal),
          Rule::OctalMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_octal_suffix)
              .map(Signed::Octal)
          }
          Rule::Hex => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix)
          }))
          .map(Signed::Hex),
          Rule::HexMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_hex_suffix)
              .map(Signed::Hex)
          }
          Rule::FloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, FloatError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::IntegerFloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::HexFloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, HexFloatError::UnexpectedSuffix)
          }))
          .map(Signed::HexFloat),
          Rule::DecimalSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Decimal),
          Rule::BinarySuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, BinaryError::UnexpectedSuffix)
          }))
          .map(Signed::Binary),
          Rule::OctalSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, OctalError::UnexpectedSuffix)
          }))
          .map(Signed::Octal),
          Rule::HexSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, HexError::UnexpectedSuffix)
          }))
          .map(Signed::Hex),
        }
      }

      /// One lexeme's answer, with its error in the grammar's error type.
      #[allow(clippy::result_large_err)]
      #[inline(always)]
      fn one<O>(answer: Result<O, TokenError>) -> Result<O, TokenErrors> {
        answer.map_err(TokenErrors::from)
      }

      /// The name rule's callback: a name; or, inside a block, when a quote touches the name, the
      /// one lexeme the name and the literal make, read out of line.
      #[allow(clippy::result_large_err)]
      #[inline(always)]
      fn name<$logos_lt>(
        lexer: &mut tokora::logos::Lexer<$logos_lt, Token<$logos_lt>>,
      ) -> Result<Named<$logos_slice>, TokenErrors> {
        if lexer.extras.in_const_block()
          && matches!(AsRef::<[u8]>::as_ref(&lexer.remainder()).first(), Some(b'"' | b'\''))
        {
          return handlers::$handlers::prefixed(lexer).map(Named::Prefixed);
        }
        one(tt_hook_map(lexer, |lexer| Named::Identifier(lexer.slice())))
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokora::Token<'b> for LosslessToken<$slice> {
        type Kind = LosslessTokenKind;
        type Error = TokenErrors;

        /// This lexer surfaces every byte, including whitespace, commas and comments, so tokora's
        /// lossless `Sink` may wrap it. A trivia-skipping lexer cannot: a skipped-whitespace gap is
        /// indistinguishable from a dropped committed token.
        const SURFACES_TRIVIA: bool = true;

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
        /// the DFA accepted, and that leaf is `-`. Its callback reads the rest of the lexeme with
        /// the sign's grammar and records no probe, so `State::take_probe` has nothing to take:
        /// over `-.`, a cut of `-.5`, the sign's grammar answers a bare `-` the complete parse does
        /// not have. The price of `Unbounded` is paid by a `Partial` consumer alone, which buffers
        /// until the stream is sealed; every door this workspace ships is `Complete`, where there is
        /// no holdback at all.
        const SCAN_LOOKAHEAD: tokora::ScanLookahead = tokora::ScanLookahead::Unbounded;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[inline(always)]
        fn is_trivia(&self) -> bool {
          self.is_trivia()
        }
      }

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = tokora::logos,
        extras = LosslessState,
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::cst_default_error)
      )]
      #[logos(subpattern ident = "[a-zA-Z_][a-zA-Z0-9_]*")]
      #[logos(subpattern digit = "[0-9]")]
      #[logos(subpattern hex_digit = "[0-9a-fA-F]")]
      #[logos(subpattern octal_digit = "[0-7]")]
      #[logos(subpattern binary_digit = "[01]")]
      #[logos(subpattern digits_with_sep = "[0-9_]*[0-9][0-9_]*")]
      #[logos(subpattern hex_digits_with_sep = "[0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*")]
      #[logos(subpattern decimal = "(?&digit)[0-9_]*")]
      #[logos(subpattern hex_start = "0x_*")]
      #[logos(subpattern hex = "(?&hex_start)(?&hex_digit)[0-9a-fA-F_]*")]
      #[logos(subpattern octal_start = "0o_*")]
      #[logos(subpattern octal = "(?&octal_start)(?&octal_digit)[0-7_]*")]
      #[logos(subpattern binary_start = "0b_*")]
      #[logos(subpattern binary = "(?&binary_start)(?&binary_digit)[01_]*")]
      #[logos(subpattern frac = "\\.(?&digits_with_sep)")]
      #[logos(subpattern esign = "[eE][+-]?")]
      #[logos(subpattern exp = "(?&esign)(?&digits_with_sep)")]
      #[logos(subpattern psign = "[pP][+-]?")]
      #[logos(subpattern hex_exp = "(?&psign)(?&digits_with_sep)")]
      #[logos(subpattern hex_frac = "\\.(?&hex_digits_with_sep)")]
      #[logos(subpattern int_suffix = "_?[iu](0|[1-9][0-9]*)")]
      #[logos(subpattern float_suffix = "_?f(32|64)")]
      pub enum Token<$logos_lt> {
        #[token("*", tt_hook)]
        Asterisk,

        #[token("&", tt_hook)]
        Ampersand,

        #[token("@", tt_hook)]
        At,

        #[token(">", close_angle)]
        RAngle(AngleCloser),

        #[token("}", close_brace)]
        RBrace,

        #[token("]", decrease_recursion_depth_and_increase_token)]
        RBracket,

        #[token(")", decrease_recursion_depth_and_increase_token)]
        RParen,

        #[token(":", tt_hook)]
        Colon,

        #[token("$", tt_hook)]
        Dollar,

        #[token("=", tt_hook)]
        Equal,

        #[token("!", tt_hook)]
        Bang,

        #[token("<", open_angle)]
        LAngle,

        #[token("{", open_brace)]
        LBrace,

        #[token("[", increase_recursion_depth_and_token)]
        LBracket,

        #[token("(", increase_recursion_depth_and_token)]
        LParen,

        #[token("|", tt_hook)]
        Pipe,

        #[token("+", tt_hook)]
        Plus,

        #[token("-", minus)]
        Minus(Signed<$logos_slice>),

        #[token("...", tt_hook)]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[token("::", tt_hook)]
        PathSeparator,

        #[token("~", tt_hook)]
        Tilde,

        #[token("..", tt_hook)]
        DotDot,

        #[token("^", tt_hook)]
        Caret,

        #[token("/", tt_hook)]
        Slash,

        #[token("%", tt_hook)]
        Percent,

        #[token("<<", tt_hook)]
        Shl,

        #[token("'", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::char_literal))]
        LitChar(LitChar<$logos_slice>),

        #[token("=>", tt_hook)]
        FatArrow,

        #[token(",", comma)]
        Comma(CommaRole),

        #[token(" ", tt_hook)]
        Space,

        #[token("\t", tt_hook)]
        Tab,

        #[token("\n", tt_hook)]
        Newline,

        #[token("\r", tt_hook)]
        CarriageReturn,

        #[token("\r\n", tt_hook)]
        CarriageReturnAndNewline,

        #[token("\u{FEFF}", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Bom($logos_slice),

        #[regex($comment, |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::comment))]
        Comment($logos_slice),

        #[regex("(?&ident)", name)]
        Identifier(Named<$logos_slice>),

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex(
          "(?&frac)(?&exp)?",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("(?&decimal)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))]
        #[regex("(?&decimal)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex("(?&decimal)(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        Float($logos_slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_float_suffix(lexer, HexFloatError::UnexpectedSuffix)))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix))]
        #[regex(
          "(?&hex_frac)(?&hex_exp)",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        #[regex("(?&hex)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_fractional_error))]
        #[regex("(?&hex)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, HexFloatError::UnexpectedSuffix)))]
        HexFloat($logos_slice),

        #[regex("(?&decimal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        #[regex("(?&decimal)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        Decimal($logos_slice),

        #[regex("(?&binary)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix)))]
        #[regex("(?&binary_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_binary_suffix))]
        #[regex("(?&binary)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, BinaryError::UnexpectedSuffix)))]
        Binary($logos_slice),

        #[regex("(?&octal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix)))]
        #[regex("(?&octal_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_octal_suffix))]
        #[regex("(?&octal)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, OctalError::UnexpectedSuffix)))]
        Octal($logos_slice),

        #[regex("(?&hex)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix)))]
        #[regex("(?&hex_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_hex_suffix))]
        #[regex("(?&hex)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, HexError::UnexpectedSuffix)))]
        Hex($logos_slice),

        #[token("\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokora::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),

        #[token("\"\"\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokora::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitBlockStr(LitBlockStr<$logos_slice>),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for LosslessToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
            Token::Asterisk => Self::Asterisk,
            Token::RAngle(AngleCloser::Closer) => Self::RAngle,
            Token::RAngle(AngleCloser::Shr) => Self::Shr,
            Token::RBrace => Self::RBrace,
            Token::RBracket => Self::RBracket,
            Token::RParen => Self::RParen,
            Token::Colon => Self::Colon,
            Token::Dollar => Self::Dollar,
            Token::Equal => Self::Equal,
            Token::FatArrow => Self::FatArrow,
            Token::Bang => Self::Bang,
            Token::LAngle => Self::LAngle,
            Token::LBrace => Self::LBrace,
            Token::LBracket => Self::LBracket,
            Token::LParen => Self::LParen,
            Token::Pipe => Self::Pipe,
            Token::Spread => Self::Spread,
            Token::Float($val) => Self::LitFloat(LitFloat::Decimal($convert)),
            Token::HexFloat($val) => Self::LitFloat(LitFloat::Hex($convert)),
            Token::Identifier(Named::Identifier($val)) => Self::Identifier($convert),
            Token::Identifier(Named::Prefixed(lit)) => {
              let (prefix, block) = (lit.prefix(), lit.is_block());
              let $val = *lit.source_ref();
              Self::LitPrefixedStr(LitPrefixedStr::new(prefix, block, $convert))
            }
            Token::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
            Token::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            Token::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
            Token::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Plus => Self::Plus,
            Token::Minus(signed) => match signed {
              Signed::Operator => Self::Minus,
              Signed::Float($val) => Self::LitFloat(LitFloat::Decimal($convert)),
              Signed::HexFloat($val) => Self::LitFloat(LitFloat::Hex($convert)),
              Signed::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
              Signed::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
              Signed::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
              Signed::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            },
            Token::PathSeparator => Self::PathSeparator,
            Token::Tilde => Self::Tilde,
            Token::DotDot => Self::DotDot,
            Token::Caret => Self::Caret,
            Token::Slash => Self::Slash,
            Token::Percent => Self::Percent,
            Token::Shl => Self::Shl,
            Token::LitChar(lit) => {
              let value = lit.value();
              let $val = *lit.source_ref();
              Self::LitChar(LitChar::new($convert, value))
            }
            Token::Comma(CommaRole::Trivia) => Self::Comma,
            Token::Comma(CommaRole::Separator) => Self::Separator,
            Token::Space => Self::Space,
            Token::Tab => Self::Tab,
            Token::Newline => Self::Newline,
            Token::CarriageReturn => Self::CarriageReturn,
            Token::CarriageReturnAndNewline => Self::CarriageReturnAndNewline,
            Token::Bom($val) => Self::Bom($convert),
            Token::Comment($val) => Self::Comment($convert),
          }
        }
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokora::lexer::FromLogos<'b> for LosslessToken<$slice> {
        type Logos = Token<$slice_lt>;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
  // Without slice lifetime (owned types)
  (
    $mod:ident [$logos_lt:lifetime] []
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $comment:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokora::utils::IntoEquivalent;
      use tokora::{
        logos::Logos, lexer::Lexable, state::LimitExceeded,
      };
      use crate::{
        error::StringErrors,
        graphqlx::lossless::LosslessState,
        graphqlx::lossless::token::{
          AngleCloser, CommaRole, Named, Signed, close_angle, close_brace, comma, open_angle,
        },
        graphqlx::{
          error::{LexerErrors, LexerError, DecimalError, HexError, FloatError, HexFloatError, BinaryError, OctalError},
          handlers::{
            self,
            tt_hook, tt_hook_and_then, tt_hook_map, tt_hook_and_then_into_errors, increase_recursion_depth_and_token,
            open_brace,
          },
          lossless::{LosslessToken, LosslessTokenKind},
          LitChar, LitInt, LitFloat, LitPrefixedStr,
        },
        handlers::{
          decrease_recursion_depth_and_increase_token,
          unterminated_spread_operator_error,
        },
        LitBlockStr, LitInlineStr, SealedWrapper,
      };

      type TokenError = LexerError<$char, LimitExceeded>;
      type TokenErrors = LexerErrors<$char, LimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      /// The `-` rule's callback: the operator, or the sign of the number the `-` begins.
      ///
      /// This grammar's number patterns take no sign, so it matches a `-` alone. The sign's
      /// grammar holds every rule a `-` can begin and reads the lexeme from the `-`. This lexer
      /// bumps to that lexeme's end, and then runs exactly what the matched rule's unsigned twin
      /// in this grammar runs. So the ceiling's check, the one charge, the handler's reading of
      /// what follows and any refusal are all this lexer's, at the lexeme's extent.
      #[allow(clippy::result_large_err)]
      fn minus<$logos_lt>(
        lexer: &mut tokora::logos::Lexer<$logos_lt, Token<$logos_lt>>,
      ) -> Result<Signed<$logos_slice>, TokenErrors> {
        use crate::graphqlx::syntactic::number::sign::{$handlers::Token as Sign, Rule};

        // Inside a block no number carries a sign: the `-` is the operator.
        if lexer.extras.in_const_block() {
          return one(tt_hook(lexer)).map(|()| Signed::Operator);
        }

        let mut sign = <Sign as Logos>::lexer(lexer.source());
        sign.bump(lexer.span().start);
        // `-` alone is a rule of the sign's grammar, so the scan matches at least the `-`.
        let (rule, end) = match sign.next() {
          Some(Ok(token)) => (token.rule(), sign.span().end),
          Some(Err(())) | None => (Rule::Minus, lexer.span().end),
        };
        lexer.bump(end.saturating_sub(lexer.span().end));

        match rule {
          Rule::Minus => one(tt_hook(lexer)).map(|()| Signed::Operator),
          Rule::Float => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::FloatMissingInteger => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix,
          )
          .map(Signed::Float),
          Rule::FloatFractionEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))
              .map(Signed::Float)
          }
          Rule::FloatEmptyFraction => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))
              .map(Signed::Float)
          }
          Rule::FloatEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))
              .map(Signed::Float)
          }
          Rule::HexFloat => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_hex_float_suffix(lexer, HexFloatError::UnexpectedSuffix)
          }))
          .map(Signed::HexFloat),
          Rule::HexFloatMissingExponent => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix,
          )
          .map(Signed::HexFloat),
          Rule::HexFloatMissingInteger => tt_hook_and_then_into_errors(
            lexer,
            handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix,
          )
          .map(Signed::HexFloat),
          Rule::HexFloatFractionEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))
              .map(Signed::HexFloat)
          }
          Rule::HexFloatEmptyFraction => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_fractional_error))
              .map(Signed::HexFloat)
          }
          Rule::HexFloatEmptyExponent => {
            one(tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))
              .map(Signed::HexFloat)
          }
          Rule::Decimal => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Decimal),
          Rule::Binary => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix)
          }))
          .map(Signed::Binary),
          Rule::BinaryMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_binary_suffix)
              .map(Signed::Binary)
          }
          Rule::Octal => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix)
          }))
          .map(Signed::Octal),
          Rule::OctalMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_octal_suffix)
              .map(Signed::Octal)
          }
          Rule::Hex => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix)
          }))
          .map(Signed::Hex),
          Rule::HexMissingDigits => {
            tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_hex_suffix)
              .map(Signed::Hex)
          }
          Rule::FloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, FloatError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::IntegerFloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Float),
          Rule::HexFloatSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_float_suffix(lexer, HexFloatError::UnexpectedSuffix)
          }))
          .map(Signed::HexFloat),
          Rule::DecimalSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, DecimalError::UnexpectedSuffix)
          }))
          .map(Signed::Decimal),
          Rule::BinarySuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, BinaryError::UnexpectedSuffix)
          }))
          .map(Signed::Binary),
          Rule::OctalSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, OctalError::UnexpectedSuffix)
          }))
          .map(Signed::Octal),
          Rule::HexSuffixed => one(tt_hook_and_then(lexer, |lexer| {
            handlers::$handlers::handle_int_suffix(lexer, HexError::UnexpectedSuffix)
          }))
          .map(Signed::Hex),
        }
      }

      /// One lexeme's answer, with its error in the grammar's error type.
      #[allow(clippy::result_large_err)]
      #[inline(always)]
      fn one<O>(answer: Result<O, TokenError>) -> Result<O, TokenErrors> {
        answer.map_err(TokenErrors::from)
      }

      /// The name rule's callback: a name; or, inside a block, when a quote touches the name, the
      /// one lexeme the name and the literal make, read out of line.
      #[allow(clippy::result_large_err)]
      #[inline(always)]
      fn name<$logos_lt>(
        lexer: &mut tokora::logos::Lexer<$logos_lt, Token<$logos_lt>>,
      ) -> Result<Named<$logos_slice>, TokenErrors> {
        if lexer.extras.in_const_block()
          && matches!(AsRef::<[u8]>::as_ref(&lexer.remainder()).first(), Some(b'"' | b'\''))
        {
          return handlers::$handlers::prefixed(lexer).map(Named::Prefixed);
        }
        one(tt_hook_map(lexer, |lexer| Named::Identifier(lexer.slice())))
      }

      impl tokora::Token<'_> for LosslessToken<$slice> {
        type Kind = LosslessTokenKind;
        type Error = TokenErrors;

        /// This lexer surfaces every byte, including whitespace, commas and comments, so tokora's
        /// lossless `Sink` may wrap it. A trivia-skipping lexer cannot: a skipped-whitespace gap is
        /// indistinguishable from a dropped committed token.
        const SURFACES_TRIVIA: bool = true;

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
        /// the DFA accepted, and that leaf is `-`. Its callback reads the rest of the lexeme with
        /// the sign's grammar and records no probe, so `State::take_probe` has nothing to take:
        /// over `-.`, a cut of `-.5`, the sign's grammar answers a bare `-` the complete parse does
        /// not have. The price of `Unbounded` is paid by a `Partial` consumer alone, which buffers
        /// until the stream is sealed; every door this workspace ships is `Complete`, where there is
        /// no holdback at all.
        const SCAN_LOOKAHEAD: tokora::ScanLookahead = tokora::ScanLookahead::Unbounded;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[inline(always)]
        fn is_trivia(&self) -> bool {
          self.is_trivia()
        }
      }

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = tokora::logos,
        extras = LosslessState,
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::cst_default_error)
      )]
      #[logos(subpattern ident = "[a-zA-Z_][a-zA-Z0-9_]*")]
      #[logos(subpattern digit = "[0-9]")]
      #[logos(subpattern hex_digit = "[0-9a-fA-F]")]
      #[logos(subpattern octal_digit = "[0-7]")]
      #[logos(subpattern binary_digit = "[01]")]
      #[logos(subpattern digits_with_sep = "[0-9_]*[0-9][0-9_]*")]
      #[logos(subpattern hex_digits_with_sep = "[0-9a-fA-F_]*[0-9a-fA-F][0-9a-fA-F_]*")]
      #[logos(subpattern decimal = "(?&digit)[0-9_]*")]
      #[logos(subpattern hex_start = "0x_*")]
      #[logos(subpattern hex = "(?&hex_start)(?&hex_digit)[0-9a-fA-F_]*")]
      #[logos(subpattern octal_start = "0o_*")]
      #[logos(subpattern octal = "(?&octal_start)(?&octal_digit)[0-7_]*")]
      #[logos(subpattern binary_start = "0b_*")]
      #[logos(subpattern binary = "(?&binary_start)(?&binary_digit)[01_]*")]
      #[logos(subpattern frac = "\\.(?&digits_with_sep)")]
      #[logos(subpattern esign = "[eE][+-]?")]
      #[logos(subpattern exp = "(?&esign)(?&digits_with_sep)")]
      #[logos(subpattern psign = "[pP][+-]?")]
      #[logos(subpattern hex_exp = "(?&psign)(?&digits_with_sep)")]
      #[logos(subpattern hex_frac = "\\.(?&hex_digits_with_sep)")]
      #[logos(subpattern int_suffix = "_?[iu](0|[1-9][0-9]*)")]
      #[logos(subpattern float_suffix = "_?f(32|64)")]
      pub enum Token<$logos_lt> {
        #[token("*", tt_hook)]
        Asterisk,

        #[token("&", tt_hook)]
        Ampersand,

        #[token("@", tt_hook)]
        At,

        #[token(">", close_angle)]
        RAngle(AngleCloser),

        #[token("}", close_brace)]
        RBrace,

        #[token("]", decrease_recursion_depth_and_increase_token)]
        RBracket,

        #[token(")", decrease_recursion_depth_and_increase_token)]
        RParen,

        #[token(":", tt_hook)]
        Colon,

        #[token("$", tt_hook)]
        Dollar,

        #[token("=", tt_hook)]
        Equal,

        #[token("!", tt_hook)]
        Bang,

        #[token("<", open_angle)]
        LAngle,

        #[token("{", open_brace)]
        LBrace,

        #[token("[", increase_recursion_depth_and_token)]
        LBracket,

        #[token("(", increase_recursion_depth_and_token)]
        LParen,

        #[token("|", tt_hook)]
        Pipe,

        #[token("+", tt_hook)]
        Plus,

        #[token("-", minus)]
        Minus(Signed<$logos_slice>),

        #[token("...", tt_hook)]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[token("::", tt_hook)]
        PathSeparator,

        #[token("~", tt_hook)]
        Tilde,

        #[token("..", tt_hook)]
        DotDot,

        #[token("^", tt_hook)]
        Caret,

        #[token("/", tt_hook)]
        Slash,

        #[token("%", tt_hook)]
        Percent,

        #[token("<<", tt_hook)]
        Shl,

        #[token("'", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::char_literal))]
        LitChar(LitChar<$logos_slice>),

        #[token("=>", tt_hook)]
        FatArrow,

        #[token(",", comma)]
        Comma(CommaRole),

        #[token(" ", tt_hook)]
        Space,

        #[token("\t", tt_hook)]
        Tab,

        #[token("\n", tt_hook)]
        Newline,

        #[token("\r", tt_hook)]
        CarriageReturn,

        #[token("\r\n", tt_hook)]
        CarriageReturnAndNewline,

        #[token("\u{FEFF}", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Bom($logos_slice),

        #[regex($comment, |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::comment))]
        Comment($logos_slice),

        #[regex("(?&ident)", name)]
        Identifier(Named<$logos_slice>),

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex(
          "(?&frac)(?&exp)?",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("(?&decimal)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))]
        #[regex("(?&decimal)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex("(?&decimal)(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        Float($logos_slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_float_suffix(lexer, HexFloatError::UnexpectedSuffix)))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix))]
        #[regex(
          "(?&hex_frac)(?&hex_exp)",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        #[regex("(?&hex)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_fractional_error))]
        #[regex("(?&hex)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)(?&float_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_float_suffix(lexer, HexFloatError::UnexpectedSuffix)))]
        HexFloat($logos_slice),

        #[regex("(?&decimal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        #[regex("(?&decimal)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        Decimal($logos_slice),

        #[regex("(?&binary)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix)))]
        #[regex("(?&binary_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_binary_suffix))]
        #[regex("(?&binary)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, BinaryError::UnexpectedSuffix)))]
        Binary($logos_slice),

        #[regex("(?&octal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix)))]
        #[regex("(?&octal_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_octal_suffix))]
        #[regex("(?&octal)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, OctalError::UnexpectedSuffix)))]
        Octal($logos_slice),

        #[regex("(?&hex)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix)))]
        #[regex("(?&hex_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_hex_suffix))]
        #[regex("(?&hex)(?&int_suffix)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_int_suffix(lexer, HexError::UnexpectedSuffix)))]
        Hex($logos_slice),

        #[token("\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokora::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),

        #[token("\"\"\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokora::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitBlockStr(LitBlockStr<$logos_slice>),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for LosslessToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
            Token::Asterisk => Self::Asterisk,
            Token::RAngle(AngleCloser::Closer) => Self::RAngle,
            Token::RAngle(AngleCloser::Shr) => Self::Shr,
            Token::RBrace => Self::RBrace,
            Token::RBracket => Self::RBracket,
            Token::RParen => Self::RParen,
            Token::Colon => Self::Colon,
            Token::Dollar => Self::Dollar,
            Token::Equal => Self::Equal,
            Token::FatArrow => Self::FatArrow,
            Token::Bang => Self::Bang,
            Token::LAngle => Self::LAngle,
            Token::LBrace => Self::LBrace,
            Token::LBracket => Self::LBracket,
            Token::LParen => Self::LParen,
            Token::Pipe => Self::Pipe,
            Token::Spread => Self::Spread,
            Token::Float($val) => Self::LitFloat(LitFloat::Decimal($convert)),
            Token::HexFloat($val) => Self::LitFloat(LitFloat::Hex($convert)),
            Token::Identifier(Named::Identifier($val)) => Self::Identifier($convert),
            Token::Identifier(Named::Prefixed(lit)) => {
              let (prefix, block) = (lit.prefix(), lit.is_block());
              let $val = *lit.source_ref();
              Self::LitPrefixedStr(LitPrefixedStr::new(prefix, block, $convert))
            }
            Token::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
            Token::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            Token::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
            Token::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Plus => Self::Plus,
            Token::Minus(signed) => match signed {
              Signed::Operator => Self::Minus,
              Signed::Float($val) => Self::LitFloat(LitFloat::Decimal($convert)),
              Signed::HexFloat($val) => Self::LitFloat(LitFloat::Hex($convert)),
              Signed::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
              Signed::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
              Signed::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
              Signed::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            },
            Token::PathSeparator => Self::PathSeparator,
            Token::Tilde => Self::Tilde,
            Token::DotDot => Self::DotDot,
            Token::Caret => Self::Caret,
            Token::Slash => Self::Slash,
            Token::Percent => Self::Percent,
            Token::Shl => Self::Shl,
            Token::LitChar(lit) => {
              let value = lit.value();
              let $val = *lit.source_ref();
              Self::LitChar(LitChar::new($convert, value))
            }
            Token::Comma(CommaRole::Trivia) => Self::Comma,
            Token::Comma(CommaRole::Separator) => Self::Separator,
            Token::Space => Self::Space,
            Token::Tab => Self::Tab,
            Token::Newline => Self::Newline,
            Token::CarriageReturn => Self::CarriageReturn,
            Token::CarriageReturnAndNewline => Self::CarriageReturnAndNewline,
            Token::Bom($val) => Self::Bom($convert),
            Token::Comment($val) => Self::Comment($convert),
          }
        }
      }

      impl<'b> tokora::lexer::FromLogos<'b> for LosslessToken<$slice> {
        type Logos = Token<'b>;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
}

/// What the grammar's `-` rule made: the operator, or the number the `-` is the sign of, with the
/// lexeme's slice.
///
/// `pub` because the logos token carries it, and that token is reachable at `pub` as
/// `FromLogos::Logos`. This module is private and nothing re-exports the type, so no caller can
/// name it.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Signed<S> {
  /// The `-` operator.
  Operator,
  /// A decimal float.
  Float(S),
  /// A hexadecimal float.
  HexFloat(S),
  /// A decimal integer.
  Decimal(S),
  /// A binary integer.
  Binary(S),
  /// An octal integer.
  Octal(S),
  /// A hexadecimal integer.
  Hex(S),
}

use tokora::{
  logos::{Lexer, Logos, Source},
  state::LimitExceeded,
};

use crate::{
  graphqlx::{
    LitPrefixedStr,
    error::LexerError,
    handlers::{increase_recursion_depth_and_token, tt_hook},
    lossless::LosslessState,
  },
  handlers::decrease_recursion_depth_and_increase_token,
};

pub(super) use token;
pub(super) use token_impl;

/// What the grammar's `,` rule made: the trivia `Comma`; or, inside a `const { … }` block with no
/// `<` open, the token `Separator`.
///
/// `pub` for the reason [`Signed`] is.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CommaRole {
  /// The trivia `Comma`.
  Trivia,
  /// The token `Separator`.
  Separator,
}

/// What the grammar's `>` rule made: a closer; or, inside a `const { … }` block with no `<` open
/// and a `>` after it, `Shr`, which took that `>` too.
///
/// `pub` for the reason [`Signed`] is.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum AngleCloser {
  /// A closer.
  Closer,
  /// The shift `>>`.
  Shr,
}

/// What the grammar's name rule made: a name; or, inside a `const { … }` block, a prefixed string,
/// the one lexeme a name and the string it touches make.
///
/// `pub` for the reason [`Signed`] is.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Named<S> {
  /// A name.
  Identifier(S),
  /// A prefixed string.
  Prefixed(LitPrefixedStr<S>),
}

/// The `,` rule's callback: one token's charge, as the trivia comma's, and which token it is.
#[inline(always)]
pub(super) fn comma<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<CommaRole, LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  let role = if lexer.extras.in_const_block() && lexer.extras.open_angles() == 0 {
    CommaRole::Separator
  } else {
    CommaRole::Trivia
  };
  tt_hook(lexer).map(|()| role)
}

/// The `<` rule's callback: an opener's hook, with the block's count of open `<` stepped first,
/// before the tally's check.
#[inline(always)]
pub(super) fn open_angle<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<(), LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
{
  lexer.extras.open_angle();
  increase_recursion_depth_and_token(lexer)
}

/// The `>` rule's callback. Inside a block with no `<` open and a `>` after it: the shift, which
/// takes that `>`, charges one token and steps no tally. Otherwise a closer's hook, and inside a
/// block with a `<` open, the count of open `<` lowered with the tally.
#[inline(always)]
pub(super) fn close_angle<'a, C, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<AngleCloser, LexerError<C, LimitExceeded>>
where
  T: Logos<'a, Extras = LosslessState>,
  <T::Source as Source>::Slice<'a>: AsRef<[u8]>,
{
  if lexer.extras.in_const_block()
    && lexer.extras.open_angles() == 0
    && lexer.remainder().as_ref().first() == Some(&b'>')
  {
    lexer.bump(1);
    return tt_hook(lexer).map(|()| AngleCloser::Shr);
  }
  lexer.extras.close_angle();
  decrease_recursion_depth_and_increase_token(lexer);
  Ok(AngleCloser::Closer)
}

/// The `}` rule's callback: a closing brace's hook, after the brace's clear of the block's count
/// of open `<`, which outside a block is the identity.
#[inline(always)]
pub(super) fn close_brace<'a, T>(lexer: &mut Lexer<'a, T>)
where
  T: Logos<'a, Extras = LosslessState>,
{
  lexer.extras.clear_angles();
  crate::handlers::close_brace(lexer);
}
