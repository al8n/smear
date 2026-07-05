macro_rules! token {
  // Borrowed slice: $slice uses lifetime $lt and IS the logos slice type (no conversion)
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt $(,)?)) => {
    $crate::graphqlx::ast::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $slice)
      {s => s}
    );
  };
  // Borrowed-with-conversion: $slice uses lifetime $lt but differs from logos slice
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt, $logos_slice:ty $(,)?)) => {
    $crate::graphqlx::ast::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $logos_slice)
      {s => s.into_equivalent()}
    );
  };
  // Owned byte-slice
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, false $(,)?)) => {
    $crate::graphqlx::ast::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, false, &'s [u8])
      {s => s.into_equivalent()}
    );
  };
  // Owned str
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, true $(,)?)) => {
    $crate::graphqlx::ast::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, true, &'s str)
      {s => s.into_equivalent()}
    );
  };
}

macro_rules! token_impl {
  // With slice lifetime
  (
    $mod:ident [$logos_lt:lifetime] [$slice_lt:lifetime]
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokit::utils::IntoEquivalent;
      use tokit::{
        logos::Logos, lexer::Lexable, state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::{
        error::StringErrors,
        graphqlx::{
          error::{LexerErrors, LexerError, DecimalError, HexError, FloatError, HexFloatError, BinaryError, OctalError},
          handlers::{increase_recursion_depth, self},
          ast::{SyntacticToken, SyntacticTokenKind, LitInt, LitFloat},
        },
        handlers::{decrease_recursion_depth, unterminated_spread_operator_error},
        LitBlockStr, LitInlineStr, SealedWrapper,
      };

      type TokenError = LexerError<$char, RecursionLimitExceeded>;
      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::Token<'b> for SyntacticToken<$slice> {
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

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = tokit::logos,
        extras = RecursionLimiter,
        skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*?",
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::default_error)
      )]
      #[logos(subpattern ident = "[a-zA-Z_][a-zA-Z0-9_]*")]
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
        #[token("*")]
        Asterisk,

        #[token("&")]
        Ampersand,

        #[token("@")]
        At,

        #[token(">", decrease_recursion_depth)]
        RAngle,

        #[token("}", decrease_recursion_depth)]
        RBrace,

        #[token("]", decrease_recursion_depth)]
        RBracket,

        #[token(")", decrease_recursion_depth)]
        RParen,

        #[token(":")]
        Colon,

        #[token("$")]
        Dollar,

        #[token("=")]
        Equal,

        #[token("!")]
        Bang,

        #[token("<", increase_recursion_depth)]
        LAngle,

        #[token("{", increase_recursion_depth)]
        LBrace,

        #[token("[", increase_recursion_depth)]
        LBracket,

        #[token("(", increase_recursion_depth)]
        LParen,

        #[token("|")]
        Pipe,

        #[token("+")]
        Plus,

        #[token("-")]
        Minus,

        #[token("...")]
        #[token("..", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        #[token(".", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        Spread,

        #[token("::")]
        PathSeparator,

        #[token("=>")]
        FatArrow,

        #[regex("(?&ident)", |lex| lex.slice())]
        Identifier($logos_slice),

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

        #[token("\"", |lexer| {
          <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),

        #[token("\"\"\"", |lexer| {
          <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitBlockStr(LitBlockStr<$logos_slice>),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for SyntacticToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
            Token::Asterisk => Self::Asterisk,
            Token::RAngle => Self::RAngle,
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
            Token::Identifier($val) => Self::Identifier($convert),
            Token::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
            Token::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            Token::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
            Token::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::PathSeparator => Self::PathSeparator,
          }
        }
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::lexer::FromLogos<'b> for SyntacticToken<$slice> {
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
    ($slice:ty, $char:ty, $handlers:ident, $utf8:tt, $logos_slice:ty)
    {$val:ident => $convert:expr}
  ) => {
    mod $mod {
      #[allow(unused_imports)]
      use tokit::utils::IntoEquivalent;
      use tokit::{
        logos::Logos, lexer::Lexable, state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::{
        error::StringErrors,
        graphqlx::{
          error::{LexerErrors, LexerError, DecimalError, HexError, FloatError, HexFloatError, BinaryError, OctalError},
          handlers::{increase_recursion_depth, self},
          ast::{SyntacticToken, SyntacticTokenKind, LitInt, LitFloat},
        },
        handlers::{decrease_recursion_depth, unterminated_spread_operator_error},
        LitBlockStr, LitInlineStr, SealedWrapper,
      };

      type TokenError = LexerError<$char, RecursionLimitExceeded>;
      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      impl tokit::Token<'_> for SyntacticToken<$slice> {
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

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = tokit::logos,
        extras = RecursionLimiter,
        skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*?",
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::default_error)
      )]
      #[logos(subpattern ident = "[a-zA-Z_][a-zA-Z0-9_]*")]
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
        #[token("*")]
        Asterisk,

        #[token("&")]
        Ampersand,

        #[token("@")]
        At,

        #[token(">", decrease_recursion_depth)]
        RAngle,

        #[token("}", decrease_recursion_depth)]
        RBrace,

        #[token("]", decrease_recursion_depth)]
        RBracket,

        #[token(")", decrease_recursion_depth)]
        RParen,

        #[token(":")]
        Colon,

        #[token("$")]
        Dollar,

        #[token("=")]
        Equal,

        #[token("!")]
        Bang,

        #[token("<", increase_recursion_depth)]
        LAngle,

        #[token("{", increase_recursion_depth)]
        LBrace,

        #[token("[", increase_recursion_depth)]
        LBracket,

        #[token("(", increase_recursion_depth)]
        LParen,

        #[token("|")]
        Pipe,

        #[token("+")]
        Plus,

        #[token("-")]
        Minus,

        #[token("...")]
        #[token("..", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        #[token(".", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        Spread,

        #[token("::")]
        PathSeparator,

        #[token("=>")]
        FatArrow,

        #[regex("(?&ident)", |lex| lex.slice())]
        Identifier($logos_slice),

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

        #[token("\"", |lexer| {
          <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),

        #[token("\"\"\"", |lexer| {
          <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitBlockStr(LitBlockStr<$logos_slice>),
      }

      impl<$logos_lt> From<Token<$logos_lt>> for SyntacticToken<$slice> {
        #[inline(always)]
        fn from(value: Token<$logos_lt>) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
            Token::Asterisk => Self::Asterisk,
            Token::RAngle => Self::RAngle,
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
            Token::Identifier($val) => Self::Identifier($convert),
            Token::Decimal($val) => Self::LitInt(LitInt::Decimal($convert)),
            Token::Hex($val) => Self::LitInt(LitInt::Hex($convert)),
            Token::Binary($val) => Self::LitInt(LitInt::Binary($convert)),
            Token::Octal($val) => Self::LitInt(LitInt::Octal($convert)),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::PathSeparator => Self::PathSeparator,
          }
        }
      }

      impl<'b> tokit::lexer::FromLogos<'b> for SyntacticToken<$slice> {
        type Logos = Token<'b>;

        #[inline(always)]
        fn from_logos(logos_token: Self::Logos) -> Self {
          Self::from(logos_token)
        }
      }
    }
  };
}

pub(super) use token;
pub(super) use token_impl;
