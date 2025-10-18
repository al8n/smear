macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
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

      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for SyntacticToken<$slice> {
        type Kind = SyntacticTokenKind;
        type Char = $char;
        type Logos = Token $(<$lt>)?;

        #[inline(always)]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }
      }

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = logosky::logos,
        extras = RecursionLimiter,
        skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*",
        source = $source,
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
      pub enum Token $(<$lt>)? {
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
        Identifier($slice),

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("(?&decimal)\\._*", handlers::$handlers::handle_fractional_error)]
        #[regex("(?&decimal)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexFloatError::UnexpectedSuffix))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix(lexer))]
        #[regex(
          "-?(?&hex_frac)(?&hex_exp)",
          handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        #[regex("(?&hex)\\._*", handlers::$handlers::handle_hex_fractional_error)]
        #[regex("(?&hex)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        HexFloat($slice),

        #[regex("(?&decimal)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        Decimal($slice),

        #[regex("(?&binary)", |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix))]
        #[regex("(?&binary_start)", |lexer| handlers::$handlers::handle_invalid_binary_suffix(lexer))]
        Binary($slice),

        #[regex("(?&octal)", |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix))]
        #[regex("(?&octal_start)", |lexer| handlers::$handlers::handle_invalid_octal_suffix(lexer))]
        Octal($slice),

        #[regex("(?&hex)", |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix))]
        #[regex("(?&hex_start)", handlers::$handlers::handle_invalid_hex_suffix)]
        Hex($slice),

        #[token("\"", |lexer| {
          <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitInlineStr(LitInlineStr<$slice>),

        #[token("\"\"\"", |lexer| {
          <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
        })]
        LitBlockStr(LitBlockStr<$slice>),
      }

      impl$(<$lt>)? From<Token $(<$lt>)?> for SyntacticToken<$slice> {
        #[inline(always)]
        fn from(value: Token $(<$lt>)?) -> Self {
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
            Token::Float(s) => Self::LitFloat(LitFloat::Decimal(s)),
            Token::HexFloat(s) => Self::LitFloat(LitFloat::Hex(s)),
            Token::Identifier(s) => Self::Identifier(s),
            Token::Decimal(s) => Self::LitInt(LitInt::Decimal(s)),
            Token::Hex(s) => Self::LitInt(LitInt::Hex(s)),
            Token::Binary(s) => Self::LitInt(LitInt::Binary(s)),
            Token::Octal(s) => Self::LitInt(LitInt::Octal(s)),
            Token::LitInlineStr(s) => Self::LitInlineStr(s),
            Token::LitBlockStr(s) => Self::LitBlockStr(s),
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::PathSeparator => Self::PathSeparator,
          }
        }
      }
    }
  };
}

pub(super) use token;
