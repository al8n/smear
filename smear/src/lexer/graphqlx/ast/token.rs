macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty, $to_slice_iter:expr)) => {
    mod $mod {
      use logosky::{
        Logos, logos::Lexer, Lexable, utils::{recursion_tracker::{RecursionLimitExceeded, RecursionLimiter}, Lexeme},
      };
      use crate::{
        error::StringErrors,
        lexer::{graphqlx::{
          error::{LexerErrors, LexerError, LexerErrorData, IntError, FloatError},
          handlers::{increase_recursion_depth, self},
          ast::{AstToken, AstTokenKind},
        }, LitBlockStr, LitInlineStr, SealedWrapper, handlers::*},
      };

      type TokenErrorData = LexerErrorData<$char, RecursionLimitExceeded>;
      type TokenError = LexerError<$char, RecursionLimitExceeded>;
      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      #[inline(always)]
      fn handle_decimal_suffix<'a, E>(
        lexer: &mut Lexer<'a, Token<'a>>,
        unexpected_suffix: impl FnOnce(Lexeme<$char>) -> E,
      ) -> Result<$slice, TokenError>
      where
        E: Into<TokenErrorData>,
      {
        let remainder = lexer.remainder();
        handle_graphqlx_decimal_suffix(lexer, remainder.len(), $to_slice_iter(remainder), unexpected_suffix)
          .map_err(|e| TokenError::new(lexer.span(), e.into()))
      }

      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for AstToken<$slice> {
        type Kind = AstTokenKind;
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
      // #[logos(subpattern invalid_octal_digit = "(?&octal_start)(?&digit)[0-9_]*")]
      #[logos(subpattern binary_start = "-?0b_*")]
      #[logos(subpattern binary = "(?&binary_start)(?&binary_digit)[01_]*")]
      // #[logos(subpattern invalid_binary_digit = "(?&binary_start)(?&digit)[0-9_]*")]
      #[logos(subpattern frac = "\\.(?&digits_with_sep)")]
      #[logos(subpattern esign = "[eE][+-]?")]
      #[logos(subpattern exp = "(?&esign)(?&digits_with_sep)")]
      #[logos(subpattern psign = "[pP][+-]?")]
      #[logos(subpattern hex_exp = "(?&psign)(?&hex_digits_with_sep)")]
      #[logos(subpattern hex_frac = "\\.(?&hex_digits_with_sep)")]
      pub enum Token $(<$lt>)? {
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

        #[token("=>")]
        FatArrow,

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($slice),

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          handlers::$handlers::handle_float_missing_integer_part_error_and_suffix
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", handlers::$handlers::handle_exponent_error)]
        #[regex("(?&decimal)\\._*", handlers::$handlers::handle_fractional_error)]
        #[regex("(?&decimal)(?&esign)", handlers::$handlers::handle_exponent_error)]
        Float($slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| handlers::$handlers::handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| handlers::$handlers::handle_hex_float_missing_exp(lexer))]
        #[regex(
          "-?(?&hex_frac)?(?&hex_exp)",
          handlers::$handlers::handle_hex_float_missing_integer_part_error_and_suffix
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        #[regex("(?&hex)\\._*", handlers::$handlers::handle_hex_fractional_error)]
        #[regex("(?&hex)(?&psign)", handlers::$handlers::handle_hex_exponent_error)]
        HexFloat($slice),

        #[regex("(?&decimal)", |lexer| handle_decimal_suffix(lexer, IntError::UnexpectedSuffix))]
        Decimal($slice),

        #[regex("(?&binary)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        // #[regex("(?&invalid_binary_digit)", |lexer| handlers::$handlers::handle_invalid_binary_digit_error_and_number_suffix(lexer))]
        #[regex("(?&binary_start)", |lexer| handlers::$handlers::missing_digits_after_binary_prefix(lexer))]
        Binary($slice),

        #[regex("(?&octal)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        // #[regex("(?&invalid_octal_digit)", |lexer| handlers::$handlers::handle_invalid_octal_digit_error_and_number_suffix(lexer))]
        #[regex("(?&octal_start)", |lexer| handlers::$handlers::missing_digits_after_octal_prefix(lexer))]
        Octal($slice),

        #[regex("(?&hex)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        #[regex("(?&hex_start)", |lexer| handlers::$handlers::missing_digits_after_hex_prefix(lexer))]
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

      impl$(<$lt>)? From<Token $(<$lt>)?> for AstToken<$slice> {
        #[inline(always)]
        fn from(value: Token $(<$lt>)?) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
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
            Token::Float(s) => Self::LitFloat(s),
            Token::Identifier(s) => Self::Identifier(s),
            Token::Int(s) => Self::LitInt(s),
            Token::LitInlineStr(s) => Self::LitInlineStr(s),
            Token::LitBlockStr(s) => Self::LitBlockStr(s),
          }
        }
      }
    }
  };
}

pub(super) use token;
