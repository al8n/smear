macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::{
        error::StringErrors,
        lexer::{graphqlx::{
          error::{LexerErrors, LexerError, IntError, FloatError},
          handlers::{unterminated_spread_operator, decrease_recursion_depth, increase_recursion_depth, self},
          ast::{AstToken, AstTokenKind},
        }, LitBlockStr, LitInlineStr, SealedWrapper,},
      };

      type TokenError = LexerError<$char, RecursionLimitExceeded>;
      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;

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
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern hex_digit = r"[0-9a-fA-F]")]
      #[logos(subpattern octal_digit = r"[0-7]")]
      #[logos(subpattern binary_digit = r"[01]")]
      #[logos(subpattern digits_with_sep = r"[0-9_]*[0-9][0-9_]*")]
      #[logos(subpattern decimal = r"(?&digit)[0-9_]*")]
      #[logos(subpattern hex = r"0x_*(?&hex_digit)[0-9a-fA-F_]*")]
      #[logos(subpattern octal = r"0o_*(?&octal_digit)[0-7_]*")]
      #[logos(subpattern invalid_octal_digit = r"0o_*(?&digit)[0-9_]*")]
      #[logos(subpattern binary = r"0b_*(?&binary_digit)[01_]*")]
      #[logos(subpattern invalid_binary_digit = r"0b_*(?&digit)[0-9_]*")]
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

        #[token("...")]
        #[token("..", unterminated_spread_operator)]
        #[token(".", unterminated_spread_operator)]
        Spread,

        #[token("=>")]
        FatArrow,

        #[regex("-?(?&decimal)(\\.(?&digits_with_sep)[eE][+-]?(?&digits_with_sep)|\\.(?&digits_with_sep)|[eE][+-]?(?&digits_with_sep))", |lexer| handlers::$handlers::handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?\\.(?&digits_with_sep)([eE][+-]?(?&digits_with_sep))?",
          handlers::$handlers::handle_float_missing_integer_part_error_and_suffix
        )]
        #[regex("-?(?&decimal)\\.[0-9_]+[eE][+-]?", handlers::$handlers::handle_exponent_error)]
        #[regex("-?(?&decimal)\\._?", handlers::$handlers::handle_fractional_error)] 
        #[regex("-?(?&decimal)[eE][+-]?", handlers::$handlers::handle_exponent_error)]
        Float($slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($slice),

        #[regex("-?(?&decimal)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Decimal($slice),

        #[regex("-?(?&binary)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        #[regex("-?(?&invalid_binary_digit)", |lexer| handlers::$handlers::handle_invalid_binary_digit_error_and_number_suffix(lexer))]
        Binary($slice),

        #[regex("-?(?&octal)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        #[regex("-?(?&invalid_octal_digit)", |lexer| handlers::$handlers::handle_invalid_octal_digit_error_and_number_suffix(lexer))]
        Octal($slice),

        #[regex("-?(?&hex)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
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
