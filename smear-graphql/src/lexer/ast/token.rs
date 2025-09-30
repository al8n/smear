
macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::{
        lexer::{
          handlers::{unterminated_spread_operator, decrease_recursion_depth, increase_recursion_depth, self},
          ast::{AstToken, TokenKind},
          string_lexer::{LitBlockStr, LitInlineStr},
          SealedWrapper,
        },
        error::{LexerErrors, LexerError, IntError, FloatError},
      };

      type TokenError = LexerError<$char, RecursionLimitExceeded>;
      type TokenErrors = LexerErrors<$char, RecursionLimitExceeded>;

      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for AstToken<$slice> {
        type Kind = TokenKind;
        type Char = $char;
        type Logos = Token $(<$lt>)?;

        #[inline(always)]
        fn from_logos(value: Self::Logos) -> Self {
          Self::from(value)
        }

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
      pub enum Token $(<$lt>)? {
        #[token("&")]
        Ampersand,

        #[token("@")]
        At,

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

        #[regex("-?0[0-9]+(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
        #[regex("-?(0|[1-9][0-9]*)(\\.[0-9]+[eE][+-]?[0-9]+|\\.[0-9]+|[eE][+-]?[0-9]+)", |lexer| handlers::$handlers::handle_number_suffix(lexer, FloatError::UnexpectedSuffix))]
        #[regex(
          "-?\\.[0-9]+([eE][+-]?[0-9]+)?",
          handlers::$handlers::handle_float_missing_integer_part_error_and_suffix
        )]
        #[regex("-?0[0-9]+\\.[0-9]+[eE][+-]?", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|[1-9][0-9]*)\\.[0-9]+[eE][+-]?", handlers::$handlers::handle_exponent_error)]
        #[regex("-?0[0-9]+\\.", handlers::$handlers::handle_leading_zeros_and_fractional_error)]
        #[regex("-?(0|[1-9][0-9]*)\\.", handlers::$handlers::handle_fractional_error)]
        #[regex("-?0[0-9]+[eE][+-]?", handlers::$handlers::handle_leading_zeros_and_exponent_error)]
        #[regex("-?(0|[1-9][0-9]*)[eE][+-]?", handlers::$handlers::handle_exponent_error)]
        Float($slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($slice),

        #[regex("-?(0|[1-9][0-9]*)", |lexer| handlers::$handlers::handle_number_suffix(lexer, IntError::UnexpectedSuffix))]
        #[regex("-?0[0-9]+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, IntError::LeadingZeros, IntError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($slice),
        #[token("\"", |lexer| {
          <LitInlineStr<_> as Lexable<_, TokenError>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer))
        })]
        LitInlineStr(LitInlineStr<$slice>),
        #[token("\"\"\"", |lexer| {
          <LitBlockStr<_> as Lexable<_, TokenError>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer))
        })]
        LitBlockStr(LitBlockStr<$slice>),
      }

      impl$(<$lt>)? From<Token $(<$lt>)?> for AstToken<$slice> {
        #[inline(always)]
        fn from(value: Token $(<$lt>)?) -> Self {
          match value {
            Token::Ampersand => Self::Ampersand,
            Token::At => Self::At,
            Token::RBrace => Self::RBrace,
            Token::RBracket => Self::RBracket,
            Token::RParen => Self::RParen,
            Token::Colon => Self::Colon,
            Token::Dollar => Self::Dollar,
            Token::Equal => Self::Equal,
            Token::Bang => Self::Bang,
            Token::LBrace => Self::LBrace,
            Token::LBracket => Self::LBracket,
            Token::LParen => Self::LParen,
            Token::Pipe => Self::Pipe,
            Token::Spread => Self::Spread,
            Token::Float(s) => Self::Float(s),
            Token::Identifier(s) => Self::Identifier(s),
            Token::Int(s) => Self::Int(s),
            Token::LitInlineStr(s) => Self::LitInlineStr(s),
            Token::LitBlockStr(s) => Self::LitBlockStr(s),
          }
        }
      }
    }
  };
}

pub(super) use token;