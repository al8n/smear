macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter},
      };
      use crate::{
        error::StringErrors,
        lexer::{graphql::{
          error::{LexerErrors, LexerError, DecimalError, FloatError},
          handlers::{
            increase_recursion_depth,
            self,
          },
          ast::{SyntacticToken, SyntacticTokenKind},
        }, handlers::*, LitBlockStr, LitInlineStr, SealedWrapper,},
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
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
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
        #[token("..", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        #[token(".", |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer)))]
        Spread,

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
        Float($slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($slice),

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($slice),
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
