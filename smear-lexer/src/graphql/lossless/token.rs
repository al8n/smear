macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::tracker::{LimitExceeded, Tracker},
      };
      use crate::{
        error::StringErrors,
        graphql::{
          error::{LexerErrors, LexerError, DecimalError, FloatError},
          handlers::{
            self,
            tt_hook, tt_hook_and_then, tt_hook_map, tt_hook_and_then_into_errors,
            increase_recursion_depth_and_token,
          },
          lossless::{LosslessToken, LosslessTokenKind},
        },
        handlers::{decrease_recursion_depth_and_increase_token, unterminated_spread_operator_error},
        LitBlockStr, LitInlineStr, SealedWrapper,
      };

      type TokenError = LexerError<$char, LimitExceeded>;
      type TokenErrors = LexerErrors<$char, LimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for LosslessToken<$slice> {
        type Kind = LosslessTokenKind;
        type Char = $char;
        type Logos = Token $(<$lt>)?;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }
      }

      #[derive(
        Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash
      )]
      #[logos(
        crate = logosky::logos,
        extras = Tracker,
        source = $source,
        error(TokenErrors, handlers::$handlers::cst_default_error)
      )]
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token $(<$lt>)? {
        #[token("&", tt_hook)]
        Ampersand,

        #[token("@", tt_hook)]
        At,

        #[token("}", decrease_recursion_depth_and_increase_token)]
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

        #[token("{", increase_recursion_depth_and_token)]
        LBrace,

        #[token("[", increase_recursion_depth_and_token)]
        LBracket,

        #[token("(", increase_recursion_depth_and_token)]
        LParen,

        #[token("|", tt_hook)]
        Pipe,

        #[token(",", tt_hook)]
        Comma,

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
        Bom($slice),

        #[token("...", tt_hook)]
        #[token("..", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[regex("#[^\n\r]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Comment($slice),

        #[regex("-?0(?&digit)+((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| tt_hook_and_then_into_errors(lexer, |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix)))]
        #[regex("(?&int)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("-?0(?&digit)+(?&frac)(?&esign)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_leading_zeros_and_exponent_error))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&frac)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("-?0(?&digit)+\\.", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_leading_zeros_and_fractional_error))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)\\.", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))]
        #[regex("-?0(?&digit)+(?&esign)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_leading_zeros_and_exponent_error))]
        #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        Float($slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Identifier($slice),

        #[regex("(?&int)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        #[regex("-?0(?&digit)+", |lexer| tt_hook_and_then_into_errors(lexer, |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix)))]
        #[token("-", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_minus_token)
        })]
        #[token("+", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_plus_token)
        })]
        Int($slice),
        #[token("\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitInlineStr(LitInlineStr<$slice>),
        #[token("\"\"\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<logosky::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitBlockStr(LitBlockStr<$slice>),
      }

      impl$(<$lt>)? From<Token $(<$lt>)?> for LosslessToken<$slice> {
        #[cfg_attr(not(tarpaulin), inline(always))]
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
            Token::Comma => Self::Comma,
            Token::Space => Self::Space,
            Token::Tab => Self::Tab,
            Token::Newline => Self::Newline,
            Token::CarriageReturn => Self::CarriageReturn,
            Token::CarriageReturnAndNewline => Self::CarriageReturnAndNewline,
            Token::Bom(s) => Self::Bom(s),
            Token::Comment(s) => Self::Comment(s),
          }
        }
      }
    }
  };
}

pub(super) use token;
