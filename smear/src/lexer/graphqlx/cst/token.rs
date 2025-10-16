macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    mod $mod {
      use logosky::{
        Logos, Lexable, utils::tracker::{LimitExceeded, Tracker},
      };
      use crate::{
        error::StringErrors,
        lexer::{graphqlx::{
          error::{LexerErrors, LexerError, DecimalError, HexError, FloatError, HexFloatError, BinaryError, OctalError},
          handlers::{
            self,
            tt_hook, tt_hook_and_then, tt_hook_map, tt_hook_and_then_into_errors, increase_recursion_depth_and_token,
          },
          cst::{CstToken, CstTokenKind},
          LitInt, LitFloat,
        }, LitBlockStr, LitInlineStr, SealedWrapper, handlers::*},
      };

      type TokenError = LexerError<$char, LimitExceeded>;
      type TokenErrors = LexerErrors<$char, LimitExceeded>;
      type TokenErrorOnlyResult = Result<(), TokenError>;

      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for CstToken<$slice> {
        type Kind = CstTokenKind;
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
        extras = Tracker,
        source = $source,
        error(TokenErrors, handlers::$handlers::cst_default_error)
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
        #[token("*", tt_hook)]
        Asterisk,

        #[token("&", tt_hook)]
        Ampersand,

        #[token("@", tt_hook)]
        At,

        #[token(">", decrease_recursion_depth_and_increase_token)]
        RAngle,

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

        #[token("<", increase_recursion_depth_and_token)]
        LAngle,

        #[token("{", increase_recursion_depth_and_token)]
        LBrace,

        #[token("[", increase_recursion_depth_and_token)]
        LBracket,

        #[token("(", increase_recursion_depth_and_token)]
        LParen,

        #[token("|", tt_hook)]
        Pipe,

        #[token("+", tt_hook)]
        Plus,

        #[token("-", tt_hook)]
        Minus,

        #[token("...", tt_hook)]
        #[token("..", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[token("::", tt_hook)]
        PathSeparator,

        #[token("=>", tt_hook)]
        FatArrow,

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

        #[regex("#[^\n\r]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Comment($slice),

        #[regex("(?&ident)", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Identifier($slice),

        #[regex("(?&decimal)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix)))]
        #[regex(
          "-?(?&frac)(?&exp)?",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&decimal)(?&frac)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        #[regex("(?&decimal)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_fractional_error))]
        #[regex("(?&decimal)(?&esign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_exponent_error))]
        Float($slice),

        #[regex("(?&hex)(?&hex_frac)?(?&hex_exp)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexFloatError::UnexpectedSuffix)))]
        #[regex("(?&hex)(?&hex_frac)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_exponent_then_check_suffix))]
        #[regex(
          "-?(?&hex_frac)(?&hex_exp)",
          |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_hex_float_missing_integer_part_error_then_check_suffix)
        )]
        #[regex("(?&hex)(?&hex_frac)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        #[regex("(?&hex)\\._*", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_fractional_error))]
        #[regex("(?&hex)(?&psign)", |lexer| tt_hook_and_then(lexer, handlers::$handlers::handle_hex_exponent_error))]
        HexFloat($slice),

        #[regex("(?&decimal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        Decimal($slice),

        #[regex("(?&binary)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix)))]
        #[regex("(?&binary_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_binary_suffix))]
        Binary($slice),

        #[regex("(?&octal)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix)))]
        #[regex("(?&octal_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_octal_suffix))]
        Octal($slice),

        #[regex("(?&hex)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix)))]
        #[regex("(?&hex_start)", |lexer| tt_hook_and_then_into_errors(lexer, handlers::$handlers::handle_invalid_hex_suffix))]
        Hex($slice),

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

      impl$(<$lt>)? From<Token $(<$lt>)?> for CstToken<$slice> {
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
