macro_rules! token {
  // Borrowed slice: $slice uses lifetime $lt and IS the logos slice type (no conversion)
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt $(,)?)) => {
    $crate::graphql::lossless::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $slice)
      {s => s}
    );
  };
  // Borrowed-with-conversion: $slice uses lifetime $lt but differs from logos slice
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt, $logos_slice:ty $(,)?)) => {
    $crate::graphql::lossless::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $logos_slice)
      {s => s.into_equivalent()}
    );
  };
  // Owned byte-slice
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, false $(,)?)) => {
    $crate::graphql::lossless::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, false, &'s [u8])
      {s => s.into_equivalent()}
    );
  };
  // Owned str
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, true $(,)?)) => {
    $crate::graphql::lossless::token_impl!(
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
        logos::Logos, lexer::Lexable, state::tracker::{LimitExceeded, Limiter},
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

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::Token<'b> for LosslessToken<$slice> {
        type Kind = LosslessTokenKind;
        type Error = TokenErrors;

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
        crate = tokit::logos,
        extras = Limiter,
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::cst_default_error)
      )]
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token<$logos_lt> {
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
        Bom($logos_slice),

        #[token("...", tt_hook)]
        #[token("..", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[regex("#[^\n\r]*?", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Comment($logos_slice),

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
        Float($logos_slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Identifier($logos_slice),

        #[regex("(?&int)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        #[regex("-?0(?&digit)+", |lexer| tt_hook_and_then_into_errors(lexer, |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix)))]
        #[token("-", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_minus_token)
        })]
        #[token("+", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_plus_token)
        })]
        Int($logos_slice),
        #[token("\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),
        #[token("\"\"\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
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
            Token::Float($val) => Self::LitFloat($convert),
            Token::Identifier($val) => Self::Identifier($convert),
            Token::Int($val) => Self::LitInt($convert),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Comma => Self::Comma,
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

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::lexer::FromLogos<'b> for LosslessToken<$slice> {
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
        logos::Logos, lexer::Lexable, state::tracker::{LimitExceeded, Limiter},
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

      impl<'b> tokit::Token<'b> for LosslessToken<$slice> {
        type Kind = LosslessTokenKind;
        type Error = TokenErrors;

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
        crate = tokit::logos,
        extras = Limiter,
        utf8 = $utf8,
        error(TokenErrors, handlers::$handlers::cst_default_error)
      )]
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token<$logos_lt> {
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
        Bom($logos_slice),

        #[token("...", tt_hook)]
        #[token("..", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        #[token(".", |lexer| tt_hook_and_then(lexer, |lexer| TokenErrorOnlyResult::Err(unterminated_spread_operator_error(lexer))))]
        Spread,

        #[regex("#[^\n\r]*?", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Comment($logos_slice),

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
        Float($logos_slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lexer| { tt_hook_map(lexer, |lexer| lexer.slice()) })]
        Identifier($logos_slice),

        #[regex("(?&int)", |lexer| tt_hook_and_then(lexer, |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix)))]
        #[regex("-?0(?&digit)+", |lexer| tt_hook_and_then_into_errors(lexer, |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix)))]
        #[token("-", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_minus_token)
        })]
        #[token("+", |lexer| {
          tt_hook_and_then(lexer, handlers::$handlers::unexpected_plus_token)
        })]
        Int($logos_slice),
        #[token("\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitInlineStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
          })
        })]
        LitInlineStr(LitInlineStr<$logos_slice>),
        #[token("\"\"\"", |lexer| {
          tt_hook_and_then(lexer, |lexer| {
            <LitBlockStr<_> as Lexable<_, StringErrors<_>>>::lex(SealedWrapper::<tokit::logos::Lexer<'_, _>>::from_mut(lexer)).map_err(|e| TokenError::new(lexer.span(), e.into()))
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
            Token::Float($val) => Self::LitFloat($convert),
            Token::Identifier($val) => Self::Identifier($convert),
            Token::Int($val) => Self::LitInt($convert),
            Token::LitInlineStr($val) => Self::LitInlineStr($convert),
            Token::LitBlockStr($val) => Self::LitBlockStr($convert),
            Token::Comma => Self::Comma,
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

      impl<'b> tokit::lexer::FromLogos<'b> for LosslessToken<$slice> {
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
