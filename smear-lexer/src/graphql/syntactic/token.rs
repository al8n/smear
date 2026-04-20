macro_rules! token {
  // Borrowed slice: $slice uses lifetime $lt and IS the logos slice type (no conversion)
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt $(,)?)) => {
    $crate::graphql::syntactic::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $slice)
      {s => s}
    );
  };
  // Borrowed-with-conversion: $slice uses lifetime $lt but differs from logos slice
  // (e.g., hipstr::HipByt<'a> with logos slice &'a [u8])
  ($mod:ident <$lt:lifetime>($slice: ty, $char: ty, $handlers:ident, $utf8:tt, $logos_slice:ty $(,)?)) => {
    $crate::graphql::syntactic::token_impl!(
      $mod [$lt] [$lt]
      ($slice, $char, $handlers, $utf8, $logos_slice)
      {s => s.into_equivalent()}
    );
  };
  // Owned byte-slice: $slice is an owned type (no lifetime)
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, false $(,)?)) => {
    $crate::graphql::syntactic::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, false, &'s [u8])
      {s => s.into_equivalent()}
    );
  };
  // Owned str: $slice is an owned type (no lifetime)
  ($mod:ident ($slice: ty, $char: ty, $handlers:ident, true $(,)?)) => {
    $crate::graphql::syntactic::token_impl!(
      $mod ['s] []
      ($slice, $char, $handlers, true, &'s str)
      {s => s.into_equivalent()}
    );
  };
}

/// Internal implementation macro.
///
/// `$logos_lt` is the lifetime for the internal Token enum (always present).
/// `$slice_lt` is optionally empty (for owned types) or contains the lifetime that
/// binds the outer type to the Token enum.
macro_rules! token_impl {
  // With slice lifetime (borrowed or borrowed-with-conversion)
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
        graphql::{
          error::{LexerErrors, LexerError, DecimalError, FloatError},
          handlers::{
            increase_recursion_depth,
            self,
          },
          syntactic::{SyntacticToken, SyntacticTokenKind},
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

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::token::KeywordToken<'b> for SyntacticToken<$slice> {
        fn keyword(&self) -> Option<&'static str> {
          match self {
            Self::Identifier(ident) => {
              let s: &[u8] = ident.as_ref();
              match s {
                b"type" => Some("type"),
                b"interface" => Some("interface"),
                b"union" => Some("union"),
                b"enum" => Some("enum"),
                b"input" => Some("input"),
                b"scalar" => Some("scalar"),
                b"extend" => Some("extend"),
                b"schema" => Some("schema"),
                b"directive" => Some("directive"),
                b"fragment" => Some("fragment"),
                b"query" => Some("query"),
                b"mutation" => Some("mutation"),
                b"subscription" => Some("subscription"),
                b"implements" => Some("implements"),
                b"repeatable" => Some("repeatable"),
                b"on" => Some("on"),
                b"true" => Some("true"),
                b"false" => Some("false"),
                b"null" => Some("null"),
                _ => None,
              }
            },
            _ => None,
          }
        }
      }

      impl<'b: $slice_lt, $slice_lt: 'b> tokit::token::PunctuatorToken<'b> for SyntacticToken<$slice> {
        fn pipe() -> Option<Self::Kind> { Some(SyntacticTokenKind::Pipe) }
        fn ampersand() -> Option<Self::Kind> { Some(SyntacticTokenKind::Ampersand) }
        fn at() -> Option<Self::Kind> { Some(SyntacticTokenKind::At) }
        fn colon() -> Option<Self::Kind> { Some(SyntacticTokenKind::Colon) }
        fn open_paren() -> Option<Self::Kind> { Some(SyntacticTokenKind::LParen) }
        fn close_paren() -> Option<Self::Kind> { Some(SyntacticTokenKind::RParen) }
        fn open_brace() -> Option<Self::Kind> { Some(SyntacticTokenKind::LBrace) }
        fn close_brace() -> Option<Self::Kind> { Some(SyntacticTokenKind::RBrace) }
        fn open_bracket() -> Option<Self::Kind> { Some(SyntacticTokenKind::LBracket) }
        fn close_bracket() -> Option<Self::Kind> { Some(SyntacticTokenKind::RBracket) }
        fn equal() -> Option<Self::Kind> { Some(SyntacticTokenKind::Equal) }
        fn exclamation() -> Option<Self::Kind> { Some(SyntacticTokenKind::Bang) }
        fn dollar() -> Option<Self::Kind> { Some(SyntacticTokenKind::Dollar) }
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
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token<$logos_lt> {
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
        Float($logos_slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($logos_slice),

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($logos_slice),
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
  // Without slice lifetime (owned types like bytes::Bytes)
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
        graphql::{
          error::{LexerErrors, LexerError, DecimalError, FloatError},
          handlers::{
            increase_recursion_depth,
            self,
          },
          syntactic::{SyntacticToken, SyntacticTokenKind},
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

      impl tokit::token::KeywordToken<'_> for SyntacticToken<$slice> {
        fn keyword(&self) -> Option<&'static str> {
          $crate::graphql::syntactic::graphql_keyword(self)
        }
      }

      impl tokit::token::PunctuatorToken<'_> for SyntacticToken<$slice> {
        fn pipe() -> Option<Self::Kind> { Some(SyntacticTokenKind::Pipe) }
        fn ampersand() -> Option<Self::Kind> { Some(SyntacticTokenKind::Ampersand) }
        fn at() -> Option<Self::Kind> { Some(SyntacticTokenKind::At) }
        fn colon() -> Option<Self::Kind> { Some(SyntacticTokenKind::Colon) }
        fn open_paren() -> Option<Self::Kind> { Some(SyntacticTokenKind::LParen) }
        fn close_paren() -> Option<Self::Kind> { Some(SyntacticTokenKind::RParen) }
        fn open_brace() -> Option<Self::Kind> { Some(SyntacticTokenKind::LBrace) }
        fn close_brace() -> Option<Self::Kind> { Some(SyntacticTokenKind::RBrace) }
        fn open_bracket() -> Option<Self::Kind> { Some(SyntacticTokenKind::LBracket) }
        fn close_bracket() -> Option<Self::Kind> { Some(SyntacticTokenKind::RBracket) }
        fn equal() -> Option<Self::Kind> { Some(SyntacticTokenKind::Equal) }
        fn exclamation() -> Option<Self::Kind> { Some(SyntacticTokenKind::Bang) }
        fn dollar() -> Option<Self::Kind> { Some(SyntacticTokenKind::Dollar) }
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
      #[logos(subpattern digit = r"[0-9]")]
      #[logos(subpattern non_zero_digit = r"[1-9]")]
      #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
      #[logos(subpattern esign = r"[eE][+-]?")]
      #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
      #[logos(subpattern frac = r"\.(?&digit)+")]
      pub enum Token<$logos_lt> {
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
        Float($logos_slice),

        #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
        Identifier($logos_slice),

        #[regex("(?&int)", |lexer| handlers::$handlers::handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
        #[regex("-?0(?&digit)+", |lexer| handlers::$handlers::handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
        #[token("-", handlers::$handlers::unexpected_minus_token)]
        #[token("+", handlers::$handlers::unexpected_plus_token)]
        Int($logos_slice),
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
