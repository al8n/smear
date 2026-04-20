// use tokit::{SimpleSpan, lexer::Lexable, logos::Logos, state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter}};

// use crate::{LitBlockStr, LitInlineStr, SealedWrapper, error::StringErrors, graphql::{error::{DecimalError, FloatError, LexerError, LexerErrors}, handlers::str::{default_error, handle_decimal_suffix, handle_exponent_error, handle_float_missing_integer_part_error_then_check_suffix, handle_fractional_error, handle_leading_zero_and_number_suffix_error, handle_leading_zeros_and_exponent_error, handle_leading_zeros_and_fractional_error, unexpected_minus_token, unexpected_plus_token}}, handlers::unterminated_spread_operator_error};

// type Error = LexerError<char, RecursionLimitExceeded>;
// type Errors = LexerErrors<char, RecursionLimitExceeded>;
// type ErrorOnlyResult = Result<(), Error>;

// #[derive(Logos, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
// #[logos(
//   crate = tokit::logos,
//   extras = RecursionLimiter,
//   skip r"[ \t,\r\n\u{FEFF}]+|#[^\n\r]*?",
//   error(Errors, default_error)
// )]
// #[logos(subpattern digit = r"[0-9]")]
// #[logos(subpattern non_zero_digit = r"[1-9]")]
// #[logos(subpattern int = r"-?(0|(?&non_zero_digit)(?&digit)*)")]
// #[logos(subpattern esign = r"[eE][+-]?")]
// #[logos(subpattern exp = r"(?&esign)(?&digit)+")]
// #[logos(subpattern frac = r"\.(?&digit)+")]
// pub enum Token {
//   #[regex("-?0(?&digit)+((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handle_leading_zero_and_number_suffix_error(lexer, FloatError::LeadingZeros, FloatError::UnexpectedSuffix))]
//   #[regex("(?&int)((?&frac)(?&exp)|(?&frac)|(?&exp))", |lexer| handle_decimal_suffix(lexer, FloatError::UnexpectedSuffix))]
//   #[regex(
//     "-?(?&frac)(?&exp)?",
//     handle_float_missing_integer_part_error_then_check_suffix
//   )]
//   #[regex("-?0(?&digit)+(?&frac)(?&esign)", handle_leading_zeros_and_exponent_error)]
//   #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&frac)(?&esign)", handle_exponent_error)]
//   #[regex("-?0(?&digit)+\\.", handle_leading_zeros_and_fractional_error)]
//   #[regex("-?(0|(?&non_zero_digit)(?&digit)*)\\.", handle_fractional_error)]
//   #[regex("-?0(?&digit)+(?&esign)", handle_leading_zeros_and_exponent_error)]
//   #[regex("-?(0|(?&non_zero_digit)(?&digit)*)(?&esign)", handle_exponent_error)]
//   Float(SimpleSpan),

//   #[regex("(?&int)", |lexer| handle_decimal_suffix(lexer, DecimalError::UnexpectedSuffix))]
//   #[regex("-?0(?&digit)+", |lexer| handle_leading_zero_and_number_suffix_error(lexer, DecimalError::LeadingZeros, DecimalError::UnexpectedSuffix))]
//   #[token("-", unexpected_minus_token)]
//   #[token("+", unexpected_plus_token)]
//   Int(SimpleSpan),
// }
