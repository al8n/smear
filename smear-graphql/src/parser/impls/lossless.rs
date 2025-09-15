use logosky::{
  TokenStream,
  utils::{Span, tracker::LimitExceeded},
};

use crate::{
  error::Extra,
  lexer::token::lossless::{Token, TokenKind},
};

mod enum_value;
mod float;
mod int;
mod list;
mod name;
mod punctuator;
mod string;
mod variable;

/// The token stream type used for the lossless parser implementation.
pub type LosslessTokenStream<'a> = TokenStream<'a, Token<'a>>;
/// The parser extra type used for the lossless parser implementation.
pub type LosslessParserExtra<'a> = Extra<'a, Token<'a>, TokenKind, char, LimitExceeded>;
