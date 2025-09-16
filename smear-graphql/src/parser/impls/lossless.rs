use logosky::{TokenStream, utils::tracker::LimitExceeded};

use crate::{
  error::Extra,
  lexer::token::lossless::{Token, TokenKind},
};

pub use ast::*;

mod ast;

/// The token stream type used for the lossless parser implementation.
pub type LosslessTokenStream<'a> = TokenStream<'a, Token<'a>>;
/// The parser extra type used for the lossless parser implementation.
pub type LosslessParserExtra<'a> = Extra<'a, Token<'a>, TokenKind, char, LimitExceeded>;
/// The error type used for the lossless parser implementation.
pub type LosslessTokenErrors<'a> =
  crate::error::Errors<'a, Token<'a>, TokenKind, char, LimitExceeded>;
