use logosky::utils::recursion_tracker::RecursionLimitExceeded;

use crate::{
  error::{Error, Errors, Extra},
  lexer::token::fast::{Token, TokenKind},
};

pub use ast::*;

/// The token stream type used for the fast parser implementation.
pub type FastTokenStream<'a> = logosky::TokenStream<'a, Token<'a>>;
/// The parser extra type used for the fast parser implementation.
pub type FastParserExtra<'a, S> = Extra<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The error type used for the fast parser implementation.
pub type FastTokenError<'a, S> = Error<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the fast parser implementation.
pub type FastTokenErrors<'a, S> = Errors<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The token type used for the fast parser implementation.
pub type FastToken<'a> = Token<'a>;
/// The token kind type used for the fast parser implementation.
pub type FastTokenKind = TokenKind;

mod ast;

mod error;
