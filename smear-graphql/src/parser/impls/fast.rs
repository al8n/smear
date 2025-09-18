use logosky::utils::recursion_tracker::RecursionLimitExceeded;

use crate::{
  error::{Errors, Extra},
  lexer::token::fast::{Token, TokenKind},
};

pub use ast::*;

/// The token stream type used for the fast parser implementation.
pub type FastTokenStream<'a> = logosky::TokenStream<'a, Token<'a>>;
/// The parser extra type used for the fast parser implementation.
pub type FastParserExtra<'a, S> = Extra<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
/// The errors type used for the fast parser implementation.
pub type FastTokenErrors<'a, S> = Errors<S, Token<'a>, TokenKind, char, RecursionLimitExceeded>;

mod ast;
