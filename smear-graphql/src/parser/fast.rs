use logosky::utils::{Span, recursion_tracker::RecursionLimitExceeded};

use crate::{
  error::Extra,
  lexer::token::fast::{Token, TokenKind},
};

mod float;
mod int;
mod name;
mod string;

pub type FastTokenStream<'a> = logosky::TokenStream<'a, Token<'a>>;
pub type FastParserExtra<'a> = Extra<Token<'a>, TokenKind, char, RecursionLimitExceeded>;
