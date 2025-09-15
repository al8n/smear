use logosky::utils::{Span, recursion_tracker::RecursionLimitExceeded};

use crate::{
  error::Extra,
  lexer::token::fast::{Token, TokenKind},
};

mod enum_value;
mod float;
mod int;
mod list;
mod name;
mod punctuator;
mod string;

pub type FastTokenStream<'a> = logosky::TokenStream<'a, Token<'a>>;
pub type FastParserExtra<'a> = Extra<'a, Token<'a>, TokenKind, char, RecursionLimitExceeded>;
