use logosky::{
  TokenStream,
  utils::{Span, tracker::LimitExceeded},
};

use crate::{
  error::Extra,
  lexer::token::lossless::{Token, TokenKind},
};

mod float;
mod int;
mod name;
mod string;

pub type LosslessTokenStream<'a> = TokenStream<'a, Token<'a>>;
pub type LosslessParserExtra<'a> = Extra<Token<'a>, TokenKind, char, LimitExceeded>;
