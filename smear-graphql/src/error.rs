pub use lexer::*;
use logosky::Token;
pub use parser::*;

mod lexer;
mod parser;

/// An extra alias
pub type Extra<T, TK, Char = char, StateError = ()> =
  chumsky::extra::Err<Errors<T, TK, Char, StateError>>;
