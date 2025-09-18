pub use lexer::*;
pub use parser::*;

mod lexer;
mod parser;

/// An extra alias
pub type Extra<S, T, TK, Char = char, StateError = ()> =
  chumsky::extra::Err<Errors<S, T, TK, Char, StateError>>;
