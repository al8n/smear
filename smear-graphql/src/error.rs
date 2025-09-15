pub use lexer::*;
pub use parser::*;

mod lexer;
mod parser;

/// An extra alias
pub type Extra<'a, T, TK, Char = char, StateError = ()> =
  chumsky::extra::Err<Errors<'a, T, TK, Char, StateError>>;
