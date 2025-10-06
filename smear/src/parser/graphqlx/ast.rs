use logosky::utils::recursion_tracker::RecursionLimitExceeded;

pub use declare_generic::*;
pub use import::*;
pub use path::*;
pub use ty::*;
pub use type_generic::*;
pub use value::*;

use super::error::{Error, Errors, Extra};
use crate::lexer::graphqlx::ast::{AstToken, AstTokenChar, AstTokenKind};

mod declare_generic;
mod import;
mod keyword;
mod location;
mod path;
mod punctuator;
mod ty;
mod type_generic;
mod value;

/// The default container type used for collections in the AST.
pub type DefaultVec<T> = Vec<T>;

/// The token stream type used for the AST parser implementation.
pub type AstTokenStream<'a, S> = logosky::TokenStream<'a, AstToken<S>>;
/// The parser extra type used for the AST parser implementation.
pub type AstParserExtra<'a, S> =
  Extra<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The error type used for the AST parser implementation.
pub type AstTokenError<'a, S> =
  Error<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
/// The errors type used for the AST parser implementation.
pub type AstTokenErrors<'a, S> =
  Errors<S, AstToken<S>, AstTokenKind, AstTokenChar<'a, S>, RecursionLimitExceeded>;
