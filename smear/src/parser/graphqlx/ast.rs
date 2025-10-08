use logosky::utils::recursion_tracker::RecursionLimitExceeded;

pub use directive::*;
pub use fragment::*;
pub use import::*;
pub use path::*;
pub use raw::*;
pub use ty::*;
pub use value::*;

use super::error::{Error, Errors, Extra};
use crate::{
  lexer::graphqlx::ast::{AstToken, AstTokenChar, AstTokenKind},
  parser::ident::Ident,
};

mod directive;
mod error;
mod fragment;
mod ident;
mod import;
mod keyword;
mod location;
mod path;
mod punctuator;
mod raw;
mod ty;
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

