use logosky::utils::Span;

use crate::lexer::token::fast::{Token, TokenKind};

use super::{FastTokenErrors, FastTokenStream};

pub use argument::*;
pub use list::*;
pub use object::*;
pub use value::*;

mod argument;
mod boolean_value;
mod enum_value;
mod float;
mod int;
mod list;
mod name;
mod null_value;
mod object;
mod punctuator;
mod string;
mod value;
mod variable;
