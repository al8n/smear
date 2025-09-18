use logosky::utils::Span;

use crate::lexer::token::fast::{Token, TokenKind};

use super::{FastTokenErrors, FastTokenStream};

pub use argument::*;
pub use directives::*;
pub use field::*;
pub use fragment::*;
pub use selection_set::*;
pub use list::*;
pub use object::*;
pub use value::*;

mod argument;
mod boolean_value;
mod directives;
mod enum_value;
mod float;
mod fragment;
mod field;
mod int;
mod list;
mod name;
mod null_value;
mod object;
mod punctuator;
mod keyword;
mod string;
mod value;
mod variable;
mod selection_set;
