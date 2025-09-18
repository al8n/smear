use logosky::utils::Span;

use crate::lexer::token::fast::{Token, TokenKind};

use super::{FastTokenErrors, FastTokenStream};

pub use argument::*;
pub use directives::*;
pub use field::*;
pub use fragment::*;
pub use list::*;
pub use object::*;
pub use selection_set::*;
pub use value::*;

mod argument;
mod boolean_value;
mod directives;
mod enum_value;
mod field;
mod float;
mod fragment;
mod int;
mod keyword;
mod list;
mod name;
mod null_value;
mod object;
mod punctuator;
mod selection_set;
mod string;
mod value;
mod variable;
