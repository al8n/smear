use chumsky::{
  extra::ParserExtra,
  input::StrInput,
  label::LabelError,
  prelude::*,
  span::Span,
  text::{Char, TextExpected},
  util::MaybeRef,
};

use crate::parser::{
  punct::{LBrace, LBracket, Quote, RBrace, RBracket, TripleQuote},
  Name, SmearChar, Spanned,
};

use super::ignored;

/// GraphQL input value and const input value parsers
pub mod graphql;

/// Groto input value and const input value parsers
pub mod groto;

pub use angle::*;
pub use boolean::*;
pub use enum_::*;
pub use float::*;
pub use int::*;
pub use list::*;
pub use map::*;
pub use null::*;
pub use object::*;
pub use set::*;
pub use string::*;
pub use variable::*;

mod angle;
mod boolean;
mod enum_;
mod float;
mod int;
mod list;
mod map;
mod null;
mod object;
mod set;
mod string;
mod variable;
