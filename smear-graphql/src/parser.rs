/// Parser for Standard GraphQL.
pub mod fast;

/// Lossless parser for Standard GraphQL.
pub mod lossless;

mod enum_value;
mod float;
mod int;
mod list;
mod name;
mod punctuator;
mod string;

pub mod ast {
  pub use super::{
    enum_value::EnumValue,
    float::Float,
    int::Int,
    list::List,
    name::Name,
    punctuator::{LBracket, RBracket},
    string::StringValue,
  };
}
