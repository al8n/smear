/// Parser for Standard GraphQL.
pub mod fast;

/// Lossless parser for Standard GraphQL.
pub mod lossless;

mod float;
mod int;
mod name;
mod string;

pub mod ast {
  pub use super::{float::Float, int::Int, name::Name, string::StringValue};
}
