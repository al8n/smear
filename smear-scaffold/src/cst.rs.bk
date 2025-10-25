pub mod graphql;
pub mod graphqlx;

pub use arguments::*;
pub use definitions::*;
pub use directives::*;
pub use field::*;
pub use fragment::*;
pub use input_value::*;
pub use selection_set::*;

mod arguments;
mod definitions;
mod directives;
mod field;
mod fragment;
mod input_value;
mod selection_set;

pub use logosky::cst::*;

/// Common components used across various CST nodes.
pub mod components;

#[cfg(feature = "smallvec")]
type DefaultTriviaContainer<T> = logosky::utils::container::FourOrMore<T>;

#[cfg(not(feature = "smallvec"))]
type DefaultTriviaContainer<T> = std::vec::Vec<T>;
