/// Parser for Standard GraphQL.
pub mod fast;

/// Lossless parser for Standard GraphQL.
pub mod lossless;

/// Error types for parser.
pub mod error;

mod name;
mod float;
mod int;
mod string;
