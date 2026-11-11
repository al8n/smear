//! The model's builder, for tests outside this crate.
//!
//! Behind the `test-support` feature, and hidden even then. `pub` is forced only because `smear`'s
//! integration tests are separate crates: they build small models through the builder the analysis
//! writes its own with. It is not API, and it changes with the builder.

pub use super::model::ModelBuilder;
