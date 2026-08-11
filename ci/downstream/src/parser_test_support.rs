//! `smear/test-support` claims to gate the lossless suites` scaffolding.
//!
//! Three levels down, which is the other thing a facade cannot reach: it is inside
//! `parser::graphql::lossless::runner`, so gating it at the crate root would not do.
pub use smear::parser::graphql::lossless::runner::test_support as gated;
