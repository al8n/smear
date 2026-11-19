//! `smear/analysis` claims to gate the GraphQLx analyzer.
//!
//! Its twin is not a namesake: `smear-compiler/graphqlx` is the analyzer, and `smear`'s own
//! `graphqlx` is the dialect, which turns on no compiler.
pub use smear::analysis::graphqlx::analyze as gated;
