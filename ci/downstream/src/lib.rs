//! A consumer of `smear`, written the way a consumer writes one.
//!
//! Each module below names one path that a `smear` feature claims to gate, and is compiled by that
//! pair's `uses-` feature. Nothing in it is gated on the *smear* feature that makes the path exist —
//! that is the whole point. `ci/downstream.sh` compiles each module three times, composing the flags
//! on the command line every time, and requires: fail without the feature, compile with
//! `smear/<f>`, and fail again when the capability is asked for through the MEMBER instead.
//!
//! The third leg is the finding. Before `smear`'s equivalence assertion, all ten of these compiled
//! with the member's feature and smear's own switched off — a consumer got a capability it never
//! enabled because some other crate in its graph named the member directly.
//!
//! One module per pair rather than one file naming everything: a bundled fixture proves the bundle
//! and not the parts. A break in `smear/graphqlx` must not be maskable by `smear/graphql` working.
//!
//! There is no `#[test]` here and there should not be. The property is "a consumer who did not
//! enable the feature cannot reach this", and `cargo build`'s exit code answers it exactly.

#[cfg(feature = "uses-compiler-introspection")]
pub mod compiler_introspection;
#[cfg(feature = "uses-compiler-rowan")]
pub mod compiler_rowan;
#[cfg(feature = "uses-lexer-bytes")]
pub mod lexer_bytes;
#[cfg(feature = "uses-lexer-graphql")]
pub mod lexer_graphql;
#[cfg(feature = "uses-lexer-graphqlx")]
pub mod lexer_graphqlx;
#[cfg(feature = "uses-parser-graphql")]
pub mod parser_graphql;
#[cfg(feature = "uses-parser-graphqlx")]
pub mod parser_graphqlx;
#[cfg(feature = "uses-parser-rowan")]
pub mod parser_rowan;
#[cfg(feature = "uses-parser-test-support")]
pub mod parser_test_support;
#[cfg(feature = "uses-schema-introspection")]
pub mod schema_introspection;

/// The path that needs no feature at all, so a run with every case switched off still compiles
/// something and the fixture cannot rot into an empty crate.
///
/// `smear::diagnostic` is ungated in every configuration — it is the vocabulary every error family
/// answers — so a consumer gets it with nothing switched on.
pub fn always_available() {
  fn is_code(_: smear::diagnostic::Severity) {}
  is_code(smear::diagnostic::Severity::Error);
}
