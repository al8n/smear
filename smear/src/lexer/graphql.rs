/// Errors for standard GraphQL lexers
pub mod error;

/// Syntactic tokens for GraphQL - fast lexing that skips trivia.
///
/// This module provides [`SyntacticToken`](syntactic::SyntacticToken), which is optimized for
/// high-performance parsing by automatically filtering out whitespace, comments, and commas.
///
/// **Use this for**: GraphQL servers, query execution, schema compilation, and any
/// performance-critical parsing where you don't need to preserve formatting.
///
/// **Key benefits**:
/// - Minimal memory footprint
/// - Maximum parsing speed
/// - Zero-copy token references
///
/// See [`SyntacticToken`](syntactic::SyntacticToken) for detailed documentation.
pub mod syntactic;

/// Lossless tokens for GraphQL - complete source preservation.
///
/// This module provides [`LosslessToken`](lossless::LosslessToken), which preserves all source
/// information including whitespace, comments, and formatting. Essential for developer tools
/// that need to maintain or manipulate source code without losing information.
///
/// **Use this for**: Code formatters, linters, IDEs, syntax highlighters, documentation tools,
/// and any application that needs perfect source reconstruction.
///
/// **Key benefits**:
/// - Complete source fidelity
/// - Access to all comments and formatting
/// - Build Concrete Syntax Trees (CST)
///
/// See [`LosslessToken`](lossless::LosslessToken) for detailed documentation.
pub mod lossless;

mod handlers;

#[cfg(test)]
mod tests;
