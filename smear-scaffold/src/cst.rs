//! Concrete Syntax Tree (CST) structures for smear-scaffold
//!
//! CST preserves all source information including whitespace, comments, and formatting.
//! Unlike AST which only preserves semantic structure, CST maintains complete fidelity
//! to the original source code, enabling:
//!
//! - Lossless round-tripping (parse and reconstruct identical source)
//! - Code formatting and pretty-printing
//! - Source code analysis and refactoring
//! - IDE features like syntax highlighting and error reporting
//!
//! CST uses `LosslessToken` instead of `SyntacticToken` to preserve all trivia.

mod and;
pub mod definitions;
pub mod lang;
pub mod trivia;

pub use and::*;
pub use definitions::*;
pub use lang::*;
pub use trivia::*;
