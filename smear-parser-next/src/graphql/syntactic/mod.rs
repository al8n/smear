//! The GraphQL dialect's grammar productions.
//!
//! Each production is a free fn generic over `L: Lexer<'inp>` — bounded only by
//! tokora capability traits and the [`ParseCtx`](crate::combinator::ParseCtx)
//! bundle — with the `Lang` type parameter pinned to the
//! [`GraphQL`](super::GraphQL) marker and `Span = SimpleSpan`. The productions
//! assemble the [atom layer](crate::combinator) into the dialect's typed AST nodes.
//!
//! The module name reflects the driving lexer the syntactic suite pairs these
//! productions with; the productions themselves are purely syntactic — lossless/CST
//! structure is a separate `lossless` module's concern (a later wave).

pub mod argument;
pub mod directive;
pub mod ty;
pub mod value;
