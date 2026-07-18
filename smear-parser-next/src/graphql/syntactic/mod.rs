//! The GraphQL dialect's grammar productions.
//!
//! Each production is a free fn generic over `L: Lexer<'inp>` — bounded only by
//! tokora capability traits and the [`ParseCtx`](crate::combinator::ParseCtx)
//! bundle — with the `Lang` type parameter pinned to the
//! [`GraphQL`](super::GraphQL) marker and `Span = SimpleSpan`. The productions
//! assemble the [atom layer](crate::combinator) into the dialect's typed AST nodes
//! and, at every node boundary, record a `node(kind)` event that is a no-op on the
//! syntactic (`Fatal`/`Verbose`) emitters and materializes a lossless CST node on a
//! recording sink — the same production set, never a twin.
//!
//! The module name reflects the driving lexer the syntactic suite pairs these
//! productions with; the lossless suite (a later wave) drives the *same* fns over a
//! trivia-surfacing lexer and a `cst::Sink`, swapping `L` and the emitter alone.

pub mod value;
