//! The combinator substrate: the generic vocabulary every parser atom is built from.
//!
//! # The Lego rule
//!
//! Atoms are Lego bricks. A brick knows the shape of its studs, never the colour
//! of the wall it ends up in. Concretely, code in this module and every atom
//! layered on top of it speaks only the *generic* vocabulary:
//!
//! - `L: Lexer<'inp>` and the token *capability* traits it satisfies,
//! - the language marker `Lang`,
//! - the aliases and bundles re-exported here ([`SliceOf`], [`ErrorOf`],
//!   [`ComposableEmitter`], [`ParseCtx`]) — promoted to tokora 0.2.0, where the
//!   definitions now live.
//!
//! It never names a concrete `SyntacticToken`, a concrete `SyntacticLexer`, or a
//! dialect error type. Those are studs the *consumer* plugs in when it picks a
//! lexer and an error sink; keeping them out of the atoms is what lets one atom
//! serve GraphQL and GraphQL-like dialects over `str` and `[u8]` alike.
//!
//! [`SliceOf`] and [`ErrorOf`] name the two projections atoms reach for
//! constantly — the source slice a lexer yields and the error its context
//! emits — without spelling out the nested associated-type paths each time.
//! [`ComposableEmitter`] and [`ParseCtx`] collapse the family of emitter
//! sub-traits and the context requirements into a single bound apiece, so an
//! atom's signature carries one `Ctx: ParseCtx<'inp, L>` instead of the whole
//! ladder.

mod literal;
mod token;

pub use literal::*;
pub use token::*;
pub use tokora::{ComposableEmitter, ErrorOf, ParseCtx, SliceOf};

#[cfg(test)]
mod tests;
