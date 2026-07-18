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
//!
//! [`AssemblyCtx`] takes that one step further for the *materialising*
//! productions: it bundles [`ParseCtx`] with a [`CstEmitter`]
//! on `Ctx::Emitter`, so a production that assembles tree structure carries a
//! single `Ctx: AssemblyCtx<'inp, L, Lang>` in place of the `ParseCtx` +
//! `Ctx::Emitter: CstEmitter` pair. Unlike the aliases above it is *defined*
//! here, not re-exported: it is smear's composition of two tokora bounds.
//! [`Equivalent`] is re-exported as the house
//! cross-flavor comparison trait, so a dispatch can byte-compare a source slice
//! against a spelling (`slice.equivalent("true")`) without an `AsRef<[u8]>`
//! detour that pins the source representation.

mod literal;
mod shape;
mod token;

use tokora::{Lexer, emitter::CstEmitter};

pub use literal::*;
pub use shape::*;
pub use token::*;
pub use tokora::{ComposableEmitter, ErrorOf, ParseCtx, SliceOf, utils::cmp::Equivalent};

/// The context bundle a *materialising* parser production takes.
///
/// Assembly is the act of building the tree. A [`ParseCtx`] already gives an atom
/// the error and diagnostic surface it needs to *parse*; `AssemblyCtx` adds the one
/// further capability the tree-building productions require — a
/// [`CstEmitter`] on `Ctx::Emitter`, so the blessed
/// `node(kind, …)` brackets and the `cst_mark` retro-wraps have somewhere to record
/// structure.
///
/// The capability is a no-op unless the context is driven by a recording CST sink:
/// under a `Fatal`/`Verbose` emitter every CST call compiles to nothing, so the same
/// production set runs tree-lessly at zero cost or records a lossless tree — never a
/// twin. So a production bounded `Ctx: AssemblyCtx<'inp, L, Lang>` collapses the
/// former `Ctx: ParseCtx<…>` + `Ctx::Emitter: CstEmitter<…>` pair into one bound.
///
/// The emitter requirement rides as an associated-type predicate on the [`ParseCtx`]
/// supertrait so it elaborates to callers of the bundle rather than being restated at
/// every use site — the same technique [`ParseCtx`] itself uses to carry
/// [`ComposableEmitter`] onto `Ctx::Emitter`. The blanket impl covers every
/// [`ParseCtx`] whose emitter is a [`CstEmitter`], so the
/// bundle is transparent: nothing implements it by hand.
pub trait AssemblyCtx<'inp, L, Lang: ?Sized = ()>:
  ParseCtx<'inp, L, Lang, Emitter: CstEmitter<'inp, L, Lang>>
where
  L: Lexer<'inp>,
{
}

impl<'inp, L, Lang: ?Sized, C> AssemblyCtx<'inp, L, Lang> for C
where
  L: Lexer<'inp>,
  C: ParseCtx<'inp, L, Lang, Emitter: CstEmitter<'inp, L, Lang>>,
{
}

#[cfg(test)]
mod tests;
