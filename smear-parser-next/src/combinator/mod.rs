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
//! - the aliases and bundles defined here ([`SliceOf`], [`ErrorOf`],
//!   [`ComposableEmitter`], [`ParseCtx`]).
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

use smear_lexer::tokora::{
  Emitter, Lexer, ParseContext, Source,
  emitter::{
    FullContainerEmitter, SeparatedEmitter, TooFewEmitter, UnexpectedLeadingSeparatorEmitter,
    UnexpectedTrailingSeparatorEmitter,
  },
};

/// The slice type lexer `L` yields from its source.
pub type SliceOf<'inp, L> =
  <<L as Lexer<'inp>>::Source as Source<<L as Lexer<'inp>>::Offset>>::Slice<'inp>;

/// The error type context `Ctx`'s emitter produces.
pub type ErrorOf<'inp, L, Ctx, Lang> =
  <<Ctx as ParseContext<'inp, L, Lang>>::Emitter as Emitter<'inp, L, Lang>>::Error;

/// Everything the shape atoms require from an emitter, as one bound.
///
/// Blanket-implemented for every emitter that satisfies the whole family, so a
/// bound of `E: ComposableEmitter<'inp, L, Lang>` is interchangeable with
/// spelling out all six sub-traits.
pub trait ComposableEmitter<'inp, L, Lang: ?Sized = ()>:
  Emitter<'inp, L, Lang>
  + FullContainerEmitter<'inp, L, Lang>
  + SeparatedEmitter<'inp, L, Lang>
  + UnexpectedLeadingSeparatorEmitter<'inp, L, Lang>
  + UnexpectedTrailingSeparatorEmitter<'inp, L, Lang>
  + TooFewEmitter<'inp, L, Lang>
where
  L: Lexer<'inp>,
{
}

impl<'inp, L, Lang: ?Sized, T> ComposableEmitter<'inp, L, Lang> for T
where
  L: Lexer<'inp>,
  T: Emitter<'inp, L, Lang>
    + FullContainerEmitter<'inp, L, Lang>
    + SeparatedEmitter<'inp, L, Lang>
    + UnexpectedLeadingSeparatorEmitter<'inp, L, Lang>
    + UnexpectedTrailingSeparatorEmitter<'inp, L, Lang>
    + TooFewEmitter<'inp, L, Lang>,
{
}

/// The context bundle every atom takes.
///
/// Implemented for every [`ParseContext`] whose emitter is a
/// [`ComposableEmitter`] and whose source slice is [`Clone`], so an atom needs
/// only `Ctx: ParseCtx<'inp, L>` to unlock the entire emitter surface. The
/// emitter requirement rides on the [`ParseContext`] supertrait as an
/// associated-type bound so it elaborates to callers of the bundle rather than
/// having to be restated at every use site; the `SliceOf<'inp, L>: Clone`
/// requirement lives on the blanket impl (a projection bound cannot be a
/// supertrait), so it gates which contexts qualify without forcing that clause
/// onto every mention of the bound. Atoms that clone a slice restate that one
/// bound locally.
pub trait ParseCtx<'inp, L, Lang: ?Sized = ()>:
  ParseContext<'inp, L, Lang, Emitter: ComposableEmitter<'inp, L, Lang>>
where
  L: Lexer<'inp>,
{
}

impl<'inp, L, Lang: ?Sized, T> ParseCtx<'inp, L, Lang> for T
where
  L: Lexer<'inp>,
  SliceOf<'inp, L>: Clone,
  T: ParseContext<'inp, L, Lang, Emitter: ComposableEmitter<'inp, L, Lang>>,
{
}

#[cfg(test)]
mod tests;
