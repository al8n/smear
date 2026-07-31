//! Document-level productions. Task 8 fills in the rest.

use tokora::{Lexer, lexer::FromLogos};

use crate::graphql::GraphQL;

/// Drive a whole document. Task 8 replaces the body.
///
/// **Why the stub consumes instead of doing nothing.** The obvious stub — `Ok(())`, committing
/// nothing — cannot produce a tree over a non-empty source, because gap tiling is *not*
/// unconditional. `finish` tiles a gap only where a recorded **lexer-error** diagnostic
/// explains it; a gap covered by neither a committed token nor such a diagnostic is the
/// signature of a dropped committed token, and is refused as
/// `FinishError::UncoveredGap` (`tokora/src/cst/sink/finish.rs:22-32`, `:156`). A second wall,
/// `StructureWithoutTokens`, refuses structure with zero committed tokens over a non-empty
/// source outright. So a no-op production yields `UncoveredGap { start: 0, end: len }`, not a
/// tiled tree.
///
/// Committing the whole source instead makes the round-trip hold through the **token** channel,
/// which is the stronger of the two guarantees: every byte reaches the tree as a real token
/// rather than as filler — and, since Task 3b, as a real *kind* too: every token committed here
/// is classified by [`super::kind_map::token_kind`]. What the tree still lacks is structure,
/// which Task 8 adds.
pub(crate) fn document<'inp, Src, Ctx>(
  inp: &mut super::GraphqlLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<(), super::GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: tokora::Source<usize> + ?Sized,
  // `LogosLexer<'inp, T>` carries `T: FromLogos<'inp>` on its **struct definition**
  // (`tokora/src/lexer/logos/mod.rs:51`), so this bound is what makes the lexer alias nameable
  // at all — a `Lexer<'inp>` bound alone leaves the type ill-formed. `syntactic/` never needs
  // it because `SyntacticLexer` hides the `LogosLexer` behind its own alias over `Src`.
  super::GraphqlLosslessToken<'inp, Src>: FromLogos<'inp>,
  super::GraphqlLosslessLexer<'inp, Src>: Lexer<'inp>,
  Ctx: tokora::ParseContext<'inp, super::GraphqlLosslessLexer<'inp, Src>, GraphQL>,
{
  // Task 8: open Document, drive definitions, recover at top level.
  inp.skip_while(|_| true)
}
