//! Argument and directive productions: `Argument`, `Arguments`, `Directive`, `Directives`.
//!
//! The conventions are `value.rs`'s and are not repeated here — the two kind spaces, the
//! `::<Src, Ctx>` on every generic call, and the fully spelled `node(…)` closure parameter.
//!
//! # An absent optional shape leaves no node behind
//!
//! `Directives` is a *collection*, and a collection with no members must not be a node. Two
//! reasons, and the second is the one that would be found late:
//!
//! - A typed `directives()` accessor answering `Some(<empty>)` where the source says nothing is
//!   a distinction the layer above cannot undo.
//! - Every atom commits leading trivia before it decides anything, so a node opened *before*
//!   the head peek would swallow the whitespace in front of whatever follows it. Opening the
//!   node only once an `@` has been seen keeps that trivia in the enclosing node, where the
//!   source puts it. Pinned by `no_directive_means_no_directives_node`.
//!
//! `Arguments` is not in that class: it is delimited, so `()` is a real, written-down empty
//! list and gets its node.
//!
//! # An undelimited repetition ends with its trailing trivia inside it
//!
//! `Directives` is the suite's first repetition with no closing delimiter, and it exposes a law
//! every later one inherits (Task 8's `ImplementsInterfaces`, `UnionMemberTypes`,
//! `DirectiveLocations` and `Document` are all in the class). The loop's terminating peek must
//! cross the trailing trivia to learn that no further `@` follows, and it crosses it while the
//! node is still open — so that trivia lands **inside** `Directives`.
//!
//! **This is forced, not chosen.** A node's extent is `[mark, now]`: `cst_start_at` takes a
//! mark but `cst_finish` has no `_at` form, so a node always ends at the current position, and
//! the only way to end it earlier would be an unbounded non-committing lookahead — which the
//! atom set deliberately does not have, since a peek that crossed trivia without committing it
//! would leave the tree's ordering to a second buffering layer. Text fidelity is untouched; only
//! the node's own `text_range` is one trivia run longer than the directives it contains. Pinned
//! by `no_directive_means_no_directives_node`, which asserts the node's text on both sides.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/argument.rs` and `grammar/directive.rs` agree on the shape of all
//! four productions. They differ in three places:
//!
//! - **`()` is accepted.** apollo reports "expected an Argument" for an empty list, following
//!   the spec's `Arguments: ( Argument+ )`. This suite's own `syntactic/` sibling documents the
//!   lenient empty spelling as accepted, and gate 1 compares the two suites' verdicts input by
//!   input — so lossless must agree with syntactic, not with apollo. Emptiness is a validation
//!   rule over the tree, and the tree records it faithfully either way.
//! - **`directives` opens no node when there is no `@`.** apollo's `directives` starts its
//!   `DIRECTIVES` node before peeking, so a field with no directives still gets an empty one.
//!   See above for why that is not free here.
//! - **Junk inside an argument list is attributed to an `Error` node**, and a garbage *run* is
//!   skipped as one nesting-aware region — the same two divergences `value.rs` records for a
//!   list value, for the same reasons.

use smear_lexer::graphql::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::node;

use super::{
  GraphqlLosslessInput, recover,
  recover::{ARGUMENT_HEADS, opener_span},
  trivia::{eat_if, expect, peek_kind},
  value::{Constness, value},
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphql::lossless;

  /// `Name : Value` — `Name : Value[Const]` when `konst` says so.
  ///
  /// One production for both the executable and the constant spelling, the flavour riding in as
  /// an argument rather than forking the body; `value.rs`'s module docs give the reasoning, and
  /// note there that `syntactic/` forks (`argument` vs `const_argument`) because its two
  /// productions return different *types*, which is not a difference this suite has.
  fn argument<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::Argument.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// `( Argument* )`
  ///
  /// `object_value`'s loop with parentheses for braces, and the head check is load-bearing for
  /// the same reason: routing a non-`Identifier` head into `argument`'s own `expect` would
  /// return `Err` and abort the whole list, so `(a: 1, !, b: 2)` would cost the rest of the
  /// parse instead of one token.
  fn arguments<'inp, Src, Ctx>(inp, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::Arguments.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => argument::<Src, Ctx>(inp, konst)?,
            Some(_) => recover::unexpected::<Src, Ctx>(inp, ARGUMENT_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `@ Name Arguments?`
  ///
  /// The argument list is dispatched on a peek rather than attempted, so its `(` is consumed
  /// *inside* the `Arguments` node and an absent list opens nothing.
  fn directive<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::Directive.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::At)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          arguments::<Src, Ctx>(inp, konst)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Directive*` — one node over the whole run, and **no node at all** when the run is empty.
  ///
  /// The loop terminates because every iteration begins with a peek for `@` and `directive`
  /// consumes that `@` before it can fail; a `directive` that cannot complete returns `Err`
  /// rather than looping, since a malformed directive is not something to resynchronise past
  /// here — the caller decides that.
  ///
  /// # `konst` is the caller's, and the two flavours are not interchangeable
  ///
  /// The spec parameterises `Directives[Const]`, and the split is **not** "SDL versus
  /// executable": a `VariableDefinition`'s directives are const too, in the middle of an
  /// executable document. Each call site names its own flavour and `syntactic/` is the
  /// reference for every one of them (`optional_const_directives` throughout `definition/`
  /// and `extension.rs`, `const_directives` in `variable_definition`, plain `directives` in
  /// `operation_definition`, `fragment_definition` and all four selection positions).
  fn directives<'inp, Src, Ctx>(inp, konst: Constness) {
    // The head peek happens OUTSIDE the node, so the trivia it crosses lands in the enclosing
    // node and a source with no directive at all opens nothing.
    if peek_kind::<Src, Ctx>(inp)? != Some(Kind::At) {
      return Ok(());
    }
    node(
      K::Directives.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        while peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
          directive::<Src, Ctx>(inp, konst)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }
}

lossless_drivers! {
  dialect = graphql::lossless;

  /// Drivers that run one argument or directive production over a `&str` and hand back the tree
  /// it built, for `tests/lossless_ty_directive.rs`.
  mod test_support;

  /// `super::argument` over `src`, in an ordinary (non-const) position.
  fn parse_argument => argument [Constness::NonConst];

  /// `super::arguments` over `src`, in an ordinary (non-const) position.
  fn parse_arguments => arguments [Constness::NonConst];

  /// `super::directive` over `src`, in an ordinary (non-const) position.
  fn parse_directive => directive [Constness::NonConst];

  /// `super::directives` over `src` — the entry every executable directive position uses.
  fn parse_directives => directives [Constness::NonConst];

  /// `super::directives` over `src` in a **const** position — the entry every SDL directive
  /// position uses, and a `VariableDefinition`'s.
  fn parse_const_directives => directives [Constness::Const];
}
