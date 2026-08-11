//! Argument and directive productions: `Argument`, `Arguments`, `Directive`, `Directives`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # The one divergence, and it is in every tree
//!
//! **A directive's name is a [`TypePath`](K::TypePath), not a `Name`** (divergence 10,
//! `graphqlx/syntactic/directive/mod.rs:81-107`). `@ns::cache<Int>` parses, `@::ns::d` parses, and
//! even `@deprecated` nests `Directive > TypePath > Path`. A port of GraphQL's
//! `expect(Identifier)` would build `Directive` with a bare `Name` token child — a tree that
//! re-prints its source byte for byte and that no consumer can navigate, which is why
//! `a_directive_name_is_a_type_path` asserts the nesting of the *one-segment* spelling and not
//! only of the qualified one.
//!
//! What did **not** widen: an `Argument`'s key is still a plain `Name`
//! (`graphqlx/syntactic/argument/mod.rs`'s `take_argument_name`), exactly as an `ObjectField`'s
//! is. Only the value side and the directive's own name moved.
//!
//! # An absent optional shape leaves no node behind
//!
//! `Directives` is a collection, and a collection with no members must not be a node — a typed
//! `directives()` answering `Some(<empty>)` where the source says nothing is a distinction the
//! layer above cannot undo, and a node opened before the head peek would swallow the whitespace in
//! front of whatever follows it. `Arguments` is not in that class: it is delimited, so `()` is a
//! real, written-down empty list and gets its node. Both rulings are GraphQL's twin's, and
//! GraphQLx's `syntactic/` sibling agrees with each — its `arguments` has no `at_least(1)`
//! (`argument/mod.rs:175-181`) and its `directives` returns a zero-width empty collection without
//! consuming (`directive/mod.rs:267-273`).
//!
//! # An undelimited repetition ends with its trailing trivia inside it
//!
//! `Directives`' terminating peek must cross the trailing trivia to learn that no further `@`
//! follows, and it crosses it while the node is still open. A node's extent is `[mark, now]` and
//! `cst_finish` has no `_at` form, so this is forced rather than chosen. Text fidelity is
//! untouched; only the node's own `text_range` is one trivia run longer.

use crate::lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::parser::graphqlx::kinds::SyntaxKind as K;

// `node` comes from `coverage`, not from `tokora::parser`. Behind `feature = "lossless-coverage"`
// it is that same combinator plus the per-node-kind hit counter gate 2 measures its reach with, so
// a production cannot open a node without being counted; without the feature it is tokora's own,
// re-exported unchanged.
use super::coverage::node;

use super::{
  GraphqlxLosslessInput, recover,
  recover::{ARGUMENT_HEADS, opener_span},
  trivia::{eat_if, expect, peek_kind},
  ty::type_path,
  value::{Constness, value},
};

use crate::parser::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `Name : Value` — `Name : Value[Const]` when `konst` says so.
  ///
  /// One production for both spellings, the flavour riding in as an argument rather than forking
  /// the body; `value.rs`'s module docs give the reasoning, and note there that `syntactic/` forks
  /// (`argument` vs `const_argument`) because its two productions return different *types*, which
  /// is not a difference this suite has.
  fn argument<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::Argument.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// `( Argument* )`
  ///
  /// `object_value`'s loop with parentheses for braces, and the head check is load-bearing for the
  /// same reason: routing a non-`Identifier` head into `argument`'s own `expect` would return
  /// `Err` and abort the whole list, so `(a: 1, !, b: 2)` would cost the rest of the parse instead
  /// of one token.
  fn arguments<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::Arguments.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
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

  /// `@ TypePath Arguments?`
  ///
  /// The name is [`type_path`], which is the whole of divergence 10; the argument list is
  /// dispatched on a peek rather than attempted, so its `(` is consumed *inside* the `Arguments`
  /// node and an absent list opens nothing.
  fn directive<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::Directive.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::At)?;
        type_path::<Src, Ctx>(inp)?;
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
  /// The loop terminates because every iteration begins with a peek for `@` and [`directive`]
  /// consumes that `@` before it can fail; a `directive` that cannot complete returns `Err` rather
  /// than looping, since a malformed directive is not something to resynchronise past here — the
  /// caller decides that.
  ///
  /// # `konst` is the caller's, and the two flavours are not interchangeable
  ///
  /// The split is **not** "SDL versus executable": a `VariableDefinition`'s directives are const
  /// too, in the middle of an executable document (`graphqlx/syntactic/executable/mod.rs:316`
  /// calls `const_directives` where its operation and fragment definitions call plain
  /// `directives`). Each call site names its own flavour and `syntactic/` is the reference for
  /// every one of them.
  fn directives<'inp, Src, Ctx>(inp, konst: Constness) {
    // The head peek happens OUTSIDE the node, so the trivia it crosses lands in the enclosing node
    // and a source with no directive at all opens nothing.
    if peek_kind::<Src, Ctx>(inp)? != Some(Kind::At) {
      return Ok(());
    }
    node(
      K::Directives.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
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
  dialect = graphqlx::lossless;

  /// Drivers that run one argument or directive production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_x_selection.rs`.
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
