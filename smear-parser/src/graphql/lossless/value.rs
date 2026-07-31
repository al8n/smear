//! Value productions. Each opens exactly one node; leaf tokens flow in on their own via the
//! sink's committed-token path, so no production wraps a bare token in a node.
//!
//! # Two kind spaces, deliberately spelled differently
//!
//! The node kinds are [`SyntaxKind`](crate::graphql::kinds::SyntaxKind) (`K::…`), the tree's
//! space; every `expect(…)` argument is a `LosslessTokenKind` (`Kind::…`), the lexer's. They
//! are different enums, and their names disagree on purpose in three places: the node kind for
//! a name is `K::Name` while the *token* kind is `Kind::Identifier`; a single-line string is
//! `K::String` but `Kind::InlineString`; and `=` is `K::Equal` and `Kind::Equal` — the one that
//! agrees. `super::kind_map` is the only module that speaks both.
//!
//! # Every generic call in this file carries `::<Src, Ctx>`, and must
//!
//! `Src` is **not inferable** from an argument. A production's only parameter is a
//! `GraphqlLosslessInput<'inp, '_, Src, Ctx>`, which expands through
//! `GraphqlLosslessLexer<'inp, Src> = LosslessLexer<'inp, Src::Slice<'inp>>` — an associated-type
//! projection, so knowing the input type does not recover `Src` (`str` and `&str` both project
//! `&'inp str`, which is the concrete instance of the same ambiguity `runner::parse_str`
//! documents). Every call to an atom, a sibling production or a recovery helper therefore names
//! both parameters explicitly. Dropping one is an `E0283`, not a silent mis-instantiation.
//!
//! # Where a value's leading trivia lands
//!
//! Every atom commits leading trivia *before* it decides anything, and a committed token lands
//! in the innermost node open at its commit. So the whitespace in `[ 1 ]` between `[` and `1`
//! is inside the `ListValue` (the list node was opened first), while the whitespace before the
//! `[` in `  [1]` is outside it — the dispatcher crossed that trivia before `list_value` ran.
//! That is the placement a formatter wants and it falls out of the sink; nothing here arranges
//! it.
//!
//! # Divergences from `apollo-parser`, decided rather than inherited
//!
//! `apollo-parser`'s `grammar/value.rs` is the closest comparable implementation, and this file
//! agrees with it on the shape of every production. It differs in three places:
//!
//! - **Garbage inside a list is attributed to an `Error` node.** apollo's `err_and_pop` drops
//!   the offending token into whatever node is open, so its tree records that a token was there
//!   but not that it was rejected. A lossless tree can afford the node, and a consumer that
//!   wants to grey out the bad region needs it.
//! - **A garbage *run* is skipped as one region**, nesting-aware, where apollo pops one token
//!   per loop turn. One diagnostic per hole reads better than one per token, and
//!   `sync_balanced` gives it for free.
//! - **A missing `:` in an object field is an error.** apollo's `object_field` parses the name
//!   and then consumes a `:` only `if let Some(T![:])`, so `{a}` parses clean there. The spec
//!   has no such production.
//!
//! It also matches apollo on one point worth stating because it reads like an oversight: `$ x`
//! — a variable with trivia between the `$` and the name — is **accepted**. Ignored tokens may
//! appear between any two lexical tokens, and neither implementation carries the adjacency
//! machinery to forbid it in this one position.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{ParseInput as _, SimpleSpan, parser::node, utils::DowncastRef as _};

/// What `DowncastRef` answers for a peeked token: the outer `Option` is the peek (`None` at
/// end of input), the inner one is the downcast (`None` for a name that is not a reserved
/// spelling). Named so the `peek_head_map` call site can annotate the downcast's target type
/// without spelling the trait path.
type PeekedKeyword = Option<Option<ContextualKeyword>>;

use crate::graphql::kinds::SyntaxKind as K;

use super::{
  GraphqlLosslessInput, recover,
  recover::{OBJECT_FIELD_HEADS, VALUE_HEADS},
  trivia::{eat_if, expect, peek_kind},
};

/// The span of the single-byte delimiter `expect` has just committed.
///
/// `expect` reports only whether it matched, so the opener's own span has to be recovered from
/// the input's committed extent: its end is the delimiter's end, and every delimiter this file
/// opens (`[`, `{`) is exactly one byte. That span is what the unclosed-delimiter diagnostic
/// points at — the opener that was never closed, not the end of input where the absence was
/// noticed.
#[inline]
fn just_committed_delimiter(end: usize) -> SimpleSpan {
  SimpleSpan::new(end.saturating_sub(1), end)
}

lossless_production! {
  /// `$ Name`
  fn variable<'inp, Src, Ctx>(inp) {
    node(
      K::Variable.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Dollar)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `IntValue`
  fn int_value<'inp, Src, Ctx>(inp) {
    node(
      K::IntValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| expect::<Src, Ctx>(inp, Kind::Int),
    )
    .parse_input(inp)
  }

  /// `FloatValue`
  fn float_value<'inp, Src, Ctx>(inp) {
    node(
      K::FloatValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| expect::<Src, Ctx>(inp, Kind::Float),
    )
    .parse_input(inp)
  }

  /// `StringValue` — one node kind over two token kinds.
  ///
  /// The block/inline distinction is the *token*'s, and it survives on the token in the tree;
  /// the node above it is `StringValue` either way, because every consumer that cares reads
  /// the token, and every consumer that does not would otherwise have to match two node kinds
  /// for one grammar production.
  fn string_value<'inp, Src, Ctx>(inp) {
    node(
      K::StringValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if eat_if::<Src, Ctx>(inp, Kind::InlineString)? {
          return Ok(());
        }
        expect::<Src, Ctx>(inp, Kind::BlockString)
      },
    )
    .parse_input(inp)
  }

  /// `true` | `false`
  ///
  /// **Precondition: the head is an `Identifier` spelled `true` or `false`.** The spelling is
  /// decided once, by `value`, and is not re-checked here — a second `downcast_ref` would read
  /// the same token again to reach the same answer. Callers other than `value` owe the same
  /// check.
  fn boolean_value<'inp, Src, Ctx>(inp) {
    node(
      K::BooleanValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `null`
  ///
  /// Precondition as for `boolean_value`: the head is an `Identifier` spelled `null`.
  fn null_value<'inp, Src, Ctx>(inp) {
    node(
      K::NullValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `Name` *but not* `true`, `false` or `null`
  ///
  /// Precondition as for `boolean_value`, negated: the head is an `Identifier` spelled as none
  /// of the three reserved values. `value` routes those away before reaching here, which is
  /// why this production carries no rejection of its own — the exclusion is the dispatch, not
  /// a check inside the arm dispatched to.
  fn enum_value<'inp, Src, Ctx>(inp) {
    node(
      K::EnumValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)
      },
    )
    .parse_input(inp)
  }

  /// `[ Value* ]`
  fn list_value<'inp, Src, Ctx>(inp) {
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = just_committed_delimiter(inp.span().end());
        while !eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
          if peek_kind::<Src, Ctx>(inp)?.is_none() {
            // Unterminated at end of input: report and return `Ok`, so the enclosing `node`
            // still closes and the rest of the file keeps its structure. There is nothing left
            // to skip and nothing to wrap — see `recover::unclosed_list` for why this is a
            // diagnostic and no more.
            return recover::unclosed_list::<Src, Ctx>(inp, open);
          }
          // `value` recovers on a head that starts no value, and that recovery is guaranteed
          // to consume — which is this loop's only termination argument.
          value::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }`
  fn object_value<'inp, Src, Ctx>(inp) {
    node(
      K::ObjectValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = just_committed_delimiter(inp.span().end());
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => object_field::<Src, Ctx>(inp)?,
            // The head is checked here rather than left to `object_field`'s own `expect`,
            // because that `expect` would return `Err` and abort the whole value — the list's
            // recovery would then have no counterpart inside an object, and `{a: 1, !, b: 2}`
            // would cost the rest of the parse instead of one token.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, OBJECT_FIELD_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Name : Value`
  fn object_field<'inp, Src, Ctx>(inp) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        value::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `= Value`
  ///
  /// The spec's `DefaultValue` takes a **const** value, which forbids a `Variable`. That
  /// constness is a *validation* rule over the tree, not a parse rule: rejecting `$x` here
  /// would leave the variable's tokens unattributed and cost a lossless consumer the very
  /// nodes it needs to report the mistake. `syntactic/` keeps the distinction in its types;
  /// this suite keeps it out of the shape and leaves it to the layer above.
  fn default_value<'inp, Src, Ctx>(inp) {
    node(
      K::DefaultValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        value::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the value head. Opens **no** node of its own; the chosen production opens its
  /// own.
  ///
  /// The `Identifier` arm is the only one that cannot decide on the kind alone: `true`, `false`
  /// and `null` are `Identifier` tokens like any other name, and the three reserved spellings
  /// are what separate `BooleanValue`/`NullValue` from `EnumValue`. `DowncastRef` reads the
  /// spelling off the peeked token without consuming it, exactly as `syntactic/value` does.
  fn value<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Dollar) => variable::<Src, Ctx>(inp),
      Some(Kind::Int) => int_value::<Src, Ctx>(inp),
      Some(Kind::Float) => float_value::<Src, Ctx>(inp),
      Some(Kind::InlineString | Kind::BlockString) => string_value::<Src, Ctx>(inp),
      Some(Kind::LBracket) => list_value::<Src, Ctx>(inp),
      Some(Kind::LBrace) => object_value::<Src, Ctx>(inp),
      Some(Kind::Identifier) => {
        // The downcast returns an owned, `Copy` keyword, so nothing borrowed escapes the
        // closure — the `&&Token` receiver that costs `kind_of` its own helper is harmless
        // here.
        let keyword: PeekedKeyword = inp.peek_head_map(|t| t.data.downcast_ref())?;
        match keyword {
          Some(Some(ContextualKeyword::True | ContextualKeyword::False)) => {
            boolean_value::<Src, Ctx>(inp)
          }
          Some(Some(ContextualKeyword::Null)) => null_value::<Src, Ctx>(inp),
          // Every other spelling — including the SDL keywords and the directive-location
          // spellings `ContextualKeyword` also carries — is a perfectly good enum value.
          Some(_) => enum_value::<Src, Ctx>(inp),
          // Unreachable: `peek_kind` just answered `Some`, so the head is there to be read.
          // Reported rather than `unreachable!`d, because a panic in a parser is never the
          // better failure.
          None => recover::unexpected::<Src, Ctx>(inp, VALUE_HEADS),
        }
      }
      _ => recover::unexpected::<Src, Ctx>(inp, VALUE_HEADS),
    }
  }
}

/// Drivers that run one value production over a `&str` and hand back the tree it built.
///
/// Test-only scaffolding, exported so the integration test at `tests/lossless_value.rs` can
/// reach it; nothing in the crate calls it.
///
/// **Why the tests do not go through `parse_str`.** `document` is Task 3's drain-everything
/// stub until Task 8, so no value production is reachable from the crate's public entry point
/// yet. Asserting through `parse_str` today would not fail — it would compare two empty trees
/// and pass, which is worse. These drivers make the assertions real now, and the `parse_str`
/// forms are kept `#[ignore]`d in the test file for Task 8 to switch on.
///
/// **This module is where the productions stop being generic.** They name only the
/// projections; a driver must choose a concrete source, emitter and context to build a `Sink`
/// at all, exactly as `super::runner::parse_str` does.
#[doc(hidden)]
pub mod test_support {
  use tokora::{InputRef, Parse as _, cache::DefaultCache, cst::Sink};

  use crate::graphql::GraphQL;

  use super::super::{
    GraphqlLosslessLexer, Parse,
    runner::{LosslessEmitter, LosslessSink, finish_root, profile},
  };

  /// The context pair and the input the driver's closure receives.
  ///
  /// Spelled out because a closure's parameter type is **not** inferred through a `ParseInput`
  /// bound — only through an `Fn` bound — so `|inp: &mut _|` leaves `L` and `Ctx` unresolved
  /// and the body's first method call is the error site. `runner::parse_str` never hits this:
  /// it applies a named function whose own signature pins both.
  type TestCtx<'inp, 'sink> = (
    &'sink mut LosslessSink<'inp>,
    DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input, 'sink> =
    InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp, 'sink>, GraphQL>;

  /// Runs one production over `src`, drains whatever it left, and materializes.
  ///
  /// The drain is not optional. `Sink::finish` refuses any source byte that no committed token
  /// covers and no lexer-error diagnostic explains (`FinishError::UncoveredGap`), and a
  /// single-value production stops at the end of its value by design. It also runs on the
  /// error path: a production that returns `Err` has committed a prefix and left the rest, and
  /// without the drain that would be a panic in the driver instead of a reportable parse.
  macro_rules! drive {
    ($lt:lifetime, $src:expr, $production:ident) => {{
      let src: &$lt str = $src;
      let mut sink: LosslessSink<$lt> =
        Sink::new(src, LosslessEmitter::default(), profile::<str>());

      let _out = tokora::Parser::with_context::<GraphqlLosslessLexer<'_, str>, (), _>((
        &mut sink,
        DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
      ))
      .apply::<_, GraphQL>(|inp: &mut TestInput<$lt, '_, '_>| {
        // `::<str, _>` for the reason the module docs give: `Src` is not inferable from the
        // input type, and `str` is the parameter that matches `L::Source`.
        let out = super::$production::<str, _>(inp);
        inp.skip_while(|_| true)?;
        out
      })
      .parse_str(src);

      finish_root(sink)
    }};
  }

  /// `super::value` over `src`.
  ///
  /// The `'inp` is **named**, threaded from `src`, for the reason `trivia::test_support`
  /// records: elided, it varies independently of the error type and the closure `E0521`s.
  pub fn parse_value<'inp>(src: &'inp str) -> Parse {
    drive!('inp, src, value)
  }

  /// `super::default_value` over `src` — the one value production `value` cannot reach.
  pub fn parse_default_value<'inp>(src: &'inp str) -> Parse {
    drive!('inp, src, default_value)
  }
}
