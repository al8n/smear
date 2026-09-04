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
//! `&'inp str`, which is the concrete instance of the same ambiguity `runner::parse_document`
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
//!
//! # `Value[Const]` is threaded, not duplicated
//!
//! The spec parameterises eight productions on `[Const]` — `Value`, `ListValue`, `ObjectValue`,
//! `ObjectField`, `Argument`, `Arguments`, `Directive`, `Directives` — and in a const position
//! `Variable` is not a production at all. `Constness` carries that parameter through all eight:
//! `value`,
//! `list_value`, `object_value`, `object_field` and — in `directive.rs` — `argument`,
//! `arguments`, `directive` and `directives`. `default_value` takes no argument: `DefaultValue`
//! is const in both positions the grammar puts it in.
//!
//! **`syntactic/` duplicates instead, and its reason does not carry over.** There, `value` and
//! `const_value` return *different types* (`InputValue` versus `ConstInputValue`), so the
//! constness lives in the type system and two productions are what a caller consumes. Here every
//! production returns `()` and writes into one kind space: a `Variable` in a const position
//! builds the same `K::Variable` node it builds anywhere else, so a second set of productions
//! would differ from the first only in which recursive callee it names — eight bodies duplicated
//! for one `if`. Threading is also the idiom this suite already uses for a per-call-site
//! parameter: every retro-wrapping production takes an `EventMark` the same way.
//!
//! # A `Variable` in a const position is reported **and still built**
//!
//! The rejection is a diagnostic, not a refusal to parse: `Constness::Const` makes `value`
//! report before it dispatches, and the `Variable` node is opened regardless. Every byte is
//! kept, `tree.text() == source` still holds, and the node a diagnostic wants to point at is
//! there. That is the ruling Task 8 made twice — `enum_value_definition` and
//! `directive_location` both report a reserved spelling and still build their node — applied to
//! the productions that predate it.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{ParseInput as _, utils::DowncastRef as _};

/// What `DowncastRef` answers for a peeked token: the outer `Option` is the peek (`None` at
/// end of input), the inner one is the downcast (`None` for a name that is not a reserved
/// spelling). Named so the `peek_head_map` call site can annotate the downcast's target type
/// without spelling the trait path.
type PeekedKeyword = Option<Option<ContextualKeyword>>;

use crate::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::node;

use super::{
  GraphqlLosslessInput, recover,
  recover::{CONST_VALUE_HEADS, OBJECT_FIELD_HEADS, VALUE_HEADS, opener_span},
  trivia::{eat_if, expect, peek_kind},
};

/// The spec's `[Const]` grammar parameter, as an argument.
///
/// Threaded through every production that can reach a value, so one set of productions serves
/// both flavours; see the module docs for why this suite threads where `syntactic/` duplicates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Constness {
  /// A `Value[Const]` position: `DefaultValue`, and every `Directives[Const]` context — which is
  /// every SDL directive position plus a `VariableDefinition`'s own directives. A `Variable`
  /// here is reported.
  Const,
  /// An ordinary `Value` position: an executable `Directives` context, and a field's
  /// `Arguments`. A `Variable` here is exactly what the grammar is for.
  NonConst,
}

impl Constness {
  /// Whether a `Variable` in this position is a grammar error.
  #[inline]
  pub(crate) const fn forbids_variable(self) -> bool {
    matches!(self, Self::Const)
  }

  /// The head set a diagnostic raised in this position names — `VALUE_HEADS` without the `$`
  /// when a variable is not a production here.
  ///
  /// **Measured, not assumed: this distinction is currently unobservable.** Collapsing both arms
  /// to `VALUE_HEADS` reds nothing (mutation, in-space silent pass), because the set reaches an
  /// error only as `UnexpectedToken::expected_one_of`, and `lossless/mod.rs`'s `expectation_of`
  /// folds every multi-kind expectation onto `Expectation::Name` — after which [`Parse`] drops
  /// the payload entirely and exposes span, severity and skipped-token count alone. There is no
  /// door through which a test could read it.
  ///
  /// It is kept because the *answer* is right and the alternative is a diagnostic that names `$`
  /// as an acceptable head in the one position where it is not. The day the payload survives to
  /// [`Parse`], this becomes observable and gains its test; until then the honest record is that
  /// it has none.
  ///
  /// [`Parse`]: super::Parse
  #[inline]
  pub(crate) const fn value_heads(self) -> &'static [Kind] {
    match self {
      Self::Const => CONST_VALUE_HEADS,
      Self::NonConst => VALUE_HEADS,
    }
  }
}

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphql::lossless;

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

  /// `[ Value* ]` — `[ Value[Const]* ]` when `konst` says so, the parameter riding down to
  /// every element.
  fn list_value<'inp, Src, Ctx>(inp, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ListValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
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
          value::<Src, Ctx>(inp, konst)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `{ ObjectField* }` — const-parameterised exactly as [`list_value`] is.
  fn object_value<'inp, Src, Ctx>(inp, konst: Constness) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ObjectValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBrace)?;
        let open = opener_span(inp.span().end());
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_object::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => object_field::<Src, Ctx>(inp, konst)?,
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
  fn object_field<'inp, Src, Ctx>(inp, konst: Constness) {
    node(
      K::ObjectField.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        value::<Src, Ctx>(inp, konst)
      },
    )
    .parse_input(inp)
  }

  /// `= Value[Const]`
  ///
  /// **Takes no [`Constness`] argument, and must not.** `DefaultValue` is const in both
  /// positions the grammar puts it in — an `InputValueDefinition`'s and a
  /// `VariableDefinition`'s — so the flavour is a property of this production rather than of its
  /// call site, and a parameter would be an invitation to pass the wrong one.
  fn default_value<'inp, Src, Ctx>(inp) {
    node(
      K::DefaultValue.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Equal)?;
        value::<Src, Ctx>(inp, Constness::Const)
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
  ///
  /// # The one place `konst` is read
  ///
  /// A `$` in a const position is **reported and then parsed anyway**. The report consumes
  /// nothing, so `variable` still opens its `K::Variable` node over the same two tokens and the
  /// tree is byte-for-byte what a non-const position would have built — the verdict is the only
  /// thing that changes. See the module docs for why that is the right shape for a lossless
  /// suite, and `recover::report_unexpected` for why a report that consumes nothing is safe
  /// here (the `variable` call after it makes the progress every enclosing loop needs).
  fn value<'inp, Src, Ctx>(inp, konst: Constness) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Dollar) => {
        if konst.forbids_variable() {
          recover::report_unexpected::<Src, Ctx>(inp, CONST_VALUE_HEADS)?;
        }
        variable::<Src, Ctx>(inp)
      }
      Some(Kind::Int) => int_value::<Src, Ctx>(inp),
      Some(Kind::Float) => float_value::<Src, Ctx>(inp),
      Some(Kind::InlineString | Kind::BlockString) => string_value::<Src, Ctx>(inp),
      Some(Kind::LBracket) => list_value::<Src, Ctx>(inp, konst),
      Some(Kind::LBrace) => object_value::<Src, Ctx>(inp, konst),
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
          None => recover::unexpected::<Src, Ctx>(inp, konst.value_heads()),
        }
      }
      _ => recover::unexpected::<Src, Ctx>(inp, konst.value_heads()),
    }
  }
}

lossless_drivers! {
  dialect = graphql::lossless;

  /// Drivers that run one value production over a `&str` and hand back the tree it built, for
  /// `tests/lossless_value.rs`.
  mod test_support;

  /// `super::value` over `src`, in an ordinary (non-const) position.
  fn parse_value => value [Constness::NonConst];

  /// `super::value` over `src`, in a **const** position — the flavour `DefaultValue` and every
  /// SDL directive argument reach, and the only door to the `$` rejection below the document
  /// level.
  fn parse_const_value => value [Constness::Const];

  /// `super::default_value` over `src` — the one value production `value` cannot reach.
  fn parse_default_value => default_value;
}
