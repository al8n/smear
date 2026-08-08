//! Executable-definition productions: `VariableDefinition`, `VariablesDefinition`,
//! `OperationDefinition`, `FragmentDefinition`, `ExecutableDocument`.
//!
//! The conventions are `value.rs`'s and are not repeated here — the two kind spaces, the
//! `::<Src, Ctx>` on every generic call, and the fully spelled `node(…)` closure parameter.
//!
//! # Every keyword here is contextual
//!
//! `query`, `mutation`, `subscription` and `fragment` are `Identifier` tokens to the lexer,
//! exactly as `true`/`false`/`null` are in `value.rs`. The spelling is read **once**, by
//! `executable_definition`, through `trivia::peek_as` on the peeked token;
//! each definition production then consumes its keyword with a plain `expect(Identifier)` and
//! states the spelling as a precondition. Re-checking inside the arm would read the same token
//! twice to reach the same answer — the ruling Task 5 recorded for the three reserved value
//! spellings, applied to the four definition heads.
//!
//! # An undelimited repetition ends with its trailing trivia inside it
//!
//! `ExecutableDocument` is a repetition with no closing delimiter, so Task 6's law applies: the
//! loop's terminating peek must cross the trailing trivia to learn that no further definition
//! follows, and it crosses it while the node is still open. Here that is not even a
//! compromise — a document *is* the whole file, leading and trailing trivia included, so the
//! node covering every byte is the right answer rather than a tolerated one. The definitions
//! inside it are each delimited by their own `}` and are unaffected, which
//! `an_executable_document_holds_every_definition` asserts by node text.
//!
//! # Every definition here is a retro-wrap, because of the description
//!
//! Every keyworded executable definition opens with `Description?`. A leading string says only
//! that *some* definition follows, and the atom set has no two-token peek, so the dispatcher
//! mints a mark, commits the description, reads the keyword, and hands the mark to the production
//! it chose — which spends it, putting the `Description` inside the definition it describes.
//! `definition.rs`'s module docs give the full reasoning; this file follows it.
//!
//! # The shorthand is the one alternative with no `Description?`
//!
//! `OperationDefinition : Description? OperationType … | SelectionSet`. The dispatcher therefore
//! carries a `described` flag into its `{` arm and reports there. The operation is still parsed
//! and the description still lands inside it, so **only the verdict moves** — no round-trip gate
//! and no golden tree can see the difference, which is why the rule is tested by verdict.
//!
//! # Divergences, decided rather than inherited
//!
//! - **A malformed definition is an `Err`, not a resync.** `document` is where
//!   catch-resync-continue belongs, and it is the only caller that can be tested at its own
//!   call site.

use crate::lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::parser::graphql::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlLosslessInput,
  definition::{description, operation_type, starts_description},
  directive::directives,
  recover,
  recover::{
    DESCRIBED_DEFINITION_HEADS, EXECUTABLE_DEFINITION_HEADS, NAME_HEADS, VARIABLE_DEFINITION_HEADS,
    opener_span,
  },
  selection::{selection_set, type_condition},
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::type_ref,
  value::{Constness, default_value, variable},
};

use crate::parser::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphql::lossless;

  /// `Description? Variable : Type DefaultValue? Directives[Const]?`
  ///
  /// **Precondition: the head is `$` or a string.** [`variables_definition`] decides that.
  ///
  /// **The directives here are const**, and that is the one place in an executable document
  /// where they are: the spec writes `VariableDefinition: Description? Variable : Type
  /// DefaultValue? Directives[Const]?`, and `syntactic/`'s `variable_definition` calls
  /// `const_directives` where its `operation_definition` and `fragment_definition` call plain
  /// `directives`. Reading the split as "SDL is const, executable is not" gets this production
  /// wrong.
  fn variable_definition<'inp, Src, Ctx>(inp) {
    // Minted before the description so the retro-wrap covers it; here it is always spent.
    let mark = inp.cst_mark();
    if starts_description(peek_kind::<Src, Ctx>(inp)?) {
      description::<Src, Ctx>(inp)?;
      // The peek is what commits the trivia between the description and the `$`. Without it the
      // space lands *inside* `Variable`, because `variable`'s `node` opens where it is called —
      // the defect `only_the_recorded_nodes_open_on_their_leading_trivia` pins, and the ruling
      // `named_type` already records.
      peek_kind::<Src, Ctx>(inp)?;
    }
    node_at(
      mark,
      K::VariableDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        // The `$name` is a `Variable` node, the same one a value position builds: a variable is
        // a variable wherever it appears, and a typed accessor that had to match two node kinds
        // for it would be paying for a distinction the grammar does not make.
        variable::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        type_ref::<Src, Ctx>(inp)?;
        // Dispatched on a peek rather than attempted, so the `=` is consumed *inside* the
        // `DefaultValue` node and an absent default opens nothing.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Equal) {
          default_value::<Src, Ctx>(inp)?;
        }
        directives::<Src, Ctx>(inp, Constness::Const)
      },
    )
    .parse_input(inp)
  }

  /// `( VariableDefinition+ )`
  ///
  /// `arguments`' loop with a different head and a different emptiness ruling: `syntactic/`
  /// rejects the empty `()` here ("one-or-more, so an empty `()` errors") where it accepts it
  /// for `Arguments`. Gate 1 compares the two suites' verdicts input by input, so the two
  /// rulings are followed one production at a time rather than unified into a house rule.
  fn variables_definition<'inp, Src, Ctx>(inp) {
    node(
      K::VariablesDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        // The report consumes nothing, so the `)` is still the loop's to eat; checked here
        // rather than after the loop, where the diagnostic would point past the closer it is
        // about.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RParen) {
          recover::report_unexpected::<Src, Ctx>(inp, VARIABLE_DEFINITION_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
            // A string head is the definition's description — `VariableDefinition` carries a
            // `Description?` like every other executable definition does.
            Some(Kind::Dollar | Kind::InlineString | Kind::BlockString) => {
              variable_definition::<Src, Ctx>(inp)?
            }
            // The head is checked here rather than left to `variable_definition`'s own
            // `expect`, because that `expect` would return `Err` and abort the whole list — the
            // ruling `arguments` and `object_value` both record.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, VARIABLE_DEFINITION_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Description? SelectionSet` (the shorthand) or
  /// `Description? OperationType Name? VariablesDefinition? Directives? SelectionSet`
  ///
  /// **Precondition: `mark` was minted before any description, which is already committed, and
  /// the head is `{` or an `Identifier` spelled `query`, `mutation` or `subscription`.** Both
  /// dispatchers establish that; see the module docs for why the keyword is not re-read here.
  fn operation_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::OperationDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          return selection_set::<Src, Ctx>(inp);
        }
        operation_type::<Src, Ctx>(inp)?;
        // The operation name is optional, and any name will do — an operation may be called
        // `query` or `on`; nothing in the grammar reserves a spelling in this position.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          variables_definition::<Src, Ctx>(inp)?;
        }
        directives::<Src, Ctx>(inp, Constness::NonConst)?;
        // Required, and an `Err` when it is absent — the same call `argument` makes for its
        // missing `:`. There is no shape left to recover into, and Task 8's `document` is where
        // a failed definition is caught and resynchronised past.
        selection_set::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `Description? fragment FragmentName TypeCondition Directives? SelectionSet`
  ///
  /// **Precondition: `mark` was minted before any description, which is already committed, and
  /// the head is an `Identifier` spelled `fragment`.**
  ///
  /// The type condition is [`type_condition`], which recovers rather than requiring: a
  /// definition whose `on` is missing keeps its name, its type and its selection set, and costs
  /// one diagnostic.
  ///
  /// # `FragmentName: Name but not "on"` is a **grammar** rule, and is enforced here
  ///
  /// `on` is contextual, so the exclusion is a spelling test rather than a kind test — and it is
  /// the parser's, not a validation pass's: the spec spends a production on it and `syntactic/`
  /// spends one too (`fragment_name`). The name is **reported and still consumed**, so
  /// `fragment on on T { f }` keeps its `FragmentDefinition`, its name token and its type
  /// condition, and differs from the accepted spelling only in the verdict. Task 8's
  /// `enum_value_definition` and `directive_location` are the same shape.
  fn fragment_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::FragmentDefinition.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // The fragment name, whose one excluded spelling is checked before it is taken. The
        // report consumes nothing — the `expect` below is what makes the progress the document
        // loop needs — so the name reaches the tree either way.
        if peek_as::<Src, Ctx, ContextualKeyword>(inp)? == Some(ContextualKeyword::On) {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        type_condition::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::NonConst)?;
        selection_set::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the definition head, reading an optional leading description first. Opens
  /// **no** node of its own; the chosen production spends the mark on its own — and this is one
  /// of the two places the contextual keywords are read.
  fn executable_definition<'inp, Src, Ctx>(inp) {
    // The head peek first — see `document::definition` for the ruling and for the measurement
    // that shows the ordering is currently redundant, every caller being a loop that peeks.
    let head = peek_kind::<Src, Ctx>(inp)?;
    let mark = inp.cst_mark();
    let described = starts_description(head);
    if described {
      description::<Src, Ctx>(inp)?;
    }
    match peek_kind::<Src, Ctx>(inp)? {
      // The shorthand operation, which has no keyword at all — and which a description may not
      // precede: `OperationDefinition : SelectionSet` is the one alternative with no
      // `Description?` slot, so `syntactic/` rejects the combination and gate 1 compares
      // verdicts. The operation is still parsed, with the description inside it: a lossless
      // consumer needs the nodes to point the diagnostic at.
      Some(Kind::LBrace) => {
        if described {
          recover::report_unexpected::<Src, Ctx>(inp, DESCRIBED_DEFINITION_HEADS)?;
        }
        operation_definition::<Src, Ctx>(inp, mark)
      }
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, ContextualKeyword>(inp)? {
        Some(
          ContextualKeyword::Query | ContextualKeyword::Mutation | ContextualKeyword::Subscription,
        ) => operation_definition::<Src, Ctx>(inp, mark),
        Some(ContextualKeyword::Fragment) => fragment_definition::<Src, Ctx>(inp, mark),
        // A name that is not one of the four. Reported and skipped — `unexpected` consumes at
        // least one token whenever input remains, which is the document loop's only termination
        // argument. The mark is left unspent, and an unspent mark materializes into nothing.
        _ => recover::unexpected::<Src, Ctx>(inp, EXECUTABLE_DEFINITION_HEADS),
      },
      _ => recover::unexpected::<Src, Ctx>(inp, EXECUTABLE_DEFINITION_HEADS),
    }
  }

  /// `ExecutableDefinition+`
  ///
  /// The empty form is reported: `syntactic/` rejects an empty input ("one-or-more, so an empty
  /// input errors"), and gate 1 compares verdicts. The node is opened either way, so a caller
  /// always finds a document to walk.
  ///
  /// The executable-only root, off [`parse_document`](super::parse_document)'s mixed-form path.
  /// [`parse_executable_document`](super::parse_executable_document) is its shipped entry point.
  fn executable_document<'inp, Src, Ctx>(inp) {
    node(
      K::ExecutableDocument.raw(),
      |inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, EXECUTABLE_DEFINITION_HEADS);
        }
        // This peek is also what crosses the trailing trivia — see the module docs.
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          executable_definition::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// [`executable_document`] plus the drain, for the reason `document.rs`'s `document_entry`
  /// carries one.
  ///
  /// This loop does not even catch — `executable_definition::<Src, Ctx>(inp)?` propagates — so an
  /// `Err` reaching [`parse_executable_document`](super::parse_executable_document) is the
  /// ordinary case rather than the exotic one, and the tail it left uncommitted would be a
  /// `FinishError::UncoveredGap` panic in materialization without this drain.
  fn executable_document_entry<'inp, Src, Ctx>(inp) {
    let out = executable_document::<Src, Ctx>(inp);
    inp.skip_while(|_| true)?;
    out
  }
}

lossless_drivers! {
  dialect = graphql::lossless;

  /// Drivers that run one executable-definition production over a `&str` and hand back the tree
  /// it built, for `tests/lossless_selection.rs`.
  mod test_support;

  /// `super::variables_definition` over `src` — the only door to `variable_definition`.
  fn parse_variables_definition => variables_definition;

  /// `super::operation_definition` over `src`.
  fn parse_operation_definition => operation_definition (mark);

  /// `super::fragment_definition` over `src`.
  fn parse_fragment_definition => fragment_definition (mark);
}
