//! Executable-definition productions: `VariableDefinition`, `VariablesDefinition`,
//! `OperationDefinition`, `FragmentDefinition`, `ExecutableDocument`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Every keyword here is contextual
//!
//! `query`, `mutation`, `subscription`, `fragment` and `where` are `Identifier` tokens to the
//! lexer, exactly as `true`/`false`/`null` are in `value.rs`. The spelling is read **once**, by
//! this module's private `executable_definition`, through the `peek_as` atom; each definition
//! production then consumes its keyword with a plain `expect(Identifier)` and states the spelling
//! as a precondition. Re-checking inside the arm would read the same token twice to reach the same
//! answer.
//!
//! # Three shapes GraphQL's twin does not have
//!
//! - **A description on a variable definition and on an executable definition** (divergence 12,
//!   `graphqlx/syntactic/executable/mod.rs:343-346, 710-713`). It reaches the tree as a **bare
//!   string token** inside the node it describes: the kind space has no `Description` kind, and
//!   that is derived rather than chosen — `tests/lossless_x_kinds.rs`'s census rejects it under
//!   *one token is not a region*, recording that a description is a token child of the
//!   definition's own retro-wrapped node while every string **value** is a
//!   [`StringValue`](K::StringValue) node, "so the two can never be confused". Reaching for
//!   `string_value` here would destroy exactly that distinction, and the tree would re-print
//!   identically.
//! - **Two generic lists on a fragment definition** (divergence 13). See
//!   this module's private `fragment_definition`.
//! - **A `where` clause before an already-mandatory selection set** (divergence 19). See
//!   this module's private `constrained_selection_set`.
//!
//! And one GraphQL rule that is **absent** here: `FragmentName: Name but not "on"`. GraphQL spends
//! a production on the exclusion and this suite's GraphQL twin enforces it; GraphQLx's
//! `executable_definition_name` is a plain `take_name` (`graphqlx/syntactic/generic/mod.rs:377`),
//! so `fragment on on T { f }` is accepted here. Gate 1 compares verdicts input by input, so
//! carrying GraphQL's rule across would be a manufactured disagreement.
//!
//! # An operation's name is a `DefinitionName`, not a bare name
//!
//! `named_operation_after_head` calls `try_definition_name`
//! (`graphqlx/syntactic/executable/mod.rs:469`), so `query Q<T = Int>` is grammatical and the
//! node under an operation is [`DefinitionName`](K::DefinitionName) — the same node an SDL
//! definition will reach in Task 12, and a *different* kind from the
//! [`ExecutableDefinitionName`](K::ExecutableDefinitionName) a fragment carries.
//!
//! # Every definition here is a retro-wrap, because of the description
//!
//! A leading string says only that *some* definition follows, and the atom set has no two-token
//! peek, so the dispatcher mints a mark, commits the description, reads the keyword, and hands the
//! mark to the production it chose — which spends it, putting the description inside the
//! definition it describes.
//!
//! # `executable_document` loops over `document.rs`'s entry, not over `executable_definition`
//!
//! `graphqlx/syntactic/executable/mod.rs`'s document admits an import at every position, and
//! divergence 22 says that import may not be described. Both facts live in
//! `document.rs`'s `import_or_executable_definition`, which is where the *other* two roots make
//! the same decision; this document's loop reaches it rather than repeating either half.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{ParseInput as _, cst::event::EventMark};

use crate::graphqlx::kinds::SyntaxKind as K;

// `node`/`node_at` come from `coverage`, not from `tokora::parser`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword,
  directive::directives,
  generic::{
    definition_name, executable_definition_name, executable_definition_type_generics,
    optional_where_clause,
  },
  recover,
  recover::{
    EXECUTABLE_DEFINITION_HEADS, VARIABLE_DEFINITION_HEADS, opener_span, starts_description,
  },
  selection::{selection_set, type_condition},
  trivia::{eat_if, expect, peek_as, peek_kind},
  ty::type_ref,
  value::{Constness, default_value, variable},
};

/// Whether `keyword` opens an executable definition — the three operation spellings and
/// `fragment`.
///
/// A free function rather than an arm inside the dispatcher, because `document.rs`'s mixed entry
/// has to ask the same question *before* it can choose between the executable and the type-system
/// dispatcher, and a question asked in two places is a question two places can answer differently.
#[inline]
pub(crate) fn starts_executable_keyword(keyword: Option<Keyword>) -> bool {
  matches!(
    keyword,
    Some(Keyword::Query | Keyword::Mutation | Keyword::Subscription | Keyword::Fragment)
  )
}

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// The description a described position may open with — a **bare string token**, no node.
  ///
  /// GraphQL's twin wraps it in a `Description` node; GraphQLx's kind space has no such kind, for
  /// the reason the module docs record. A production rather than an inline `eat_if` because three
  /// call sites make the same two-kind choice and a fourth arrives in Task 12, and because the
  /// *absence* of a node here is the decision worth having one place to read.
  ///
  /// **Precondition: [`starts_description`] answered `true` for the head.**
  fn description<'inp, Src, Ctx>(inp) {
    if eat_if::<Src, Ctx>(inp, Kind::InlineString)? {
      return Ok(());
    }
    expect::<Src, Ctx>(inp, Kind::BlockString)
  }

  /// `Description? Variable : Type DefaultValue? Directives[Const]?`
  ///
  /// **Precondition: the head is a string or a `$`.** [`variables_definition`] decides that, and
  /// its peek is what commits the leading trivia — so the mark minted here rides at the
  /// definition's own first token.
  ///
  /// **The directives here are const**, and that is the one place in an executable document where
  /// they are: `graphqlx/syntactic/executable/mod.rs:316` calls `const_directives` where its
  /// operation and fragment definitions call plain `directives`. Reading the split as "SDL is
  /// const, executable is not" gets this production wrong.
  fn variable_definition<'inp, Src, Ctx>(inp) {
    // Minted before the description so the retro-wrap covers it; unspent it would materialize into
    // nothing, and here it is always spent.
    let mark = inp.cst_mark();
    if starts_description(peek_kind::<Src, Ctx>(inp)?) {
      description::<Src, Ctx>(inp)?;
    }
    node_at(
      mark,
      K::VariableDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        // The `$name` is a `VariableValue` node, the same one a value position builds: a variable
        // is a variable wherever it appears, and a typed accessor that had to match two node kinds
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
  /// `arguments`' loop with a different head set and a different emptiness ruling: `syntactic/`
  /// rejects the empty `()` here (`.at_least(1)`, `executable/mod.rs:408`) where it accepts it for
  /// `Arguments`. Gate 1 compares the two suites' verdicts input by input, so the two rulings are
  /// followed one production at a time rather than unified into a house rule.
  fn variables_definition<'inp, Src, Ctx>(inp) {
    node(
      K::VariablesDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LParen)?;
        let open = opener_span(inp.span().end());
        // The report consumes nothing, so the `)` is still the loop's to eat; checked here rather
        // than after the loop, where the diagnostic would point past the closer it is about.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RParen) {
          recover::report_unexpected::<Src, Ctx>(inp, VARIABLE_DEFINITION_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RParen)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_parens::<Src, Ctx>(inp, open),
            // A string head is a description, which is why this set is three kinds wide and
            // GraphQL's is one.
            Some(Kind::Dollar | Kind::InlineString | Kind::BlockString) => {
              variable_definition::<Src, Ctx>(inp)?
            }
            // The head is checked here rather than left to `variable_definition`'s own `expect`,
            // because that `expect` would return `Err` and abort the whole list.
            Some(_) => recover::unexpected::<Src, Ctx>(inp, VARIABLE_DEFINITION_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `query` | `mutation` | `subscription` — one token, and **no node**.
  ///
  /// GraphQL's twin opens an `OperationType` node; GraphQLx's census gives one
  /// `query`/`mutation`/`subscription` identifier token no kind of its own, because the
  /// `OperationDefinition` around it already says what it is. A production rather than a bare
  /// `expect` because Task 12's `root_operation_type_definition` reaches the same position, and
  /// because the *absence* of a node is worth one place to read.
  ///
  /// **Precondition: the spelling has already been read**, by [`executable_definition`] or by
  /// [`operation_definition`]'s own dispatch.
  fn operation_type<'inp, Src, Ctx>(inp) {
    expect::<Src, Ctx>(inp, Kind::Identifier)
  }

  /// `WhereClause? SelectionSet` — divergence 19.
  ///
  /// The clause and the set are **siblings**, not nested: the census gives
  /// `graphqlx::ast::Constrained` no kind of its own, recording that "the clause and the target
  /// are siblings, both already nodes". A wrapper node here would be a region the kind space
  /// cannot name, and it would re-print identically.
  ///
  /// The block after an executable `where` is **already mandatory**, so a missing `{` surfaces as
  /// [`selection_set`]'s own error rather than as an `Expectation::LBrace` raised at the `where`
  /// site — the difference from the five SDL sites Task 12 owns, where a present `where` is what
  /// *makes* the block mandatory.
  fn constrained_selection_set<'inp, Src, Ctx>(inp) {
    optional_where_clause::<Src, Ctx>(inp)?;
    selection_set::<Src, Ctx>(inp)
  }

  /// `Description? SelectionSet` (the shorthand) or
  /// `Description? OperationType DefinitionName? VariablesDefinition? Directives? WhereClause?
  /// SelectionSet`
  ///
  /// **Precondition: `mark` was minted before any description, which is already committed, and the
  /// head is `{` or an `Identifier` spelled `query`, `mutation` or `subscription`.**
  ///
  /// The shorthand takes no `where` clause: `syntactic/`'s `ExecutableDefinitionHead::Shorthand`
  /// arm is a bare `selection_set` (`executable/mod.rs:576`), and only the named form goes through
  /// `constrained_selection_set`.
  fn operation_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::OperationDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LBrace) {
          return selection_set::<Src, Ctx>(inp);
        }
        operation_type::<Src, Ctx>(inp)?;
        // The operation name is optional, and any spelling will do — an operation may be called
        // `query` or `on`; nothing in the grammar reserves one in this position.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
          definition_name::<Src, Ctx>(inp)?;
        }
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
          variables_definition::<Src, Ctx>(inp)?;
        }
        directives::<Src, Ctx>(inp, Constness::NonConst)?;
        constrained_selection_set::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `Description? fragment ExecutableDefinitionTypeGenerics? ExecutableDefinitionName
  /// TypeCondition Directives? WhereClause? SelectionSet`
  ///
  /// **Precondition: `mark` was minted before any description, which is already committed, and the
  /// head is an `Identifier` spelled `fragment`.**
  ///
  /// # Divergence 13 — the two generic lists, and why their order is load-bearing
  ///
  /// `fragment <T, U> F<A, B> on X { f }` declares **two** lists. The first is the definition's
  /// *implementation* generics, parsed before the name
  /// (`try_executable_definition_type_generics`, `executable/mod.rs:535`); the second is the
  /// name's own, parsed inside [`executable_definition_name`] (`generic/mod.rs:378`). They are the
  /// same node kind but not the same position: the first is a **sibling** of the name node and the
  /// second is a **child** of it.
  ///
  /// Parsing them in the other order consumes exactly the same tokens and re-prints exactly the
  /// same text, so only the node-kind pre-order can tell the two apart — which is why
  /// `a_fragment_definition_has_two_distinct_generic_lists` asserts the pre-order and the name
  /// node's extent rather than a verdict.
  ///
  /// The type condition is [`type_condition`], which recovers rather than requiring: a definition
  /// whose `on` is missing keeps its name, its type and its selection set, and costs one
  /// diagnostic.
  fn fragment_definition<'inp, Src, Ctx>(inp, mark: EventMark) {
    node_at(
      mark,
      K::FragmentDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // The implementation generics, BEFORE the name. Dispatched on a peek rather than
        // attempted, so an absent list opens nothing.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle) {
          executable_definition_type_generics::<Src, Ctx>(inp)?;
        }
        // The name, which carries its own list inside itself. GraphQLx does not reserve `on`
        // here — see the module docs.
        executable_definition_name::<Src, Ctx>(inp)?;
        type_condition::<Src, Ctx>(inp)?;
        directives::<Src, Ctx>(inp, Constness::NonConst)?;
        constrained_selection_set::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// Dispatch on the definition head, with any description already committed and its `mark`
  /// handed in. Opens **no** node of its own; the chosen production spends the mark on its own.
  ///
  /// **The description is the caller's**, and that is not an accident of layering: divergence 22
  /// makes *whether* a description is allowed a property of the document root, so the three entry
  /// dispatchers in `document.rs` read it and this production never does.
  fn executable_definition_at<'inp, Src, Ctx>(inp, mark: EventMark) {
    match peek_kind::<Src, Ctx>(inp)? {
      // The shorthand operation, which has no keyword at all.
      Some(Kind::LBrace) => operation_definition::<Src, Ctx>(inp, mark),
      Some(Kind::Identifier) => match peek_as::<Src, Ctx, Keyword>(inp)? {
        Some(Keyword::Query | Keyword::Mutation | Keyword::Subscription) => {
          operation_definition::<Src, Ctx>(inp, mark)
        }
        Some(Keyword::Fragment) => fragment_definition::<Src, Ctx>(inp, mark),
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
  /// The empty form is reported: `syntactic/` rejects an empty input (`.at_least(1)`), and gate 1
  /// compares verdicts. The node is opened either way, so a caller always finds a document to
  /// walk. This loop's terminating peek is also what crosses the trailing trivia, which therefore
  /// lands inside the document — a document *is* the whole file, so that is the right answer
  /// rather than a tolerated one.
  ///
  /// The executable-only root, off [`super::parse_document`]'s mixed-form path.
  /// [`super::parse_executable_document`] is its shipped entry point.
  fn executable_document<'inp, Src, Ctx>(inp) {
    node(
      K::ExecutableDocument.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        if peek_kind::<Src, Ctx>(inp)?.is_none() {
          return recover::report_unexpected::<Src, Ctx>(inp, EXECUTABLE_DEFINITION_HEADS);
        }
        while peek_kind::<Src, Ctx>(inp)?.is_some() {
          if super::document::import_or_executable_definition::<Src, Ctx>(inp).is_err() {
            recover::resync_to_definition::<Src, Ctx>(inp)?;
          }
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// [`executable_document`], then a drain — the production [`super::parse_executable_document`]
  /// applies.
  ///
  /// See `document.rs`'s module docs for why the drain is not optional.
  fn executable_document_entry<'inp, Src, Ctx>(inp) {
    let out = executable_document::<Src, Ctx>(inp);
    inp.skip_while(|_| true)?;
    out
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one executable-definition production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_x_selection.rs`.
  mod test_support;

  /// `super::variables_definition` over `src` — the only door to `variable_definition`.
  fn parse_variables_definition => variables_definition;

  /// `super::operation_definition` over `src`.
  fn parse_operation_definition => operation_definition (mark);

  /// `super::fragment_definition` over `src`.
  fn parse_fragment_definition => fragment_definition (mark);
}
