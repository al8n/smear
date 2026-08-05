//! The generic-parameter and `where`-clause productions an executable definition reaches:
//! `DefinitionTypeParam`, `DefinitionTypeGenerics`, `DefinitionName`,
//! `ExecutableDefinitionTypeGenerics`, `ExecutableDefinitionName`, `WherePredicate` and
//! `WhereClause`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Nothing in this file is a port
//!
//! GraphQL has no generic parameters, no `where` clause and no definition-name node. Every
//! production below is written against `graphqlx/syntactic/generic/mod.rs` directly, and the habit
//! the previous tasks build — fork the GraphQL production and edit it — has nothing to fork.
//!
//! # Why these seven are here and not in Task 13's file
//!
//! The plan lists the whole generic family under Task 13, and Task 11's own production list
//! depends on seven of them: divergence 13 needs both executable generic lists, divergence 19
//! needs the `where` clause, and an operation's name is a
//! [`DefinitionName`](K::DefinitionName) — `named_operation_after_head` calls
//! `try_definition_name`, not the executable one (`graphqlx/syntactic/executable/mod.rs:469`), so
//! `query Q<T = Int>` is grammatical and the parameter-with-default shape is reachable from here.
//! Writing them twice, or writing a narrower stand-in for Task 13 to replace, both cost more than
//! writing them once in the file they belong to. Task 13 adds the extension family and the
//! imports.
//!
//! # The three generic lists are three node kinds, and the difference is their members
//!
//! | node | member | reached from |
//! |---|---|---|
//! | [`DefinitionTypeGenerics`](K::DefinitionTypeGenerics) | `Name ('=' Type)?` — a node | a definition's name |
//! | [`ExtensionTypeGenerics`](K::ExtensionTypeGenerics) | a bare `Name` token | an extension's target (Task 13) |
//! | [`ExecutableDefinitionTypeGenerics`](K::ExecutableDefinitionTypeGenerics) | a bare `Name` token | a fragment's header and its name |
//!
//! Only the first has a member node, because only its member can carry something —
//! `ExtensionTypeParam` is excluded from the kind space by the census's *one token is not a
//! region* rule and the executable list's members were never a production at all.
//!
//! # The `where` continuation test needs **two** significant tokens, and one position proves it
//!
//! `graphqlx/syntactic/generic/mod.rs:442-472` decides whether another predicate follows with a
//! two-token window: `(Identifier, Colon | PathSeparator | LAngle)` or
//! `(PathSeparator, Identifier)`. A one-token test — *does a `TypePath` start here?* — is exact
//! wherever the clause is followed by a token that cannot start one, which covers divergence 19's
//! two executable sites and divergence 16's five block sites, because a `{` follows all seven.
//!
//! It is **wrong** at the eighth. Divergence 18 puts a `where` at the *end* of a directive
//! definition, forcing nothing after it, so `directive @d on FIELD where A: B` followed by
//! `type X { f: Int }` presents an ordinary `Identifier` that belongs to the next definition. A
//! one-token test consumes `type` as another predicate's bounded type, fails on the missing `:`,
//! and costs the whole `ObjectTypeDefinition` — on **valid** input.
//!
//! So this module's `where_clause` asks the two-token question, through the atom set's
//! `peek_second_kind` — the one atom GraphQLx adds to the shared set, and the only place in either
//! suite that spends the sink's rollback. Its own docs carry why a fixed `peek::<U2>()` cannot
//! serve and why this is the sink's discipline rather than a second buffering layer.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphqlx::kinds::SyntaxKind as K;

use super::coverage::node;

use super::{
  GraphqlxLosslessInput, Keyword, recover,
  recover::{NAME_HEADS, PATH_HEADS, opener_span},
  trivia::{eat_if, expect, peek_as, peek_kind, peek_second_kind},
  ty::{type_path, type_ref},
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `Name ('=' Type)?` — one declared generic parameter.
  ///
  /// **Precondition: the head is an `Identifier`.** Its two callers both check, so the `expect`
  /// below can only fail on a stream that changed under it.
  ///
  /// The default is `= Type`, **not** a [`DefaultValue`](K::DefaultValue): the `=` here introduces
  /// a type reference, and reusing `default_value` would put a value node where a type belongs.
  /// The two spell the same token and mean different things.
  fn definition_type_param<'inp, Src, Ctx>(inp) {
    node(
      K::DefinitionTypeParam.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if eat_if::<Src, Ctx>(inp, Kind::Equal)? {
          type_ref::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `< DefinitionTypeParam+ >` — the parameters a definition declares.
  ///
  /// Nonempty (`generic/mod.rs:263` is `.at_least(1)`), and the empty spelling is reported with
  /// [`crate::lossless::recover::report_unexpected`] rather than
  /// [`crate::lossless::recover::unexpected`]: the report consumes nothing, so the `>` is still
  /// the loop's to eat and `<>` closes on its own delimiter. Reaching for the consuming form here
  /// would eat that `>` — a closer is a depth-zero sync point — and the list would then run to end
  /// of input hunting a closer it had already swallowed.
  fn definition_type_generics<'inp, Src, Ctx>(inp) {
    node(
      K::DefinitionTypeGenerics.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LAngle)?;
        let open = opener_span(inp.span().end());
        // `<>` is reported here rather than after the loop, where the diagnostic would point past
        // the closer it is about.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RAngle) {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RAngle)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_angle::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => definition_type_param::<Src, Ctx>(inp)?,
            Some(_) => recover::unexpected::<Src, Ctx>(inp, NAME_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Name DefinitionTypeGenerics?` — a definition's name, and an operation's.
  ///
  /// **Precondition: the head is an `Identifier`.** Every caller dispatches on it, and that peek
  /// is what commits the leading trivia so the node starts at its own first token.
  fn definition_name<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::DefinitionName.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle) {
          definition_type_generics::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `< Name+ >` — the generic list an executable definition declares.
  ///
  /// Its members are **bare `Name` tokens**, not nodes: `generic/mod.rs:309` repeats `take_name`,
  /// and the census gives one identifier token no kind of its own. A port of
  /// [`definition_type_generics`] that kept the member node here would nest a
  /// `DefinitionTypeParam` inside an executable list, which re-prints identically.
  fn executable_definition_type_generics<'inp, Src, Ctx>(inp) {
    node(
      K::ExecutableDefinitionTypeGenerics.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LAngle)?;
        let open = opener_span(inp.span().end());
        // `<>` is reported, and the report consumes nothing so the `>` is still the loop's.
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RAngle) {
          recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)?;
        }
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RAngle)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_angle::<Src, Ctx>(inp, open),
            Some(Kind::Identifier) => {
              expect::<Src, Ctx>(inp, Kind::Identifier)?;
            }
            Some(_) => recover::unexpected::<Src, Ctx>(inp, NAME_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Name ExecutableDefinitionTypeGenerics?` — an executable definition's name.
  ///
  /// **Precondition: the head is an `Identifier`**, checked by the caller, whose peek commits the
  /// leading trivia.
  ///
  /// The generics this node may hold are the name's **own**, and they are not the implementation
  /// generics a fragment declares before it — see [`super::executable::fragment_definition`] for
  /// the two-list shape divergence 13 names.
  fn executable_definition_name<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::ExecutableDefinitionName.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle) {
          executable_definition_type_generics::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `TypePath ':' TypePath ('&' TypePath)*`
  ///
  /// **Precondition: the head starts a path.** [`where_clause`] checks it; the check is also what
  /// keeps the mandatory first bound's `type_path` from returning `Err` and aborting the clause.
  ///
  /// The bounds after the first are gated on the `&`, so a predicate ends the moment one is
  /// absent — which is what lets the clause's own loop see the next predicate's head.
  ///
  /// The loop's `report_unexpected` consumes nothing, which is normally unsafe inside a loop that
  /// continues; here the only path back round is through an `eat_if` that consumed an `&`, so the
  /// iteration cannot starve.
  fn where_predicate<'inp, Src, Ctx>(inp) {
    node(
      K::WherePredicate.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        type_path::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::Colon)?;
        loop {
          match peek_kind::<Src, Ctx>(inp)? {
            Some(Kind::Identifier | Kind::PathSeparator) => type_path::<Src, Ctx>(inp)?,
            // A `:` or an `&` with nothing after it. Reported without consuming: whatever is
            // here belongs to the next component, and `type_path` above already made this
            // production's progress.
            _ => recover::report_unexpected::<Src, Ctx>(inp, PATH_HEADS)?,
          }
          if !eat_if::<Src, Ctx>(inp, Kind::Ampersand)? {
            return Ok(());
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `where WherePredicate+`
  ///
  /// **Precondition: the head is an `Identifier` spelled `where`**, which every caller reads
  /// through [`peek_as`] before dispatching here; the keyword is consumed with a plain `expect`
  /// and not re-tested, the ruling Task 10 recorded for the reserved value spellings.
  ///
  /// The continuation test is the syntactic parser's two-token window, reproduced through
  /// [`peek_second_kind`](super::trivia::peek_second_kind); the module docs give the position that
  /// makes the second token load-bearing.
  fn where_clause<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::WhereClause.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        // The mandatory first predicate. Guarded rather than required, so a `where` with nothing
        // after it costs one diagnostic instead of the enclosing definition.
        match peek_kind::<Src, Ctx>(inp)? {
          Some(Kind::Identifier | Kind::PathSeparator) => where_predicate::<Src, Ctx>(inp)?,
          _ => return recover::report_unexpected::<Src, Ctx>(inp, PATH_HEADS),
        }
        // The two-token continuation test, reproduced exactly. Every arm that continues has an
        // `Identifier` or a `::` at the head, which `where_predicate`'s `type_path` consumes, so
        // the loop makes progress on every turn it takes.
        loop {
          let another = match peek_kind::<Src, Ctx>(inp)? {
            Some(Kind::Identifier) => matches!(
              peek_second_kind::<Src, Ctx>(inp)?,
              Some(Kind::Colon | Kind::PathSeparator | Kind::LAngle)
            ),
            Some(Kind::PathSeparator) => {
              matches!(peek_second_kind::<Src, Ctx>(inp)?, Some(Kind::Identifier))
            }
            _ => false,
          };
          if !another {
            return Ok(());
          }
          where_predicate::<Src, Ctx>(inp)?;
        }
      },
    )
    .parse_input(inp)
  }

  /// [`where_clause`] when a `where` is here, and nothing at all when it is not.
  ///
  /// The spelling test is the atom that reads it — `where` is an ordinary identifier token to the
  /// lexer — and it happens **outside** any node this opens, so a definition with no clause keeps
  /// the trivia the peek crossed in its own node rather than in a `WhereClause` that should not
  /// exist. `Directives`' absent-run rule, applied to the other undelimited optional shape.
  fn optional_where_clause<'inp, Src, Ctx>(inp) {
    if peek_as::<Src, Ctx, Keyword>(inp)? == Some(Keyword::Where) {
      return where_clause::<Src, Ctx>(inp);
    }
    Ok(())
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one generic or `where` production over a `&str` and hand back the tree it
  /// built, for `tests/lossless_x_selection.rs`.
  mod test_support;

  /// `super::definition_type_generics` over `src` — the list whose members are nodes.
  fn parse_definition_type_generics => definition_type_generics;

  /// `super::definition_name` over `src` — the name an operation and (from Task 12) every SDL
  /// definition reaches.
  fn parse_definition_name => definition_name;

  /// `super::executable_definition_type_generics` over `src` — the list whose members are bare
  /// name tokens.
  fn parse_executable_definition_type_generics => executable_definition_type_generics;

  /// `super::executable_definition_name` over `src`.
  fn parse_executable_definition_name => executable_definition_name;

  /// `super::where_predicate` over `src`, on its own — the production a malformed clause would
  /// otherwise only be observable through.
  fn parse_where_predicate => where_predicate;

  /// `super::where_clause` over `src` — the committed form, whose `where` is already known to be
  /// there.
  fn parse_where_clause => where_clause;
}
