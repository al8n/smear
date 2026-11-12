//! Paths and type references: `Path`, `TypeGenerics`, `TypePath`, `DefinitionTypePath` and
//! `ListType`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # `!` is folded into the node it follows — there is no non-null wrapper
//!
//! GraphQL's lossless CST has a `NonNullType` node that wraps the type it modifies. **GraphQLx's
//! kind space has no such kind, and that is a derived fact rather than a preference.**
//! `graphqlx/syntactic/ty.rs:215-229` builds `DefinitionTypePath` and `ListType` each carrying its
//! own `required: bool`; nothing in the GraphQLx grammar or AST names a non-null node, so Task 8's
//! census — which fails on a kind that names nothing in GraphQLx's own sources — could not have
//! admitted one.
//!
//! The consequence for this file is concrete: the `!` is a [`Bang`](K::Bang) **token child of the
//! type node**, so every type production consumes its own optional `!` from *inside* its `node(…)`
//! closure. A production that ate the `!` outside its node would leave it as a sibling, which
//! round-trips identically and is wrong about which type is non-null.
//!
//! # A path commits its leading trivia before its node opens
//!
//! `node(…)` opens the node and *then* runs the inner parser, so an `expect`'s trivia skip would
//! run inside a node that is already open and the leading trivia would land in the node's range — a
//! `Path` spanning `" Q"` rather than `"Q"`. Phase A shipped exactly that bug for `NamedType` and
//! had to re-bless six goldens to fix it. Every production here that can be reached without a
//! dispatch peek in front of it opens with `peek_kind`, which is the atom set's only door to the
//! skip; where the caller has already peeked, the skip finds nothing and the peek is served out of
//! the cache.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphqlx::kinds::SyntaxKind as K;

use super::coverage::node;

use super::{
  GraphqlxLosslessInput, recover,
  recover::{PATH_HEADS, TYPE_HEADS, is_closer, opener_span, starts_type},
  trivia::{eat_if, expect, peek_kind},
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// The `:: Name` suffixes of a path, with the first name already consumed.
  ///
  /// Opens no node: it is the tail of whichever node its caller opened. Two callers — [`path`] and
  /// `value.rs`'s `collection_or_enum`, which has to consume a `set`/`map` keyword before it can
  /// know whether a path is being built at all.
  fn path_tail<'inp, Src, Ctx>(inp) {
    while eat_if::<Src, Ctx>(inp, Kind::PathSeparator)? {
      expect::<Src, Ctx>(inp, Kind::Identifier)?;
    }
    Ok(())
  }

  /// A path's root — `~ ::`, `^ ::`, or `.. ::` once per level — or nothing. Opens no node: the
  /// root's tokens are leading token children of the [`path`] that roots them.
  ///
  /// A fixed pair of tokens per sigil, the `.. ::` pairs read in a loop rather than by recursion, so
  /// a long chain costs no stack; their count is bounded by the input, as the `:: Name` suffixes
  /// after them are. A sigil without its `::` is the `expect`'s to refuse.
  fn path_root<'inp, Src, Ctx>(inp) {
    if eat_if::<Src, Ctx>(inp, Kind::Tilde)? || eat_if::<Src, Ctx>(inp, Kind::Caret)? {
      return expect::<Src, Ctx>(inp, Kind::PathSeparator);
    }
    while eat_if::<Src, Ctx>(inp, Kind::DotDot)? {
      expect::<Src, Ctx>(inp, Kind::PathSeparator)?;
    }
    Ok(())
  }

  /// `PathRoot? Name (:: Name)*`
  ///
  /// **The root is part of the path, not a prefix operator on it.** `graphqlx::ast::Path` carries a
  /// `PathRoot`, and the tree records the same fact by keeping the root's tokens as the path's
  /// leading children: a consumer reads which root the path has off whether its first token is a
  /// `Tilde`, a `Caret` or a `DotDot`. That is why there is no separate node for the root — a node
  /// whose entire content is the root's two tokens says nothing the tokens do not.
  ///
  /// **A `::` is not a head.** It only joins two segments, so a path never starts with one, and a
  /// caller that dispatches here on a `::` gets the `expect`'s refusal of a missing name.
  fn path<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::Path.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        // Optional, and its absence is not an error: `Colour` and `^::geo::Colour` are both paths.
        path_root::<Src, Ctx>(inp)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        path_tail::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `< Type (Type)* >` — the arguments applied to a path.
  ///
  /// Nonempty: `graphqlx/syntactic/ty.rs:86` is `.at_least(1)`, so `Foo<>` is an error rather than
  /// a path with no arguments. The first element is parsed unconditionally and the loop handles the
  /// rest, which is what makes the emptiness a diagnostic instead of a silently accepted shape.
  fn type_generics<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::TypeGenerics.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LAngle)?;
        let open = opener_span(inp.span().end());
        // The mandatory first argument. `type_ref` reports a closer that stands where a type
        // should and leaves it, so an empty `<>` reports here and the list closes on its own `>`.
        type_ref::<Src, Ctx>(inp)?;
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RAngle)? {
            return Ok(());
          }
          match peek_kind::<Src, Ctx>(inp)? {
            None => return recover::unclosed_angle::<Src, Ctx>(inp, open),
            Some(head) if starts_type(head) => type_ref::<Src, Ctx>(inp)?,
            Some(_) => recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS)?,
          }
        }
      },
    )
    .parse_input(inp)
  }

  /// `Path TypeGenerics?` in a **non-type** position.
  ///
  /// A directive's name, an interface, a union member, a type condition, a fragment spread's
  /// target, a `where` bound — every place GraphQL would write a `NamedType`. It carries no `!`,
  /// which is the whole difference from [`definition_type_path`]: none of those positions admits
  /// one, so a `!` after a `TypePath` is left for the caller to recover.
  fn type_path<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::TypePath.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        path::<Src, Ctx>(inp)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle) {
          type_generics::<Src, Ctx>(inp)?;
        }
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `Path TypeGenerics? '!'?` — a type reference's path head.
  ///
  /// The `!` is consumed **inside** the node, so it is a child of the type it modifies. See the
  /// module docs for why there is no wrapper node to put it under instead.
  fn definition_type_path<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::DefinitionTypePath.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        path::<Src, Ctx>(inp)?;
        if peek_kind::<Src, Ctx>(inp)? == Some(Kind::LAngle) {
          type_generics::<Src, Ctx>(inp)?;
        }
        eat_if::<Src, Ctx>(inp, Kind::Bang)?;
        Ok(())
      },
    )
    .parse_input(inp)
  }

  /// `[ Type ] '!'?`
  ///
  /// Exactly one inner type, then the closer, then the optional `!`. The loop exists only for what
  /// may sit *between* the element and the closer: it consumes nothing on the well-formed path, and
  /// on the malformed one it attributes each junk region to an `Error` node until the `]` or end of
  /// input arrives. `unexpected` consumes at least one token whenever input remains, which is this
  /// loop's whole termination argument.
  fn list_type<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    node(
      K::ListType.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LBracket)?;
        let open = opener_span(inp.span().end());
        type_ref::<Src, Ctx>(inp)?;
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RBracket)? {
            eat_if::<Src, Ctx>(inp, Kind::Bang)?;
            return Ok(());
          }
          if peek_kind::<Src, Ctx>(inp)?.is_none() {
            return recover::unclosed_list::<Src, Ctx>(inp, open);
          }
          recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS)?;
        }
      },
    )
    .parse_input(inp)
  }

  /// `DefinitionTypePath | ListType`
  ///
  /// **Five heads, against GraphQL's two** (`graphqlx/syntactic/ty.rs:38-49`): a path's name, one of
  /// its three root sigils `~`, `..` and `^`, or a list's `[`. Opens no node of its own — the chosen
  /// production opens, and closes, its own, `!` included.
  fn type_ref<'inp, Src, Ctx>(inp) {
    // The head peek first, so the leading trivia it crosses is committed BEFORE any node opens and
    // therefore lands outside it.
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier | Kind::Tilde | Kind::DotDot | Kind::Caret) => {
        definition_type_path::<Src, Ctx>(inp)
      }
      Some(Kind::LBracket) => list_type::<Src, Ctx>(inp),
      // No type starts here, and the head closes a block: the block this position stands in.
      // Reported and left, so that block closes on it and what follows the block is still parsed.
      // Every caller has consumed a token of its own before it asks for a type, and goes on to a
      // loop that tests its closer first, so leaving the head costs no progress.
      Some(head) if is_closer(head) => recover::report_unexpected::<Src, Ctx>(inp, TYPE_HEADS),
      // No type starts here. Report and consume, exactly as `value`'s dispatcher does: an `Err`
      // would abort every enclosing production.
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS),
    }
  }

  /// A path where a path — and not a type — is what the grammar wants, with recovery.
  ///
  /// [`path`]'s `expect` returns `Err` on a head that is neither a name nor a root's sigil, which
  /// aborts every enclosing production. This is the door a *list* of paths uses, where one bad member
  /// must cost one `Error` node and not the list.
  // No list production names it yet, so its driver is its only caller and the gate takes it —
  // see `lossless_drivers!` on why the allow is narrowed to the gate rather than written bare.
  #[cfg_attr(not(feature = "test-support"), allow(dead_code))]
  fn path_or_recover<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier | Kind::Tilde | Kind::DotDot | Kind::Caret) => path::<Src, Ctx>(inp),
      _ => recover::unexpected::<Src, Ctx>(inp, PATH_HEADS),
    }
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one path or type production over a `&str` and hand back the tree it built,
  /// for `tests/lossless_x_value.rs`.
  mod test_support;

  /// `super::type_ref` over `src` — the entry every type position uses, and the only door to
  /// `definition_type_path` and `list_type`.
  fn parse_type_ref => type_ref;

  /// `super::type_path` over `src` — the non-type path position, which carries no `!`.
  fn parse_type_path => type_path;

  /// `super::path` over `src` — the bare path, with no generics and no `!`.
  fn parse_path => path;

  /// `super::type_generics` over `src` — the `<…>` argument list on its own, which no valid
  /// document ever starts with and which therefore has no other entry point.
  fn parse_type_generics => type_generics;

  /// `super::path_or_recover` over `src` — the recovering path door a list of paths uses.
  fn parse_path_or_recover => path_or_recover;
}
