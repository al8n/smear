//! Paths and type references: `Path`, `TypeGenerics`, `TypePath`, `DefinitionTypePath`,
//! `ListType`, `SetType` and `MapType`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # `!` is folded into the node it follows — there is no non-null wrapper
//!
//! GraphQL's lossless CST has a `NonNullType` node that wraps the type it modifies. **GraphQLx's
//! kind space has no such kind, and that is a derived fact rather than a preference.**
//! `graphqlx/syntactic/ty.rs:250-261` builds `DefinitionTypePath`, `ListType`, `SetType` and
//! `MapType` each carrying its own `required: bool`; nothing in the GraphQLx grammar or AST names a
//! non-null node, so Task 8's census — which fails on a kind that names nothing in GraphQLx's own
//! sources — could not have admitted one.
//!
//! The consequence for this file is concrete: the `!` is a [`Bang`](K::Bang) **token child of the
//! type node**, so every type production consumes its own optional `!` from *inside* its `node(…)`
//! closure. A production that ate the `!` outside its node would leave it as a sibling, which
//! round-trips identically and is wrong about which type is non-null.
//!
//! # The angle form is the one shape that cannot open its node up front
//!
//! `< T >` is a set and `< K => V >` is a map, and the two are told apart only *after* the first
//! inner type has been parsed and a `=>` has been probed for (`ty.rs:182-192`). The kind is
//! therefore unknown when the `<` is committed, which is what [`tokora::parser::node_at`] is for:
//! mint an inert mark before the `<`, parse until the answer is known, then retro-wrap at the mark
//! with the kind that won. GraphQL uses the same mechanism for the opposite reason — its unknown is
//! a *suffix*, this one's is a *separator* — and it is the same three events either way.
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

use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, recover,
  recover::{PATH_HEADS, TYPE_HEADS, opener_span, starts_type},
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

  /// `::? Name (:: Name)*`
  ///
  /// **The leading `::` is part of the path, not a prefix operator on it.** `graphqlx::ast::Path`
  /// carries `fully_qualified: bool`, and the tree records the same fact by keeping the token: a
  /// consumer asks whether the path's first child is a `PathSeparator`. That is why there is no
  /// separate node for the qualifier — a node whose entire content is one token says nothing the
  /// token does not.
  fn path<'inp, Src, Ctx>(inp) {
    peek_kind::<Src, Ctx>(inp)?;
    node(
      K::Path.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        // Optional, and its absence is not an error: `Colour` and `::Colour` are both paths.
        eat_if::<Src, Ctx>(inp, Kind::PathSeparator)?;
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        path_tail::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `< Type (Type)* >` — the arguments applied to a path.
  ///
  /// Nonempty: `graphqlx/syntactic/ty.rs:87` is `.at_least(1)`, so `Foo<>` is an error rather than
  /// a path with no arguments. The first element is parsed unconditionally and the loop handles the
  /// rest, which is what makes the emptiness a diagnostic instead of a silently accepted shape.
  fn type_generics<'inp, Src, Ctx>(inp) {
    node(
      K::TypeGenerics.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::LAngle)?;
        let open = opener_span(inp.span().end());
        // The mandatory first argument. `type_ref` recovers on a head that starts no type, so an
        // empty `<>` reports here and the list still closes on its own delimiter.
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

  /// `< Type > '!'?` or `< Type => Type > '!'?` — the one production whose node kind is decided
  /// after its first token is committed.
  ///
  /// Accept a `=>` after the first inner type and it is a [`MapType`](K::MapType); decline and it
  /// is a [`SetType`](K::SetType) (`graphqlx/syntactic/ty.rs:182-192`). The mark is minted before
  /// the `<` so the retro-wrap covers the opener, and the closer and the `!` are parsed **inside**
  /// the wrap so they are children of the node rather than siblings of it.
  ///
  /// The caller has already crossed the leading trivia at its dispatch peek, so the mark is minted
  /// after that trivia is committed and the node starts at its own `<`.
  fn set_or_map_type<'inp, Src, Ctx>(inp) {
    let mark = inp.cst_mark();
    expect::<Src, Ctx>(inp, Kind::LAngle)?;
    let open = opener_span(inp.span().end());
    type_ref::<Src, Ctx>(inp)?;
    let is_map = eat_if::<Src, Ctx>(inp, Kind::FatArrow)?;
    if is_map {
      type_ref::<Src, Ctx>(inp)?;
    }
    let kind = if is_map { K::MapType } else { K::SetType };
    node_at(
      mark,
      kind.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        loop {
          if eat_if::<Src, Ctx>(inp, Kind::RAngle)? {
            eat_if::<Src, Ctx>(inp, Kind::Bang)?;
            return Ok(());
          }
          if peek_kind::<Src, Ctx>(inp)?.is_none() {
            return recover::unclosed_angle::<Src, Ctx>(inp, open);
          }
          recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS)?;
        }
      },
    )
    .parse_input(inp)
  }

  /// `DefinitionTypePath | ListType | SetType | MapType`
  ///
  /// **Four heads, against GraphQL's two** (`graphqlx/syntactic/ty.rs:42-50`): a path's name, a
  /// path's leading `::`, a list's `[`, or a set/map's `<`. Opens no node of its own — the chosen
  /// production opens, and closes, its own, `!` included.
  fn type_ref<'inp, Src, Ctx>(inp) {
    // The head peek first, so the leading trivia it crosses is committed BEFORE any node opens and
    // therefore lands outside it.
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier | Kind::PathSeparator) => definition_type_path::<Src, Ctx>(inp),
      Some(Kind::LBracket) => list_type::<Src, Ctx>(inp),
      Some(Kind::LAngle) => set_or_map_type::<Src, Ctx>(inp),
      // No type starts here. Report and consume, exactly as `value`'s dispatcher does: an `Err`
      // would abort every enclosing production.
      _ => recover::unexpected::<Src, Ctx>(inp, TYPE_HEADS),
    }
  }

  /// A path where a path — and not a type — is what the grammar wants, with recovery.
  ///
  /// [`path`]'s `expect` returns `Err` on a head that is neither a name nor a `::`, which aborts
  /// every enclosing production. This is the door a *list* of paths uses, where one bad member
  /// must cost one `Error` node and not the list.
  fn path_or_recover<'inp, Src, Ctx>(inp) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Identifier | Kind::PathSeparator) => path::<Src, Ctx>(inp),
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
  /// `definition_type_path`, `list_type` and the set/map retro-wrap.
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
