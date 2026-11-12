//! The `use` productions: `UseDefinition` and `UseTree`.
//!
//! The conventions are `value.rs`'s and are not repeated here.
//!
//! # Nothing in this file is a port
//!
//! GraphQL has no `use` item, so there is no production to fork and no `apollo-parser`
//! counterpart to diverge from. Every shape below is read off `graphqlx/syntactic/use_tree.rs`
//! directly, and one token decides at every step in both: after `use` a name, a root's sigil or
//! an inline string; after a root `*`, `{` or a name; after a path, or a source and its names, a
//! `::` into a name, `*` or `{`, or an `as`; anything else ends the tree.
//!
//! # Two node kinds for the whole grammar
//!
//! `UsePrefix` and `UseTreeKind` are **choices** — a root, a path or a source; a binding, a glob
//! or a group — and the tree records the alternative that won, so a node for either would wrap
//! exactly one child. A group is its braces and its trees, all children of the tree that holds it,
//! and `use` and `as` are **contextual keywords**, bare token children of the node they open. What
//! is left is [`UseDefinition`](K::UseDefinition) and [`UseTree`](K::UseTree). Inside a tree:
//!
//! - a path, rooted or not, is a [`Path`](K::Path) child, as in every other path position;
//! - a bare root's tokens are the tree's own leading tokens, there being no path for them to lead;
//! - a string source is a [`StringValue`](K::StringValue) child, and the names written after its
//!   `::` are a relative [`Path`](K::Path) child;
//! - the `::` in front of a glob or a group, the `*`, the braces, `as` and the alias are the tree's
//!   own tokens.
//!
//! # The one two-token peek, and what it decides
//!
//! A `::` after a path's name continues the path when a name follows it and leads to a glob or a
//! group when a `*` or a `{` does. The grammar decides that on the token after the `::`, as the
//! syntactic layer does; the tree has to know it **before** taking the `::`, because a
//! [`Path`](K::Path) ends where its last name ends and the sink cannot close a node behind a token
//! it has already committed. So a path's tail asks `peek_second_kind` — the atom the `where`
//! window uses for the same reason — and the answer only says which node the `::` lands in.
//!
//! A root has the opposite problem: its tokens come before anything says whether a path follows,
//! and a `..::` root is any number of them. They are committed as the tree's own and retro-wrapped
//! into a [`Path`](K::Path) when a name follows, over a mark taken before the first sigil.
//!
//! # What bounds the recursion
//!
//! A group is the only recursive position. It opens through `descend` before it reads its `{`, so
//! the parse's own nesting budget counts every level, and it is entered only once its `{` has been
//! peeked. A path's names and a root's `..::` are read in loops, each turn consuming two tokens.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::ParseInput as _;

use crate::graphqlx::kinds::SyntaxKind as K;

// `node` and `node_at` come from `coverage`, not from `tokora::combinator`. Behind
// `feature = "lossless-coverage"` they are those same combinators plus the per-node-kind hit
// counter gate 2 measures its reach with, so a production cannot open a node without being
// counted; without the feature they are tokora's own, re-exported unchanged.
use super::coverage::{node, node_at};

use super::{
  GraphqlxLosslessInput, Keyword, recover,
  recover::{
    NAME_HEADS, USE_ITEM_END_HEADS, USE_MEMBER_HEADS, USE_SCOPE_HEADS, USE_SOURCE_HEADS,
    USE_TREE_HEADS, opener_span,
  },
  trivia::{eat_if, expect, peek_as, peek_kind, peek_second_kind},
  ty::path_root,
  value::string_value,
};

use crate::lossless::{lossless_drivers, lossless_production};

lossless_production! {
  dialect = graphqlx::lossless;

  /// `(:: Name)*` after a use path's first name, for as long as a name follows each `::`.
  ///
  /// Opens no node: it is the tail of the [`Path`](K::Path) its caller opened. A `::` that no name
  /// follows is left for the tree, which reads it as the way into a glob or a group — see the
  /// module docs. Each turn consumes a `::` and a name.
  fn use_path_tail<'inp, Src, Ctx>(inp) {
    while peek_kind::<Src, Ctx>(inp)? == Some(Kind::PathSeparator)
      && peek_second_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier)
    {
      expect::<Src, Ctx>(inp, Kind::PathSeparator)?;
      expect::<Src, Ctx>(inp, Kind::Identifier)?;
    }
    Ok(())
  }

  /// `Name (:: Name)*` — a use tree's relative path: a group member's, an item's unrooted one, or
  /// the names after a string source.
  ///
  /// **Precondition: the head is an `Identifier`.** Every caller dispatches on it, and its peek is
  /// what commits the leading trivia so the node starts at its own first token.
  fn use_path<'inp, Src, Ctx>(inp) {
    node(
      K::Path.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        use_path_tail::<Src, Ctx>(inp)
      },
    )
    .parse_input(inp)
  }

  /// `(as Name)?` after a complete path, or after a source and its names.
  ///
  /// After a complete path `as` is always the keyword, so a tree whose item is named `as` writes
  /// it first in its group or straight after a `::`, where the grammar reads a name.
  fn alias<'inp, Src, Ctx>(inp) {
    if peek_as::<Src, Ctx, Keyword>(inp)? != Some(Keyword::As) {
      return Ok(());
    }
    expect::<Src, Ctx>(inp, Kind::Identifier)?;
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
      return expect::<Src, Ctx>(inp, Kind::Identifier);
    }
    // `as` with no name after it. Reported without consuming: whatever is here belongs to what
    // follows the tree.
    recover::report_unexpected::<Src, Ctx>(inp, NAME_HEADS)
  }

  /// An `as` after a `use` item's own glob or group.
  ///
  /// The grammar gives neither an alias, so the `as` is reported, and the alias is still built
  /// into the tree so that every token keeps its place. Inside a group nothing is read here at
  /// all: an `as` after a member's glob or group begins the next member.
  fn misplaced_alias<'inp, Src, Ctx>(inp, item: bool) {
    if !item || peek_as::<Src, Ctx, Keyword>(inp)? != Some(Keyword::As) {
      return Ok(());
    }
    recover::report_unexpected::<Src, Ctx>(inp, USE_ITEM_END_HEADS)?;
    alias::<Src, Ctx>(inp)
  }

  /// `{ UseTree+ }`. Opens no node: the braces and the trees are children of the tree that holds
  /// them.
  ///
  /// **Precondition: the head is a `{` its caller has peeked.** The frame is taken first, so the
  /// nesting budget counts every group before its `{` is read. Nonempty, as the syntactic group's
  /// `.at_least(1)` is, and the empty form is reported with the non-consuming recovery so the `}`
  /// stays the loop's.
  fn group<'inp, Src, Ctx>(inp) {
    let mut frame = super::descend::<Src, Ctx>(inp)?;
    let inp = &mut *frame;
    expect::<Src, Ctx>(inp, Kind::LBrace)?;
    let open = opener_span(inp.span().end());
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::RBrace) {
      recover::report_unexpected::<Src, Ctx>(inp, USE_MEMBER_HEADS)?;
    }
    loop {
      if eat_if::<Src, Ctx>(inp, Kind::RBrace)? {
        return Ok(());
      }
      match peek_kind::<Src, Ctx>(inp)? {
        None => return recover::unclosed_object::<Src, Ctx>(inp, open),
        Some(Kind::Identifier) => use_tree::<Src, Ctx>(inp, false)?,
        // A root or a string source starts no tree inside a group. Reported, and the tree is still
        // built, so its tokens keep their place.
        Some(
          Kind::Tilde | Kind::DotDot | Kind::Caret | Kind::InlineString | Kind::BlockString,
        ) => {
          recover::report_unexpected::<Src, Ctx>(inp, USE_MEMBER_HEADS)?;
          use_tree::<Src, Ctx>(inp, false)?;
        }
        // Consumes at least one token, which is this loop's termination argument.
        Some(_) => recover::unexpected::<Src, Ctx>(inp, USE_MEMBER_HEADS)?,
      }
    }
  }

  /// After a `::` that no name follows, or after a bare root: a glob or a group, then the `as`
  /// an item's tree may not take.
  fn glob_or_group<'inp, Src, Ctx>(inp, item: bool) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::Asterisk) => expect::<Src, Ctx>(inp, Kind::Asterisk)?,
      // The `{` is confirmed here, before the group is entered.
      Some(Kind::LBrace) => group::<Src, Ctx>(inp)?,
      // Reported without consuming: whatever is here belongs to what follows the tree.
      _ => return recover::report_unexpected::<Src, Ctx>(inp, USE_SCOPE_HEADS),
    }
    misplaced_alias::<Src, Ctx>(inp, item)
  }

  /// What follows a tree's path: a `::` into a glob or a group, or an alias.
  ///
  /// A `::` here is one [`use_path_tail`] left because no name follows it.
  fn after_path<'inp, Src, Ctx>(inp, item: bool) {
    if eat_if::<Src, Ctx>(inp, Kind::PathSeparator)? {
      return glob_or_group::<Src, Ctx>(inp, item);
    }
    alias::<Src, Ctx>(inp)
  }

  /// A tree that starts with a root: its tokens, then the path they root, a glob or a group.
  ///
  /// The root's tokens are read before anything says whether a path follows them, so they are
  /// committed as the tree's own and retro-wrapped into the [`Path`](K::Path) when a name does.
  fn rooted_tree<'inp, Src, Ctx>(inp, item: bool) {
    let mark = inp.cst_mark();
    path_root::<Src, Ctx>(inp)?;
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
      node_at(
        mark,
        K::Path.raw(),
        |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
          expect::<Src, Ctx>(inp, Kind::Identifier)?;
          use_path_tail::<Src, Ctx>(inp)
        },
      )
      .parse_input(inp)?;
      return after_path::<Src, Ctx>(inp, item);
    }
    glob_or_group::<Src, Ctx>(inp, item)
  }

  /// A tree whose prefix is a string source: the string, then the names after its `::` and what
  /// they lead to, or an alias.
  ///
  /// The grammar narrows the source to an **inline** string, and the narrowing is the token's: a
  /// block string is reported and still built as the same [`StringValue`](K::StringValue).
  fn source_tree<'inp, Src, Ctx>(inp, item: bool) {
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::BlockString) {
      recover::report_unexpected::<Src, Ctx>(inp, USE_SOURCE_HEADS)?;
    }
    string_value::<Src, Ctx>(inp)?;
    if !eat_if::<Src, Ctx>(inp, Kind::PathSeparator)? {
      return alias::<Src, Ctx>(inp);
    }
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Identifier) {
      use_path::<Src, Ctx>(inp)?;
      return after_path::<Src, Ctx>(inp, item);
    }
    glob_or_group::<Src, Ctx>(inp, item)
  }

  /// One use tree: a bare root, a path or a string source, then what it binds.
  ///
  /// **Precondition: the head is a name, a root's sigil or a string.** Every caller dispatches on
  /// it, and its peek commits the leading trivia before the node opens.
  ///
  /// `item` says whether the tree is a `use` item's own rather than a group's member, which only
  /// [`misplaced_alias`] reads.
  fn use_tree<'inp, Src, Ctx>(inp, item: bool) {
    node(
      K::UseTree.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| match peek_kind::<Src, Ctx>(inp)? {
        Some(Kind::InlineString | Kind::BlockString) => source_tree::<Src, Ctx>(inp, item),
        Some(Kind::Tilde | Kind::DotDot | Kind::Caret) => rooted_tree::<Src, Ctx>(inp, item),
        _ => {
          use_path::<Src, Ctx>(inp)?;
          after_path::<Src, Ctx>(inp, item)
        }
      },
    )
    .parse_input(inp)
  }

  /// `use UseTree`
  ///
  /// **Precondition: the head is an `Identifier` spelled `use`.** The document dispatchers decide
  /// that; the keyword is consumed with a plain `expect` and not re-tested.
  ///
  /// A `use` item carries **no description**: divergence 22 says a description may precede only
  /// an executable or a type-system definition, which is why this production takes no mark and is
  /// not a retro-wrap.
  fn use_definition<'inp, Src, Ctx>(inp) {
    node(
      K::UseDefinition.raw(),
      |inp: &mut GraphqlxLosslessInput<'inp, '_, Src, Ctx>| {
        expect::<Src, Ctx>(inp, Kind::Identifier)?;
        match peek_kind::<Src, Ctx>(inp)? {
          Some(
            Kind::Identifier
            | Kind::Tilde
            | Kind::DotDot
            | Kind::Caret
            | Kind::InlineString
            | Kind::BlockString,
          ) => use_tree::<Src, Ctx>(inp, true),
          // `use` with no tree after it. Reported without consuming: whatever is here belongs to
          // what follows the item.
          _ => recover::report_unexpected::<Src, Ctx>(inp, USE_TREE_HEADS),
        }
      },
    )
    .parse_input(inp)
  }
}

lossless_drivers! {
  dialect = graphqlx::lossless;

  /// Drivers that run one `use` production over a `&str` and hand back the tree it built, for
  /// `tests/lossless_x_use.rs`.
  mod test_support;

  /// `super::use_tree` over `src`, as a `use` item's own tree.
  fn parse_use_tree => use_tree [true];

  /// `super::use_definition` over `src` — the entry the document dispatchers use.
  fn parse_use_definition => use_definition;
}
