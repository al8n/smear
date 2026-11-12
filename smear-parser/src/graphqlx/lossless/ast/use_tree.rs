//! Typed wrappers over the two `use` node kinds — GraphQLx only.
//!
//! GraphQL has no `use` item at all, so nothing here is a port.
//!
//! # Where a tree's parts are
//!
//! A [`UseTree`] holds its path as a [`Path`] child, so a rooted path's root is read through
//! [`Path::tilde_token`], [`Path::caret_token`] and [`Path::dot_dot_tokens`] as anywhere else. A
//! **bare** root — `use ~::*`, `use ..::{A}` — has no path to lead, and its tokens are the tree's
//! own: the tree's [`tilde_token`](UseTree::tilde_token), [`caret_token`](UseTree::caret_token)
//! and [`dot_dot_tokens`](UseTree::dot_dot_tokens) answer for those, and only for those.
//!
//! A string source is a [`StringValue`] child, [`source`](UseTree::source); the names written
//! after it are the tree's [`Path`] child, relative to the module the string names.
//!
//! # The tree's own names are an `as` and its alias, read as one run
//!
//! Every name of a path is inside its [`Path`], so the only [`Name`](K::Name) tokens a tree holds
//! directly are an `as` and the alias written after it. They come back through one getter,
//! [`names`](UseTree::names), in that order: two getters would have to tell two tokens of one kind
//! apart by position, and this dialect has no positional getter.
//!
//! # A group's trees are the tree's children
//!
//! `UseTreeKind` is a choice and has no node, and neither has a group: its braces and its trees
//! are direct children of the tree that holds them, so [`use_trees`](UseTree::use_trees) answers
//! the members of a group and nothing else.

use crate::{
  ast_node,
  graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{ty::Path, value::StringValue},
  },
};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// One use tree: a bare root, a path or a string source, then `as Name`, `:: *` or
  /// `:: { UseTree+ }`.
  UseTree => K::UseTree {
    /// The `~` of a bare package root, `use ~::*`. A rooted path's `~` is its [`Path`]'s.
    tilde_token: tok K::Tilde,
    /// The `^` of a bare dependency root, `use ^::{a, b}`. A rooted path's `^` is its [`Path`]'s.
    caret_token: tok K::Caret,
    /// The `..` of a bare parent-module root, one per level of `..::`.
    dot_dot_tokens: toks K::DotDot,
    /// The tree's own `::`s, in order: a bare root's, the one after a string source, and the one
    /// in front of a glob or a group. The `::`s between a path's names are its [`Path`]'s.
    path_separators: toks K::PathSeparator,
    /// The string source, when the tree starts with one.
    ///
    /// The grammar narrows this to an *inline* string, and the narrowing is the token's: a block
    /// string is reported and still built as a [`StringValue`], so a consumer checking the rule
    /// reads [`string_token`](StringValue::string_token)`().kind()`.
    source: opt StringValue,
    /// The tree's path — its whole prefix, or the names after its string source.
    path: opt Path,
    /// The `*` of a glob.
    asterisk_token: tok K::Asterisk,
    /// A group's `{`.
    l_brace_token: tok K::LBrace,
    /// Every tree a group holds, in order.
    use_trees: many UseTree,
    /// A group's `}`.
    r_brace_token: tok K::RBrace,
    /// The tree's own names: an `as`, then the alias it introduces — see this module's docs.
    names: toks K::Name,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A `use` item, `use UseTree`.
  ///
  /// **Never described.** A description may precede only an executable or a type-system
  /// definition, so a string written in front of a `use` item stays outside this node and is
  /// reported — which is why there is no `description` getter here and there is one on every
  /// definition.
  UseDefinition => K::UseDefinition {
    /// The `use` keyword.
    use_token: tok K::Name,
    /// The item's tree.
    use_tree: opt UseTree,
  }
);
