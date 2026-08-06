//! Typed wrappers over the six selection node kinds.
//!
//! # `TypeCondition` **is** a node here, where GraphQL's kind space has none
//!
//! GraphQL's lossless suite lets a condition surface as the `NamedType` after the `on`, its space
//! having no image for one; GraphQLx's does, so the keyword and the [`TypePath`] after it are one
//! region and [`InlineFragment::type_condition`] answers a [`TypeCondition`] node rather than the
//! type inside it. The extra level is worth naming because it is where the `on` lives: a formatter
//! that wants to move or drop the keyword has a node to point at.
//!
//! # There is no `Name` node, so a field's name is a token
//!
//! [`Name`](K::Name) is a token image, which is why [`Field::name`] is a `tok` getter and why the
//! [`Alias`] level matters: an aliased field puts *two* `Name` tokens in play and only the second
//! is the field's own. The first belongs to the [`Alias`] node, so a direct-children scan answers
//! correctly and a descendant scan would not.
//!
//! A spread's target and an inline fragment's condition are both [`TypePath`]s — divergence 11 —
//! so `... ns::F<Int>` is one spread rather than a spread followed by junk, and the getter reaches
//! a node where GraphQL's reaches a token.

use crate::{
  ast_node,
  graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      directive::{Arguments, Directives},
      ty::TypePath,
    },
  },
};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A field alias, `name :`.
  ///
  /// A retro-wrap: both an alias and a bare field name start with a `Name`, and only the `:` after
  /// it tells them apart, so this node is opened once that `:` has been seen.
  Alias => K::Alias {
    /// The alias itself — the name the result is reported under.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A field selection.
  Field => K::Field {
    /// The field's alias, if it was given one.
    alias: opt Alias,
    /// The field's own name.
    ///
    /// An aliased field's *first* `Name` token belongs to its [`Alias`] node, so this answers the
    /// field's real name in both spellings.
    name: tok K::Name,
    /// The field's arguments, if it was given any.
    arguments: opt Arguments,
    /// The field's directives, if it was given any.
    directives: opt Directives,
    /// The field's own selection set, if it selects anything.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A type condition, `on T` — a node of its own in this dialect.
  ///
  /// The `on` is a bare [`Name`](K::Name) token inside it, this kind space having no per-keyword
  /// image, and it takes no getter: there is nothing to name that the node does not already say.
  TypeCondition => K::TypeCondition {
    /// The type the condition names.
    type_path: opt TypePath,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A fragment spread, `... F`.
  FragmentSpread => K::FragmentSpread {
    /// The spread fragment's name — a whole path, so `... ::ns::F` is one spread.
    type_path: opt TypePath,
    /// The spread's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// An inline fragment, `... on T { … }`.
  InlineFragment => K::InlineFragment {
    /// The fragment's type condition, if it has one — a node here, unlike in GraphQL.
    type_condition: opt TypeCondition,
    /// The fragment's directives, if it was given any.
    directives: opt Directives,
    /// The fragment's selection set.
    selection_set: opt SelectionSet,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A selection set, `{ … }`.
  ///
  /// A selection is a three-way union with no wrapper of its own, so the three kinds come back
  /// through three iterators; a consumer needing them in written order walks
  /// [`syntax`](SelectionSet::syntax) instead.
  SelectionSet => K::SelectionSet {
    /// Every field selected, in order.
    fields: many Field,
    /// Every fragment spread in the set, in order.
    fragment_spreads: many FragmentSpread,
    /// Every inline fragment in the set, in order.
    inline_fragments: many InlineFragment,
  }
);
