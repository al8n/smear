//! Typed wrappers over the five selection node kinds.
//!
//! # Two places the tree does not have the node a reader expects
//!
//! - **There is no `TypeCondition` node.** `apollo-parser` wraps `on NamedType` in one; this kind
//!   space has no image for it, so an [`InlineFragment`]'s condition surfaces as the
//!   [`NamedType`] after the `on` keyword. [`InlineFragment::type_condition`] is that getter,
//!   named for the grammar production rather than for the node kind it happens to reach.
//! - **There is no `Name` node.** `K::Name` is a token image, so a field's name is a token and
//!   not a child — which is why [`Field::name`] is a `tok` getter and why the [`Alias`] level
//!   matters: an aliased field puts *two* `Name` tokens in play, and only the second is the
//!   field's own. The first belongs to the `Alias` node, so a direct-children scan answers
//!   correctly and a descendant scan would not.

use crate::{
  ast_node,
  parser::graphql::{
    kinds::SyntaxKind as K,
    lossless::ast::{
      directive::{Arguments, Directives},
      ty::NamedType,
    },
  },
};

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A field alias, `name :`.
  ///
  /// A retro-wrap: both an alias and a bare field name start with a `Name`, and only the `:`
  /// after it tells them apart, so this node is opened once that `:` has been seen.
  Alias => K::Alias {
    /// The alias itself — the name the result is reported under.
    name: tok K::Name,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A field selection.
  Field => K::Field {
    /// The field's alias, if it was given one.
    alias: opt Alias,
    /// The field's own name.
    ///
    /// An aliased field's *first* `Name` token belongs to its [`Alias`] node, so this answers
    /// the field's real name in both spellings.
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
  lang = crate::parser::graphql::kinds::GraphQLLang;
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

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// A fragment spread, `... FragmentName`.
  FragmentSpread => K::FragmentSpread {
    /// The spread fragment's name.
    name: tok K::Name,
    /// The spread's directives, if it was given any.
    directives: opt Directives,
  }
);

ast_node!(
  lang = crate::parser::graphql::kinds::GraphQLLang;
  /// An inline fragment, `... on T { … }`.
  InlineFragment => K::InlineFragment {
    /// The type condition's type, if the fragment has a condition.
    ///
    /// There is no `TypeCondition` node in this kind space — see the module docs — so this is the
    /// [`NamedType`] the `on` keyword introduces.
    type_condition: opt NamedType,
    /// The fragment's directives, if it was given any.
    directives: opt Directives,
    /// The fragment's selection set.
    selection_set: opt SelectionSet,
  }
);
