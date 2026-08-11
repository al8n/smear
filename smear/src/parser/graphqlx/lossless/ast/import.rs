//! Typed wrappers over the four import node kinds — GraphQLx only.
//!
//! GraphQL has no import statement at all, so nothing here is a port.
//!
//! # `import`, `from` and `as` are bare tokens with no getters
//!
//! All three are contextual keywords — ordinary [`Name`](K::Name) tokens to the lexer — and each
//! is a direct token child of the node it opens. None gets a getter: a keyword whose presence the
//! node's own kind already implies has nothing to tell a consumer, and a `tok K::Name` getter on
//! [`ImportDefinition`] would answer `import` while looking like it answered something useful.
//!
//! [`NamedSpecifier::name`] is the one place a `Name` getter is real, and it is sharp because the
//! `as` that may follow is **another** `Name` token under the same node: `cast::token` is
//! first-match, and the specifier's own name is written first.
//!
//! # A clause is a choice, so an import carries both getters and answers one
//!
//! `ImportClause` has no node kind — the census's *a choice is not a region* — so
//! [`ImportDefinition`] exposes [`import_list`](ImportDefinition::import_list) and
//! [`wildcard_specifier`](ImportDefinition::wildcard_specifier) and exactly one of them answers
//! `Some`. `import * as ns from "x"` is a whole-module import renamed into `ns`, and it reaches
//! the same [`WildcardSpecifier`] a list member does.

use crate::{
  ast_node,
  parser::graphqlx::{
    kinds::SyntaxKind as K,
    lossless::ast::{ty::Path, value::StringValue},
  },
};

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// One named import member, `Name (as Path)?`.
  NamedSpecifier => K::NamedSpecifier {
    /// The imported name — the **first** `Name` token, the `as` after it being another; see this
    /// module's docs.
    name: tok K::Name,
    /// The path the member is renamed to, if an `as` clause was written.
    ///
    /// A whole path and not a name: `A as ns::B` renames into a namespace.
    alias: opt Path,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A wildcard import member or clause, `* (as Path)?`.
  ///
  /// The same node in both positions: a member of a list, and a whole clause.
  WildcardSpecifier => K::WildcardSpecifier {
    /// The path the wildcard is renamed to, if an `as` clause was written.
    alias: opt Path,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// A braced import list, `{ ImportMember+ }`.
  ///
  /// A member is a choice between the two specifiers, so the two come back through two iterators;
  /// a consumer needing them in written order walks [`syntax`](ImportList::syntax) instead.
  ImportList => K::ImportList {
    /// Every named member of the list, in order.
    named_specifiers: many NamedSpecifier,
    /// Every wildcard member of the list, in order.
    wildcard_specifiers: many WildcardSpecifier,
  }
);

ast_node!(
  lang = crate::parser::graphqlx::kinds::GraphQLxLang;
  /// An import statement, `import ImportClause from "source"`.
  ///
  /// **Never described.** A description may precede only an executable or a type-system
  /// definition, so a string written in front of an import stays outside this node and is
  /// reported — which is why there is no `description` getter here and there is one on every
  /// definition.
  ImportDefinition => K::ImportDefinition {
    /// The clause, when it is a braced list.
    import_list: opt ImportList,
    /// The clause, when it is a bare wildcard.
    wildcard_specifier: opt WildcardSpecifier,
    /// The module the members are imported from.
    ///
    /// The grammar narrows this to an *inline* string, and the narrowing is the token's: a block
    /// string is reported and still built as a [`StringValue`], so the node is the same either way
    /// and a consumer checking the rule reads
    /// [`string_token`](StringValue::string_token)`().kind()`.
    source: opt StringValue,
  }
);
