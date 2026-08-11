//! The typed accessor layer over the GraphQL lossless CST.
//!
//! **The substrate is tokora's, and the language-generic layer over it is
//! [`crate::parser::lossless::ast`].** [`CastNode`] is a one-method trait — a kind check and a wrap — and
//! [`cast::child`], [`cast::children`] and [`NodeChildren`] are bound on it rather than on
//! tokora's parser-facing `Node`, so a wrapper whose entire job is `field.name()` never names
//! the `Syntax` component model. That is a deliberate upstream split (tokora 0.8.0, PR #132),
//! and this module takes the navigation side of it.
//!
//! What is smear's own is the [`ast_node!`](crate::ast_node) macro: fifty-nine wrappers with
//! identical bodies is a code-generation problem, not a trait-design one. The macro and its four
//! helpers now live at the crate root, parameterised by `rowan::Language`; **everything left in
//! this module is the pinning of that language to this dialect**, so a wrapper file's only
//! dialect statement is its `lang =` line.
//!
//! # Two token getters the substrate has to write itself, and why *this* grammar needs them
//!
//! `tokora::cst::cast` offers exactly [`child`](cast::child), [`children`](cast::children) and
//! [`token`](cast::token), and [`token`](cast::token) is *one kind, first match*. Two shapes in
//! this grammar fall outside that, so `token_any` and `tokens` exist in
//! [`crate::parser::lossless::ast`] as a filter over direct children and nothing more. They are not a
//! divergence from the upstream model; they are the two members of it that do not exist yet. The
//! reasons below are **this dialect's**, which is why they are recorded here rather than beside
//! the generic implementations.
//!
//! - **One getter, several token kinds.** A GraphQL string literal has two images —
//!   [`String`](crate::parser::graphql::kinds::SyntaxKind::String) for `"s"` and
//!   [`BlockString`](crate::parser::graphql::kinds::SyntaxKind::BlockString) for `"""s"""` — and two node
//!   kinds wrap one directly: `StringValue` and `Description`. Splitting that into two getters
//!   with an `.or()` at every call site pushes a lexical detail onto every consumer.
//! - **Several tokens of one kind.** `DirectiveLocations` holds its locations as bare
//!   [`Name`](crate::parser::graphql::kinds::SyntaxKind::Name) tokens, a location having no node kind of
//!   its own, so `"| FIELD | QUERY"` is two `Name` tokens of which [`cast::token`] can reach
//!   exactly one.
//!
//! [`tokens`] also answers a third shape the plan did not anticipate and which is far more
//! common than either: **this kind space has no `Name` node and no per-keyword token kind**, so a
//! definition's keyword and its name are both `Name` tokens under the same parent. `type T`,
//! `fragment F on T` and `extend type T` all put the name *after* one or two keywords, and
//! [`ast_node!`](crate::ast_node)'s `tok_nth` arm is what reaches it.

pub use crate::parser::lossless::ast::{CastNode, NodeChildren, cast};

use crate::parser::graphql::{
  kinds::SyntaxKind,
  lossless::{GraphQLLang, SyntaxNode, SyntaxToken},
};

// The wrappers are grouped by grammar area, one module per production file, and every one of them
// is re-exported flat below: a consumer names `ast::Field`, not `ast::selection::Field`. The
// modules stay public so a reader can find a wrapper beside the production that builds it.
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod selection;
pub mod ty;
pub mod value;

pub use definition::*;
pub use directive::*;
pub use document::*;
pub use executable::*;
pub use selection::*;
pub use ty::*;
pub use value::*;

/// [`crate::parser::lossless::ast::AstChildren`] with this crate's language pinned.
///
/// A convenience alias and nothing more — the iterator, and its `Iterator` impl, are tokora's.
/// It exists so a `many` getter's return type carries one parameter instead of two, across every
/// wrapper [`ast_node!`](crate::ast_node) generates.
pub type AstChildren<N> = crate::parser::lossless::ast::AstChildren<N, GraphQLLang>;

/// [`crate::parser::lossless::ast::AstTokens`] with this crate's language pinned.
///
/// The token counterpart of [`AstChildren`], and unlike that one it is not an alias for an
/// upstream type: tokora's `cst` layer has no token iterator.
pub type AstTokens = crate::parser::lossless::ast::AstTokens<GraphQLLang>;

/// The first direct token child of `parent` whose kind is one of `kinds`.
///
/// [`cast::token`]'s multi-kind form. The scan is in **document order**, not in `kinds` order:
/// asking for the first `K::String` and falling back to the first `K::BlockString` would answer
/// the wrong token for a node that carried both, which is a difference no caller should have to
/// know about.
#[inline]
pub fn token_any(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<SyntaxToken> {
  crate::parser::lossless::ast::token_any(parent, kinds)
}

/// Every direct token child of `parent` with the given kind, in document order.
///
/// [`cast::token`]'s plural form. `cast::token` answers its *first* match and tokora has no
/// `cast::tokens`, so without this a node carrying several tokens of one kind — a
/// `DirectiveLocations`, a definition's keyword and name — exposes only one of them.
#[inline]
pub fn tokens(parent: &SyntaxNode, kind: SyntaxKind) -> AstTokens {
  crate::parser::lossless::ast::tokens(parent, kind)
}
