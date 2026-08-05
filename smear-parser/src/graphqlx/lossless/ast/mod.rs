//! The typed accessor layer over the GraphQLx lossless CST.
//!
//! **The substrate is tokora's, and the language-generic layer over it is
//! [`crate::lossless::ast`].** [`CastNode`] is a one-method trait — a kind check and a wrap — and
//! [`cast::child`], [`cast::children`] and [`NodeChildren`] are bound on it rather than on
//! tokora's parser-facing `Node`, so a wrapper whose entire job is `field.name()` never names the
//! `Syntax` component model. Everything in this module is the **pinning of that shared layer to
//! this dialect**, so a wrapper file's only dialect statement is its `lang =` line —
//! [`crate::graphql::lossless::ast`] is the same module for the other dialect, and the two share
//! every line of implementation and no line of type.
//!
//! # Three token getters, and why *this* grammar needs each
//!
//! `tokora::cst::cast` offers exactly [`child`](cast::child), [`children`](cast::children) and
//! [`token`](cast::token), and [`token`](cast::token) is *one kind, first match*. The reasons
//! below are **this dialect's**, which is why they are recorded here rather than beside the
//! generic implementations in [`crate::lossless::ast`].
//!
//! - **One getter, several token kinds** ([`token_any`]). A GraphQLx string literal has two images
//!   — [`InlineString`](crate::graphqlx::kinds::SyntaxKind::InlineString) for `"s"` and
//!   [`BlockString`](crate::graphqlx::kinds::SyntaxKind::BlockString) for `"""s"""`. One node kind
//!   wraps one directly ([`StringValue`]), and — unlike GraphQL — **six more node kinds hold one
//!   as a bare token**, because this kind space has no `Description` node: an
//!   [`OperationDefinition`], a [`FragmentDefinition`], a [`VariableDefinition`], an
//!   [`InputValueDefinition`], a [`FieldDefinition`] and an [`EnumValueDefinition`] each carry
//!   their description as a direct string token.
//! - **Several tokens of one kind** ([`tokens`]). [`Path`] holds its segments as bare
//!   [`Name`](crate::graphqlx::kinds::SyntaxKind::Name) tokens and its `::`s as bare
//!   [`PathSeparator`](crate::graphqlx::kinds::SyntaxKind::PathSeparator) tokens;
//!   [`DirectiveLocations`], [`ExtensionTypeGenerics`] and
//!   [`ExecutableDefinitionTypeGenerics`] each hold a whole list of bare `Name` tokens, their
//!   members having no node kind of their own.
//! - **A token at a known index** (`tok_nth`). **Not used anywhere in this dialect**, and that is
//!   a fact about GraphQLx's grammar rather than an omission — see
//!   [`definition`]'s module docs.
//!
//! [`OperationDefinition`]: executable::OperationDefinition
//! [`FragmentDefinition`]: executable::FragmentDefinition
//! [`VariableDefinition`]: executable::VariableDefinition
//! [`InputValueDefinition`]: definition::InputValueDefinition
//! [`FieldDefinition`]: definition::FieldDefinition
//! [`EnumValueDefinition`]: definition::EnumValueDefinition
//! [`DirectiveLocations`]: definition::DirectiveLocations
//! [`ExtensionTypeGenerics`]: generic::ExtensionTypeGenerics
//! [`ExecutableDefinitionTypeGenerics`]: generic::ExecutableDefinitionTypeGenerics
//! [`StringValue`]: value::StringValue
//! [`Path`]: ty::Path

pub use crate::lossless::ast::{CastNode, NodeChildren, cast};

use crate::graphqlx::{
  kinds::SyntaxKind,
  lossless::{GraphQLxLang, SyntaxNode, SyntaxToken},
};

// The wrappers are grouped by grammar area, one module per production file, and every one of them
// is re-exported flat below: a consumer names `ast::Field`, not `ast::selection::Field`. The
// modules stay public so a reader can find a wrapper beside the production that builds it — which
// is why there are ten of them and not the nine the GraphQL twin has: GraphQLx's productions put
// the seven type-system extensions in their own file, and the wrappers follow.
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod extension;
pub mod generic;
pub mod import;
pub mod selection;
pub mod ty;
pub mod value;

pub use definition::*;
pub use directive::*;
pub use document::*;
pub use executable::*;
pub use extension::*;
pub use generic::*;
pub use import::*;
pub use selection::*;
pub use ty::*;
pub use value::*;

/// [`crate::lossless::ast::AstChildren`] with this dialect's language pinned.
///
/// A convenience alias and nothing more — the iterator, and its `Iterator` impl, are tokora's. It
/// exists so a `many` getter's return type carries one parameter instead of two, across every
/// wrapper [`ast_node!`](crate::ast_node) generates.
pub type AstChildren<N> = crate::lossless::ast::AstChildren<N, GraphQLxLang>;

/// [`crate::lossless::ast::AstTokens`] with this dialect's language pinned.
///
/// The token counterpart of [`AstChildren`], and unlike that one it is not an alias for an
/// upstream type: tokora's `cst` layer has no token iterator.
pub type AstTokens = crate::lossless::ast::AstTokens<GraphQLxLang>;

/// The first direct token child of `parent` whose kind is one of `kinds`.
///
/// [`cast::token`]'s multi-kind form. The scan is in **document order**, not in `kinds` order:
/// asking for the first `K::InlineString` and falling back to the first `K::BlockString` would
/// answer the wrong token for a node that carried both, which is a difference no caller should
/// have to know about.
#[inline]
pub fn token_any(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<SyntaxToken> {
  crate::lossless::ast::token_any(parent, kinds)
}

/// Every direct token child of `parent` with the given kind, in document order.
///
/// [`cast::token`]'s plural form. `cast::token` answers its *first* match and tokora has no
/// `cast::tokens`, so without this a node carrying several tokens of one kind — a [`Path`]'s
/// segments, a [`DirectiveLocations`]' locations — exposes only one of them.
///
/// [`Path`]: ty::Path
/// [`DirectiveLocations`]: definition::DirectiveLocations
#[inline]
pub fn tokens(parent: &SyntaxNode, kind: SyntaxKind) -> AstTokens {
  crate::lossless::ast::tokens(parent, kind)
}
