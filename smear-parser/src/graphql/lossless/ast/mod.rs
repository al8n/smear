//! The typed accessor layer over the GraphQL lossless CST.
//!
//! **The substrate is tokora's.** [`CastNode`] is a one-method trait — a kind check and a wrap —
//! and [`cast::child`], [`cast::children`] and [`NodeChildren`] are bound on it rather than on
//! tokora's parser-facing `Node`, so a wrapper whose entire job is `field.name()` never names
//! the `Syntax` component model. That is a deliberate upstream split (tokora 0.8.0, PR #132),
//! and this module takes the navigation side of it.
//!
//! What is smear's own is the [`ast_node!`](crate::ast_node) macro: fifty-nine wrappers with
//! identical bodies is a code-generation problem, not a trait-design one.
//!
//! # Two token getters this module has to write itself
//!
//! `tokora::cst::cast` offers exactly [`child`](cast::child), [`children`](cast::children) and
//! [`token`](cast::token), and [`token`](cast::token) is *one kind, first match*. Two shapes in
//! this grammar fall outside that, so [`token_any`] and [`tokens`] are written here over
//! [`SyntaxNode::children_with_tokens`] in the same style — a filter over direct children, and
//! nothing more. They are not a divergence from the upstream model; they are the two members of
//! it that do not exist yet.
//!
//! - **One getter, several token kinds.** A GraphQL string literal has two images —
//!   [`String`](crate::graphql::kinds::SyntaxKind::String) for `"s"` and
//!   [`BlockString`](crate::graphql::kinds::SyntaxKind::BlockString) for `"""s"""` — and two node
//!   kinds wrap one directly: `StringValue` and `Description`. Splitting that into two getters
//!   with an `.or()` at every call site pushes a lexical detail onto every consumer.
//! - **Several tokens of one kind.** `DirectiveLocations` holds its locations as bare
//!   [`Name`](crate::graphql::kinds::SyntaxKind::Name) tokens, a location having no node kind of
//!   its own, so `"| FIELD | QUERY"` is two `Name` tokens of which [`cast::token`] can reach
//!   exactly one.
//!
//! [`tokens`] also answers a third shape the plan did not anticipate and which is far more
//! common than either: **this kind space has no `Name` node and no per-keyword token kind**, so a
//! definition's keyword and its name are both `Name` tokens under the same parent. `type T`,
//! `fragment F on T` and `extend type T` all put the name *after* one or two keywords, and the
//! `tok_nth` arm below is what reaches it.

pub use tokora::cst::{CastNode, NodeChildren, cast};

use crate::graphql::{
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

/// [`NodeChildren`] with this crate's language pinned.
///
/// A convenience alias and nothing more — the iterator, and its `Iterator` impl, are tokora's.
/// It exists so a `many` getter's return type carries one parameter instead of two, across every
/// wrapper [`ast_node!`](crate::ast_node) generates.
pub type AstChildren<N> = NodeChildren<N, GraphQLLang>;

/// The first direct token child of `parent` whose kind is one of `kinds`.
///
/// [`cast::token`]'s multi-kind form. The scan is in **document order**, not in `kinds` order:
/// asking for the first `K::String` and falling back to the first `K::BlockString` would answer
/// the wrong token for a node that carried both, which is a difference no caller should have to
/// know about.
#[inline]
pub fn token_any(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<SyntaxToken> {
  parent
    .children_with_tokens()
    .filter_map(|child| child.into_token())
    .find(|t| kinds.contains(&t.kind()))
}

/// Every direct token child of `parent` with the given kind, in document order.
///
/// [`cast::token`]'s plural form. `cast::token` answers its *first* match and tokora has no
/// `cast::tokens`, so without this a node carrying several tokens of one kind — a
/// `DirectiveLocations`, a definition's keyword and name — exposes only one of them.
#[inline]
pub fn tokens(parent: &SyntaxNode, kind: SyntaxKind) -> AstTokens {
  AstTokens {
    inner: parent.children_with_tokens(),
    kind,
  }
}

/// An iterator over a node's direct token children of one kind, in document order.
///
/// [`tokens`] is its only constructor. The token counterpart of [`AstChildren`], and unlike that
/// one it is not an alias for an upstream type: tokora's `cst` layer has no token iterator.
#[derive(Debug, Clone)]
pub struct AstTokens {
  inner: rowan::SyntaxElementChildren<GraphQLLang>,
  kind: SyntaxKind,
}

impl Iterator for AstTokens {
  type Item = SyntaxToken;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    // Direct children only, exactly as `cast::token` scans: a `Name` belonging to a child node
    // is that node's, and reaching it here would make a parent answer for its child.
    self
      .inner
      .by_ref()
      .find_map(|child| child.into_token().filter(|t| t.kind() == self.kind))
  }
}

/// Declare a typed wrapper over one [`SyntaxKind`](crate::graphql::kinds::SyntaxKind).
///
/// ```text
/// ast_node!(
///   /// A field selection.
///   Field => K::Field {
///     /// The field's alias, if it has one.
///     alias: opt Alias,           // -> Option<Alias>          via cast::child
///     /// The field's directives.
///     directives: many Directive, // -> AstChildren<Directive> via cast::children
///     /// The field's own name token.
///     name: tok K::Name,          // -> Option<SyntaxToken>    via cast::token
///   }
/// );
/// ```
///
/// Three further getter forms exist for the token shapes [`cast::token`] cannot express:
///
/// ```text
/// string_token: tok_any K::String | K::BlockString, // -> Option<SyntaxToken> via token_any
/// locations: toks K::Name,                          // -> AstTokens           via tokens
/// name: tok_nth 1 K::Name,                          // -> Option<SyntaxToken> via tokens().nth
/// ```
///
/// `tok_nth` is positional and therefore only as stable as the grammar's own ordering. It is
/// used **only** where the position is forced by a production that `expect`s each token in turn —
/// a definition's keyword then its name — never where a token is optional. `repeatable` in a
/// directive definition is the counter-example that decided the rule: it is a `Name` at index 2
/// when it is written and absent when it is not, so an index would silently answer `on` instead.
///
/// **Documentation goes inside the delimiters, as above.** This crate is
/// `#![deny(missing_docs)]` and the items here are generated, so their docs have to arrive as
/// part of the invocation's token stream. A `///` written *above* `ast_node!(…)` documents the
/// invocation instead: rustc discards it with an `unused_doc_comment` warning and then reports
/// the generated `pub struct` as undocumented. Both the wrapper's attributes and each getter's
/// are forwarded.
///
/// **Every getter line ends with a comma, including the last** — the macro munches its getter
/// list one comma-terminated entry at a time, so a missing trailing comma is a match failure
/// rather than a tolerated style.
///
/// Getters that yield a single child return `Option` because the parser recovers: in a broken
/// document a `Field` may genuinely have no name. A total getter would be a lie the type system
/// then enforces on every consumer.
///
/// A `tok` getter matches **direct** token children only, so a `Name` that belongs to a child
/// node cannot answer for its parent.
///
/// Every path the macro emits is rooted at `$crate`, so an invocation compiles wherever
/// `smear_parser` is nameable — including an integration test crate that never imports tokora
/// itself.
#[macro_export]
macro_rules! ast_node {
  (
    $(#[$meta:meta])*
    $name:ident => $kind:path { $($getters:tt)* }
  ) => {
    $(#[$meta])*
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    pub struct $name($crate::graphql::lossless::SyntaxNode);

    impl $crate::graphql::lossless::ast::CastNode<$crate::graphql::lossless::GraphQLLang>
      for $name
    {
      #[inline]
      fn cast_node(
        syntax: $crate::graphql::lossless::SyntaxNode,
      ) -> ::core::option::Option<Self> {
        // A kind check and a wrap. `CastNode`'s contract is that this never panics: the
        // navigation helpers call it once per child and read `None` as "not this type, keep
        // looking", so a panicking impl would abort a walk instead of skipping a sibling.
        if syntax.kind() == $kind {
          ::core::option::Option::Some(Self(syntax))
        } else {
          ::core::option::Option::None
        }
      }
    }

    impl $name {
      /// The untyped node this wrapper is transparent over.
      ///
      /// Generated here rather than inherited: `CastNode` is a one-way door and carries no
      /// `syntax()`. Tokora's `Node` does, at the price of the component model — which is the
      /// price this layer declines.
      #[inline]
      pub fn syntax(&self) -> &$crate::graphql::lossless::SyntaxNode {
        &self.0
      }

      $crate::ast_node!(@getters $($getters)*);
    }
  };

  // ---- getter muncher: one comma-terminated entry per recursion ----

  (@getters) => {};

  (@getters $(#[$gmeta:meta])* $getter:ident : opt $target:ty , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$target> {
      $crate::graphql::lossless::ast::cast::child(&self.0)
    }
    $crate::ast_node!(@getters $($rest)*);
  };

  (@getters $(#[$gmeta:meta])* $getter:ident : many $target:ty , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> $crate::graphql::lossless::ast::AstChildren<$target> {
      $crate::graphql::lossless::ast::cast::children(&self.0)
    }
    $crate::ast_node!(@getters $($rest)*);
  };

  (@getters $(#[$gmeta:meta])* $getter:ident : tok $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::graphql::lossless::SyntaxToken> {
      // `cast::token` matches a `Lang::Kind` value against **direct** token children, so a
      // nested `Name` — an argument's, say — cannot answer here.
      $crate::graphql::lossless::ast::cast::token(&self.0, &$tk)
    }
    $crate::ast_node!(@getters $($rest)*);
  };

  // The three arms below are local: `cast` has no multi-kind, plural or positional token
  // helper. Each is distinguished by its own literal keyword sitting before any fragment, which
  // is what lets an arm fail at a plain token and the next one be tried with nothing parsed —
  // the same property the three arms above rely on.

  (@getters $(#[$gmeta:meta])* $getter:ident : tok_any $($tk:path)|+ , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::graphql::lossless::SyntaxToken> {
      $crate::graphql::lossless::ast::token_any(&self.0, &[$($tk),+])
    }
    $crate::ast_node!(@getters $($rest)*);
  };

  (@getters $(#[$gmeta:meta])* $getter:ident : toks $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> $crate::graphql::lossless::ast::AstTokens {
      $crate::graphql::lossless::ast::tokens(&self.0, $tk)
    }
    $crate::ast_node!(@getters $($rest)*);
  };

  (@getters $(#[$gmeta:meta])* $getter:ident : tok_nth $n:literal $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::graphql::lossless::SyntaxToken> {
      ::core::iter::Iterator::nth(
        &mut $crate::graphql::lossless::ast::tokens(&self.0, $tk),
        $n,
      )
    }
    $crate::ast_node!(@getters $($rest)*);
  };
}
