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

pub use tokora::cst::{CastNode, NodeChildren, cast};

use crate::graphql::lossless::GraphQLLang;

/// [`NodeChildren`] with this crate's language pinned.
///
/// A convenience alias and nothing more — the iterator, and its `Iterator` impl, are tokora's.
/// It exists so a `many` getter's return type carries one parameter instead of two, across every
/// wrapper [`ast_node!`](crate::ast_node) generates.
pub type AstChildren<N> = NodeChildren<N, GraphQLLang>;

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
}
