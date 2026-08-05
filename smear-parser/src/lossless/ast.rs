//! The typed-accessor substrate, generic over the dialect's `rowan::Language`.
//!
//! **Most of it is tokora's.** [`CastNode`](tokora::cst::CastNode) is a one-method trait — a kind
//! check and a wrap — and [`cast::child`](tokora::cst::cast::child),
//! [`cast::children`](tokora::cst::cast::children) and
//! [`NodeChildren`](tokora::cst::NodeChildren) are bound on it rather than on tokora's
//! parser-facing `Node`, so a wrapper whose entire job is `field.name()` never names the `Syntax`
//! component model.
//!
//! What is smear's own is [`ast_node!`](crate::ast_node) and the two token getters `cast` does not
//! have. Both were dialect-bound in Phase A; both are language-generic here.
//!
//! # Why the rowan types are re-exported as aliases
//!
//! [`ast_node!`](crate::ast_node) is `#[macro_export]`ed and every path it emits is rooted at
//! `$crate`, so an invocation compiles in a crate that imports neither rowan nor tokora. That
//! property is load-bearing — `tests/lossless_substrate.rs` declares a wrapper and depends on it —
//! and it is why the macro spells `$crate::lossless::ast::SyntaxNode<$lang>` rather than
//! `::rowan::SyntaxNode<$lang>`.

pub use tokora::cst::{CastNode, NodeChildren, cast};

/// A lossless syntax node in `L`'s space.
pub type SyntaxNode<L> = rowan::SyntaxNode<L>;
/// A lossless syntax token in `L`'s space.
pub type SyntaxToken<L> = rowan::SyntaxToken<L>;
/// A node-or-token in `L`'s space.
pub type SyntaxElement<L> = rowan::SyntaxElement<L>;

/// [`NodeChildren`] with the language named second.
///
/// A convenience alias and nothing more — the iterator and its `Iterator` impl are tokora's. It
/// exists so a `many` getter's return type reads the same in both dialects.
pub type AstChildren<N, L> = NodeChildren<N, L>;

/// The first direct token child of `parent` whose kind is one of `kinds`.
///
/// [`cast::token`]'s multi-kind form. The scan is in **document order**, not in `kinds` order:
/// asking for the first `String` and falling back to the first `BlockString` would answer the
/// wrong token for a node that carried both, which is a difference no caller should have to know
/// about.
#[inline]
pub fn token_any<L: rowan::Language>(
  parent: &SyntaxNode<L>,
  kinds: &[L::Kind],
) -> Option<SyntaxToken<L>> {
  parent
    .children_with_tokens()
    .filter_map(|child| child.into_token())
    .find(|t| kinds.contains(&t.kind()))
}

/// Every direct token child of `parent` with the given kind, in document order.
///
/// [`cast::token`]'s plural form. `cast::token` answers its *first* match and tokora has no
/// `cast::tokens`, so without this a node carrying several tokens of one kind — a
/// `DirectiveLocations`, or a definition's keyword and its name — exposes only one of them.
#[inline]
pub fn tokens<L: rowan::Language>(parent: &SyntaxNode<L>, kind: L::Kind) -> AstTokens<L> {
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
pub struct AstTokens<L: rowan::Language> {
  inner: rowan::SyntaxElementChildren<L>,
  kind: L::Kind,
}

impl<L: rowan::Language> Iterator for AstTokens<L> {
  type Item = SyntaxToken<L>;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    // Direct children only, exactly as `cast::token` scans: a `Name` belonging to a child node is
    // that node's, and reaching it here would make a parent answer for its child.
    self
      .inner
      .by_ref()
      .find_map(|child| child.into_token().filter(|t| t.kind() == self.kind))
  }
}

/// Declare a typed wrapper over one node kind in a dialect's syntax-kind space.
///
/// The example names a placeholder dialect rather than a real one on purpose: this module is
/// mechanically checked to mention no dialect at all, docs included, because a substrate that
/// names one dialect in prose is one edit away from naming it in code.
///
/// ```text
/// ast_node!(
///   lang = crate::the_dialect::kinds::TheDialectLang;
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
/// # `lang =` is mandatory and is the first thing in the invocation
///
/// The macro is `#[macro_export]`ed, so it is visible as `crate::ast_node!` from anywhere. Before
/// this parameter existed its body hardcoded one dialect's `SyntaxNode` and one dialect's
/// `rowan::Language`, so an author in a *second* dialect who reached for it got wrappers typed over
/// the *first* dialect's tree — a compile error at best, a wrong-`Language` cast at worst. Naming
/// the language at the invocation is what makes the wrong answer unspellable rather than merely
/// unlikely.
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
    lang = $lang:path;
    $(#[$meta:meta])*
    $name:ident => $kind:path { $($getters:tt)* }
  ) => {
    $(#[$meta])*
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    pub struct $name($crate::lossless::ast::SyntaxNode<$lang>);

    impl $crate::lossless::ast::CastNode<$lang> for $name {
      #[inline]
      fn cast_node(
        syntax: $crate::lossless::ast::SyntaxNode<$lang>,
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
      pub fn syntax(&self) -> &$crate::lossless::ast::SyntaxNode<$lang> {
        &self.0
      }

      $crate::ast_node!(@getters $lang; $($getters)*);
    }
  };

  // ---- getter muncher: one comma-terminated entry per recursion ----
  //
  // `$lang` is threaded through every arm because each getter's return type names it. It is the
  // first fragment in every pattern, before the doc-comment repetition, so an arm still fails at
  // a plain token with nothing parsed — the property the six arms rely on to be tried in order.

  (@getters $lang:path;) => {};

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : opt $target:ty , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$target> {
      $crate::lossless::ast::cast::child(&self.0)
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : many $target:ty , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> $crate::lossless::ast::AstChildren<$target, $lang> {
      $crate::lossless::ast::cast::children(&self.0)
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : tok $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::lossless::ast::SyntaxToken<$lang>> {
      // `cast::token` matches a `Lang::Kind` value against **direct** token children, so a
      // nested `Name` — an argument's, say — cannot answer here.
      $crate::lossless::ast::cast::token(&self.0, &$tk)
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };

  // The three arms below are local: `cast` has no multi-kind, plural or positional token
  // helper. Each is distinguished by its own literal keyword sitting before any fragment, which
  // is what lets an arm fail at a plain token and the next one be tried with nothing parsed —
  // the same property the three arms above rely on.

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : tok_any $($tk:path)|+ , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::lossless::ast::SyntaxToken<$lang>> {
      $crate::lossless::ast::token_any(&self.0, &[$($tk),+])
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : toks $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> $crate::lossless::ast::AstTokens<$lang> {
      $crate::lossless::ast::tokens(&self.0, $tk)
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };

  (@getters $lang:path; $(#[$gmeta:meta])* $getter:ident : tok_nth $n:literal $tk:path , $($rest:tt)*) => {
    $(#[$gmeta])*
    #[inline]
    pub fn $getter(&self) -> ::core::option::Option<$crate::lossless::ast::SyntaxToken<$lang>> {
      ::core::iter::Iterator::nth(
        &mut $crate::lossless::ast::tokens(&self.0, $tk),
        $n,
      )
    }
    $crate::ast_node!(@getters $lang; $($rest)*);
  };
}
