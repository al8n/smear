//! Typed wrappers over the five path and type-reference node kinds.
//!
//! A type reference is a two-way union — `DefinitionTypePath | ListType` — and this layer has no
//! union type, so every position that holds one carries two `opt` getters of which exactly one
//! answers. See `ast/value.rs`'s module docs for the same ruling at greater width.
//!
//! # There is no non-null wrapper: the `!` is a token of the type it modifies
//!
//! GraphQL's kind space has a `NonNullType` node wrapping the type it modifies. GraphQLx's has
//! none — `graphqlx/syntactic/ty.rs` gives [`DefinitionTypePath`] and [`ListType`] each a
//! `required: bool` — so the `!` reaches the tree as a [`Bang`](K::Bang) **token child** of the
//! type node, and both take a `bang_token` getter whose *presence* is the non-null flag. `[Int!]!`
//! puts one under the [`ListType`] and one under the [`DefinitionTypePath`] inside it, and a getter
//! matching direct children only is what keeps the two apart.
//!
//! # Reading a path's leading `::` off its tokens
//!
//! `graphqlx::ast::Path` carries `fully_qualified: bool` and the tree records the same fact by
//! keeping the token, there being no node for a one-token qualifier. [`Path::separators`] is
//! therefore the whole answer: a path has one `::` between each pair of segments, plus one more in
//! front when it is fully qualified, so `separators().count() == names().count()` **is** the flag.
//! `a::b` is two names and one separator; `::a::b` is two and two.
//!
//! A `tok K::PathSeparator` getter would have looked like the natural spelling and been wrong in
//! exactly the case it exists for — it answers `Some` for `a::b` too, the first separator being a
//! separator either way.

use crate::{ast_node, graphqlx::kinds::SyntaxKind as K};

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A `::`-separated path, `::? Name (:: Name)*`.
  ///
  /// GraphQLx only: GraphQL has no path at all, and every position that would hold a `NamedType`
  /// there holds a [`TypePath`] or a [`DefinitionTypePath`] over one of these here.
  Path => K::Path {
    /// Every segment of the path, in order.
    names: toks K::Name,
    /// Every `::` in the path, in order.
    ///
    /// One per gap between segments, plus a leading one when the path is fully qualified — see
    /// this module's docs for the count that reads the flag off.
    separators: toks K::PathSeparator,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// The generic arguments applied to a path, `< Type+ >`.
  ///
  /// The arguments are type references, so they come back through two iterators — one per type
  /// kind. A consumer needing them in written order walks [`syntax`](TypeGenerics::syntax)
  /// instead; see `ast/value.rs`'s module docs for the same ruling and the same Phase B answer.
  TypeGenerics => K::TypeGenerics {
    /// Every argument that is a path type, in order.
    definition_type_paths: many DefinitionTypePath,
    /// Every argument that is a list type, in order.
    list_types: many ListType,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A type reference's path head, `Path TypeGenerics? '!'?`.
  ///
  /// Where GraphQL writes a `NamedType`. It is the *type* position's spelling: [`TypePath`] is the
  /// same shape in every position that admits no `!`.
  DefinitionTypePath => K::DefinitionTypePath {
    /// The type's path.
    path: opt Path,
    /// The generic arguments applied to it, if any were written.
    type_generics: opt TypeGenerics,
    /// The `!`, present exactly when the reference is non-null.
    bang_token: tok K::Bang,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A list type reference, `[ T ] '!'?`.
  ListType => K::ListType {
    /// The element type, when it is a path type.
    definition_type_path: opt DefinitionTypePath,
    /// The element type, when it is itself a list.
    list_type: opt ListType,
    /// The `!`, present exactly when the list itself is non-null.
    ///
    /// The element's own `!` is a token of the *element*, so `[Int!]` answers `None` here.
    bang_token: tok K::Bang,
  }
);

ast_node!(
  lang = crate::graphqlx::kinds::GraphQLxLang;
  /// A path in a **non-type** position, `Path TypeGenerics?`.
  ///
  /// A directive's name, an interface, a union member, a type condition, a fragment spread's
  /// target, a `where` bound. It carries no `!`, which is the whole difference from
  /// [`DefinitionTypePath`]: none of those positions admits one.
  TypePath => K::TypePath {
    /// The path itself.
    path: opt Path,
    /// The generic arguments applied to it, if any were written.
    type_generics: opt TypeGenerics,
  }
);
