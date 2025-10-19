use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoSpan, Span},
};
use smear_lexer::punctuator::{Bang, LBracket, RBracket};
use smear_scaffold::{ListType, NamedType};

use std::{boxed::Box, rc::Rc, sync::Arc};

macro_rules! ty {
  ($(
    $(#[$meta:meta])*
    $ty:ident<$name:ident>), +$(,)?
  ) => {
    paste::paste! {
      $(
        ///
        /// Represents a complete GraphQL type that can be either named or a list.
        ///
        /// This enum captures the two fundamental categories of types in GraphQL:
        /// - **Named Types**: Direct references to schema-defined types
        /// - **List Types**: Collections wrapping other types
        ///
        /// The type system is recursive - list types can contain other list types,
        /// enabling complex nested structures like lists of lists.
        ///
        /// ## Type Categories
        ///
        /// ### Named Types (`Name` variant)
        /// Reference types defined in the schema:
        /// - **Scalars**: `String`, `Int`, `Float`, `Boolean`, `ID`, custom scalars
        /// - **Objects**: User-defined object types
        /// - **Interfaces**: Abstract types with shared fields
        /// - **Unions**: Types that can be one of several object types
        /// - **Enums**: Types with predefined values
        /// - **Input Objects**: Complex input types for mutations
        ///
        /// ### List Types (`List` variant)
        /// Collections of other types:
        /// - **Simple Lists**: `[String]`, `[User]`, `[ID!]`
        /// - **Nested Lists**: `[[String]]`, `[[[Int]]]`
        /// - **Mixed Nullability**: `[String!]!`, `[User]!`, `[[String!]]`
        ///
        /// ## Examples
        ///
        /// ```text
        /// # Named types in field definitions
        /// type User {
        ///   id: ID!                    # Named type: non-null ID
        ///   name: String!              # Named type: non-null String
        ///   email: String              # Named type: nullable String
        ///   role: UserRole!            # Named type: non-null enum
        /// }
        ///
        /// # List types in field definitions
        /// type Post {
        ///   tags: [String!]!           # List type: non-null list of non-null strings
        ///   comments: [Comment]        # List type: nullable list of nullable comments
        ///   relatedPosts: [[Post!]]    # List type: nested lists
        /// }
        ///
        /// # Complex type combinations
        /// type SearchResult {
        ///   users: [User!]             # List of non-null users (list can be null)
        ///   posts: [Post]!             # Non-null list of nullable posts
        ///   categories: [[Category!]]! # Non-null list of non-null lists of non-null categories
        /// }
        /// ```
        ///
        /// ## Type Resolution
        ///
        /// During schema processing, types are resolved as follows:
        /// 1. **Named Types**: Lookup in schema type registry
        /// 2. **List Types**: Recursively resolve element type, then wrap in list
        /// 3. **Validation**: Ensure all referenced types exist and are valid
        ///
        /// ## Memory Management
        ///
        #[doc = "This type uses `" $ty "` for self-referential structures:"]
        ///
        $(#[$meta])*
        #[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
        #[unwrap(ref, ref_mut)]
        #[try_unwrap(ref, ref_mut)]
        pub enum $name<Name> {
          /// A named type referencing a schema-defined type.
          ///
          /// Examples: `String!`, `User`, `PostStatus!`, `ID`
          Name(NamedType<Name>),

          /// A list type containing elements of another type.
          ///
          /// Examples: `[String]!`, `[[User!]]`, `[ID!]`
          List($ty<ListType<Self>>),
        }

        impl<Name> From<ListType<Self>> for $name<Name> {
          #[inline]
          fn from(ty: ListType<Self>) -> Self {
            Self::List(<$ty<ListType<Self>>>::new(ty))
          }
        }

        impl<Name> AsSpan<Span> for $name<Name> {
          #[inline]
          fn as_span(&self) -> &Span {
            match self {
              Self::Name(ty) => ty.span(),
              Self::List(ty) => ty.span(),
            }
          }
        }

        impl<Name> IntoSpan<Span> for $name<Name> {
          #[inline]
          fn into_span(self) -> Span {
            match self {
              Self::Name(ty) => ty.into_span(),
              Self::List(ty) => *ty.span(),
            }
          }
        }

        impl<Name> $name<Name> {
          /// Creates a recursive parser for GraphQL types.
          ///
          /// This parser handles the complete GraphQL type syntax including
          /// named types, list types, and nested combinations. It uses recursion
          /// to handle arbitrarily nested list types.
          #[inline]
          pub fn parser_with<'a, I, T, Error, E, NP>(name_parser: NP) -> impl Parser<'a, I, Self, E> + Clone
          where
            T: Token<'a>,
            I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
            Error: 'a,
            E: ParserExtra<'a, I, Error = Error> + 'a,
            Name: 'a,
            Bang: Parseable<'a, I, T, Error> + 'a,
            LBracket: Parseable<'a, I, T, Error> + 'a,
            RBracket: Parseable<'a, I, T, Error> + 'a,
            NP: Parser<'a, I, Name, E> + Clone + 'a,
          {
            recursive(|parser| {
              choice((NamedType::parser_with(name_parser).map(Self::Name), ListType::parser_with(parser).map(Self::from)))
            })
          }
        }

        impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for $name<Name>
        where
          Name: Parseable<'a, I, T, Error>,
          Bang: Parseable<'a, I, T, Error>,
          LBracket: Parseable<'a, I, T, Error>,
          RBracket: Parseable<'a, I, T, Error>,
        {
          #[inline]
          fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
          where
            Self: Sized + 'a,
            T: Token<'a>,
            I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
            Error: 'a,
            E: ParserExtra<'a, I, Error = Error> + 'a
          {
            Self::parser_with(Name::parser())
          }
        }
      )*
    }
  };
}

ty!(
  /// GraphQL type using `Box` for recursive list types.
  ///
  /// This is the standard type representation that uses heap allocation
  /// for recursive structures. Suitable for most use cases where types
  /// are processed once and don't require sharing.
  Box<Type>,
  /// GraphQL type using `Rc` for recursive list types with reference counting.
  ///
  /// This type uses `Rc` (Reference Counted) smart pointers to enable
  /// sharing of type structures within single-threaded contexts. Useful
  /// when the same type structure is referenced in multiple places.
  Rc<RcType>,
  /// GraphQL type using `Arc` for recursive list types with atomic reference counting.
  ///
  /// This type uses `Arc` (Atomically Reference Counted) smart pointers
  /// to enable sharing of type structures across thread boundaries. Required
  /// for multi-threaded schema processing or when types need to be Send + Sync.
  Arc<ArcType>,
);
