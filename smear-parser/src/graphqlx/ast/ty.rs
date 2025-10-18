use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoSpan, Span},
};

use std::{boxed::Box, rc::Rc, sync::Arc, vec::Vec};

use crate::{
  parser::ident::Ident,
  punctuator::{Bang, FatArrow, LAngle, LBracket, PathSeparator, RAngle, RBracket},
  scaffold::{self, ListType, MapType, SetType},
};

pub type Path<S> = scaffold::Path<Ident<S>>;
pub type TypePath<S, Ty = Type<S>> = scaffold::generic::TypePath<Ident<S>, Ty>;
pub type DefinitionTypePath<S, Ty = Type<S>> = scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;

pub type ArcDefinitionTypePath<S, Ty = ArcType<S>> =
  scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;
pub type RcDefinitionTypePath<S, Ty = RcType<S>> =
  scaffold::generic::DefinitionTypePath<Ident<S>, Ty>;

impl<S> From<Ident<S>> for Path<S> {
  #[inline]
  fn from(ident: Ident<S>) -> Self {
    Self::new(*ident.span(), Vec::from_iter([ident]), false)
  }
}

macro_rules! ty {
  ($(
    $(#[$meta:meta])*
    $ty:ident<$name:ident>), +$(,)?
  ) => {
    paste::paste! {
      $(
        ///
        /// Represents a complete GraphQLx type that can be either named, a list, a set or a map.
        ///
        /// This enum captures the two fundamental categories of types in GraphQL:
        /// - **Named Types**: Direct references to schema-defined types
        /// - **List Types**: Collections wrapping other types
        /// - **Set Types**: Collections of unique elements
        /// - **Map Types**: Key-value pair collections
        ///
        /// The type system is recursive - set, map or list types can contain other list types,
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
        /// ### Set Types (`Set` variant)
        /// Collections of unique elements:
        /// - **Simple Sets**: `<String>`, `<User>`, `<ID!>`
        /// - **Nested Sets**: `<<String>>`, `<<<Int>>>`
        ///
        /// ### Map Types (`Map` variant)
        /// Key-value pair collections:
        /// - **Simple Maps**: `<String => Int>`, `<ID! => User!>`
        /// - **Nested Maps**: `<<String => Int>>`, `<<<ID! => User!>>>`
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
        ///
        /// # Set types in field definitions
        /// type Collection {
        ///   uniqueTags: <String!>!     # Set type: non-null set of non-null strings
        ///   uniqueUsers: <User>         # Set type: nullable set of nullable users
        /// }
        ///
        /// # Map types in field definitions
        /// type Dictionary {
        ///   translations: <String => String!>! # Map type: non-null map of non-null strings
        ///   userRoles: <ID! => UserRole>        # Map type: nullable map of nullable enums
        /// }
        /// ```
        ///
        /// ## Type Resolution
        ///
        /// During schema processing, types are resolved as follows:
        /// 1. **Named Types**: Lookup in schema type registry
        /// 2. **List Types**: Recursively resolve element type, then wrap in list
        /// 3. **Set Types**: Recursively resolve element type, then wrap in set
        /// 4. **Map Types**: Recursively resolve key and value types, then wrap in map
        /// 5. **Validation**: Ensure all referenced types exist and are valid
        ///
        /// ## Memory Management
        ///
        #[doc = "This type uses `" $ty "` for self-referential structures:"]
        ///
        $(#[$meta])*
        #[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
        #[unwrap(ref, ref_mut)]
        #[try_unwrap(ref, ref_mut)]
        pub enum $name<S> {
          /// A path type referencing a schema-defined type.
          ///
          /// Examples: `String!`, `user::User`, `PostStatus!`, `ID`
          Path(scaffold::generic::DefinitionTypePath<Ident<S>, Self>),

          /// A list type containing elements of another type.
          ///
          /// Examples: `[String]!`, `[[User!]]`, `[ID!]`
          List($ty<ListType<Self>>),

          /// A set type containing elements of another type.
          ///
          /// Examples: `<String>!`, `<<ID!>>`, `<ID!>`
          Set($ty<SetType<Self>>),

          /// A map type containing key-value pairs.
          ///
          /// Examples: `<String => Int>!`, `<<ID! => User!>>`, `<ID! => Comment!>!`
          Map($ty<MapType<Self, Self>>),
        }

        impl<S> From<ListType<Self>> for $name<S> {
          #[inline]
          fn from(ty: ListType<Self>) -> Self {
            Self::List(<$ty<ListType<Self>>>::new(ty))
          }
        }

        impl<S> From<SetType<Self>> for $name<S> {
          #[inline]
          fn from(ty: SetType<Self>) -> Self {
            Self::Set(<$ty<SetType<Self>>>::new(ty))
          }
        }

        impl<S> From<MapType<Self, Self>> for $name<S> {
          #[inline]
          fn from(ty: MapType<Self, Self>) -> Self {
            Self::Map(<$ty<MapType<Self, Self>>>::new(ty))
          }
        }

        impl<S> AsSpan<Span> for $name<S> {
          #[inline]
          fn as_span(&self) -> &Span {
            self.span()
          }
        }

        impl<S> IntoSpan<Span> for $name<S> {
          #[inline]
          fn into_span(self) -> Span {
            match self {
              Self::Path(ty) => ty.into_span(),
              Self::List(ty) => *ty.span(),
              Self::Map(ty) => *ty.span(),
              Self::Set(ty) => *ty.span(),
            }
          }
        }

        impl<S> $name<S> {
          /// Returns the span of the type.
          #[inline]
          pub fn span(&self) -> &Span {
            match self {
              Self::Path(ty) => ty.span(),
              Self::List(ty) => ty.span(),
              Self::Set(ty) => ty.span(),
              Self::Map(ty) => ty.span(),
            }
          }

          /// Creates a recursive parser for GraphQLx types.
          ///
          /// This parser handles the complete GraphQLx type syntax including
          /// named types, list types, and nested combinations. It uses recursion
          /// to handle arbitrarily nested list types.
          #[inline]
          pub fn parser_with<'a, I, T, Error, E, IP>(ident_parser: IP) -> impl Parser<'a, I, Self, E> + Clone
          where
            T: Token<'a>,
            I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
            Error: 'a,
            E: ParserExtra<'a, I, Error = Error> + 'a,
            S: 'a,
            Bang: Parseable<'a, I, T, Error> + 'a,
            LBracket: Parseable<'a, I, T, Error> + 'a,
            RBracket: Parseable<'a, I, T, Error> + 'a,
            LAngle: Parseable<'a, I, T, Error> + 'a,
            RAngle: Parseable<'a, I, T, Error> + 'a,
            FatArrow: Parseable<'a, I, T, Error> + 'a,
            PathSeparator: Parseable<'a, I, T, Error> + 'a,
            IP: Parser<'a, I, Ident<S>, E> + Clone + 'a,
          {
            recursive(|parser| {
              let angle = LAngle::parser()
                .ignore_then(parser.clone())
                .then(
                  RAngle::parser()
                    .ignored()
                    .map(|_| None)
                    .or(
                      FatArrow::parser()
                        .ignore_then(
                          parser.clone()
                            .then_ignore(RAngle::parser())
                            .map(Some)
                        )
                    )
                )
                .then(Bang::parser().or_not())
                .map_with(|((k, v), bang), exa| match v {
                  None => Self::Set(SetType::new(exa.span(), k, bang.is_some()).into()),
                  Some(v) => Self::Map(MapType::new(exa.span(), k, v, bang.is_some()).into()),
                });

              choice((
                angle,
                ListType::parser_with(parser.clone()).map(Self::from),
                scaffold::generic::DefinitionTypePath::<Ident<S>, Self>::parser_with(ident_parser, parser).map(Self::Path),
              ))
            })
          }
        }

        impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for $name<S>
        where
          Ident<S>: Parseable<'a, I, T, Error> + 'a,
          Bang: Parseable<'a, I, T, Error> + 'a,
          LBracket: Parseable<'a, I, T, Error> + 'a,
          RBracket: Parseable<'a, I, T, Error> + 'a,
          LAngle: Parseable<'a, I, T, Error> + 'a,
          RAngle: Parseable<'a, I, T, Error> + 'a,
          FatArrow: Parseable<'a, I, T, Error> + 'a,
          PathSeparator: Parseable<'a, I, T, Error> + 'a,
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
            Self::parser_with(Ident::parser())
          }
        }
      )*
    }
  };
}

ty!(
  /// GraphQLx type using `Box` for recursive types.
  ///
  /// This is the standard type representation that uses heap allocation
  /// for recursive structures. Suitable for most use cases where types
  /// are processed once and don't require sharing.
  Box<Type>,
  /// GraphQLx type using `Rc` for recursive types with reference counting.
  ///
  /// This type uses `Rc` (Reference Counted) smart pointers to enable
  /// sharing of type structures within single-threaded contexts. Useful
  /// when the same type structure is referenced in multiple places.
  Rc<RcType>,
  /// GraphQLx type using `Arc` for recursive types with atomic reference counting.
  ///
  /// This type uses `Arc` (Atomically Reference Counted) smart pointers
  /// to enable sharing of type structures across thread boundaries. Required
  /// for multi-threaded schema processing or when types need to be Send + Sync.
  Arc<ArcType>,
);
