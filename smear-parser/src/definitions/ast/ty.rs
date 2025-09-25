use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use std::{boxed::Box, rc::Rc, sync::Arc};

use crate::lang::punctuator::{Bang, LBracket, RBracket};

/// Represents a named GraphQL type with optional non-null modifier.
///
/// Named types are the foundation of GraphQL's type system, referencing
/// concrete types by name. They can represent any defined type in the schema:
/// scalars, objects, interfaces, unions, enums, or input objects.
///
/// The optional bang (`!`) suffix makes the type non-null, meaning it cannot
/// return `null` values and must always provide a valid value of the specified type.
///
/// ## Examples
///
/// ```text
/// # Nullable named types
/// String          # Can be null or a string value
/// User            # Can be null or a User object
/// PostStatus      # Can be null or a PostStatus enum value
///
/// # Non-null named types  
/// String!         # Must be a string value, never null
/// User!           # Must be a User object, never null
/// ID!             # Must be an ID value, never null
///
/// # Usage in field definitions
/// type User {
///   id: ID!                    # Required ID
///   name: String!              # Required name
///   email: String              # Optional email (can be null)
///   avatar: Image              # Optional avatar image
///   status: UserStatus!        # Required status enum
/// }
/// ```
///
/// ## Nullability Semantics
/// - **Without `!`**: Field can return `null` or a valid value
/// - **With `!`**: Field must always return a valid value, never `null`
/// - **Error Handling**: Non-null fields that would return `null` cause query errors
/// - **Schema Evolution**: Adding `!` to existing fields is a breaking change
///
/// ## Grammar
/// ```text
/// NamedType : Name !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct NamedType<Name> {
  span: Span,
  name: Name,
  required: bool,
}

impl<Name> AsSpan<Span> for NamedType<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for NamedType<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for NamedType<Name> {
  type Components = (Span, Name, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.required)
  }
}

impl<Name> NamedType<Name> {
  /// Returns a reference to the span covering the entire named type.
  ///
  /// The span includes the type name and optional bang modifier,
  /// providing complete source location information.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the type.
  ///
  /// This is the identifier that references a type defined elsewhere in the schema.
  /// Type names must follow GraphQL naming conventions and resolve to valid
  /// schema types (scalars, objects, interfaces, unions, enums, or input objects).
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns whether the named type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for named types.
  ///
  /// This parser recognizes a type name followed by an optional bang (`!`) modifier.
  /// It handles whitespace between the name and bang appropriately.
  ///
  /// ## Grammar Handled
  /// ```text
  /// NamedType : Name !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// String      # Nullable string type
  /// String!     # Non-null string type
  /// User        # Nullable User object type
  /// ID!         # Non-null ID scalar type
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, NP>(name_parser: NP) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    NP: Parser<'a, I, Name, E> + Clone,
  {
    name_parser
      .then(Bang::parser().or_not())
      .map_with(|(name, bang), exa| Self {
        span: exa.span(),
        name,
        required: bang.is_some(),
      })
  }
}

/// Represents a GraphQL list type with optional non-null modifier.
///
/// List types represent arrays or collections of values in GraphQL. They wrap
/// another type (the element type) to indicate that fields of this type return
/// multiple values of the wrapped type.
///
/// List types support complex nullability semantics:
/// - The list itself can be null or non-null
/// - The elements within the list can be null or non-null
/// - These nullability rules are independent and composable
///
/// ## Examples
///
/// ```text
/// # Nullable list of nullable strings
/// [String]         # Can be null, or a list containing strings and nulls
///
/// # Non-null list of nullable strings
/// [String]!        # Must be a list (never null), but can contain nulls
///
/// # Nullable list of non-null strings
/// [String!]        # Can be null, or a list containing only strings (no nulls)
///
/// # Non-null list of non-null strings
/// [String!]!       # Must be a list containing only strings (no nulls anywhere)
///
/// # Nested list types
/// [[String]]       # List of lists of strings
/// [User!]!         # Non-null list of non-null User objects
/// [[String!]!]!    # Non-null list of non-null lists of non-null strings
/// ```
///
/// ## Nullability Combinations
///
/// | Type Syntax | List Nullability | Element Nullability | Example Values |
/// |-------------|------------------|---------------------|----------------|
/// | `[String]`  | Nullable | Nullable | `null`, `["a", null, "c"]` |
/// | `[String]!` | Non-null | Nullable | `["a", null, "c"]`, `[]` |
/// | `[String!]` | Nullable | Non-null | `null`, `["a", "b", "c"]` |
/// | `[String!]!`| Non-null | Non-null | `["a", "b", "c"]`, `[]` |
///
/// ## Use Cases
/// - **Collections**: Arrays of objects, IDs, or scalar values
/// - **Relationships**: One-to-many relationships in object types
/// - **Batch Operations**: Multiple inputs or outputs in mutations
/// - **Search Results**: Variable-length result sets
/// - **Tags/Categories**: Multiple classifications or labels
///
/// ## Grammar
/// ```text
/// ListType : [ Type ] !?
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ListType<Type> {
  span: Span,
  ty: Type,
  required: bool,
}

impl<Type> AsSpan<Span> for ListType<Type> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Type> IntoSpan<Span> for ListType<Type> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Type> IntoComponents for ListType<Type> {
  type Components = (Span, Type, bool);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ty, self.required)
  }
}

impl<Type> ListType<Type> {
  /// Returns a reference to the span covering the entire list type.
  ///
  /// The span includes the brackets, element type, and optional bang modifier.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the element type contained within the list.
  ///
  /// This is the type of individual elements in the list. It can be any
  /// valid GraphQL type including named types, other list types, or even
  /// nested list types for multi-dimensional arrays.
  ///
  /// ## Examples
  /// ```rust
  /// // For list type [String!]
  /// let element_type = list_type.ty(); // References String!
  ///
  /// // For nested list [[User]]
  /// let element_type = list_type.ty(); // References [User]
  /// ```
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// Returns whether the list type is non-null (required).
  #[inline]
  pub const fn required(&self) -> bool {
    self.required
  }

  /// Creates a parser for list types using the provided element type parser.
  ///
  /// This parser handles the complete list type syntax including brackets,
  /// element type parsing, and optional bang modifier. The element type
  /// parsing is delegated to the provided parser for flexibility.
  ///
  /// ## Parser Flow
  /// 1. Parse opening bracket `[`
  /// 2. Parse element type (with whitespace handling)
  /// 3. Parse closing bracket `]`
  /// 4. Parse optional bang modifier `!`
  /// 5. Capture complete span information
  ///
  /// ## Parameters
  /// - `parser`: Parser for the element type within the list
  ///
  /// ## Grammar Handled
  /// ```text
  /// ListType : [ Type ] !?
  /// ```
  ///
  /// ## Example Parsed Input
  /// ```text
  /// [String]        # Nullable list of nullable strings
  /// [String!]!      # Non-null list of non-null strings
  /// [[User]]        # Nested list type
  /// [ID!]           # Nullable list of non-null IDs
  /// ```
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, P>(parser: P) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    LBracket: Parseable<'a, I, T, Error> + 'a,
    RBracket: Parseable<'a, I, T, Error> + 'a,
    Bang: Parseable<'a, I, T, Error> + 'a,
    P: Parser<'a, I, Type, E> + Clone,
  {
    LBracket::parser()
      .ignore_then(parser)
      .then_ignore(RBracket::parser())
      .then(Bang::parser().or_not())
      .map_with(|(ty, bang), exa| Self {
        span: exa.span(),
        ty,
        required: bang.is_some(),
      })
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
            I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
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
            I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
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

impl<Name> IntoSpan<Span> for Type<Name> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Name(ty) => ty.into_span(),
      Self::List(ty) => ty.into_span(),
    }
  }
}
