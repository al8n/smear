pub use string_lexer::*;

macro_rules! variant_type {
  (
    $(#[$meta:meta])*
    $vis:vis struct $name:ident {
      $(
        $(#[$field_meta:meta])*
        $field:ident: $ty:ty $(,)?
      )*
    }
  ) => {
    $(#[$meta])*
    $vis struct $name<S> {
      source: S,
      $($field: $ty),*
    }

    impl<'a> TryFrom<$name<&'a [u8]>> for $name<&'a str> {
      type Error = core::str::Utf8Error;

      #[inline]
      fn try_from(value: $name<&'a [u8]>) -> Result<Self, Self::Error> {
        core::str::from_utf8(value.source())
          .map(|s| {
            Self::new(s, $(value.$field),*)
          })
      }
    }

    impl<S> $name<S> {
      #[inline(always)]
      #[allow(clippy::too_many_arguments)]
      pub(crate) const fn new(source: S, $($field: $ty),*) -> Self {
        Self { source, $($field),* }
      }

      $(
        $( #[$field_meta] )*
        #[inline(always)]
        pub const fn $field(&self) -> $ty {
          self.$field
        }
      )*

      /// Returns the source of the simple escape string.
      #[inline(always)]
      pub const fn source_ref(&self) -> &S {
        &self.source
      }

      /// Returns the underlying source.
      #[inline(always)]
      pub const fn source(&self) -> S where S: Copy {
        self.source
      }

      /// Converts this to an equivalent type.
      #[inline(always)]
      pub fn to_equivalent<T>(&self) -> $name<T>
      where
        S: logosky::utils::ToEquivalent<T>,
      {
        $name::new(self.source.to_equivalent(), $(self.$field),*)
      }

      /// Converts this to an equivalent type.
      #[inline(always)]
      pub fn into_equivalent<T>(self) -> $name<T>
      where
        S: logosky::utils::IntoEquivalent<T>,
      {
        $name::new(self.source.into_equivalent(), $(self.$field),*)
      }
    }

    impl<S: logosky::utils::human_display::DisplayHuman> core::fmt::Display for $name<S> {
      #[inline]
      fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        logosky::utils::human_display::DisplayHuman::fmt(&self.source, f)
      }
    }

    impl<'a> $name<&'a str> {
      /// Returns the str representation.
      #[inline(always)]
      pub const fn as_str(&self) -> &'a str {
        self.source
      }
    }

    impl<'a> $name<&'a [u8]> {
      /// Returns the byte slice representation.
      #[inline(always)]
      pub const fn as_bytes(&self) -> &'a [u8] {
        self.source
      }
    }
  }
}

macro_rules! impl_common_traits {
  ($name:ident::<&$lt:lifetime $ty:ty>::$fn:ident) => {
    impl<$lt> PartialEq<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn eq(&self, other: &$ty) -> bool {
        self.$fn().eq(other)
      }
    }

    impl<$lt> PartialEq<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn eq(&self, other: &$name<&$lt $ty>) -> bool {
        other.eq(self)
      }
    }

    impl<$lt> PartialOrd<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn partial_cmp(&self, other: &$ty) -> Option<core::cmp::Ordering> {
        self.$fn().partial_cmp(other)
      }
    }

    impl<$lt> PartialOrd<$name<&$lt $ty>> for $ty {
      #[inline(always)]
      fn partial_cmp(&self, other: &$name<&$lt $ty>) -> Option<core::cmp::Ordering> {
        other.partial_cmp(self).map(core::cmp::Ordering::reverse)
      }
    }

    impl<$lt> core::borrow::Borrow<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn borrow(&self) -> &$ty {
        self
      }
    }

    impl<$lt> AsRef<$ty> for $name<&$lt $ty> {
      #[inline(always)]
      fn as_ref(&self) -> &$ty {
        core::borrow::Borrow::borrow(self)
      }
    }

    impl<$lt> core::ops::Deref for $name<&$lt $ty> {
      type Target = $ty;

      #[inline(always)]
      fn deref(&self) -> &Self::Target {
        self.$fn()
      }
    }

    impl<$lt> From<$name<&$lt $ty>> for &$lt $ty {
      #[inline(always)]
      fn from(s: $name<&$lt $ty>) -> Self {
        s.$fn()
      }
    }
  };
}

mod string_lexer;

/// Lexers for standard GraphQL (draft specification).
///
/// This module provides zero-copy tokenization for GraphQL source text. All tokens
/// reference spans in the original source, avoiding unnecessary allocations.
///
/// # Token Streams
///
/// GraphQL lexing offers two complementary token types:
///
/// - **[`SyntacticToken`](graphql::syntactic::SyntacticToken)** - Fast token stream that skips trivia (whitespace, comments, commas)
///   - Use for: GraphQL servers, query execution, performance-critical parsing
///   - Benefits: Minimal memory, maximum speed
///
/// - **[`LosslessToken`](graphql::lossless::LosslessToken)** - Complete token stream that preserves all source information
///   - Use for: Code formatters, linters, IDEs, syntax highlighters
///   - Benefits: Perfect source reconstruction, access to comments and formatting
///
/// # Recognized Tokens
///
/// The GraphQL lexer recognizes:
/// - **Identifiers**: Names for types, fields, arguments, etc.
/// - **Literals**: Integers, floats, strings (inline and block), booleans, null
/// - **Punctuators**: `(`, `)`, `{`, `}`, `[`, `]`, `:`, `=`, `@`, `$`, `!`, `|`, `&`, `,`
/// - **Keywords**: `query`, `mutation`, `subscription`, `fragment`, `type`, `interface`, etc.
/// - **Trivia** (LosslessToken only): Whitespace, comments, commas
///
/// # Source Types
///
/// The lexer is generic over source type `S`:
/// - `&str`: Most common, UTF-8 validated
/// - `&[u8]`: For binary sources, can be converted to `&str` when needed
/// - `bytes::Bytes`: For shared ownership (requires `bytes` feature)
///
/// # Modules
///
/// - [`syntactic`](graphql::syntactic): Syntactic tokens (fast, skips trivia)
/// - [`lossless`](graphql::lossless): Lossless tokens (complete, preserves all formatting)
/// - [`error`](graphql::error): Lexer-specific error types
///
/// # Example
///
/// ```rust,ignore
/// use smear::lexer::graphql::syntactic::SyntacticToken;
/// use logosky::TokenStream;
///
/// let source = "query { user { id } }";
/// let tokens = TokenStream::<SyntacticToken<&str>>::new(source);
/// for token in tokens {
///   // Only syntactically significant tokens (whitespace automatically skipped)
/// }
/// ```
pub mod graphql;

/// Lexers for GraphQLX (extended GraphQL).
///
/// This module extends the standard GraphQL lexer with additional tokens for GraphQLX
/// features like generics, imports, map types, and namespacing.
///
/// # Token Streams
///
/// GraphQLX lexing offers two complementary token types:
///
/// - **[`SyntacticToken`](graphqlx::syntactic::SyntacticToken)** - Fast token stream that skips trivia (whitespace, comments, commas)
///   - Use for: GraphQLX servers, query execution, performance-critical parsing
///   - Benefits: Minimal memory, maximum speed
///
/// - **[`LosslessToken`](graphqlx::lossless::LosslessToken)** - Complete token stream that preserves all source information
///   - Use for: Code formatters, linters, IDEs, syntax highlighters
///   - Benefits: Perfect source reconstruction, access to comments and formatting
///
/// # Additional Tokens (Beyond GraphQL)
///
/// GraphQLX adds these tokens for advanced features:
/// - **Path separator**: `::` for namespaced types (`namespace::Type`)
/// - **Angle brackets**: `<`, `>` for generics (`Container<T>`)
/// - **Fat arrow**: `=>` for map types (`<Key => Value>`)
/// - **Arithmetic operators**: `+`, `-` for extended type operations
/// - **Additional keywords**: `import`, `from`, `as`, `where`, `map`, `set`
/// - **Asterisk**: `*` for wildcard imports
///
/// # Source Types
///
/// Like the GraphQL lexer, GraphQLX is generic over source type `S`:
/// - `&str`: UTF-8 validated strings
/// - `&[u8]`: Byte slices
/// - `bytes::Bytes`: Shared ownership (requires `bytes` feature)
/// - `hipstr::HipStr`: Hybrid storage (requires `hipstr` feature)
///
/// # Modules
///
/// - [`syntactic`](graphqlx::syntactic): Syntactic tokens (fast, skips trivia)
/// - [`lossless`](graphqlx::lossless): Lossless tokens (complete, preserves all formatting)
/// - [`error`](graphqlx::error): Lexer-specific error types
///
/// # Example
///
/// ```rust,ignore
/// use smear::lexer::graphqlx::syntactic::SyntacticToken;
/// use logosky::TokenStream;
///
/// let source = "import { User } from \"./types.graphqlx\"";
/// let tokens = TokenStream::<SyntacticToken<&str>>::new(source);
/// // Fast tokenization with trivia automatically skipped
/// ```
///
/// # Note
///
/// GraphQLX requires the `unstable` feature flag.
pub mod graphqlx;

mod handlers;
