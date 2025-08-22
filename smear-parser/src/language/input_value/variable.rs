use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    super::{
      convert::*,
      name::Name,
      source::{Char, Slice, Source},
    },
    punct::Dollar,
  },
  ignored::ignored,
};

/// A GraphQL variable reference.
///
/// Represents a variable reference as defined by the GraphQL specification.
/// Variables are placeholders in GraphQL operations that get their values
/// from variables passed during execution. They enable parameterized queries
/// and mutations without requiring string interpolation or dynamic query
/// construction.
///
/// ## Specification Rules
///
/// GraphQL variable references follow strict formatting rules:
/// - **Dollar prefix**: Must start with a `$` character
/// - **Variable name**: Followed by a valid GraphQL name identifier
/// - **Case-sensitive**: Variable names are case-sensitive identifiers
/// - **Scope-aware**: Variables must be declared in the operation signature
///
/// ## Format
///
/// ```text
/// Variable ::= '$' Name
/// ```
///
/// ## Component Structure
///
/// Each variable reference contains:
/// - **Overall span**: Covers the entire variable including `$` and name
/// - **Dollar token**: The `$` prefix character with its position
/// - **Variable name**: The identifier following the `$`
///
/// ## Variable Declaration and Usage
///
/// Variables must be declared in operation signatures before use:
/// ```text
/// query GetUser($userId: ID!, $includeProfile: Boolean = false) {
///   user(id: $userId) {
///     name
///     profile @include(if: $includeProfile) {
///       bio
///     }
///   }
/// }
/// ```
///
/// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
#[derive(Debug, Clone, Copy)]
pub struct Variable<Span> {
  span: Span,
  /// The span of the dollar character
  dollar: Dollar<Span>,
  /// The name of the variable value
  name: Name<Span>,
}

impl<Span> Variable<Span> {
  /// Returns the variable name identifier.
  ///
  /// This provides access to the GraphQL name that identifies this variable.
  /// The name follows standard GraphQL identifier rules and is used to match
  /// the variable with values provided during execution.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns the dollar prefix token.
  ///
  /// This provides access to the `$` character that prefixes all variable
  /// references, including its exact source position. Useful for syntax
  /// highlighting and precise error reporting.
  #[inline]
  pub const fn dollar(&self) -> &Dollar<Span> {
    &self.dollar
  }

  /// Returns the source span of the entire variable reference.
  ///
  /// This span covers from the `$` character through the last character of
  /// the variable name, providing the complete source location for error
  /// reporting and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Creates a parser for GraphQL variable references.
  ///
  /// This parser implements the complete GraphQL variable specification,
  /// handling the dollar prefix, optional whitespace, and variable name
  /// according to GraphQL's lexical rules.
  ///
  /// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the variable.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Dollar::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(dollar, name), sp| Self {
        name,
        span: Span::from_map_extra(sp),
        dollar,
      })
  }
}

impl<Span> AsRef<Span> for Variable<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for Variable<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for Variable<Span> {
  type Components = (Span, Dollar<Span>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.dollar, self.name)
  }
}
