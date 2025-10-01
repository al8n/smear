use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::punctuator::{LBrace, RBrace};

use core::marker::PhantomData;

/// Represents a selection set in GraphQL syntax.
///
/// A selection set is a collection of fields, fragment spreads, and inline fragments
/// enclosed in curly braces. It defines what data should be fetched from a GraphQL
/// object or interface type. Selection sets are fundamental to GraphQL queries,
/// mutations, and subscriptions.
///
/// ## Examples
///
/// ```text
/// # Simple field selection
/// {
///   name
///   email
/// }
///
/// # Mixed selections with fields and fragments
/// {
///   id
///   name
///   ...UserFragment
///   ... on Admin {
///     permissions
///   }
///   posts {
///     title
///     content
///   }
/// }
///
/// # Nested selection sets
/// {
///   user {
///     profile {
///       name
///       avatar {
///         url
///         width
///         height
///       }
///     }
///   }
/// }
///
/// # Selection set with directives on fields
/// {
///   name
///   email @include(if: $showEmail)
///   phone @skip(if: $hidePhone)
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Selection` - The type of individual selections (fields, fragment spreads, inline fragments)
/// * `Span` - The type representing source location information for error reporting and tooling
/// * `Container` - The container type for storing selections (defaults to `Vec<Selection>`)
///
/// ## Grammar
///
/// ```text
/// SelectionSet : { Selection+ }
/// ```
///
/// Note: The grammar requires at least one selection (the `+` indicates one-or-more).
/// Empty selection sets `{}` are not valid in GraphQL.
///
/// Spec: [Selection Sets](https://spec.graphql.org/draft/#sec-Selection-Sets)
#[derive(Debug, Clone, Copy)]
pub struct SelectionSet<Selection, Container = Vec<Selection>> {
  span: Span,
  selections: Container,
  _marker: PhantomData<Selection>,
}

impl<Selection, Container> AsSpan<Span> for SelectionSet<Selection, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Selection, Container> IntoSpan<Span> for SelectionSet<Selection, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Selection, Container> IntoComponents for SelectionSet<Selection, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.selections)
  }
}

impl<Selection, Container> SelectionSet<Selection, Container> {
  /// Returns a reference to the span covering the entire selection set.
  ///
  /// The span includes the opening brace, all selections, and the closing brace.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all selections.
  ///
  /// The selections can include fields, fragment spreads, and inline fragments.
  /// This allows iteration over, indexing into, or otherwise working with
  /// the collection of parsed selections.
  #[inline]
  pub const fn selections(&self) -> &Container {
    &self.selections
  }

  /// Consumes the selection set and returns the underlying container of selections.
  ///
  /// This method takes ownership of the entire `SelectionSet` structure and
  /// extracts just the container holding the selection collection. This is useful
  /// for transferring ownership or when you no longer need the brace or span information.
  #[inline]
  pub fn into_selections(self) -> Container {
    self.selections
  }

  /// Creates a parser that can parse a selection set with a custom selection parser.
  ///
  /// This parser handles the complete selection set syntax including the braces
  /// and ensures at least one selection is present. The parsing of individual
  /// selections is delegated to the provided selection parser.
  pub fn parser_with<'a, I, T, Error, E, P>(
    selection_parser: P,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    LBrace: Parseable<'a, I, T, Error>,
    RBrace: Parseable<'a, I, T, Error>,
    P: Parser<'a, I, Selection, E> + Clone,
    Container: chumsky::container::Container<Selection> + 'a,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    LBrace::parser()
      .ignore_then(selection_parser.repeated().at_least(1).collect())
      .then_ignore(RBrace::parser())
      .map_with(|selections, exa| Self {
        span: exa.span(),
        selections,
        _marker: PhantomData,
      })
  }
}

impl<'a, Selection, Container, I, T, Error> Parseable<'a, I, T, Error>
  for SelectionSet<Selection, Container>
where
  Selection: Parseable<'a, I, T, Error>,
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  Container: chumsky::container::Container<Selection>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Self::parser_with(Selection::parser())
  }
}
