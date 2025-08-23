use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    source::{Char, Slice, Source},
  },
  ignored,
  punct::{LBrace, RBrace},
};

use core::marker::PhantomData;
use std::vec::Vec;

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
pub struct SelectionSet<Selection, Span, Container = Vec<Selection>> {
  span: Span,
  l_brace: LBrace<Span>,
  selections: Container,
  r_brace: RBrace<Span>,
  _marker: PhantomData<Selection>,
}

impl<Selection, Span, Container> AsRef<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Selection, Span, Container> IntoSpan<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Selection, Span, Container> IntoComponents for SelectionSet<Selection, Span, Container> {
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.selections, self.r_brace)
  }
}

impl<Selection, Span, Container> SelectionSet<Selection, Span, Container> {
  /// Returns a reference to the span covering the entire selection set.
  ///
  /// The span includes the opening brace, all selections, and the closing brace.
  /// This is useful for error reporting, syntax highlighting, and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the opening left brace (`{`) of the selection set.
  ///
  /// This provides access to the exact location and span information of the
  /// opening delimiter.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
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

  /// Returns a reference to the closing right brace (`}`) of the selection set.
  ///
  /// This provides access to the exact location and span information of the
  /// closing delimiter.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Creates a parser that can parse a selection set with a custom selection parser.
  ///
  /// This parser handles the complete selection set syntax including the braces
  /// and ensures at least one selection is present. The parsing of individual
  /// selections is delegated to the provided selection parser.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the selection set.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(selection_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Selection, E> + Clone,
    Container: chumsky::container::Container<Selection>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        selection_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect()
          .then(RBrace::parser()),
      )
      .map_with(|(l_brace, (selections, r_brace)), sp| Self {
        span: Span::from_map_extra(sp),
        l_brace,
        selections,
        r_brace,
        _marker: PhantomData,
      })
  }
}
