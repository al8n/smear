use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    source::{Char, Slice, Source},
  },
  ignored, keywords,
  punct::Ellipsis,
  Const, Name,
};

/// Represents a type condition used in GraphQL fragments.
///
/// A type condition specifies which type a fragment applies to, using the `on` keyword
/// followed by a type name. This is essential for fragments to work with union and
/// interface types, where different fields may be available depending on the concrete type.
///
/// ## Examples
///
/// ```text
/// on User        # Apply fragment when type is User
/// on Product     # Apply fragment when type is Product
/// on SearchResult # Apply fragment when type is SearchResult
/// ```
///
/// ## Grammar
///
/// ```text
/// TypeCondition : on NamedType
/// ```
///
/// Spec: [Type Conditions](https://spec.graphql.org/draft/#sec-Type-Conditions)
#[derive(Debug, Clone, Copy)]
pub struct TypeCondition<Span> {
  span: Span,
  on: keywords::On<Span>,
  type_name: Name<Span>,
}

impl<Span> TypeCondition<Span> {
  /// Returns a reference to the span covering the entire type condition.
  ///
  /// The span includes both the `on` keyword and the type name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  /// Returns a reference to the `on` keyword.
  ///
  /// This provides access to the exact location and span information of the keyword.
  #[inline]
  pub const fn on_keyword(&self) -> &keywords::On<Span> {
    &self.on
  }
  /// Returns a reference to the type name that follows the `on` keyword.
  ///
  /// This identifies the specific type that the fragment condition applies to.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.type_name
  }

  /// Creates a parser that can parse a type condition.
  ///
  /// The parser expects the keyword `on` followed by optional ignored tokens
  /// and then a type name.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the type condition.
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
    keywords::On::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(on, type_name), sp| Self {
        span: Span::from_map_extra(sp),
        on,
        type_name,
      })
  }
}

impl<Span> AsRef<Span> for TypeCondition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeCondition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for TypeCondition<Span> {
  type Components = (Span, keywords::On<Span>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.on, self.type_name)
  }
}

/// Represents a fragment name with the special restriction that it cannot be "on".
///
/// In GraphQL, fragment names are regular identifiers with one exception: they cannot
/// be the keyword "on" since this would create ambiguity with type conditions.
/// This type ensures this constraint is enforced at the parser level.
///
/// ## Examples
///
/// ```text
/// # Valid fragment names
/// UserFragment
/// productInfo
/// SearchResultFields
///
/// # Invalid fragment name (would be rejected)
/// on  # This is reserved for type conditions
/// ```
///
/// ## Grammar
///
/// ```text
/// FragmentName : Name but not `on`
/// ```
///
/// Spec: [Fragment Name](https://spec.graphql.org/draft/#FragmentName)
#[derive(Debug, Clone, Copy)]
pub struct FragmentName<Span>(Span);

impl<Span> From<FragmentName<Span>> for Name<Span> {
  #[inline]
  fn from(value: FragmentName<Span>) -> Self {
    Self(value.into_span())
  }
}

impl<Span> AsRef<Span> for FragmentName<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FragmentName<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for FragmentName<Span> {
  type Components = Name<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into()
  }
}

impl<Span> FragmentName<Span> {
  /// Returns a reference to the span covering the fragment name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  /// Creates a parser that can parse a valid fragment name.
  ///
  /// The parser accepts any valid identifier except for the keyword "on",
  /// which is reserved for type conditions in fragment syntax.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the fragment name.
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
    Name::<Span>::parser()
      .to_slice()
      .filter(|slice| !slice.equivalent([I::Token::o, I::Token::n].into_iter()))
      .map_with(|_, sp| Self(Span::from_map_extra(sp)))
  }
}

/// Represents a fragment spread in GraphQL selection sets.
///
/// A fragment spread allows you to include a named fragment's selection set
/// at the current location. It consists of three dots (`...`) followed by
/// the fragment name and optional directives.
///
/// Fragment spreads enable reuse of common field selections and help avoid
/// duplication in GraphQL operations.
///
/// ## Examples
///
/// ```text
/// # Simple fragment spread
/// ...UserFragment
///
/// # Fragment spread with directives
/// ...UserFragment @include(if: $includeUser)
///
/// # Fragment spread with multiple directives
/// ...ProductDetails @skip(if: $hideProducts) @deprecated
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the fragment spread
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// FragmentSpread : ... FragmentName Directives?
/// ```
///
/// Spec: [Fragment Spreads](https://spec.graphql.org/draft/#sec-Language.Fragments.Fragment-Spreads)
#[derive(Debug, Clone, Copy)]
pub struct FragmentSpread<Directives, Span> {
  span: Span,
  ellipsis: Ellipsis<Span>,
  name: FragmentName<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> AsRef<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, Span> IntoSpan<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, Span> IntoComponents for FragmentSpread<Directives, Span> {
  type Components = (Span, Ellipsis<Span>, FragmentName<Span>, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ellipsis, self.name, self.directives)
  }
}

impl<Directives, Span> FragmentSpread<Directives, Span> {
  /// Returns a reference to the span covering the entire fragment spread.
  ///
  /// The span includes the ellipsis (`...`), fragment name, and any directives.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the fragment name being spread.
  ///
  /// This identifies which named fragment's selection set should be included
  /// at this location in the query.
  pub const fn name(&self) -> &FragmentName<Span> {
    &self.name
  }

  /// Returns a reference to the ellipsis (`...`) that starts the fragment spread.
  ///
  /// This provides access to the exact location and span information of the
  /// three-dot syntax that introduces fragment spreads.
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    &self.ellipsis
  }

  /// Returns a reference to the directives applied to this fragment spread, if any.
  ///
  /// Directives on fragment spreads can control when the fragment is included,
  /// provide metadata, or trigger custom processing behaviors.
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Creates a parser that can parse a fragment spread with custom directive parsing.
  ///
  /// The parser handles the complete fragment spread syntax including the ellipsis,
  /// fragment name, and optional directives.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the fragment spread.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, P>(directives: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Directives, E> + Clone,
  {
    Ellipsis::parser()
      .then_ignore(ignored())
      .then(FragmentName::parser())
      .then(ignored().ignore_then(directives).or_not())
      .map_with(|((ellipsis, name), directives), sp| Self {
        span: Span::from_map_extra(sp),
        ellipsis,
        name,
        directives,
      })
  }
}

/// Represents an inline fragment in GraphQL selection sets.
///
/// An inline fragment allows you to conditionally include fields based on the
/// concrete type of the object. Unlike fragment spreads, inline fragments don't
/// reference named fragments but instead include their selection set directly.
///
/// Inline fragments are essential when working with union and interface types,
/// where different fields are available depending on the actual type returned.
///
/// ## Examples
///
/// ```text
/// # Inline fragment with type condition
/// ... on User {
///   name
///   email
/// }
///
/// # Inline fragment without type condition (applies to all types)
/// ... {
///   id
///   __typename
/// }
///
/// # Inline fragment with directives
/// ... on Product @include(if: $showProducts) {
///   name
///   price
/// }
///
/// # Complex inline fragment
/// ... on SearchResult @skip(if: $hideResults) {
///   score
///   item {
///     ... on Product {
///       name
///       price
///     }
///     ... on User {
///       name
///       email
///     }
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the inline fragment
/// * `SelectionSet` - The type representing the nested field selections
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// InlineFragment : ... TypeCondition? Directives? SelectionSet
/// ```
///
/// Spec: [Inline Fragments](https://spec.graphql.org/draft/#sec-Inline-Fragments)
#[derive(Debug, Clone, Copy)]
pub struct InlineFragment<Directives, SelectionSet, Span> {
  span: Span,
  ellipsis: Ellipsis<Span>,
  type_condition: Option<TypeCondition<Span>>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Directives, SelectionSet, Span> AsRef<Span>
  for InlineFragment<Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, SelectionSet, Span> IntoSpan<Span>
  for InlineFragment<Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, SelectionSet, Span> IntoComponents
  for InlineFragment<Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Ellipsis<Span>,
    Option<TypeCondition<Span>>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.ellipsis,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Directives, SelectionSet, Span> InlineFragment<Directives, SelectionSet, Span> {
  /// Returns a reference to the span covering the entire inline fragment.
  ///
  /// The span includes the ellipsis, type condition (if present), directives,
  /// and the complete selection set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the ellipsis (`...`) that starts the inline fragment.
  ///
  /// This provides access to the exact location and span information of the
  /// three-dot syntax that introduces inline fragments.
  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    &self.ellipsis
  }

  /// Returns a reference to the type condition, if present.
  ///
  /// The type condition specifies which concrete type this fragment applies to.
  /// If no type condition is provided, the fragment applies to all possible types
  /// at this location.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Span>> {
    self.type_condition.as_ref()
  }

  /// Returns a reference to the directives applied to this inline fragment, if any.
  ///
  /// Directives on inline fragments can control when the fragment is included,
  /// provide metadata, or trigger custom processing behaviors.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the selection set containing the fields to be selected.
  ///
  /// The selection set is required for inline fragments and defines what fields
  /// should be included when this fragment's conditions are met.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  /// Creates a parser that can parse an inline fragment with custom component parsers.
  ///
  /// The parser handles the complete inline fragment syntax including the ellipsis,
  /// optional type condition, optional directives, and required selection set.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the inline fragment.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, S, D>(
    directives: D,
    selection_set: S,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    S: Parser<'src, I, SelectionSet, E> + Clone,
    D: Parser<'src, I, Directives, E> + Clone,
  {
    Ellipsis::parser()
      .then(
        ignored()
          .ignore_then(TypeCondition::<Span>::parser())
          .or_not(),
      )
      .then(ignored().ignore_then(directives).or_not())
      .then(ignored().ignore_then(selection_set))
      .map_with(
        |(((ell, type_condition), directives), selection_set), sp| Self {
          span: Span::from_map_extra(sp),
          ellipsis: ell,
          type_condition,
          directives,
          selection_set,
        },
      )
  }
}
