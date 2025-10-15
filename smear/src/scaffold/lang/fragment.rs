use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

use crate::{keywords::On, punctuator::Spread};

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
pub struct FragmentName<S> {
  span: Span,
  value: S,
}

impl<S> PartialEq<S> for FragmentName<S>
where
  S: PartialEq,
{
  #[inline]
  fn eq(&self, other: &S) -> bool {
    self.source_ref().eq(other)
  }
}

impl<S> AsSpan<Span> for FragmentName<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for FragmentName<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for FragmentName<S> {
  type Components = (Span, S);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.value)
  }
}

impl<S> FragmentName<S> {
  #[inline]
  pub(crate) const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns a reference to the span covering the fragment name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the underlying source value.
  #[inline]
  pub const fn source(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns a reference to the underlying source value.
  #[inline]
  pub const fn source_ref(&self) -> &S {
    &self.value
  }
}

impl<S> core::fmt::Display for FragmentName<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.source_ref(), f)
  }
}

impl<S> core::ops::Deref for FragmentName<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.source_ref()
  }
}

impl<S> DisplayCompact for FragmentName<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<S> DisplayPretty for FragmentName<S>
where
  S: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

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
pub struct TypeCondition<Name> {
  span: Span,
  name: Name,
}

impl<Name> TypeCondition<Name> {
  pub(crate) const fn new(span: Span, name: Name) -> Self {
    Self { span, name }
  }

  /// Returns a reference to the span covering the entire type condition.
  ///
  /// The span includes both the `on` keyword and the type name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the type name that follows the `on` keyword.
  ///
  /// This identifies the specific type that the fragment condition applies to.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser that can parse a type condition with a custom name parser.
  ///
  /// This allows for greater flexibility in how type conditions are defined and parsed.
  /// The parser handles the complete type condition syntax including the `on` keyword
  /// and the type name.
  pub fn parser_with<'a, I, T, Error, E, P>(name_parser: P) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    On: Parseable<'a, I, T, Error>,
    P: Parser<'a, I, Name, E> + Clone,
  {
    On::parser()
      .ignore_then(name_parser)
      .map_with(|name, exa| Self {
        span: exa.span(),
        name,
      })
  }
}

impl<Name> AsSpan<Span> for TypeCondition<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for TypeCondition<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for TypeCondition<Name> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for TypeCondition<Name>
where
  Name: Parseable<'a, I, T, Error>,
  On: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser())
  }
}

impl<Name> core::fmt::Display for TypeCondition<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "on {}", self.name.display())
  }
}

impl<Name> DisplayCompact for TypeCondition<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayHuman::fmt(self, f)
  }
}

impl<Name> DisplayPretty for TypeCondition<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    DisplayCompact::fmt(self, f, &())
  }
}

impl<Name> DisplayHuman for TypeCondition<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
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
pub struct FragmentSpread<FragmentName, Directives> {
  span: Span,
  name: FragmentName,
  directives: Option<Directives>,
}

impl<FragmentName, Directives> AsSpan<Span> for FragmentSpread<FragmentName, Directives> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FragmentName, Directives> IntoSpan<Span> for FragmentSpread<FragmentName, Directives> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FragmentName, Directives> IntoComponents for FragmentSpread<FragmentName, Directives> {
  type Components = (Span, FragmentName, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives)
  }
}

impl<FragmentName, Directives> FragmentSpread<FragmentName, Directives> {
  #[inline]
  pub(crate) const fn new(span: Span, name: FragmentName, directives: Option<Directives>) -> Self {
    Self {
      span,
      name,
      directives,
    }
  }

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
  pub const fn name(&self) -> &FragmentName {
    &self.name
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
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, FP, DP>(
    fragment_name_parser: FP,
    directives_parser: DP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Error: 'a,
    Spread: Parseable<'a, I, T, Error> + 'a,
    DP: Parser<'a, I, Directives, E> + Clone,
    FP: Parser<'a, I, FragmentName, E> + Clone,
  {
    Spread::parser()
      .ignore_then(fragment_name_parser)
      .then(directives_parser.or_not())
      .map_with(|(name, directives), exa| Self {
        span: exa.span(),
        name,
        directives,
      })
  }
}

impl<'a, FragmentName, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for FragmentSpread<FragmentName, Directives>
where
  FragmentName: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  Spread: Parseable<'a, I, T, Error>,
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
    Self::parser_with(FragmentName::parser(), Directives::parser())
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
pub struct InlineFragment<TypeCondition, Directives, SelectionSet> {
  span: Span,
  type_condition: Option<TypeCondition>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<TypeCondition, Directives, SelectionSet> AsSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<TypeCondition, Directives, SelectionSet> IntoSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<TypeCondition, Directives, SelectionSet> IntoComponents
  for InlineFragment<TypeCondition, Directives, SelectionSet>
{
  type Components = (
    Span,
    Option<TypeCondition>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<TypeCondition, Directives, SelectionSet>
  InlineFragment<TypeCondition, Directives, SelectionSet>
{
  pub(crate) const fn new(
    span: Span,
    type_condition: Option<TypeCondition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      span,
      type_condition,
      directives,
      selection_set,
    }
  }

  /// Returns a reference to the span covering the entire inline fragment.
  ///
  /// The span includes the ellipsis, type condition (if present), directives,
  /// and the complete selection set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the type condition, if present.
  ///
  /// The type condition specifies which concrete type this fragment applies to.
  /// If no type condition is provided, the fragment applies to all possible types
  /// at this location.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition> {
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
  #[inline]
  pub fn parser_with<'a, I, T, Error, E, TP, DP, SP>(
    type_condition_parser: TP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    Error: 'a,
    Spread: Parseable<'a, I, T, Error> + 'a,
    SP: Parser<'a, I, SelectionSet, E> + Clone,
    DP: Parser<'a, I, Directives, E> + Clone,
    TP: Parser<'a, I, TypeCondition, E> + Clone,
  {
    Spread::parser()
      .ignore_then(type_condition_parser.or_not())
      .then(directives_parser.or_not())
      .then(selection_set_parser)
      .map_with(|((type_condition, directives), selection_set), exa| Self {
        span: exa.span(),
        type_condition,
        directives,
        selection_set,
      })
  }
}

impl<'a, TypeCondition, Directives, SelectionSet, I, T, Error> Parseable<'a, I, T, Error>
  for InlineFragment<TypeCondition, Directives, SelectionSet>
where
  TypeCondition: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  SelectionSet: Parseable<'a, I, T, Error>,
  Spread: Parseable<'a, I, T, Error>,
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
    Self::parser_with(
      TypeCondition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
  }
}
