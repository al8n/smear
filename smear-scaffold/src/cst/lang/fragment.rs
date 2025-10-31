use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};

use crate::cst::Padding;

/// CST representation of a fragment spread: `...fragmentName`
///
/// Unlike the AST version, preserves:
/// - The spread operator `...` with its padding
/// - The fragment name with its padding
/// - Optional directives
///
/// ## Examples
/// ```text
/// ...UserFields
/// ... ProfileFragment @include(if: true)
/// ...  fragmentName  # preserves spacing
/// ```
#[derive(Debug, Clone)]
pub struct FragmentSpread<Name, Directives, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the spread operator
  spread_padding: Padding<S, TriviaContainer>,
  /// The fragment name
  name: Name,
  /// Optional directives
  directives: Option<Directives>,
}

impl<Name, Directives, S, TriviaContainer> AsSpan<Span>
  for FragmentSpread<Name, Directives, S, TriviaContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, S, TriviaContainer> IntoSpan<Span>
  for FragmentSpread<Name, Directives, S, TriviaContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, S, TriviaContainer> IntoComponents
  for FragmentSpread<Name, Directives, S, TriviaContainer>
{
  type Components = (Span, Padding<S, TriviaContainer>, Name, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.spread_padding, self.name, self.directives)
  }
}

impl<Name, Directives, S, TriviaContainer> FragmentSpread<Name, Directives, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST FragmentSpread.
  pub fn new(span: Span, name: Name, directives: Option<Directives>) -> Self {
    Self {
      span,
      spread_padding: Padding::new(),
      name,
      directives,
    }
  }

  /// Creates a new CST FragmentSpread with spread padding.
  pub const fn with_spread_padding(
    span: Span,
    spread_padding: Padding<S, TriviaContainer>,
    name: Name,
    directives: Option<Directives>,
  ) -> Self {
    Self {
      span,
      spread_padding,
      name,
      directives,
    }
  }
}

impl<Name, Directives, S, TriviaContainer> FragmentSpread<Name, Directives, S, TriviaContainer> {
  /// Returns the span covering the entire fragment spread.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the spread padding.
  #[inline]
  pub const fn spread_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.spread_padding
  }

  /// Returns a reference to the fragment name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the directives, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
}

/// CST representation of an inline fragment: `... on Type { selections }`
///
/// Unlike the AST version, preserves:
/// - The spread operator `...` with its padding
/// - Optional type condition with `on` keyword and its padding
/// - Optional directives
/// - Selection set with braces and padding
///
/// ## Examples
/// ```text
/// ... on User { id name }
/// ... { id }  # no type condition
/// ... on Admin @include(if: true) { role }
/// ```
#[derive(Debug, Clone)]
pub struct InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// Padding around the spread operator
  spread_padding: Padding<S, TriviaContainer>,
  /// Optional type condition with `on` keyword padding
  type_condition: Option<(Padding<S, TriviaContainer>, TypeCondition)>,
  /// Optional directives
  directives: Option<Directives>,
  /// Selection set
  selection_set: SelectionSet,
}

impl<TypeCondition, Directives, SelectionSet, S, TriviaContainer> AsSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<TypeCondition, Directives, SelectionSet, S, TriviaContainer> IntoSpan<Span>
  for InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<TypeCondition, Directives, SelectionSet, S, TriviaContainer> IntoComponents
  for InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  type Components = (
    Span,
    Padding<S, TriviaContainer>,
    Option<(Padding<S, TriviaContainer>, TypeCondition)>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.spread_padding,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
  InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST InlineFragment.
  pub fn new(
    span: Span,
    type_condition: Option<TypeCondition>,
    directives: Option<Directives>,
    selection_set: SelectionSet,
  ) -> Self {
    Self {
      span,
      spread_padding: Padding::new(),
      type_condition: type_condition.map(|tc| (Padding::new(), tc)),
      directives,
      selection_set,
    }
  }
}

impl<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
  InlineFragment<TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  /// Returns the span covering the entire inline fragment.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the spread padding.
  #[inline]
  pub const fn spread_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.spread_padding
  }

  /// Returns a reference to the type condition with its padding, if present.
  #[inline]
  pub const fn type_condition(&self) -> Option<&(Padding<S, TriviaContainer>, TypeCondition)> {
    self.type_condition.as_ref()
  }

  /// Returns a reference to the directives, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the selection set.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }
}
