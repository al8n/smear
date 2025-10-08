use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

use crate::keywords::Fragment;

/// Represents a named fragment definition in GraphQL.
///
/// A fragment definition defines a reusable set of fields that can be included
/// in GraphQL operations through fragment spreads. Fragments help reduce duplication
/// and improve maintainability by allowing common field selections to be defined
/// once and reused across multiple operations.
///
/// Fragments are particularly useful when working with complex schemas where
/// the same sets of fields are frequently requested together.
///
/// ## GraphQL Fragment Philosophy
///
/// Fragments support GraphQL's principle of avoiding duplication:
/// - **Reusability**: Define common field selections once
/// - **Maintainability**: Update field selections in one place
/// - **Readability**: Give meaningful names to complex field sets
/// - **Type safety**: Fragments are bound to specific types
/// - **Composition**: Fragments can include other fragments
///
/// ## Examples
///
/// ```text
/// # Simple fragment definition
/// fragment UserInfo on User {
///   id
///   name
///   email
/// }
///
/// # Fragment with description
/// """
/// Essential user information displayed in user cards
/// """
/// fragment UserCard on User {
///   id
///   name
///   avatar {
///     url
///     width
///     height
///   }
///   joinedAt
/// }
///
/// # Fragment with directives
/// fragment UserProfile on User @auth(requires: USER) {
///   id
///   name
///   email
///   phone @include(if: $showPhone)
///   address @include(if: $showAddress) {
///     street
///     city
///     country
///   }
/// }
///
/// # Complex nested fragment
/// """
/// Comprehensive user profile with all related data
/// """
/// fragment FullUserProfile on User @rateLimit(max: 10) {
///   ...UserCard
///   
///   profile {
///     bio
///     website
///     socialLinks {
///       platform
///       url
///     }
///   }
///   
///   posts(first: 5) {
///     edges {
///       node {
///         title
///         excerpt
///         publishedAt
///       }
///     }
///   }
/// }
///
/// # Fragment usage in operations
/// query GetUser($id: ID!, $showPhone: Boolean = false) {
///   user(id: $id) {
///     ...UserProfile
///   }
/// }
/// ```
///
/// ## Type Parameters
///
/// * `Directives` - The type representing directives applied to the fragment
/// * `SelectionSet` - The type representing the fragment's field selections
/// * `Span` - The type representing source location information
///
/// ## Grammar
///
/// ```text
/// FragmentDefinition : fragment FragmentName TypeCondition Directives? SelectionSet
/// ```
///
/// Spec: [Fragment Definition](https://spec.graphql.org/draft/#sec-Language.Fragments.Fragment-Definitions)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet> {
  span: Span,
  name: FragmentName,
  type_condition: TypeCondition,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet> AsSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet> IntoSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet> IntoComponents
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet>
{
  type Components = (
    Span,
    FragmentName,
    TypeCondition,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.name,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet>
{
  /// Returns a reference to the span covering the entire fragment definition.
  ///
  /// The span includes the optional description, fragment keyword, name,
  /// type condition, optional directives, and selection set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the fragment name.
  ///
  /// This is the identifier used to reference this fragment in fragment spreads
  /// within GraphQL operations. Fragment names must be unique within a document
  /// and cannot be the reserved word "on".
  #[inline]
  pub const fn name(&self) -> &FragmentName {
    &self.name
  }

  /// Returns a reference to the type condition.
  ///
  /// The type condition specifies which GraphQL type this fragment applies to.
  /// It consists of the "on" keyword followed by a type name. This ensures
  /// type safety by guaranteeing that the fragment's fields are valid for
  /// the specified type.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition {
    &self.type_condition
  }

  /// Returns a reference to the optional directives applied to this fragment.
  ///
  /// Directives provide metadata or specify behavior for the fragment,
  /// such as access control, conditional inclusion, or custom processing.
  /// Fragment-level directives affect the entire fragment when it's used.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the selection set containing the fragment's fields.
  ///
  /// The selection set defines what fields, nested objects, and other fragments
  /// to include when this fragment is spread into an operation. This is the
  /// core content of the fragment that gets reused.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  /// Creates a parser that can parse a complete fragment definition.
  ///
  /// This parser handles the full fragment definition syntax including all
  /// optional and required components. The parsing of selection set and
  /// directives is delegated to the provided parsers.
  pub fn parser_with<'src, I, T, Error, E, FP, TP, DP, SP>(
    fragment_name_parser: FP,
    type_condition_parser: TP,
    directives_parser: DP,
    selection_set_parser: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Fragment: Parseable<'src, I, T, Error> + 'src,
    FP: Parser<'src, I, FragmentName, E> + Clone,
    TP: Parser<'src, I, TypeCondition, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    Fragment::parser()
      .ignore_then(fragment_name_parser)
      .then(type_condition_parser)
      .then(directives_parser.or_not())
      .then(selection_set_parser)
      .map_with(
        |(((name, type_condition), directives), selection_set), exa| Self {
          name,
          type_condition,
          directives,
          selection_set,
          span: exa.span(),
        },
      )
  }
}

impl<'a, FragmentName, TypeCondition, Directives, SelectionSet, I, T, Error>
  Parseable<'a, I, T, Error>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet>
where
  Fragment: Parseable<'a, I, T, Error>,
  FragmentName: Parseable<'a, I, T, Error>,
  TypeCondition: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
  SelectionSet: Parseable<'a, I, T, Error>,
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
      FragmentName::parser(),
      TypeCondition::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
  }
}
