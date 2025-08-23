use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  convert::*,
  lang::{ignored, keywords::Fragment, FragmentName, StringValue, TypeCondition},
  source::{Char, Slice, Source},
};

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
/// FragmentDefinition : Description? fragment FragmentName TypeCondition Directives? SelectionSet
/// ```
///
/// Spec: [Fragment Definition](https://spec.graphql.org/draft/#sec-Language.Fragments.Fragment-Definitions)
#[derive(Debug, Clone, Copy)]
pub struct FragmentDefinition<Directives, SelectionSet, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  fragment: Fragment<Span>,
  name: FragmentName<Span>,
  type_condition: TypeCondition<Span>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Directives, SelectionSet, Span> AsRef<Span>
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, SelectionSet, Span> IntoSpan<Span>
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, SelectionSet, Span> IntoComponents
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    Fragment<Span>,
    FragmentName<Span>,
    TypeCondition<Span>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.fragment,
      self.name,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Directives, SelectionSet, Span> FragmentDefinition<Directives, SelectionSet, Span> {
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
  pub const fn name(&self) -> &FragmentName<Span> {
    &self.name
  }

  /// Returns a reference to the optional description of the fragment definition.
  ///
  /// The description provides documentation for the fragment's purpose and usage.
  /// It appears before the fragment keyword and can be either a single-line string
  /// or a block string.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// Returns a reference to the `fragment` keyword.
  ///
  /// This provides access to the exact location and span information of the
  /// fragment keyword that starts the fragment definition.
  #[inline]
  pub const fn fragment_keyword(&self) -> &Fragment<Span> {
    &self.fragment
  }

  /// Returns a reference to the type condition.
  ///
  /// The type condition specifies which GraphQL type this fragment applies to.
  /// It consists of the "on" keyword followed by a type name. This ensures
  /// type safety by guaranteeing that the fragment's fields are valid for
  /// the specified type.
  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<Span> {
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
  pub fn parser_with<'src, I, E, SP, DP>(
    selection_set_parser: SP,
    directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(Fragment::parser().then_ignore(ignored()))
      .then(FragmentName::parser().then_ignore(ignored()))
      .then(TypeCondition::parser().then_ignore(ignored()))
      .then(directives_parser.then_ignore(ignored()).or_not())
      .then(selection_set_parser)
      .map_with(
        |(((((description, fragment), name), type_condition), directives), selection_set), sp| {
          Self {
            fragment,
            description,
            name,
            type_condition,
            directives,
            selection_set,
            span: Span::from_map_extra(sp),
          }
        },
      )
      .padded_by(ignored())
  }
}
