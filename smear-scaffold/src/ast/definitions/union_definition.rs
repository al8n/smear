use smear_lexer::tokit::{
  SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::{IntoComponents, sdl_display::{DisplayCompact, DisplayPretty}},
};
use core::marker::PhantomData;
use std::vec::Vec;



/// Represents a collection of member types that a GraphQL union can include.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionMemberTypes<Name, Container = Vec<Name>> {
  span: Span,
  members: Container,
  _m: PhantomData<Name>,
}

impl<Name, Container> AsSpan<Span> for UnionMemberTypes<Name, Container> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Container> IntoSpan<Span> for UnionMemberTypes<Name, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Container> IntoComponents for UnionMemberTypes<Name, Container> {
  type Components = (Span, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.members)
  }
}

impl<Name, Container> UnionMemberTypes<Name, Container> {
  /// Creates a new union member types list.
  #[inline]
  pub const fn new(span: Span, members: Container) -> Self {
    Self { span, members, _m: PhantomData }
  }

  /// Returns a reference to the span covering the entire union member types.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the container holding all member types in the union.
  #[inline]
  pub const fn members(&self) -> &Container {
    &self.members
  }

  /// Returns a reference to the container holding all member types in the union.
  #[inline]
  pub fn members_slice(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.members().as_ref()
  }
}

impl<Name, Container> DisplayCompact for UnionMemberTypes<Name, Container>
where
  Container: AsRef<[Name]>,
  Name: DisplayCompact,
{
  type Options = Name::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let members = self.members().as_ref();

    for member in members.iter() {
      write!(f, "|{}", member.display(opts))?;
    }
    Ok(())
  }
}

impl<Name, Container> DisplayPretty for UnionMemberTypes<Name, Container>
where
  Container: AsRef<[Name]>,
  Name: DisplayPretty,
{
  type Options = Name::Options;

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, opts: &Self::Options) -> core::fmt::Result {
    let members = self.members().as_ref();
    for member in members.iter() {
      writeln!(f, "\t| {}", member.display(opts))?;
    }
    Ok(())
  }
}

/// Represents a GraphQL Union type definition.
///
/// Union types represent objects that could be one of several possible types,
/// enabling polymorphic return values and flexible API design. They are essential
/// for representing heterogeneous collections and result types that can vary.
///
/// ## Examples
///
/// ```text
/// # Simple union type
/// union SearchResult = Post | User
///
/// # Union with description
/// """
/// Represents different types of content that can appear in a feed.
/// Supports posts, comments, and promotional content.
/// """
/// union FeedItem = Post | Comment | Advertisement
///
/// # Union with directives
/// union SearchResult @deprecated(reason: "Use SearchResultV2") = Post | User
///
/// # Complex union with multiple alternatives
/// """
/// Payment method union supporting various payment types.
/// Each type has different validation and processing requirements.
/// """
/// union PaymentMethod
///   @auth(required: true)
///   @validation(strict: true)
/// = CreditCard | PayPal | BankTransfer | Cryptocurrency | GiftCard
///
/// # Union without members (placeholder)
/// union PendingResult @experimental
/// ```
///
/// ## Usage in Queries
/// ```text
/// query GetSearchResults($query: String!) {
///   search(query: $query) {
///     ... on Post {
///       title
///       content  
///       author { name }
///     }
///     ... on User {
///       username
///       email
///       profile { bio }
///     }
///     ... on Comment {
///       text
///       author { name }
///       post { title }
///     }
///   }
/// }
/// ```
///
/// ## Grammar
/// ```text
/// UnionTypeDefinition:
///   Description? union Name Directives? UnionMemberTypes?
/// ```
///
/// Spec: [Union Type Definition](https://spec.graphql.org/draft/#sec-Union-Type-Definition)
#[derive(Debug, Clone, Copy)]
pub struct UnionTypeDefinition<Name, Directives, MemberTypes> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<Name, Directives, MemberTypes> AsSpan<Span>
  for UnionTypeDefinition<Name, Directives, MemberTypes>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, MemberTypes> IntoSpan<Span>
  for UnionTypeDefinition<Name, Directives, MemberTypes>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, MemberTypes> IntoComponents
  for UnionTypeDefinition<Name, Directives, MemberTypes>
{
  type Components = (Span, Name, Option<Directives>, Option<MemberTypes>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.directives, self.members)
  }
}

impl<Name, Directives, MemberTypes> UnionTypeDefinition<Name, Directives, MemberTypes> {
  /// Creates a new union type definition.
  #[inline]
  pub const fn new(span: Span, name: Name, directives: Option<Directives>, members: Option<MemberTypes>) -> Self {
    Self { span, name, directives, members }
  }

  /// Returns a reference to the span covering the entire union definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the union type.
  ///
  /// Union names should clearly indicate the common concept or category
  /// that all member types represent, following GraphQL naming conventions.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the optional directives applied to the union type.
  ///
  /// Union-level directives can specify authorization requirements, deprecation
  /// status, or other metadata that applies to the entire union.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the optional union member types.
  ///
  /// Union members define the possible Object types that can be returned.
  /// Unions without members are valid (placeholder unions) but uncommon.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    self.members.as_ref()
  }
}

/// Represents the content portion of a union type extension.
///
/// Union extensions can add directives or new member types to existing unions,
/// enabling schema evolution without modifying original definitions.
#[derive(Debug, Clone, Copy)]
pub enum UnionTypeExtensionData<Directives, MemberTypes> {
  /// Extension adds only directives to the union.
  ///
  /// Used to add metadata or behavioral modifications without changing
  /// the union's member types.
  ///
  /// ## Examples
  /// ```text
  /// extend union SearchResult @deprecated(reason: "Use SearchResultV2")
  /// extend union PaymentMethod @auth(required: true) @validation(strict: true)
  /// ```
  Directives(Directives),

  /// Extension adds new member types, optionally with directives.
  ///
  /// Used to expand the union with additional alternatives, possibly
  /// along with new directives.
  ///
  /// ## Examples
  /// ```text
  /// extend union SearchResult = Video | Podcast
  ///
  /// extend union FeedItem @cache(maxAge: 300) = Advertisement | Poll
  /// ```
  Members {
    /// Optional directives to apply with the member additions
    directives: Option<Directives>,
    /// New member types being added to the union
    members: MemberTypes,
  },
}

impl<Directives, MemberTypes> UnionTypeExtensionData<Directives, MemberTypes> {
  /// Returns the directives associated with this union extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives(directives) => Some(directives),
      Self::Members { directives, .. } => directives.as_ref(),
    }
  }

  /// Returns the member types being added by this union extension, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    match self {
      Self::Directives(_) => None,
      Self::Members { members, .. } => Some(members),
    }
  }
}

/// Represents a GraphQL Union type extension.
///
/// Union extensions enable incremental enhancement of existing union types
/// by adding new member types or directives without modifying the original
/// definition. They support schema evolution and modular development.
///
/// ## Examples
///
/// ```text
/// # Add new member types
/// extend union SearchResult = Video | Podcast
///
/// # Add directives
/// extend union SearchResult @deprecated(reason: "Use SearchResultV2")
///
/// # Add both directives and members
/// extend union FeedItem @cache(maxAge: 300) = Advertisement | Poll | Survey
/// ```
///
/// ## Grammar
/// ```text
/// UnionTypeExtension:
///   extend union Name Directives? UnionMemberTypes
///   | extend union Name Directives
/// ```
///
/// Spec: [Union Type Extension](https://spec.graphql.org/draft/#sec-Union-Type-Extension)
#[derive(Debug, Clone, Copy)]
pub struct UnionTypeExtension<Name, Directives, MemberTypes> {
  span: Span,
  name: Name,
  data: UnionTypeExtensionData<Directives, MemberTypes>,
}

impl<Name, Directives, MemberTypes> AsSpan<Span>
  for UnionTypeExtension<Name, Directives, MemberTypes>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, MemberTypes> IntoSpan<Span>
  for UnionTypeExtension<Name, Directives, MemberTypes>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, MemberTypes> IntoComponents
  for UnionTypeExtension<Name, Directives, MemberTypes>
{
  type Components = (Span, Name, UnionTypeExtensionData<Directives, MemberTypes>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.data)
  }
}

impl<Name, Directives, MemberTypes> UnionTypeExtension<Name, Directives, MemberTypes> {
  /// Creates a new union type extension.
  #[inline]
  pub const fn new(span: Span, name: Name, data: UnionTypeExtensionData<Directives, MemberTypes>) -> Self {
    Self { span, name, data }
  }

  /// Returns a reference to the span covering the entire union extension.
  ///
  /// The span includes the `extend union` keywords, union name, and all extension
  /// content (directives and/or member types).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the name of the union being extended.
  ///
  /// The union name identifies which existing union type this extension applies to.
  /// The referenced union must be defined elsewhere in the schema (either in the
  /// base schema or in previously applied extensions).
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns the directives applied by this extension, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.data.directives()
  }

  /// Returns the member types being added by this extension, if any.
  #[inline]
  pub const fn member_types(&self) -> Option<&MemberTypes> {
    self.data.member_types()
  }

  /// Returns a reference to the content being added by this extension.
  #[inline]
  pub const fn data(&self) -> &UnionTypeExtensionData<Directives, MemberTypes> {
    &self.data
  }
}
