use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  convert::*,
  lang::{
    ignored, keywords,
    punct::{Equal, Pipe},
    StringValue,
  },
  source::{Char, Slice, Source},
};

use std::vec::Vec;

/// Represents the first union member type, where the pipe is optional.
///
/// In GraphQL's union member syntax, the first member type can optionally
/// be preceded by a pipe (`|`), but subsequent members must have pipes.
/// This distinction mirrors the interface implementation pattern and exists
/// for backwards compatibility and visual consistency.
///
/// ## Syntax Variations
/// ```text
/// # Both syntaxes are valid for the first member:
/// union SearchResult = Post           # No pipe
/// union SearchResult = | Post         # With pipe
///
/// # Multiple members - first can be with or without pipe:
/// union SearchResult = Post | User    # First without |
/// union SearchResult = | Post | User  # First with |
/// ```
///
/// The optional pipe design allows for consistent syntax when adding/removing
/// union members while maintaining backwards compatibility with existing schemas.
///
/// ## Grammar
/// ```text
/// LeadingUnionMemberType:
///   |? Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct LeadingUnionMemberType<Name, Span> {
  span: Span,
  pipe: Option<Pipe<Span>>,
  name: Name,
}

impl<Name, Span> AsRef<Span> for LeadingUnionMemberType<Name, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for LeadingUnionMemberType<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span> IntoComponents for LeadingUnionMemberType<Name, Span> {
  type Components = (Span, Option<Pipe<Span>>, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.name)
  }
}

impl<Name, Span> LeadingUnionMemberType<Name, Span> {
  /// Returns a reference to the span covering the leading union member type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the optional pipe token.
  ///
  /// The pipe is optional for the first union member, allowing both
  /// `Post` and `| Post` syntax variations.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<Span>> {
    self.pipe.as_ref()
  }

  /// Returns a reference to the union member type name.
  ///
  /// This must be the name of an Object type defined elsewhere in the schema.
  /// Union members can only be concrete Object types, not interfaces,
  /// unions, or scalar types.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for the first union member in a union definition.
  ///
  /// This parser handles the optional pipe syntax specific to the leading
  /// union member, accepting both `Name` and `| Name` patterns.
  ///
  /// ## Example Parsed Input
  /// ```text
  /// Post           # Without pipe
  /// | Post         # With pipe
  /// ```
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the leading union member.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP>(name_parser: NP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .or_not()
      .then(name_parser)
      .map_with(|(pipe, name), sp| Self {
        span: Span::from_map_extra(sp),
        pipe,
        name,
      })
  }
}

/// Represents a subsequent union member type, where the pipe is required.
///
/// After the first union member, all subsequent members must be preceded by
/// a pipe (`|`) to clearly separate them and maintain unambiguous parsing.
///
/// ## Why Pipes are Required
///
/// The pipe requirement for subsequent union members serves several purposes:
/// 1. **Visual Clarity**: Makes union alternatives easy to distinguish
/// 2. **Parsing Disambiguation**: Eliminates ambiguity in complex union definitions
/// 3. **Type Theory Alignment**: Mirrors disjunctive union notation (`A | B | C`)
/// 4. **Consistency**: Provides uniform syntax across union definitions
///
/// ## Examples
/// ```text
/// # Multiple union members - all after first require pipes:
/// union SearchResult = Post | User | Comment {
///   # Post is leading (| optional)
///   # User is subsequent (| required)  
///   # Comment is subsequent (| required)
/// }
/// ```
///
/// ## Grammar
/// ```text
/// UnionMemberType:
///   | Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct UnionMemberType<Name, Span> {
  span: Span,
  pipe: Pipe<Span>,
  name: Name,
}

impl<Name, Span> AsRef<Span> for UnionMemberType<Name, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for UnionMemberType<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span> IntoComponents for UnionMemberType<Name, Span> {
  type Components = (Span, Pipe<Span>, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.name)
  }
}

impl<Name, Span> UnionMemberType<Name, Span> {
  /// Returns a reference to the span covering this union member type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the required pipe token.
  ///
  /// Unlike `LeadingUnionMemberType`, the pipe is always required for
  /// subsequent union members in a union definition.
  #[inline]
  pub const fn pipe(&self) -> &Pipe<Span> {
    &self.pipe
  }

  /// Returns a reference to the union member type name.
  ///
  /// This must be the name of an Object type defined elsewhere in the schema.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Creates a parser for subsequent union members in a union definition.
  ///
  /// This parser requires a pipe followed by the member type name,
  /// enforcing the mandatory pipe syntax for non-leading union members.
  ///
  /// ## Example Parsed Input
  /// ```text
  /// | User         # Required pipe
  /// | Comment      # Required pipe
  /// ```
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the union member type.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP>(name_parser: NP) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .then(name_parser)
      .map_with(|(pipe, name), sp| Self {
        span: Span::from_map_extra(sp),
        pipe,
        name,
      })
  }
}

/// Represents a complete union member types list with proper pipe handling.
///
/// This structure correctly parses GraphQL's union member syntax, which follows
/// specific rules about when pipes are required vs optional. The distinction
/// between leading and subsequent members ensures correct parsing of union
/// type definitions with multiple alternatives.
///
/// ## Pipe Rules for Union Members
///
/// GraphQL uses a specific pipe syntax for union member types:
/// - **First Member**: Pipe is optional (`Post` or `| Post`)
/// - **Subsequent Members**: Pipe is required (`| User | Comment`)
///
/// ## Examples
/// ```text
/// # All valid syntax variations:
/// union Result = Post
/// union Result = | Post  
/// union Result = Post | User
/// union Result = | Post | User
/// union Result = Post | User | Comment | Product
/// ```
///
/// ## Grammar
/// ```text
/// UnionMemberTypes:
///   = LeadingUnionMemberType UnionMemberType*
///
/// LeadingUnionMemberType:
///   |? Name
///
/// UnionMemberType:
///   | Name
/// ```
#[derive(Debug, Clone, Copy)]
pub struct UnionMemberTypes<Name, Span, Container = Vec<UnionMemberType<Name, Span>>> {
  span: Span,
  eq: Equal<Span>,
  leading: LeadingUnionMemberType<Name, Span>,
  remaining: Container,
}

impl<Name, Span, Container> AsRef<Span> for UnionMemberTypes<Name, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span, Container> IntoSpan<Span> for UnionMemberTypes<Name, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span, Container> IntoComponents for UnionMemberTypes<Name, Span, Container> {
  type Components = (
    Span,
    Equal<Span>,
    LeadingUnionMemberType<Name, Span>,
    Container,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.eq, self.leading, self.remaining)
  }
}

impl<Name, Span, Container> UnionMemberTypes<Name, Span, Container> {
  /// Returns a reference to the span covering the entire union member types definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  /// Returns a reference to the equals token that begins the union member types.
  ///
  /// The equals sign separates the union name from its member types in the syntax
  /// `union Name = MemberTypes`.
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    &self.eq
  }
  /// Returns a reference to the first union member type.
  ///
  /// The leading member has special parsing rules where the pipe is optional,
  /// unlike subsequent members where it's required.
  #[inline]
  pub const fn leading(&self) -> &LeadingUnionMemberType<Name, Span> {
    &self.leading
  }

  /// Returns a reference to the container holding subsequent union member types.
  ///
  /// All members in this container have required pipes and represent the
  /// additional union alternatives beyond the first one.
  #[inline]
  pub const fn remaining(&self) -> &Container {
    &self.remaining
  }

  /// Creates a parser for complete union member type lists.
  ///
  /// This parser correctly handles the pipe rules, parsing the equals sign,
  /// the first member with optional pipe, followed by zero or more subsequent
  /// members with required pipes.
  ///
  /// ## Example Parsed Input
  /// ```text
  /// = Post
  /// = | Post  
  /// = Post | User
  /// = | Post | User | Comment
  /// ```
  pub fn parser_with<'src, I, E, NP>(
    name_parser: impl Fn() -> NP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    NP: Parser<'src, I, Name, E> + Clone,
    Span: crate::source::FromMapExtra<'src, I, E>,
    Container: chumsky::container::Container<UnionMemberType<Name, Span>>,
  {
    Equal::parser()
      .then_ignore(ignored())
      .then(LeadingUnionMemberType::parser_with(name_parser()).then_ignore(ignored()))
      .then(
        UnionMemberType::parser_with(name_parser())
          .padded_by(ignored())
          .repeated()
          .collect(),
      )
      .map_with(|((eq, leading), remaining), sp| Self {
        span: Span::from_map_extra(sp),
        eq,
        leading,
        remaining,
      })
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
pub struct UnionTypeDefinition<Name, Directives, MemberTypes, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  keyword: keywords::Union<Span>,
  name: Name,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<Name, Directives, MemberTypes, Span> AsRef<Span>
  for UnionTypeDefinition<Name, Directives, MemberTypes, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, MemberTypes, Span> IntoSpan<Span>
  for UnionTypeDefinition<Name, Directives, MemberTypes, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, MemberTypes, Span> IntoComponents
  for UnionTypeDefinition<Name, Directives, MemberTypes, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Union<Span>,
    Name,
    Option<Directives>,
    Option<MemberTypes>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.keyword,
      self.name,
      self.directives,
      self.members,
    )
  }
}

impl<Name, Directives, MemberTypes, Span> UnionTypeDefinition<Name, Directives, MemberTypes, Span> {
  /// Returns a reference to the span covering the entire union definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  /// Returns a reference to the optional description of the union type.
  ///
  /// Union descriptions should explain what the union represents, when to use
  /// different member types, and any important behavioral considerations.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }
  /// Returns a reference to the `union` keyword token.
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    &self.keyword
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
  pub const fn members(&self) -> Option<&MemberTypes> {
    self.members.as_ref()
  }

  /// Creates a parser for union type definitions.
  ///
  /// This parser handles the complete syntax for GraphQL union types, including
  /// optional descriptions, directives, and member type definitions.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the union type definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP, DP, MP>(
    name_parser: NP,
    directives_parser: DP,
    member_types: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then(keywords::Union::parser().padded_by(ignored()))
      .then(name_parser)
      .then(ignored().ignore_then(directives_parser).or_not())
      .then(ignored().ignore_then(member_types).or_not())
      .map_with(
        |((((description, keyword), name), directives), members), sp| Self {
          span: Span::from_map_extra(sp),
          description,
          keyword,
          name,
          directives,
          members,
        },
      )
  }
}

/// Represents the content portion of a union type extension.
///
/// Union extensions can add directives or new member types to existing unions,
/// enabling schema evolution without modifying original definitions.
#[derive(Debug, Clone, Copy)]
pub enum UnionTypeExtensionContent<Directives, MemberTypes> {
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
    fields: MemberTypes,
  },
}

impl<Directives, MemberTypes> UnionTypeExtensionContent<Directives, MemberTypes> {
  /// Creates a parser for union extension content.
  ///
  /// The parser tries member extensions first, then directive-only extensions.
  /// This ordering ensures that extensions with both directives and members
  /// are correctly parsed as the `Members` variant.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the union type extension content.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, DP, MP>(
    directives_parser: impl Fn() -> DP,
    member_types_parser: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    choice((
      directives_parser()
        .or_not()
        .then(ignored().ignore_then(member_types_parser))
        .map(|(directives, members)| Self::Members {
          directives,
          fields: members,
        }),
      directives_parser().map(|directives| Self::Directives(directives)),
    ))
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
pub struct UnionTypeExtension<Name, Directives, MemberTypes, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  keyword: keywords::Union<Span>,
  name: Name,
  content: UnionTypeExtensionContent<Directives, MemberTypes>,
}

impl<Name, Directives, MemberTypes, Span> AsRef<Span>
  for UnionTypeExtension<Name, Directives, MemberTypes, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name, Directives, MemberTypes, Span> IntoSpan<Span>
  for UnionTypeExtension<Name, Directives, MemberTypes, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Directives, MemberTypes, Span> IntoComponents
  for UnionTypeExtension<Name, Directives, MemberTypes, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Union<Span>,
    Name,
    UnionTypeExtensionContent<Directives, MemberTypes>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.extend,
      self.keyword,
      self.name,
      self.content,
    )
  }
}

impl<Name, Directives, MemberTypes, Span> UnionTypeExtension<Name, Directives, MemberTypes, Span> {
  /// Returns a reference to the span covering the entire union extension.
  ///
  /// The span includes the `extend union` keywords, union name, and all extension
  /// content (directives and/or member types).
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the `extend` keyword token.
  ///
  /// This provides access to the exact `extend` keyword that begins the extension,
  /// along with its precise source location.
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }

  /// Returns a reference to the `union` keyword token.
  ///
  /// This provides access to the `union` keyword that follows `extend`,
  /// helping distinguish union extensions from other types of extensions
  /// (object, interface, scalar, enum extensions) in schema analysis.
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    &self.keyword
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

  /// Returns a reference to the content being added by this extension.
  #[inline]
  pub const fn content(&self) -> &UnionTypeExtensionContent<Directives, MemberTypes> {
    &self.content
  }

  /// Creates a parser for union type extensions with comprehensive syntax support.
  ///
  /// This parser handles the complete `extend union` syntax, managing keyword
  /// recognition, name validation, and content parsing through a structured
  /// approach that ensures robust error handling and proper whitespace management.
  ///
  /// ## Notes
  ///
  /// This parser does not handle surrounding [ignored tokens].
  /// The calling parser is responsible for handling any necessary
  /// whitespace skipping or comment processing around the operation definition.
  ///
  /// [ignored tokens]: https://spec.graphql.org/draft/#sec-Language.Source-Text.Ignored-Tokens
  pub fn parser_with<'src, I, E, NP, DP, MP>(
    name_parser: NP,
    directives_parser: impl Fn() -> DP,
    member_types_parser: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    keywords::Extend::parser()
      .then(keywords::Union::parser().padded_by(ignored()))
      .then(name_parser)
      .then(
        ignored().ignore_then(UnionTypeExtensionContent::parser_with(
          directives_parser,
          member_types_parser,
        )),
      )
      .map_with(|(((extend, keyword), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        keyword,
        name,
        content,
      })
  }
}
