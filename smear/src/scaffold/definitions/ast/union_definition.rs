use core::marker::PhantomData;

use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, sdl_display::DisplaySDL},
};

use crate::{
  error::UnexpectedEndOfUnionExtensionError,
  hints::UnionTypeExtensionHint,
  keywords::{Extend, Union},
  punctuator::{Equal, Pipe},
};

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
    self.members.as_ref()
  }
}

impl<'a, Name, Container, I, T, Error> Parseable<'a, I, T, Error>
  for UnionMemberTypes<Name, Container>
where
  Container: chumsky::container::Container<Name>,
  Name: Parseable<'a, I, T, Error>,
  Pipe: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
    T: Token<'a>,
  {
    Name::parser()
      .separated_by(Pipe::parser())
      .allow_leading()
      .at_least(1)
      .collect()
      .map_with(|members, exa| {
        let span = exa.span();
        Self {
          span,
          members,
          _m: PhantomData,
        }
      })
  }
}

impl<Name, Container> DisplaySDL for UnionMemberTypes<Name, Container>
where
  Container: AsRef<[Name]>,
  Name: DisplaySDL,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();

    for (i, member) in members.iter().enumerate() {
      if i == 0 {
        write!(f, " {}", member.display())?;
        continue;
      }
      write!(f, " | {}", member.display())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_compact(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();

    for member in members.iter() {
      write!(f, "|{}", member.display_compact())?;
    }
    Ok(())
  }

  #[inline]
  fn fmt_pretty(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let members = self.members().as_ref();
    for member in members.iter() {
      writeln!(f, "\t| {}", member.display_pretty())?;
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

  /// Creates a parser for union type definitions.
  ///
  /// This parser handles the complete syntax for GraphQL union types, including
  /// optional descriptions, directives, and member type definitions.
  pub fn parser_with<'src, I, T, Error, E, NP, DP, MP>(
    name_parser: NP,
    directives_parser: DP,
    member_types: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Union: Parseable<'src, I, T, Error> + Clone,
    Equal: Parseable<'src, I, T, Error> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
  {
    Union::parser()
      .ignore_then(name_parser)
      .then(directives_parser.or_not())
      .then(Equal::parser().ignore_then(member_types).or_not())
      .map_with(|((name, directives), members), exa| Self {
        span: exa.span(),
        name,
        directives,
        members,
      })
  }
}

impl<'a, Name, Directives, MemberTypes, I, T, Error> Parseable<'a, I, T, Error>
  for UnionTypeDefinition<Name, Directives, MemberTypes>
where
  Union: Parseable<'a, I, T, Error> + Clone,
  Equal: Parseable<'a, I, T, Error> + Clone,
  Directives: Parseable<'a, I, T, Error>,
  MemberTypes: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Directives::parser(), MemberTypes::parser())
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

  /// Creates a parser for union type extensions with comprehensive syntax support.
  ///
  /// This parser handles the complete `extend union` syntax, managing keyword
  /// recognition, name validation, and content parsing through a structured
  /// approach that ensures robust error handling and proper whitespace management.
  pub fn parser_with<'src, I, T, Error, E, NP, DP, MP>(
    name_parser: NP,
    directives_parser: DP,
    member_types_parser: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    T: Token<'src>,
    I: Tokenizer<'src, T, Slice = <<T::Logos as Logos<'src>>::Source as Source>::Slice<'src>>,
    Error: UnexpectedEndOfUnionExtensionError + 'src,
    E: ParserExtra<'src, I, Error = Error> + 'src,
    Extend: Parseable<'src, I, T, Error> + Clone,
    Union: Parseable<'src, I, T, Error> + Clone,
    Equal: Parseable<'src, I, T, Error> + Clone,
    NP: Parser<'src, I, Name, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    Extend::parser()
      .then(Union::parser())
      .ignore_then(name_parser)
      .then(directives_parser.or_not())
      .then(Equal::parser().ignore_then(member_types_parser).or_not())
      .try_map_with(|((name, directives), members), exa| {
        let data = match (directives, members) {
          (directives, Some(members)) => UnionTypeExtensionData::Members {
            directives,
            members,
          },
          (Some(directives), None) => UnionTypeExtensionData::Directives(directives),
          (None, None) => {
            return Err(Error::unexpected_end_of_union_extension(
              exa.span(),
              UnionTypeExtensionHint::DirectivesOrUnionMemberTypes,
            ));
          }
        };

        Ok(Self {
          span: exa.span(),
          name,
          data,
        })
      })
  }
}

impl<'a, Name, Directives, MemberTypes, I, T, Error> Parseable<'a, I, T, Error>
  for UnionTypeExtension<Name, Directives, MemberTypes>
where
  Error: UnexpectedEndOfUnionExtensionError,
  Extend: Parseable<'a, I, T, Error> + Clone,
  Union: Parseable<'a, I, T, Error> + Clone,
  Equal: Parseable<'a, I, T, Error> + Clone,
  Directives: Parseable<'a, I, T, Error>,
  MemberTypes: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
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
    Self::parser_with(Name::parser(), Directives::parser(), MemberTypes::parser())
  }
}
