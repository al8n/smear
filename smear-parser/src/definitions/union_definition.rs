use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  convert::*,
  lang::{
    ignored, keywords,
    punct::{Equal, Pipe},
    Name, StringValue,
  },
  source::{Char, Slice, Source},
};

use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct LeadingUnionMemberType<Span> {
  span: Span,
  pipe: Option<Pipe<Span>>,
  name: Name<Span>,
}

impl<Span> AsRef<Span> for LeadingUnionMemberType<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for LeadingUnionMemberType<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for LeadingUnionMemberType<Span> {
  type Components = (Span, Option<Pipe<Span>>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.name)
  }
}

impl<Span> LeadingUnionMemberType<Span> {
  /// The span of the union member type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The pipe of the union member type.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<Span>> {
    self.pipe.as_ref()
  }

  /// The name of the union member type.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .or_not()
      .then(Name::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Span::from_map_extra(sp),
        pipe,
        name,
      })
  }
}

#[derive(Debug, Clone)]
pub struct UnionMemberType<Span> {
  span: Span,
  pipe: Pipe<Span>,
  name: Name<Span>,
}

impl<Span> AsRef<Span> for UnionMemberType<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for UnionMemberType<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for UnionMemberType<Span> {
  type Components = (Span, Pipe<Span>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.pipe, self.name)
  }
}

impl<Span> UnionMemberType<Span> {
  /// The span of the union member type.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// The pipe of the union member type.
  #[inline]
  pub const fn pipe(&self) -> &Pipe<Span> {
    &self.pipe
  }

  /// The name of the union member type.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Subsequent members: `| Name`  (pipe is **required**)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Span::from_map_extra(sp),
        pipe,
        name,
      })
  }
}

/// `= |? NamedType ('|' NamedType)*`
#[derive(Debug, Clone)]
pub struct UnionMemberTypes<Span, Container = Vec<UnionMemberType<Span>>> {
  span: Span,
  eq: Equal<Span>,
  leading: LeadingUnionMemberType<Span>,
  remaining: Container,
}

impl<Span, Container> AsRef<Span> for UnionMemberTypes<Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span, Container> IntoSpan<Span> for UnionMemberTypes<Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span, Container> IntoComponents for UnionMemberTypes<Span, Container> {
  type Components = (Span, Equal<Span>, LeadingUnionMemberType<Span>, Container);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.eq, self.leading, self.remaining)
  }
}

impl<Span, Container> UnionMemberTypes<Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    &self.eq
  }
  #[inline]
  pub const fn leading(&self) -> &LeadingUnionMemberType<Span> {
    &self.leading
  }

  #[inline]
  pub const fn remaining(&self) -> &Container {
    &self.remaining
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    Container: chumsky::container::Container<UnionMemberType<Span>>,
  {
    Equal::parser()
      .then_ignore(ignored())
      .then(LeadingUnionMemberType::parser().then_ignore(ignored()))
      .then(
        UnionMemberType::parser()
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

/// A union type definition
///
/// Spec: [UnionTypeDefinition](https://spec.graphql.org/draft/#UnionTypeDefinition)
#[derive(Debug, Clone)]
pub struct UnionDefinition<Directives, MemberTypes, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  keyword: keywords::Union<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<Directives, MemberTypes, Span> AsRef<Span> for UnionDefinition<Directives, MemberTypes, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, MemberTypes, Span> IntoSpan<Span>
  for UnionDefinition<Directives, MemberTypes, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, MemberTypes, Span> IntoComponents
  for UnionDefinition<Directives, MemberTypes, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    keywords::Union<Span>,
    Name<Span>,
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

impl<Directives, MemberTypes, Span> UnionDefinition<Directives, MemberTypes, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    &self.keyword
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  #[inline]
  pub const fn members(&self) -> Option<&MemberTypes> {
    self.members.as_ref()
  }

  pub fn parser_with<'src, I, E, DP, MP>(
    directives_parser: DP,
    member_types: MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    // Description? 'union' Name Directives[Const]? UnionMemberTypes?
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Union::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .then_ignore(ignored())
      .then(member_types.or_not())
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
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub enum UnionExtensionContent<Directives, MemberTypes> {
  Directives(Directives),
  Members {
    directives: Option<Directives>,
    fields: MemberTypes,
  },
}

impl<Directives, MemberTypes> UnionExtensionContent<Directives, MemberTypes> {
  pub fn parser_with<'src, I, E, DP, MP>(
    directives_parser: impl Fn() -> DP,
    member_types: impl Fn() -> MP,
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
        .then_ignore(ignored())
        .or_not()
        .then(member_types())
        .map(|(directives, members)| Self::Members {
          directives,
          fields: members,
        }),
      directives_parser().map(|directives| Self::Directives(directives)),
    ))
  }
}

/// A union type extension
///
/// Spec: [UnionTypeExtension](https://spec.graphql.org/draft/#UnionTypeExtension)
#[derive(Debug, Clone)]
pub struct UnionExtension<Directives, MemberTypes, Span> {
  span: Span,
  extend: keywords::Extend<Span>,
  keyword: keywords::Union<Span>,
  name: Name<Span>,
  content: UnionExtensionContent<Directives, MemberTypes>,
}

impl<Directives, MemberTypes, Span> AsRef<Span> for UnionExtension<Directives, MemberTypes, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, MemberTypes, Span> IntoSpan<Span>
  for UnionExtension<Directives, MemberTypes, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, MemberTypes, Span> IntoComponents
  for UnionExtension<Directives, MemberTypes, Span>
{
  type Components = (
    Span,
    keywords::Extend<Span>,
    keywords::Union<Span>,
    Name<Span>,
    UnionExtensionContent<Directives, MemberTypes>,
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

impl<Directives, MemberTypes, Span> UnionExtension<Directives, MemberTypes, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Span> {
    &self.extend
  }
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Span> {
    &self.keyword
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }
  #[inline]
  pub const fn content(&self) -> &UnionExtensionContent<Directives, MemberTypes> {
    &self.content
  }

  pub fn parser_with<'src, I, E, DP, MP>(
    directives_parser: impl Fn() -> DP,
    member_types: impl Fn() -> MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Union::parser())
      .then_ignore(ignored())
      .then(Name::parser().then_ignore(ignored()))
      .then(UnionExtensionContent::parser_with(
        directives_parser,
        member_types,
      ))
      .map_with(|(((extend, keyword), name), content), sp| Self {
        span: Span::from_map_extra(sp),
        extend,
        keyword,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
