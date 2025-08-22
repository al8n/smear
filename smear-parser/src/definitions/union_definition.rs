use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  lang::{
    ignored, keywords,
    punct::{Equal, Pipe},
    Name, StringValue,
  },
  source::{Char, Slice, Source},
};

use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct UnionMemberType<Span> {
  span: Span,
  pipe: Option<Pipe<Span>>,
  name: Name<Span>,
}

impl<Span> UnionMemberType<Span> {
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
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Span::from_map_extra(sp),
        pipe,
        name,
      })
  }

  /// Subsequent members: `| Name`  (pipe is **required**)
  pub fn parser_with_pipe<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
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
        pipe: Some(pipe),
        name,
      })
  }
}

/// `= |? NamedType ('|' NamedType)*`
#[derive(Debug, Clone)]
pub struct UnionMemberTypes<Span, Container = Vec<UnionMemberType<Span>>> {
  span: Span,
  eq: Equal<Span>,
  members: Container,
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
  pub const fn members(&self) -> &Container {
    &self.members
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
      .then(
        UnionMemberType::parser()
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .map_with(|(eq, members), sp| Self {
        span: Span::from_map_extra(sp),
        eq,
        members,
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

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    keywords::Extend<Span>,
    keywords::Union<Span>,
    Name<Span>,
    UnionExtensionContent<Directives, MemberTypes>,
  ) {
    (
      self.span,
      self.extend,
      self.keyword,
      self.name,
      self.content,
    )
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
