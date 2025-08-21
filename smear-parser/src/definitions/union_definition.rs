use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::super::{
  char::Char,
  keywords,
  language::{
    ignored::ignored,
    input_value::StringValue,
    punct::{Equal, Pipe},
  },
  name::Name,
  source::Source,
  spanned::Spanned,
};

use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct UnionMemberType<Src, Span> {
  span: Spanned<Src, Span>,
  pipe: Option<Pipe<Src, Span>>,
  name: Name<Src, Span>,
}

impl<Src, Span> UnionMemberType<Src, Span> {
  /// The span of the union member type.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// The pipe of the union member type.
  #[inline]
  pub const fn pipe(&self) -> Option<&Pipe<Src, Span>> {
    self.pipe.as_ref()
  }

  /// The name of the union member type.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Pipe::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Spanned::from(sp),
        pipe,
        name,
      })
  }

  /// Subsequent members: `| Name`  (pipe is **required**)
  pub fn parser_with_pipe<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Spanned::from(sp),
        pipe: Some(pipe),
        name,
      })
  }
}

/// `= |? NamedType ('|' NamedType)*`
#[derive(Debug, Clone)]
pub struct UnionMemberTypes<Src, Span, Container = Vec<UnionMemberType<Src, Span>>> {
  span: Spanned<Src, Span>,
  eq: Equal<Src, Span>,
  members: Container,
}

impl<Src, Span, Container> UnionMemberTypes<Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn eq(&self) -> &Equal<Src, Span> {
    &self.eq
  }
  #[inline]
  pub const fn members(&self) -> &Container {
    &self.members
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    Container: chumsky::container::Container<UnionMemberType<Src, Span>>,
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
        span: Spanned::from(sp),
        eq,
        members,
      })
  }
}

/// A union type definition
///
/// Spec: [UnionTypeDefinition](https://spec.graphql.org/draft/#UnionTypeDefinition)
#[derive(Debug, Clone)]
pub struct UnionDefinition<Directives, MemberTypes, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  keyword: keywords::Union<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<Directives, MemberTypes, Src, Span> UnionDefinition<Directives, MemberTypes, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Src, Span> {
    &self.keyword
  }
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
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
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    // Description? 'union' Name Directives[Const]? UnionMemberTypes?
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Union::parser())
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
      .then_ignore(ignored())
      .then(directives_parser.or_not())
      .then_ignore(ignored())
      .then(member_types.or_not())
      .map_with(
        |((((description, keyword), name), directives), members), sp| Self {
          span: Spanned::from(sp),
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
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
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
pub struct UnionExtension<Directives, MemberTypes, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  keyword: keywords::Union<Src, Span>,
  name: Name<Src, Span>,
  content: UnionExtensionContent<Directives, MemberTypes>,
}

impl<Directives, MemberTypes, Src, Span> UnionExtension<Directives, MemberTypes, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Src, Span> {
    &self.keyword
  }
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
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
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Union<Src, Span>,
    Name<Src, Span>,
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
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
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
        span: Spanned::from(sp),
        extend,
        keyword,
        name,
        content,
      })
      .padded_by(ignored())
  }
}
