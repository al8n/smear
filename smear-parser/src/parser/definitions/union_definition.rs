use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords,
  language::{
    ignored::ignored,
    input_value::String,
    punct::{Equal, Pipe},
  },
  Name, SmearChar, Spanned,
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
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// First member: `|? Name`  (pipe is optional)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Pipe::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
      .map_with(|(pipe, name), sp| Self {
        span: Spanned::from(sp),
        pipe,
        name,
      })
  }

  /// Subsequent members: `| Name`  (pipe is **required**)
  pub fn parser_with_pipe<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    Pipe::parser()
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
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

  pub fn parser<'src, I, E, P>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    Container: chumsky::container::Container<UnionMemberType<Src, Span>>,
  {
    Equal::parser()
      .then_ignore(ignored())
      .then(
        UnionMemberType::parser::<I, E>()
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
pub struct UnionDefinition<MemberTypes, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<String<Src, Span>>,
  keyword: keywords::Union<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<MemberTypes, Directives, Src, Span> UnionDefinition<MemberTypes, Directives, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn description(
    &self,
  ) -> Option<&crate::parser::language::input_value::String<Src, Span>> {
    self.description.as_ref()
  }
  #[inline]
  pub const fn union_keyword(&self) -> &keywords::Union<Src, Span> {
    &self.keyword
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    // Description? 'union' Name Directives[Const]? UnionMemberTypes?
    String::parser()
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
  }
}

/// A union type extension
///
/// Spec: [UnionTypeExtension](https://spec.graphql.org/draft/#UnionTypeExtension)
#[derive(Debug, Clone)]
pub struct UnionExtension<MemberTypes, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  keyword: keywords::Union<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
  members: Option<MemberTypes>,
}

impl<MemberTypes, Directives, Src, Span> UnionExtension<MemberTypes, Directives, Src, Span> {
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
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  #[inline]
  pub const fn members(&self) -> Option<&MemberTypes> {
    self.members.as_ref()
  }
  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Union<Src, Span>,
    Name<Src, Span>,
    Option<Directives>,
    Option<MemberTypes>,
  ) {
    (
      self.span,
      self.extend,
      self.keyword,
      self.name,
      self.directives,
      self.members,
    )
  }

  pub fn parser_with<'src, I, E, DP, MP>(
    directives_parser: impl Fn() -> DP,
    member_types: impl Fn() -> MP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    DP: Parser<'src, I, Directives, E> + Clone,
    MP: Parser<'src, I, MemberTypes, E> + Clone,
  {
    // extend union Name
    let head = keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Union::parser())
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
      .then_ignore(ignored());

    // Negative look-ahead for '=' used by the "no members" branch.
    let no_eq_guard = just(I::Token::EQUAL).rewind().not().ignored();

    // Branch 1: extend union Name Directives[Const]? UnionMemberTypes
    let with_members = head
      .clone()
      .then(directives_parser().or_not())
      .then_ignore(ignored())
      .then(member_types())
      .map_with(
        |((((extend, keyword), name), directives), members), sp| Self {
          span: Spanned::from(sp),
          extend,
          keyword,
          name,
          directives,
          members: Some(members),
        },
      );

    // Branch 2: extend union Name Directives[Const]   (and ensure no '=' follows)
    let without_members = head
      .then(directives_parser())
      .then_ignore(ignored())
      .then_ignore(no_eq_guard)
      .map_with(|(((extend, keyword), name), directives), sp| Self {
        span: Spanned::from(sp),
        extend,
        keyword,
        name,
        directives: Some(directives),
        members: None,
      });

    choice((with_members, without_members))
  }
}
