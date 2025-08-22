use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    keywords,
    name::Name,
    source::{Char, Slice, Source},
  },
  ignored::ignored,
  punct::Ellipsis,
};

#[derive(Debug, Clone)]
pub struct TypeCondition<Span> {
  span: Span,
  on: keywords::On<Span>,
  type_name: Name<Span>,
}

impl<Span> TypeCondition<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }
  #[inline]
  pub const fn on_keyword(&self) -> &keywords::On<Span> {
    &self.on
  }
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.type_name
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    keywords::On::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(on, type_name), sp| Self {
        span: Span::from_map_extra(sp),
        on,
        type_name,
      })
  }
}

impl<Span> AsRef<Span> for TypeCondition<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for TypeCondition<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for TypeCondition<Span> {
  type Components = (Span, keywords::On<Span>, Name<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.on, self.type_name)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct FragmentName<Span>(Span);

impl<Span> From<FragmentName<Span>> for Name<Span> {
  #[inline]
  fn from(value: FragmentName<Span>) -> Self {
    Self(value.into_span())
  }
}

impl<Span> AsRef<Span> for FragmentName<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpan<Span> for FragmentName<Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.0
  }
}

impl<Span> IntoComponents for FragmentName<Span> {
  type Components = Name<Span>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into()
  }
}

impl<Span> FragmentName<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.0
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    Name::<Span>::parser()
      .to_slice()
      .filter(|slice| !slice.equivalent([I::Token::o, I::Token::n].into_iter()))
      .map_with(|_, sp| Self(Span::from_map_extra(sp)))
  }
}

#[derive(Debug, Clone)]
pub struct FragmentSpread<Directives, Span> {
  span: Span,
  ellipsis: Ellipsis<Span>,
  name: FragmentName<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> AsRef<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, Span> IntoSpan<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, Span> IntoComponents for FragmentSpread<Directives, Span> {
  type Components = (Span, Ellipsis<Span>, FragmentName<Span>, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ellipsis, self.name, self.directives)
  }
}

impl<Directives, Span> FragmentSpread<Directives, Span> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn name(&self) -> &FragmentName<Span> {
    &self.name
  }
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    &self.ellipsis
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  pub fn parser_with<'src, I, E, P>(directives: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Directives, E> + Clone,
  {
    Ellipsis::parser()
      .then_ignore(ignored())
      .then(FragmentName::parser())
      .then_ignore(ignored())
      .then(directives.or_not())
      .map_with(|((ellipsis, name), directives), sp| Self {
        span: Span::from_map_extra(sp),
        ellipsis,
        name,
        directives,
      })
  }
}

#[derive(Debug, Clone)]
pub struct InlineFragment<Directives, SelectionSet, Span> {
  span: Span,
  ellipsis: Ellipsis<Span>,
  type_condition: Option<TypeCondition<Span>>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Directives, SelectionSet, Span> AsRef<Span>
  for InlineFragment<Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, SelectionSet, Span> IntoSpan<Span>
  for InlineFragment<Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, SelectionSet, Span> IntoComponents
  for InlineFragment<Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Ellipsis<Span>,
    Option<TypeCondition<Span>>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.ellipsis,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Directives, SelectionSet, Span> InlineFragment<Directives, SelectionSet, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    &self.ellipsis
  }

  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Span>> {
    self.type_condition.as_ref()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  pub fn parser_with<'src, I, E, S, D>(
    selection_set: S,
    directives: D,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    S: Parser<'src, I, SelectionSet, E> + Clone,
    D: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = super::ignored::ignored();

    Ellipsis::parser()
      .then_ignore(ws.clone())
      .then(TypeCondition::<Span>::parser().or_not())
      .then_ignore(ws.clone())
      .then(directives.or_not())
      .then_ignore(ws.clone())
      .then(selection_set)
      .map_with(
        |(((ell, type_condition), directives), selection_set), sp| Self {
          span: Span::from_map_extra(sp),
          ellipsis: ell,
          type_condition,
          directives,
          selection_set,
        },
      )
  }
}
