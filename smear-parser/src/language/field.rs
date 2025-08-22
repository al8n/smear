use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    keywords,
    name::Name,
    source::{Char, Slice, Source},
    spanned::Spanned,
  },
  ignored::ignored,
  punct::{Colon, Ellipsis, LBrace, RBrace},
};

use core::marker::PhantomData;
use std::vec::Vec;

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
    Span: Spanned<'src, I, E>,
  {
    keywords::On::parser()
      .then_ignore(ignored())
      .then(Name::parser())
      .map_with(|(on, type_name), sp| Self {
        span: Spanned::from_map_extra(sp),
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

impl<Span> IntoSpanned<Span> for TypeCondition<Span> {
  #[inline]
  fn into_spanned(self) -> Span {
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

#[derive(Debug, Clone)]
pub struct FragmentSpread<Directives, Span> {
  span: Span,
  ellipsis: Ellipsis<Span>,
  name: Name<Span>,
  directives: Option<Directives>,
}

impl<Directives, Span> AsRef<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, Span> IntoSpanned<Span> for FragmentSpread<Directives, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Directives, Span> IntoComponents for FragmentSpread<Directives, Span> {
  type Components = (Span, Ellipsis<Span>, Name<Span>, Option<Directives>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.ellipsis, self.name, self.directives)
  }
}

impl<Directives, Span> FragmentSpread<Directives, Span> {
  pub const fn span(&self) -> &Span {
    &self.span
  }
  pub const fn name(&self) -> &Name<Span> {
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
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = super::ignored::ignored();

    // `!type_condition`   i.e., not: ws* "on" WS+ ...
    let not_type_condition = ws
      .clone()
      .ignore_then(just([I::Token::o, I::Token::n]))
      .then_ignore(ignored())
      .rewind()
      .not()
      .ignored();

    Ellipsis::parser()
      .then_ignore(ws.clone())
      .then_ignore(not_type_condition)
      .then(Name::parser())
      .then_ignore(ws.clone())
      .then(directives.or_not())
      .map_with(|((ellipsis_sp, name), directives), sp| Self {
        span: Spanned::from_map_extra(sp),
        ellipsis: ellipsis_sp,
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

impl<Directives, SelectionSet, Span> IntoSpanned<Span>
  for InlineFragment<Directives, SelectionSet, Span>
{
  #[inline]
  fn into_spanned(self) -> Span {
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
    Span: Spanned<'src, I, E>,
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
          span: Spanned::from_map_extra(sp),
          ellipsis: ell,
          type_condition,
          directives,
          selection_set,
        },
      )
  }
}

#[derive(Debug, Clone)]
pub struct Alias<Span> {
  span: Span,
  name: Name<Span>,
  colon: Colon<Span>,
}

impl<Span> AsRef<Span> for Alias<Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Span> IntoSpanned<Span> for Alias<Span> {
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Span> IntoComponents for Alias<Span> {
  type Components = (Span, Name<Span>, Colon<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon)
  }
}

impl<Span> Alias<Span> {
  /// Returns the span of the alias.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name of the alias.
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns the colon of the alias.
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// Returns a parser of the alias.
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    Name::<Span>::parser()
      .then_ignore(ignored())
      .then(Colon::parser())
      .map_with(|(name, colon), sp| Self {
        span: Spanned::from_map_extra(sp),
        name,
        colon,
      })
  }
}

#[derive(Debug, Clone)]
pub struct SelectionSet<Selection, Span, Container = Vec<Selection>> {
  span: Span,
  l_brace: LBrace<Span>,
  selections: Container,
  r_brace: RBrace<Span>,
  _marker: PhantomData<Selection>,
}

impl<Selection, Span, Container> SelectionSet<Selection, Span, Container> {
  /// Returns the span of the selection set.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the left brace of the selection set.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  /// Returns the selections of the selection set.
  #[inline]
  pub const fn selections(&self) -> &Container {
    &self.selections
  }

  /// Consumes the selection set and returns the contained selections.
  #[inline]
  pub fn into_selections(self) -> Container {
    self.selections
  }

  /// Returns the right brace of the selection set.
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  /// Returns a parser of the selection set.
  pub fn parser_with<'src, I, E, P>(selection_parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, Selection, E> + Clone,
    Container: chumsky::container::Container<Selection>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        selection_parser
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect()
          .then(RBrace::parser()),
      )
      .map_with(|(l_brace, (selections, r_brace)), sp| Self {
        span: Spanned::from_map_extra(sp),
        l_brace,
        selections,
        r_brace,
        _marker: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Field<Args, Directives, SelectionSet, Span> {
  span: Span,
  alias: Option<Alias<Span>>,
  name: Name<Span>,
  arguments: Option<Args>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Args, Directives, SelectionSet, Span> AsRef<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Args, Directives, SelectionSet, Span> IntoSpanned<Span>
  for Field<Args, Directives, SelectionSet, Span>
{
  #[inline]
  fn into_spanned(self) -> Span {
    self.span
  }
}

impl<Args, Directives, SelectionSet, Span> IntoComponents
  for Field<Args, Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<Alias<Span>>,
    Name<Span>,
    Option<Args>,
    Option<Directives>,
    Option<SelectionSet>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.alias,
      self.name,
      self.arguments,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Args, Directives, SelectionSet, Span> Field<Args, Directives, SelectionSet, Span> {
  /// Returns the span of the field
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the alias of the field
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Span>> {
    self.alias.as_ref()
  }

  /// Returns the span of the field name
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// Returns the arguments of the field
  #[inline]
  pub const fn arguments(&self) -> Option<&Args> {
    self.arguments.as_ref()
  }

  /// Returns the directives of the field
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns the selection set of the field
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }

  /// Returns a parser of the field
  pub fn parser_with<'src, I, E, AP, DP, SP>(
    args: AP,
    directives: DP,
    selection_set: SP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    AP: Parser<'src, I, Args, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    Alias::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored())
      .then(args.or_not())
      .then_ignore(ignored())
      .then(directives.or_not())
      .then_ignore(ignored())
      .then(selection_set.or_not())
      .map_with(
        |((((alias, name), arguments), directives), selection_set), sp| Self {
          span: Spanned::from_map_extra(sp),
          alias,
          name,
          arguments,
          directives,
          selection_set,
        },
      )
  }
}
