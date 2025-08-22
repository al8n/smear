use chumsky::{extra::ParserExtra, prelude::*, text::TextExpected, util::MaybeRef};
use derive_more::{AsMut, AsRef, From, Into};

use super::{
  language::{
    field::{self, Alias, TypeCondition},
    punct::{Ellipsis, LBrace, RBrace},
  },
  Char, Spanned,
};

use super::{arguments::Arguments, directives::Directives};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct FragmentSpread<Span>(field::FragmentSpread<Directives<Span>, Span>);

impl<Span> FragmentSpread<Span> {
  pub const fn span(&self) -> &Span {
    self.0.span()
  }
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    self.0.ellipsis()
  }
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    field::FragmentSpread::parser_with(Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct InlineFragment<Span>(
  field::InlineFragment<SelectionSetValue<Span>, Directives<Span>, Span>,
);

impl<Span> InlineFragment<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Span> {
    self.0.ellipsis()
  }

  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Span>> {
    self.0.type_condition()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSetValue<Span> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    field::InlineFragment::parser_with(SelectionSet::parser(), Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Field<Span>(
  field::Field<Arguments<Span>, Directives<Span>, SelectionSetValue<Span>, Src, Span>,
);

impl<Span> Field<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Span>> {
    self.0.alias()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Span>> {
    self.0.arguments()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSetValue<Span>> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    field::Field::parser_with(
      Arguments::parser(),
      Directives::parser(),
      SelectionSet::parser(),
    )
    .map(Self)
  }
}

#[derive(Debug, Clone)]
pub enum Selection<Span> {
  Field(Field<Span>),
  FragmentSpread(FragmentSpread<Span>),
  InlineFragment(InlineFragment<Span>),
}

impl<Span> Selection<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Selection::Field(f) => f.span(),
      Selection::FragmentSpread(fs) => fs.span(),
      Selection::InlineFragment(ifr) => ifr.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    recursive(|selection| {
      let selset = field::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Span>);

      let field_p =
        field::Field::parser_with(Arguments::parser(), Directives::parser(), selset.clone())
          .map(Field::<Span>);

      let inline_p = field::InlineFragment::parser_with(selset.clone(), Directives::parser())
        .map(InlineFragment::<Span>);

      let spread_p =
        field::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

      choice((
        field_p.map(Self::Field),
        spread_p.map(Self::FragmentSpread),
        inline_p.map(Self::InlineFragment),
      ))
    })
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct SelectionSetValue<Span>(field::SelectionSet<Selection<Span>, Span>);

impl<Span> SelectionSetValue<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn selections(&self) -> &[Selection<Span>] {
    self.0.selections().as_slice()
  }

  /// Consumes the selections.
  #[inline]
  pub fn into_selections(self) -> Vec<Selection<Span>> {
    self.0.into_selections()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
  {
    recursive(|selset| {
      let field_p =
        field::Field::parser_with(Arguments::parser(), Directives::parser(), selset.clone())
          .map(Field::<Span>);

      let inline_p = field::InlineFragment::parser_with(selset.clone(), Directives::parser())
        .map(InlineFragment::<Span>);

      let spread_p =
        field::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

      let selection = choice((
        field_p.map(Selection::<Span>::Field),
        spread_p.map(Selection::<Span>::FragmentSpread),
        inline_p.map(Selection::<Span>::InlineFragment),
      ));

      field::SelectionSet::parser_with(selection).map(Self)
    })
  }
}
