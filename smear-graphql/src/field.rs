use chumsky::{extra::ParserExtra, prelude::*};
use derive_more::{AsMut, AsRef, From, Into, IsVariant, TryUnwrap, Unwrap};

use smear_parser::{
  lang::{
    self,
    punct::{Ellipsis, LBrace, RBrace},
    *,
  },
  source::{self, Char, Slice, Source},
};

use super::{arguments::Arguments, directives::Directives};

#[derive(Debug, Clone, derive_more::From, derive_more::Into)]
pub struct FragmentSpread<Span>(pub(super) lang::FragmentSpread<Directives<Span>, Span>);

impl<Span> FragmentSpread<Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &FragmentName<Span> {
    self.0.name()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Span>> {
    self.0.directives()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::FragmentSpread::parser_with(Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone)]
pub struct InlineFragment<Span>(lang::InlineFragment<Directives<Span>, SelectionSet<Span>, Span>);

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

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    lang::InlineFragment::parser_with(Directives::parser(), SelectionSet::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Field<Span>(lang::Field<Arguments<Span>, Directives<Span>, SelectionSet<Span>, Span>);

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
  pub const fn selection_set(&self) -> Option<&SelectionSet<Span>> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|field_parser| {
      // Inner fixpoint: build a `Selection<Span>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        // SelectionSet needs a `Selection` parser
        let selection_set =
          lang::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Span>);

        let spread = lang::FragmentSpread::parser_with(Directives::parser())
          .map(|fs| Selection::<Span>::FragmentSpread(FragmentSpread::<Span>(fs)));

        let inline = lang::InlineFragment::parser_with(Directives::parser(), selection_set.clone())
          .map(|f| Selection::<Span>::InlineFragment(InlineFragment::<Span>(f)));

        choice((field_parser.map(Selection::<Span>::Field), spread, inline))
      });

      // Pass the selection parser to the selection set
      let selection_set = lang::SelectionSet::parser_with(selection).map(SelectionSet::<Span>);

      lang::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set).map(Self)
    })
  }
}

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
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
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|selection| {
      let selsetion_set =
        lang::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Span>);

      let field_p = lang::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selsetion_set.clone(),
      )
      .map(Field::<Span>);

      let inline_p = lang::InlineFragment::parser_with(Directives::parser(), selsetion_set.clone())
        .map(InlineFragment::<Span>);

      let spread_p =
        lang::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

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
pub struct SelectionSet<Span>(lang::SelectionSet<Selection<Span>, Span>);

impl<Span> SelectionSet<Span> {
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
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: source::Span<'src, I, E>,
  {
    recursive(|selection_set| {
      let field_p = lang::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selection_set.clone(),
      )
      .map(Field::<Span>);

      let inline_p = lang::InlineFragment::parser_with(Directives::parser(), selection_set)
        .map(InlineFragment::<Span>);

      let spread_p =
        lang::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Span>);

      let selection = choice((
        field_p.map(Selection::<Span>::Field),
        spread_p.map(Selection::<Span>::FragmentSpread),
        inline_p.map(Selection::<Span>::InlineFragment),
      ));

      lang::SelectionSet::parser_with(selection).map(Self)
    })
  }
}
