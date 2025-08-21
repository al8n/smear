use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};
use derive_more::{AsMut, AsRef, From, Into};

use crate::parser::{
  language::{
    field::{self, Alias, TypeCondition},
    punct::{Ellipsis, LBrace, RBrace},
  },
  Char, Spanned,
};

use super::{arguments::Arguments, directives::Directives};

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct FragmentSpread<Src, Span>(field::FragmentSpread<Directives<Src, Span>, Src, Span>);

impl<Src, Span> FragmentSpread<Src, Span> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.0.name()
  }
  pub const fn ellipsis(&self) -> &Ellipsis<Src, Span> {
    self.0.ellipsis()
  }
  pub const fn directives(&self) -> Option<&Directives<Src, Span>> {
    self.0.directives()
  }
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    field::FragmentSpread::parser_with(Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct InlineFragment<Src, Span>(
  field::InlineFragment<SelectionSetValue<Src, Span>, Directives<Src, Span>, Src, Span>,
);

impl<Src, Span> InlineFragment<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Src, Span> {
    self.0.ellipsis()
  }

  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Src, Span>> {
    self.0.type_condition()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Src, Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSetValue<Src, Span> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    field::InlineFragment::parser_with(SelectionSet::parser(), Directives::parser()).map(Self)
  }
}

#[derive(Debug, Clone, From, Into, AsMut, AsRef)]
#[repr(transparent)]
pub struct Field<Src, Span>(
  field::Field<
    Arguments<Src, Span>,
    Directives<Src, Span>,
    SelectionSetValue<Src, Span>,
    Src,
    Span,
  >,
);

impl<Src, Span> Field<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.0.name()
  }

  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Src, Span>> {
    self.0.alias()
  }

  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<Src, Span>> {
    self.0.arguments()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives<Src, Span>> {
    self.0.directives()
  }

  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSetValue<Src, Span>> {
    self.0.selection_set()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
pub enum Selection<Src, Span> {
  Field(Field<Src, Span>),
  FragmentSpread(FragmentSpread<Src, Span>),
  InlineFragment(InlineFragment<Src, Span>),
}

impl<Src, Span> Selection<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Selection::Field(f) => f.span(),
      Selection::FragmentSpread(fs) => fs.span(),
      Selection::InlineFragment(ifr) => ifr.span(),
    }
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    recursive(|selection| {
      let selset =
        field::SelectionSet::parser_with(selection.clone()).map(SelectionSet::<Src, Span>);

      let field_p =
        field::Field::parser_with(Arguments::parser(), Directives::parser(), selset.clone())
          .map(Field::<Src, Span>);

      let inline_p = field::InlineFragment::parser_with(selset.clone(), Directives::parser())
        .map(InlineFragment::<Src, Span>);

      let spread_p =
        field::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Src, Span>);

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
pub struct SelectionSetValue<Src, Span>(field::SelectionSet<Selection<Src, Span>, Src, Span>);

impl<Src, Span> SelectionSetValue<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.0.span()
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    self.0.l_brace()
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    self.0.r_brace()
  }

  #[inline]
  pub const fn selections(&self) -> &[Selection<Src, Span>] {
    self.0.selections().as_slice()
  }

  /// Consumes the selections.
  #[inline]
  pub fn into_selections(self) -> Vec<Selection<Src, Span>> {
    self.0.into_selections()
  }

  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    recursive(|selset| {
      let field_p =
        field::Field::parser_with(Arguments::parser(), Directives::parser(), selset.clone())
          .map(Field::<Src, Span>);

      let inline_p = field::InlineFragment::parser_with(selset.clone(), Directives::parser())
        .map(InlineFragment::<Src, Span>);

      let spread_p =
        field::FragmentSpread::parser_with(Directives::parser()).map(FragmentSpread::<Src, Span>);

      let selection = choice((
        field_p.map(Selection::<Src, Span>::Field),
        spread_p.map(Selection::<Src, Span>::FragmentSpread),
        inline_p.map(Selection::<Src, Span>::InlineFragment),
      ));

      field::SelectionSet::parser_with(selection).map(Self)
    })
  }
}
