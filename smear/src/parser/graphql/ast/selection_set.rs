use crate::{
  keywords::On,
  punctuator::{LBrace, RBrace, Spread},
  scaffold::{self, FragmentSpread, InlineFragment},
};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoSpan, Span},
};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use super::field::Field;

pub type SelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> =
  scaffold::SelectionSet<
    Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>,
  >;

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> {
  Field(Field<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>),
  FragmentSpread(FragmentSpread<FragmentName, Directives>),
  InlineFragment(
    InlineFragment<
      TypeCondition,
      Directives,
      SelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>,
    >,
  ),
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> AsSpan<Span>
  for Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives> IntoSpan<Span>
  for Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<
  'a,
  Alias: 'a,
  Name: 'a,
  FragmentName: 'a,
  TypeCondition: 'a,
  Arguments: 'a,
  Directives: 'a,
  I,
  T,
  Error,
> Parseable<'a, I, T, Error>
  for Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
where
  On: Parseable<'a, I, T, Error>,
  Spread: Parseable<'a, I, T, Error>,
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  TypeCondition: Parseable<'a, I, T, Error>,
  Alias: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  FragmentName: Parseable<'a, I, T, Error>,
  Arguments: Parseable<'a, I, T, Error>,
  Directives: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>> + 'a,
    T: Token<'a>,
    Error: 'a,
  {
    recursive(|selection| {
      let selsetion_set = scaffold::SelectionSet::<Self>::parser_with(selection.clone());

      let field_p = scaffold::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selsetion_set.clone(),
      )
      .map(Field);

      let inline_p = scaffold::InlineFragment::parser_with(
        TypeCondition::parser(),
        Directives::parser(),
        selsetion_set.clone(),
      )
      .map(Self::InlineFragment);

      let spread_p = FragmentSpread::parser().map(Self::FragmentSpread);

      choice((field_p.map(Self::Field), spread_p, inline_p))
    })
  }
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
  Selection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives>
{
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
    }
  }
}
