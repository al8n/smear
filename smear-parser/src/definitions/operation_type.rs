use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  char::Char,
  keywords::{Mutation, Query, Subscription},
  source::Source,
  spanned::Spanned,
};

#[derive(
  Debug,
  Clone,
  derive_more::From,
  derive_more::IsVariant,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum OperationType<Src, Span> {
  /// `query`
  Query(Query<Src, Span>),
  /// `mutation`
  Mutation(Mutation<Src, Span>),
  /// `subscription`
  Subscription(Subscription<Src, Span>),
}

impl<Src, Span> OperationType<Src, Span> {
  /// Returns the span of the operation type.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    match self {
      Self::Query(q) => q.span(),
      Self::Mutation(m) => m.span(),
      Self::Subscription(s) => s.span(),
    }
  }

  /// Returns a parser for Operation Type
  #[inline]
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
  {
    choice((
      Query::parser().map(Self::Query),
      Mutation::parser().map(Self::Mutation),
      Subscription::parser().map(Self::Subscription),
    ))
  }
}
