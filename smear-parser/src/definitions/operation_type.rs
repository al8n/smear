use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  lang::keywords::{Mutation, Query, Subscription},
  source::{Char, Slice, Source},
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
pub enum OperationType<Span> {
  /// `query`
  Query(Query<Span>),
  /// `mutation`
  Mutation(Mutation<Span>),
  /// `subscription`
  Subscription(Subscription<Span>),
}

impl<Span> OperationType<Span> {
  /// Returns the span of the operation type.
  #[inline]
  pub const fn span(&self) -> &Span {
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
  {
    choice((
      Query::parser().map(Self::Query),
      Mutation::parser().map(Self::Mutation),
      Subscription::parser().map(Self::Subscription),
    ))
  }
}
