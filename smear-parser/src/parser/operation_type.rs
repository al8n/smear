use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{SmearChar, Spanned};

word!(Query: [I::Token::q, I::Token::u, I::Token::e, I::Token::r, I::Token::y]);
word!(Mutation: [I::Token::m, I::Token::u, I::Token::t, I::Token::a, I::Token::t, I::Token::i, I::Token::o, I::Token::n]);
word!(Subscription: [I::Token::s, I::Token::u, I::Token::b, I::Token::s, I::Token::c, I::Token::r, I::Token::i, I::Token::p, I::Token::t, I::Token::i, I::Token::o, I::Token::n]);

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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  {
    choice((
      Query::parser().map(Self::Query),
      Mutation::parser().map(Self::Mutation),
      Subscription::parser().map(Self::Subscription),
    ))
  }
}
