use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{
    convert::*,
    source::{Char, Slice, Source},
  },
  ignored::ignored,
  punct::{LBrace, RBrace},
};

use core::marker::PhantomData;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct SelectionSet<Selection, Span, Container = Vec<Selection>> {
  span: Span,
  l_brace: LBrace<Span>,
  selections: Container,
  r_brace: RBrace<Span>,
  _marker: PhantomData<Selection>,
}

impl<Selection, Span, Container> AsRef<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Selection, Span, Container> IntoSpan<Span> for SelectionSet<Selection, Span, Container> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Selection, Span, Container> IntoComponents for SelectionSet<Selection, Span, Container> {
  type Components = (Span, LBrace<Span>, Container, RBrace<Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.selections, self.r_brace)
  }
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
    Span: crate::source::Span<'src, I, E>,
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
        span: Span::from_map_extra(sp),
        l_brace,
        selections,
        r_brace,
        _marker: PhantomData,
      })
  }
}
