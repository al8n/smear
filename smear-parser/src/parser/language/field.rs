use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords,
  language::{
    ignored::{bom, comma, comment, line_terminator, white_space},
    punct::{Ellipsis, LBrace, RBrace},
  },
  Char, Name, Spanned,
};

use core::marker::PhantomData;
use std::vec::Vec;

// /// Helper: parse '...' as either a dedicated ELLIPSIS token or three DOTs.
// fn ellipsis<'src, I, E>() -> impl Parser<'src, I, Ellipsis<Spanned<I::Slice, I::Span>>, E> + Clone
// where
//   I: StrInput<'src>,
//   I::Token: Char + 'src,
//   E: ParserExtra<'src, I>,
//   E::Error:
//     LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
// {
//   just([I::Token::DOT, I::Token::DOT, I::Token::DOT])
//     .map_with(|_, sp| Ellipsis::new(Spanned::from(sp)))
// }

// WHITESPACE+ per your pest rule (space/tabs; tweak if you allow more)
fn ws_plus<'src, I, E>() -> impl Parser<'src, I, (), E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  E: ParserExtra<'src, I>,
  E::Error: LabelError<'src, I, TextExpected<'src, I>>,
{
  choice((bom(), white_space(), line_terminator(), comment(), comma()))
    .repeated()
    .at_least(1)
    .ignored()
}

#[derive(Debug, Clone)]
pub struct TypeCondition<Src, Span> {
  span: Spanned<Src, Span>,
  on: keywords::On<Src, Span>,
  type_name: Name<Src, Span>, // NamedType in spec; Name node is fine here
}

impl<Src, Span> TypeCondition<Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn on_keyword(&self) -> &keywords::On<Src, Span> {
    &self.on
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.type_name.span()
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
    let ws = super::ignored::ignored();

    keywords::On::parser()
      .then_ignore(ws_plus::<I, E>())
      .then(Name::parser())
      .map_with(|(on, type_name), sp| Self {
        span: Spanned::from(sp),
        on,
        type_name,
      })
      .then_ignore(ws)
  }
}

#[derive(Debug, Clone)]
pub struct FragmentSpread<Directives, Src, Span> {
  span: Spanned<Src, Span>,
  ellipsis: Ellipsis<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
}

impl<Directives, Src, Span> FragmentSpread<Directives, Src, Span> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }
  pub const fn ellipsis(&self) -> &Ellipsis<Src, Span> {
    &self.ellipsis
  }
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }
  pub fn parser_with<'src, I, E, P>(directives: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = super::ignored::ignored();

    // `!type_condition`   i.e., not: ws* "on" WS+ ...
    let not_type_condition = ws.clone()
      .ignore_then(just([I::Token::o, I::Token::n]))
      .then_ignore(ws_plus::<I, E>())
      .rewind()                  // don't consume if it matches
      .not()
      .ignored();

    Ellipsis::parser()
      .then_ignore(ws.clone())
      .then_ignore(not_type_condition)
      .then(Name::<Src, Span>::parser())
      .then_ignore(ws.clone())
      .then(directives.or_not())
      .map_with(|((ellipsis_sp, name), directives), sp| Self {
        span: Spanned::from(sp),
        ellipsis: ellipsis_sp,
        name,
        directives,
      })
      .then_ignore(ws)
  }
}

#[derive(Debug, Clone)]
pub struct InlineFragment<SelectionSet, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  ellipsis: Ellipsis<Src, Span>,
  type_condition: Option<TypeCondition<Src, Span>>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<SelectionSet, Directives, Src, Span> InlineFragment<SelectionSet, Directives, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn ellipsis(&self) -> &Ellipsis<Src, Span> {
    &self.ellipsis
  }

  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<Src, Span>> {
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

  pub fn parser_with<'src, I, E, PS, P>(
    selection_set: PS,
    directives: P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    PS: Parser<'src, I, SelectionSet, E> + Clone,
    P: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = super::ignored::ignored();

    Ellipsis::parser()
      .then_ignore(ws.clone())
      .then(TypeCondition::<Src, Span>::parser().or_not())
      .then_ignore(ws.clone())
      .then(directives.or_not())
      .then_ignore(ws.clone())
      .then(selection_set)
      .map_with(
        |(((ell, type_condition), directives), selection_set), sp| Self {
          span: Spanned::from(sp),
          ellipsis: ell,
          type_condition,
          directives,
          selection_set,
        },
      )
      .then_ignore(ws)
  }
}

#[derive(Debug, Clone)]
pub struct Alias<Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Spanned<Src, Span>,
}

impl<Src, Span> Alias<Src, Span> {
  /// Returns the span of the alias.
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the name of the alias.
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// Returns the colon of the alias.
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }

  /// Returns a parser of the alias.
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
    let ws = super::ignored::ignored();

    Name::<Src, Span>::parser()
      .then(
        just(I::Token::COLON)
          .map_with(|_, sp| Spanned::from(sp))
          .padded_by(ws.clone()),
      )
      .map_with(|(name, colon), sp| Self {
        span: Spanned::from(sp),
        name,
        colon,
      })
      .then_ignore(ws) // trailing ignored
  }
}

#[derive(Debug, Clone)]
pub struct SelectionSet<S, Src, Span, Container = Vec<S>> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  selections: Container,
  r_brace: RBrace<Src, Span>,
  _marker: PhantomData<S>,
}

impl<S, Src, Span, Container> SelectionSet<S, Src, Span, Container> {
  /// Returns the span of the selection set.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left brace of the selection set.
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
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
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }
}

impl<S, Src, Span, Container> SelectionSet<S, Src, Span, Container>
where
  Container: chumsky::container::Container<S>,
{
  /// Returns a parser of the selection set.
  pub fn parser_with<'src, I, E, PS>(selection: PS) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    PS: Parser<'src, I, S, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let open = LBrace::parser();
    let close = RBrace::parser();

    open
      .then_ignore(ws.clone())
      .then(
        selection
          .repeated()
          .at_least(1)
          .collect()
          .then(close.clone()),
      )
      .map_with(|(l_brace, (selections, r_brace)), sp| Self {
        span: Spanned::from(sp),
        l_brace,
        selections,
        r_brace,
        _marker: PhantomData,
      })
  }
}

#[derive(Debug, Clone)]
pub struct Field<Args, Directives, SelectionSet, Src, Span> {
  span: Spanned<Src, Span>,
  alias: Option<Alias<Src, Span>>,
  name: Name<Src, Span>,
  arguments: Option<Args>,
  directives: Option<Directives>,
  selection_set: Option<SelectionSet>,
}

impl<Args, Directives, SelectionSet, Src, Span> Field<Args, Directives, SelectionSet, Src, Span> {
  /// Returns the span of the field
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the alias of the field
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<Src, Span>> {
    self.alias.as_ref()
  }

  /// Returns the span of the field name
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    AP: Parser<'src, I, Args, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
  {
    let ws = super::ignored::ignored();

    Alias::<Src, Span>::parser()
      .or_not()
      .then_ignore(ws.clone())
      .then(Name::<Src, Span>::parser())
      .then_ignore(ws.clone())
      .then(args.or_not())
      .then_ignore(ws.clone())
      .then(directives.or_not())
      .then_ignore(ws.clone())
      .then(selection_set.or_not())
      .map_with(
        |((((alias, name), arguments), directives), selection_set), sp| Self {
          span: Spanned::from(sp),
          alias,
          name,
          arguments,
          directives,
          selection_set,
        },
      )
      .then_ignore(ws)
  }
}
