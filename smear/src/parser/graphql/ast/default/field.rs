use crate::lexer::graphql::ast::AstLexerErrors;

use super::*;
use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, Spanned, cmp::Equivalent},
};

pub type FragmentSpread<S> = scaffold::FragmentSpread<FragmentName<S>, Directives<S>>;

pub type InlineFragment<S> =
  scaffold::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

pub type SelectionSet<S> = scaffold::SelectionSet<Selection<S>>;

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S> {
  Field(Field<S>),
  FragmentSpread(FragmentSpread<S>),
  InlineFragment(InlineFragment<S>),
}

impl<S> AsSpan<Span> for Selection<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for Selection<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<'a, S: 'a> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for Selection<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  Directives<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
    AstTokenStream<'a, S>: Tokenizer<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstTokenErrors<'a, S>: 'a,
  {
    recursive(|selection| {
      let selection_set = scaffold::SelectionSet::<Self>::parser_with(selection.clone());

      let field_p = scaffold::Field::parser_with(
        Arguments::parser(),
        Directives::parser(),
        selection_set.clone(),
      )
      .map(Field::from);

      // let inline_p = InlineFragment::parser_with(selection_set.clone()).map(Self::InlineFragment);
      // let spread_p = FragmentSpread::parser().map(Self::FragmentSpread);
      // choice((field_p.map(Self::Field), spread_p, inline_p))

      field_p.map(Self::Field).or(fragment_parser(selection_set))
    })
  }
}

impl<S> Selection<S> {
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
    }
  }
}

type FieldAlias<S> =
  scaffold::Field<Alias<S>, Name<S>, Arguments<S>, Directives<S>, SelectionSet<S>>;

#[derive(Debug, Clone, From, Into)]
pub struct Field<S>(FieldAlias<S>);

impl<S> AsSpan<Span> for Field<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for Field<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for Field<S> {
  type Components = (
    Span,
    Option<Alias<S>>,
    Name<S>,
    Option<Arguments<S>>,
    Option<Directives<S>>,
    Option<SelectionSet<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> Field<S> {
  /// Returns a reference to the span covering the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns a reference to the alias of the field, if any.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias<S>> {
    self.0.alias()
  }

  /// Returns a reference to the name of the field.
  #[inline]
  pub const fn name(&self) -> &Name<S> {
    self.0.name()
  }

  /// Returns a reference to the arguments of the field, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<S>> {
    self.0.arguments()
  }

  /// Returns a reference to the directives of the field, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the field, if any.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<S>> {
    self.0.selection_set()
  }
}

impl<'a, S: 'a> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for Field<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  Directives<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
    AstTokenStream<'a, S>: Tokenizer<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstTokenErrors<'a, S>: 'a,
  {
    recursive(|field_parser| {
      // Inner fixpoint: build a `Selection<S>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        // StandardSelectionSet needs a `Selection` parser
        let selection_set = SelectionSet::parser_with(selection.clone());

        // let spread = FragmentSpread::parser().map(|fs| Selection::FragmentSpread(fs));

        // let inline =
        //   InlineFragment::parser_with(selection_set.clone()).map(|f| Selection::InlineFragment(f));
        // choice((field_parser.map(Selection::Field), spread, inline))

        field_parser
          .map(Selection::Field)
          .or(fragment_parser(selection_set))
      });

      // Pass the selection parser to the selection set
      let selection_set = SelectionSet::parser_with(selection);

      scaffold::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set)
        .map(Self)
    })
  }
}

fn fragment_parser<'a, S, E>(
  selection_set: impl Parser<'a, AstTokenStream<'a, S>, SelectionSet<S>, E> + Clone,
) -> impl Parser<'a, AstTokenStream<'a, S>, Selection<S>, E> + Clone
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  Directives<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
  S: 'a,
  E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  AstTokenStream<'a, S>: Tokenizer<
      'a,
      AstToken<S>,
      Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
    >,
  AstTokenErrors<'a, S>: 'a,
{
  custom(move |inp| {
    let before = inp.cursor();

    match inp.next() {
      None => Err(AstTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
      Some(Lexed::Error(errs)) => {
        Err(AstTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
      }
      Some(Lexed::Token(Spanned { span, data: token })) => {
        match token {
          AstToken::Spread => {
            let ckp = inp.save();
            let current_cursor = inp.cursor();

            match inp.next() {
              None => {
                Err(AstTokenError::unexpected_end_of_input(inp.span_since(&current_cursor)).into())
              }
              Some(Lexed::Error(errs)) => {
                Err(AstTokenError::from_lexer_errors(errs, inp.span_since(&current_cursor)).into())
              }
              Some(Lexed::Token(Spanned {
                span: fragment_span,
                data: fragment_token,
              })) => {
                match fragment_token {
                  AstToken::Identifier(name) => {
                    // if we do not have on, then it's a fragment spread
                    if !"on".equivalent(&name) {
                      let directives = inp.parse(Directives::parser().or_not())?;

                      return Ok(Selection::FragmentSpread(FragmentSpread::new(
                        inp.span_since(&before),
                        FragmentName::new(fragment_span, name),
                        directives,
                      )));
                    }

                    // otherwise, it's an inline fragment with type condition
                    let ((name, directives), selection_set) = inp.parse(
                      Name::<S>::parser()
                        .then(Directives::parser().or_not())
                        .then(selection_set.clone()),
                    )?;
                    let tc = TypeCondition::new(fragment_span, name);
                    Ok(Selection::InlineFragment(InlineFragment::new(
                      inp.span_since(&before),
                      Some(tc),
                      directives,
                      selection_set,
                    )))
                  }
                  AstToken::LBrace => {
                    inp.rewind(ckp);
                    let selection_set = inp.parse(selection_set.clone())?;
                    Ok(Selection::InlineFragment(InlineFragment::new(
                      inp.span_since(&before),
                      None,
                      None,
                      selection_set,
                    )))
                  }
                  AstToken::At => {
                    inp.rewind(ckp);
                    let (directives, selection_set) =
                      inp.parse(Directives::parser().then(selection_set.clone()))?;
                    Ok(Selection::InlineFragment(InlineFragment::new(
                      inp.span_since(&before),
                      None,
                      Some(directives),
                      selection_set,
                    )))
                  }
                  token => Err(
                    AstTokenError::unexpected_token(
                      token,
                      Expectation::FragmentSpreadOrInlineFragment,
                      fragment_span,
                    )
                    .into(),
                  ),
                }
              }
            }
          }
          token => Err(AstTokenError::unexpected_token(token, Expectation::Spread, span).into()),
        }
      }
    }
  })
}
