use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, Spanned, cmp::Equivalent},
};
use smear_lexer::{
  graphql::syntactic::SyntacticLexerErrors,
  punctuator::{At, RBrace},
};
use smear_scaffold::error::UnclosedBraceError;

use super::*;

/// A fragment spread in a selection set.
pub type FragmentSpread<S> = scaffold::FragmentSpread<FragmentName<S>, Directives<S>>;

/// An inline fragment in a selection set.
pub type InlineFragment<S> =
  scaffold::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

/// A selection set containing fields, fragment spreads, and inline fragments.
pub type SelectionSet<S> = scaffold::SelectionSet<Selection<S>>;

/// A selection in a GraphQL selection set.
///
/// A selection can be a field, a fragment spread, or an inline fragment.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S> {
  /// A field selection.
  Field(Field<S>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<S>),
  /// An inline fragment selection.
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

impl<'a, S: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Selection<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|selection| {
      let selection_set = scaffold::SelectionSet::<Self>::parser_with(selection.clone());

      let field_p =
        scaffold::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set)
          .map(Field::from);

      field_p.map(Self::Field).or(fragment_parser(selection))
    })
  }
}

impl<S> Selection<S> {
  /// Returns a reference to the span covering the entire selection.
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

/// A GraphQL field.
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

impl<'a, S: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Field<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|field_parser| {
      // Inner fixpoint: build a `Selection<S>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        field_parser
          .map(Selection::Field)
          .or(fragment_parser(selection))
      });

      // Pass the selection parser to the selection set
      let selection_set = SelectionSet::parser_with(selection);

      scaffold::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set)
        .map(Self)
    })
  }
}

fn fragment_parser<'a, S, E>(
  selection_parser: impl Parser<'a, SyntacticTokenStream<'a, S>, Selection<S>, E> + Clone + 'a,
) -> impl Parser<'a, SyntacticTokenStream<'a, S>, Selection<S>, E> + Clone
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directive<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  At:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
  S: 'a,
  E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticTokenStream<'a, S>: Tokenizer<
      'a,
      SyntacticToken<S>,
      Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
    >,
  SyntacticTokenErrors<'a, S>: 'a,
{
  let selection_set = SelectionSet::parser_with(selection_parser.clone());
  custom(move |inp| {
    let before = inp.cursor();

    match inp.next() {
      None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
      Some(Lexed::Error(errs)) => {
        Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
      }
      Some(Lexed::Token(Spanned { span, data: token })) => {
        match token {
          SyntacticToken::Spread => {
            let current_cursor = inp.cursor();

            match inp.next() {
              None => Err(
                SyntacticTokenError::unexpected_end_of_input(inp.span_since(&current_cursor))
                  .into(),
              ),
              Some(Lexed::Error(errs)) => Err(
                SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&current_cursor))
                  .into(),
              ),
              Some(Lexed::Token(Spanned {
                span: fragment_span,
                data: fragment_token,
              })) => {
                match fragment_token {
                  SyntacticToken::Identifier(name) => {
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
                    let tc =
                      TypeCondition::new((fragment_span.start()..name.span().end()).into(), name);
                    Ok(Selection::InlineFragment(InlineFragment::new(
                      inp.span_since(&before),
                      Some(tc),
                      directives,
                      selection_set,
                    )))
                  }
                  SyntacticToken::LBrace => {
                    let (selection, rbrace) = inp.parse(
                      selection_parser
                        .clone()
                        .repeated()
                        .at_least(1)
                        .collect()
                        .then(RBrace::parser().or_not()),
                    )?;

                    match rbrace {
                      None => {
                        Err(SyntacticTokenError::unclosed_brace(inp.span_since(&before)).into())
                      }
                      Some(_) => {
                        let span = inp.span_since(&before);
                        Ok(Selection::InlineFragment(InlineFragment::new(
                          span,
                          None,
                          None,
                          SelectionSet::new(span, selection),
                        )))
                      }
                    }
                  }
                  SyntacticToken::At => {
                    let (directives, selection_set) = inp.parse(
                      Name::<S>::parser::<E>()
                        .then(Arguments::parser().or_not())
                        .map_with(|(name, args), exa| Directive::new(exa.span(), name, args))
                        .separated_by(At::parser())
                        .collect()
                        .map_with(|directives, exa| Directives::new(exa.span(), directives))
                        .then(selection_set.clone()),
                    )?;
                    Ok(Selection::InlineFragment(InlineFragment::new(
                      inp.span_since(&before),
                      None,
                      Some(directives),
                      selection_set,
                    )))
                  }
                  token => Err(
                    SyntacticTokenError::unexpected_token(
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
          token => {
            Err(SyntacticTokenError::unexpected_token(token, Expectation::Spread, span).into())
          }
        }
      }
    }
  })
}
