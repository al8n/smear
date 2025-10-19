use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, Spanned, cmp::Equivalent},
};
use smear_lexer::{
  graphqlx::syntactic::SyntacticLexerErrors,
  punctuator::{At, RBrace},
};

use super::{ty::Path, *};

type FragmentSpreadAlias<S, Ty = Type<S>> =
  scaffold::FragmentSpread<FragmentTypePath<S, Ty>, Directives<S, Ty>>;

/// A fragment spread in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct FragmentSpread<S, Ty = Type<S>>(FragmentSpreadAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for FragmentSpread<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for FragmentSpread<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for FragmentSpread<S, Ty> {
  type Components = (
    Span,
    Path<S>,
    Option<scaffold::generic::TypeGenerics<Ty>>,
    Option<Directives<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives) = self.0.into_components();
    let (_, path, type_generics) = name.into_components();
    (span, path, type_generics, directives)
  }
}

impl<S, Ty> FragmentSpread<S, Ty> {
  #[inline]
  pub(super) const fn new(
    span: Span,
    name: FragmentTypePath<S, Ty>,
    directives: Option<Directives<S, Ty>>,
  ) -> Self {
    Self(FragmentSpreadAlias::new(span, name, directives))
  }

  /// Returns the span of the fragment spread.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the path of the fragment spread.
  #[inline]
  pub const fn path(&self) -> &Path<S> {
    self.0.name().path()
  }

  /// Returns the type generics of the fragment spread name, if any.
  #[inline]
  pub const fn type_generics(&self) -> Option<&scaffold::generic::TypeGenerics<Ty>> {
    self.0.name().type_generics()
  }

  /// Returns the directives of the fragment spread.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for FragmentSpread<S, Ty>
where
  FragmentSpreadAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    FragmentSpreadAlias::parser::<E>().map(Self)
  }
}

type InlineFragmentAlias<S, Ty = Type<S>> =
  scaffold::InlineFragment<TypeCondition<S, Ty>, Directives<S, Ty>, SelectionSet<S, Ty>>;

/// An inline fragment in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct InlineFragment<S, Ty = Type<S>>(InlineFragmentAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InlineFragment<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InlineFragment<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InlineFragment<S, Ty> {
  type Components = (
    Span,
    Option<TypeCondition<S, Ty>>,
    Option<Directives<S, Ty>>,
    SelectionSet<S, Ty>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> InlineFragment<S, Ty> {
  #[inline]
  pub(super) const fn new(
    span: Span,
    type_condition: Option<TypeCondition<S, Ty>>,
    directives: Option<Directives<S, Ty>>,
    selection_set: SelectionSet<S, Ty>,
  ) -> Self {
    Self(InlineFragmentAlias::new(
      span,
      type_condition,
      directives,
      selection_set,
    ))
  }

  /// Returns the span of the inline fragment.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the type condition of the inline fragment, if any.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<S, Ty>> {
    self.0.type_condition()
  }

  /// Returns the directives of the inline fragment, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns the selection set of the inline fragment.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S, Ty> {
    self.0.selection_set()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InlineFragment<S, Ty>
where
  InlineFragmentAlias<S, Ty>: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InlineFragmentAlias::parser::<E>().map(Self)
  }
}

/// A selection set containing fields, fragment spreads, and inline fragments.
pub type SelectionSet<S, Ty = Type<S>> = scaffold::SelectionSet<Selection<S, Ty>>;

/// A selection in a GraphQLx selection set.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Selection<S, Ty = Type<S>> {
  /// A field selection.
  Field(Field<S, Ty>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<S, Ty>),
  /// An inline fragment selection.
  InlineFragment(InlineFragment<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for Selection<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for Selection<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Field(f) => f.into_span(),
      Self::FragmentSpread(fs) => fs.into_span(),
      Self::InlineFragment(ifr) => ifr.into_span(),
    }
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Selection<S, Ty>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S, Ty>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Ty:
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

impl<S, Ty> Selection<S, Ty> {
  /// Returns the span of the selection.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Field(f) => f.span(),
      Self::FragmentSpread(fs) => fs.span(),
      Self::InlineFragment(ifr) => ifr.span(),
    }
  }
}

type FieldAlias<S, Ty = Type<S>> =
  scaffold::Field<Alias<S>, Ident<S>, Arguments<S>, Directives<S, Ty>, SelectionSet<S, Ty>>;

/// A field in a GraphQLx selection set.
#[derive(Debug, Clone, From, Into)]
pub struct Field<S, Ty = Type<S>>(FieldAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for Field<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for Field<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for Field<S, Ty> {
  type Components = (
    Span,
    Option<Alias<S>>,
    Ident<S>,
    Option<Arguments<S>>,
    Option<Directives<S, Ty>>,
    Option<SelectionSet<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S, Ty> Field<S, Ty> {
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
  pub const fn name(&self) -> &Ident<S> {
    self.0.name()
  }

  /// Returns a reference to the arguments of the field, if any.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments<S>> {
    self.0.arguments()
  }

  /// Returns a reference to the directives of the field, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S, Ty>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the field, if any.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<S, Ty>> {
    self.0.selection_set()
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Field<S, Ty>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S, Ty>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Ty:
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

fn fragment_parser<'a, S, Ty, E>(
  selection_parser: impl Parser<'a, SyntacticTokenStream<'a, S>, Selection<S, Ty>, E> + Clone + 'a,
) -> impl Parser<'a, SyntacticTokenStream<'a, S>, Selection<S, Ty>, E> + Clone
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directive<S, Ty>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S, Ty>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  At:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  str: Equivalent<S>,
  S: 'a,
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
            let cp = inp.save();
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
                      inp.rewind(cp);
                      let (p, directives) = inp
                        .parse(FragmentTypePath::parser().then(Directives::parser().or_not()))?;

                      return Ok(Selection::FragmentSpread(FragmentSpread::new(
                        inp.span_since(&before),
                        p,
                        directives,
                      )));
                    }

                    // otherwise, it's an inline fragment with type condition
                    let ((name, directives), selection_set) = inp.parse(
                      TypePath::<S, Ty>::parser()
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
                  // Fragment spread
                  SyntacticToken::PathSeparator => {
                    let (mut tp, directives) =
                      inp.parse(TypePath::<S, Ty>::parser().then(Directives::parser().or_not()))?;
                    tp.path_mut().set_fqdp(true);

                    let (tp_span, path, generics) = tp.into_components();
                    Ok(Selection::FragmentSpread(FragmentSpread::new(
                      inp.span_since(&before),
                      FragmentTypePath::new(
                        (fragment_span.start()..tp_span.end()).into(),
                        path,
                        generics,
                      ),
                      directives,
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
                      TypePath::<S, Ty>::parser::<E>()
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
