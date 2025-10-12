use crate::{
  keywords::On,
  punctuator::{LBrace, RBrace, Spread},
};

use super::{ty::Path, *};
use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{extra::ParserExtra, prelude::*},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

type FragmentSpreadAlias<S> = scaffold::FragmentSpread<FragmentTypePath<S>, Directives<S>>;

#[derive(Debug, Clone, From, Into)]
pub struct FragmentSpread<S>(FragmentSpreadAlias<S>);

impl<S> AsSpan<Span> for FragmentSpread<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for FragmentSpread<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for FragmentSpread<S> {
  type Components = (
    Span,
    Path<S>,
    Option<TypeGenerics<S>>,
    Option<Directives<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, name, directives) = self.0.into_components();
    let (_, path, type_generics) = name.into_components();
    (span, path, type_generics, directives)
  }
}

impl<S> FragmentSpread<S> {
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
  pub const fn type_generics(&self) -> Option<&TypeGenerics<S>> {
    self.0.name().type_generics()
  }

  /// Returns the directives of the fragment spread.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for FragmentSpread<S>
where
  FragmentSpreadAlias<S>: Parseable<'a, I, T, Error>,
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

type InlineFragmentAlias<S> =
  scaffold::InlineFragment<TypeCondition<S>, Directives<S>, SelectionSet<S>>;

#[derive(Debug, Clone, From, Into)]
pub struct InlineFragment<S>(InlineFragmentAlias<S>);

impl<S> AsSpan<Span> for InlineFragment<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for InlineFragment<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for InlineFragment<S> {
  type Components = (
    Span,
    Option<TypeCondition<S>>,
    Option<Directives<S>>,
    SelectionSet<S>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> InlineFragment<S> {
  /// Returns the span of the inline fragment.
  #[inline]
  pub const fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the type condition of the inline fragment, if any.
  #[inline]
  pub const fn type_condition(&self) -> Option<&TypeCondition<S>> {
    self.0.type_condition()
  }

  /// Returns the directives of the inline fragment, if any.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns the selection set of the inline fragment.
  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet<S> {
    self.0.selection_set()
  }

  #[inline]
  fn parser_with<'a, I, T, Error, E>(
    selection_set_parser: impl Parser<'a, I, SelectionSet<S>, E> + Clone,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    TypeCondition<S>: Parseable<'a, I, T, Error>,
    Directives<S>: Parseable<'a, I, T, Error>,
    Spread: Parseable<'a, I, T, Error>,
    On: Parseable<'a, I, T, Error>,
    LBrace: Parseable<'a, I, T, Error>,
    RBrace: Parseable<'a, I, T, Error>,
  {
    InlineFragmentAlias::parser_with(
      TypeCondition::parser(),
      Directives::parser(),
      selection_set_parser,
    )
    .map(Self)
  }
}

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

impl<'a, S: 'a, I, T, Error> Parseable<'a, I, T, Error> for Selection<S>
where
  On: Parseable<'a, I, T, Error>,
  Spread: Parseable<'a, I, T, Error>,
  LBrace: Parseable<'a, I, T, Error>,
  RBrace: Parseable<'a, I, T, Error>,
  TypeCondition<S>: Parseable<'a, I, T, Error>,
  FragmentTypePath<S>: Parseable<'a, I, T, Error>,
  Alias<S>: Parseable<'a, I, T, Error>,
  Ident<S>: Parseable<'a, I, T, Error>,
  Arguments<S>: Parseable<'a, I, T, Error>,
  Directives<S>: Parseable<'a, I, T, Error>,
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
      .map(Field::from);

      let inline_p = InlineFragment::parser_with(selsetion_set.clone()).map(Self::InlineFragment);

      let spread_p = FragmentSpread::parser().map(Self::FragmentSpread);

      choice((field_p.map(Self::Field), spread_p, inline_p))
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
  scaffold::Field<Alias<S>, Ident<S>, Arguments<S>, Directives<S>, SelectionSet<S>>;

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
    Ident<S>,
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
  pub const fn directives(&self) -> Option<&Directives<S>> {
    self.0.directives()
  }

  /// Returns a reference to the selection set of the field, if any.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet<S>> {
    self.0.selection_set()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for Field<S>
where
  On: Parseable<'a, I, T, Error> + 'a,
  Spread: Parseable<'a, I, T, Error> + 'a,
  LBrace: Parseable<'a, I, T, Error> + 'a,
  RBrace: Parseable<'a, I, T, Error> + 'a,
  TypeCondition<S>: Parseable<'a, I, T, Error> + 'a,
  FragmentTypePath<S>: Parseable<'a, I, T, Error> + 'a,
  Alias<S>: Parseable<'a, I, T, Error> + 'a,
  Ident<S>: Parseable<'a, I, T, Error> + 'a,
  Arguments<S>: Parseable<'a, I, T, Error> + 'a,
  Directives<S>: Parseable<'a, I, T, Error> + 'a,
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
    recursive(|field_parser| {
      // Inner fixpoint: build a `Selection<S>` parser by using the recursive `field_parser`.
      let selection = recursive(|selection| {
        // StandardSelectionSet needs a `Selection` parser
        let selection_set = SelectionSet::parser_with(selection.clone());

        let spread = FragmentSpread::parser().map(|fs| Selection::FragmentSpread(fs));

        let inline =
          InlineFragment::parser_with(selection_set.clone()).map(|f| Selection::InlineFragment(f));

        choice((field_parser.map(Selection::Field), spread, inline))
      });

      // Pass the selection parser to the selection set
      let selection_set = SelectionSet::parser_with(selection);

      scaffold::Field::parser_with(Arguments::parser(), Directives::parser(), selection_set)
        .map(Self)
    })
  }
}
