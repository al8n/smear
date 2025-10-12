use super::*;
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

pub(super) type InputFieldsDefinitionAlias<S> = scaffold::generic::Constrained<
  Ident<S>,
  Type<S>,
  scaffold::InputFieldsDefinition<InputValueDefinition<S>>,
>;

#[derive(Debug, Clone, From, Into)]
pub(super) struct InputFieldsDefinition<S>(InputFieldsDefinitionAlias<S>);

impl<S> AsSpan<Span> for InputFieldsDefinition<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for InputFieldsDefinition<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for InputFieldsDefinition<S> {
  type Components = (
    Span,
    Option<WhereClause<S>>,
    scaffold::InputFieldsDefinition<InputValueDefinition<S>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, where_clause, fields) = self.0.into_components();
    (span, where_clause, fields)
  }
}

impl<S> InputFieldsDefinition<S> {
  /// Returns the optional where clause of the input fields definition.
  #[inline]
  pub(super) const fn where_clause(&self) -> Option<&WhereClause<S>> {
    self.0.where_clause()
  }

  /// Returns the input value definitions of the input fields definition.
  #[inline]
  pub(super) const fn fields(&self) -> &scaffold::InputFieldsDefinition<InputValueDefinition<S>> {
    self.0.target()
  }
}

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for InputFieldsDefinition<S>
where
  InputFieldsDefinitionAlias<S>: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    InputFieldsDefinitionAlias::parser::<E>().map(Self)
  }
}
