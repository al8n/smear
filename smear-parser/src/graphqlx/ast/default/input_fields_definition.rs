use super::*;
use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::extra::ParserExtra,
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};

pub(super) type InputFieldsDefinitionAlias<S, Ty = Type<S>> = scaffold::generic::Constrained<
  Ident<S>,
  Ty,
  scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>,
>;

#[derive(Debug, Clone, From, Into)]
pub(super) struct InputFieldsDefinition<S, Ty = Type<S>>(InputFieldsDefinitionAlias<S, Ty>);

impl<S, Ty> AsSpan<Span> for InputFieldsDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S, Ty> IntoSpan<Span> for InputFieldsDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S, Ty> IntoComponents for InputFieldsDefinition<S, Ty> {
  type Components = (
    Span,
    Option<WhereClause<S, Ty>>,
    scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>>,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    let (span, where_clause, fields) = self.0.into_components();
    (span, where_clause, fields)
  }
}

impl<S, Ty> InputFieldsDefinition<S, Ty> {
  /// Returns the optional where clause of the input fields definition.
  #[inline]
  pub(super) const fn where_clause(&self) -> Option<&WhereClause<S, Ty>> {
    self.0.where_clause()
  }

  /// Returns the input value definitions of the input fields definition.
  #[inline]
  pub(super) const fn fields(
    &self,
  ) -> &scaffold::InputFieldsDefinition<InputValueDefinition<S, Ty>> {
    self.0.target()
  }
}

impl<'a, S, Ty, I, T, Error> Parseable<'a, I, T, Error> for InputFieldsDefinition<S, Ty>
where
  InputFieldsDefinitionAlias<S, Ty>: Parseable<'a, I, T, Error>,
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
