use core::marker::PhantomData;

use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::super::{
  char::Char,
  language::{
    ignored::ignored,
    input_value::{DefaultInputValue, StringValue, Variable},
    punct::{Colon, LParen, RParen},
  },
  source::Source,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct VariableDefinition<Type, Directives, Value, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  variable: Variable<Src, Span>,
  colon: Colon<Src, Span>,
  ty: Type,
  directives: Option<Directives>,
  default_value: Option<DefaultInputValue<Value, Src, Span>>,
}

impl<Type, Directives, Value, Src, Span> VariableDefinition<Type, Directives, Value, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn variable(&self) -> &Variable<Src, Span> {
    &self.variable
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Src, Span> {
    &self.colon
  }

  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultInputValue<Value, Src, Span>> {
    self.default_value.as_ref()
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    Variable<Src, Span>,
    Colon<Src, Span>,
    Type,
    Option<Directives>,
    Option<DefaultInputValue<Value, Src, Span>>,
  ) {
    (
      self.span,
      self.description,
      self.variable,
      self.colon,
      self.ty,
      self.directives,
      self.default_value,
    )
  }

  /// Returns a parser for the variable definition.
  ///
  /// Spec: [Variable Definition](https://spec.graphql.org/draft/#sec-Variable-Definition)
  pub fn parser<'src, I, E, TP, DP, VP>(
    type_parser: impl FnOnce() -> TP,
    directives_parser: impl FnOnce() -> DP,
    value_parser: impl FnOnce() -> VP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Variable::parser())
      .then(Colon::parser().padded_by(ignored()))
      .then(type_parser())
      .then_ignore(ignored())
      .then(directives_parser().or_not().then_ignore(ignored()))
      .then(DefaultInputValue::parser_with(value_parser()).or_not())
      .map_with(
        |(((((description, variable), colon), ty), directives), default_value), sp| Self {
          span: Spanned::from(sp),
          variable,
          description,
          colon,
          ty,
          directives,
          default_value,
        },
      )
  }
}

#[derive(Debug, Clone)]
pub struct VariablesDefinition<VariableDefinition, Src, Span, Container = Vec<VariableDefinition>> {
  span: Spanned<Src, Span>,
  l_paren: LParen<Src, Span>,
  r_paren: RParen<Src, Span>,
  variables: Container,
  _v: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Src, Span, Container>
  VariablesDefinition<VariableDefinition, Src, Span, Container>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn l_paren(&self) -> &LParen<Src, Span> {
    &self.l_paren
  }

  #[inline]
  pub const fn r_paren(&self) -> &RParen<Src, Span> {
    &self.r_paren
  }

  #[inline]
  pub const fn variable_definitions(&self) -> &Container {
    &self.variables
  }

  #[inline]
  pub fn into_variable_definitions(self) -> Container {
    self.variables
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    LParen<Src, Span>,
    RParen<Src, Span>,
    Container,
  ) {
    (self.span, self.l_paren, self.r_paren, self.variables)
  }

  pub fn parser_with<'src, I, E, P>(parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    Container: chumsky::container::Container<VariableDefinition>,
    P: Parser<'src, I, VariableDefinition, E> + Clone,
  {
    LParen::parser()
      .then_ignore(ignored())
      .then(parser.then_ignore(ignored()).repeated().collect())
      .then_ignore(ignored())
      .then(RParen::parser())
      .map_with(|((l_paren, variables), r_paren), sp| Self {
        span: Spanned::from(sp),
        l_paren,
        r_paren,
        variables,
        _v: PhantomData,
      })
  }
}
