use core::marker::PhantomData;

use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  language::{
    ignored::ignored,
    input_value::{DefaultInputValue, StringValue, Variable},
    punct::{Colon, LParen, RParen},
  },
  source::{Char, Slice, Source},
};

#[derive(Debug, Clone)]
pub struct VariableDefinition<Type, Directives, Value, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  variable: Variable<Span>,
  colon: Colon<Span>,
  ty: Type,
  directives: Option<Directives>,
  default_value: Option<DefaultInputValue<Value, Span>>,
}

impl<Type, Directives, Value, Span> VariableDefinition<Type, Directives, Value, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn variable(&self) -> &Variable<Span> {
    &self.variable
  }

  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
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
  pub const fn default_value(&self) -> Option<&DefaultInputValue<Value, Span>> {
    self.default_value.as_ref()
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    Variable<Span>,
    Colon<Span>,
    Type,
    Option<Directives>,
    Option<DefaultInputValue<Value, Span>>,
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
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    VP: Parser<'src, I, Value, E> + Clone,
    Value: crate::language::input_value::InputValue<true>,
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
          span: Span::from_map_extra(sp),
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
pub struct VariablesDefinition<VariableDefinition, Span, Container = Vec<VariableDefinition>> {
  span: Span,
  l_paren: LParen<Span>,
  r_paren: RParen<Span>,
  variables: Container,
  _v: PhantomData<VariableDefinition>,
}

impl<VariableDefinition, Span, Container> VariablesDefinition<VariableDefinition, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn l_paren(&self) -> &LParen<Span> {
    &self.l_paren
  }

  #[inline]
  pub const fn r_paren(&self) -> &RParen<Span> {
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
  pub fn into_components(self) -> (Span, LParen<Span>, RParen<Span>, Container) {
    (self.span, self.l_paren, self.r_paren, self.variables)
  }

  pub fn parser_with<'src, I, E, P>(parser: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    Container: chumsky::container::Container<VariableDefinition>,
    P: Parser<'src, I, VariableDefinition, E> + Clone,
  {
    LParen::parser()
      .then_ignore(ignored())
      .then(parser.then_ignore(ignored()).repeated().collect())
      .then_ignore(ignored())
      .then(RParen::parser())
      .map_with(|((l_paren, variables), r_paren), sp| Self {
        span: Span::from_map_extra(sp),
        l_paren,
        r_paren,
        variables,
        _v: PhantomData,
      })
  }
}
