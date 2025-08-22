use core::marker::PhantomData;

use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  char::Char,
  language::{
    ignored::ignored,
    input_value::StringValue,
    punct::{Colon, LBrace, RBrace},
  },
  name::Name,
  source::Source,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct FieldDefinition<Args, Type, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  name: Name<Span>,
  arguments_definition: Option<Args>,
  colon: Colon<Span>,
  ty: Type,
  directives: Option<Directives>,
}

impl<Args, Type, Directives, Span> FieldDefinition<Args, Type, Directives, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Args> {
    self.arguments_definition.as_ref()
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
  pub fn into_components(
    self,
  ) -> (
    Option<StringValue<Span>>,
    Name<Span>,
    Option<Args>,
    Colon<Span>,
    Type,
    Option<Directives>,
  ) {
    (
      self.description,
      self.name,
      self.arguments_definition,
      self.colon,
      self.ty,
      self.directives,
    )
  }

  /// Returns a parser for the arguments definition.
  #[inline]
  pub fn parser_with<'src, I, E, AP, TP, DP>(
    args_definition_parser: impl FnOnce() -> AP,
    type_parser: impl FnOnce() -> TP,
    directives_parser: impl FnOnce() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    AP: Parser<'src, I, Args, E> + Clone,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored())
      .then(args_definition_parser().or_not())
      .then(Colon::parser().padded_by(ignored()))
      .then(type_parser())
      .then(directives_parser().or_not())
      .map_with(
        |(((((description, name), arguments_definition), colon), ty), directives), sp| Self {
          span: Spanned::from_map_extra(sp),
          description,
          name,
          arguments_definition,
          colon,
          ty,
          directives,
        },
      )
  }
}

#[derive(Debug, Clone)]
pub struct FieldsDefinition<FieldDefinition, Span, Container = Vec<FieldDefinition>> {
  span: Span,
  l_brace: LBrace<Span>,
  r_brace: RBrace<Span>,
  fields: Container,
  _m: PhantomData<FieldDefinition>,
}

impl<FieldDefinition, Span, Container> FieldsDefinition<FieldDefinition, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Span> {
    &self.l_brace
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Span> {
    &self.r_brace
  }

  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }

  #[inline]
  pub fn into_fields(self) -> Container {
    self.fields
  }

  #[inline]
  pub fn into_components(self) -> (Span, LBrace<Span>, RBrace<Span>, Container) {
    (self.span, self.l_brace, self.r_brace, self.fields)
  }

  pub fn parser_with<'src, I, E, P>(
    field_definition_parser: impl FnOnce() -> P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,

    P: Parser<'src, I, FieldDefinition, E> + Clone,
    Container: chumsky::container::Container<FieldDefinition>,
  {
    LBrace::parser()
      .then_ignore(ignored())
      .then(
        field_definition_parser()
          .padded_by(ignored())
          .repeated()
          .at_least(1)
          .collect(),
      )
      .then_ignore(ignored())
      .then(RBrace::parser())
      .map_with(|((l_brace, fields), r_brace), sp| Self {
        span: Spanned::from_map_extra(sp),
        l_brace,
        r_brace,
        fields,
        _m: PhantomData,
      })
  }
}
