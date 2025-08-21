use core::marker::PhantomData;

use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  language::{
    ignored::ignored,
    input_value::StringValue,
    punct::{Colon, LBrace, RBrace},
  },
  Char, Name, Spanned,
};

#[derive(Debug, Clone)]
pub struct FieldDefinition<Args, Type, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  name: Name<Src, Span>,
  arguments_definition: Option<Args>,
  colon: Colon<Src, Span>,
  ty: Type,
  directives: Option<Directives>,
}

impl<Args, Type, Directives, Src, Span> FieldDefinition<Args, Type, Directives, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  #[inline]
  pub const fn arguments_definition(&self) -> Option<&Args> {
    self.arguments_definition.as_ref()
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
  pub fn into_components(
    self,
  ) -> (
    Option<StringValue<Src, Span>>,
    Name<Src, Span>,
    Option<Args>,
    Colon<Src, Span>,
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
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    AP: Parser<'src, I, Args, E> + Clone,
    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    String::parser()
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
          span: Spanned::from(sp),
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
pub struct FieldsDefinition<FieldDefinition, Src, Span, Container = Vec<FieldDefinition>> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  fields: Container,
  _m: PhantomData<FieldDefinition>,
}

impl<FieldDefinition, Src, Span, Container>
  FieldsDefinition<FieldDefinition, Src, Span, Container>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }

  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
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
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    LBrace<Src, Span>,
    RBrace<Src, Span>,
    Container,
  ) {
    (self.span, self.l_brace, self.r_brace, self.fields)
  }

  pub fn parser_with<'src, I, E, P>(
    field_definition_parser: impl FnOnce() -> P,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
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
        span: Spanned::from(sp),
        l_brace,
        r_brace,
        fields,
        _m: PhantomData,
      })
  }
}
