use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use crate::parser::{
  keywords,
  language::{ignored::ignored, input_value::String, punct::LBrace},
  Name, SmearChar, Spanned,
};

#[derive(Debug, Clone)]
pub struct InputObjectDefinition<FieldsDefinition, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<String<Src, Span>>,
  input: keywords::Input<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
  fields: Option<FieldsDefinition>,
}

impl<FieldsDefinition, Directives, Src, Span>
  InputObjectDefinition<FieldsDefinition, Directives, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn description(&self) -> Option<&String<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Src, Span> {
    &self.input
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn fields(&self) -> Option<&FieldsDefinition> {
    self.fields.as_ref()
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    Option<String<Src, Span>>,
    keywords::Input<Src, Span>,
    Name<Src, Span>,
    Option<Directives>,
    Option<FieldsDefinition>,
  ) {
    (
      self.span,
      self.description,
      self.input,
      self.name,
      self.directives,
      self.fields,
    )
  }

  #[inline]
  pub fn parser_with<'src, I, E, IFDP, DP>(
    input_fields_definition_parser: impl FnOnce() -> IFDP,
    directives_parser: impl FnOnce() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    IFDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    String::parser()
      .or_not()
      .then_ignore(ignored())
      .then(keywords::Input::parser().then_ignore(ignored()))
      .then(Name::parser())
      .then(directives_parser().padded_by(ignored()).or_not())
      .then(
        input_fields_definition_parser()
          .padded_by(ignored())
          .or_not(),
      )
      .map_with(
        |((((description, input), name), directives), fields), sp| Self {
          span: Spanned::from(sp),
          description,
          name,
          input,
          directives,
          fields,
        },
      )
      .padded_by(ignored())
  }
}

#[derive(Debug, Clone)]
pub struct InputObjectExtension<FieldsDefinition, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  extend: keywords::Extend<Src, Span>,
  input: keywords::Input<Src, Span>,
  name: Name<Src, Span>,
  directives: Option<Directives>,
  fields: Option<FieldsDefinition>,
}

impl<FieldsDefinition, Directives, Src, Span>
  InputObjectExtension<FieldsDefinition, Directives, Src, Span>
{
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn extend_keyword(&self) -> &keywords::Extend<Src, Span> {
    &self.extend
  }

  #[inline]
  pub const fn input_keyword(&self) -> &keywords::Input<Src, Span> {
    &self.input
  }

  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn fields(&self) -> Option<&FieldsDefinition> {
    self.fields.as_ref()
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Spanned<Src, Span>,
    keywords::Extend<Src, Span>,
    keywords::Input<Src, Span>,
    Name<Src, Span>,
    Option<Directives>,
    Option<FieldsDefinition>,
  ) {
    (
      self.span,
      self.extend,
      self.input,
      self.name,
      self.directives,
      self.fields,
    )
  }

  #[inline]
  pub fn parser_with<'src, I, E, IFDP, DP>(
    input_fields_definition_parser: impl Fn() -> IFDP,
    directives_parser: impl Fn() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    IFDP: Parser<'src, I, FieldsDefinition, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    // extend input Name
    let head = keywords::Extend::parser()
      .then_ignore(ignored())
      .then(keywords::Input::parser())
      .then_ignore(ignored())
      .then(Name::parser())
      .then_ignore(ignored());

    // Alt A: (Directives[Const]?) InputFieldsDefinition
    // If no '{' follows, this alt fails and rewinds.
    let with_fields = directives_parser()
      .padded_by(ignored())
      .or_not()
      .then(input_fields_definition_parser())
      .map(|(directives, fields)| (directives, Some(fields)))
      .rewind();

    // Alt B: Directives[Const]  and lookahead ≠ '{'
    let without_fields = directives_parser()
      .padded_by(ignored())
      .then_ignore(LBrace::parser().rewind().not().ignored())
      .map(|directives| (Some(directives), None));

    head
      .then(choice((with_fields, without_fields)))
      .map_with(|(((extend, input), name), (directives, fields)), sp| Self {
        span: Spanned::from(sp),
        extend,
        input,
        name,
        directives,
        fields,
      })
      .padded_by(ignored())
  }
}
