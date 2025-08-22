use chumsky::{extra::ParserExtra, prelude::*};

use crate::{
  lang::{ignored, punct::Colon, Name, StringValue},
  source::{Char, Slice, Source},
};

/// Represents a GraphQL input value definition.
///
/// Spec: [InputValueDefinition](https://spec.graphql.org/draft/#InputValueDefinition)
#[derive(Debug, Clone)]
pub struct InputValueDefinition<Type, DefaultValue, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  name: Name<Span>,
  colon: Colon<Span>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
}

impl<Type, DefaultValue, Directives, Span>
  InputValueDefinition<Type, DefaultValue, Directives, Span>
{
  /// The description of the input value definition.
  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  /// The name of the input value definition.
  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  /// The colon of the input value definition.
  #[inline]
  pub const fn colon(&self) -> &Colon<Span> {
    &self.colon
  }

  /// The type of the input value definition.
  #[inline]
  pub const fn ty(&self) -> &Type {
    &self.ty
  }

  /// The default value of the input value definition.
  #[inline]
  pub const fn default_value(&self) -> Option<&DefaultValue> {
    self.default_value.as_ref()
  }

  /// The directives of the input value definition.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Consumes the input value definition and returns its components.
  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    Name<Span>,
    Colon<Span>,
    Type,
    Option<DefaultValue>,
    Option<Directives>,
  ) {
    (
      self.span,
      self.description,
      self.name,
      self.colon,
      self.ty,
      self.default_value,
      self.directives,
    )
  }

  /// Returns a parser for the input value definition.
  #[inline]
  pub fn parser_with<'src, I, E, TP, VP, DP>(
    type_parser: TP,
    default_const_value_parser: VP,
    const_directives_parser: DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    TP: Parser<'src, I, Type, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
    VP: Parser<'src, I, DefaultValue, E> + Clone,
  {
    let ws = ignored();

    StringValue::parser()
      .then_ignore(ignored())
      .or_not()
      .then(Name::parser())
      .then(Colon::parser().padded_by(ws.clone()))
      .then(type_parser.padded_by(ignored()))
      .then(default_const_value_parser.padded_by(ignored()).or_not())
      .then(const_directives_parser.or_not())
      .map_with(
        |(((((description, name), colon), ty), default_value), directives), span| Self {
          span: Span::from_map_extra(span),
          description,
          name,
          colon,
          ty,
          default_value,
          directives,
        },
      )
  }
}
