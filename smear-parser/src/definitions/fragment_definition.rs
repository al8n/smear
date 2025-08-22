use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  keywords::Fragment,
  language::{field::TypeCondition, ignored::ignored, input_value::StringValue},
  name::Name,
  source::{Char, Slice, Source},
};

#[derive(Debug, Clone)]
pub struct FragmentDefinition<SelectionSet, Directives, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  fragment: Fragment<Span>,
  name: Name<Span>,
  type_condition: TypeCondition<Span>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<SelectionSet, Directives, Span> FragmentDefinition<SelectionSet, Directives, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn name(&self) -> &Name<Span> {
    &self.name
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn fragment_keyword(&self) -> &Fragment<Span> {
    &self.fragment
  }

  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<Span> {
    &self.type_condition
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn selection_set(&self) -> &SelectionSet {
    &self.selection_set
  }

  #[inline]
  pub fn into_components(
    self,
  ) -> (
    Span,
    Option<StringValue<Span>>,
    Name<Span>,
    TypeCondition<Span>,
    Option<Directives>,
    SelectionSet,
  ) {
    (
      self.span,
      self.description,
      self.name,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }

  /// Returns a parser for the fragment definition.
  pub fn parser_with<'src, I, E, SP, DP>(
    selection_set_parser: impl FnOnce() -> SP,
    directives_parser: impl FnOnce() -> DP,
  ) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,

    SP: Parser<'src, I, SelectionSet, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = ignored();

    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Fragment::parser())
      .then_ignore(ignored())
      .then(Name::<Span>::parser())
      .then_ignore(ignored())
      .then(TypeCondition::parser())
      .then_ignore(ignored())
      .then(directives_parser().or_not())
      .then_ignore(ws.clone())
      .then(selection_set_parser())
      .map_with(
        |(((((description, fragment), name), type_condition), directives), selection_set), sp| {
          Self {
            fragment,
            description,
            name,
            type_condition,
            directives,
            selection_set,
            span: Span::from_map_extra(sp),
          }
        },
      )
      .padded_by(ws)
  }
}
