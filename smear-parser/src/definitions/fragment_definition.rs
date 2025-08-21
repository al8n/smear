use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::super::{
  char::Char,
  keywords::Fragment,
  language::{field::TypeCondition, ignored::ignored, input_value::StringValue},
  name::Name,
  source::Source,
  spanned::Spanned,
};

#[derive(Debug, Clone)]
pub struct FragmentDefinition<SelectionSet, Directives, Src, Span> {
  span: Spanned<Src, Span>,
  description: Option<StringValue<Src, Span>>,
  fragment: Fragment<Src, Span>,
  name: Name<Src, Span>,
  type_condition: TypeCondition<Src, Span>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<SelectionSet, Directives, Src, Span> FragmentDefinition<SelectionSet, Directives, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  #[inline]
  pub const fn description(&self) -> Option<&StringValue<Src, Span>> {
    self.description.as_ref()
  }

  #[inline]
  pub const fn fragment_keyword(&self) -> &Fragment<Src, Span> {
    &self.fragment
  }

  #[inline]
  pub const fn type_condition(&self) -> &TypeCondition<Src, Span> {
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
    Spanned<Src, Span>,
    Option<StringValue<Src, Span>>,
    Name<Src, Span>,
    TypeCondition<Src, Span>,
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
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    SP: Parser<'src, I, SelectionSet, E> + Clone,
    DP: Parser<'src, I, Directives, E> + Clone,
  {
    let ws = ignored();

    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Fragment::parser())
      .then_ignore(ignored())
      .then(Name::<Src, Span>::parser())
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
            span: Spanned::from(sp),
          }
        },
      )
      .padded_by(ws)
  }
}
