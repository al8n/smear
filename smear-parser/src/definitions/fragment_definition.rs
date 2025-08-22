use chumsky::{extra::ParserExtra, prelude::*};

use super::super::{
  convert::*,
  lang::{ignored, keywords::Fragment, FragmentName, StringValue, TypeCondition},
  source::{Char, Slice, Source},
};

#[derive(Debug, Clone)]
pub struct FragmentDefinition<Directives, SelectionSet, Span> {
  span: Span,
  description: Option<StringValue<Span>>,
  fragment: Fragment<Span>,
  name: FragmentName<Span>,
  type_condition: TypeCondition<Span>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
}

impl<Directives, SelectionSet, Span> AsRef<Span>
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Directives, SelectionSet, Span> IntoSpan<Span>
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Directives, SelectionSet, Span> IntoComponents
  for FragmentDefinition<Directives, SelectionSet, Span>
{
  type Components = (
    Span,
    Option<StringValue<Span>>,
    Fragment<Span>,
    FragmentName<Span>,
    TypeCondition<Span>,
    Option<Directives>,
    SelectionSet,
  );

  #[inline]
  fn into_components(self) -> Self::Components {
    (
      self.span,
      self.description,
      self.fragment,
      self.name,
      self.type_condition,
      self.directives,
      self.selection_set,
    )
  }
}

impl<Directives, SelectionSet, Span> FragmentDefinition<Directives, SelectionSet, Span> {
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  #[inline]
  pub const fn name(&self) -> &FragmentName<Span> {
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
    StringValue::parser()
      .or_not()
      .then_ignore(ignored())
      .then(Fragment::parser())
      .then_ignore(ignored())
      .then(FragmentName::parser())
      .then_ignore(ignored())
      .then(TypeCondition::parser())
      .then_ignore(ignored())
      .then(directives_parser().or_not())
      .then_ignore(ignored())
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
      .padded_by(ignored())
  }
}
