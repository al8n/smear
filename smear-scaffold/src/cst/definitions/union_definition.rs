use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of union type definition: `union Name = Type1 | Type2`
#[derive(Debug, Clone)]
pub struct UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  union_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
  union_member_types: Option<UnionMembers>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, UnionMembers, Directives, S, TriviaContainer>
  UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn union_member_types(&self) -> Option<&UnionMembers> { self.union_member_types.as_ref() }
}

impl<Name, UnionMembers, Directives, S, TriviaContainer> AsSpan<Span>
  for UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, UnionMembers, Directives, S, TriviaContainer> IntoSpan<Span>
  for UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
