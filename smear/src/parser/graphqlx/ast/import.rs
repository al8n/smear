use crate::{
  parser::{ident::Ident, value::InlineStringValue},
  scaffold::{self, Path},
};

pub type NamedSpecifier<S, Container = Vec<Ident<S>>> =
  scaffold::NamedSpecifier<Ident<S>, Path<Ident<S>, Container>>;
pub type WildcardSpecifier<S, Container = Vec<Ident<S>>> =
  scaffold::WildcardSpecifier<Ident<S>, Container>;
pub type ImportMember<S, Container = Vec<Ident<S>>> =
  scaffold::ImportMember<Ident<S>, Path<Ident<S>, Container>>;
pub type ImportList<
  S,
  PathContainer = Vec<Ident<S>>,
  MemberContainer = Vec<ImportMember<Ident<S>, Vec<Ident<S>>>>,
> = scaffold::ImportList<
  Ident<S>,
  Path<Ident<S>, PathContainer>,
  ImportMember<Ident<S>, MemberContainer>,
>;
pub type ImportDefinition<
  S,
  PathContainer = Vec<Ident<S>>,
  MemberContainer = Vec<ImportMember<Ident<S>, Vec<Ident<S>>>>,
> = scaffold::ImportDefinition<
  Ident<S>,
  InlineStringValue<S>,
  PathContainer,
  ImportMember<Ident<S>, MemberContainer>,
>;
