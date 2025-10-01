use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{Span, cmp::Equivalent},
};
use smear_parser::definitions::ast::{
  self, ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation,
};

use crate::{error::Error, lexer::ast::AstLexerErrors};

use super::*;

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> for Location
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, AstToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          AstToken::Identifier(name) => Ok({
            match () {
              () if "QUERY".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Query(ast::QueryLocation::new(span)),
              ),
              () if "MUTATION".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Mutation(ast::MutationLocation::new(span)),
              ),
              () if "SUBSCRIPTION".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Subscription(ast::SubscriptionLocation::new(span)),
              ),
              () if "FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::FieldDefinition(
                  ast::FieldDefinitionLocation::new(span),
                ))
              }
              () if "FIELD".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Field(ast::FieldLocation::new(span)),
              ),
              () if "FRAGMENT_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::FragmentDefinition(
                  ast::FragmentDefinitionLocation::new(span),
                ))
              }
              () if "FRAGMENT_SPREAD".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::FragmentSpread(ast::FragmentSpreadLocation::new(span)),
              ),
              () if "INLINE_FRAGMENT".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::InlineFragment(ast::InlineFragmentLocation::new(span)),
              ),
              () if "VARIABLE_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::VariableDefinition(
                  ast::VariableDefinitionLocation::new(span),
                ))
              }
              () if "SCHEMA".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Schema(ast::SchemaLocation::new(span)),
              ),
              () if "SCALAR".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Scalar(ast::ScalarLocation::new(span)),
              ),
              () if "OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Object(ast::ObjectLocation::new(span)),
              ),
              () if "ARGUMENT_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::ArgumentDefinition(
                  ast::ArgumentDefinitionLocation::new(span),
                ))
              }
              () if "INTERFACE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Interface(ast::InterfaceLocation::new(span)),
              ),
              () if "UNION".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Union(ast::UnionLocation::new(span)),
              ),
              () if "ENUM_VALUE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::EnumValue(ast::EnumValueLocation::new(span)),
              ),
              () if "ENUM".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Enum(ast::EnumLocation::new(span)),
              ),
              () if "INPUT_OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::InputObject(ast::InputObjectLocation::new(span)),
              ),
              () if "INPUT_FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::InputFieldDefinition(
                  ast::InputFieldDefinitionLocation::new(span),
                ))
              }
              _ => return Err(Error::unknown_directive_location(name, span).into()),
            }
          }),
          tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
