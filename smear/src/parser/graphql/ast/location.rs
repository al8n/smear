use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  utils::{Span, cmp::Equivalent},
};

use crate::{
  keywords,
  lexer::graphql::ast::AstLexerErrors,
  scaffold::{ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation},
};

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
                ExecutableDirectiveLocation::Query(keywords::QueryLocation::new(span)),
              ),
              () if "MUTATION".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Mutation(keywords::MutationLocation::new(span)),
              ),
              () if "SUBSCRIPTION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::Subscription(
                  keywords::SubscriptionLocation::new(span),
                ))
              }
              () if "FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::FieldDefinition(
                  keywords::FieldDefinitionLocation::new(span),
                ))
              }
              () if "FIELD".equivalent(&name) => Self::Executable(
                ExecutableDirectiveLocation::Field(keywords::FieldLocation::new(span)),
              ),
              () if "FRAGMENT_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::FragmentDefinition(
                  keywords::FragmentDefinitionLocation::new(span),
                ))
              }
              () if "FRAGMENT_SPREAD".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::FragmentSpread(
                  keywords::FragmentSpreadLocation::new(span),
                ))
              }
              () if "INLINE_FRAGMENT".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::InlineFragment(
                  keywords::InlineFragmentLocation::new(span),
                ))
              }
              () if "VARIABLE_DEFINITION".equivalent(&name) => {
                Self::Executable(ExecutableDirectiveLocation::VariableDefinition(
                  keywords::VariableDefinitionLocation::new(span),
                ))
              }
              () if "SCHEMA".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Schema(keywords::SchemaLocation::new(span)),
              ),
              () if "SCALAR".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Scalar(keywords::ScalarLocation::new(span)),
              ),
              () if "OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Object(keywords::ObjectLocation::new(span)),
              ),
              () if "ARGUMENT_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::ArgumentDefinition(
                  keywords::ArgumentDefinitionLocation::new(span),
                ))
              }
              () if "INTERFACE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Interface(keywords::InterfaceLocation::new(span)),
              ),
              () if "UNION".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Union(keywords::UnionLocation::new(span)),
              ),
              () if "ENUM_VALUE".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::EnumValue(keywords::EnumValueLocation::new(span)),
              ),
              () if "ENUM".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::Enum(keywords::EnumLocation::new(span)),
              ),
              () if "INPUT_OBJECT".equivalent(&name) => Self::TypeSystem(
                TypeSystemDirectiveLocation::InputObject(keywords::InputObjectLocation::new(span)),
              ),
              () if "INPUT_FIELD_DEFINITION".equivalent(&name) => {
                Self::TypeSystem(TypeSystemDirectiveLocation::InputFieldDefinition(
                  keywords::InputFieldDefinitionLocation::new(span),
                ))
              }
              _ => return Err(Error::unknown_directive_location(name, span).into()),
            }
          }),
          tok => Err(Error::unexpected_token(tok, Expectation::DirectiveLocation, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
