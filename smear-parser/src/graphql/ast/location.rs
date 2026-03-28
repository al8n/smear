use smear_lexer::tokit::{
  lexer::FromLogos,
  Emitter, InputRef, Lexer, ParseContext,
  span::Spanned,
  utils::cmp::Equivalent,
};
use smear_lexer::keywords;
use smear_scaffold::ast::{ExecutableDirectiveLocation, Location, TypeSystemDirectiveLocation};

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphql::syntactic::{SyntacticLexer, SyntacticToken};

/// Parses a directive location from the input.
pub fn parse_location<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Location, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::Identifier(name) => {
      let loc = match () {
        () if "QUERY".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::Query(keywords::QueryLocation::new(span)),
        ),
        () if "MUTATION".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::Mutation(keywords::MutationLocation::new(span)),
        ),
        () if "SUBSCRIPTION".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::Subscription(keywords::SubscriptionLocation::new(span)),
        ),
        () if "FIELD_DEFINITION".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::FieldDefinition(keywords::FieldDefinitionLocation::new(span)),
        ),
        () if "FIELD".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::Field(keywords::FieldLocation::new(span)),
        ),
        () if "FRAGMENT_DEFINITION".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::FragmentDefinition(keywords::FragmentDefinitionLocation::new(span)),
        ),
        () if "FRAGMENT_SPREAD".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::FragmentSpread(keywords::FragmentSpreadLocation::new(span)),
        ),
        () if "INLINE_FRAGMENT".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::InlineFragment(keywords::InlineFragmentLocation::new(span)),
        ),
        () if "VARIABLE_DEFINITION".equivalent(&name) => Location::Executable(
          ExecutableDirectiveLocation::VariableDefinition(keywords::VariableDefinitionLocation::new(span)),
        ),
        () if "SCHEMA".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Schema(keywords::SchemaLocation::new(span)),
        ),
        () if "SCALAR".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Scalar(keywords::ScalarLocation::new(span)),
        ),
        () if "OBJECT".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Object(keywords::ObjectLocation::new(span)),
        ),
        () if "ARGUMENT_DEFINITION".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::ArgumentDefinition(keywords::ArgumentDefinitionLocation::new(span)),
        ),
        () if "INTERFACE".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Interface(keywords::InterfaceLocation::new(span)),
        ),
        () if "UNION".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Union(keywords::UnionLocation::new(span)),
        ),
        () if "ENUM_VALUE".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::EnumValue(keywords::EnumValueLocation::new(span)),
        ),
        () if "ENUM".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::Enum(keywords::EnumLocation::new(span)),
        ),
        () if "INPUT_OBJECT".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::InputObject(keywords::InputObjectLocation::new(span)),
        ),
        () if "INPUT_FIELD_DEFINITION".equivalent(&name) => Location::TypeSystem(
          TypeSystemDirectiveLocation::InputFieldDefinition(keywords::InputFieldDefinitionLocation::new(span)),
        ),
        _ => return Err(SyntacticTokenError::unknown_directive_location(name, span).into()),
      };
      Ok(loc)
    }
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::DirectiveLocation, span).into()),
  }
}
