use smear_lexer::tokit::{Emitter, InputRef, Lexer, ParseContext, lexer::FromLogos, span::Spanned};

use super::{Expectation, SyntacticTokenError, SyntacticTokenErrors, next_token};
use crate::lexer::graphqlx::syntactic::{SyntacticLexer, SyntacticToken};

/// A GraphQLx identifier.
///
/// Represents a valid GraphQLx name as defined by the specification.
///
/// ## Grammar
///
/// ```text
/// Ident ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
pub type Name<S> = crate::ident::Ident<S>;

/// Parses a GraphQLx identifier from the input.
pub fn parse_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Name<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>:
    Lexer<'inp, Token = SyntacticToken<S>, Span = smear_lexer::tokit::SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  Lang: ?Sized,
{
  let Spanned { span, data: token } = next_token(input)?;
  match token {
    SyntacticToken::Identifier(name) => Ok(Name::new(span, name)),
    tok => Err(SyntacticTokenError::unexpected_token(tok, Expectation::Identifier, span).into()),
  }
}
