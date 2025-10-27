use logosky::{
  Lexed, Logos, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, prelude::any},
  utils::Span,
};

use super::{Error, Expectation, SyntacticToken, SyntacticTokenErrors, SyntacticTokenStream};
use crate::lexer::graphqlx::syntactic::SyntacticLexerErrors;

/// A GraphQLx identifier.
///
/// Represents a valid GraphQLx name as defined by the specification. Names are
/// used throughout GraphQLx for field names, type names, argument names, directive
/// names, and other identifiers. They follow strict lexical rules to ensure
/// consistent parsing across different GraphQLx implementations.
///
/// ## Specification Rules
///
/// A GraphQL name must:
/// - Start with a letter (`A-Z`, `a-z`) or underscore (`_`)
/// - Contain only letters, digits (`0-9`), and underscores in subsequent positions
/// - Be at least one character long
/// - Be case-sensitive (`myField` and `MyField` are different names)
///
/// ## Grammar
///
/// ```text
/// Ident ::= [_A-Za-z][_0-9A-Za-z]*
/// ```
///
/// ## Examples
///
/// **Valid names:**
/// ```text
/// user           // Simple lowercase name
/// User           // Capitalized name
/// _private       // Starting with underscore
/// field123       // Contains digits
/// __typename     // GraphQL introspection field
/// MyCustomType   // PascalCase type name
/// _id            // Underscore prefix
/// a              // Single character
/// ```
///
/// **Invalid names:**
/// ```text
/// 123field       // Cannot start with digit
/// my-field       // Hyphens not allowed
/// my.field       // Dots not allowed
/// my field       // Spaces not allowed
/// my@field       // Special characters not allowed
/// ""             // Empty string not allowed
/// ```
///
/// ## Implementation Notes
///
/// This parser only handles the lexical structure of names and does not validate
/// GraphQL-specific naming conventions (e.g., type names should be PascalCase).
/// Such semantic validation should be performed at a higher level.
///
/// Spec: [Name](https://spec.graphql.org/draft/#sec-Names)
pub type Name<S> = crate::ident::Ident<S>;

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Name<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
  {
    any().try_map(|res: Lexed<'_, SyntacticToken<S>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          SyntacticToken::Identifier(name) => Ok(Name::new(span, name)),
          tok => Err(Error::unexpected_token(tok, Expectation::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}
