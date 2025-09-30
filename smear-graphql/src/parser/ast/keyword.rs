use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};
use smear_parser::{
  definitions::ast,
  lang::keywords::{self, *},
};

use crate::{error::Error, lexer::ast::AstLexerErrors};

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>> for $name
      where
        AstToken<S>: Token<'a>,
        <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
        <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
        str: logosky::utils::cmp::Equivalent<S>,
      {
        #[inline]
        fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
        {
          use logosky::utils::cmp::Equivalent;

          any().try_map(|res: Lexed<'_, AstToken<S>>, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                AstToken::Identifier(name) => if $kw.equivalent(&name) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
              }
            },
            Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
          })
        }
      }
    )*
  };
}

keyword_parser! {
  On:"on",
  Fragment:"fragment",
  Query:"query",
  Mutation:"mutation",
  Subscription:"subscription",
  keywords::Type:"type",
  Interface:"interface",
  Union:"union",
  Enum:"enum",
  keywords::Input:"input",
  Implements:"implements",
  Extend:"extend",
  keywords::Directive:"directive",
  Schema:"schema",
  Scalar:"scalar",
  Repeatable:"repeatable",
}

keyword_parser! {
  ast::QueryLocation:"QUERY",
  ast::MutationLocation:"MUTATION",
  ast::SubscriptionLocation:"SUBSCRIPTION",
  ast::FieldLocation:"FIELD",
  ast::FragmentDefinitionLocation:"FRAGMENT_DEFINITION",
  ast::FragmentSpreadLocation:"FRAGMENT_SPREAD",
  ast::InlineFragmentLocation:"INLINE_FRAGMENT",
  ast::VariableDefinitionLocation:"VARIABLE_DEFINITION",
  ast::SchemaLocation:"SCHEMA",
  ast::ScalarLocation:"SCALAR",
  ast::ObjectLocation:"OBJECT",
  ast::FieldDefinitionLocation:"FIELD_DEFINITION",
  ast::ArgumentDefinitionLocation:"ARGUMENT_DEFINITION",
  ast::InterfaceLocation:"INTERFACE",
  ast::UnionLocation:"UNION",
  ast::EnumLocation:"ENUM",
  ast::EnumValueLocation:"ENUM_VALUE",
  ast::InputObjectLocation:"INPUT_OBJECT",
  ast::InputFieldDefinitionLocation:"INPUT_FIELD_DEFINITION",
}
