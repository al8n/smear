use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};

use crate::{
  keywords::{self, *},
  lexer::graphql::ast::AstLexerErrors,
  scaffold,
};

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
  scaffold::QueryLocation:"QUERY",
  scaffold::MutationLocation:"MUTATION",
  scaffold::SubscriptionLocation:"SUBSCRIPTION",
  scaffold::FieldLocation:"FIELD",
  scaffold::FragmentDefinitionLocation:"FRAGMENT_DEFINITION",
  scaffold::FragmentSpreadLocation:"FRAGMENT_SPREAD",
  scaffold::InlineFragmentLocation:"INLINE_FRAGMENT",
  scaffold::VariableDefinitionLocation:"VARIABLE_DEFINITION",
  scaffold::SchemaLocation:"SCHEMA",
  scaffold::ScalarLocation:"SCALAR",
  scaffold::ObjectLocation:"OBJECT",
  scaffold::FieldDefinitionLocation:"FIELD_DEFINITION",
  scaffold::ArgumentDefinitionLocation:"ARGUMENT_DEFINITION",
  scaffold::InterfaceLocation:"INTERFACE",
  scaffold::UnionLocation:"UNION",
  scaffold::EnumLocation:"ENUM",
  scaffold::EnumValueLocation:"ENUM_VALUE",
  scaffold::InputObjectLocation:"INPUT_OBJECT",
  scaffold::InputFieldDefinitionLocation:"INPUT_FIELD_DEFINITION",
}
