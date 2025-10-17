use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};

use crate::{
  keywords::{self, *},
  lexer::graphql::syntactic::SyntacticLexerErrors,
};

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a, S> Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> for $name
      where
        SyntacticToken<S>: Token<'a>,
        <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
        <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
        str: logosky::utils::cmp::Equivalent<S>,
      {
        #[inline]
        fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
        {
          use logosky::utils::cmp::Equivalent;

          any().try_map(|res: Lexed<'_, SyntacticToken<S>>, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                SyntacticToken::Identifier(name) => if $kw.equivalent(&name) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, Expectation::Keyword($kw), span).into()),
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
  keywords::QueryLocation:"QUERY",
  keywords::MutationLocation:"MUTATION",
  keywords::SubscriptionLocation:"SUBSCRIPTION",
  keywords::FieldLocation:"FIELD",
  keywords::FragmentDefinitionLocation:"FRAGMENT_DEFINITION",
  keywords::FragmentSpreadLocation:"FRAGMENT_SPREAD",
  keywords::InlineFragmentLocation:"INLINE_FRAGMENT",
  keywords::VariableDefinitionLocation:"VARIABLE_DEFINITION",
  keywords::SchemaLocation:"SCHEMA",
  keywords::ScalarLocation:"SCALAR",
  keywords::ObjectLocation:"OBJECT",
  keywords::FieldDefinitionLocation:"FIELD_DEFINITION",
  keywords::ArgumentDefinitionLocation:"ARGUMENT_DEFINITION",
  keywords::InterfaceLocation:"INTERFACE",
  keywords::UnionLocation:"UNION",
  keywords::EnumLocation:"ENUM",
  keywords::EnumValueLocation:"ENUM_VALUE",
  keywords::InputObjectLocation:"INPUT_OBJECT",
  keywords::InputFieldDefinitionLocation:"INPUT_FIELD_DEFINITION",
}
