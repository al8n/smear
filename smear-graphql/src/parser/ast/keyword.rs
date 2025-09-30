use logosky::{
  Lexed, Parseable,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
};
use smear_parser::{
  definitions::ast,
  lang::keywords::{self, *},
};

use crate::error::Error;

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, StrAstTokenStream<'a>, StrAstToken<'a>, StrAstTokenErrors<'a, &'a str>> for $name {
        #[inline]
        fn parser<E>() -> impl Parser<'a, StrAstTokenStream<'a>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, StrAstTokenStream<'a>, Error = StrAstTokenErrors<'a, &'a str>> + 'a,
        {
          any().try_map(|res: Lexed<'_, StrAstToken<'_>>, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                StrAstToken::Identifier(name) => if name.eq($kw) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, StrAstTokenKind::Identifier, span).into()),
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
