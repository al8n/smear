use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::{lang::v2::keyword::{self, *}, definitions::v2};

use crate::error::Error;

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>> for $name {
        #[inline]
        fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
        where
          Self: Sized,
          E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
        {
          any().try_map(|res, span: Span| match res {
            Lexed::Token(tok) => {
              let (span, tok) = tok.into_components();
              match tok {
                Token::Identifier(name) => if name.eq($kw) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, TokenKind::Identifier, span).into()),
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
  Type:"type",
  Interface:"interface",
  Union:"union",
  Enum:"enum",
  Input:"input",
  Implements:"implements",
  Extend:"extend",
  keyword::Directive:"directive",
  Schema:"schema",
  Scalar:"scalar",
  Repeatable:"repeatable",
}

keyword_parser! {
  v2::QueryLocation:"QUERY",
  v2::MutationLocation:"MUTATION",
  v2::SubscriptionLocation:"SUBSCRIPTION",
  v2::FieldLocation:"FIELD",
  v2::FragmentDefinitionLocation:"FRAGMENT_DEFINITION",
  v2::FragmentSpreadLocation:"FRAGMENT_SPREAD",
  v2::InlineFragmentLocation:"INLINE_FRAGMENT",
  v2::VariableDefinitionLocation:"VARIABLE_DEFINITION",
  v2::SchemaLocation:"SCHEMA",
  v2::ScalarLocation:"SCALAR",
  v2::ObjectLocation:"OBJECT",
  v2::FieldDefinitionLocation:"FIELD_DEFINITION",
  v2::ArgumentDefinitionLocation:"ARGUMENT_DEFINITION",
  v2::InterfaceLocation:"INTERFACE",
  v2::UnionLocation:"UNION",
  v2::EnumLocation:"ENUM",
  v2::EnumValueLocation:"ENUM_VALUE",
  v2::InputObjectLocation:"INPUT_OBJECT",
  v2::InputFieldDefinitionLocation:"INPUT_FIELD_DEFINITION",
}
