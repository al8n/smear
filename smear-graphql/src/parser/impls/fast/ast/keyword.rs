use chumsky::{Parser, extra::ParserExtra, prelude::any};
use logosky::{Lexed, Parseable};
use smear_parser::{
  definitions::minized,
  lang::minized::keywords::{self, *},
};

use crate::error::Error;

use super::*;

macro_rules! keyword_parser {
  ($($name:ty:$kw:literal),+$(,)?) => {
    $(
      impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>> for $name {
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
                FastToken::Identifier(name) => if name.eq($kw) {
                  Ok(<$name>::new(span))
                } else {
                  Err(Error::unexpected_keyword(name, $kw, span).into())
                },
                tok => Err(Error::unexpected_token(tok, FastTokenKind::Identifier, span).into()),
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
  keywords::Input:"input",
  Implements:"implements",
  Extend:"extend",
  keywords::Directive:"directive",
  Schema:"schema",
  Scalar:"scalar",
  Repeatable:"repeatable",
}

keyword_parser! {
  minized::QueryLocation:"QUERY",
  minized::MutationLocation:"MUTATION",
  minized::SubscriptionLocation:"SUBSCRIPTION",
  minized::FieldLocation:"FIELD",
  minized::FragmentDefinitionLocation:"FRAGMENT_DEFINITION",
  minized::FragmentSpreadLocation:"FRAGMENT_SPREAD",
  minized::InlineFragmentLocation:"INLINE_FRAGMENT",
  minized::VariableDefinitionLocation:"VARIABLE_DEFINITION",
  minized::SchemaLocation:"SCHEMA",
  minized::ScalarLocation:"SCALAR",
  minized::ObjectLocation:"OBJECT",
  minized::FieldDefinitionLocation:"FIELD_DEFINITION",
  minized::ArgumentDefinitionLocation:"ARGUMENT_DEFINITION",
  minized::InterfaceLocation:"INTERFACE",
  minized::UnionLocation:"UNION",
  minized::EnumLocation:"ENUM",
  minized::EnumValueLocation:"ENUM_VALUE",
  minized::InputObjectLocation:"INPUT_OBJECT",
  minized::InputFieldDefinitionLocation:"INPUT_FIELD_DEFINITION",
}
