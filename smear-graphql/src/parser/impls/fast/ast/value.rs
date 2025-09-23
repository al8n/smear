use crate::error::Error;

use chumsky::{Parser, extra::ParserExtra, prelude::*, select};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logos::Logos;
use logosky::{Lexed, Parseable, Source, Token, Tokenizer, utils::Spanned};
use smear_parser::lang::{minized::{List, Name, Object, ObjectField}, punctuator::{RBracket, RBrace}};

use super::{FastToken, FastTokenErrors, FastTokenStream};

use crate::lexer::token::fast::TokenKind;

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod list;
mod null_value;
mod object;
mod string;

pub type Variable<S> = smear_parser::lang::minized::Variable<Name<S>>;

pub type DefaultInputValue<S> = smear_parser::lang::minized::DefaultInputValue<ConstInputValue<S>>;

macro_rules! int_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: FastToken::Int(value) }) => Self::Int(IntValue::new(span, value)),
    }
  }};
}

macro_rules! float_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: FastToken::Float(value) }) => Self::Float(FloatValue::new(span, value)),
    }
  }};
}

macro_rules! string_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: FastToken::StringLiteral(raw) }) => Self::String(StringValue::inline(span, raw, raw.trim_matches('"'))),
      Lexed::Token(Spanned { span, data: FastToken::BlockStringLiteral(raw) }) => Self::String(StringValue::block(span, raw, raw.trim_matches('"'))),
    }
  }};
}

macro_rules! enum_boolean_null_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: FastToken::Identifier(name) }) => {
        match name {
          "true" => Self::Boolean(BooleanValue::new(span, name, true)),
          "false" => Self::Boolean(BooleanValue::new(span, name, false)),
          "null" => Self::Null(NullValue::new(span, name)),
          val => Self::Enum(EnumValue::new(span, val)),
        }
      }
    }
  }};
}

macro_rules! list_parser {
  ($parser:expr) => {{
    select! {
      Lexed::Token(Spanned { data: FastToken::LBracket, .. }) => (),
    }.ignore_then(
        $parser
          .repeated()
          .collect()
      ).then(RBracket::parser().or_not())
      .try_map_with(|(values, r), exa| {
        match r {
          Some(_) => Ok(Self::List(List::new(exa.span(), values))),
          None => Err(Error::unclosed_list(exa.span()).into()),
        }
      })
  }};
}

macro_rules! object_value_parser {
  ($parser:expr) => {{
    select! {
      Lexed::Token(Spanned { data: FastToken::LBrace, .. }) => (),
    }.ignore_then(
        ObjectField::parser_with(Name::parser(), $parser)
        .repeated()
        .collect()
      ).then(RBrace::parser().or_not())
      .try_map_with(|(fields, r), exa| {
        match r {
          Some(_) => Ok(Self::Object(Object::new(exa.span(), fields))),
          None => Err(Error::unclosed_object(exa.span()).into()),
        }
      })
  }};
}

#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  Variable(Variable<S>),
  Boolean(BooleanValue<S>),
  String(StringValue<S>),
  Float(FloatValue<S>),
  Int(IntValue<S>),
  Enum(EnumValue<S>),
  Null(NullValue<S>),
  List(List<InputValue<S>>),
  Object(Object<Name<S>, InputValue<S>>),
}

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for InputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    recursive(|parser| {
      let object_value_parser = object_value_parser!(parser.clone());
      let list_value_parser = list_parser!(parser);
      let int_value_parser = int_parser!();
      let float_value_parser = float_parser!();
      let string_value_parser = string_parser!();
      choice((
        list_value_parser,
        object_value_parser,
        enum_boolean_null_parser!(),
        select! {
          Lexed::Token(Spanned { span, data: FastToken::Dollar }) => span,
        }.then(Name::parser::<E>()).map(|(span, name)| {
          Variable::new(span.with_end(name.span().end()), name)
        }).map(Self::Variable),
        string_value_parser,
        float_value_parser,
        int_value_parser,
      ))
    })
  }
}

#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  Boolean(BooleanValue<S>),
  String(StringValue<S>),
  Float(FloatValue<S>),
  Int(IntValue<S>),
  Enum(EnumValue<S>),
  Null(NullValue<S>),
  List(List<ConstInputValue<S>>),
  Object(Object<Name<S>, ConstInputValue<S>>),
}

impl<'a> Parseable<'a, FastTokenStream<'a>, FastToken<'a>, FastTokenErrors<'a, &'a str>>
  for ConstInputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl chumsky::Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
    FastTokenStream<'a>: Tokenizer<
        'a,
        FastToken<'a>,
        Slice = <<FastToken<'a> as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    FastToken<'a>: Token<'a>,
    FastTokenErrors<'a, &'a str>: 'a,
  {
    recursive(|parser| {
      let object_value_parser = object_value_parser!(parser.clone());
      let list_value_parser = list_parser!(parser);
      let int_value_parser = int_parser!();
      let float_value_parser = float_parser!();
      let string_value_parser = string_parser!();

      choice((
        object_value_parser,
        list_value_parser,
        enum_boolean_null_parser!(),
        string_value_parser,
        float_value_parser,
        int_value_parser,
      ))
    })
  }
}
