use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*, select},
  logos::Logos,
  utils::Spanned,
};
use smear_parser::lang::{List, Name, Object};

use super::{AstToken, AstTokenErrors, AstTokenStream};

use crate::lexer::ast::TokenKind;

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
mod null_value;
mod string;

pub type Variable<S> = smear_parser::lang::Variable<Name<S>>;

pub type DefaultInputValue<S> = smear_parser::lang::DefaultInputValue<ConstInputValue<S>>;

macro_rules! atom_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: AstToken::Identifier(name) }) => {
        match name {
          "true" => Self::Boolean(BooleanValue::new(span, name, true)),
          "false" => Self::Boolean(BooleanValue::new(span, name, false)),
          "null" => Self::Null(NullValue::new(span, name)),
          val => Self::Enum(EnumValue::new(span, val)),
        }
      },
      Lexed::Token(Spanned { span, data: AstToken::Int(val) }) => {
        Self::Int(IntValue::new(span, val))
      },
      Lexed::Token(Spanned { span, data: AstToken::Float(val) }) => {
        Self::Float(FloatValue::new(span, val))
      },
      Lexed::Token(Spanned { span, data: AstToken::StringLiteral(raw) }) => Self::String(StringValue::inline(span, raw, raw.trim_matches('"'))),
      Lexed::Token(Spanned { span, data: AstToken::BlockStringLiteral(raw) }) => Self::String(StringValue::block(span, raw, raw.trim_matches('"'))),
    }
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

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for InputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
  {
    recursive(|parser| {
      let object_value_parser =
        Object::parser_with(Name::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = List::parser_with(parser).map(Self::List);
      choice((
        atom_parser!(),
        select! {
          Lexed::Token(Spanned { span, data: AstToken::Dollar }) => span,
        }
        .then(Name::parser::<E>())
        .map(|(span, name)| Variable::new(span.with_end(name.span().end()), name))
        .map(Self::Variable),
        list_value_parser,
        object_value_parser,
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

impl<'a> Parseable<'a, AstTokenStream<'a>, AstToken<'a>, AstTokenErrors<'a, &'a str>>
  for ConstInputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, AstTokenStream<'a>, Error = AstTokenErrors<'a, &'a str>> + 'a,
    AstTokenStream<'a>: Tokenizer<
        'a,
        AstToken<'a>,
        Slice = <<AstToken<'a> as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstToken<'a>: Token<'a>,
    AstTokenErrors<'a, &'a str>: 'a,
  {
    recursive(|parser| {
      let object_value_parser =
        Object::parser_with(Name::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = List::parser_with(parser).map(Self::List);

      choice((atom_parser!(), object_value_parser, list_value_parser))
    })
  }
}
