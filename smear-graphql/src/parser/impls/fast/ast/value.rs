use crate::{error::Error, lexer::token::fast::Token};

use chumsky::{Parser, extra::ParserExtra, prelude::*};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::Parseable;
use smear_parser::lang::minized::{List, Name, Object};

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

impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>>
  for InputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    recursive(|parser| {
      let boolean_value_parser = BooleanValue::parser::<E>().map(Self::Boolean);
      let null_value_parser = NullValue::parser::<E>().map(Self::Null);
      let int_value_parser = IntValue::parser::<E>().map(Self::Int);
      let float_value_parser = FloatValue::parser::<E>().map(Self::Float);
      let string_value_parser = StringValue::parser::<E>().map(Self::String);
      let enum_value_parser = EnumValue::parser::<E>().map(Self::Enum);
      let variable_value_parser = Variable::parser::<E>().map(Self::Variable);
      let object_value_parser = Object::parser_with(Name::parser(), parser.clone(), |span| {
        Error::unclosed_object(span).into()
      })
      .map(Self::Object);
      let list_value_parser =
        List::parser_with(parser, |span| Error::unclosed_list(span).into()).map(Self::List);

      choice((
        boolean_value_parser,
        null_value_parser,
        enum_value_parser,
        variable_value_parser,
        string_value_parser,
        float_value_parser,
        int_value_parser,
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

impl<'a> Parseable<'a, FastTokenStream<'a>, Token<'a>, FastTokenErrors<'a, &'a str>>
  for ConstInputValue<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, FastTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, FastTokenStream<'a>, Error = FastTokenErrors<'a, &'a str>> + 'a,
  {
    recursive(|parser| {
      let boolean_value_parser = BooleanValue::parser::<E>().map(Self::Boolean);
      let null_value_parser = NullValue::parser::<E>().map(Self::Null);
      let int_value_parser = IntValue::parser::<E>().map(Self::Int);
      let float_value_parser = FloatValue::parser::<E>().map(Self::Float);
      let string_value_parser = StringValue::parser::<E>().map(Self::String);
      let enum_value_parser = EnumValue::parser::<E>().map(Self::Enum);
      let object_value_parser = Object::parser_with(Name::parser(), parser.clone(), |span| {
        Error::unclosed_object(span).into()
      })
      .map(Self::Object);
      let list_value_parser =
        List::parser_with(parser, |span| Error::unclosed_list(span).into()).map(Self::List);

      choice((
        boolean_value_parser,
        null_value_parser,
        enum_value_parser,
        string_value_parser,
        float_value_parser,
        int_value_parser,
        list_value_parser,
        object_value_parser,
      ))
    })
  }
}
