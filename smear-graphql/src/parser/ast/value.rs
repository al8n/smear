use super::{AstToken, AstTokenErrors, AstTokenStream, DefaultVec, Name};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*, select},
  logos::Logos,
  utils::{AsSpan, IntoSpan, Span, Spanned},
};

use smear_parser::lang;

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

pub type Variable<S> = variable::Variable<super::Name<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;
mod variable;

macro_rules! atom_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: AstToken::Identifier(name) }) => {
        match name {
          "true" => Self::Boolean(BooleanValue::new(span, true)),
          "false" => Self::Boolean(BooleanValue::new(span, false)),
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
      Lexed::Token(Spanned { span, data: AstToken::LitInlineStr(raw) }) => Self::String(StringValue::new(span, raw.into())),
      Lexed::Token(Spanned { span, data: AstToken::LitBlockStr(raw) }) => Self::String(StringValue::new(span, raw.into())),
    }
  }};
}

pub type List<S, Container = DefaultVec<InputValue<S>>> = lang::List<InputValue<S>, Container>;
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  lang::Object<Name<S>, InputValue<S>, Container>;

pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  lang::List<ConstInputValue<S>, Container>;
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  lang::Object<Name<S>, ConstInputValue<S>, Container>;

/// GraphQL Input Value
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// GraphQL Variable
  Variable(Variable<S>),
  /// GraphQL Boolean
  Boolean(BooleanValue),
  /// GraphQL String
  String(StringValue<S>),
  /// GraphQL Float
  Float(FloatValue<S>),
  /// GraphQL Int
  Int(IntValue<S>),
  /// GraphQL Enum
  Enum(EnumValue<S>),
  /// GraphQL Null
  Null(NullValue<S>),
  /// GraphQL List
  List(lang::List<InputValue<S>>),
  /// GraphQL Object
  Object(lang::Object<Name<S>, InputValue<S>>),
}

impl<S> AsSpan<Span> for InputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Variable(v) => v.as_span(),
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S> IntoSpan<Span> for InputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Variable(v) => v.into_span(),
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
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
        lang::Object::parser_with(Name::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = lang::List::parser_with(parser).map(Self::List);
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

/// GraphQL Const Input Value
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// GraphQL Boolean value
  Boolean(BooleanValue),
  /// GraphQL String value
  String(StringValue<S>),
  /// GraphQL Float value
  Float(FloatValue<S>),
  /// GraphQL Int value
  Int(IntValue<S>),
  /// GraphQL Enum value
  Enum(EnumValue<S>),
  /// GraphQL Null value
  Null(NullValue<S>),
  /// GraphQL List value
  List(lang::List<ConstInputValue<S>>),
  /// GraphQL Object value
  Object(lang::Object<Name<S>, ConstInputValue<S>>),
}

impl<S> AsSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Boolean(v) => v.as_span(),
      Self::String(v) => v.as_span(),
      Self::Float(v) => v.as_span(),
      Self::Int(v) => v.as_span(),
      Self::Enum(v) => v.as_span(),
      Self::Null(v) => v.as_span(),
      Self::List(v) => v.as_span(),
      Self::Object(v) => v.as_span(),
    }
  }
}

impl<S> IntoSpan<Span> for ConstInputValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Boolean(v) => v.into_span(),
      Self::String(v) => v.into_span(),
      Self::Float(v) => v.into_span(),
      Self::Int(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::Null(v) => v.into_span(),
      Self::List(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
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
        lang::Object::parser_with(Name::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = lang::List::parser_with(parser).map(Self::List);

      choice((atom_parser!(), object_value_parser, list_value_parser))
    })
  }
}
