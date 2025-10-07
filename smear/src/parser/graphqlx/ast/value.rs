use crate::{lexer::graphqlx::ast::AstLexerErrors, parser::ident::Ident, scaffold};

use super::{AstToken, AstTokenErrors, AstTokenStream, DefaultVec};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*, select},
  logos::Logos,
  utils::{AsSpan, IntoSpan, Span, Spanned, cmp::Equivalent},
};

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

pub type VariableValue<S> = crate::parser::value::VariableValue<Ident<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;

macro_rules! atom_parser {
  () => {{
    select! {
      Lexed::Token(Spanned { span, data: AstToken::Identifier(name) }) => {
        match () {
          () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
          () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
          () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
          _ => Self::Enum(EnumValue::new(span, name)),
        }
      },
      Lexed::Token(Spanned { span, data: AstToken::LitInt(val) }) => {
        Self::Int(IntValue::new(span, val))
      },
      Lexed::Token(Spanned { span, data: AstToken::LitFloat(val) }) => {
        Self::Float(FloatValue::new(span, val))
      },
      Lexed::Token(Spanned { span, data: AstToken::LitInlineStr(raw) }) => Self::String(StringValue::new(span, raw.into())),
      Lexed::Token(Spanned { span, data: AstToken::LitBlockStr(raw) }) => Self::String(StringValue::new(span, raw.into())),
    }
  }};
}

pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
pub type Set<S, Container = DefaultVec<InputValue<S>>> = scaffold::Set<InputValue<S>, Container>;
pub type Map<S, Container = DefaultVec<(InputValue<S>, InputValue<S>)>> =
  scaffold::Map<InputValue<S>, InputValue<S>, Container>;
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Ident<S>, InputValue<S>, Container>;

pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Set<ConstInputValue<S>, Container>;
pub type ConstMap<S, Container = DefaultVec<(ConstInputValue<S>, ConstInputValue<S>)>> =
  scaffold::Map<ConstInputValue<S>, ConstInputValue<S>, Container>;
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Ident<S>, ConstInputValue<S>, Container>;

/// GraphQLx Input Value
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// GraphQLx Variable
  Variable(VariableValue<S>),
  /// GraphQLx Boolean
  Boolean(BooleanValue),
  /// GraphQLx String
  String(StringValue<S>),
  /// GraphQLx Float
  Float(FloatValue<S>),
  /// GraphQLx Int
  Int(IntValue<S>),
  /// GraphQLx Enum
  Enum(EnumValue<S>),
  /// GraphQLx Null
  Null(NullValue<S>),
  /// GraphQLx List
  List(scaffold::List<InputValue<S>>),
  /// GraphQLx Set
  Set(scaffold::Set<InputValue<S>>),
  /// GraphQLx Map
  Map(scaffold::Map<InputValue<S>, InputValue<S>>),
  /// GraphQLx Object
  Object(scaffold::Object<Ident<S>, InputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for InputValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
    AstTokenStream<'a, S>: Tokenizer<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      let object_value_parser =
        scaffold::Object::parser_with(Ident::<S>::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = scaffold::List::parser_with(parser.clone()).map(Self::List);
      let set_value_parser = scaffold::Set::parser_with(parser.clone()).map(Self::Set);
      let map_value_parser = scaffold::Map::parser_with(parser.clone(), parser).map(Self::Map);

      choice((
        atom_parser!(),
        select! {
          Lexed::Token(Spanned { span, data: AstToken::Dollar }) => span,
        }
        .then(Ident::parser::<E>())
        .map(|(span, name)| VariableValue::new(span.with_end(name.span().end()), name))
        .map(Self::Variable),
        set_value_parser,
        map_value_parser,
        list_value_parser,
        object_value_parser,
      ))
    })
  }
}

/// GraphQLx Const Input Value
#[derive(Debug, Clone, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ConstInputValue<S> {
  /// GraphQLx Boolean value
  Boolean(BooleanValue),
  /// GraphQLx String value
  String(StringValue<S>),
  /// GraphQLx Float value
  Float(FloatValue<S>),
  /// GraphQLx Int value
  Int(IntValue<S>),
  /// GraphQLx Enum value
  Enum(EnumValue<S>),
  /// GraphQLx Null value
  Null(NullValue<S>),
  /// GraphQLx List value
  List(scaffold::List<ConstInputValue<S>>),
  /// GraphQLx Set value
  Set(scaffold::Set<ConstInputValue<S>>),
  /// GraphQLx Map value
  Map(scaffold::Map<ConstInputValue<S>, ConstInputValue<S>>),
  /// GraphQLx Object value
  Object(scaffold::Object<Ident<S>, ConstInputValue<S>>),
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
      Self::Set(v) => v.as_span(),
      Self::Map(v) => v.as_span(),
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
      Self::Set(v) => v.into_span(),
      Self::Map(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
    }
  }
}

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for ConstInputValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
    AstTokenStream<'a, S>: Tokenizer<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      let object_value_parser =
        scaffold::Object::parser_with(Ident::<S>::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = scaffold::List::parser_with(parser.clone()).map(Self::List);
      let set_value_parser = scaffold::Set::parser_with(parser.clone()).map(Self::Set);
      let map_value_parser = scaffold::Map::parser_with(parser.clone(), parser).map(Self::Map);

      choice((atom_parser!(), set_value_parser, map_value_parser, object_value_parser, list_value_parser,))
    })
  }
}
