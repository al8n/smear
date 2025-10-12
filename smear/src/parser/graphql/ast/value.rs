use crate::{lexer::graphql::ast::AstLexerErrors, scaffold};

use super::{AstToken, AstTokenErrors, AstTokenStream, AstTokenError, DefaultVec, Name};
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

pub type VariableValue<S> = crate::parser::value::VariableValue<super::Name<S>>;

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
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Name<S>, InputValue<S>, Container>;

pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Name<S>, ConstInputValue<S>, Container>;

/// GraphQL Input Value
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InputValue<S> {
  /// GraphQL Variable
  Variable(VariableValue<S>),
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
  List(scaffold::List<InputValue<S>>),
  /// GraphQL Object
  Object(scaffold::Object<Name<S>, InputValue<S>>),
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
        scaffold::Object::parser_with(Name::<S>::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = scaffold::List::parser_with(parser).map(Self::List);

      let all = custom::<_, AstTokenStream<'_, S>, Self, E>(|inp| {
        let before = inp.cursor();

        match inp.next() {
          Some(tok) => match tok {
            Lexed::Error(err) => {
              inp.save();
              Err(AstTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned {
              span,
              data: token,
            }) => {
              let output = match token {
                AstToken::Ampersand => todo!(),
                AstToken::At => todo!(),
                AstToken::RBrace => todo!(),
                AstToken::RBracket => todo!(),
                AstToken::RParen => todo!(),
                AstToken::Colon => todo!(),
                AstToken::Dollar => todo!(),
                AstToken::Equal => todo!(),
                AstToken::Bang => todo!(),
                AstToken::LParen => todo!(),
                AstToken::Pipe => todo!(),
                AstToken::Spread => todo!(),
                AstToken::LBrace => todo!(),
                AstToken::LBracket => todo!(),
                AstToken::Identifier(name) => {
                  match () {
                    () if "true".equivalent(&name) => {
                      Self::Boolean(BooleanValue::new(span, true))
                    }
                    () if "false".equivalent(&name) => {
                      Self::Boolean(BooleanValue::new(span, false))
                    }
                    () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                    _ => Self::Enum(EnumValue::new(span, name)),
                  }
                },
                AstToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                AstToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                AstToken::LitInlineStr(raw) => Self::String(StringValue::new(span, raw.into())),
                AstToken::LitBlockStr(raw) => Self::String(StringValue::new(span, raw.into())),
              };

              inp.save();
              Ok(output)
            },
          },
          None => {
            Err(AstTokenError::unexpected_end_of_input(inp.span_since(&before)).into())
          },
        }
      });

      choice((
        atom_parser!(),
        select! {
          Lexed::Token(Spanned { span, data: AstToken::Dollar }) => span,
        }
        .then(Name::parser::<E>())
        .map(|(span, name)| VariableValue::new(span.with_end(name.span().end()), name))
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
  List(scaffold::List<ConstInputValue<S>>),
  /// GraphQL Object value
  Object(scaffold::Object<Name<S>, ConstInputValue<S>>),
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
        scaffold::Object::parser_with(Name::<S>::parser(), parser.clone()).map(Self::Object);
      let list_value_parser = scaffold::List::parser_with(parser).map(Self::List);

      choice((atom_parser!(), object_value_parser, list_value_parser))
    })
  }
}
