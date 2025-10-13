use crate::{
  error::{UnclosedBraceError as _, UnclosedBracketError},
  hints::VariableValueHint,
  lexer::graphql::ast::AstLexerErrors,
  punctuator::{RBrace, RBracket},
  scaffold,
};

use super::{
  AstToken, AstTokenError, AstTokenErrors, AstTokenStream, DefaultVec, Expectation, Name,
};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
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

pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Name<S>, InputValue<S>, Container>;
pub type ObjectField<S> = scaffold::ObjectField<Name<S>, InputValue<S>>;

pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Name<S>, ConstInputValue<S>, Container>;
pub type ConstObjectField<S> = scaffold::ObjectField<Name<S>, ConstInputValue<S>>;

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
  RBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBracket: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
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
      custom::<_, AstTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(AstTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(AstTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                AstToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                AstToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                AstToken::LitInlineStr(raw) => Self::String(StringValue::new(span, raw.into())),
                AstToken::LitBlockStr(raw) => Self::String(StringValue::new(span, raw.into())),
                AstToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  _ => Self::Enum(EnumValue::new(span, name)),
                },
                AstToken::Dollar => {
                  let current_cursor = inp.cursor();
                  let name = match inp.next() {
                    Some(Lexed::Token(Spanned {
                      span,
                      data: AstToken::Identifier(name),
                    })) => Name::new(span, name),
                    Some(Lexed::Token(Spanned { span, data })) => {
                      return Err(
                        AstTokenError::unexpected_token(data, Expectation::Name, span).into(),
                      );
                    }
                    Some(Lexed::Error(err)) => {
                      return Err(
                        AstTokenError::from_lexer_errors(err, inp.span_since(&current_cursor))
                          .into(),
                      );
                    }
                    None => {
                      return Err(
                        AstTokenError::unexpected_end_of_variable_value(
                          VariableValueHint::Name,
                          inp.span_since(&before),
                        )
                        .into(),
                      );
                    }
                  };
                  Self::Variable(VariableValue::new(inp.span_since(&before), name))
                }
                AstToken::LBrace => {
                  let (fields, rbrace) = inp.parse(
                    ObjectField::<S>::parser_with(Name::<S>::parser(), parser.clone())
                      .repeated()
                      .collect()
                      .then(RBrace::parser().or_not()),
                  )?;
                  return match rbrace {
                    Some(_) => Ok(Self::Object(scaffold::Object::new(
                      inp.span_since(&before),
                      fields,
                    ))),
                    None => Err(AstTokenErrors::unclosed_brace(inp.span_since(&before))),
                  };
                }
                AstToken::LBracket => {
                  let (elements, rbracket) = inp.parse(
                    parser
                      .clone()
                      .repeated()
                      .collect()
                      .then(RBracket::parser().or_not()),
                  )?;

                  return match rbracket {
                    Some(_) => Ok(Self::List(scaffold::List::new(
                      inp.span_since(&before),
                      elements,
                    ))),
                    None => Err(AstTokenErrors::unclosed_bracket(inp.span_since(&before))),
                  };
                }
                tok => {
                  return Err(
                    AstTokenError::unexpected_token(tok, Expectation::InputValue, span).into(),
                  );
                }
              };

              Ok(output)
            }
          },
        }
      })
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
  RBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBracket: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
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
      custom::<_, AstTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(AstTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(AstTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                AstToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                AstToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                AstToken::LitInlineStr(raw) => Self::String(StringValue::new(span, raw.into())),
                AstToken::LitBlockStr(raw) => Self::String(StringValue::new(span, raw.into())),
                AstToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  _ => Self::Enum(EnumValue::new(span, name)),
                },
                AstToken::LBrace => {
                  let (fields, rbrace) = inp.parse(
                    ConstObjectField::<S>::parser_with(Name::<S>::parser(), parser.clone())
                      .repeated()
                      .collect()
                      .then(RBrace::parser().or_not()),
                  )?;
                  return match rbrace {
                    Some(_) => Ok(Self::Object(scaffold::Object::new(
                      inp.span_since(&before),
                      fields,
                    ))),
                    None => Err(AstTokenErrors::unclosed_brace(inp.span_since(&before))),
                  };
                }
                AstToken::LBracket => {
                  let (elements, rbracket) = inp.parse(
                    parser
                      .clone()
                      .repeated()
                      .collect()
                      .then(RBracket::parser().or_not()),
                  )?;

                  return match rbracket {
                    Some(_) => Ok(Self::List(scaffold::List::new(
                      inp.span_since(&before),
                      elements,
                    ))),
                    None => Err(AstTokenErrors::unclosed_bracket(inp.span_since(&before))),
                  };
                }
                tok => {
                  return Err(
                    AstTokenError::unexpected_token(tok, Expectation::ConstInputValue, span).into(),
                  );
                }
              };

              Ok(output)
            }
          },
        }
      })
    })
  }
}
