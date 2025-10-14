use crate::{
  error::{UnclosedBraceError, UnclosedBracketError},
  hints::VariableValueHint,
  lexer::graphqlx::ast::AstLexerErrors,
  parser::{graphqlx::Expectation, ident::Ident},
  punctuator::{LBrace, PathSeparator, RBrace, RBracket},
  scaffold::{self, Path},
};

use super::{AstToken, AstTokenError, AstTokenErrors, AstTokenStream, DefaultVec};
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

pub type VariableValue<S> = crate::parser::value::VariableValue<Ident<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;

pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
pub type Set<S, Container = DefaultVec<InputValue<S>>> = scaffold::Set<InputValue<S>, Container>;
pub type MapEntry<S> = scaffold::MapEntry<InputValue<S>, InputValue<S>>;
pub type Map<S, Container = DefaultVec<(InputValue<S>, InputValue<S>)>> =
  scaffold::Map<InputValue<S>, InputValue<S>, Container>;
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Ident<S>, InputValue<S>, Container>;
pub type ObjectField<S> = scaffold::ObjectField<Ident<S>, InputValue<S>>;

pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Set<ConstInputValue<S>, Container>;
pub type ConstMapEntry<S> = scaffold::MapEntry<ConstInputValue<S>, ConstInputValue<S>>;
pub type ConstMap<S, Container = DefaultVec<(ConstInputValue<S>, ConstInputValue<S>)>> =
  scaffold::Map<ConstInputValue<S>, ConstInputValue<S>, Container>;
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Ident<S>, ConstInputValue<S>, Container>;
pub type ConstObjectField<S> = scaffold::ObjectField<Ident<S>, ConstInputValue<S>>;

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
  LBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBracket: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  PathSeparator: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
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
                AstToken::PathSeparator => {
                  let segments = inp.parse(
                    Ident::<S>::parser()
                      .separated_by(PathSeparator::parser())
                      .at_least(1)
                      .collect(),
                  )?;

                  Self::Enum(EnumValue::new(Path::new(
                    inp.span_since(&before),
                    segments,
                    true,
                  )))
                }
                AstToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  () if "set".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: AstToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(parser.clone().repeated().collect())
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Set(Set::new(inp.span_since(&before), values)),
                        None => return Err(AstTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  () if "map".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: AstToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(
                            MapEntry::parser_with(parser.clone(), parser.clone())
                              .repeated()
                              .collect(),
                          )
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Map(Map::new(inp.span_since(&before), values)),
                        None => return Err(AstTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                },
                AstToken::Dollar => {
                  let current_cursor = inp.cursor();
                  let name = match inp.next() {
                    Some(Lexed::Token(Spanned {
                      span,
                      data: AstToken::Identifier(name),
                    })) => Ident::new(span, name),
                    Some(Lexed::Token(Spanned { span, data })) => {
                      return Err(
                        AstTokenError::unexpected_token(data, Expectation::Identifier, span).into(),
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
                    ObjectField::<S>::parser_with(Ident::<S>::parser(), parser.clone())
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
  LBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBrace: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  RBracket: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  PathSeparator: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
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
                AstToken::PathSeparator => {
                  let segments = inp.parse(
                    Ident::<S>::parser()
                      .separated_by(PathSeparator::parser())
                      .at_least(1)
                      .collect(),
                  )?;

                  Self::Enum(EnumValue::new(Path::new(
                    inp.span_since(&before),
                    segments,
                    true,
                  )))
                }
                AstToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  () if "set".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: AstToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(parser.clone().repeated().collect())
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Set(ConstSet::new(inp.span_since(&before), values)),
                        None => return Err(AstTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  () if "map".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: AstToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(
                            ConstMapEntry::parser_with(parser.clone(), parser.clone())
                              .repeated()
                              .collect(),
                          )
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Map(ConstMap::new(inp.span_since(&before), values)),
                        None => return Err(AstTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                },
                AstToken::LBrace => {
                  let (fields, rbrace) = inp.parse(
                    ConstObjectField::<S>::parser_with(Ident::<S>::parser(), parser.clone())
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
