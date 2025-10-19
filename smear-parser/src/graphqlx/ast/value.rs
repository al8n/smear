use crate::{
  hints::VariableValueHint, ident::Ident, lexer::graphqlx::syntactic::SyntacticLexerErrors,
};

use super::{
  DefaultVec, Expectation, SyntacticToken, SyntacticTokenError, SyntacticTokenErrors,
  SyntacticTokenStream,
};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra, prelude::*},
  logos::Logos,
  utils::{AsSpan, IntoSpan, Span, Spanned, cmp::Equivalent},
};
use smear_lexer::punctuator::{LBrace, PathSeparator, RBrace, RBracket};
use smear_scaffold::{
  self as scaffold, Path,
  error::{UnclosedBraceError, UnclosedBracketError},
};

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;

/// A variable value reference in GraphQLx.
pub type VariableValue<S> = crate::value::VariableValue<Ident<S>>;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;

/// A list value in GraphQLx that can contain any input value.
pub type List<S, Container = DefaultVec<InputValue<S>>> = scaffold::List<InputValue<S>, Container>;
/// A set value in GraphQLx containing unique input values.
pub type Set<S, Container = DefaultVec<InputValue<S>>> = scaffold::Set<InputValue<S>, Container>;
/// A key-value entry in a map.
pub type MapEntry<S> = scaffold::MapEntry<InputValue<S>, InputValue<S>>;
/// A map value in GraphQLx containing key-value pairs.
pub type Map<S, Container = DefaultVec<(InputValue<S>, InputValue<S>)>> =
  scaffold::Map<InputValue<S>, InputValue<S>, Container>;
/// An object value in GraphQLx containing named fields.
pub type Object<S, Container = DefaultVec<InputValue<S>>> =
  scaffold::Object<Ident<S>, InputValue<S>, Container>;
/// A field in an object value.
pub type ObjectField<S> = scaffold::ObjectField<Ident<S>, InputValue<S>>;

/// A constant list value in GraphQLx (cannot contain variables).
pub type ConstList<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::List<ConstInputValue<S>, Container>;
/// A constant set value in GraphQLx (cannot contain variables).
pub type ConstSet<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Set<ConstInputValue<S>, Container>;
/// A constant key-value entry in a map.
pub type ConstMapEntry<S> = scaffold::MapEntry<ConstInputValue<S>, ConstInputValue<S>>;
/// A constant map value in GraphQLx (cannot contain variables).
pub type ConstMap<S, Container = DefaultVec<(ConstInputValue<S>, ConstInputValue<S>)>> =
  scaffold::Map<ConstInputValue<S>, ConstInputValue<S>, Container>;
/// A constant object value in GraphQLx (cannot contain variables).
pub type ConstObject<S, Container = DefaultVec<ConstInputValue<S>>> =
  scaffold::Object<Ident<S>, ConstInputValue<S>, Container>;
/// A field in a constant object value.
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

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for InputValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  LBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBracket:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  PathSeparator:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      custom::<_, SyntacticTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(SyntacticTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                SyntacticToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                SyntacticToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                SyntacticToken::LitInlineStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::LitBlockStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::PathSeparator => {
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
                SyntacticToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  () if "set".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: SyntacticToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(parser.clone().repeated().collect())
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Set(Set::new(inp.span_since(&before), values)),
                        None => return Err(SyntacticTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  () if "map".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: SyntacticToken::LBrace,
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
                        None => return Err(SyntacticTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                },
                SyntacticToken::Dollar => {
                  let current_cursor = inp.cursor();
                  let name = match inp.next() {
                    Some(Lexed::Token(Spanned {
                      span,
                      data: SyntacticToken::Identifier(name),
                    })) => Ident::new(span, name),
                    Some(Lexed::Token(Spanned { span, data })) => {
                      return Err(
                        SyntacticTokenError::unexpected_token(data, Expectation::Identifier, span)
                          .into(),
                      );
                    }
                    Some(Lexed::Error(err)) => {
                      return Err(
                        SyntacticTokenError::from_lexer_errors(
                          err,
                          inp.span_since(&current_cursor),
                        )
                        .into(),
                      );
                    }
                    None => {
                      return Err(
                        SyntacticTokenError::unexpected_end_of_variable_value(
                          VariableValueHint::Name,
                          inp.span_since(&before),
                        )
                        .into(),
                      );
                    }
                  };
                  Self::Variable(VariableValue::new(inp.span_since(&before), name))
                }
                SyntacticToken::LBrace => {
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
                    None => Err(SyntacticTokenErrors::unclosed_brace(
                      inp.span_since(&before),
                    )),
                  };
                }
                SyntacticToken::LBracket => {
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
                    None => Err(SyntacticTokenErrors::unclosed_bracket(
                      inp.span_since(&before),
                    )),
                  };
                }
                tok => {
                  return Err(
                    SyntacticTokenError::unexpected_token(tok, Expectation::InputValue, span)
                      .into(),
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

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for ConstInputValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  LBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  RBracket:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  PathSeparator:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  str: Equivalent<S>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticTokenErrors<'a, S>: 'a,
  {
    recursive(|parser| {
      custom::<_, SyntacticTokenStream<'_, S>, Self, E>(move |inp| {
        let before = inp.cursor();

        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(tok) => match tok {
            Lexed::Error(err) => {
              Err(SyntacticTokenError::from_lexer_errors(err, inp.span_since(&before)).into())
            }
            Lexed::Token(Spanned { span, data: token }) => {
              let output = match token {
                SyntacticToken::LitFloat(raw) => Self::Float(FloatValue::new(span, raw)),
                SyntacticToken::LitInt(raw) => Self::Int(IntValue::new(span, raw)),
                SyntacticToken::LitInlineStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::LitBlockStr(raw) => {
                  Self::String(StringValue::new(span, raw.into()))
                }
                SyntacticToken::PathSeparator => {
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
                SyntacticToken::Identifier(name) => match () {
                  () if "true".equivalent(&name) => Self::Boolean(BooleanValue::new(span, true)),
                  () if "false".equivalent(&name) => Self::Boolean(BooleanValue::new(span, false)),
                  () if "null".equivalent(&name) => Self::Null(NullValue::new(span, name)),
                  () if "set".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: SyntacticToken::LBrace,
                      ..
                    })) => {
                      let (values, r) = inp.parse(
                        LBrace::parser()
                          .ignore_then(parser.clone().repeated().collect())
                          .then(RBrace::parser().or_not()),
                      )?;

                      match r {
                        Some(_) => Self::Set(ConstSet::new(inp.span_since(&before), values)),
                        None => return Err(SyntacticTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  () if "map".equivalent(&name) => match inp.peek() {
                    None => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                    Some(Lexed::Token(Spanned {
                      data: SyntacticToken::LBrace,
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
                        None => return Err(SyntacticTokenError::unclosed_brace(span).into()),
                      }
                    }
                    _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                  },
                  _ => Self::Enum(EnumValue::new(Path::from(Ident::new(span, name)))),
                },
                SyntacticToken::LBrace => {
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
                    None => Err(SyntacticTokenErrors::unclosed_brace(
                      inp.span_since(&before),
                    )),
                  };
                }
                SyntacticToken::LBracket => {
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
                    None => Err(SyntacticTokenErrors::unclosed_bracket(
                      inp.span_since(&before),
                    )),
                  };
                }
                tok => {
                  return Err(
                    SyntacticTokenError::unexpected_token(tok, Expectation::InputValue, span)
                      .into(),
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
