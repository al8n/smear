use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{
    extra::ParserExtra,
    input::{Cursor, InputRef},
    prelude::*,
  },
  utils::{AsSpan, IntoSpan, Span, Spanned, cmp::Equivalent},
};
use smear_lexer::{
  graphql::syntactic::SyntacticLexerErrors,
  keywords::{Mutation, Query, Subscription},
  punctuator::RBrace,
};
use smear_scaffold::ast::{DirectiveLocations, OperationType};

use super::*;

/// Type definition for GraphQL specification.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum TypeDefinition<S, Ty = Type<S>> {
  /// A scalar type definition.
  Scalar(ScalarTypeDefinition<S>),
  /// An object type definition.
  Object(ObjectTypeDefinition<S, Ty>),
  /// An interface type definition.
  Interface(InterfaceTypeDefinition<S, Ty>),
  /// A union type definition.
  Union(UnionTypeDefinition<S>),
  /// An enum type definition.
  Enum(EnumTypeDefinition<S>),
  /// An input object type definition.
  InputObject(InputObjectTypeDefinition<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(s) => s.into_span(),
      Self::InputObject(i) => i.into_span(),
      Self::Object(o) => o.into_span(),
      Self::Interface(i) => i.into_span(),
      Self::Union(u) => u.into_span(),
      Self::Enum(e) => e.into_span(),
    }
  }
}

impl<S, Ty> TypeDefinition<S, Ty> {
  /// Returns the span of the type definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(s) => s.span(),
      Self::InputObject(i) => i.span(),
      Self::Object(o) => o.span(),
      Self::Interface(i) => i.span(),
      Self::Union(u) => u.span(),
      Self::Enum(e) => e.span(),
    }
  }

  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "scalar".equivalent(ident) => {
        match inp.parse(ScalarTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, directives)) => Ok(Self::Scalar(ScalarTypeDefinition::new(
            inp.span_since(&before),
            name,
            directives,
          ))),
        }
      }
      () if "type".equivalent(ident) => {
        match inp.parse(ObjectTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ImplementsInterfaces::parser(),
          ConstDirectives::parser(),
          FieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, interfaces, directives, fields)) => {
            Ok(Self::Object(ObjectTypeDefinition::new(
              inp.span_since(&before),
              name,
              interfaces,
              directives,
              fields,
            )))
          }
        }
      }
      () if "interface".equivalent(ident) => {
        match inp.parse(InterfaceTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ImplementsInterfaces::parser(),
          ConstDirectives::parser(),
          FieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, interfaces, directives, fields)) => {
            Ok(Self::Interface(InterfaceTypeDefinition::new(
              inp.span_since(&before),
              name,
              interfaces,
              directives,
              fields,
            )))
          }
        }
      }
      () if "union".equivalent(ident) => {
        match inp.parse(UnionTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          UnionMembers::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, directives, members)) => Ok(Self::Union(UnionTypeDefinition::new(
            inp.span_since(&before),
            name,
            directives,
            members,
          ))),
        }
      }
      () if "enum".equivalent(ident) => {
        match inp.parse(EnumTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          EnumValuesDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, directives, values)) => Ok(Self::Enum(EnumTypeDefinition::new(
            inp.span_since(&before),
            name,
            directives,
            values,
          ))),
        }
      }
      () if "input".equivalent(ident) => {
        match inp.parse(InputObjectTypeDefinition::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          InputFieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, directives, fields)) => Ok(Self::InputObject(InputObjectTypeDefinition::new(
            inp.span_since(&before),
            name,
            directives,
            fields,
          ))),
        }
      }
      _ => return None,
    })
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for TypeDefinition<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();
      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, before, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&["scalar", "type", "interface", "union", "enum", "input"]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned { span, data })) => {
          Err(SyntacticTokenError::unexpected_token(data, Expectation::TypeDefinition, span).into())
        }
      }
    })
  }
}

/// A GraphQL type extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeExtension<S, Ty = Type<Name<S>>> {
  /// The scalar type extension.
  Scalar(ScalarTypeExtension<S>),
  /// The object type extension.
  Object(ObjectTypeExtension<S, Ty>),
  /// The interface type extension.
  Interface(InterfaceTypeExtension<S, Ty>),
  /// The union type extension.
  Union(UnionTypeExtension<S>),
  /// The enum type extension.
  Enum(EnumTypeExtension<S>),
  /// The input object type extension.
  InputObject(InputObjectTypeExtension<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Scalar(v) => v.into_span(),
      Self::Object(v) => v.into_span(),
      Self::Interface(v) => v.into_span(),
      Self::Union(v) => v.into_span(),
      Self::Enum(v) => v.into_span(),
      Self::InputObject(v) => v.into_span(),
    }
  }
}

impl<S, Ty> TypeExtension<S, Ty> {
  /// Returns the span of the type extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Scalar(v) => v.span(),
      Self::Object(v) => v.span(),
      Self::Interface(v) => v.span(),
      Self::Union(v) => v.span(),
      Self::Enum(v) => v.span(),
      Self::InputObject(v) => v.span(),
    }
  }

  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "scalar".equivalent(ident) => {
        match inp.parse(ScalarTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, directives)) => Ok(Self::Scalar(ScalarTypeExtension::new(
            inp.span_since(&before),
            name,
            directives,
          ))),
        }
      }
      () if "type".equivalent(ident) => {
        match inp.parse(ObjectTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ImplementsInterfaces::parser(),
          ConstDirectives::parser(),
          FieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, data)) => Ok(Self::Object(ObjectTypeExtension::new(
            inp.span_since(&before),
            name,
            data,
          ))),
        }
      }
      () if "interface".equivalent(ident) => {
        match inp.parse(InterfaceTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ImplementsInterfaces::parser(),
          ConstDirectives::parser(),
          FieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, data)) => Ok(Self::Interface(InterfaceTypeExtension::new(
            inp.span_since(&before),
            name,
            data,
          ))),
        }
      }
      () if "union".equivalent(ident) => {
        match inp.parse(UnionTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          UnionMembers::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, data)) => Ok(Self::Union(UnionTypeExtension::new(
            inp.span_since(&before),
            name,
            data,
          ))),
        }
      }
      () if "enum".equivalent(ident) => {
        match inp.parse(EnumTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          EnumValuesDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, data)) => Ok(Self::Enum(EnumTypeExtension::new(
            inp.span_since(&before),
            name,
            data,
          ))),
        }
      }
      () if "input".equivalent(ident) => {
        match inp.parse(InputObjectTypeExtension::content_parser_with(
          Name::<S>::parser(),
          ConstDirectives::parser(),
          InputFieldsDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, data)) => Ok(Self::InputObject(InputObjectTypeExtension::new(
            inp.span_since(&before),
            name,
            data,
          ))),
        }
      }
      _ => return None,
    })
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for TypeExtension<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();
      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span: extend_span,
          data: SyntacticToken::Identifier(extend_ident),
        }))
          if "extend".equivalent(&extend_ident) =>
        {
          match inp.next() {
            None => {
              Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into())
            }
            Some(Lexed::Error(errs)) => {
              Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
            }
            Some(Lexed::Token(Spanned {
              span,
              data: SyntacticToken::Identifier(ident),
            })) => match Self::parser_inner(inp, before, &ident) {
              None => Err(
                SyntacticTokenError::unexpected_token(
                  SyntacticToken::Identifier(ident),
                  Expectation::Keyword(&["scalar", "type", "interface", "union", "enum", "input"]),
                  span,
                )
                .into(),
              ),
              Some(res) => res,
            },
            Some(Lexed::Token(Spanned { span, data })) => Err(
              SyntacticTokenError::unexpected_token(data, Expectation::TypeExtension, span).into(),
            ),
          }
        }
        Some(Lexed::Token(Spanned { span, data })) => Err(
          SyntacticTokenError::unexpected_token(data, Expectation::Keyword(&["extend"]), span)
            .into(),
        ),
      }
    })
  }
}

/// A GraphQL type system definition.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinition<S, Ty = Type<Name<S>>> {
  /// The type definition.
  Type(TypeDefinition<S, Ty>),
  /// The directive definition.
  Directive(DirectiveDefinition<S, Ty>),
  /// The schema definition.
  Schema(SchemaDefinition<S>),
}

impl<S, Ty> AsSpan<Span> for TypeSystemDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeSystemDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Directive(d) => d.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<S, Ty> TypeSystemDefinition<S, Ty> {
  /// Returns the span of the type system definition.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Directive(d) => d.span(),
      Self::Schema(s) => s.span(),
    }
  }

  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "directive".equivalent(ident) => {
        match inp.parse(DirectiveDefinition::content_parser_with(
          Name::<S>::parser(),
          ArgumentsDefinition::parser(),
          DirectiveLocations::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((name, arguments, repeatable, locations)) => {
            Ok(Self::Directive(DirectiveDefinition::new(
              inp.span_since(&before),
              name,
              arguments,
              repeatable,
              locations,
            )))
          }
        }
      }
      () if "schema".equivalent(ident) => {
        match inp.parse(SchemaDefinition::content_parser_with(
          ConstDirectives::parser(),
          RootOperationTypesDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok((directives, operation_types)) => Ok(Self::Schema(SchemaDefinition::new(
            inp.span_since(&before),
            directives,
            operation_types,
          ))),
        }
      }
      _ => {
        // Try to parse as a TypeDefinition (scalar, type, interface, union, enum, input)
        return TypeDefinition::parser_inner(inp, before, ident).map(|res| res.map(Self::Type));
      }
    })
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for TypeSystemDefinition<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();
      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, before, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&[
                "scalar",
                "type",
                "interface",
                "union",
                "enum",
                "input",
                "directive",
                "schema",
              ]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned { span, data })) => Err(
          SyntacticTokenError::unexpected_token(data, Expectation::TypeSystemDefinition, span)
            .into(),
        ),
      }
    })
  }
}

/// A GraphQL type system extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemExtension<S, Ty = Type<Name<S>>> {
  /// The type extension.
  Type(TypeExtension<S, Ty>),
  /// The schema extension.
  Schema(SchemaExtension<S>),
}

impl<S, Ty> AsSpan<Span> for TypeSystemExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeSystemExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Type(t) => t.into_span(),
      Self::Schema(s) => s.into_span(),
    }
  }
}

impl<S, Ty> TypeSystemExtension<S, Ty> {
  /// Returns the span of the type system extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Type(t) => t.span(),
      Self::Schema(s) => s.span(),
    }
  }

  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "schema".equivalent(ident) => {
        match inp.parse(SchemaExtension::content_parser_with(
          ConstDirectives::parser(),
          RootOperationTypesDefinition::parser(),
        )) {
          Err(errs) => Err(errs),
          Ok(data) => Ok(Self::Schema(SchemaExtension::new(
            inp.span_since(&before),
            data,
          ))),
        }
      }
      _ => {
        // Try to parse as a TypeExtension (scalar, type, interface, union, enum, input)
        return TypeExtension::parser_inner(inp, before, ident).map(|res| res.map(Self::Type));
      }
    })
  }
}

/// A GraphQL type system definition or extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum TypeSystemDefinitionOrExtension<S, Ty = Type<Name<S>>> {
  /// The type system definition (with optional description).
  Definition(Described<TypeSystemDefinition<S, Ty>, S>),
  /// The type system extension.
  Extension(TypeSystemExtension<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for TypeSystemDefinitionOrExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S, Ty> IntoSpan<Span> for TypeSystemDefinitionOrExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

impl<S, Ty> TypeSystemDefinitionOrExtension<S, Ty> {
  /// Returns the span of the type system definition or extension.
  #[inline]
  pub const fn span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.span(),
      Self::Extension(e) => e.span(),
    }
  }

  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "extend".equivalent(ident) => {
        // Extension - parse the type extension after "extend"
        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(Lexed::Error(errs)) => {
            Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
          }
          Some(Lexed::Token(Spanned {
            span: ext_span,
            data: SyntacticToken::Identifier(ext_ident),
          })) => match TypeSystemExtension::parser_inner(inp, before, &ext_ident) {
            None => Err(
              SyntacticTokenError::unexpected_token(
                SyntacticToken::Identifier(ext_ident),
                Expectation::Keyword(&[
                  "scalar",
                  "type",
                  "interface",
                  "union",
                  "enum",
                  "input",
                  "schema",
                ]),
                ext_span,
              )
              .into(),
            ),
            Some(Ok(ext)) => Ok(Self::Extension(ext)),
            Some(Err(errs)) => Err(errs),
          },
          Some(Lexed::Token(Spanned { span, data })) => Err(
            SyntacticTokenError::unexpected_token(data, Expectation::TypeSystemExtension, span)
              .into(),
          ),
        }
      }
      _ => {
        // Definition - parse as TypeSystemDefinition
        return TypeSystemDefinition::parser_inner(inp, before, ident)
          .map(|res| res.map(|def| Self::Definition(Described::new(*def.span(), None, def))));
      }
    })
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for TypeSystemDefinitionOrExtension<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();

      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, before, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&[
                "extend",
                "scalar",
                "type",
                "interface",
                "union",
                "enum",
                "input",
                "directive",
                "schema",
              ]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::LitBlockStr(description),
        })) => inp.parse(TypeSystemDefinition::parser()).map(|def| {
          Self::Definition(Described::new(
            inp.span_since(&before),
            Some(StringValue::<S>::block(span, description)),
            def,
          ))
        }),
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::LitInlineStr(description),
        })) => inp.parse(TypeSystemDefinition::parser()).map(|def| {
          Self::Definition(Described::new(
            inp.span_since(&before),
            Some(StringValue::<S>::inline(span, description)),
            def,
          ))
        }),
        Some(Lexed::Token(Spanned { span, data })) => Err(
          SyntacticTokenError::unexpected_token(
            data,
            Expectation::TypeSystemDefinitionOrExtension,
            span,
          )
          .into(),
        ),
      }
    })
  }
}

/// A GraphQL executable definition.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ExecutableDefinition<S, Ty = Type<Name<S>>> {
  /// An operation definition.
  Operation(OperationDefinition<S, Ty>),
  /// A fragment definition.
  Fragment(FragmentDefinition<S>),
}

impl<S, Ty> AsSpan<Span> for ExecutableDefinition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Operation(o) => o.as_span(),
      Self::Fragment(f) => f.as_span(),
    }
  }
}

impl<S, Ty> IntoSpan<Span> for ExecutableDefinition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Operation(o) => o.into_span(),
      Self::Fragment(f) => f.into_span(),
    }
  }
}

impl<S, Ty> ExecutableDefinition<S, Ty> {
  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: &Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    span: Span,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    RBrace: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    macro_rules! parse_named_operation_type_definition {
      ($kind:ident::new($span:ident)) => {{
        let op_type = OperationType::$kind($kind::new($span));

        match inp.parse(NamedOperationDefinition::content_parser_with(
          Name::<S>::parser(),
          VariablesDefinition::parser(),
          Directives::parser(),
          SelectionSet::parser(),
        )) {
          Err(err) => Err(err),
          Ok((name, variable_definitions, directives, selection_set)) => Ok(Self::Operation(
            smear_scaffold::ast::OperationDefinition::Named(NamedOperationDefinition::new(
              inp.span_since(before),
              op_type,
              name,
              variable_definitions,
              directives,
              selection_set,
            )),
          )),
        }
      }};
    }
    Some(match () {
      () if "fragment".equivalent(ident) => {
        match inp.parse(FragmentDefinition::content_parser_with(
          FragmentName::parser(),
          TypeCondition::parser(),
          Directives::parser(),
          SelectionSet::parser(),
        )) {
          Err(err) => Err(err),
          Ok((name, type_condition, directives, selection_set)) => {
            Ok(Self::Fragment(FragmentDefinition::new(
              inp.span_since(before),
              name,
              type_condition,
              directives,
              selection_set,
            )))
          }
        }
      }
      () if "query".equivalent(ident) => {
        parse_named_operation_type_definition!(Query::new(span))
      }
      () if "mutation".equivalent(ident) => {
        parse_named_operation_type_definition!(Mutation::new(span))
      }
      () if "subscription".equivalent(ident) => {
        parse_named_operation_type_definition!(Subscription::new(span))
      }
      _ => return None,
    })
  }

  fn parser_short_hand<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
  ) -> Result<Self, SyntacticTokenErrors<'a, S>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    RBrace: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    match inp.parse(
      Selection::parser()
        .repeated()
        .at_least(1)
        .collect()
        .then(RBrace::parser().or_not()),
    ) {
      Err(err) => Err(err),
      Ok((selections, rbrace)) => match rbrace {
        Some(_) => Ok(Self::Operation(OperationDefinition::Shorthand(
          SelectionSet::new(inp.span_since(&before), selections),
        ))),
        None => Err(SyntacticTokenError::unclosed_object(inp.span_since(&before)).into()),
      },
    }
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for ExecutableDefinition<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();

      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span: _,
          data: SyntacticToken::LBrace,
        })) => Self::parser_short_hand(inp, before),
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, &before, span, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&["fragment", "query", "mutation", "subscription"]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned { span, data })) => Err(
          SyntacticTokenError::unexpected_token(data, Expectation::ExecutableDefinition, span)
            .into(),
        ),
      }
    })
  }
}

/// A GraphQL definition (type system or executable).
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Definition<S, Ty = Type<Name<S>>> {
  /// A type system definition.
  TypeSystem(TypeSystemDefinition<S, Ty>),
  /// An executable definition.
  Executable(ExecutableDefinition<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for Definition<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::TypeSystem(t) => t.as_span(),
      Self::Executable(e) => e.as_span(),
    }
  }
}

impl<S, Ty> IntoSpan<Span> for Definition<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::TypeSystem(t) => t.into_span(),
      Self::Executable(e) => e.into_span(),
    }
  }
}

impl<S, Ty> Definition<S, Ty> {
  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    span: Span,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    RBrace: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    // Try to parse as executable definition first (fragment, query, mutation, subscription)
    if let Some(result) = ExecutableDefinition::parser_inner(inp, &before, span, ident) {
      return Some(result.map(Self::Executable));
    }

    // Otherwise, try to parse as type system definition
    TypeSystemDefinition::parser_inner(inp, before, ident).map(|res| res.map(Self::TypeSystem))
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for Definition<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();

      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span: _,
          data: SyntacticToken::LBrace,
        })) => ExecutableDefinition::parser_short_hand(inp, before).map(Self::Executable),
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, before, span, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&[
                "fragment",
                "query",
                "mutation",
                "subscription",
                "scalar",
                "type",
                "interface",
                "union",
                "enum",
                "input",
                "directive",
                "schema",
              ]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned { span, data })) => {
          Err(SyntacticTokenError::unexpected_token(data, Expectation::Definition, span).into())
        }
      }
    })
  }
}

/// A GraphQL definition or extension.
#[derive(Debug, Clone, From, Unwrap, IsVariant, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum DefinitionOrExtension<S, Ty = Type<Name<S>>> {
  /// A definition (with optional description for type system definitions).
  Definition(Described<Definition<S, Ty>, S>),
  /// A type system extension.
  Extension(TypeSystemExtension<S, Ty>),
}

impl<S, Ty> AsSpan<Span> for DefinitionOrExtension<S, Ty> {
  #[inline]
  fn as_span(&self) -> &Span {
    match self {
      Self::Definition(d) => d.as_span(),
      Self::Extension(e) => e.as_span(),
    }
  }
}

impl<S, Ty> IntoSpan<Span> for DefinitionOrExtension<S, Ty> {
  #[inline]
  fn into_span(self) -> Span {
    match self {
      Self::Definition(d) => d.into_span(),
      Self::Extension(e) => e.into_span(),
    }
  }
}

impl<S, Ty> DefinitionOrExtension<S, Ty> {
  fn parser_inner<'a, 'p, E>(
    inp: &mut InputRef<'a, 'p, SyntacticTokenStream<'a, S>, E>,
    before: Cursor<'a, 'p, SyntacticTokenStream<'a, S>>,
    span: Span,
    ident: &S,
  ) -> Option<Result<Self, SyntacticTokenErrors<'a, S>>>
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
    Ty: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    SyntacticToken<S>: Token<'a>,
    <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
    <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
    Arguments<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    Directives<S>: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    RBrace: Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
      + 'a,
    str: Equivalent<S>,
  {
    Some(match () {
      () if "extend".equivalent(ident) => {
        // Extension - parse the type system extension after "extend"
        match inp.next() {
          None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
          Some(Lexed::Error(errs)) => {
            Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
          }
          Some(Lexed::Token(Spanned {
            span: ext_span,
            data: SyntacticToken::Identifier(ext_ident),
          })) => match TypeSystemExtension::parser_inner(inp, before, &ext_ident) {
            None => Err(
              SyntacticTokenError::unexpected_token(
                SyntacticToken::Identifier(ext_ident),
                Expectation::Keyword(&[
                  "scalar",
                  "type",
                  "interface",
                  "union",
                  "enum",
                  "input",
                  "schema",
                ]),
                ext_span,
              )
              .into(),
            ),
            Some(Ok(ext)) => Ok(Self::Extension(ext)),
            Some(Err(errs)) => Err(errs),
          },
          Some(Lexed::Token(Spanned { span, data })) => Err(
            SyntacticTokenError::unexpected_token(data, Expectation::TypeSystemExtension, span)
              .into(),
          ),
        }
      }
      _ => {
        // Definition - parse as Definition
        return Definition::parser_inner(inp, before, span, ident)
          .map(|res| res.map(|def| Self::Definition(Described::new(*def.as_span(), None, def))));
      }
    })
  }
}

impl<'a, S: 'a, Ty: 'a>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for DefinitionOrExtension<S, Ty>
where
  Ty:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Arguments<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  Directives<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
  RBrace:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>> + 'a,
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
    custom(|inp| {
      let before = inp.cursor();

      match inp.next() {
        None => Err(SyntacticTokenError::unexpected_end_of_input(inp.span_since(&before)).into()),
        Some(Lexed::Error(errs)) => {
          Err(SyntacticTokenError::from_lexer_errors(errs, inp.span_since(&before)).into())
        }
        Some(Lexed::Token(Spanned {
          span: _,
          data: SyntacticToken::LBrace,
        })) => ExecutableDefinition::parser_short_hand(inp, before)
          .map(Definition::Executable)
          .map(|def| Self::Definition(Described::new(*def.as_span(), None, def))),
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::Identifier(ident),
        })) => match Self::parser_inner(inp, before, span, &ident) {
          None => Err(
            SyntacticTokenError::unexpected_token(
              SyntacticToken::Identifier(ident),
              Expectation::Keyword(&[
                "extend",
                "fragment",
                "query",
                "mutation",
                "subscription",
                "scalar",
                "type",
                "interface",
                "union",
                "enum",
                "input",
                "directive",
                "schema",
              ]),
              span,
            )
            .into(),
          ),
          Some(res) => res,
        },
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::LitBlockStr(description),
        })) => inp.parse(Definition::parser()).map(|def| {
          Self::Definition(Described::new(
            inp.span_since(&before),
            Some(StringValue::<S>::block(span, description)),
            def,
          ))
        }),
        Some(Lexed::Token(Spanned {
          span,
          data: SyntacticToken::LitInlineStr(description),
        })) => inp.parse(Definition::parser()).map(|def| {
          Self::Definition(Described::new(
            inp.span_since(&before),
            Some(StringValue::<S>::inline(span, description)),
            def,
          ))
        }),
        Some(Lexed::Token(Spanned { span, data })) => Err(
          SyntacticTokenError::unexpected_token(data, Expectation::DefinitionOrExtension, span)
            .into(),
        ),
      }
    })
  }
}
