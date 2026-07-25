//! GraphQLX parser errors and Tokora conversion glue.
//!
//! The GraphQLX parser owns its error family so it remains usable with the
//! `graphqlx` feature alone; it deliberately does not depend on the separately
//! feature-gated GraphQL dialect module.

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::{
  graphqlx::{
    error::LexerErrors,
    syntactic::{SyntacticToken, SyntacticTokenKind},
  },
  tokora::error::UnexpectedEnd,
};
use tokora::{
  SimpleSpan as Span,
  error::{
    Unclosed as TokoraUnclosed, UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken as TokUnexpectedToken},
  },
  punct::{Angle, Brace, Bracket, Paren},
  utils::Expected,
};

use super::GraphQLX;

/// Typed expectations reported by GraphQLX productions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Expectation {
  /// An inline string literal.
  InlineString,
  /// A block string literal.
  BlockString,
  /// A dollar sign (`$`).
  Dollar,
  /// An opening parenthesis (`(`).
  LParen,
  /// A closing parenthesis (`)`).
  RParen,
  /// A spread (`...`).
  Spread,
  /// A colon (`:`).
  Colon,
  /// An equals sign (`=`).
  Equal,
  /// An at sign (`@`).
  At,
  /// An opening angle bracket (`<`).
  LAngle,
  /// A closing angle bracket (`>`).
  RAngle,
  /// An opening bracket (`[`).
  LBracket,
  /// A closing bracket (`]`).
  RBracket,
  /// An opening brace (`{`).
  LBrace,
  /// A closing brace (`}`).
  RBrace,
  /// An asterisk (`*`).
  Asterisk,
  /// A plus sign (`+`).
  Plus,
  /// A minus sign (`-`).
  Minus,
  /// A path separator (`::`).
  PathSeparator,
  /// A fat arrow (`=>`).
  FatArrow,
  /// A pipe (`|`).
  Pipe,
  /// An exclamation mark (`!`).
  Bang,
  /// An ampersand (`&`).
  Ampersand,
  /// A name.
  Name,
  /// A namespaced path.
  Path,
  /// An integer literal.
  IntValue,
  /// A floating-point literal.
  FloatValue,
  /// A string literal.
  StringValue,
  /// A boolean literal.
  BooleanValue,
  /// The `null` literal.
  NullValue,
  /// An enum path value.
  EnumValue,
  /// A variable reference (`$name`).
  VariableValue,
  /// A GraphQLX type reference.
  Type,
  /// A GraphQLX input value.
  InputValue,
  /// A constant GraphQLX input value.
  ConstInputValue,
  /// A GraphQLX import clause.
  ImportClause,
  /// An import member.
  ImportMember,
  /// The contextual `import` keyword.
  Import,
  /// The contextual `from` keyword.
  From,
  /// The contextual `as` keyword.
  As,
  /// A keyword with the given spelling.
  Keyword(&'static str),
}

/// An unexpected token with the found token kind and typed expectation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedToken<T, TK> {
  found: Option<T>,
  expected: TK,
}

impl<T, TK> UnexpectedToken<T, TK> {
  /// Creates an unexpected token error without a found token.
  #[inline]
  pub const fn new(expected: TK) -> Self {
    Self::maybe_found(None, expected)
  }

  /// Creates an unexpected token error from an optional found token.
  #[inline]
  pub const fn maybe_found(found: Option<T>, expected: TK) -> Self {
    Self { found, expected }
  }

  /// Creates an unexpected token error with a found token.
  #[inline]
  pub const fn with_found(found: T, expected: TK) -> Self {
    Self::maybe_found(Some(found), expected)
  }

  /// Returns the found token, if any.
  #[inline]
  pub const fn found(&self) -> Option<&T> {
    self.found.as_ref()
  }

  /// Returns the typed expectation.
  #[inline]
  pub const fn expected(&self) -> &TK {
    &self.expected
  }
}

/// The delimiter kind left unclosed by a GraphQLX production.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum Unclosed {
  /// Parentheses, missing `)`.
  Parentheses,
  /// A list, missing `]`.
  List,
  /// A brace-delimited object, set, map, or import list, missing `}`.
  Object,
  /// An angle-delimited set, map, or type-argument list, missing `>`.
  Angle,
}

/// The data carried by a GraphQLX parser error.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more lexer errors.
  Lexer(LexerErrors<Char, StateError>),
  /// A delimiter was not closed.
  Unclosed(Unclosed),
  /// A token did not satisfy its typed expectation.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// A production reached end of input unexpectedly.
  UnexpectedEnd(UnexpectedEnd<Exp>),
  /// A dialect-specific message not represented by a dedicated variant.
  Other(std::borrow::Cow<'static, str>),
  /// Retains the source type in this generic family even when the current
  /// vertical slice does not need a source-carrying diagnostic.
  #[from(skip)]
  Source(core::marker::PhantomData<S>),
}

/// A single GraphQLX parser error.
#[derive(Debug, Clone)]
pub struct Error<S, T, Char = char, Exp = Expectation, StateError = ()> {
  span: Span,
  data: ErrorData<S, T, Char, Exp, StateError>,
}

impl<S, T, Char, Exp, StateError> Error<S, T, Char, Exp, StateError> {
  /// Creates an error from its span and data.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<S, T, Char, Exp, StateError>) -> Self {
    Self { span, data }
  }

  /// Creates an unexpected-token error with an optional found token.
  #[inline]
  pub const fn maybe_unexpected_token(found: Option<T>, expected: Exp, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedToken(UnexpectedToken::maybe_found(found, expected)),
    )
  }

  /// Creates an unexpected-token error with a found token.
  #[inline]
  pub const fn unexpected_token(found: T, expected: Exp, span: Span) -> Self {
    Self::maybe_unexpected_token(Some(found), expected, span)
  }

  /// Creates an unexpected-end error with a typed expectation.
  #[inline]
  pub fn unexpected_end(expected: Exp, span: Span) -> Self
  where
    Exp: core::fmt::Debug,
  {
    Self::new(
      span,
      ErrorData::UnexpectedEnd(UnexpectedEnd::with_name(
        0,
        tokora::utils::CowStr::from_static("GraphQLX production"),
        expected,
      )),
    )
  }

  /// Creates an unclosed-list error.
  #[inline]
  pub const fn unclosed_list(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::List))
  }

  /// Creates an unclosed-parentheses error.
  #[inline]
  pub const fn unclosed_parentheses(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Parentheses))
  }

  /// Creates an unclosed-brace error.
  #[inline]
  pub const fn unclosed_object(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Object))
  }

  /// Creates an unclosed-angle error.
  #[inline]
  pub const fn unclosed_angle(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Angle))
  }

  /// Returns the error span.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the error data.
  #[inline]
  pub const fn data(&self) -> &ErrorData<S, T, Char, Exp, StateError> {
    &self.data
  }

  /// Consumes this error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<S, T, Char, Exp, StateError> {
    self.data
  }
}

type DefaultErrorsContainer<S, T, Char = char, Exp = Expectation, StateError = ()> =
  std::vec::Vec<Error<S, T, Char, Exp, StateError>>;

/// A collection of GraphQLX parser errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, Char = char, Exp = Expectation, StateError = ()>(
  DefaultErrorsContainer<S, T, Char, Exp, StateError>,
);

impl<S, T, Char, Exp, StateError> Default for Errors<S, T, Char, Exp, StateError> {
  #[inline]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<S, T, Exp, Char, StateError> From<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline]
  fn from(error: Error<S, T, Char, Exp, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, Char, Exp, StateError> Errors<S, T, Char, Exp, StateError> {
  /// Creates an empty error container with the requested capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<S, T, Char, Exp, StateError> IntoIterator for Errors<S, T, Char, Exp, StateError> {
  type Item = Error<S, T, Char, Exp, StateError>;
  type IntoIter = <DefaultErrorsContainer<S, T, Char, Exp, StateError> as IntoIterator>::IntoIter;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, Char, Exp, StateError> Extend<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline]
  fn extend<I: IntoIterator<Item = Error<S, T, Char, Exp, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

/// The GraphQLX dialect error keyed to a source slice and concrete syntactic
/// token kind.
pub type GraphqlxError<S> = Error<S, SyntacticTokenKind, char, Expectation>;

/// The GraphQLX dialect error container used by parser contexts.
pub type GraphqlxErrors<S> = Errors<S, SyntacticTokenKind, char, Expectation>;

#[inline]
fn expectation_from_token_kind(kind: SyntacticTokenKind) -> Expectation {
  match kind {
    SyntacticTokenKind::Identifier => Expectation::Name,
    SyntacticTokenKind::Int => Expectation::InputValue,
    SyntacticTokenKind::Float => Expectation::InputValue,
    SyntacticTokenKind::InlineString => Expectation::InlineString,
    SyntacticTokenKind::BlockString => Expectation::BlockString,
    SyntacticTokenKind::Dollar => Expectation::Dollar,
    SyntacticTokenKind::FatArrow => Expectation::FatArrow,
    SyntacticTokenKind::LAngle => Expectation::LAngle,
    SyntacticTokenKind::RAngle => Expectation::RAngle,
    SyntacticTokenKind::LParen => Expectation::LParen,
    SyntacticTokenKind::RParen => Expectation::RParen,
    SyntacticTokenKind::Spread => Expectation::Spread,
    SyntacticTokenKind::Colon => Expectation::Colon,
    SyntacticTokenKind::Equal => Expectation::Equal,
    SyntacticTokenKind::Asterisk => Expectation::Asterisk,
    SyntacticTokenKind::At => Expectation::At,
    SyntacticTokenKind::LBracket => Expectation::LBracket,
    SyntacticTokenKind::RBracket => Expectation::RBracket,
    SyntacticTokenKind::LBrace => Expectation::LBrace,
    SyntacticTokenKind::RBrace => Expectation::RBrace,
    SyntacticTokenKind::Pipe => Expectation::Pipe,
    SyntacticTokenKind::Bang => Expectation::Bang,
    SyntacticTokenKind::Ampersand => Expectation::Ampersand,
    SyntacticTokenKind::Plus => Expectation::Plus,
    SyntacticTokenKind::Minus => Expectation::Minus,
    SyntacticTokenKind::PathSeparator => Expectation::PathSeparator,
    _ => Expectation::Name,
  }
}

#[inline]
fn expectation_from_tokora(expected: Option<Expected<'_, SyntacticTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => expectation_from_token_kind(kind),
    _ => Expectation::Name,
  }
}

impl<'a, S, Lang: ?Sized>
  From<TokUnexpectedToken<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>>
  for GraphqlxErrors<S>
{
  #[inline]
  fn from(err: TokUnexpectedToken<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>) -> Self {
    let (span, found, expected) = err.into_components();
    match found {
      Some(token) => {
        GraphqlxError::unexpected_token(token.kind(), expectation_from_tokora(expected), span)
          .into()
      }
      None => {
        GraphqlxError::maybe_unexpected_token(None, expectation_from_tokora(expected), span).into()
      }
    }
  }
}

impl<'a, S, Lang: ?Sized>
  From<SeparatedError<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>>
  for GraphqlxErrors<S>
{
  #[inline]
  fn from(err: SeparatedError<'a, SyntacticToken<S>, SyntacticTokenKind, Span, Lang>) -> Self {
    Self::from(err.into_inner())
  }
}

impl<S, Kind: Clone, Lang: ?Sized> From<MissingToken<'_, Kind, usize, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: MissingToken<'_, Kind, usize, Lang>) -> Self {
    let offset = err.offset();
    GraphqlxError::new(
      Span::new(offset, offset),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing token")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<MissingSyntax<usize, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: MissingSyntax<usize, Lang>) -> Self {
    let offset = err.offset();
    GraphqlxError::new(
      Span::new(offset, offset),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing syntax")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<FullContainer<Span, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: FullContainer<Span, Lang>) -> Self {
    GraphqlxError::new(
      *err.span(),
      ErrorData::Other(std::borrow::Cow::Borrowed("container full")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<TooFew<Span, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: TooFew<Span, Lang>) -> Self {
    GraphqlxError::new(
      err.span(),
      ErrorData::Other(std::borrow::Cow::Borrowed("too few elements")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<UnexpectedEot<usize, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: UnexpectedEot<usize, Lang>) -> Self {
    let offset = err.offset();
    GraphqlxError::maybe_unexpected_token(None, Expectation::InputValue, Span::new(offset, offset))
      .into()
  }
}

impl<S> From<TokoraUnclosed<Bracket, Span, GraphQLX>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: TokoraUnclosed<Bracket, Span, GraphQLX>) -> Self {
    GraphqlxError::unclosed_list(err.span()).into()
  }
}

impl<S> From<TokoraUnclosed<Paren, Span, GraphQLX>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: TokoraUnclosed<Paren, Span, GraphQLX>) -> Self {
    GraphqlxError::unclosed_parentheses(err.span()).into()
  }
}

impl<S> From<TokoraUnclosed<Brace, Span, GraphQLX>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: TokoraUnclosed<Brace, Span, GraphQLX>) -> Self {
    GraphqlxError::unclosed_object(err.span()).into()
  }
}

impl<S> From<TokoraUnclosed<Angle, Span, GraphQLX>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: TokoraUnclosed<Angle, Span, GraphQLX>) -> Self {
    GraphqlxError::unclosed_angle(err.span()).into()
  }
}

impl<S, Char, StateError> From<LexerErrors<Char, StateError>> for GraphqlxErrors<S> {
  #[inline]
  fn from(_err: LexerErrors<Char, StateError>) -> Self {
    GraphqlxError::new(
      Span::new(0, 0),
      ErrorData::Other(std::borrow::Cow::Borrowed("lexer error")),
    )
    .into()
  }
}

#[cfg(test)]
mod tests;
