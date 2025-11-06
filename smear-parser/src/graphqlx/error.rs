use std::borrow::Cow;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  Lexed, Logos, Token, Tokenizer,
  chumsky::{
    self, DefaultExpected,
    error::{self, LabelError},
    util::{Maybe, MaybeRef},
  },
  error::{
    DefaultContainer, Invalid, InvalidBooleanLiteral, InvalidEnumValueLiteral, InvalidNullLiteral,
    Unclosed, UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen, UnexpectedEnd,
    UnexpectedEot, UnexpectedKeyword, UnexpectedToken, UnknownLexeme,
  },
  utils::{CharLen, Expected, Message, Span, Spanned},
};

use super::{
  SyntaxKind,
  syntax::{DirectiveLocationSyntax, FragmentTypePathSyntax, OperationTypeSyntax},
};

pub use crate::{
  hints::{ObjectFieldValueHint, VariableValueHint},
  lexer::graphqlx::error::LexerErrors,
};

/// A malformed fragment type path error.
pub type InvalidFragmentTypePath = Invalid<FragmentTypePathSyntax>;

/// An extra alias
pub type Extra<S, T, Char = char, Exp = SyntaxKind, StateError = ()> =
  logosky::chumsky::extra::Err<Errors<S, T, Char, Exp, StateError>>;

/// The data of a parser error.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Error<S, T, Char = char, Exp: 'static = SyntaxKind, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(Spanned<LexerErrors<Char, StateError>>),
  /// An enum value is invalid.
  InvalidEnumValue(InvalidEnumValueLiteral),
  /// A boolean value is invalid.
  InvalidBooleanValue(InvalidBooleanLiteral),
  /// A null value is invalid.
  InvalidNullValue(InvalidNullLiteral),
  /// A fragment type path is invalid.
  InvalidFragmentTypePath(InvalidFragmentTypePath),
  /// A list was not closed.
  UnclosedBracket(UnclosedBracket),
  /// A brace was not closed.
  UnclosedBrace(UnclosedBrace),
  /// An angle bracket was not closed.
  UnclosedAngle(UnclosedAngle),
  /// A parenthesis was not closed.
  UnclosedParen(UnclosedParen),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<'static, T, Exp>),
  /// An unexpected keyword was found.
  UnexpectedKeyword(UnexpectedKeyword<'static, S>),
  /// An unexpected end was found in a variable value.
  UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>),
  /// An unexpected end was found in an object field value.
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  /// An unknown directive location was found.
  UnknownDirectiveLocation(UnknownLexeme<Char, DirectiveLocationSyntax>),
  /// An unknown operation type was found.
  UnknownOperationType(UnknownLexeme<Char, OperationTypeSyntax>),
  /// An end of input was found.
  EndOfInput(UnexpectedEot),
  /// Some other error.
  Other(Spanned<Message>),
}

impl<S, T, Char, SyntaxKind: 'static, StateError> Error<S, T, Char, SyntaxKind, StateError> {
  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(found: T, expected: SyntaxKind, span: Span) -> Self {
    Self::UnexpectedToken(UnexpectedToken::with_found(
      span,
      found,
      Expected::One(expected),
    ))
  }

  /// Creates an unexpected end in variable value error.
  #[inline]
  pub const fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::UnexpectedEndOfVariableValue(UnexpectedEnd::with_name(
      span,
      Message::from_static("variable value"),
      hint,
    ))
  }

  /// Creates an unexpected keyword error.
  #[inline]
  pub const fn unexpected_keyword(found: S, expected_kw: &'static str, span: Span) -> Self {
    Self::UnexpectedKeyword(UnexpectedKeyword::new(
      span,
      found,
      Expected::One(expected_kw),
    ))
  }

  /// Creates an unclosed bracket error.
  #[inline]
  pub const fn unclosed_bracket(span: Span) -> Self {
    Self::UnclosedBracket(Unclosed::bracket(span))
  }

  /// Creates an unclosed brace error.
  #[inline]
  pub const fn unclosed_brace(span: Span) -> Self {
    Self::UnclosedBrace(Unclosed::brace(span))
  }

  /// Creates an unclosed angle bracket error.
  #[inline]
  pub const fn unclosed_angle(span: Span) -> Self {
    Self::UnclosedAngle(Unclosed::angle(span))
  }

  /// Creates an unclosed parenthesis error.
  #[inline]
  pub const fn unclosed_paren(span: Span) -> Self {
    Self::UnclosedParen(Unclosed::paren(span))
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::Lexer(Spanned::new(span, err))
  }

  /// Creates an unexpected end of input error.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::EndOfInput(UnexpectedEot::eot(span))
  }

  /// Creates an invalid fragment type path error.
  #[inline]
  pub const fn invalid_fragment_type_path(span: Span) -> Self {
    Self::InvalidFragmentTypePath(InvalidFragmentTypePath::with_knowledge(
      span,
      FragmentTypePathSyntax(()),
    ))
  }

  /// Creates an unknown directive location error.
  #[inline]
  pub const fn unknown_directive_location(span: Span) -> Self {
    Self::UnknownDirectiveLocation(UnknownLexeme::from_range_const(
      span,
      DirectiveLocationSyntax(()),
    ))
  }

  /// Creates an unknown operation type error.
  #[inline]
  pub const fn unknown_operation_type(span: Span) -> Self {
    Self::UnknownOperationType(UnknownLexeme::from_range_const(
      span,
      OperationTypeSyntax(()),
    ))
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(span: Span) -> Self {
    Self::InvalidEnumValue(InvalidEnumValueLiteral::enum_value(span))
  }

  /// Creates an invalid boolean value error.
  #[inline]
  pub const fn invalid_boolean_value(span: Span) -> Self {
    Self::InvalidBooleanValue(InvalidBooleanLiteral::boolean(span))
  }

  /// Creates an invalid null value error.
  #[inline]
  pub const fn invalid_null_value(span: Span) -> Self {
    Self::InvalidNullValue(InvalidNullLiteral::null(span))
  }

  /// Creates a other error.
  #[inline]
  pub fn other(span: Span, msg: impl Into<Cow<'static, str>>) -> Self {
    Self::Other(Spanned::new(span, Message::from(msg.into())))
  }

  /// Returns the span of the error.
  #[inline]
  pub fn span(&self) -> Span
  where
    Char: CharLen,
  {
    match self {
      Self::Lexer(spanned) => *spanned.span(),
      Self::InvalidEnumValue(e) => e.span(),
      Self::InvalidBooleanValue(e) => e.span(),
      Self::InvalidNullValue(e) => e.span(),
      Self::InvalidFragmentTypePath(e) => e.span(),
      Self::UnclosedBracket(unclosed) => unclosed.span(),
      Self::UnclosedBrace(unclosed) => unclosed.span(),
      Self::UnclosedAngle(unclosed) => unclosed.span(),
      Self::UnclosedParen(unclosed) => unclosed.span(),
      Self::UnexpectedToken(e) => e.span(),
      Self::UnexpectedKeyword(e) => e.span(),
      Self::UnexpectedEndOfVariableValue(e) => e.span(),
      Self::UnexpectedEndOfObjectFieldValue(e) => e.span(),
      Self::UnknownDirectiveLocation(e) => e.span(),
      Self::UnknownOperationType(e) => e.span(),
      Self::EndOfInput(e) => e.span(),
      Self::Other(e) => *e.span(),
    }
  }
}

/// A container for storing multiple parser errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, Char = char, Exp: 'static = SyntaxKind, StateError = ()>(
  DefaultContainer<Error<S, T, Char, Exp, StateError>>,
);

impl<S, T, Char, SyntaxKind, StateError> Default for Errors<S, T, Char, SyntaxKind, StateError> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(DefaultContainer::default())
  }
}

impl<S, T, Char, SyntaxKind, StateError> From<Error<S, T, Char, SyntaxKind, StateError>>
  for Errors<S, T, Char, SyntaxKind, StateError>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(error: Error<S, T, Char, SyntaxKind, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, Char, SyntaxKind, StateError> Errors<S, T, Char, SyntaxKind, StateError> {
  /// Create a new empty errors container with given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultContainer::with_capacity(capacity))
  }
}

impl<S, T, Char, SyntaxKind, StateError> IntoIterator
  for Errors<S, T, Char, SyntaxKind, StateError>
{
  type Item = Error<S, T, Char, SyntaxKind, StateError>;
  type IntoIter =
    <DefaultContainer<Error<S, T, Char, SyntaxKind, StateError>> as IntoIterator>::IntoIter;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, Char, SyntaxKind, StateError> Extend<Error<S, T, Char, SyntaxKind, StateError>>
  for Errors<S, T, Char, SyntaxKind, StateError>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn extend<I: IntoIterator<Item = Error<S, T, Char, SyntaxKind, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

impl<'a, S, T, Char, SyntaxKind, StateError>
  LabelError<'a, Tokenizer<'a, T>, DefaultExpected<'a, Lexed<'a, T>>>
  for Errors<S, T, Char, SyntaxKind, StateError>
where
  T: Token<'a>,
  SyntaxKind: From<<T as Token<'a>>::Kind>,
  T::Logos: Logos<'a, Error = LexerErrors<Char, StateError>>,
  <T::Logos as Logos<'a>>::Extras: Copy,
  Char: Clone,
  StateError: Clone,
{
  fn expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
    expected: E,
    found: Option<MaybeRef<'a, Lexed<'a, T>>>,
    span: logosky::utils::Span,
  ) -> Self {
    let mut errs = Self::default();

    // Helper to extract Lexed from found option
    let found_lexed = found.as_ref().map(|f| match f {
      MaybeRef::Ref(lexed) => *lexed,
      MaybeRef::Val(lexed) => lexed,
    });

    for exp in expected {
      let ed = match exp {
        DefaultExpected::Token(maybe) => {
          // Extract Lexed from Maybe wrapper
          let expected_lexed = match maybe {
            Maybe::Ref(lexed) => lexed,
            Maybe::Val(ref lexed) => lexed,
          };

          match expected_lexed {
            Lexed::Token(expected_tok) => {
              let expected_tok = expected_tok.as_ref().into_data();
              match found_lexed {
                None => Error::UnexpectedToken(UnexpectedToken::new(
                  span,
                  Expected::One(SyntaxKind::from(expected_tok.kind())),
                )),
                Some(Lexed::Token(found_tok)) => {
                  let found_span = found_tok.span();
                  let found_tok = found_tok.as_ref().into_data();
                  Error::UnexpectedToken(UnexpectedToken::with_found(
                    *found_span,
                    found_tok.clone(),
                    Expected::One(SyntaxKind::from(expected_tok.kind())),
                  ))
                }
                Some(Lexed::Error(err)) => Error::Lexer(Spanned::new(span, err.clone())),
              }
            }
            Lexed::Error(err) => Error::Lexer(Spanned::new(span, err.clone())),
          }
        }
        DefaultExpected::Any => Error::other(span, "expected any token"),
        DefaultExpected::SomethingElse => Error::other(span, "expected something else"),
        DefaultExpected::EndOfInput => Error::unexpected_end_of_input(span),
        _ => Error::other(span, "unknown expected"),
      };

      errs.push(ed);
    }

    errs
  }

  fn merge_expected_found<E: IntoIterator<Item = DefaultExpected<'a, Lexed<'a, T>>>>(
    mut self,
    expected: E,
    found: Option<MaybeRef<'a, Lexed<'a, T>>>,
    span: logosky::utils::Span,
  ) -> Self
  where
    Self: error::Error<'a, Tokenizer<'a, T>>,
  {
    // Create new errors from the expected/found combination
    let new_errors = Self::expected_found(expected, found, span);

    // Merge the new errors into self
    self.extend(new_errors);
    self
  }
}

impl<'a, S, T, Char, SyntaxKind, StateError> chumsky::error::Error<'a, Tokenizer<'a, T>>
  for Errors<S, T, Char, SyntaxKind, StateError>
where
  T: Token<'a>,
  T::Logos: Logos<'a, Error = LexerErrors<Char, StateError>>,
  <T::Logos as Logos<'a>>::Extras: Copy,
  Char: Clone,
  SyntaxKind: From<T::Kind>,
  StateError: Clone,
{
}
