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
    DefaultContainer, ErrorContainer, Invalid, InvalidBooleanLiteral, InvalidEnumValueLiteral, InvalidNullLiteral,
    Unclosed, UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen,
    UnexpectedEot, UnexpectedKeyword, UnexpectedToken, UnknownLexeme, IncompleteSyntax,
  },
  utils::{CharLen, Expected, Message, Span, Spanned},
};

use super::SyntaxKind as Exp;

use crate::graphql::syntax::*;

pub use crate::{
  error::MissingDollarTokenError,
  graphql::SyntaxKind,
  lexer::graphql::error::LexerErrors,
};

pub use core::num::{IntErrorKind, ParseFloatError, ParseIntError};

/// A malformed fragment name error.
pub type InvalidFragmentName = Invalid<FragmentNameSyntax>;

// ============================================================================
// Incomplete Syntax Type Aliases
// ============================================================================

/// Incomplete named type syntax
pub type IncompleteNamedTypeSyntax = IncompleteSyntax<NamedTypeSyntax>;
/// Incomplete list type syntax
pub type IncompleteListTypeSyntax = IncompleteSyntax<ListTypeSyntax>;
/// Incomplete scalar type definition syntax
pub type IncompleteScalarTypeDefinitionSyntax = IncompleteSyntax<ScalarTypeDefinitionSyntax>;
/// Incomplete object type definition syntax
pub type IncompleteObjectTypeDefinitionSyntax = IncompleteSyntax<ObjectTypeDefinitionSyntax>;
/// Incomplete interface type definition syntax
pub type IncompleteInterfaceTypeDefinitionSyntax = IncompleteSyntax<InterfaceTypeDefinitionSyntax>;
/// Incomplete union type definition syntax
pub type IncompleteUnionTypeDefinitionSyntax = IncompleteSyntax<UnionTypeDefinitionSyntax>;
/// Incomplete enum type definition syntax
pub type IncompleteEnumTypeDefinitionSyntax = IncompleteSyntax<EnumTypeDefinitionSyntax>;
/// Incomplete input object type definition syntax
pub type IncompleteInputObjectTypeDefinitionSyntax = IncompleteSyntax<InputObjectTypeDefinitionSyntax>;
/// Incomplete scalar type extension syntax
pub type IncompleteScalarTypeExtensionSyntax = IncompleteSyntax<ScalarTypeExtensionSyntax>;
/// Incomplete object type extension syntax
pub type IncompleteObjectTypeExtensionSyntax = IncompleteSyntax<ObjectTypeExtensionSyntax>;
/// Incomplete interface type extension syntax
pub type IncompleteInterfaceTypeExtensionSyntax = IncompleteSyntax<InterfaceTypeExtensionSyntax>;
/// Incomplete union type extension syntax
pub type IncompleteUnionTypeExtensionSyntax = IncompleteSyntax<UnionTypeExtensionSyntax>;
/// Incomplete enum type extension syntax
pub type IncompleteEnumTypeExtensionSyntax = IncompleteSyntax<EnumTypeExtensionSyntax>;
/// Incomplete input object type extension syntax
pub type IncompleteInputObjectTypeExtensionSyntax = IncompleteSyntax<InputObjectTypeExtensionSyntax>;
/// Incomplete field definition syntax
pub type IncompleteFieldDefinitionSyntax = IncompleteSyntax<FieldDefinitionSyntax>;
/// Incomplete input value definition syntax
pub type IncompleteInputValueDefinitionSyntax = IncompleteSyntax<InputValueDefinitionSyntax>;
/// Incomplete directive definition syntax
pub type IncompleteDirectiveDefinitionSyntax = IncompleteSyntax<DirectiveDefinitionSyntax>;
/// Incomplete directive syntax
pub type IncompleteDirectiveSyntax = IncompleteSyntax<DirectiveSyntax>;
/// Incomplete schema definition syntax
pub type IncompleteSchemaDefinitionSyntax = IncompleteSyntax<SchemaDefinitionSyntax>;
/// Incomplete schema extension syntax
pub type IncompleteSchemaExtensionSyntax = IncompleteSyntax<SchemaExtensionSyntax>;
/// Incomplete root operation type definition syntax
pub type IncompleteRootOperationTypeDefinitionSyntax = IncompleteSyntax<RootOperationTypeDefinitionSyntax>;
/// Incomplete enum value definition syntax
pub type IncompleteEnumValueDefinitionSyntax = IncompleteSyntax<EnumValueDefinitionSyntax>;
/// Incomplete named operation definition syntax
pub type IncompleteNamedOperationDefinitionSyntax = IncompleteSyntax<NamedOperationDefinitionSyntax>;
/// Incomplete fragment definition syntax
pub type IncompleteFragmentDefinitionSyntax = IncompleteSyntax<FragmentDefinitionSyntax>;
/// Incomplete fragment spread syntax
pub type IncompleteFragmentSpreadSyntax = IncompleteSyntax<FragmentSpreadSyntax>;
/// Incomplete inline fragment syntax
pub type IncompleteInlineFragmentSyntax = IncompleteSyntax<InlineFragmentSyntax>;
/// Incomplete field syntax
pub type IncompleteFieldSyntax = IncompleteSyntax<FieldSyntax>;
/// Incomplete variable definition syntax
pub type IncompleteVariableDefinitionSyntax = IncompleteSyntax<VariableDefinitionSyntax>;
/// Incomplete argument syntax
pub type IncompleteArgumentSyntax = IncompleteSyntax<ArgumentSyntax>;
/// Incomplete object field syntax
pub type IncompleteObjectFieldSyntax = IncompleteSyntax<ObjectFieldSyntax>;
/// Incomplete type condition syntax
pub type IncompleteTypeConditionSyntax = IncompleteSyntax<TypeConditionSyntax>;

/// An extra alias
pub type Extra<S, T, Char = char, SyntaxKind = Exp, StateError = ()> =
  logosky::chumsky::extra::Err<Errors<S, T, Char, SyntaxKind, StateError>>;

/// The data of a parser error.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Error<S, T, Char = char, SyntaxKind: 'static = Exp, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(Spanned<LexerErrors<Char, StateError>>),
  /// An enum value is invalid.
  InvalidEnumValue(InvalidEnumValueLiteral),
  /// A boolean value is invalid.
  InvalidBooleanValue(InvalidBooleanLiteral),
  /// A null value is invalid.
  InvalidNullValue(InvalidNullLiteral),
  /// A fragment name is invalid.
  InvalidFragmentName(InvalidFragmentName),
  /// A bracket was not closed.
  UnclosedBracket(UnclosedBracket),
  /// A brace was not closed.
  UnclosedBrace(UnclosedBrace),
  /// An angle bracket was not closed.
  UnclosedAngle(UnclosedAngle),
  /// A parenthesis was not closed.
  UnclosedParen(UnclosedParen),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<'static, T, SyntaxKind>),
  /// An unexpected keyword was found.
  UnexpectedKeyword(UnexpectedKeyword<'static, S>),
  /// An unexpected end was found in a variable value.
  IncompleteVariableValue(IncompleteSyntax<VariableValueSyntax>),
  /// An unknown directive location was found.
  UnknownDirectiveLocation(UnknownLexeme<Char, DirectiveLocationSyntax>),
  /// An unknown operation type was found.
  UnknownOperationType(UnknownLexeme<Char, OperationTypeSyntax>),
  /// An unexpected end was found in an object type extension.
  IncompleteObjectExtension(IncompleteSyntax<ObjectTypeExtensionSyntax>),
  /// An unexpected end was found in an interface type extension.
  IncompleteInterfaceExtension(IncompleteSyntax<InterfaceTypeExtensionSyntax>),
  /// An unexpected end was found in an enum type extension.
  IncompleteEnumExtension(IncompleteSyntax<EnumTypeExtensionSyntax>),
  /// An unexpected end was found in an input object type extension.
  IncompleteInputObjectExtension(IncompleteSyntax<InputObjectTypeExtensionSyntax>),
  /// An unexpected end was found in a union type extension.
  IncompleteUnionExtension(IncompleteSyntax<UnionTypeExtensionSyntax>),
  /// An unexpected end was found in a schema extension.
  IncompleteSchemaExtension(IncompleteSyntax<SchemaExtensionSyntax>),
  /// An end of input was found.
  EndOfInput(UnexpectedEot),
  /// Some other error.
  Other(Spanned<Message>),
}

impl<S, T, Char, SyntaxKind, StateError> Error<S, T, Char, SyntaxKind, StateError> {
  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(span: Span, found: T, expected: SyntaxKind) -> Self {
    Self::UnexpectedToken(UnexpectedToken::expected_one_with_found(
      span,
      found,
      expected
    ))
  }

  /// Creates an unexpected keyword error.
  #[inline]
  pub const fn unexpected_keyword(span: Span, found: S, expected_kw: &'static str) -> Self {
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
  pub const fn from_lexer_errors(span: Span, err: LexerErrors<Char, StateError>) -> Self {
    Self::Lexer(Spanned::new(span, err))
  }

  /// Creates an unexpected end of input error.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::EndOfInput(UnexpectedEot::eot(span))
  }

  /// Creates an invalid fragment name error.
  #[inline]
  pub const fn invalid_fragment_name(span: Span) -> Self {
    Self::InvalidFragmentName(InvalidFragmentName::with_knowledge(
      span,
      FragmentNameSyntax(()),
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

  /// Creates an unknown error in object type extension.
  #[inline]
  pub const fn unexpected_end_of_object_extension(components: IncompleteSyntax<ObjectTypeExtensionSyntax>) -> Self {
    Self::IncompleteObjectExtension(components)
  }

  /// Creates an unknown error in interface type extension.
  #[inline]
  pub const fn unexpected_end_of_interface_extension(components: IncompleteSyntax<InterfaceTypeExtensionSyntax>) -> Self {
    Self::IncompleteInterfaceExtension(components)
  }

  /// Creates an unknown error in enum type extension.
  #[inline]
  pub const fn unexpected_end_of_enum_extension(components: IncompleteSyntax<EnumTypeExtensionSyntax>) -> Self {
    Self::IncompleteEnumExtension(components)
  }

  /// Creates an unknown error in input object type extension.
  #[inline]
  pub const fn unexpected_end_of_input_object_extension(components: IncompleteSyntax<InputObjectTypeExtensionSyntax>) -> Self {
    Self::IncompleteInputObjectExtension(components)
  }

  /// Creates an unknown error in union type extension.
  #[inline]
  pub const fn unexpected_end_of_union_extension(components: IncompleteSyntax<UnionTypeExtensionSyntax>) -> Self {
    Self::IncompleteUnionExtension(components)
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
      Self::InvalidFragmentName(e) => e.span(),
      Self::UnclosedBracket(unclosed) => unclosed.span(),
      Self::UnclosedBrace(unclosed) => unclosed.span(),
      Self::UnclosedAngle(unclosed) => unclosed.span(),
      Self::UnclosedParen(unclosed) => unclosed.span(),
      Self::UnexpectedToken(e) => e.span(),
      Self::UnexpectedKeyword(e) => e.span(),
      Self::IncompleteVariableValue(e) => e.span(),
      Self::UnknownDirectiveLocation(e) => e.span(),
      Self::UnknownOperationType(e) => e.span(),
      Self::EndOfInput(e) => e.span(),
      Self::Other(e) => *e.span(),
      Self::IncompleteObjectExtension(e) => e.span(),
      Self::IncompleteInterfaceExtension(e) => e.span(),
      Self::IncompleteEnumExtension(e) => e.span(),
      Self::IncompleteInputObjectExtension(e) => e.span(),
      Self::IncompleteUnionExtension(e) => e.span(),
      Self::IncompleteSchemaExtension(e) => e.span(),
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
    IntoIterator::into_iter(self.0)
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
                  Error::unexpected_token(
                    *found_span,
                    found_tok.clone(),
                    SyntaxKind::from(expected_tok.kind())
                  )
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
