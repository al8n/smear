//! The GraphQLx dialect error and the `From` glue that makes it a
//! [`ParseCtx`](crate::combinator::ParseCtx) error.
//!
//! The error family — [`Expectation`], [`Unclosed`], [`InvalidEnumValue`],
//! [`ErrorData`], [`Error`], and the [`Errors`] container — is copied from the
//! frozen `smear-parser` crate (`graphqlx/error.rs`), re-keyed to the source slice
//! `S`. [`GraphqlxError`] and [`GraphqlxErrors`] pin `S` and the token to the
//! concrete GraphQLx syntactic token, the one dialect-specific instantiation the
//! entry runners and tests name.
//!
//! The atoms speak tokora's generic error families ([`UnexpectedToken`],
//! [`SeparatedError`], the container/`TooFew` families, [`UnexpectedEot`], and the
//! lexer errors). tokora 0.3.0's `ComposableEmitter` bundle and `From*` blankets
//! shrink the old impl set to the handful of `From` conversions the atoms' bounds
//! actually demand; each lands here so [`Fatal`](tokora::emitter::Fatal) over
//! [`GraphqlxErrors`] is a complete [`ParseCtx`](crate::combinator::ParseCtx) over
//! both `str` and `[u8]` syntactic lexers — proven by the module's compile test.

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::{
  graphqlx::{error::LexerErrors, syntactic::SyntacticToken},
  tokora::error::UnexpectedEnd,
};
use smear_scaffold::hints::{
  EnumTypeExtensionHint, InputObjectTypeExtensionHint, InterfaceTypeExtensionHint,
  ObjectFieldValueHint, ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint,
};
use tokora::{
  SimpleSpan as Span,
  error::{
    UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken as TokUnexpectedToken},
  },
  utils::CowStr,
};

// Only the `tests` submodule references `super::GraphQLx`; a plain top-level import
// reads as unused under `-Dwarnings` in non-test builds, so gate it to `test`.
#[cfg(test)]
use super::GraphQLx;

/// Hints for parsing a variable value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]
#[non_exhaustive]
pub enum VariableValueHint {
  /// A name was expected.
  #[display("name")]
  Name,
  /// A dollar `$` was expected.
  #[display("dollar")]
  Dollar,
}

/// Expectations for the GraphQLx parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Expectation {
  /// An inline string was expected.
  InlineString,
  /// A block string was expected.
  BlockString,
  /// A `$` was expected.
  Dollar,
  /// A `(` was expected.
  LParen,
  /// A `)` was expected.
  RParen,
  /// A `...` was expected.
  Spread,
  /// A `:` was expected.
  Colon,
  /// A `=` was expected.
  Equal,
  /// An `@` was expected.
  At,
  /// A `<` was expected.
  LAngle,
  /// A `>` was expected.
  RAngle,
  /// A `[` was expected.
  LBracket,
  /// A `]` was expected.
  RBracket,
  /// A `{` was expected.
  LBrace,
  /// A `}` was expected.
  RBrace,
  /// A `*` was expected.
  Asterisk,
  /// A `+` was expected.
  Plus,
  /// A `-` was expected.
  Minus,
  /// A `::` was expected.
  PathSeparator,
  /// A `=>` was expected.
  FatArrow,
  /// A `|` was expected.
  Pipe,
  /// A `!` was expected.
  Bang,
  /// An `&` was expected.
  Ampersand,

  /// Const input value was expected.
  ConstInputValue,
  /// Input value was expected.
  InputValue,
  /// Fragment name was expected.
  FragmentName,
  /// An identifier was expected.
  Identifier,
  /// An operation name was expected.
  OperationName,
  /// A directive location was expected.
  DirectiveLocation,
  /// Either a fragment spread or an inline fragment was expected.
  FragmentSpreadOrInlineFragment,
  /// A number was expected.
  IntValue,
  /// A boolean was expected.
  BooleanValue,
  /// A float was expected.
  FloatValue,
  /// A null value was expected.
  NullValue,
  /// An enum value was expected.
  EnumValue,
  /// A string value was expected.
  StringValue,
  /// A keyword was expected.
  Keyword(&'static str),
}

/// An unexpected token error carrying the found token and the expectation.
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

  /// Creates a new unexpected token error.
  #[inline]
  pub const fn maybe_found(found: Option<T>, expected: TK) -> Self {
    Self { found, expected }
  }

  /// Creates a new unexpected token error with a found token.
  #[inline]
  pub const fn with_found(found: T, expected: TK) -> Self {
    Self::maybe_found(Some(found), expected)
  }

  /// Returns the found token, if any.
  #[inline]
  pub const fn found(&self) -> Option<&T> {
    self.found.as_ref()
  }

  /// Returns the expectation.
  #[inline]
  pub const fn expected(&self) -> &TK {
    &self.expected
  }
}

/// An unexpected keyword error.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedKeyword<S> {
  found: S,
  expected: &'static str,
}

impl<S> UnexpectedKeyword<S> {
  /// Creates a new unexpected keyword error.
  #[inline]
  pub const fn new(found: S, expected_kw: &'static str) -> Self {
    Self {
      found,
      expected: expected_kw,
    }
  }

  /// Returns the found keyword.
  #[inline]
  pub const fn found(&self) -> &S {
    &self.found
  }

  /// Returns the name of the expected keyword.
  #[inline]
  pub const fn expected(&self) -> &'static str {
    self.expected
  }
}

/// Represents an unclosed delimiter in GraphQLx source.
#[derive(Debug, Copy, Clone, IsVariant)]
pub enum Unclosed {
  /// An unclosed bracket (missing `]`).
  Bracket,
  /// An unclosed brace (missing `}`).
  Brace,
}

/// The value name the spec excludes from an enum value (`true`/`false`/`null`).
#[derive(Debug, Copy, Clone, IsVariant)]
pub enum InvalidEnumValue {
  /// The enum value is spelled `true`.
  True,
  /// The enum value is spelled `false`.
  False,
  /// The enum value is spelled `null`.
  Null,
}

/// The data of a parser error.
#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more errors from the lexer.
  Lexer(LexerErrors<Char, StateError>),
  /// An integer value could not be parsed due to overflow.
  #[from(skip)]
  IntOverflow(S),
  /// A floating point value could not be parsed due to overflow.
  #[from(skip)]
  FloatOverflow(S),
  /// An enum value is invalid (`true`/`false`/`null`).
  #[from(skip)]
  InvalidEnumValue(InvalidEnumValue),
  /// A boolean value is invalid.
  #[from(skip)]
  InvalidBooleanValue(S),
  /// A null value is invalid.
  #[from(skip)]
  InvalidNullValue(S),
  /// A fragment type path is invalid.
  #[from(skip)]
  InvalidFragmentTypePath,
  /// A list or object was not closed.
  Unclosed(Unclosed),
  /// An unexpected token was found.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// An unexpected keyword was found.
  UnexpectedKeyword(UnexpectedKeyword<S>),
  /// An unexpected end was found in a variable value.
  UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>),
  /// An unexpected end was found in an object field value.
  UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>),
  /// An unknown directive location was found.
  #[from(skip)]
  UnknownDirectiveLocation(S),
  /// An unknown operation type was found.
  #[from(skip)]
  UnknownOperationType(S),
  /// An unexpected end was found in an object type extension.
  UnexpectedEndOfObjectExtension(UnexpectedEnd<ObjectTypeExtensionHint>),
  /// An unexpected end was found in an interface type extension.
  UnexpectedEndOfInterfaceExtension(UnexpectedEnd<InterfaceTypeExtensionHint>),
  /// An unexpected end was found in an enum type extension.
  UnexpectedEndOfEnumExtension(UnexpectedEnd<EnumTypeExtensionHint>),
  /// An unexpected end was found in an input object type extension.
  UnexpectedEndOfInputObjectExtension(UnexpectedEnd<InputObjectTypeExtensionHint>),
  /// An unexpected end was found in a union type extension.
  UnexpectedEndOfUnionExtension(UnexpectedEnd<UnionTypeExtensionHint>),
  /// An unexpected end was found in a schema extension.
  UnexpectedEndOfSchemaExtension(UnexpectedEnd<SchemaExtensionHint>),
  /// An end of input was found.
  EndOfInput,
  /// Some other error.
  Other(std::borrow::Cow<'static, str>),
}

/// A parser error.
#[derive(Debug, Clone)]
pub struct Error<S, T, Char = char, Exp = Expectation, StateError = ()> {
  span: Span,
  data: ErrorData<S, T, Char, Exp, StateError>,
}

impl<S, T, Char, Exp, StateError> Error<S, T, Char, Exp, StateError> {
  /// Creates a new error.
  #[inline]
  pub const fn new(span: Span, data: ErrorData<S, T, Char, Exp, StateError>) -> Self {
    Self { span, data }
  }

  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(found: T, expected: Exp, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedToken(UnexpectedToken::with_found(found, expected)),
    )
  }

  /// Creates an unexpected end in variable value error.
  #[inline]
  pub const fn unexpected_end_of_variable_value(hint: VariableValueHint, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfVariableValue(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("variable value"),
        hint,
      )),
    )
  }

  /// Creates an unexpected keyword error.
  #[inline]
  pub const fn unexpected_keyword(found: S, expected_kw: &'static str, span: Span) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedKeyword(UnexpectedKeyword::new(found, expected_kw)),
    )
  }

  /// Creates an unexpected end in object field value error.
  #[inline]
  pub const fn unexpected_end_of_object_field_value(
    hint: ObjectFieldValueHint,
    span: Span,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectFieldValue(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("object field value"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in object type extension error.
  #[inline]
  pub const fn unexpected_end_of_object_extension(
    span: Span,
    hint: ObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfObjectExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("object type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in interface type extension error.
  #[inline]
  pub const fn unexpected_end_of_interface_extension(
    span: Span,
    hint: InterfaceTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInterfaceExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("interface type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in enum type extension error.
  #[inline]
  pub const fn unexpected_end_of_enum_extension(span: Span, hint: EnumTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfEnumExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("enum type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in input object type extension error.
  #[inline]
  pub const fn unexpected_end_of_input_object_extension(
    span: Span,
    hint: InputObjectTypeExtensionHint,
  ) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfInputObjectExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("input object type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in union type extension error.
  #[inline]
  pub const fn unexpected_end_of_union_extension(span: Span, hint: UnionTypeExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfUnionExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("union type extension"),
        hint,
      )),
    )
  }

  /// Creates an unexpected end in schema extension error.
  #[inline]
  pub const fn unexpected_end_of_schema_extension(span: Span, hint: SchemaExtensionHint) -> Self {
    Self::new(
      span,
      ErrorData::UnexpectedEndOfSchemaExtension(UnexpectedEnd::with_name(
        0,
        CowStr::from_static("schema extension"),
        hint,
      )),
    )
  }

  /// Creates an unclosed bracket error.
  #[inline]
  pub const fn unclosed_bracket(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Bracket))
  }

  /// Creates an unclosed brace error.
  #[inline]
  pub const fn unclosed_brace(span: Span) -> Self {
    Self::new(span, ErrorData::Unclosed(Unclosed::Brace))
  }

  /// Creates an error from a lexer error.
  #[inline]
  pub const fn from_lexer_errors(err: LexerErrors<Char, StateError>, span: Span) -> Self {
    Self::new(span, ErrorData::Lexer(err))
  }

  /// Creates an invalid fragment type path error.
  #[inline]
  pub const fn invalid_fragment_type_path(span: Span) -> Self {
    Self::new(span, ErrorData::InvalidFragmentTypePath)
  }

  /// Creates an invalid enum value error.
  #[inline]
  pub const fn invalid_enum_value(value: InvalidEnumValue, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidEnumValue(value))
  }

  /// Creates an invalid boolean value error.
  #[inline]
  pub const fn invalid_boolean_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidBooleanValue(value))
  }

  /// Creates an invalid null value error.
  #[inline]
  pub const fn invalid_null_value(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::InvalidNullValue(value))
  }

  /// Creates an unknown directive location error.
  #[inline]
  pub const fn unknown_directive_location(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownDirectiveLocation(value))
  }

  /// Creates an unknown operation type error.
  #[inline]
  pub const fn unknown_operation_type(value: S, span: Span) -> Self {
    Self::new(span, ErrorData::UnknownOperationType(value))
  }

  /// Creates an unexpected end of input error.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::EndOfInput)
  }

  /// Returns the span of the error.
  #[inline]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the data of the error.
  #[inline]
  pub const fn data(&self) -> &ErrorData<S, T, Char, Exp, StateError> {
    &self.data
  }

  /// Returns a mutable reference to the data of the error.
  #[inline]
  pub const fn data_mut(&mut self) -> &mut ErrorData<S, T, Char, Exp, StateError> {
    &mut self.data
  }

  /// Consumes the error and returns its data.
  #[inline]
  pub fn into_data(self) -> ErrorData<S, T, Char, Exp, StateError> {
    self.data
  }
}

type DefaultErrorsContainer<S, T, Char = char, Exp = Expectation, StateError = ()> =
  std::vec::Vec<Error<S, T, Char, Exp, StateError>>;

/// A container for storing multiple parser errors.
#[derive(Debug, Clone, From, Into, Deref, DerefMut, AsMut, AsRef)]
pub struct Errors<S, T, Char = char, Exp = Expectation, StateError = ()>(
  DefaultErrorsContainer<S, T, Char, Exp, StateError>,
);

impl<S, T, Char, Exp, StateError> Default for Errors<S, T, Char, Exp, StateError> {
  #[inline(always)]
  fn default() -> Self {
    Self(DefaultErrorsContainer::default())
  }
}

impl<S, T, Exp, Char, StateError> From<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline(always)]
  fn from(error: Error<S, T, Char, Exp, StateError>) -> Self {
    Self(core::iter::once(error).collect())
  }
}

impl<S, T, Char, Exp, StateError> Errors<S, T, Char, Exp, StateError> {
  /// Creates a new empty errors container with the given capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(DefaultErrorsContainer::with_capacity(capacity))
  }
}

impl<S, T, Char, Exp, StateError> IntoIterator for Errors<S, T, Char, Exp, StateError> {
  type Item = Error<S, T, Char, Exp, StateError>;
  type IntoIter = <DefaultErrorsContainer<S, T, Char, Exp, StateError> as IntoIterator>::IntoIter;

  #[inline(always)]
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<S, T, Char, Exp, StateError> Extend<Error<S, T, Char, Exp, StateError>>
  for Errors<S, T, Char, Exp, StateError>
{
  #[inline(always)]
  fn extend<I: IntoIterator<Item = Error<S, T, Char, Exp, StateError>>>(&mut self, iter: I) {
    self.0.extend(iter);
  }
}

/// The GraphQLx dialect error, keyed to the source slice `S` and the concrete
/// GraphQLx syntactic token.
pub type GraphqlxError<S> = Error<S, SyntacticToken<S>, char, Expectation>;

/// The GraphQLx dialect error container — the error type a
/// [`ParseCtx`](crate::combinator::ParseCtx) over a GraphQLx lexer emits.
pub type GraphqlxErrors<S> = Errors<S, SyntacticToken<S>, char, Expectation>;

// ---- `From` glue -----------------------------------------------------------
//
// The atoms bound `ErrorOf: From<…>` over tokora's generic error families; each
// conversion below lands the dialect side. Kinds and the `Lang` marker stay
// generic so the bound is satisfied for `Lang = GraphQLx` (and any other marker a
// production pins); the found token and slice are the dialect's concrete types.
// Diagnostics-carrying families (missing separator/element, full container, too
// few, lexer errors) map to `Other` exactly as the frozen glue did — these are
// emitter paths a `Vec` container and a fail-fast context rarely reach.

impl<'a, S, Kind: Clone, Lang: ?Sized>
  From<TokUnexpectedToken<'a, SyntacticToken<S>, Kind, Span, Lang>> for GraphqlxErrors<S>
{
  #[inline]
  fn from(err: TokUnexpectedToken<'a, SyntacticToken<S>, Kind, Span, Lang>) -> Self {
    let (span, found, _expected) = err.into_components();
    match found {
      Some(token) => GraphqlxError::unexpected_token(token, Expectation::Identifier, span).into(),
      None => GraphqlxError::unexpected_end_of_input(span).into(),
    }
  }
}

impl<'a, S, Kind: Clone, Lang: ?Sized> From<SeparatedError<'a, SyntacticToken<S>, Kind, Span, Lang>>
  for GraphqlxErrors<S>
{
  #[inline]
  fn from(err: SeparatedError<'a, SyntacticToken<S>, Kind, Span, Lang>) -> Self {
    Self::from(err.into_inner())
  }
}

impl<S, Kind: Clone, Lang: ?Sized> From<MissingToken<'_, Kind, usize, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: MissingToken<'_, Kind, usize, Lang>) -> Self {
    let off = err.offset();
    GraphqlxError::new(
      Span::new(off, off),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing token")),
    )
    .into()
  }
}

impl<S, Lang: ?Sized> From<MissingSyntax<usize, Lang>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: MissingSyntax<usize, Lang>) -> Self {
    let off = err.offset();
    GraphqlxError::new(
      Span::new(off, off),
      ErrorData::Other(std::borrow::Cow::Borrowed("missing element")),
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
    let off = err.offset();
    GraphqlxError::unexpected_end_of_input(Span::new(off, off)).into()
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
mod tests {
  use smear_lexer::graphqlx::syntactic::SyntacticLexer;
  use tokora::{FatalContext, Lexer, ParseCtx};

  use super::{GraphQLx, GraphqlxErrors};

  fn assert_parse_ctx<'inp, L, Ctx>()
  where
    L: Lexer<'inp>,
    Ctx: ParseCtx<'inp, L, GraphQLx>,
  {
  }

  #[test]
  fn graphqlx_error_is_parse_ctx_over_str_and_slice() {
    // `Fatal<GraphqlxErrors<slice>>` is a complete `ParseCtx` over both source
    // representations — the whole point of the `From` glue above.
    assert_parse_ctx::<
      SyntacticLexer<'_, str>,
      FatalContext<'_, SyntacticLexer<'_, str>, GraphqlxErrors<&str>, GraphQLx>,
    >();
    assert_parse_ctx::<
      SyntacticLexer<'_, [u8]>,
      FatalContext<'_, SyntacticLexer<'_, [u8]>, GraphqlxErrors<&[u8]>, GraphQLx>,
    >();
  }
}
