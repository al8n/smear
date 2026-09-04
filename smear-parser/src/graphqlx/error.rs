//! GraphQLx parser errors and Tokora conversion glue.
//!
//! The GraphQLx parser owns its error family so it remains usable with the
//! `graphqlx` feature alone; it deliberately does not depend on the separately
//! feature-gated GraphQL dialect module.

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into, IsVariant, TryUnwrap, Unwrap};
use smear_lexer::{
  graphqlx::{
    error::{LexerErrorData, LexerErrors},
    syntactic::{SyntacticToken, SyntacticTokenKind},
  },
  tokora::error::UnexpectedEnd,
};
use tokora::{
  Lexer, SimpleSpan as Span,
  emitter::FromUnclosed,
  error::{
    MaybeTerminal, Unclosed as TokoraUnclosed, UnexpectedEot,
    syntax::{FullContainer, MissingSyntax, TooFew},
    token::{MissingToken, SeparatedError, UnexpectedToken as TokUnexpectedToken},
  },
  utils::Expected,
};

/// Typed expectations reported by GraphQLx productions.
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
  /// A GraphQLx type reference.
  Type,
  /// A GraphQLx input value.
  InputValue,
  /// A constant GraphQLx input value.
  ConstInputValue,
  /// A GraphQLx import clause.
  ImportClause,
  /// An import member.
  ImportMember,
  /// The contextual `import` keyword.
  Import,
  /// The contextual `from` keyword.
  From,
  /// The contextual `as` keyword.
  As,
  /// An operation type or the `fragment` keyword.
  ///
  /// The position after a leading description on an executable definition. Only
  /// the keyworded alternatives carry a `Description?` — `OperationDefinition :
  /// Description? OperationType …` and `FragmentDefinition : Description?
  /// fragment …` — so the shorthand `OperationDefinition : SelectionSet` cannot
  /// follow one.
  OperationTypeOrFragment,
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

/// The delimiter kind left unclosed by a GraphQLx production.
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

/// The data carried by a GraphQLx parser error.
///
/// `#[non_exhaustive]` under the rule `7b9b293` wrote down and `387dc34` applied to the sibling
/// `graphql::error::ErrorData`: the attribute belongs on vocabulary this crate owns, not on
/// vocabulary a specification enumerates. This list is ours — [`Other`](Self::Other) exists
/// because it is open, [`Source`](Self::Source) exists only to retain `S` and could not appear in
/// a spec-transcribed enum at all, and the `From` impls below already funnel diagnostics into
/// `Other` wherever no dedicated variant fits.
#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum ErrorData<S, T, Char = char, Exp = Expectation, StateError = ()> {
  /// One or more lexer errors.
  Lexer(LexerErrors<Char, StateError>),
  /// A delimiter was not closed.
  Unclosed(Unclosed),
  /// A token did not satisfy its typed expectation.
  UnexpectedToken(UnexpectedToken<T, Exp>),
  /// A production reached end of input unexpectedly.
  UnexpectedEnd(UnexpectedEnd<Exp>),
  /// An end of input that stands in for a **terminal scanner stop** — smear issue #177.
  ///
  /// GraphQL's twin carries the reasoning and the shape: a resource-limit trip, or the poison
  /// boundary it latches, surfaces at a committed leaf as the end-of-input error that leaf would
  /// have raised anyway, and tokora's
  /// [`is_terminal`](tokora::error::UnexpectedEnd::is_terminal) mark is the only thing separating
  /// the two. The dialect difference is which spelling this sits beside: GraphQLx has no
  /// `EndOfInput` variant, deliberately (see [`Error::unexpected_end_of_input`]), so a *genuine*
  /// end of input arrives here as an [`UnexpectedToken`](Self::UnexpectedToken) with no found
  /// token.
  ///
  /// **That deliberate absence is not an argument against this variant, and its own note says
  /// why.** What it refuses is a second spelling of *one* event across two layers of one dialect —
  /// a difference no consumer could act on and every consumer would have to handle. A scanner stop
  /// is a different event: it answers the opposite question about whether the parse may continue,
  /// both layers reach it through the same conversion, and its reader is the parser itself rather
  /// than a consumer. One spelling per event is what that note asks for, and this is the second
  /// event.
  #[from(skip)]
  TerminalEndOfInput,
  /// The parse tried to descend one level past the nesting budget.
  ///
  /// A variant rather than an [`Other`](Self::Other) message for the reason GraphQL's twin
  /// records: smear issue #169's repair makes the **parser itself** ask whether an error is this
  /// one, at every document root that catches, and a string discriminator is one edit away from
  /// answering `false` forever with nothing failing. This enum's [`MaybeTerminal`] arm is the
  /// reader, and it answers off the variant — beside tokora's resource-trip counter, which the
  /// substrate's crate-private `root_turn` reads and no error value can discard (smear issue
  /// #178).
  #[from(skip)]
  NestingLimitExceeded,
  /// The input's durable token budget refused an item, so the parse stopped reading.
  ///
  /// GraphQL's twin carries the reasoning. In short: tokora refuses the item **silently** — the
  /// refusal has no diagnostic channel at all — so without this variant a document that ran out
  /// of `LosslessLimits::max_tokens` comes back indistinguishable from one that parsed.
  #[from(skip)]
  TokenBudgetExhausted,
  /// A dialect-specific message not represented by a dedicated variant.
  Other(std::borrow::Cow<'static, str>),
  /// Retains the source type in this generic family even when the current
  /// vertical slice does not need a source-carrying diagnostic.
  #[from(skip)]
  Source(core::marker::PhantomData<S>),
}

/// Which of these errors **end the parse** rather than being something to recover from.
///
/// GraphQL's twin carries the reasoning and the rule each arm came from. This dialect's variant
/// set is narrower and every arm is the same ruling: [`UnexpectedEnd`] delegates,
/// [`NestingLimitExceeded`](Self::NestingLimitExceeded),
/// [`TokenBudgetExhausted`](Self::TokenBudgetExhausted) and
/// [`TerminalEndOfInput`](Self::TerminalEndOfInput) are always terminal, and
/// [`Lexer`](Self::Lexer) is terminal when it holds a `State` refusal — the arm tokora's rule warns
/// catches people, and the one a scanner trip lands on unmarked when the emitter rejects its
/// diagnostic. That arm answers only for the keying that can hold the batch; the syntactic
/// container pins `StateError = ()`, so its `From<LexerErrors>` reads the same `State` variant and
/// routes to [`TerminalEndOfInput`](Self::TerminalEndOfInput) instead.
///
/// GraphQLx has **no `EndOfInput` variant**, deliberately (see
/// [`Error::unexpected_end_of_input`]): a genuine end of input arrives as `UnexpectedToken` with
/// no found token, which is affirmatively a grammar rejection. The residual GraphQL used to record
/// — an end of input that could be a stop, on a variant that could not say so — took a different
/// shape here for that reason, and smear issue #177 closed both with the same split: the
/// conversion routes on tokora's mark and the marked case is its own variant, which is the one
/// added above.
impl<S, T, Char, Exp, StateError> MaybeTerminal for ErrorData<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    // Wildcard-free ON PURPOSE: a variant added without an arm is an `E0004` here.
    match self {
      Self::Lexer(errors) => errors
        .iter()
        .any(|e| matches!(e.data(), LexerErrorData::State(_))),
      Self::NestingLimitExceeded => true,
      // A budget the input has already refused against is never cleared by more input: the tally
      // is monotone, no `Checkpoint` carries it and no public mutator lowers it.
      Self::TokenBudgetExhausted => true,
      // Neither is a scanner stop, and this arm is what stops the conversion's routing from being
      // discarded one layer later.
      Self::TerminalEndOfInput => true,
      Self::UnexpectedEnd(e) => e.is_terminal(),
      Self::Unclosed(_) | Self::UnexpectedToken(_) | Self::Other(_) | Self::Source(_) => false,
    }
  }
}

/// Delegated to the payload: the span says where, not whether.
impl<S, T, Char, Exp, StateError> MaybeTerminal for Error<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    self.data.is_terminal()
  }
}

/// `any`, because a container ends the parse if anything in it does.
impl<S, T, Char, Exp, StateError> MaybeTerminal for Errors<S, T, Char, Exp, StateError> {
  fn is_terminal(&self) -> bool {
    self.0.iter().any(MaybeTerminal::is_terminal)
  }
}

/// A single GraphQLx parser error.
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
        tokora::utils::CowStr::from_static("GraphQLx production"),
        expected,
      )),
    )
  }

  /// Creates a parser-frame nesting refusal.
  ///
  /// `span` is empty and sits at the parse's committed end: a refused frame has consumed nothing
  /// of its own, so there is no lexeme to point at. This is the producer
  /// [`ErrorData::NestingLimitExceeded`] is reached through, generic over every parameter of this
  /// family — unlike `lossless_error_impls!`'s
  /// [`FromNestingLimit`](crate::lossless::depth::FromNestingLimit) impl, which is pinned to the
  /// lossless keying.
  #[inline]
  pub const fn nesting_limit_exceeded(span: Span) -> Self {
    Self::new(span, ErrorData::NestingLimitExceeded)
  }

  /// Creates a durable token-budget refusal.
  ///
  /// `span` is empty and sits at the parse's committed end: tokora drops the refused item where
  /// it stands and publishes no span for it, so there is no lexeme to point at. This is the
  /// producer [`ErrorData::TokenBudgetExhausted`] is reached through, generic over every
  /// parameter of this family — unlike `lossless_error_impls!`'s
  /// [`FromTokenBudget`](crate::lossless::depth::FromTokenBudget) impl, which is pinned to the
  /// lossless keying.
  #[inline]
  pub const fn token_budget_exhausted(span: Span) -> Self {
    Self::new(span, ErrorData::TokenBudgetExhausted)
  }

  /// Creates the end-of-input error that stands in for a **terminal scanner stop** — smear issue
  /// #177.
  ///
  /// The producer [`ErrorData::TerminalEndOfInput`] is reached through: the two
  /// `From<UnexpectedEot>` conversions — this dialect's syntactic one and the one
  /// `lossless_error_impls!` generates — and the variant census, which may not name a variant
  /// directly. Generic over every parameter of this family, unlike
  /// [`unexpected_end_of_input`](Self::unexpected_end_of_input), which names an [`Expectation`]
  /// and therefore cannot be.
  ///
  /// `span` is empty and sits where the ordinary end of input would have been reported: tokora
  /// builds the marked value at the same offset as the plain one, because the stop *is* the leaf's
  /// end of input as far as position goes — what differs is whether more input could ever change
  /// the answer.
  #[inline]
  pub const fn terminal_end_of_input(span: Span) -> Self {
    Self::new(span, ErrorData::TerminalEndOfInput)
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

impl<S, T, Char, StateError> Error<S, T, Char, Expectation, StateError> {
  /// Creates the end-of-input error a production raises when it has no particular expectation
  /// to name.
  ///
  /// # Why this is a second impl block, and why it is not `ErrorData::UnexpectedEnd`
  ///
  /// It names an [`Expectation`] value, so it cannot live in the `Exp`-generic block above.
  ///
  /// And it is spelled as an [`UnexpectedToken`] with no found token rather than as an
  /// [`UnexpectedEnd`], because **that is what this dialect's own `UnexpectedEot` conversion
  /// already produces** — the syntactic layer reports the identical event as
  /// `maybe_unexpected_token(None, Expectation::InputValue, …)`, and the lossless layer is
  /// compared against the syntactic one, input by input, by the acceptance-parity gate. Two
  /// spellings of one event across two layers of one dialect is a difference no consumer could
  /// act on and every consumer would have to handle.
  ///
  /// GraphQL's error family has a dedicated expectation-free `ErrorData::EndOfInput` and this one
  /// deliberately does not gain a copy: adding a variant here would give GraphQLx two spellings
  /// where it currently has one, and only the *lossless* half would use the new one.
  ///
  /// **[`ErrorData::TerminalEndOfInput`] is not that copy** — smear issue #177. It spells a
  /// different *event*: an end of input that stands in for a terminal scanner stop, which answers
  /// the opposite question about whether the parse may continue. Both layers reach it, through the
  /// same routing on tokora's own mark, so the count this paragraph protects is unchanged — one
  /// spelling per event, in both layers. [`Error::terminal_end_of_input`] is its constructor and
  /// `smear/tests/lossless_x_errors.rs` pins the two layers together on both events.
  #[inline]
  pub const fn unexpected_end_of_input(span: Span) -> Self {
    Self::maybe_unexpected_token(None, Expectation::InputValue, span)
  }
}

type DefaultErrorsContainer<S, T, Char = char, Exp = Expectation, StateError = ()> =
  std::vec::Vec<Error<S, T, Char, Exp, StateError>>;

/// A collection of GraphQLx parser errors.
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

/// The GraphQLx dialect error keyed to a source slice and concrete syntactic
/// token kind.
pub type GraphqlxError<S> = Error<S, SyntacticTokenKind, char, Expectation>;

/// The GraphQLx dialect error container used by parser contexts.
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
    // `#[allow]` and not a deletion. `SyntacticTokenKind` is `#[non_exhaustive]`, which forced this
    // arm while the lexer was a separate crate; with the crates merged (#83) the compiler can see
    // every variant from here and calls it unreachable. The arm stays because deleting it would
    // turn "a new token kind falls back to `Name`" into "a new token kind fails to compile" — a
    // real change to this function's contract, and this merge is a relocation. Revisiting that
    // choice is follow-up work, not part of the move.
    #[allow(unreachable_patterns)]
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

// The end-of-input conversion is written `Set`-generic so the one impl covers both
// members tokora's `FromTokenErrors` bundle names: the default `&'static str` set the
// `_or_stop` family raises, and the `&'static [Kind]` classification table a committed
// dispatch driver feeds straight into the diagnostic.
//
// IT ROUTES ON THE FLAG — smear issue #177, and GraphQL's twin carries the note. The offset is
// identical on both arms, so this `if` is the whole of what keeps a scanner stop distinguishable
// from an end of input; reading only `offset()` here landed both on a variant whose
// `MaybeTerminal` arm answers `false`.
impl<S, Lang: ?Sized, Set: Clone + 'static> From<UnexpectedEot<usize, Lang, Set>>
  for GraphqlxErrors<S>
{
  #[inline]
  fn from(err: UnexpectedEot<usize, Lang, Set>) -> Self {
    let offset = err.offset();
    let span = Span::new(offset, offset);
    if err.is_terminal() {
      GraphqlxError::terminal_end_of_input(span).into()
    } else {
      GraphqlxError::maybe_unexpected_token(None, Expectation::InputValue, span).into()
    }
  }
}

// One impl absorbs every delimiter pair. `Unclosed` carries the pair's name as data, so
// the pair is discriminated at run time on `name_ref` rather than by a `From` impl per
// pair; `Delimiter` is generic, so the catch-all arm is mandatory. GraphQLx opens all four
// built-in pairs, so the catch-all only covers a user-defined pair the grammar never uses.
impl<'inp, S, L, Lang: ?Sized> FromUnclosed<'inp, L, Lang> for GraphqlxErrors<S>
where
  L: Lexer<'inp, Span = Span>,
{
  #[inline]
  fn from_unclosed<Delimiter>(err: TokoraUnclosed<Delimiter, Span, Lang>) -> Self {
    let span = err.span();
    match err.name_ref() {
      "[]" => GraphqlxError::unclosed_list(span).into(),
      "()" => GraphqlxError::unclosed_parentheses(span).into(),
      "{}" => GraphqlxError::unclosed_object(span).into(),
      "<>" => GraphqlxError::unclosed_angle(span).into(),
      _ => GraphqlxError::new(
        span,
        ErrorData::Other(std::borrow::Cow::Borrowed("unclosed delimiter")),
      )
      .into(),
    }
  }
}

// THE BATCH IS INSPECTED, NOT ERASED — smear issue #177, Codex round 1.
//
// This is the conversion tokora reaches when an emitter **rejects** a lexer diagnostic: the scan
// propagates the emitter's own error, built from the lexer error, and no `UnexpectedEnd` is
// constructed anywhere on that path — so nothing is marked and there is nothing for the
// end-of-input routing above to keep. Landing the whole batch on `Other`, whose `MaybeTerminal`
// arm is unconditionally `false`, therefore made a rejecting host's `Errors::is_terminal()` answer
// `false` on a tripped `smear_lexer::limits` budget.
//
// `LexerErrorData::State` is the scanner's own stop kind and the only one of the ten that is: the
// other nine are built from a lexeme the **grammar** rejected — a malformed literal, an unknown
// character, an unterminated `...`, invalid UTF-8 — and each is recoverable. `State` is the
// tracker refusing, which no further input clears. It is the same variant the
// [`Lexer`](ErrorData::Lexer) arm keys on one layer up, so the two readings of a state refusal
// cannot drift apart.
//
// # Why `TerminalEndOfInput` and not a payload-carrying variant of its own
//
// A `State` refusal means the scanner has stopped and this is the end of what it will hand over.
// That is the event `TerminalEndOfInput` already names, and a consumer reading `is_terminal()`
// must not be able to tell *who built the value* — tokora at a committed leaf when the emitter
// accepted the diagnostic, or smear here when it rejected — because the fact about the document is
// the same.
//
// A variant carrying the batch is not available at this keying anyway, which is what made this
// impl erase in the first place: it is generic over the batch's `Char` and `StateError` while the
// container pins its own (`char`, `()`), so `ErrorData::Lexer` does not typecheck here. The
// lossless conversion `lossless_error_impls!` generates is pinned to `LexerErrors<char,
// LimitExceeded>` and therefore keeps the payload; that one already routed correctly and is
// unchanged. A new variant here would have to erase the payload too — it would be
// `TerminalEndOfInput` under another name, with a census row and a parity row bought for nothing.
//
// The span is the batch's first, on **both** arms. `Span::new(0, 0)` pointed every lexer error at
// byte zero, which is a position the error is not at; a `State` error minted through
// `From<StateError> for LexerErrors` genuinely carries `0..0`, and that is the value's own
// statement rather than this conversion's.
impl<S, Char, StateError> From<LexerErrors<Char, StateError>> for GraphqlxErrors<S> {
  #[inline]
  fn from(err: LexerErrors<Char, StateError>) -> Self {
    let span = err.iter().next().map_or(Span::new(0, 0), |e| e.span());
    if err
      .iter()
      .any(|e| matches!(e.data(), LexerErrorData::State(_)))
    {
      return GraphqlxError::terminal_end_of_input(span).into();
    }
    GraphqlxError::new(
      span,
      ErrorData::Other(std::borrow::Cow::Borrowed("lexer error")),
    )
    .into()
  }
}

#[cfg(test)]
mod tests;
