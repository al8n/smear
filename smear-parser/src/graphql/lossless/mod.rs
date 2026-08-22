//! The GraphQL lossless parser suite: a `rowan` CST over the trivia-surfacing lexer.

use smear_lexer::graphql::{
  error::LexerErrors,
  lossless::{LosslessLexer, LosslessToken, LosslessTokenKind},
};
use tokora::{
  InputRef,
  // Aliased because this module also declares a `Lexer` **type alias** — the name the shared
  // macros in `crate::lossless` reach this dialect's lexer by — and a trait and a type alias share
  // one namespace.
  Lexer as TokoraLexer,
  SimpleSpan,
  Source,
  error::RecursionLimitReached,
  input::Descent,
  state::tracker::LimitExceeded,
  utils::Expected,
};

use crate::{
  combinator::ErrorOf,
  graphql::{
    GraphQL,
    error::{Error as DialectError, ErrorData, Errors as DialectErrors, Expectation},
  },
  lossless::depth::FromNestingLimit,
};

pub use crate::graphql::kinds::GraphQLLang;

/// A GraphQL lossless syntax node.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLLang>;
/// A GraphQL lossless syntax token.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLLang>;
/// A node-or-token in the GraphQL lossless CST.
pub type SyntaxElement = rowan::SyntaxElement<GraphQLLang>;

/// The source slice emitted by [`GraphqlLosslessLexer`] for `Src`.
///
/// Declared before the lexer alias, not after it as in `syntactic/`, because the lexer alias is
/// written in terms of this one.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessSlice<'inp, Src: Source<usize> + ?Sized> =
  <Src as Source<usize>>::Slice<'inp>;

/// The concrete lexer used by GraphQL lossless productions over `Src`.
///
/// **Note the argument.** `LosslessLexer<'a, S = &'a str> = LogosLexer<'a, LosslessToken<S>>`
/// (`smear-lexer/src/graphql/lossless/mod.rs:16`) is parameterised by the **slice** type, not by
/// the source type — unlike `SyntacticLexer`, which takes the source. Writing
/// `LosslessLexer<'inp, Src>` here compiles into a lexer over the wrong token and then fails far
/// away, at the first `Lexer<'inp>` obligation.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessLexer<'inp, Src: Source<usize> + ?Sized> =
  LosslessLexer<'inp, GraphqlLosslessSlice<'inp, Src>>;

/// The concrete token emitted by [`GraphqlLosslessLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessToken<'inp, Src: Source<usize> + ?Sized> =
  LosslessToken<GraphqlLosslessSlice<'inp, Src>>;

/// The parser error emitted by a GraphQL lossless production.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessError<'inp, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlLosslessLexer<'inp, Src>: TokoraLexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
= ErrorOf<'inp, GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>;

/// A mutable GraphQL lossless parser input over `Src` and parser context `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlLosslessInput<'inp, 'input, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlLosslessLexer<'inp, Src>: TokoraLexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
= InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>;

// ---------------------------------------------------------------------------------------------
// The eight names the shared macros in `crate::lossless` reach this dialect by.
//
// Aliases, not renames: the `GraphqlLossless*` spellings stay, because ~200 signatures use them
// and renaming those is a diff that buys nothing and hides everything else. What these buy is
// that `lossless_production!` and `lossless_drivers!` can be written once, over `Input`, `Error`,
// `Token`, `Lexer`, `LexerState`, `Brand`, `TokenKind` and `Keyword`, and a second dialect adopts
// them by declaring the same eight.
// ---------------------------------------------------------------------------------------------

/// This dialect's tokora grammar brand.
pub type Brand = GraphQL;

/// This dialect's lossless token-kind vocabulary.
pub type TokenKind = LosslessTokenKind;

/// This dialect's contextual-keyword projection — the door a production reads a keyword's
/// *spelling* through, since every one of them arrives as an ordinary identifier token.
pub type Keyword = smear_lexer::graphql::ContextualKeyword;

/// [`GraphqlLosslessLexer`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Lexer<'inp, Src: Source<usize> + ?Sized> = GraphqlLosslessLexer<'inp, Src>;

/// The resource budget this dialect's lossless lex runs under — the Logos `Extras`, and the
/// [`Lexer::State`](tokora::Lexer::State) the shared production bundle pins.
///
/// Pinned rather than left as a projection because a production has to *read* it: the nesting
/// ceiling a parse was configured with lives here, and this module's `descend` hands it to the
/// parser-frame budget. Over a generic `Src` the projection has nothing to normalize against
/// without the equality, exactly as the token's `Kind` does not.
pub type LexerState = smear_lexer::limits::LosslessLimits;

/// [`GraphqlLosslessToken`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Token<'inp, Src: Source<usize> + ?Sized> = GraphqlLosslessToken<'inp, Src>;

/// [`GraphqlLosslessInput`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Input<'inp, 'input, Src: Source<usize> + ?Sized, Ctx> =
  GraphqlLosslessInput<'inp, 'input, Src, Ctx>;

/// [`GraphqlLosslessError`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Error<'inp, Src: Source<usize> + ?Sized, Ctx> = GraphqlLosslessError<'inp, Src, Ctx>;

/// Enters one level of parser recursion under the budget the door installed.
///
/// A pass-through to [`crate::lossless::depth::descend`], which is where the reasoning lives, and
/// it is deliberately still a per-dialect function after losing its body. It used to read
/// `inp.state().max_nesting_depth()` and hand the number down, because the substrate may not name
/// `smear-lexer`; the ceiling now arrives as the parse's own
/// [`RecursionLimiter`](tokora::state::recursion_tracker::RecursionLimiter), installed once at the
/// door, so there is no number left to read on this side of the line. What the wrapper still buys
/// is that every production writes one call against this dialect's concrete input, error and
/// brand, so a bound that stops holding is reported here rather than at twenty-eight call sites.
///
/// **Bind the guard for the whole frame** — `let mut frame = descend(inp)?; let inp = &mut *frame;`
/// — because dropping it early releases the level before the recursion it was taken for, which
/// type-checks and silently reinstates the unbounded descent.
#[inline]
pub(crate) fn descend<'r, 'inp, 'input, Src, Ctx>(
  inp: &'r mut GraphqlLosslessInput<'inp, 'input, Src, Ctx>,
) -> Result<
  Descent<'r, 'inp, 'input, GraphqlLosslessLexer<'inp, Src>, Ctx, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: tokora::Token<'inp, Kind = TokenKind>
    + tokora::lexer::FromLogos<'inp>
    + Clone
    + tokora::utils::DowncastRef<Keyword>,
  GraphqlLosslessLexer<'inp, Src>: TokoraLexer<
      'inp,
      Token = GraphqlLosslessToken<'inp, Src>,
      Span = SimpleSpan,
      Offset = usize,
      State = LexerState,
    >,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>:
    From<RecursionLimitReached<usize, GraphQL>> + FromNestingLimit,
{
  crate::lossless::depth::descend(inp)
}

/// One error value a GraphQL lossless parse can record.
///
/// The dialect's own error, **re-keyed** to the lossless token kind and the lossless lexer's
/// state error. `GraphqlError` cannot serve: it pins `SyntacticTokenKind`, which has no image
/// for a trivia token, so a lossless "unexpected token" would have to invent a kind for the
/// comment or the newline it found.
pub type GraphqlLosslessErrorValue<S> =
  DialectError<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

/// The error container the lossless driver pins for `Verbose`.
///
/// Productions never name this: they name [`GraphqlLosslessError`], which projects the error
/// out of the parse context, and constrain it with `From<…>` bounds. Only
/// [`runner::parse_document`] — the one place a concrete context is chosen — has to say which
/// container that is.
pub type GraphqlLosslessErrors<S> =
  DialectErrors<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

crate::lossless::lossless_error_impls! {
  errors       = GraphqlLosslessErrors;
  value        = GraphqlLosslessErrorValue;
  token        = LosslessToken;
  kind         = LosslessTokenKind;
  lexer_errors = LexerErrors;
  error_data   = ErrorData;
  expectation  = expectation_of;
  unclosed     = {
    "[]" => unclosed_list,
    "()" => unclosed_parentheses,
    "{}" => unclosed_object,
  };
}

/// The lossless twin of `error.rs`'s `expectation_from_token_kind`.
///
/// `ErrorData::UnexpectedToken` has one expectation slot, so a multi-kind or absent expectation
/// falls back to `Name` exactly as the syntactic mapping does. Every trivia kind falls back too:
/// no production expects a comment or a newline — trivia is skipped into the tree by the atom
/// set, never demanded — so those arms are unreachable in practice rather than merely lossy.
fn expectation_of(expected: Option<Expected<'_, LosslessTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => match kind {
      LosslessTokenKind::Identifier => Expectation::Name,
      LosslessTokenKind::Int => Expectation::IntValue,
      LosslessTokenKind::Float => Expectation::FloatValue,
      LosslessTokenKind::InlineString => Expectation::InlineString,
      LosslessTokenKind::BlockString => Expectation::BlockString,
      LosslessTokenKind::Dollar => Expectation::Dollar,
      LosslessTokenKind::LParen => Expectation::LParen,
      LosslessTokenKind::RParen => Expectation::RParen,
      LosslessTokenKind::Spread => Expectation::Spread,
      LosslessTokenKind::Colon => Expectation::Colon,
      LosslessTokenKind::Equal => Expectation::Equal,
      LosslessTokenKind::At => Expectation::At,
      LosslessTokenKind::LBracket => Expectation::LBracket,
      LosslessTokenKind::RBracket => Expectation::RBracket,
      LosslessTokenKind::LBrace => Expectation::LBrace,
      LosslessTokenKind::RBrace => Expectation::RBrace,
      LosslessTokenKind::Pipe => Expectation::Pipe,
      LosslessTokenKind::Bang => Expectation::Bang,
      LosslessTokenKind::Ampersand => Expectation::Ampersand,
      _ => Expectation::Name,
    },
    _ => Expectation::Name,
  }
}

// The two production macros — `lossless_production!` and `lossless_drivers!` — are
// `crate::lossless::macros`', shared with every other dialect, and each production module imports
// them by path. They used to be defined here, immediately above these `mod` declarations, because
// a `macro_rules!` is in scope for a child module only if it is declared before that module's
// `mod` item; **that ordering constraint is gone** and nothing below depends on the order of
// anything above it.
pub mod ast;
pub mod coverage;
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod kind_map;
pub mod project;
pub mod recover;
pub mod runner;
pub mod selection;
pub mod trivia;
pub mod ty;
pub mod value;

// THE SUBSTRATE'S VERDICT MACHINERY IS DRIVEN FROM HERE — smear PR #189, round 5. Four cells call
// `lossless::depth`'s `root_turn`, `RootStop` and `drain_unless_stopped` directly, which is the
// only way to ask which term of a root's stop is alone on which population and what the drain does
// with a failure no turn judged. They lived in `smear/tests/nesting_depth.rs` until that machinery
// went `pub(crate)` — an integration test is a separate crate and sees `pub` and nothing else — so
// they had to come in-crate, and this dialect is where they belong rather than beside the code
// they drive: every cell pins GraphQL's lexer, its `Lang` marker and `smear_lexer::limits`, and a
// `mod tests` under `lossless/` would have put all of that inside the dialect-generic substrate.
// Gate 6 (`smear/tests/lossless_isolation.rs`) is the instrument that says so, and it reddened on
// exactly that. A dialect assembly driving the substrate is what the Lego rule is *for*; the
// substrate naming a dialect is what it forbids. No `feature = "graphql"` on the gate — this file
// is only compiled when that feature is on.
#[cfg(test)]
mod tests;

// The three document roots this suite parses, all at the same module level and all
// `fn(&str) -> Parse`: the mixed one (`parse_document`), the SDL-only one and the executable-only
// one. A consumer picks a root here, once, rather than parsing the mixed form and filtering the
// tree.
pub use runner::{
  Parse, parse_document, parse_document_with_limits, parse_executable_document,
  parse_executable_document_with_limits, parse_type_system_document,
  parse_type_system_document_with_limits, profile,
};

// Beside the roots, and named for the symmetry: `parse_document(src) -> Parse`,
// `project(&parse, src) -> Result<Document, _>`; `parse_executable_document(src) -> Parse`,
// `project_executable_document(&parse, src) -> Result<ExecutableDocument, _>`; and the same pair
// again at the SDL root. Every root has a projection, and each single-half root has a recovering
// one beside it. `Recovery` is the substrate's — it says how much of a tree the recovering door
// could see and nothing about a dialect — so it is re-exported here rather than copied.
pub use crate::lossless::project::Recovery;
pub use project::{
  ProjectError, ProjectErrorKind, project, project_executable_document,
  project_executable_document_recovered, project_type_system_document,
  project_type_system_document_recovered,
};
