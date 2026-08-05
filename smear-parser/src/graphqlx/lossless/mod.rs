//! The GraphQLx lossless parser suite: a `rowan` CST over the trivia-surfacing lexer.
//!
//! The second assembly over [`crate::lossless`]. Everything grammar-independent — the kind-space
//! contract, the trivia atoms, the `Parse` surface, the coverage shims, the typed-wrapper macro —
//! is the substrate's and is shared with GraphQL verbatim; what is here is the GraphQLx-specific
//! half: which lexer, which kind space, which error container, and which delimiter pairs.
//!
//! # The four places this dialect is genuinely different, not merely renamed
//!
//! - **A fourth balanced pair.** GraphQLx's `<>` is depth-counted by the lexer alongside `()`,
//!   `[]` and `{}` (`smear-lexer/src/graphqlx/syntactic/mod.rs:807-814`), so the `unclosed` list
//!   below has four entries and [`crate::graphqlx::error::Unclosed`] a fourth variant.
//! - **A wider image space.** Seven images GraphQL has no counterpart for — `<`, `>`, `::`, `=>`,
//!   `*`, `+`, `-` — which is why [`crate::graphqlx::kinds::SyntaxKind`] cannot be GraphQL's space
//!   with a tail appended, and why [`kind_map`] is not GraphQL's mapper with arms added.
//! - **A different expectation vocabulary.** The two dialects' `Expectation` enums agree on their
//!   first nine variants and neither is a superset, so this module's private `expectation_of` is
//!   hand-written per dialect. It is the one member the design's measurement showed survives every
//!   shape of every shared-prefix scheme.
//! - **`#[non_exhaustive]` on the lexer's token and kind enums.** GraphQL's are not marked, so its
//!   [`kind_map`] can be exhaustive; GraphQLx's are, and `#[non_exhaustive]` binds across the
//!   *crate* boundary, so a match written here needs a wildcard arm no matter what. See
//!   [`kind_map`]'s own docs for what stands in for the compiler's exhaustiveness check.

use smear_lexer::graphqlx::{
  error::LexerErrors,
  lossless::{LosslessLexer, LosslessToken, LosslessTokenKind},
};
use tokora::{
  InputRef,
  // Aliased because this module also declares a `Lexer` **type alias** — the name the shared
  // macros in `crate::lossless` reach this dialect's lexer by — and a trait and a type alias share
  // one namespace.
  Lexer as TokoraLexer,
  Source,
  state::tracker::LimitExceeded,
  utils::Expected,
};

use crate::{
  combinator::ErrorOf,
  graphqlx::{
    GraphQLx,
    error::{Error as DialectError, ErrorData, Errors as DialectErrors, Expectation},
  },
};

pub use crate::graphqlx::kinds::GraphQLxLang;

/// A GraphQLx lossless syntax node.
pub type SyntaxNode = rowan::SyntaxNode<GraphQLxLang>;
/// A GraphQLx lossless syntax token.
pub type SyntaxToken = rowan::SyntaxToken<GraphQLxLang>;
/// A node-or-token in the GraphQLx lossless CST.
pub type SyntaxElement = rowan::SyntaxElement<GraphQLxLang>;

/// The source slice emitted by [`GraphqlxLosslessLexer`] for `Src`.
///
/// Declared before the lexer alias, not after it as in `syntactic/`, because the lexer alias is
/// written in terms of this one.
#[allow(type_alias_bounds)]
pub type GraphqlxLosslessSlice<'inp, Src: Source<usize> + ?Sized> =
  <Src as Source<usize>>::Slice<'inp>;

/// The concrete lexer used by GraphQLx lossless productions over `Src`.
///
/// **Note the argument.** `LosslessLexer<'a, S = &'a str> = LogosLexer<'a, LosslessToken<S>>`
/// (`smear-lexer/src/graphqlx/lossless/mod.rs:17`) is parameterised by the **slice** type, not by
/// the source type — unlike `SyntacticLexer`, which takes the source. Writing
/// `LosslessLexer<'inp, Src>` here compiles into a lexer over the wrong token and then fails far
/// away, at the first `Lexer<'inp>` obligation.
#[allow(type_alias_bounds)]
pub type GraphqlxLosslessLexer<'inp, Src: Source<usize> + ?Sized> =
  LosslessLexer<'inp, GraphqlxLosslessSlice<'inp, Src>>;

/// The concrete token emitted by [`GraphqlxLosslessLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlxLosslessToken<'inp, Src: Source<usize> + ?Sized> =
  LosslessToken<GraphqlxLosslessSlice<'inp, Src>>;

/// The parser error emitted by a GraphQLx lossless production.
#[allow(type_alias_bounds)]
pub type GraphqlxLosslessError<'inp, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlxLosslessLexer<'inp, Src>: TokoraLexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
= ErrorOf<'inp, GraphqlxLosslessLexer<'inp, Src>, Ctx, GraphQLx>;

/// A mutable GraphQLx lossless parser input over `Src` and parser context `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlxLosslessInput<'inp, 'input, Src: Source<usize> + ?Sized, Ctx>
where
  GraphqlxLosslessLexer<'inp, Src>: TokoraLexer<'inp>,
  Ctx: tokora::ParseContext<'inp, GraphqlxLosslessLexer<'inp, Src>, GraphQLx>,
= InputRef<'inp, 'input, GraphqlxLosslessLexer<'inp, Src>, Ctx, GraphQLx>;

// ---------------------------------------------------------------------------------------------
// The seven names the shared macros in `crate::lossless` reach this dialect by.
//
// Aliases, not renames, exactly as GraphQL's are: the `GraphqlxLossless*` spellings stay, because
// they are what every signature in this suite reads. What these buy is that
// `lossless_production!` and `lossless_drivers!` — written once, over `Input`, `Error`, `Token`,
// `Lexer`, `Brand`, `TokenKind` and `Keyword` — apply here with nothing but a `dialect =` header.
//
// The macros take **two fixed idents** (`graphqlx::lossless`), not a `$dialect:path`. That is not
// a style choice: a `:path` fragment is an opaque AST node, so `$dialect::Input<…>` parses as an
// associated item, and a `$($d:ident)::+` run cannot nest inside the repetition that walks the
// productions. The consequence is the one this module satisfies by existing: a dialect lives at
// exactly `crate::<a>::<b>`.
// ---------------------------------------------------------------------------------------------

/// This dialect's tokora grammar brand.
pub type Brand = GraphQLx;

/// This dialect's lossless token-kind vocabulary.
pub type TokenKind = LosslessTokenKind;

/// This dialect's contextual-keyword projection — the door a production reads a keyword's
/// *spelling* through, since every one of them arrives as an ordinary identifier token.
///
/// GraphQLx has forty-four of them against GraphQL's thirty-two: `import`, `from`, `as`, `where`,
/// `set` and `map` are keywords here and ordinary names there.
pub type Keyword = smear_lexer::graphqlx::ContextualKeyword;

/// [`GraphqlxLosslessLexer`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Lexer<'inp, Src: Source<usize> + ?Sized> = GraphqlxLosslessLexer<'inp, Src>;

/// [`GraphqlxLosslessToken`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Token<'inp, Src: Source<usize> + ?Sized> = GraphqlxLosslessToken<'inp, Src>;

/// [`GraphqlxLosslessInput`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Input<'inp, 'input, Src: Source<usize> + ?Sized, Ctx> =
  GraphqlxLosslessInput<'inp, 'input, Src, Ctx>;

/// [`GraphqlxLosslessError`], under the name the shared macros reach it by.
#[allow(type_alias_bounds)]
pub type Error<'inp, Src: Source<usize> + ?Sized, Ctx> = GraphqlxLosslessError<'inp, Src, Ctx>;

/// One error value a GraphQLx lossless parse can record.
///
/// The dialect's own error, **re-keyed** to the lossless token kind and the lossless lexer's
/// state error. [`GraphqlxError`](crate::graphqlx::error::GraphqlxError) cannot serve: it pins
/// `SyntacticTokenKind`, which has no image for a trivia token, so a lossless "unexpected token"
/// would have to invent a kind for the comment or the newline it found.
pub type GraphqlxLosslessErrorValue<S> =
  DialectError<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

/// The error container the lossless driver pins for `Verbose`.
///
/// Productions never name this: they name [`GraphqlxLosslessError`], which projects the error out
/// of the parse context, and constrain it with `From<…>` bounds. Only [`runner::parse_str`] — the
/// one place a concrete context is chosen — has to say which container that is.
pub type GraphqlxLosslessErrors<S> =
  DialectErrors<S, LosslessTokenKind, char, Expectation, LimitExceeded>;

crate::lossless::lossless_error_impls! {
  errors       = GraphqlxLosslessErrors;
  value        = GraphqlxLosslessErrorValue;
  token        = LosslessToken;
  kind         = LosslessTokenKind;
  lexer_errors = LexerErrors;
  error_data   = ErrorData;
  expectation  = expectation_of;
  // **Four pairs, and the fourth is not decoration.** GraphQLx's lexer depth-counts `<` and `>`
  // alongside the other three (`increase_recursion!`/`decrease_recursion!`,
  // `smear-lexer/src/graphqlx/syntactic/mod.rs:807-814`), so `<…>` is a genuinely balanced pair
  // and an unterminated one has a real report to make. `unclosed_angle` already existed on the
  // dialect error (`graphqlx/error.rs:252`), so the fourth pair costs nothing here and does not
  // fall through to the catch-all's `ErrorData::Other("unclosed delimiter")`.
  unclosed     = {
    "[]" => unclosed_list,
    "()" => unclosed_parentheses,
    "{}" => unclosed_object,
    "<>" => unclosed_angle,
  };
}

/// The lossless twin of `error.rs`'s `expectation_from_token_kind`.
///
/// `ErrorData::UnexpectedToken` has one expectation slot, so a multi-kind or absent expectation
/// falls back to `Name` exactly as the syntactic mapping does. Every trivia kind falls back too:
/// no production expects a comment or a newline — trivia is skipped into the tree by the atom
/// set, never demanded — so those arms are unreachable in practice rather than merely lossy.
///
/// # This is GraphQLx's table, not GraphQL's with rows added
///
/// Two rows differ from the GraphQL twin in a way that looks like a bug and is not.
/// [`Int`](LosslessTokenKind::Int) and [`Float`](LosslessTokenKind::Float) map to
/// [`Expectation::InputValue`], not to `IntValue`/`FloatValue`, because that is what GraphQLx's
/// own `expectation_from_token_kind` (`graphqlx/error.rs:337-338`) does — and the two GraphQLx
/// layers agreeing matters more than the two dialects agreeing, since it is the *syntactic* suite
/// this one is compared against. `IntValue` and `FloatValue` exist in the enum and are reached
/// from productions that name a value shape directly.
///
/// The seven rows GraphQL has no counterpart for — `<`, `>`, `*`, `+`, `-`, `::`, `=>` — are the
/// same seven images that widened the kind space.
fn expectation_of(expected: Option<Expected<'_, LosslessTokenKind>>) -> Expectation {
  match expected {
    Some(Expected::One(kind)) => match kind {
      LosslessTokenKind::Identifier => Expectation::Name,
      LosslessTokenKind::Int => Expectation::InputValue,
      LosslessTokenKind::Float => Expectation::InputValue,
      LosslessTokenKind::InlineString => Expectation::InlineString,
      LosslessTokenKind::BlockString => Expectation::BlockString,
      LosslessTokenKind::Dollar => Expectation::Dollar,
      LosslessTokenKind::FatArrow => Expectation::FatArrow,
      LosslessTokenKind::LAngle => Expectation::LAngle,
      LosslessTokenKind::RAngle => Expectation::RAngle,
      LosslessTokenKind::LParen => Expectation::LParen,
      LosslessTokenKind::RParen => Expectation::RParen,
      LosslessTokenKind::Spread => Expectation::Spread,
      LosslessTokenKind::Colon => Expectation::Colon,
      LosslessTokenKind::Equal => Expectation::Equal,
      LosslessTokenKind::Asterisk => Expectation::Asterisk,
      LosslessTokenKind::At => Expectation::At,
      LosslessTokenKind::LBracket => Expectation::LBracket,
      LosslessTokenKind::RBracket => Expectation::RBracket,
      LosslessTokenKind::LBrace => Expectation::LBrace,
      LosslessTokenKind::RBrace => Expectation::RBrace,
      LosslessTokenKind::Pipe => Expectation::Pipe,
      LosslessTokenKind::Bang => Expectation::Bang,
      LosslessTokenKind::Ampersand => Expectation::Ampersand,
      LosslessTokenKind::Plus => Expectation::Plus,
      LosslessTokenKind::Minus => Expectation::Minus,
      LosslessTokenKind::PathSeparator => Expectation::PathSeparator,
      _ => Expectation::Name,
    },
    _ => Expectation::Name,
  }
}

pub mod coverage;
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod extension;
pub mod generic;
pub mod import;
pub mod kind_map;
pub mod recover;
pub mod runner;
pub mod selection;
pub mod trivia;
pub mod ty;
pub mod value;

pub use runner::{Parse, parse_str, profile};
