//! GraphQLx syntactic productions over the concrete GraphQLx lexer.
//!
//! Public AST methods use the `graphqlx` / `try_graphqlx` names. Their source
//! slice is fixed by the AST result while the lexer source is inferred from the
//! supplied parser input, allowing the same APIs over `str`, `[u8]`, and the
//! lexer-supported owned source wrappers.

use core::num::NonZeroUsize;
use std::vec::Vec;
use tokora::{
  ErrorOf, InputRef, Lexer, ParseAttempt, ParseContext, SimpleSpan, Slice, Source,
  error::{UnexpectedEot, UnexpectedToken},
  span::Spanned,
};

use smear_lexer::graphqlx::{
  ContextualKeyword,
  syntactic::{SyntacticLexer, SyntacticToken, SyntacticTokenKind},
};

use super::GraphQLx;
use crate::{
  combinator::{ParseCtx, caret, double_colon, ident, tilde, try_double_colon, try_ident},
  graphqlx::{
    ast,
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

/// The concrete lexer used by GraphQLx syntactic productions over `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlxLexer<'inp, Src: ?Sized> = SyntacticLexer<'inp, Src>;

/// The source slice emitted by [`GraphqlxLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlxSlice<'inp, Src: Source<usize> + ?Sized> = <Src as Source<usize>>::Slice<'inp>;

/// The concrete token emitted by [`GraphqlxLexer`] for `Src`.
#[allow(type_alias_bounds)]
pub type GraphqlxToken<'inp, Src: Source<usize> + ?Sized> =
  SyntacticToken<GraphqlxSlice<'inp, Src>>;

/// The parser error emitted by a GraphQLx syntactic production.
#[allow(type_alias_bounds)]
pub type GraphqlxError<'inp, Src: ?Sized, Ctx>
where
  GraphqlxLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
= ErrorOf<'inp, GraphqlxLexer<'inp, Src>, Ctx, GraphQLx>;

/// A mutable GraphQLx syntactic parser input over `Src` and parser context
/// `Ctx`.
#[allow(type_alias_bounds)]
pub type GraphqlxInput<'inp, 'input, Src: ?Sized, Ctx>
where
  GraphqlxLexer<'inp, Src>: Lexer<'inp>,
  Ctx: ParseContext<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
= InputRef<'inp, 'input, GraphqlxLexer<'inp, Src>, Ctx, GraphQLx>;

pub mod argument;
pub mod definition;
pub mod directive;
pub mod document;
pub mod executable;
pub mod generic;
pub mod selection;
pub mod ty;
pub mod use_tree;
pub mod value;

pub use document::{document, document_item};

// The suite's three document roots, side by side, for the reason GraphQL's `syntactic/mod.rs`
// records: each alternate was already `pub` where it is defined, but only the mixed one was
// hoisted here, so the other two read as internals of the modules they happen to live in.
pub use definition::type_system_document;
pub use executable::executable_document;

/// Returns the GraphQLx contextual classification for an identifier token.
#[inline]
pub(crate) fn keyword_of<S>(token: &SyntacticToken<S>) -> Option<ContextualKeyword>
where
  SyntacticToken<S>: tokora::utils::DowncastRef<ContextualKeyword>,
{
  <SyntacticToken<S> as tokora::utils::DowncastRef<ContextualKeyword>>::downcast_ref(token)
}

/// Parses one committed GraphQLx name.
///
/// GraphQLx names use GraphQL's lexical `Name` shape; contextual spellings
/// remain valid names outside productions that ask for a keyword.
pub fn name<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Name<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  ident(inp).map(ast::Name::from)
}

/// Attempts one GraphQLx name without consuming on a head mismatch.
pub fn try_name<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<ast::Name<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  try_ident(inp).map(|attempt| attempt.map(ast::Name::from))
}

/// Parses a committed GraphQLx path, `PathRoot? Name (:: Name)*`.
///
/// The root is `~ ::` (the own package), `^ ::` (a dependency, named by the first segment), or
/// `.. ::` once per parent level, and trivia may stand between a root's tokens. A `::` where the
/// path begins roots nothing — `::` only joins — so it is refused with
/// [`ErrorData::LeadingPathSeparator`](crate::graphqlx::error::ErrorData::LeadingPathSeparator),
/// whose help names `^::`. Once a separator has been consumed, the next name is committed rather
/// than transactionally retried.
///
/// Each refusal names what its position takes, at the token found there: a path whose first name
/// is missing, after its root or with none, is refused as a missing
/// [`Path`](crate::graphqlx::error::Expectation::Path), and a `::` with no name after it as a
/// missing [`Name`](crate::graphqlx::error::Expectation::Name). A scanner stop at either position
/// is still the stop.
pub fn path<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Path<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let root = path_root(inp)?;
  // `try_name`, not the committed `name`: tokora's committed identifier refuses with no expectation
  // of its own. `try_name` reads through `try_expect_or_stop`, so a stop is still the stop.
  let first = match try_name(inp)? {
    ParseAttempt::Accept(name) => name,
    ParseAttempt::Decline => return unexpected_here(inp, Expectation::Path),
  };
  let start = root
    .span()
    .map_or_else(|| first.span().start(), |span| span.start());
  path_after_first(start, first, root, inp)
}

/// Attempts a GraphQLx path without consuming on a non-path head.
///
/// A path's heads are a name and the three root sigils `~`, `..` and `^`; a `::` is not one.
pub fn try_path<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<ast::Path<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match peek_kind(inp)? {
    Some(
      SyntacticTokenKind::Identifier
      | SyntacticTokenKind::Tilde
      | SyntacticTokenKind::DotDot
      | SyntacticTokenKind::Caret,
    ) => path(inp).map(ParseAttempt::Accept),
    _ => Ok(ParseAttempt::Decline),
  }
}

/// Reads the root a path starts with — nothing, `~ ::`, `^ ::`, or `.. ::` repeated — and refuses a
/// `::` where the path begins.
///
/// A root is a fixed pair of tokens per sigil, and the `.. ::` pairs are read in a loop rather than
/// by recursion, so a long chain costs no stack. Their count is bounded the way the `:: Name`
/// segments after them are: by the input, each pair consuming two tokens.
pub(crate) fn path_root<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::PathRoot, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  Ok(match peek_kind(inp)? {
    Some(SyntacticTokenKind::Tilde) => {
      let start = tilde(inp)?.span().start();
      ast::PathRoot::Package(SimpleSpan::new(start, root_separator(inp)?))
    }
    Some(SyntacticTokenKind::Caret) => {
      let start = caret(inp)?.span().start();
      ast::PathRoot::Dependency(SimpleSpan::new(start, root_separator(inp)?))
    }
    Some(SyntacticTokenKind::DotDot) => {
      let start = dot_dot(inp)?.start();
      let mut end = root_separator(inp)?;
      let mut count = NonZeroUsize::MIN;
      while matches!(peek_kind(inp)?, Some(SyntacticTokenKind::DotDot)) {
        dot_dot(inp)?;
        end = root_separator(inp)?;
        count = count.saturating_add(1);
      }
      ast::PathRoot::Parent {
        span: SimpleSpan::new(start, end),
        count,
      }
    }
    Some(SyntacticTokenKind::PathSeparator) => return leading_path_separator(inp),
    _ => ast::PathRoot::Relative,
  })
}

/// The `::` that ends a root's sigil, and where it ends.
fn root_separator<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<usize, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  double_colon(inp).map(|separator| separator.span().end())
}

/// Commits to the `..` punctuator; errors on anything else.
///
/// tokora has no `..` punctuator to build an atom from, so this one is written over the token kind,
/// with the shape and the errors of [`combinator`](crate::combinator)'s committed atoms.
pub(crate) fn dot_dot<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  // `next_or_stop`, not `next`: a terminal scanner stop must not read as an end of input here, as
  // at every committed leaf — smear issue #177.
  match inp.next_or_stop()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::DotDot,
    }) => Ok(span),
    Some(Spanned { span, data: token }) => {
      Err(UnexpectedToken::expected_one_with_found(span, token, SyntacticTokenKind::DotDot).into())
    }
    // Fully qualified so the `Span` trait need not join this module's imports.
    None => Err(UnexpectedEot::eot_of(tokora::Span::end(inp.span())).into()),
  }
}

/// Refuses the `::` at the input head, where a path begins: a path's root is written before its
/// first `::`, so this one roots nothing. Consumes nothing, as [`unexpected_here`] does not.
pub(crate) fn leading_path_separator<'inp, Src, Ctx, T>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<T, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  let span = inp
    .peek_head_map(|token| *token.span)?
    .unwrap_or_else(|| SimpleSpan::new(offset, offset));
  Err(DialectGraphqlxError::leading_path_separator(span).into())
}

/// Builds a path after its root and its first name have been consumed by a fused caller.
///
/// A `::` with no name after it is refused as a missing name, at the token found in its place.
pub(crate) fn path_after_first<'inp, Src, Ctx>(
  start: usize,
  first: ast::Name<GraphqlxSlice<'inp, Src>>,
  root: ast::PathRoot,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Path<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let mut segments = Vec::from([first]);
  while matches!(try_double_colon(inp)?, ParseAttempt::Accept(_)) {
    // As in `path`: `try_name`, so the refusal names its position and a stop stays a stop.
    match try_name(inp)? {
      ParseAttempt::Accept(name) => segments.push(name),
      ParseAttempt::Decline => return unexpected_here(inp, Expectation::Name),
    }
  }
  let end = segments
    .last()
    .expect("a GraphQLx path always contains its first name")
    .span()
    .end();
  Ok(ast::Path::new(SimpleSpan::new(start, end), root, segments))
}

/// Peeks one concrete token kind without consuming it.
pub(crate) fn peek_kind<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<SyntacticTokenKind>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  // THE DIALECT'S OWN `peek_kind`, and it shadows tokora's — which is terminal-aware. Written over
  // a raw `peek` it was not, so five call sites read a scanner stop as `Ok(None)`: "no head here"
  // on an input the scanner had already given up on. `peek_head_map` is tokora's terminal-aware
  // width-1 primitive and is what `InputRef::peek_kind` is itself written over. smear issue #177.
  inp.peek_head_map(|token| token.data.kind())
}

/// Produces a typed GraphQLx unexpected-token error at the current input head.
pub(crate) fn unexpected_here<'inp, Src, Ctx, T>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  // `peek_head_map`, not a raw `peek`: a window the scanner truncated is byte-for-byte a window a
  // short document produces, and reading the first as the second reports a stop as an absent token
  // — smear issue #177, Codex round 1. The terminal end-of-input error it raises instead carries
  // tokora's mark into this dialect's `TerminalEndOfInput`.
  let found = inp.peek_head_map(|token| (*token.span, token.data.kind()))?;
  Err(match found {
    Some((span, kind)) => DialectGraphqlxError::unexpected_token(kind, expected, span).into(),
    None => {
      DialectGraphqlxError::maybe_unexpected_token(None, expected, SimpleSpan::new(offset, offset))
        .into()
    }
  })
}

impl<S> ast::Name<S> {
  /// Parses one committed GraphQLx name.
  ///
  /// GraphQLx retains GraphQL's `Name` lexical production.
  ///
  /// See the [GraphQL Names specification](https://spec.graphql.org/draft/#sec-Names).
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  {
    name(inp)
  }

  /// Attempts one GraphQLx name without consuming on a head mismatch.
  pub fn try_graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  {
    try_name(inp)
  }
}

impl<S> ast::Path<S> {
  /// Parses one committed GraphQLx path, root included.
  ///
  /// Rooted `::`-separated paths are a GraphQLx extension, so GraphQL has no
  /// corresponding grammar anchor. See [`path`].
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    path(inp)
  }

  /// Attempts one GraphQLx path without consuming on a non-path head.
  ///
  /// Rooted `::`-separated paths are a GraphQLx extension, so GraphQL has no
  /// corresponding grammar anchor. See [`try_path`].
  pub fn try_graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
    GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
  {
    try_path(inp)
  }
}
