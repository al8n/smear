//! GraphQLx syntactic productions over the concrete GraphQLx lexer.
//!
//! Public AST methods use the `graphqlx` / `try_graphqlx` names. Their source
//! slice is fixed by the AST result while the lexer source is inferred from the
//! supplied parser input, allowing the same APIs over `str`, `[u8]`, and the
//! lexer-supported owned source wrappers.

use std::vec::Vec;
use tokora::{
  ErrorOf, InputRef, Lexer, ParseContext, SimpleSpan, Slice, Source, cache::PeekedTokenExt,
  try_parse_input::ParseAttempt, utils::typenum::U1,
};

use smear_lexer::graphqlx::{
  ContextualKeyword,
  syntactic::{SyntacticLexer, SyntacticToken, SyntacticTokenKind},
};

use super::GraphQLx;
use crate::{
  combinator::{ParseCtx, ident, try_double_colon, try_ident},
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
pub mod import;
pub mod selection;
pub mod ty;
pub mod value;

pub use document::{document, import_or_definition_or_extension};

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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
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
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  try_ident(inp).map(|attempt| attempt.map(ast::Name::from))
}

/// Parses a committed `::`-separated GraphQLx path.
///
/// The grammar accepts an optional leading `::`, then one name followed by zero
/// or more `:: Name` suffixes. Once a separator has been consumed, the next
/// name is committed rather than transactionally retried.
pub fn path<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Path<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  let leading_separator = try_double_colon(inp)?;
  let fully_qualified = matches!(&leading_separator, ParseAttempt::Accept(_));
  let first = name(inp)?;
  let start = match leading_separator {
    ParseAttempt::Accept(separator) => separator.span().start(),
    ParseAttempt::Decline => first.span().start(),
  };
  path_after_first(start, first, fully_qualified, inp)
}

/// Attempts a GraphQLx path without consuming on a non-path head.
pub fn try_path<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ParseAttempt<ast::Path<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  match peek_kind(inp)? {
    Some(SyntacticTokenKind::Identifier | SyntacticTokenKind::PathSeparator) => {
      path(inp).map(ParseAttempt::Accept)
    }
    _ => Ok(ParseAttempt::Decline),
  }
}

/// Builds a path after its first name has been consumed by a fused caller.
pub(crate) fn path_after_first<'inp, Src, Ctx>(
  start: usize,
  first: ast::Name<GraphqlxSlice<'inp, Src>>,
  fully_qualified: bool,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ast::Path<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  let mut segments = Vec::from([first]);
  while matches!(try_double_colon(inp)?, ParseAttempt::Accept(_)) {
    segments.push(name(inp)?);
  }
  let end = segments
    .last()
    .expect("a GraphQLx path always contains its first name")
    .span()
    .end();
  Ok(ast::Path::new(
    SimpleSpan::new(start, end),
    segments,
    fully_qualified,
  ))
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
  let mut peeked = inp.peek::<U1>()?;
  Ok(peeked.pop_front().map(|token| token.token().kind()))
}

/// Produces a typed GraphQLx unexpected-token error at the current input head.
pub(crate) fn unexpected_here<'inp, Src, Ctx, T>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<T, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let offset = *inp.offset();
  let found = {
    let mut peeked = inp.peek::<U1>()?;
    peeked
      .pop_front()
      .map(|token| (*token.span(), token.token().kind()))
  };
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
    S: Slice<'inp> + Clone + 'inp,
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
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  {
    try_name(inp)
  }
}

impl<S> ast::Path<S> {
  /// Parses one committed GraphQLx qualified path.
  ///
  /// `::`-separated paths are a GraphQLx extension, so GraphQL has no
  /// corresponding grammar anchor.
  pub fn graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  {
    path(inp)
  }

  /// Attempts one GraphQLx qualified path without consuming on a non-path head.
  ///
  /// `::`-separated paths are a GraphQLx extension, so GraphQL has no
  /// corresponding grammar anchor.
  pub fn try_graphqlx<'inp, Src, Ctx>(
    inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  ) -> Result<ParseAttempt<Self>, GraphqlxError<'inp, Src, Ctx>>
  where
    Src: Source<usize, Slice<'inp> = S> + ?Sized,
    S: Slice<'inp> + Clone + 'inp,
    GraphqlxLexer<'inp, Src>: Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
    Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  {
    try_path(inp)
  }
}
