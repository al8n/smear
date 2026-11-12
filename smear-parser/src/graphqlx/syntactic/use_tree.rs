//! GraphQLx `use` item productions.
//!
//! A `use` item is a GraphQLx dialect production with no GraphQL counterpart. Its grammar is
//! [`crate::graphqlx::ast::use_tree`]'s, and one token decides at every step:
//!
//! - after `use`, a root's sigil (`~`, `..`, `^`) opens a root, a name opens a path, and an inline
//!   string opens a source;
//! - after a root, `*` is a glob, `{` a group, and a name continues the path;
//! - after a complete path, after a source, and after each name that follows a source, `::`
//!   continues, into a name, `*` or `{`;
//! - after a complete path, or a source and its names, `as` takes the alias;
//! - any other token ends the tree, and with it the item: there is no terminator.
//!
//! # What bounds the recursion, and what bounds the loops
//!
//! A group is the one recursive position, and it is entered only once its `{` has been peeked, so
//! every nested group consumes a `{` before its members are read: under a recording emitter the
//! brace delimiter would report a missing opener and read members anyway. The lexer counts every
//! such `{` against the nesting ceiling. A path's `::` continuation and a source's names are read
//! in a loop, each turn consuming a `::` and then a name, a `*` or a `{`, so their length is
//! bounded by the input and costs no stack.

use std::vec::Vec;
use tokora::{
  Accumulator, Action, EmitterView, Lexer, ParseAttempt, ParseInput, SimpleSpan, Slice, Source,
  Token,
  cache::{Peeked, PeekedTokenExt},
  span::Spanned,
  utils::{DowncastRef, typenum::U1},
};

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken, keyword_of,
  leading_path_separator, name, path_root, peek_kind, try_name, unexpected_here,
};
use crate::{
  combinator::{ParseCtx, TokenSpannedExt, asterisk, try_double_colon},
  graphqlx::{
    GraphQLx,
    ast::{
      InlineStringValue, Name, Nested, Path, PathRoot, UseDefinition, UsePrefix, UseTree,
      UseTreeKind,
    },
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! use_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    #[doc = "Parses this committed GraphQLx `use` production."]
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
        + DowncastRef<ContextualKeyword>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

/// The trees a group holds, and the span of its braces.
#[allow(type_alias_bounds)]
type Group<'inp, Src: Source<usize> + ?Sized> =
  (SimpleSpan, Nested<UseTree<GraphqlxSlice<'inp, Src>>>);

/// The glob or the group a `::` led to, and where it ends.
#[allow(type_alias_bounds)]
type Scope<'inp, Src: Source<usize> + ?Sized> = (usize, UseTreeKind<GraphqlxSlice<'inp, Src>>);

fn contextual_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  expected: Expectation,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next_or_stop()? {
    Some(Spanned { span, data: token }) if keyword_of(&token) == Some(keyword) => Ok(span),
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
    }
    None => unexpected_here(inp, expected),
  }
}

/// `(as Name)?` after a complete path, or after a source and its names.
///
/// After a complete path `as` is always the keyword, so an item named `as` is written first in its
/// group or straight after a `::`, where a name is what the grammar reads.
fn optional_alias<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Name<GraphqlxSlice<'inp, Src>>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  // `peek_head_map`, not a raw `peek`: a scanner stop must not read as "no `as` here" and hand
  // back a tree with its alias cut off — smear issue #177.
  let spelled_as =
    inp.peek_head_map(|token| keyword_of(token.data) == Some(ContextualKeyword::As))?;
  if spelled_as != Some(true) {
    return Ok(None);
  }
  contextual_keyword(inp, ContextualKeyword::As, Expectation::As)?;
  // `try_name`, not `name`: the committed atom refuses with no expectation, and this refusal names
  // the name that `as` takes.
  match try_name(inp)? {
    ParseAttempt::Accept(alias) => Ok(Some(alias)),
    ParseAttempt::Decline => unexpected_here(inp, Expectation::Name),
  }
}

/// Reads `:: Name` pairs onto `segments` for as long as each `::` is followed by a name, and
/// answers the glob or the group the first other `::` leads to — `None` when no `::` follows.
///
/// A loop, so a path or a source's names cost no stack however long. Each turn consumes a `::` and
/// then a name, a `*` or a whole group, or refuses.
fn segments_then_scope<'inp, Src, Ctx>(
  segments: &mut Vec<Name<GraphqlxSlice<'inp, Src>>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<Scope<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  while matches!(try_double_colon(inp)?, ParseAttempt::Accept(_)) {
    match peek_kind(inp)? {
      Some(SyntacticTokenKind::Identifier) => segments.push(name(inp)?),
      Some(SyntacticTokenKind::Asterisk) => {
        let glob = asterisk(inp)?;
        return Ok(Some((glob.span().end(), UseTreeKind::Glob)));
      }
      // The `{` is confirmed here, before the group is entered.
      Some(SyntacticTokenKind::LBrace) => {
        let (span, trees) = group(inp)?;
        return Ok(Some((span.end(), UseTreeKind::Group(trees))));
      }
      _ => return unexpected_here(inp, Expectation::NameGlobOrGroup),
    }
  }
  Ok(None)
}

/// Finishes a tree once its prefix is read: the glob or the group a `::` reached, or else a
/// binding, with its alias if one is written.
fn finish_tree<'inp, Src, Ctx>(
  start: usize,
  prefix: UsePrefix<GraphqlxSlice<'inp, Src>>,
  prefix_end: usize,
  scope: Option<Scope<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseTree<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match scope {
    Some((end, kind)) => Ok(UseTree::new(SimpleSpan::new(start, end), prefix, kind)),
    None => {
      let alias = optional_alias(inp)?;
      let end = alias
        .as_ref()
        .map_or(prefix_end, |alias| alias.span().end());
      Ok(UseTree::new(
        SimpleSpan::new(start, end),
        prefix,
        UseTreeKind::Simple { alias },
      ))
    }
  }
}

/// A tree whose prefix is a path, once the path's root and its first name are consumed.
fn tree_after_first_name<'inp, Src, Ctx>(
  start: usize,
  root: PathRoot,
  first: Name<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseTree<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let mut segments = Vec::from([first]);
  let scope = segments_then_scope(&mut segments, inp)?;
  let path_end = segments
    .last()
    .expect("a use tree's path always holds its first name")
    .span()
    .end();
  let path = Path::new(SimpleSpan::new(start, path_end), root, segments);
  finish_tree(start, UsePrefix::Path(path), path_end, scope, inp)
}

/// A tree after its root: `*`, a group, or the first name of the path the root roots.
fn tree_after_root<'inp, Src, Ctx>(
  start: usize,
  root: PathRoot,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseTree<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match peek_kind(inp)? {
    Some(SyntacticTokenKind::Identifier) => {
      let first = name(inp)?;
      tree_after_first_name(start, root, first, inp)
    }
    Some(SyntacticTokenKind::Asterisk) => {
      let glob = asterisk(inp)?;
      Ok(UseTree::new(
        SimpleSpan::new(start, glob.span().end()),
        UsePrefix::Root(root),
        UseTreeKind::Glob,
      ))
    }
    // The `{` is confirmed here, before the group is entered.
    Some(SyntacticTokenKind::LBrace) => {
      let (span, trees) = group(inp)?;
      Ok(UseTree::new(
        SimpleSpan::new(start, span.end()),
        UsePrefix::Root(root),
        UseTreeKind::Group(trees),
      ))
    }
    _ => unexpected_here(inp, Expectation::NameGlobOrGroup),
  }
}

/// A tree whose prefix is a string source: the names after it, then what it binds.
fn tree_after_source<'inp, Src, Ctx>(
  source: InlineStringValue<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseTree<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = source.span().start();
  let mut segments = Vec::new();
  let scope = segments_then_scope(&mut segments, inp)?;
  let prefix_end = segments
    .last()
    .map_or_else(|| source.span().end(), |segment| segment.span().end());
  finish_tree(
    start,
    UsePrefix::Source { source, segments },
    prefix_end,
    scope,
    inp,
  )
}

/// One tree inside a group: it starts with a name and is relative to the group's prefix, so a root
/// and a string source are refused here.
fn group_member<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseTree<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match peek_kind(inp)? {
    Some(SyntacticTokenKind::Identifier) => {
      let first = name(inp)?;
      let start = first.span().start();
      tree_after_first_name(start, PathRoot::Relative, first, inp)
    }
    _ => unexpected_here(inp, Expectation::Name),
  }
}

fn decide_group_member<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: EmitterView<'_, 'inp, GraphqlxLexer<'inp, Src>, Ctx::Emitter, GraphQLx>,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(match peeked.pop_front() {
    Some(token) if matches!(token.token(), GraphqlxToken::<'inp, Src>::RBrace) => Action::Stop,
    Some(_) => Action::Continue,
    None => Action::Stop,
  })
}

/// `{ UseTree+ }`. **Precondition: the head is a `{` its caller has peeked** — see the module docs
/// for why the group may not be entered on anything else.
fn group<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Group<'inp, Src>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  (|inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| group_member(inp))
    .repeated_while::<_, U1>(decide_group_member::<_, Ctx>)
    .at_least(1)
    .delimited_by_braces()
    // `Nested`, as a selection set collects into: the container is where a deep tree's release
    // lives.
    .collect_with(Nested::empty())
    .token_spanned()
    .parse_input(inp)
    .map(|Spanned { span, data }| (span, data))
}

use_parser!(
  /// Parses one committed GraphQLx use tree, the part of a `use` item after its keyword: a root,
  /// a path or a string source, then what it binds — one item, a glob, or a group of trees.
  ///
  /// This is a GraphQLx dialect production with no corresponding GraphQL draft
  /// production.
  pub use_tree,
  inp,
  UseTree<GraphqlxSlice<'inp, Src>>,
  {
    match peek_kind(inp)? {
      Some(
        SyntacticTokenKind::Tilde | SyntacticTokenKind::DotDot | SyntacticTokenKind::Caret,
      ) => {
        let root = path_root(inp)?;
        let start = root
          .span()
          .expect("a root read from its sigil has a span")
          .start();
        tree_after_root(start, root, inp)
      }
      Some(SyntacticTokenKind::Identifier) => {
        let first = name(inp)?;
        let start = first.span().start();
        tree_after_first_name(start, PathRoot::Relative, first, inp)
      }
      Some(SyntacticTokenKind::InlineString) => {
        let source = super::value::inline_string_value(inp)?;
        tree_after_source(source, inp)
      }
      // A path never starts with `::`, so this one roots nothing; the refusal's help names `^::`.
      Some(SyntacticTokenKind::PathSeparator) => leading_path_separator(inp),
      _ => unexpected_here(inp, Expectation::UseTree),
    }
  }
);

/// Parses `use UseTree` after its keyword was consumed by a fused dispatcher.
pub(crate) fn use_definition_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<UseDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let tree = use_tree(inp)?;
  Ok(UseDefinition::new(
    SimpleSpan::new(start, tree.span().end()),
    tree,
  ))
}

/// Parses `use UseTree`.
///
/// This is a GraphQLx dialect production with no corresponding GraphQL draft
/// production.
pub fn use_definition<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<UseDefinition<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let start = contextual_keyword(inp, ContextualKeyword::Use, Expectation::Use)?.start();
  use_definition_after_keyword(inp, start)
}

macro_rules! graphqlx_use_api {
  ($(#[$meta:meta])* $node:ty, $parse:ident) => {
    impl<S> $node {
      $(#[$meta])*
      /// Parses this committed GraphQLx `use` production.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = S> + ?Sized,
        S: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + DowncastRef<ContextualKeyword>,
        GraphqlxLexer<'inp, Src>: Lexer<
            'inp,
            Source = Src,
            Token = GraphqlxToken<'inp, Src>,
            Span = SimpleSpan,
            Offset = usize,
          >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<S>>,
      {
        $parse(inp)
      }
    }
  };
}

graphqlx_use_api!(
  /// Parses a committed GraphQLx use tree, the part of a `use` item after its keyword.
  ///
  /// See [`use_tree`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  UseTree<S>,
  use_tree
);
graphqlx_use_api!(
  /// Parses a committed GraphQLx `use` item.
  ///
  /// See [`use_definition`]. This is a GraphQLx dialect production with no
  /// corresponding GraphQL draft production.
  UseDefinition<S>,
  use_definition
);
