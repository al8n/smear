//! GraphQLx selection productions — fields, selections, selection sets, and the
//! `...` fragment dispatch.
//!
//! Structurally the GraphQL selection layer (`graphql::syntactic::selection`)
//! over the GraphQLx shapes: [`selection`] commits a non-`...` head to [`field`];
//! a `...` head hands off to the fragment fork, which — after consuming the
//! spread — disambiguates on a one-token lookahead: a soft `on` keyword opens an
//! inline fragment with a type condition, a `{` or `@` opens a bare inline
//! fragment, and anything else is a fragment spread. The GraphQLx twists:
//!
//! - a type condition names a generic-applicable type path (`... on Item<T>`,
//!   fixture `0016`), parsed by [`type_path`] and wrapped
//!   as a `K::TypeCondition` node;
//! - a fragment spread names a `FragmentTypePath` — a `::`-path with optional
//!   generic *arguments* (`...UserFields<T>`, `...ConnectionFields<String,
//!   Int>`, fixture `0022`) — whose first segment goes through the
//!   [`fragment_name`] exclusion atom.
//!
//! Because the fork rules `on` out FIRST, a fragment spread named `on` is
//! structurally unrepresentable — `... on …` always commits to the
//! inline-fragment arm — which is exactly the shared grammar's `FragmentName :
//! Name but not on` exclusion, carried to GraphQLx. The spread arm still parses
//! its first path segment through the exclusion atom as defense in depth, and the
//! per-shape behavior is pinned by
//! `fragment_spread_named_on_is_unrepresentable`.
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! [`selection_set`] enforces `SelectionSet : { Selection+ }` natively (the W3
//! deviation site, carried to GraphQLx): the first selection is committed before
//! the `list_of` rest, so an empty `{}` errors. The regression is
//! `selection_set_empty_braces_error_per_spec`.
//!
//! # Node placement
//!
//! [`field`] retro-wraps `K::Field` around the whole field and, when a `:`
//! follows the first name, `K::Alias` around that name and colon — both
//! content-dependent (Amendment 1). The fragment fork retro-wraps
//! `K::FragmentSpread`/`K::InlineFragment` likewise, since the node kind is only
//! known after the lookahead; the spread's path region is a `K::Path` node and a
//! present type condition a `K::TypeCondition` node. [`selection_set`] opens
//! `K::SelectionSet` up front with [`node`] over the `braces` region.
//! [`selection`] itself opens no node — the committed arm's own node kind is the
//! selection's, per the sum-type convention.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  graphqlx::{LitFloat, LitInt},
  keywords::On,
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, ParseInput, SimpleSpan, Token,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, list_of, node},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{
  argument::arguments,
  directive::directives,
  peeks_where,
  ty::{try_type_generics, type_path},
};
use crate::{
  combinator::{
    ErrorOf, LiteralValueToken, ParseCtx, SliceOf, fragment_name, ident, spread, try_colon,
    try_path_sep,
  },
  graphqlx::{
    ast::{Field, FragmentTypePath, Name, Path, Selection, SelectionSet, TypeCondition},
    keyword::try_on,
    kinds::SyntaxKind as K,
  },
};

/// Parses a `Field` (`Alias? Name Arguments? Directives? SelectionSet?`).
///
/// The alias is a two-name lookahead: the first name, if followed by `:`, is the
/// alias and the second name is the field name; otherwise the first name is the
/// field name and there is no alias.
///
/// Spec: [Field](https://spec.graphql.org/draft/#Field) (GraphQLx shapes).
pub fn field<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Field<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  // Speculative alias mark: minted before the first name so `K::Alias` can wrap the
  // name and colon retroactively if a `:` follows; left unspent (no node) otherwise.
  let alias_mark = inp.emitter().cst_mark();
  let (name1_span, name1_src) = ident(inp)?.into_components();
  let name1 = Name::new(name1_span, name1_src);
  let (alias, name) = match try_colon(inp)? {
    ParseAttempt::Accept(_colon) => {
      let alias_span = inp.span_since(&cursor);
      let emitter = inp.emitter();
      emitter.cst_start_at(alias_mark, K::Alias.raw());
      emitter.cst_finish();
      let (name2_span, name2_src) = ident(inp)?.into_components();
      (
        Some(scaffold::Alias::new(alias_span, name1)),
        Name::new(name2_span, name2_src),
      )
    }
    ParseAttempt::Decline => (None, name1),
  };
  let args = arguments(inp)?;
  let dirs = directives(inp)?;
  let ss = if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    Some(selection_set(inp)?)
  } else {
    None
  };
  let span = inp.span_since(&cursor);
  let field = scaffold::Field::new(span, alias, name, args, dirs, ss);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::Field.raw());
  emitter.cst_finish();
  Ok(field.into())
}

/// Parses the type condition following an already-consumed `on` keyword
/// (`on TypePath`), wrapping `on` and the type path together as a
/// `K::TypeCondition` node spent from `mark` (minted before the `on`).
pub(super) fn type_condition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: tokora::cst::event::EventMark,
  on_kw: On,
) -> Result<TypeCondition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let tp = type_path(inp)?;
  let span = SimpleSpan::new(on_kw.span().start(), tp.span().end());
  let tc = scaffold::TypeCondition::new(span, tp);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::TypeCondition.raw());
  emitter.cst_finish();
  Ok(tc)
}

/// Parses a `FragmentTypePath` — the spread's target: a `::`-path whose first
/// segment passes the [`fragment_name`] exclusion (`Name but not on`), with
/// optional generic arguments (`ns::Frag<T>`). The path region is wrapped as a
/// `K::Path` node; present generics self-wrap as `K::TypeGenerics`.
fn fragment_type_path<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FragmentTypePath<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let path_mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let (first_span, first_src) = fragment_name(inp)?.into_components();
  let mut segments = std::vec::Vec::from_iter([Name::new(first_span, first_src)]);
  while let ParseAttempt::Accept(_sep) = try_path_sep(inp)? {
    let (seg_span, seg_src) = ident(inp)?.into_components();
    segments.push(Name::new(seg_span, seg_src));
  }
  let path_span = inp.span_since(&cursor);
  let path = Path::new(path_span, segments, false);
  let emitter = inp.emitter();
  emitter.cst_start_at(path_mark, K::Path.raw());
  emitter.cst_finish();
  let generics = try_type_generics(inp)?;
  let end = match &generics {
    Some(g) => g.span().end(),
    None => path_span.end(),
  };
  let span = SimpleSpan::new(path_span.start(), end);
  Ok(FragmentTypePath::new(span, path, generics))
}

/// Parses a `Selection` — a field, a fragment spread, or an inline fragment.
///
/// One-token dispatch: a non-`...` head is a [`field`]; a `...` head hands off to
/// the fragment fork (`spread_selection`). No wrapper node of its own — the
/// committed arm's kind is the selection's (sum-type convention).
///
/// Spec: [Selection](https://spec.graphql.org/draft/#Selection) (GraphQLx shapes).
pub fn selection<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Selection<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_spread)? {
    spread_selection(inp)
  } else {
    field(inp).map(Selection::Field)
  }
}

/// The `...` fork of [`selection`]: consumes the spread, then disambiguates on a
/// one-token lookahead into an `InlineFragment` (with or without a type
/// condition) or a `FragmentSpread`, retro-wrapping the resolved node kind over
/// the whole region (`...` included).
fn spread_selection<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Selection<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  spread(inp)?;
  // Speculative type-condition mark: spent only when the `on` arm commits.
  let tc_mark = inp.emitter().cst_mark();
  match try_on(inp)? {
    ParseAttempt::Accept(on_kw) => {
      // `... on TypePath Directives? SelectionSet` — inline fragment with a type
      // condition spanning exactly `on TypePath` (generic application allowed:
      // `... on Document<T>`, fixture `0022`).
      let tc = type_condition_body(inp, tc_mark, on_kw)?;
      let dirs = directives(inp)?;
      let ss = selection_set(inp)?;
      let span = inp.span_since(&cursor);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::InlineFragment.raw());
      emitter.cst_finish();
      Ok(Selection::InlineFragment(scaffold::InlineFragment::new(
        span,
        Some(tc),
        dirs,
        ss,
      )))
    }
    ParseAttempt::Decline => {
      if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)?
        || peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_at)?
      {
        // `... Directives? SelectionSet` — inline fragment without a type
        // condition (fixture `0022`'s bare `... { … }`).
        let dirs = directives(inp)?;
        let ss = selection_set(inp)?;
        let span = inp.span_since(&cursor);
        let emitter = inp.emitter();
        emitter.cst_start_at(mark, K::InlineFragment.raw());
        emitter.cst_finish();
        Ok(Selection::InlineFragment(scaffold::InlineFragment::new(
          span, None, dirs, ss,
        )))
      } else {
        // `... FragmentTypePath Directives?` — fragment spread. The `on` head was
        // ruled out above (the fork consumed it into the inline-fragment arm), so
        // a spread named `on` is structurally unrepresentable here; the
        // `fragment_name` exclusion atom on the first path segment is defense in
        // depth.
        let ftp = fragment_type_path(inp)?;
        let dirs = directives(inp)?;
        let span = inp.span_since(&cursor);
        let emitter = inp.emitter();
        emitter.cst_start_at(mark, K::FragmentSpread.raw());
        emitter.cst_finish();
        Ok(Selection::FragmentSpread(scaffold::FragmentSpread::new(
          span, ftp, dirs,
        )))
      }
    }
  }
}

/// Parses a `SelectionSet` (`{ Selection+ }`).
///
/// Spec cardinality (Amendment 2, the W3 site carried to GraphQLx): the spec's
/// `Selection+` demands one-or-more, so an empty `{}` errors. The first selection
/// is committed before the `list_of` rest (commas are trivia, so `separated1`
/// does not fit).
///
/// Spec: [SelectionSet](https://spec.graphql.org/draft/#SelectionSet) (GraphQLx
/// shapes).
pub fn selection_set<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<SelectionSet<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  node(
    K::SelectionSet.raw(),
    braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
      // Spec cardinality (`Selection+`): the first selection is committed, so an
      // empty `{}` errors at the `}` exactly as the committed selection reports it.
      let first = selection(inp)?;
      let mut items = list_of(selection, <L::Token as PunctuatorTokenExt>::is_close_brace)(inp)?;
      items.insert(0, first);
      Ok(items)
    }),
  )
  .parse_input(inp)
  .map(|delimited| {
    let (span, _open, _close, sels) = delimited.into_components();
    scaffold::SelectionSet::new(span, sels)
  })
}

#[cfg(test)]
mod tests;
