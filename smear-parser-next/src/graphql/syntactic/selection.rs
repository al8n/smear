//! GraphQL selection productions — fields, selections, selection sets, and the
//! `...` fragment dispatch.
//!
//! [`selection`] is the hot dispatcher: a non-`...` head commits to [`field`]; a
//! `...` head hands off to the fragment fork, which — after consuming the spread —
//! disambiguates on a one-token lookahead (matching the frozen crate's
//! `parse_selection`): a soft `on` keyword opens an inline fragment with a type
//! condition, a `{` or `@` opens a bare inline fragment, and anything else is a
//! fragment spread. The `on` keyword is soft (a string-compared identifier, no
//! strict keywords) so [`try_on`] resolves it by slice; the shared `...` plus that
//! lookahead make `FragmentSpread` and `InlineFragment` one deterministic fork
//! rather than two independently-callable productions.
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! [`selection_set`] enforces `SelectionSet : { Selection+ }` natively: the first
//! selection is committed before the `list_of` rest, so an empty `{}` errors — a
//! documented deviation from the frozen parser, whose `while` loop accepted it. The
//! regression is `selection_set_empty_braces_error_per_spec`.
//!
//! # Node placement
//!
//! [`field`] retro-wraps `K::Field` around the whole field and, when a `:` follows
//! the first name, `K::Alias` around that name and colon — both content-dependent
//! (Amendment 1), so both use the manual `cst_mark`/`cst_start_at`/`cst_finish`
//! idiom rather than [`node`]. The fragment fork retro-wraps
//! `K::FragmentSpread`/`K::InlineFragment` likewise, since the node kind is only
//! known after the lookahead. [`selection_set`] opens `K::SelectionSet` up front
//! with [`node`] over the `braces` region. [`selection`] itself opens no node — the
//! committed arm's own node kind is the selection's, per the sum-type convention.

use smear_lexer::{LitBlockStr, LitInlineStr};
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

use super::{argument::arguments, directive::directives, peeks_where};
use crate::{
  combinator::{ErrorOf, LiteralValueToken, ParseCtx, SliceOf, ident, spread, try_colon},
  graphql::{
    ast::{Field, FragmentName, Name, Selection, SelectionSet},
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
/// Spec: [Field](https://spec.graphql.org/draft/#Field).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
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

/// Parses a `Selection` — a field, a fragment spread, or an inline fragment.
///
/// One-token dispatch: a non-`...` head is a [`field`]; a `...` head hands off to
/// the fragment fork (`spread_selection`). No wrapper node of its own — the
/// committed arm's kind is the selection's (sum-type convention).
///
/// Spec: [Selection](https://spec.graphql.org/draft/#Selection).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
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
/// one-token lookahead into an `InlineFragment` (with or without a type condition)
/// or a `FragmentSpread`, retro-wrapping the resolved node kind over the whole
/// region (`...` included).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
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
  match try_on(inp)? {
    ParseAttempt::Accept(on_kw) => {
      // `... on NamedType Directives? SelectionSet` — inline fragment with a type
      // condition spanning exactly `on NamedType`.
      let (tn_span, tn_src) = ident(inp)?.into_components();
      let tn = Name::new(tn_span, tn_src);
      let tc_span = SimpleSpan::new(on_kw.span().start(), tn.span().end());
      let tc = scaffold::TypeCondition::new(tc_span, tn);
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
        // `... Directives? SelectionSet` — inline fragment without a type condition.
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
        // `... FragmentName Directives?` — fragment spread. The `on` head was ruled
        // out above, so any name here is a valid fragment name.
        let (name_span, name_src) = ident(inp)?.into_components();
        let fname = FragmentName::new(name_span, name_src);
        let dirs = directives(inp)?;
        let span = inp.span_since(&cursor);
        let emitter = inp.emitter();
        emitter.cst_start_at(mark, K::FragmentSpread.raw());
        emitter.cst_finish();
        Ok(Selection::FragmentSpread(scaffold::FragmentSpread::new(
          span, fname, dirs,
        )))
      }
    }
  }
}

/// Parses a `SelectionSet` (`{ Selection+ }`).
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `Selection+` demands one-or-more, so an empty `{}` errors here where
/// frozen's `while` loop accepted it. The first selection is committed before the
/// `list_of` rest (commas are trivia, so `separated1` does not fit).
///
/// Spec: [SelectionSet](https://spec.graphql.org/draft/#SelectionSet).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
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
