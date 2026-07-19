//! GraphQLx generic-**parameter** productions — the declaration side of generics
//! (`<T, U = String>` on definitions, `<T>` on extensions and executable
//! definitions) and the name-with-generics carriers.
//!
//! These are distinct productions from the generic-**argument** side
//! ([`try_type_generics`](super::ty::try_type_generics), an *application* like
//! `Container<String!>`): a parameter list introduces fresh type-parameter
//! identifiers, optionally with default *types* on definitions, while an argument
//! list applies existing types. The three parameter flavors mirror the frozen
//! GraphQLx model exactly:
//!
//! - [`definition_type_param`] / [`try_definition_type_generics`] — definitions
//!   (`type Response<T = String>`): each parameter is an identifier with an
//!   optional `= Type` default;
//! - [`extension_type_param`] / [`try_extension_type_generics`] — extensions
//!   (`extend type Box<T>`): bare identifiers, no defaults;
//! - [`try_executable_definition_type_generics`] — operations and fragments
//!   (`query GetData<T>`, `fragment<T>`): bare identifiers, kept as plain names
//!   (no per-parameter node — the parameter is a leaf).
//!
//! [`definition_name`] and [`executable_definition_name`] parse the carriers that
//! own a parameter list (`Response<T = String>`, `ItemFragment<T>`); the
//! executable flavor goes through the
//! [`fragment_name`] exclusion atom because its only
//! grammar position is a fragment's name, where `FragmentName : Name but not on`
//! applies (the shared-grammar rule W3 enforces for GraphQL).
//!
//! The where-clause family ([`where_predicate`], [`try_where_clause`]) constrains
//! the parameters a list introduced: `where T: Node & Timestamped` bounds `T` by
//! two interface type paths (fixtures `0006`/`0007`/`0020`).
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! Every parameter list is `< Param+ >`: an empty `<>` rejects, exactly as the
//! generic-argument list rejects it (the fixtures never write empty generics, and
//! the scaffold shapes document non-empty lists). Each list commits its first
//! parameter and collects the rest with a `list_of` loop — commas between
//! parameters are trivia. The where clause is `where WherePredicate+` and a
//! predicate's bounds are `TypePath (& TypePath)*` — both one-or-more, both
//! committed-first (the bounds continue on a `try_ampersand` commit-first loop,
//! never a separated1 with an identifier follow-set).
//!
//! # The predicate probe
//!
//! Predicates carry no separator (fixture `0020` stacks them on bare newlines), and
//! a predicate's bounded type is a full [`type_path`] — a
//! variable-length head — so "does another predicate follow?" is not decidable by
//! any fixed-width peek: after `where T: Node`, a following `type Bar` must end the
//! clause while `u::V<W>: X` must continue it. The loop therefore probes with
//! [`InputRef::try_attempt`]: it speculatively parses `TypePath ':'` and, on any
//! failure, rolls the probe back — tokens, diagnostics, and emitted events alike
//! (the checkpoint carries the emitter's emission mark) — and ends the clause. A
//! probe that commits hands its bounded type to the committed predicate tail, so a
//! true malformation *after* the `:` (`U:` with no bound) is still an error, never
//! a silent stop.
//!
//! # Node placement
//!
//! Parameter lists retro-wrap their kind over the whole `< … >` region when
//! present (`try_` productions decline without a node); a definition parameter
//! retro-wraps `K::DefinitionTypeParam` after its optional default settles
//! (Amendment 1: content-dependent spans use the manual
//! `cst_mark`/`cst_start_at`/`cst_finish` idiom). The name carriers wrap
//! `K::DefinitionName` / `K::ExecutableDefinitionName` around the identifier and
//! any parameter list. Each predicate retro-wraps `K::WherePredicate` (its mark
//! minted inside the probe, before the bounded type, so a committed probe's events
//! nest correctly and a rolled-back probe's mark vanishes with the rollback);
//! [`try_where_clause`] retro-wraps `K::WhereClause` around the keyword and every
//! predicate.

use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  cst::event::EventMark,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{list_of, try_angles},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::ty::{ty, type_path};
use crate::{
  combinator::{
    Equivalent, ErrorOf, ParseCtx, SliceOf, colon, fragment_name, ident, try_ampersand, try_equal,
  },
  graphqlx::{
    ast::{
      DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam, ExecutableDefinitionName,
      ExecutableDefinitionTypeGenerics, ExtensionTypeGenerics, ExtensionTypeParam, Name, TypePath,
      WhereClause, WherePredicate,
    },
    keyword::try_where,
    kinds::SyntaxKind as K,
  },
};

/// Parses a `DefinitionTypeParam` (`Name ('=' Type)?`) — one generic parameter on
/// a definition, with an optional default type.
///
/// Grammar: `DefinitionTypeParam : Name (= Type)?` (frozen GraphQLx model; fixture
/// `0010_generics_with_default`).
pub fn definition_type_param<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DefinitionTypeParam<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let default = match try_equal(inp)? {
    ParseAttempt::Accept(_equal) => Some(ty(inp)?),
    ParseAttempt::Decline => None,
  };
  let span = inp.span_since(&cursor);
  let param = DefinitionTypeParam::new(span, name, default);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::DefinitionTypeParam.raw());
  emitter.cst_finish();
  Ok(param)
}

/// Parses an optional `DefinitionTypeGenerics` list (`'<' DefinitionTypeParam+
/// '>'`), declining to `None` (no tokens consumed) unless the next token is `<`.
///
/// Once the `<` commits, at least one parameter is required — an empty `<>`
/// rejects (Amendment 2). Retro-wrapped as `K::DefinitionTypeGenerics` when
/// present.
// The `Option<DefinitionTypeGenerics<…>>` return is inherent to the optional list.
#[allow(clippy::type_complexity)]
pub fn try_definition_type_generics<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<DefinitionTypeGenerics<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_angles(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`DefinitionTypeParam+`): the first parameter commits, so an
    // empty `<>` errors at the `>` exactly as the committed ident atom reports it.
    let first = definition_type_param(inp)?;
    let mut params = list_of(
      definition_type_param,
      <L::Token as PunctuatorTokenExt>::is_close_angle,
    )(inp)?;
    params.insert(0, first);
    Ok(params)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, params) = delimited.into_components();
      let generics = DefinitionTypeGenerics::new(span, params);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::DefinitionTypeGenerics.raw());
      emitter.cst_finish();
      Ok(Some(generics))
    }
    None => Ok(None),
  }
}

/// Parses an `ExtensionTypeParam` (a bare `Name`) — one generic parameter on an
/// extension, which carries no default type.
///
/// Grammar: `ExtensionTypeParam : Name` (frozen GraphQLx model; fixture
/// `0015_extend_with_generics`).
pub fn extension_type_param<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ExtensionTypeParam<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  let param = ExtensionTypeParam::new(name_span, Name::new(name_span, name_src));
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ExtensionTypeParam.raw());
  emitter.cst_finish();
  Ok(param)
}

/// Parses an optional `ExtensionTypeGenerics` list (`'<' ExtensionTypeParam+
/// '>'`), declining to `None` (no tokens consumed) unless the next token is `<`.
///
/// Once the `<` commits, at least one parameter is required — an empty `<>`
/// rejects (Amendment 2) — and a `= Type` default rejects (extension parameters
/// carry none). Retro-wrapped as `K::ExtensionTypeGenerics` when present.
// The `Option<ExtensionTypeGenerics<…>>` return is inherent to the optional list.
#[allow(clippy::type_complexity)]
pub fn try_extension_type_generics<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ExtensionTypeGenerics<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_angles(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`ExtensionTypeParam+`): the first parameter commits.
    let first = extension_type_param(inp)?;
    let mut params = list_of(
      extension_type_param,
      <L::Token as PunctuatorTokenExt>::is_close_angle,
    )(inp)?;
    params.insert(0, first);
    Ok(params)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, params) = delimited.into_components();
      let generics = ExtensionTypeGenerics::new(span, params);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::ExtensionTypeGenerics.raw());
      emitter.cst_finish();
      Ok(Some(generics))
    }
    None => Ok(None),
  }
}

/// Parses an optional `ExecutableDefinitionTypeGenerics` list (`'<' Name+ '>'`),
/// declining to `None` (no tokens consumed) unless the next token is `<`.
///
/// The parameters are bare names (leaf tokens — no per-parameter node, and no
/// defaults: `<T = X>` rejects). Once the `<` commits, at least one name is
/// required — an empty `<>` rejects (Amendment 2). Retro-wrapped as
/// `K::ExecutableDefinitionTypeGenerics` when present.
// The `Option<ExecutableDefinitionTypeGenerics<…>>` return is inherent to the
// optional list.
#[allow(clippy::type_complexity)]
pub fn try_executable_definition_type_generics<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ExecutableDefinitionTypeGenerics<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_angles(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`Name+`): the first parameter commits.
    let (first_span, first_src) = ident(inp)?.into_components();
    let mut params = std::vec::Vec::from_iter([Name::new(first_span, first_src)]);
    let rest = list_of(
      |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
        let (span, src) = ident(inp)?.into_components();
        Ok(Name::new(span, src))
      },
      <L::Token as PunctuatorTokenExt>::is_close_angle,
    )(inp)?;
    params.extend(rest);
    Ok(params)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, params) = delimited.into_components();
      let generics = ExecutableDefinitionTypeGenerics::new(span, params);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::ExecutableDefinitionTypeGenerics.raw());
      emitter.cst_finish();
      Ok(Some(generics))
    }
    None => Ok(None),
  }
}

/// Parses the tail of a `DefinitionName` whose leading identifier the caller has
/// already consumed (`name` here, its speculative `mark` minted before it):
/// the optional parameter list, the node spend, and the carrier construction.
///
/// Shared by [`definition_name`] and the operation production, whose name is
/// optional (a `try_ident` decides whether a name exists before this tail runs).
pub(super) fn definition_name_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
  name: Name<SliceOf<'inp, L>>,
) -> Result<DefinitionName<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let generics = try_definition_type_generics(inp)?;
  let end = match &generics {
    Some(g) => g.span().end(),
    None => name.span().end(),
  };
  let span = SimpleSpan::new(name.span().start(), end);
  let def_name = DefinitionName::new(span, name, generics);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::DefinitionName.raw());
  emitter.cst_finish();
  Ok(def_name)
}

/// Parses a `DefinitionName` (`Name DefinitionTypeGenerics?`) — the
/// generic-parameter-carrying name a definition (or a named operation)
/// introduces, wrapped as a `DefinitionName` node.
///
/// Grammar: `DefinitionName : Name DefinitionTypeGenerics?` (frozen GraphQLx
/// model; fixtures `0004`/`0005`/`0010`).
pub fn definition_name<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DefinitionName<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  definition_name_body(inp, mark, Name::new(name_span, name_src))
}

/// Parses an `ExecutableDefinitionName`
/// (`FragmentName ExecutableDefinitionTypeGenerics?`) — the generic-carrying name
/// of a fragment definition, wrapped as an `ExecutableDefinitionName` node.
///
/// The identifier goes through the
/// [`fragment_name`] exclusion atom: the carrier's
/// only grammar position is a fragment's name, and `FragmentName : Name but not
/// on` is the shared grammar's rule there (the same exclusion W3 enforces for
/// GraphQL, carried to GraphQLx), so `on` rejects.
pub fn executable_definition_name<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ExecutableDefinitionName<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = fragment_name(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let generics = try_executable_definition_type_generics(inp)?;
  let end = match &generics {
    Some(g) => g.span().end(),
    None => name.span().end(),
  };
  let span = SimpleSpan::new(name.span().start(), end);
  let def_name = ExecutableDefinitionName::new(span, name, generics);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ExecutableDefinitionName.raw());
  emitter.cst_finish();
  Ok(def_name)
}

/// Parses the bounds and construction tail of a where predicate whose
/// `TypePath ':'` head the caller has already consumed (`bounded` here, its
/// speculative `mark` minted before it).
///
/// The bounds are `TypePath (& TypePath)*` — the first commits (Amendment 2:
/// one-or-more), the rest continue on a `try_ampersand` commit-first loop.
/// Shared by [`where_predicate`] (committed head) and [`try_where_clause`]'s
/// probe loop (speculative head).
fn where_predicate_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
  bounded: TypePath<SliceOf<'inp, L>>,
) -> Result<WherePredicate<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let first = type_path(inp)?;
  let mut bounds = std::vec::Vec::from_iter([first]);
  while let ParseAttempt::Accept(_amp) = try_ampersand(inp)? {
    bounds.push(type_path(inp)?);
  }
  // The first bound is committed above, so `bounds` is never empty.
  let end = bounds.last().expect("at least one bound").span().end();
  let span = SimpleSpan::new(bounded.span().start(), end);
  let predicate = WherePredicate::new(span, bounded, bounds);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::WherePredicate.raw());
  emitter.cst_finish();
  Ok(predicate)
}

/// Parses a `WherePredicate` (`TypePath ':' TypePath ('&' TypePath)*`) — one
/// bounded type and its one-or-more interface bounds, wrapped as a
/// `WherePredicate` node.
///
/// Grammar: `WherePredicate : TypePath : TypePath (& TypePath)*` (frozen GraphQLx
/// model; fixtures `0006`/`0007`/`0020`).
pub fn where_predicate<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<WherePredicate<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let bounded = type_path(inp)?;
  colon(inp)?;
  where_predicate_body(inp, mark, bounded)
}

/// Parses an optional `WhereClause` (`'where' WherePredicate+`), declining to
/// `None` (no tokens consumed) unless the soft `where` keyword is next.
///
/// Once `where` commits, at least one predicate is required (Amendment 2); the
/// rest continue while a speculative `TypePath ':'` probe commits (see the module
/// note on the predicate probe — the clause ends at the first non-predicate,
/// whatever construct follows). Retro-wrapped as `K::WhereClause` when present.
// The `Option<WhereClause<…>>` return is inherent to an optional generic
// production; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn try_where_clause<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<WhereClause<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = match try_where(inp)? {
    ParseAttempt::Accept(kw) => kw,
    ParseAttempt::Decline => return Ok(None),
  };
  // Spec cardinality (`WherePredicate+`): the first predicate is committed.
  let first = where_predicate(inp)?;
  let mut predicates = std::vec::Vec::from_iter([first]);
  // Predicate probe: speculatively parse the next predicate's `TypePath ':'` head;
  // a probe that fails rolls back (tokens, diagnostics, events) and ends the
  // clause. A committed probe hands off to the committed tail, so `U:` with no
  // bound stays an error.
  loop {
    let probed = inp.try_attempt(
      |inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| -> Result<_, ErrorOf<'inp, L, Ctx, Lang>> {
        let mark = inp.emitter().cst_mark();
        let bounded = type_path(inp)?;
        colon(inp)?;
        Ok((mark, bounded))
      },
    );
    match probed {
      Ok((pred_mark, bounded)) => {
        predicates.push(where_predicate_body(inp, pred_mark, bounded)?);
      }
      Err(_not_a_predicate) => break,
    }
  }
  // The first predicate is committed above, so `predicates` is never empty.
  let end = predicates
    .last()
    .expect("at least one predicate")
    .span()
    .end();
  let span = SimpleSpan::new(kw.span().start(), end);
  let clause = WhereClause::new(span, predicates);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::WhereClause.raw());
  emitter.cst_finish();
  Ok(Some(clause))
}

#[cfg(test)]
mod tests;
