//! GraphQLx executable-definition productions — variable definitions, operations,
//! fragments, imports at document level, and the executable document.
//!
//! Structurally the GraphQL executable layer (`graphql::syntactic::executable`)
//! over the GraphQLx shapes, with the dialect's four twists:
//!
//! - **Generics on names.** An operation's optional name is a
//!   [`definition_name`](super::generics::definition_name)
//!   (`query GetData<T, U = String>` — parameters with
//!   defaults); a fragment carries generics twice, impl generics on the keyword
//!   and an [`executable_definition_name`] on the fragment itself
//!   (`fragment<T> ItemFragment<T>`, fixtures `0016`/`0022`).
//! - **Where clauses.** Both definitions may constrain their selection set
//!   (`fragment<T> F<T> on Node<T> where T: Serializable { … }`); the clause
//!   sits between the directives and the selection set, exactly the frozen
//!   component order (`Constrained<…, SelectionSet>`).
//! - **Descriptions.** An executable definition may carry a leading string
//!   description (fixture `0016`), so the document collects
//!   [`DescribedExecutableDefinition`]s.
//! - **Imports.** The executable document interleaves import definitions with
//!   the described definitions ([`import_or_executable_definition`]).
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! [`variables_definition`] rejects an empty `()` (`VariableDefinition+`) and
//! [`executable_document`] rejects an empty input
//! (`ImportOrExecutableDefinition+`, the GraphQL `Document : Definition+`
//! parallel) — both committed-first-then-`list_of`.
//!
//! # Deviation: operation name vs the `where` soft keyword (adjudicated)
//!
//! The optional operation name is any `Name` — including the soft keyword
//! `where` — mirroring the W3 relaxation (the shared grammar restricts only
//! `FragmentName`). Name-position resolution is greedy: in `query where { x }`
//! the identifier `where` is the *name*, never an empty where clause. The form
//! this forecloses — an anonymous operation with a where clause — is
//! semantically void (a clause constrains type parameters, which only a *name*
//! can introduce), so nothing representable is lost; the ruling is pinned by
//! `operation_named_where_parses_greedily`.
//!
//! # Deviation: fragment name (spec-enforced, carried from W3)
//!
//! `FragmentName : Name but not on` — the fragment's name goes through the
//! [`executable_definition_name`] exclusion, so `fragment on on X { … }` errors;
//! the spread side is structurally unrepresentable (`selection`'s fork).
//! Regression: `fragment_named_on_error_per_spec`.
//!
//! # Node placement
//!
//! Every materialising definition retro-wraps its kind after the body settles
//! (Amendment 1); a described definition's mark is minted *before* the optional
//! description, so `K::OperationDefinition` / `K::FragmentDefinition` cover the
//! description (which is itself a `K::Description` node) — the plan's
//! described-definition convention. [`operation_type`] wraps `K::OperationType`
//! around the keyword. [`executable_definition`] and
//! [`import_or_executable_definition`] add no wrapper node — the committed arm's
//! kind is the item's (sum-type convention).

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  graphqlx::{LitFloat, LitInt},
  keywords::Fragment,
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  cst::event::EventMark,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{list_of, try_parens},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{
  directive::directives,
  generics::{
    definition_name_body, executable_definition_name, try_executable_definition_type_generics,
    try_where_clause,
  },
  import::import_definition_body,
  peeks_where,
  selection::{selection_set, type_condition_body},
  ty::ty,
  value::{default_value, variable_value},
};
use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, StringLiteral, try_description,
    try_ident,
  },
  graphqlx::{
    ast::{
      ConstrainedSelectionSet, DescribedExecutableDefinition, DescribedVariableDefinition,
      ExecutableDefinition, ExecutableDocument, FragmentDefinition, ImportOrExecutableDefinition,
      Name, OperationDefinition, OperationType, StringValue, VariablesDefinition,
    },
    keyword::{on, try_fragment, try_import, try_mutation, try_query, try_subscription},
    kinds::SyntaxKind as K,
  },
};

/// The shared error tail: reports the offending token as unexpected, or end of
/// input.
fn unexpected<'inp, L, Ctx, T, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      Err(UnexpectedToken::of(span).with_found(token).into())
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Parses a `VariableDefinition`
/// (`Description? Variable ':' Type DefaultValue? Directives?`).
///
/// Carries an optional leading description exactly as the frozen crate does, so
/// the return is a [`DescribedVariableDefinition`]. Note the source order —
/// default value before directives — matching the spec
/// (`Type DefaultValue? Directives?`); the type is the full GraphQLx [`ty`]
/// (paths, generics, lists, sets, maps).
///
/// Spec: [VariableDefinition](https://spec.graphql.org/draft/#VariableDefinition)
/// (GraphQLx shapes).
pub fn variable_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DescribedVariableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  // Optional leading description; `K::Description` wraps it when present, else the
  // speculative mark is left unspent (no node).
  let desc_mark = inp.emitter().cst_mark();
  let description = match try_description(inp)? {
    Some((lit, dspan)) => {
      let value = match lit {
        StringLiteral::Inline(inline) => StringValue::new(dspan, inline.into()),
        StringLiteral::Block(block) => StringValue::new(dspan, block.into()),
      };
      let emitter = inp.emitter();
      emitter.cst_start_at(desc_mark, K::Description.raw());
      emitter.cst_finish();
      Some(value)
    }
    None => None,
  };
  let var = variable_value(inp)?;
  crate::combinator::colon(inp)?;
  let ty = ty(inp)?;
  let default = default_value(inp)?;
  let dirs = directives(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::VariableDefinition::new(span, var, ty, dirs, default);
  let described = scaffold::Described::new(span, description, def);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::VariableDefinition.raw());
  emitter.cst_finish();
  Ok(described)
}

/// Parses an optional `VariablesDefinition` list (`'(' VariableDefinition+ ')'`),
/// declining to `None` (no tokens consumed) unless the next token is `(`.
///
/// Spec cardinality (Amendment 2, the W3 site carried to GraphQLx):
/// `VariableDefinition+` demands one-or-more, so an empty `()` errors.
///
/// Spec:
/// [VariablesDefinition](https://spec.graphql.org/draft/#VariablesDefinition)
/// (GraphQLx shapes).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn variables_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<VariablesDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_parens(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`VariableDefinition+`): the first is committed, so an empty
    // `()` errors at the `)` exactly as the committed variable reports it.
    let first = variable_definition(inp)?;
    let mut items = list_of(
      variable_definition,
      <L::Token as PunctuatorTokenExt>::is_close_paren,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, items) = delimited.into_components();
      let vars = scaffold::VariablesDefinition::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::VariablesDefinition.raw());
      emitter.cst_finish();
      Ok(Some(vars))
    }
    None => Ok(None),
  }
}

/// Parses an `OperationType` (`query` / `mutation` / `subscription`), soft
/// keywords resolved by slice compare, wrapping the keyword in a
/// `K::OperationType` node.
///
/// Spec: [OperationType](https://spec.graphql.org/draft/#OperationType).
pub fn operation_type<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<OperationType, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: KeywordToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let op = if let ParseAttempt::Accept(kw) = try_query(inp)? {
    OperationType::Query(kw)
  } else if let ParseAttempt::Accept(kw) = try_mutation(inp)? {
    OperationType::Mutation(kw)
  } else if let ParseAttempt::Accept(kw) = try_subscription(inp)? {
    OperationType::Subscription(kw)
  } else {
    return unexpected(inp);
  };
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::OperationType.raw());
  emitter.cst_finish();
  Ok(op)
}

/// Parses the `WhereClause? SelectionSet` tail every executable definition ends
/// with, folding the optional clause into a [`ConstrainedSelectionSet`].
fn constrained_selection_set<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstrainedSelectionSet<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let wc = try_where_clause(inp)?;
  let ss = selection_set(inp)?;
  let start = match &wc {
    Some(clause) => clause.span().start(),
    None => ss.span().start(),
  };
  let span = SimpleSpan::new(start, ss.span().end());
  Ok(ConstrainedSelectionSet::new(span, wc, ss))
}

/// Parses an `OperationDefinition` — the query-shorthand `SelectionSet`, or a
/// named `OperationType DefinitionName? VariablesDefinition? Directives?
/// WhereClause? SelectionSet`.
///
/// Spec: [OperationDefinition](https://spec.graphql.org/draft/#OperationDefinition)
/// (GraphQLx shapes; fixture `0016`).
pub fn operation_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<OperationDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  operation_definition_body(inp, mark)
}

/// Parses an operation definition, spending the caller-minted `mark` as
/// `K::OperationDefinition` over the whole definition (the description lands
/// inside when a described dispatch minted the mark first).
pub(super) fn operation_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
) -> Result<OperationDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // Query-shorthand: a bare selection set is an operation definition too, wrapped
  // in `K::OperationDefinition` over its `K::SelectionSet`.
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let ss = selection_set(inp)?;
    let emitter = inp.emitter();
    emitter.cst_start_at(mark, K::OperationDefinition.raw());
    emitter.cst_finish();
    return Ok(OperationDefinition::Shorthand(ss));
  }
  let cursor = inp.cursor().clone();
  let op = operation_type(inp)?;
  // Optional name with optional generic parameters: any identifier — including
  // the soft `where` (greedy name resolution; see the module deviation note) —
  // then `DefinitionTypeGenerics?`, wrapped as `K::DefinitionName`.
  let name_mark = inp.emitter().cst_mark();
  let name = match try_ident(inp)? {
    ParseAttempt::Accept(id) => {
      let (span, src) = id.into_components();
      Some(definition_name_body(inp, name_mark, Name::new(span, src))?)
    }
    ParseAttempt::Decline => None,
  };
  let vars = variables_definition(inp)?;
  let dirs = directives(inp)?;
  let constrained = constrained_selection_set(inp)?;
  let span = inp.span_since(&cursor);
  let named = scaffold::NamedOperationDefinition::new(span, op, name, vars, dirs, constrained);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::OperationDefinition.raw());
  emitter.cst_finish();
  Ok(OperationDefinition::Named(named))
}

/// Parses a `FragmentDefinition`
/// (`fragment ImplGenerics? FragmentName Generics? on TypePath Directives?
/// WhereClause? SelectionSet`).
///
/// Spec: [FragmentDefinition](https://spec.graphql.org/draft/#FragmentDefinition)
/// (GraphQLx shapes; fixtures `0016`/`0022`).
pub fn fragment_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FragmentDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = crate::graphqlx::keyword::fragment(inp)?;
  fragment_definition_body(inp, mark, kw)
}

/// Parses the body of a fragment definition after the `fragment` keyword has
/// been consumed, spending `mark` as `K::FragmentDefinition` over the whole
/// definition.
///
/// Shared by [`fragment_definition`] (which mints `mark` and consumes the
/// keyword), [`executable_definition`], and the described dispatch — since a
/// soft keyword cannot be peeked without consuming, they converge on this tail.
pub(super) fn fragment_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
  kw: Fragment,
) -> Result<FragmentDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // Impl generics on the keyword itself (`fragment<T>`), then the fragment's own
  // generic-carrying name (`ItemFragment<T>`, `FragmentName : Name but not on`).
  let impl_generics = try_executable_definition_type_generics(inp)?;
  let name = executable_definition_name(inp)?;
  let and_start = match &impl_generics {
    Some(g) => g.span().start(),
    None => name.span().start(),
  };
  let and_span = SimpleSpan::new(and_start, name.span().end());
  let name_slot = scaffold::And::new(and_span, impl_generics, name);
  // Type condition (`on TypePath`, generic application allowed): the mark is
  // minted before the committed `on`.
  let tc_mark = inp.emitter().cst_mark();
  let on_kw = on(inp)?;
  let tc = type_condition_body(inp, tc_mark, on_kw)?;
  let dirs = directives(inp)?;
  let constrained = constrained_selection_set(inp)?;
  let span = SimpleSpan::new(kw.span().start(), constrained.span().end());
  let def = scaffold::FragmentDefinition::new(span, name_slot, tc, dirs, constrained);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::FragmentDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `ExecutableDefinition` — an operation or a fragment.
///
/// The leading `fragment` soft keyword selects a fragment definition; anything
/// else is an operation definition. No wrapper node of its own — the committed
/// arm's kind is the definition's (sum-type convention).
///
/// Spec:
/// [ExecutableDefinition](https://spec.graphql.org/draft/#ExecutableDefinition)
/// (GraphQLx shapes).
pub fn executable_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ExecutableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // Mint the fragment node's mark before the dispatch consumes the soft
  // `fragment` keyword; on the operation arm it is left unspent (no wrapper node).
  let mark = inp.emitter().cst_mark();
  match try_fragment(inp)? {
    ParseAttempt::Accept(kw) => {
      fragment_definition_body(inp, mark, kw).map(ExecutableDefinition::Fragment)
    }
    ParseAttempt::Decline => operation_definition(inp).map(ExecutableDefinition::Operation),
  }
}

/// Parses the described-definition tail from a caller-minted `mark`: the
/// optional leading description (its own `K::Description` node), then the
/// operation-or-fragment dispatch whose committed arm spends `mark` as its
/// definition kind — the description lands *inside* the definition node (the
/// plan's described-definition convention).
fn described_executable_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
) -> Result<DescribedExecutableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let desc_mark = inp.emitter().cst_mark();
  let description = match try_description(inp)? {
    Some((lit, dspan)) => {
      let value = match lit {
        StringLiteral::Inline(inline) => StringValue::new(dspan, inline.into()),
        StringLiteral::Block(block) => StringValue::new(dspan, block.into()),
      };
      let emitter = inp.emitter();
      emitter.cst_start_at(desc_mark, K::Description.raw());
      emitter.cst_finish();
      Some(value)
    }
    None => None,
  };
  let def = match try_fragment(inp)? {
    ParseAttempt::Accept(kw) => {
      ExecutableDefinition::Fragment(fragment_definition_body(inp, mark, kw)?)
    }
    ParseAttempt::Decline => ExecutableDefinition::Operation(operation_definition_body(inp, mark)?),
  };
  let span = inp.span_since(&cursor);
  Ok(scaffold::Described::new(span, description, def))
}

/// Parses a `DescribedExecutableDefinition` — an executable definition with an
/// optional leading description (fixture `0016`'s described operation).
pub fn described_executable_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DescribedExecutableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  described_executable_definition_body(inp, mark)
}

/// Parses one executable-document item — an import definition when the soft
/// `import` keyword is next, otherwise a described executable definition. No
/// wrapper node of its own (sum-type convention).
///
/// A top-level `import` commits to the import arm: no executable definition can
/// begin with that bare identifier, exactly the frozen
/// `ImportOrExecutableDefinition` split.
pub fn import_or_executable_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ImportOrExecutableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_import(inp)? {
    ParseAttempt::Accept(kw) => {
      import_definition_body(inp, mark, kw).map(ImportOrExecutableDefinition::Import)
    }
    ParseAttempt::Decline => {
      described_executable_definition_body(inp, mark).map(ImportOrExecutableDefinition::Executable)
    }
  }
}

/// Parses an `ExecutableDocument` (`ImportOrExecutableDefinition+`).
///
/// Spec cardinality (Amendment 2, the GraphQL `Document : Definition+` parallel):
/// an empty input errors; imports count as items, so an import-only document
/// (fixtures `0001`–`0003`) is valid. The first item is committed before the
/// `list_of` rest, which runs to end of input.
///
/// Spec: [ExecutableDocument](https://spec.graphql.org/draft/#ExecutableDocument)
/// (GraphQLx shapes).
pub fn executable_document<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ExecutableDocument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  // Spec cardinality (`ImportOrExecutableDefinition+`): the first item is
  // committed, so an empty input errors as end of input; the rest collect until
  // end of input (no stop token — a document has no closing delimiter).
  let first = import_or_executable_definition(inp)?;
  let mut items = list_of(import_or_executable_definition, |_: &L::Token| false)(inp)?;
  items.insert(0, first);
  let span = inp.span_since(&cursor);
  let doc = scaffold::Document::new(span, items);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ExecutableDocument.raw());
  emitter.cst_finish();
  Ok(doc)
}

#[cfg(test)]
mod tests;
