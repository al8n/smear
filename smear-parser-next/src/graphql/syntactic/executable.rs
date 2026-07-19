//! GraphQL executable-definition productions — variable definitions, operations,
//! fragments, and the executable document.
//!
//! [`operation_definition`] handles both the query-shorthand (`{ … }`) and the named
//! `OperationType Name? VariablesDefinition? Directives? SelectionSet` forms;
//! [`fragment_definition`] parses `fragment FragmentName on NamedType Directives?
//! SelectionSet`; [`executable_definition`] dispatches between them; and
//! [`executable_document`] collects the whole `ExecutableDefinition+` stream.
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! Two `+` sites are enforced natively, each a documented deviation from the frozen
//! parser (whose unenforced `repeated_while::<_, U1>` accepted empty):
//! [`variables_definition`] rejects an empty `()` (`VariableDefinition+`), and
//! [`executable_document`] rejects an empty input (`ExecutableDefinition+`). Both use
//! the committed-first-element-then-`list_of`-rest idiom (commas are trivia, so
//! `separated1` does not fit).
//!
//! # Deviation: operation name (adjudicated — spec-correct relaxation vs frozen)
//!
//! The optional operation name is any `Name` (spec-correct: `OperationDefinition`'s
//! `Name?` is unrestricted; the grammar reserves `on` ONLY in `FragmentName`). The
//! frozen parser additionally excludes `on` (`!peek_keyword("on")`), an
//! over-restriction with no spec basis; parser-next accepts `on` as an operation
//! name.
//!
//! # Deviation: fragment name (spec-enforced where frozen was not)
//!
//! `FragmentName : Name but not on` — the spec's second named exclusion, exactly
//! parallel to `enum_value`'s — is enforced through the
//! [`fragment_name`] atom, so
//! `fragment on on X { … }` errors. The frozen parser did NOT enforce it at either
//! call site (its `parse_fragment_name` helper carried the check but
//! `parse_fragment_definition` and `parse_selection` both bypassed it via
//! `parse_name`). Regressions: `fragment_named_on_error_per_spec` here and
//! `fragment_spread_named_on_is_unrepresentable` on the spread side (where the
//! `...`-fork's `on`-first dispatch makes a spread named `on` structurally
//! unrepresentable — the atom there is defense in depth).

use smear_lexer::keywords::Fragment;
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{list_of, try_parens},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{
  directive::directives,
  peeks_where,
  selection::selection_set,
  ty::ty,
  value::{default_value, variable_value},
};
use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, StringLiteral, colon, fragment_name,
    ident, try_description, try_ident,
  },
  graphql::{
    ast::{
      DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, FragmentDefinition,
      FragmentName, Name, OperationDefinition, OperationType, StringValue, VariablesDefinition,
    },
    keyword::{fragment, on, try_fragment, try_mutation, try_query, try_subscription},
  },
};

/// The shared error tail: reports the offending token as unexpected, or end of input.
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
/// Carries an optional leading description exactly as the frozen crate does, so the
/// return is a [`DescribedVariableDefinition`]. Note the source order — default value
/// before directives — matching the spec (`Type DefaultValue? Directives?`).
///
/// Spec: [VariableDefinition](https://spec.graphql.org/draft/#VariableDefinition).
pub fn variable_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DescribedVariableDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  // Optional leading description, carried exactly as the frozen crate does.
  let description = match try_description(inp)? {
    Some((lit, dspan)) => {
      let value = match lit {
        StringLiteral::Inline(inline) => StringValue::new(dspan, inline.into()),
        StringLiteral::Block(block) => StringValue::new(dspan, block.into()),
      };
      Some(value)
    }
    None => None,
  };
  let var = variable_value(inp)?;
  colon(inp)?;
  let ty = ty(inp)?;
  let default = default_value(inp)?;
  let dirs = directives(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::VariableDefinition::new(span, var, ty, dirs, default);
  let described = scaffold::Described::new(span, description, def);
  Ok(described)
}

/// Parses an optional `VariablesDefinition` list (`'(' VariableDefinition+ ')'`),
/// declining to `None` (no tokens consumed) unless the next token is `(`.
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `VariableDefinition+` demands one-or-more, so an empty `()` errors here
/// where frozen's unenforced `+` accepted it.
///
/// Spec: [VariablesDefinition](https://spec.graphql.org/draft/#VariablesDefinition).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
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
      Ok(Some(vars))
    }
    None => Ok(None),
  }
}

/// Parses an `OperationType` (`query` / `mutation` / `subscription`), soft keywords
/// resolved by slice compare.
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
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let op = if let ParseAttempt::Accept(kw) = try_query(inp)? {
    OperationType::Query(kw)
  } else if let ParseAttempt::Accept(kw) = try_mutation(inp)? {
    OperationType::Mutation(kw)
  } else if let ParseAttempt::Accept(kw) = try_subscription(inp)? {
    OperationType::Subscription(kw)
  } else {
    return unexpected(inp);
  };
  Ok(op)
}

/// Parses an `OperationDefinition` — the query-shorthand `SelectionSet`, or a named
/// `OperationType Name? VariablesDefinition? Directives? SelectionSet`.
///
/// Spec: [OperationDefinition](https://spec.graphql.org/draft/#OperationDefinition).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  operation_definition_body(inp)
}

/// Parses an operation definition.
///
/// Shared by [`operation_definition`] and the document-level definition dispatches
/// (the `fragment_definition_body` convergence pattern).
pub(super) fn operation_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<OperationDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // Query-shorthand: a bare selection set is an operation definition too.
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    let ss = selection_set(inp)?;
    return Ok(OperationDefinition::Shorthand(ss));
  }
  let cursor = inp.cursor().clone();
  let op = operation_type(inp)?;
  // Optional name: any identifier (spec-correct; see the module note on `on`).
  let name = match try_ident(inp)? {
    ParseAttempt::Accept(id) => {
      let (span, src) = id.into_components();
      Some(Name::new(span, src))
    }
    ParseAttempt::Decline => None,
  };
  let vars = variables_definition(inp)?;
  let dirs = directives(inp)?;
  let ss = selection_set(inp)?;
  let span = inp.span_since(&cursor);
  let named = scaffold::NamedOperationDefinition::new(span, op, name, vars, dirs, ss);
  Ok(OperationDefinition::Named(named))
}

/// Parses a `FragmentDefinition`
/// (`fragment FragmentName on NamedType Directives? SelectionSet`).
///
/// Spec: [FragmentDefinition](https://spec.graphql.org/draft/#FragmentDefinition).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let kw = fragment(inp)?;
  fragment_definition_body(inp, kw)
}

/// Parses the body of a fragment definition after the `fragment` keyword has been
/// consumed.
///
/// Shared by [`fragment_definition`], [`executable_definition`], and the
/// document-level definition dispatches (each consumes the keyword as its dispatch
/// and hands it here) — since a soft keyword cannot be peeked without consuming, the
/// dispatches and the standalone production converge on this tail.
pub(super) fn fragment_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw: Fragment,
) -> Result<FragmentDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  // `FragmentName : Name but not on` — the exclusion atom, so `fragment on on X`
  // errors here (spec-enforced; frozen bypassed its own check via `parse_name`).
  let (name_span, name_src) = fragment_name(inp)?.into_components();
  let fname = FragmentName::new(name_span, name_src);
  let on_kw = on(inp)?;
  let (tn_span, tn_src) = ident(inp)?.into_components();
  let tn = Name::new(tn_span, tn_src);
  let tc_span = SimpleSpan::new(on_kw.span().start(), tn.span().end());
  let tc = scaffold::TypeCondition::new(tc_span, tn);
  let dirs = directives(inp)?;
  let ss = selection_set(inp)?;
  let span = SimpleSpan::new(kw.span().start(), ss.span().end());
  let def = scaffold::FragmentDefinition::new(span, fname, tc, dirs, ss);
  Ok(def)
}

/// Parses an `ExecutableDefinition` — an operation or a fragment.
///
/// The leading `fragment` soft keyword selects a fragment definition; anything else
/// is an operation definition.
///
/// Spec: [ExecutableDefinition](https://spec.graphql.org/draft/#ExecutableDefinition).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match try_fragment(inp)? {
    ParseAttempt::Accept(kw) => {
      fragment_definition_body(inp, kw).map(scaffold::ExecutableDefinition::Fragment)
    }
    ParseAttempt::Decline => {
      operation_definition(inp).map(scaffold::ExecutableDefinition::Operation)
    }
  }
}

/// Parses an `ExecutableDocument` (`ExecutableDefinition+`).
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `ExecutableDefinition+` demands one-or-more, so an empty input errors here
/// where frozen's unenforced `+` accepted it. The first definition is committed
/// before the `list_of` rest, which runs to end of input.
///
/// Spec: [ExecutableDocument](https://spec.graphql.org/draft/#ExecutableDocument).
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
      Int = SliceOf<'inp, L>,
      Float = SliceOf<'inp, L>,
      InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = smear_lexer::LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  // Spec cardinality (`ExecutableDefinition+`): the first definition is committed, so
  // an empty input errors as end of input; the rest collect until end of input (no
  // stop token — a document has no closing delimiter).
  let first = executable_definition(inp)?;
  let mut defs = list_of(executable_definition, |_: &L::Token| false)(inp)?;
  defs.insert(0, first);
  let span = inp.span_since(&cursor);
  let doc = scaffold::Document::new(span, defs);
  Ok(doc)
}

#[cfg(test)]
mod tests;
