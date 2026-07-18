//! GraphQL type-system (SDL) definition productions — descriptions, input-value and
//! field definitions, the `implements`/union-member/directive-location clauses, enum
//! value definitions, root operation types, and the scalar/object/interface/union/
//! enum/input-object/directive/schema type definitions with their dispatch.
//!
//! # Headline deviation: `EnumValueDefinition` uses the `enum_value` atom
//!
//! [`enum_value_definition`] introduces its name through the
//! [`enum_value`] exclusion atom (`Name` but not
//! `true`/`false`/`null`), so `enum X { true }`, `enum X { false }`, and
//! `enum X { null }` all REJECT — where the frozen parser accepted them. Frozen's
//! `parse_enum_value_definition` used plain `parse_name`; its spec-enforcing
//! `parse_enum_value` had zero callers (the same dead-code pattern the fragment-name
//! exclusion showed in Wave 3). This is Deviations Register entry 1 (plan Ruling 3):
//! the frozen-parity oracle EXCLUDES these three rows, and
//! `enum_value_definition_rejects_true_false_null_per_spec` pins the rejection while a
//! positive row keeps every other soft keyword (`on`/`type`/`query`) legal as an enum
//! value.
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! Every `+` list is enforced non-empty. The brace/paren lists — `ArgumentsDefinition
//! ( InputValueDefinition+ )`, `FieldsDefinition { FieldDefinition+ }`,
//! `InputFieldsDefinition { InputValueDefinition+ }`, `EnumValuesDefinition {
//! EnumValueDefinition+ }`, and `SchemaDefinition`'s `{ RootOperationTypeDefinition+ }`
//! — commit the first element before the `list_of` rest (commas are trivia, so
//! `separated1` does not fit), so an empty `()`/`{}` errors: a documented deviation
//! from the frozen parser, whose unenforced `+` accepted the empty forms. The
//! pipe/amp lists — [`implements`] (`&`), [`union_members`] (`|`),
//! [`directive_locations`] (`|`) — use [`separated1`]
//! with `allow_leading` (already non-empty in frozen via `at_least(1)`).
//!
//! # Node placement
//!
//! Definitions retro-wrap their kind after the body settles (Amendment 1: content is
//! not known up front). The description-carrying productions ([`input_value_definition`],
//! [`field_definition`], [`enum_value_definition`]) mint the definition mark first, then
//! an inner `K::Description` node via [`description`], so the description lands inside
//! the definition node. The optional list clauses ([`fields_definition`],
//! [`input_fields_definition`], [`enum_values_definition`], [`implements`],
//! [`union_members`]) mint the mark before the attempt and spend it only when the clause
//! is actually present. The committed delimited regions ([`arguments_definition`],
//! [`root_operation_types_definition`]) open their kind up front with
//! [`node`] over the delimiter shape. [`type_definition`] adds no
//! wrapper of its own beyond the resolved arm's kind (sum-type convention), spent by a
//! content-dependent retro-wrap once the dispatch reveals which arm; [`described_type_definition`]
//! reuses that dispatch with a leading description landing inside the same node.

use smear_lexer::{LitBlockStr, LitInlineStr, keywords};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, ParseInput, SimpleSpan, Token,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, list_of, node, parens, try_braces},
  punct::{Ampersand, Pipe},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{
  directive::const_directives, executable::operation_type, peeks_where, ty::ty,
  value::default_value,
};
use crate::{
  combinator::{
    ErrorOf, LiteralValueToken, ParseCtx, SliceOf, StringLiteral, at, colon, enum_value, ident,
    separated1, try_description, try_equal,
  },
  graphql::{
    ast::{
      ArgumentsDefinition, DirectiveDefinition, DirectiveLocations, EnumTypeDefinition,
      EnumValueDefinition, EnumValuesDefinition, FieldDefinition, FieldsDefinition,
      ImplementInterfaces, InputFieldsDefinition, InputObjectTypeDefinition, InputValueDefinition,
      InterfaceTypeDefinition, Location, Name, ObjectTypeDefinition, RootOperationTypeDefinition,
      RootOperationTypesDefinition, ScalarTypeDefinition, SchemaDefinition, StringValue,
      TypeDefinition, UnionMemberTypes, UnionTypeDefinition,
    },
    keyword::{
      directive as directive_kw, r#enum as enum_kw, input as input_kw, interface, on, scalar,
      schema, try_enum, try_implements, try_input, try_interface, try_repeatable, try_scalar,
      try_type, try_union, r#type as type_kw, union,
    },
    kinds::SyntaxKind as K,
  },
};

// ─── shared leaf/error helpers ───────────────────────────────────────────────

/// Parses a bare `Name`, the item shape the `implements` and union-member clauses
/// separate. No node: a name is a leaf token (the enclosing clause carries the node).
fn name<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Name<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let (span, src) = ident(inp)?.into_components();
  Ok(Name::new(span, src))
}

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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      Err(UnexpectedToken::of(span).with_found(token).into())
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

// ─── description ─────────────────────────────────────────────────────────────

/// Parses an optional leading `Description` (a string literal), retro-wrapping it as a
/// `K::Description` node when present and declining to `None` (no tokens consumed)
/// otherwise.
///
/// The description-carrying definitions call this first, minting their own definition
/// mark before it, so the `K::Description` node nests inside the definition node.
/// Content is optional, so this uses the manual retro-wrap (Amendment 1) rather than
/// [`node_opt`](tokora::parser::node_opt).
///
/// Spec: [Description](https://spec.graphql.org/draft/#Description).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn description<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<StringValue<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<
      'inp,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
{
  let mark = inp.emitter().cst_mark();
  match try_description(inp)? {
    Some((lit, dspan)) => {
      let value = match lit {
        StringLiteral::Inline(inline) => StringValue::new(dspan, inline.into()),
        StringLiteral::Block(block) => StringValue::new(dspan, block.into()),
      };
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::Description.raw());
      emitter.cst_finish();
      Ok(Some(value))
    }
    None => Ok(None),
  }
}

// ─── input value / arguments definitions ─────────────────────────────────────

/// Parses an `InputValueDefinition`
/// (`Description? Name ':' Type DefaultValue? Directives?`), carrying the optional
/// leading description exactly as the frozen crate does (a [`Described`](crate::graphql::ast::Described) wrapper).
///
/// Spec: [InputValueDefinition](https://spec.graphql.org/draft/#InputValueDefinition).
pub fn input_value_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputValueDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let name = name(inp)?;
  colon(inp)?;
  let ty = ty(inp)?;
  let default = default_value(inp)?;
  let dirs = const_directives(inp)?;
  let span = inp.span_since(&cursor);
  let inner = scaffold::InputValueDefinition::new(span, name, ty, default, dirs);
  let described = scaffold::Described::new(span, desc, inner);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::InputValueDefinition.raw());
  emitter.cst_finish();
  Ok(described)
}

/// Parses an `ArgumentsDefinition` (`'(' InputValueDefinition+ ')'`).
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `InputValueDefinition+` demands one-or-more, so an empty `()` errors here
/// where frozen's unenforced `+` accepted it. The first definition is committed before
/// the `list_of` rest.
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition).
pub fn arguments_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ArgumentsDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  node(
    K::ArgumentsDefinition.raw(),
    parens(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
      let first = input_value_definition(inp)?;
      let mut items = list_of(
        input_value_definition,
        <L::Token as PunctuatorTokenExt>::is_close_paren,
      )(inp)?;
      items.insert(0, first);
      Ok(items)
    }),
  )
  .parse_input(inp)
  .map(|delimited| {
    let (span, _open, _close, items) = delimited.into_components();
    scaffold::ArgumentsDefinition::new(span, items)
  })
}

/// Parses an optional `ArgumentsDefinition`, declining to `None` (no tokens consumed)
/// unless the next token is `(`.
///
/// Spec: [ArgumentsDefinition](https://spec.graphql.org/draft/#ArgumentsDefinition).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn opt_arguments_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ArgumentsDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_paren)? {
    arguments_definition(inp).map(Some)
  } else {
    Ok(None)
  }
}

// ─── field / fields definition ───────────────────────────────────────────────

/// Parses a `FieldDefinition`
/// (`Description? Name ArgumentsDefinition? ':' Type Directives?`), carrying the
/// optional leading description (a [`Described`](crate::graphql::ast::Described) wrapper).
///
/// Spec: [FieldDefinition](https://spec.graphql.org/draft/#FieldDefinition).
pub fn field_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<FieldDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let name = name(inp)?;
  let args = opt_arguments_definition(inp)?;
  colon(inp)?;
  let ty = ty(inp)?;
  let dirs = const_directives(inp)?;
  let span = inp.span_since(&cursor);
  let inner = scaffold::FieldDefinition::new(span, name, args, ty, dirs);
  let described = scaffold::Described::new(span, desc, inner);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::FieldDefinition.raw());
  emitter.cst_finish();
  Ok(described)
}

/// Parses an optional `FieldsDefinition` (`'{' FieldDefinition+ '}'`), declining to
/// `None` (no tokens consumed) unless the next token is `{`.
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `FieldDefinition+` demands one-or-more, so an empty `{}` errors here where
/// frozen's unenforced `+` accepted it.
///
/// Spec: [FieldsDefinition](https://spec.graphql.org/draft/#FieldsDefinition).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn fields_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<FieldsDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  match try_braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let first = field_definition(inp)?;
    let mut items = list_of(
      field_definition,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, items) = delimited.into_components();
      let fields = scaffold::FieldsDefinition::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::FieldsDefinition.raw());
      emitter.cst_finish();
      Ok(Some(fields))
    }
    None => Ok(None),
  }
}

/// Parses an optional `InputFieldsDefinition` (`'{' InputValueDefinition+ '}'`),
/// declining to `None` (no tokens consumed) unless the next token is `{`.
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `InputValueDefinition+` demands one-or-more, so an empty `{}` errors here
/// where frozen's unenforced `+` accepted it.
///
/// Spec: [InputFieldsDefinition](https://spec.graphql.org/draft/#InputFieldsDefinition).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn input_fields_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<InputFieldsDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  match try_braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let first = input_value_definition(inp)?;
    let mut items = list_of(
      input_value_definition,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, items) = delimited.into_components();
      let fields = scaffold::InputFieldsDefinition::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::InputFieldsDefinition.raw());
      emitter.cst_finish();
      Ok(Some(fields))
    }
    None => Ok(None),
  }
}

// ─── implements / union members / directive locations ────────────────────────

/// Parses an optional `ImplementsInterfaces` clause (`'implements' '&'? Name ('&'
/// Name)*`), declining to `None` (no tokens consumed) unless `implements` is next.
///
/// The `&`-separated names use [`separated1`] with an
/// optional leading `&` (spec `implements &? …`), so the list is non-empty exactly as
/// the frozen parser enforced (`at_least(1)`).
///
/// Spec: [ImplementsInterfaces](https://spec.graphql.org/draft/#ImplementsInterfaces).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn implements<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ImplementInterfaces<Name<SliceOf<'inp, L>>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  match try_implements(inp)? {
    ParseAttempt::Accept(_kw) => {
      let items = separated1::<Ampersand<(), (), Lang>, _, _, _, _, _, _>(
        name,
        <L::Token as IdentifierToken>::is_identifier,
      )(inp)?;
      let span = inp.span_since(&cursor);
      let clause = scaffold::ImplementInterfaces::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::ImplementsInterfaces.raw());
      emitter.cst_finish();
      Ok(Some(clause))
    }
    ParseAttempt::Decline => Ok(None),
  }
}

/// Parses an optional `UnionMemberTypes` clause (`'=' '|'? Name ('|' Name)*`),
/// declining to `None` (no tokens consumed) unless `=` is next.
///
/// The `|`-separated names use [`separated1`] with an
/// optional leading `|` (spec `= |? …`), so the list is non-empty exactly as the
/// frozen parser enforced (`at_least(1)`).
///
/// Spec: [UnionMemberTypes](https://spec.graphql.org/draft/#UnionMemberTypes).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn union_members<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<UnionMemberTypes<Name<SliceOf<'inp, L>>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  match try_equal(inp)? {
    ParseAttempt::Accept(_eq) => {
      let items = separated1::<Pipe<(), (), Lang>, _, _, _, _, _, _>(
        name,
        <L::Token as IdentifierToken>::is_identifier,
      )(inp)?;
      let span = inp.span_since(&cursor);
      let clause = scaffold::UnionMemberTypes::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::UnionMemberTypes.raw());
      emitter.cst_finish();
      Ok(Some(clause))
    }
    ParseAttempt::Decline => Ok(None),
  }
}

/// Classifies one directive-location spelling against the spec's 19 fixed names,
/// building the typed [`Location`]; `None` for any other identifier.
fn classify_location(text: &[u8], span: SimpleSpan) -> Option<Location> {
  Some(match text {
    b"QUERY" => keywords::QueryLocation::new(span).into(),
    b"MUTATION" => keywords::MutationLocation::new(span).into(),
    b"SUBSCRIPTION" => keywords::SubscriptionLocation::new(span).into(),
    b"FIELD" => keywords::FieldLocation::new(span).into(),
    b"FRAGMENT_DEFINITION" => keywords::FragmentDefinitionLocation::new(span).into(),
    b"FRAGMENT_SPREAD" => keywords::FragmentSpreadLocation::new(span).into(),
    b"INLINE_FRAGMENT" => keywords::InlineFragmentLocation::new(span).into(),
    b"VARIABLE_DEFINITION" => keywords::VariableDefinitionLocation::new(span).into(),
    b"SCHEMA" => keywords::SchemaLocation::new(span).into(),
    b"SCALAR" => keywords::ScalarLocation::new(span).into(),
    b"OBJECT" => keywords::ObjectLocation::new(span).into(),
    b"FIELD_DEFINITION" => keywords::FieldDefinitionLocation::new(span).into(),
    b"ARGUMENT_DEFINITION" => keywords::ArgumentDefinitionLocation::new(span).into(),
    b"INTERFACE" => keywords::InterfaceLocation::new(span).into(),
    b"UNION" => keywords::UnionLocation::new(span).into(),
    b"ENUM_VALUE" => keywords::EnumValueLocation::new(span).into(),
    b"ENUM" => keywords::EnumLocation::new(span).into(),
    b"INPUT_OBJECT" => keywords::InputObjectLocation::new(span).into(),
    b"INPUT_FIELD_DEFINITION" => keywords::InputFieldDefinitionLocation::new(span).into(),
    _ => return None,
  })
}

/// Parses one directive `Location`: an identifier matched against the spec's fixed
/// location table. An unknown identifier — or a non-identifier token — is the
/// unexpected-token error (the dialect maps it to a `DirectiveLocation` expectation).
///
/// Spec: [DirectiveLocation](https://spec.graphql.org/draft/#DirectiveLocation).
fn location<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Location, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  SliceOf<'inp, L>: AsRef<[u8]>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  match inp.next()? {
    Some(spanned) => {
      if spanned.data().is_identifier() {
        let span = spanned.span();
        let text = inp.slice();
        match classify_location(text.as_ref(), span) {
          Some(loc) => Ok(loc),
          None => {
            let (span, tok) = spanned.into_components();
            Err(UnexpectedToken::of(span).with_found(tok).into())
          }
        }
      } else {
        let (span, tok) = spanned.into_components();
        Err(UnexpectedToken::of(span).with_found(tok).into())
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Parses a `DirectiveLocations` clause (`'|'? Location ('|' Location)*`).
///
/// The `|`-separated locations use [`separated1`] with
/// an optional leading `|` (spec `|? …`), so the list is non-empty exactly as the
/// frozen parser enforced (`at_least(1)`).
///
/// Spec: [DirectiveLocations](https://spec.graphql.org/draft/#DirectiveLocations).
pub fn directive_locations<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DirectiveLocations<Location>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: AsRef<[u8]>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let items = separated1::<Pipe<(), (), Lang>, _, _, _, _, _, _>(
    location,
    <L::Token as IdentifierToken>::is_identifier,
  )(inp)?;
  let span = inp.span_since(&cursor);
  let clause = scaffold::DirectiveLocations::new(span, items);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::DirectiveLocations.raw());
  emitter.cst_finish();
  Ok(clause)
}

// ─── enum value definitions ──────────────────────────────────────────────────

/// Parses an `EnumValueDefinition` (`Description? EnumValue Directives?`).
///
/// # Headline deviation (Deviations Register entry 1, plan Ruling 3)
///
/// The name is introduced through the [`enum_value`]
/// exclusion atom (`Name` but not `true`/`false`/`null`), so a value spelled `true`,
/// `false`, or `null` REJECTS — where the frozen parser accepted it (its
/// `parse_enum_value_definition` used plain `parse_name`, and its spec-enforcing
/// `parse_enum_value` had zero callers). Every other soft keyword (`on`, `type`,
/// `query`, …) stays legal.
///
/// Spec: [EnumValueDefinition](https://spec.graphql.org/draft/#EnumValueDefinition).
pub fn enum_value_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<EnumValueDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  // Headline deviation: the `enum_value` atom excludes `true`/`false`/`null`.
  let (nspan, nsrc) = enum_value(inp)?.into_components();
  let name = Name::new(nspan, nsrc);
  let dirs = const_directives(inp)?;
  let span = inp.span_since(&cursor);
  let inner = scaffold::EnumValueDefinition::new(span, name, dirs);
  let described = scaffold::Described::new(span, desc, inner);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::EnumValueDefinition.raw());
  emitter.cst_finish();
  Ok(described)
}

/// Parses an optional `EnumValuesDefinition` (`'{' EnumValueDefinition+ '}'`),
/// declining to `None` (no tokens consumed) unless the next token is `{`.
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `EnumValueDefinition+` demands one-or-more, so an empty `{}` errors here
/// where frozen's unenforced `+` accepted it.
///
/// Spec: [EnumValuesDefinition](https://spec.graphql.org/draft/#EnumValuesDefinition).
// The `Result<Option<…>, …>` return is inherent to an optional generic production.
#[allow(clippy::type_complexity)]
pub fn enum_values_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<EnumValuesDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  match try_braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let first = enum_value_definition(inp)?;
    let mut items = list_of(
      enum_value_definition,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, _open, _close, items) = delimited.into_components();
      let values = scaffold::EnumValuesDefinition::new(span, items);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::EnumValuesDefinition.raw());
      emitter.cst_finish();
      Ok(Some(values))
    }
    None => Ok(None),
  }
}

// ─── root operation types ────────────────────────────────────────────────────

/// Parses a `RootOperationTypeDefinition` (`OperationType ':' Name`).
///
/// Spec: [RootOperationTypeDefinition](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
pub fn root_operation_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<RootOperationTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let op = operation_type(inp)?;
  colon(inp)?;
  let name = name(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::RootOperationTypeDefinition::new(span, op, name);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::RootOperationTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses a `RootOperationTypesDefinition` (`'{' RootOperationTypeDefinition+ '}'`).
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `RootOperationTypeDefinition+` demands one-or-more, so an empty `{}` errors
/// here where frozen's unenforced `+` accepted it.
///
/// Spec: [RootOperationTypeDefinition](https://spec.graphql.org/draft/#RootOperationTypeDefinition).
pub fn root_operation_types_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<RootOperationTypesDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  node(
    K::RootOperationTypeDefinitions.raw(),
    braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
      let first = root_operation_type_definition(inp)?;
      let mut items = list_of(
        root_operation_type_definition,
        <L::Token as PunctuatorTokenExt>::is_close_brace,
      )(inp)?;
      items.insert(0, first);
      Ok(items)
    }),
  )
  .parse_input(inp)
  .map(|delimited| {
    let (span, _open, _close, items) = delimited.into_components();
    scaffold::RootOperationTypesDefinition::new(span, items)
  })
}

// ─── type-definition bodies (after the leading keyword) ──────────────────────

/// Parses a scalar type definition's body after its `scalar` keyword, spanning from
/// `kw_start`. No node: the caller retro-wraps the resolved kind.
fn scalar_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<ScalarTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let dirs = const_directives(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::ScalarTypeDefinition::new(span, name, dirs))
}

/// Parses an object type definition's body after its `type` keyword, spanning from
/// `kw_start`. No node: the caller retro-wraps the resolved kind.
fn object_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<ObjectTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let impls = implements(inp)?;
  let dirs = const_directives(inp)?;
  let fields = fields_definition(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::ObjectTypeDefinition::new(
    span, name, impls, dirs, fields,
  ))
}

/// Parses an interface type definition's body after its `interface` keyword, spanning
/// from `kw_start`. No node: the caller retro-wraps the resolved kind.
fn interface_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<InterfaceTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let impls = implements(inp)?;
  let dirs = const_directives(inp)?;
  let fields = fields_definition(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::InterfaceTypeDefinition::new(
    span, name, impls, dirs, fields,
  ))
}

/// Parses a union type definition's body after its `union` keyword, spanning from
/// `kw_start`. No node: the caller retro-wraps the resolved kind.
fn union_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<UnionTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let dirs = const_directives(inp)?;
  let members = union_members(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::UnionTypeDefinition::new(
    span, name, dirs, members,
  ))
}

/// Parses an enum type definition's body after its `enum` keyword, spanning from
/// `kw_start`. No node: the caller retro-wraps the resolved kind.
fn enum_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<EnumTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let dirs = const_directives(inp)?;
  let values = enum_values_definition(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::EnumTypeDefinition::new(span, name, dirs, values))
}

/// Parses an input object type definition's body after its `input` keyword, spanning
/// from `kw_start`. No node: the caller retro-wraps the resolved kind.
fn input_object_after_kw<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
) -> Result<InputObjectTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let name = name(inp)?;
  let dirs = const_directives(inp)?;
  let fields = input_fields_definition(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  Ok(scaffold::InputObjectTypeDefinition::new(
    span, name, dirs, fields,
  ))
}

// ─── standalone type-system definitions ──────────────────────────────────────

/// Parses a `ScalarTypeDefinition` (`scalar Name Directives?`).
///
/// Spec: [ScalarTypeDefinition](https://spec.graphql.org/draft/#ScalarTypeDefinition).
pub fn scalar_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ScalarTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = scalar(inp)?;
  let def = scalar_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ScalarTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `ObjectTypeDefinition`
/// (`type Name ImplementsInterfaces? Directives? FieldsDefinition?`).
///
/// Spec: [ObjectTypeDefinition](https://spec.graphql.org/draft/#ObjectTypeDefinition).
pub fn object_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ObjectTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = type_kw(inp)?;
  let def = object_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ObjectTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `InterfaceTypeDefinition`
/// (`interface Name ImplementsInterfaces? Directives? FieldsDefinition?`).
///
/// Spec: [InterfaceTypeDefinition](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
pub fn interface_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InterfaceTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = interface(inp)?;
  let def = interface_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::InterfaceTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses a `UnionTypeDefinition` (`union Name Directives? UnionMemberTypes?`).
///
/// Spec: [UnionTypeDefinition](https://spec.graphql.org/draft/#UnionTypeDefinition).
pub fn union_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<UnionTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = union(inp)?;
  let def = union_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::UnionTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `EnumTypeDefinition` (`enum Name Directives? EnumValuesDefinition?`).
///
/// Spec: [EnumTypeDefinition](https://spec.graphql.org/draft/#EnumTypeDefinition).
pub fn enum_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<EnumTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = enum_kw(inp)?;
  let def = enum_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::EnumTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `InputObjectTypeDefinition`
/// (`input Name Directives? InputFieldsDefinition?`).
///
/// Spec: [InputObjectTypeDefinition](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
pub fn input_object_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputObjectTypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = input_kw(inp)?;
  let def = input_object_after_kw(inp, kw.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::InputObjectTypeDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses a `DirectiveDefinition`
/// (`directive '@' Name ArgumentsDefinition? repeatable? 'on' DirectiveLocations`).
///
/// `repeatable` is a plain flag (the frozen `bool` field): present iff the soft
/// `repeatable` keyword follows the arguments.
///
/// Spec: [DirectiveDefinition](https://spec.graphql.org/draft/#DirectiveDefinition).
pub fn directive_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DirectiveDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  directive_kw(inp)?;
  at(inp)?;
  let name = name(inp)?;
  let args = opt_arguments_definition(inp)?;
  let repeatable = matches!(try_repeatable(inp)?, ParseAttempt::Accept(_));
  on(inp)?;
  let locations = directive_locations(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::DirectiveDefinition::new(span, name, args, repeatable, locations);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::DirectiveDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses a `SchemaDefinition` (`schema Directives? RootOperationTypesDefinition`).
///
/// Spec: [SchemaDefinition](https://spec.graphql.org/draft/#SchemaDefinition).
pub fn schema_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<SchemaDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  schema(inp)?;
  let dirs = const_directives(inp)?;
  let ops = root_operation_types_definition(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::SchemaDefinition::new(span, dirs, ops);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::SchemaDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

// ─── type-definition dispatch ────────────────────────────────────────────────

/// Dispatches on the leading soft keyword to the matching type-definition body,
/// consuming the keyword through the declining `try_*` atoms. No node: the caller
/// retro-wraps the resolved arm's kind (sum-type convention).
#[allow(clippy::type_complexity)]
fn dispatch_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  if let ParseAttempt::Accept(kw) = try_scalar(inp)? {
    return scalar_after_kw(inp, kw.span().start()).map(TypeDefinition::Scalar);
  }
  if let ParseAttempt::Accept(kw) = try_type(inp)? {
    return object_after_kw(inp, kw.span().start()).map(TypeDefinition::Object);
  }
  if let ParseAttempt::Accept(kw) = try_interface(inp)? {
    return interface_after_kw(inp, kw.span().start()).map(TypeDefinition::Interface);
  }
  if let ParseAttempt::Accept(kw) = try_union(inp)? {
    return union_after_kw(inp, kw.span().start()).map(TypeDefinition::Union);
  }
  if let ParseAttempt::Accept(kw) = try_enum(inp)? {
    return enum_after_kw(inp, kw.span().start()).map(TypeDefinition::Enum);
  }
  if let ParseAttempt::Accept(kw) = try_input(inp)? {
    return input_object_after_kw(inp, kw.span().start()).map(TypeDefinition::InputObject);
  }
  unexpected(inp)
}

/// The `K::…` kind the resolved [`TypeDefinition`] arm materializes as.
fn type_definition_kind<S, Ty>(def: &TypeDefinition<S, Ty>) -> u16 {
  match def {
    TypeDefinition::Scalar(_) => K::ScalarTypeDefinition.raw(),
    TypeDefinition::Object(_) => K::ObjectTypeDefinition.raw(),
    TypeDefinition::Interface(_) => K::InterfaceTypeDefinition.raw(),
    TypeDefinition::Union(_) => K::UnionTypeDefinition.raw(),
    TypeDefinition::Enum(_) => K::EnumTypeDefinition.raw(),
    TypeDefinition::InputObject(_) => K::InputObjectTypeDefinition.raw(),
  }
}

/// Parses a `TypeDefinition` by dispatching on the leading soft keyword
/// (`scalar`/`type`/`interface`/`union`/`enum`/`input`).
///
/// No wrapper node of its own beyond the resolved arm's kind (sum-type convention):
/// the dispatch reveals which arm, and the mark — minted before the keyword — is spent
/// as that arm's kind (content-dependent retro-wrap, Amendment 1).
///
/// Spec: [TypeDefinition](https://spec.graphql.org/draft/#TypeDefinition).
pub fn type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let def = dispatch_type_definition(inp)?;
  let kind = type_definition_kind(&def);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind);
  emitter.cst_finish();
  Ok(def)
}

/// Parses a `TypeDefinition` preceded by an optional `Description`, the described
/// dispatch the plan's node-placement convention calls the described-definition
/// retro-wrap: mark first, then the description (a `K::Description` node), then the
/// keyword dispatch, then the resolved arm's kind spent over the whole region — so the
/// description lands inside the definition node.
///
/// Spec: [TypeDefinition](https://spec.graphql.org/draft/#TypeDefinition) (described).
// The `Result<Described<…>, …>` return is inherent to this generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn described_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  scaffold::Described<TypeDefinition<SliceOf<'inp, L>>, StringValue<SliceOf<'inp, L>>>,
  ErrorOf<'inp, L, Ctx, Lang>,
>
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
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let mark = inp.emitter().cst_mark();
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let def = dispatch_type_definition(inp)?;
  let kind = type_definition_kind(&def);
  let span = inp.span_since(&cursor);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind);
  emitter.cst_finish();
  Ok(scaffold::Described::new(span, desc, def))
}

#[cfg(test)]
mod tests;
