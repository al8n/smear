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
//! [`directive_locations`] (`|`) — commit their first item after an optional
//! leading separator and continue only through real separators (already non-empty
//! in frozen via `at_least(1)`): `separated1`'s peek-driven continuation treats an
//! adjacent identifier as a missing separator, but at these list tails an adjacent
//! identifier legitimately starts the NEXT definition.
use std::vec::Vec;

use smear_lexer::{LitBlockStr, LitInlineStr, keywords};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, list_of, parens, try_braces},
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
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, StringLiteral, at, colon,
    enum_value, ident, try_ampersand, try_description, try_equal, try_pipe,
  },
  graphql::{
    ast::{
      ArgumentsDefinition, DescribedEnumTypeDefinition, DescribedInputObjectTypeDefinition,
      DescribedInterfaceTypeDefinition, DescribedObjectTypeDefinition, DirectiveDefinition,
      DirectiveLocations, EnumTypeDefinition, EnumValueDefinition, EnumValuesDefinition,
      FieldDefinition, FieldsDefinition, ImplementInterfaces, InputFieldsDefinition,
      InputObjectTypeDefinition, InputValueDefinition, InterfaceTypeDefinition, Location, Name,
      ObjectTypeDefinition, RootOperationTypeDefinition, RootOperationTypesDefinition,
      ScalarTypeDefinition, SchemaDefinition, StringValue, TypeDefinition, UnionMemberTypes,
      UnionTypeDefinition,
    },
    keyword::{
      directive as directive_kw, r#enum as enum_kw, input as input_kw, interface, on, scalar,
      schema, try_enum, try_implements, try_input, try_interface, try_repeatable, try_scalar,
      try_type, try_union, r#type as type_kw, union,
    },
  },
};

// ─── shared leaf/error helpers ───────────────────────────────────────────────

/// Parses a bare `Name`, the item shape the `implements` and union-member clauses
/// separate.
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

/// Parses an optional leading `Description` (a string literal), declining to `None`
/// (no tokens consumed) otherwise.
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
{
  match try_description(inp)? {
    Some((lit, dspan)) => {
      let value = match lit {
        StringLiteral::Inline(inline) => StringValue::new(dspan, inline.into()),
        StringLiteral::Block(block) => StringValue::new(dspan, block.into()),
      };
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  parens(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let first = input_value_definition(inp)?;
    let mut items = list_of(
      input_value_definition,
      <L::Token as PunctuatorTokenExt>::is_close_paren,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
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
      Ok(Some(fields))
    }
    None => Ok(None),
  }
}

// ─── implements / union members / directive locations ────────────────────────

/// Parses an optional `ImplementsInterfaces` clause (`'implements' '&'? Name ('&'
/// Name)*`), declining to `None` (no tokens consumed) unless `implements` is next.
///
/// The `&`-separated names commit the first after an optional leading `&` (spec
/// `implements &? …`) and continue only through real `&` separators, so the list is
/// non-empty exactly as the frozen parser enforced (`at_least(1)`) and ends cleanly
/// at any non-`&` token.
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
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  match try_implements(inp)? {
    ParseAttempt::Accept(_kw) => {
      // `&`-separated names, optional leading `&` (spec `implements &? …`), first
      // name committed (non-empty), each further name committed after its `&` (a
      // trailing `&` errors), and any non-`&` token cleanly ENDS the clause — an
      // adjacent identifier belongs to whatever follows (the next definition),
      // never to this list. `separated1`'s peek-driven continuation treats a
      // following identifier as a missing separator, so it does not fit here.
      let mut items = Vec::new();
      let _ = try_ampersand(inp)?;
      items.push(name(inp)?);
      while let ParseAttempt::Accept(_amp) = try_ampersand(inp)? {
        items.push(name(inp)?);
      }
      let span = inp.span_since(&cursor);
      let clause = scaffold::ImplementInterfaces::new(span, items);
      Ok(Some(clause))
    }
    ParseAttempt::Decline => Ok(None),
  }
}

/// Parses an optional `UnionMemberTypes` clause (`'=' '|'? Name ('|' Name)*`),
/// declining to `None` (no tokens consumed) unless `=` is next.
///
/// The `|`-separated names commit the first after an optional leading `|` (spec
/// `= |? …`) and continue only through real `|` separators, so the list is non-empty
/// exactly as the frozen parser enforced (`at_least(1)`) and ends cleanly at any
/// non-`|` token.
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
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  match try_equal(inp)? {
    ParseAttempt::Accept(_eq) => {
      // `|`-separated names, optional leading `|` (spec `= |? …`), first member
      // committed (non-empty), each further member committed after its `|` (a
      // trailing `|` errors), and any non-`|` token cleanly ENDS the clause — an
      // adjacent identifier starts the NEXT definition (`union A = X | Y union B`),
      // never another member. `separated1`'s peek-driven continuation treats that
      // identifier as a missing separator, so it does not fit here.
      let mut items = Vec::new();
      let _ = try_pipe(inp)?;
      items.push(name(inp)?);
      while let ParseAttempt::Accept(_pipe) = try_pipe(inp)? {
        items.push(name(inp)?);
      }
      let span = inp.span_since(&cursor);
      let clause = scaffold::UnionMemberTypes::new(span, items);
      Ok(Some(clause))
    }
    ParseAttempt::Decline => Ok(None),
  }
}

/// Classifies one directive-location spelling against the spec's 19 fixed names,
/// building the typed [`Location`]; `None` for any other identifier.
fn classify_location<S>(text: &S, span: SimpleSpan) -> Option<Location>
where
  S: Equivalent<str>,
{
  // Each spec spelling maps to its typed location; `equivalent` compares the source
  // slice against the spelling across flavors, so no `AsRef<[u8]>` pins the source.
  macro_rules! classify {
    ($($spelling:literal => $loc:ident),+ $(,)?) => {{
      $(
        if text.equivalent($spelling) {
          return Some(keywords::$loc::new(span).into());
        }
      )+
      None
    }};
  }
  classify! {
    "QUERY" => QueryLocation,
    "MUTATION" => MutationLocation,
    "SUBSCRIPTION" => SubscriptionLocation,
    "FIELD" => FieldLocation,
    "FRAGMENT_DEFINITION" => FragmentDefinitionLocation,
    "FRAGMENT_SPREAD" => FragmentSpreadLocation,
    "INLINE_FRAGMENT" => InlineFragmentLocation,
    "VARIABLE_DEFINITION" => VariableDefinitionLocation,
    "SCHEMA" => SchemaLocation,
    "SCALAR" => ScalarLocation,
    "OBJECT" => ObjectLocation,
    "FIELD_DEFINITION" => FieldDefinitionLocation,
    "ARGUMENT_DEFINITION" => ArgumentDefinitionLocation,
    "INTERFACE" => InterfaceLocation,
    "UNION" => UnionLocation,
    "ENUM_VALUE" => EnumValueLocation,
    "ENUM" => EnumLocation,
    "INPUT_OBJECT" => InputObjectLocation,
    "INPUT_FIELD_DEFINITION" => InputFieldDefinitionLocation,
  }
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
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  match inp.next()? {
    Some(spanned) => {
      if spanned.data().is_identifier() {
        let span = spanned.span();
        let text = inp.slice();
        match classify_location(&text, span) {
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
/// The `|`-separated locations commit the first after an optional leading `|`
/// (spec `|? …`) and continue only through real `|` separators, so the list is
/// non-empty exactly as the frozen parser enforced (`at_least(1)`) and ends cleanly
/// at any non-`|` token.
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
  SliceOf<'inp, L>: Equivalent<str>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  // `|`-separated locations, optional leading `|` (spec `|? …`), first location
  // committed (non-empty), each further location committed after its `|` (a
  // trailing `|` errors), and any non-`|` token cleanly ENDS the clause — an
  // adjacent identifier starts the NEXT definition (`directive @a on FIELD type
  // T`), never another location. `separated1`'s peek-driven continuation treats
  // that identifier as a missing separator, so it does not fit here.
  let mut items = Vec::new();
  let _ = try_pipe(inp)?;
  items.push(location(inp)?);
  while let ParseAttempt::Accept(_pipe) = try_pipe(inp)? {
    items.push(location(inp)?);
  }
  let span = inp.span_since(&cursor);
  let clause = scaffold::DirectiveLocations::new(span, items);
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  // Headline deviation: the `enum_value` atom excludes `true`/`false`/`null`.
  let (nspan, nsrc) = enum_value(inp)?.into_components();
  let name = Name::new(nspan, nsrc);
  let dirs = const_directives(inp)?;
  let span = inp.span_since(&cursor);
  let inner = scaffold::EnumValueDefinition::new(span, name, dirs);
  let described = scaffold::Described::new(span, desc, inner);
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
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
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let op = operation_type(inp)?;
  colon(inp)?;
  let name = name(inp)?;
  let span = inp.span_since(&cursor);
  let def = scaffold::RootOperationTypeDefinition::new(span, op, name);
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
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    let first = root_operation_type_definition(inp)?;
    let mut items = list_of(
      root_operation_type_definition,
      <L::Token as PunctuatorTokenExt>::is_close_brace,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)
  .map(|delimited| {
    let (span, _open, _close, items) = delimited.into_components();
    scaffold::RootOperationTypesDefinition::new(span, items)
  })
}

// ─── type-definition bodies (after the leading keyword) ──────────────────────

/// Parses a scalar type definition's body after its `scalar` keyword, spanning from
/// `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
/// `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
/// from `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
/// `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
/// `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
/// from `kw_start`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = scalar(inp)?;
  let def = scalar_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = type_kw(inp)?;
  let def = object_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = interface(inp)?;
  let def = interface_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = union(inp)?;
  let def = union_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = enum_kw(inp)?;
  let def = enum_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = input_kw(inp)?;
  let def = input_object_after_kw(inp, kw.span().start())?;
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = directive_kw(inp)?;
  directive_definition_body(inp, kw.span().start())
}

/// Parses a directive definition's body after its `directive` keyword has been
/// consumed.
///
/// Shared by [`directive_definition`] and the document-level definition dispatches
/// (the W3 `fragment_definition_body` convergence pattern — a soft keyword cannot be
/// peeked without consuming).
pub(super) fn directive_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  at(inp)?;
  let name = name(inp)?;
  let args = opt_arguments_definition(inp)?;
  let repeatable = matches!(try_repeatable(inp)?, ParseAttempt::Accept(_));
  on(inp)?;
  let locations = directive_locations(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  let def = scaffold::DirectiveDefinition::new(span, name, args, repeatable, locations);
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let kw = schema(inp)?;
  schema_definition_body(inp, kw.span().start())
}

/// Parses a schema definition's body after its `schema` keyword has been consumed.
///
/// Shared by [`schema_definition`] and the document-level definition dispatches
/// (the W3 `fragment_definition_body` convergence pattern).
pub(super) fn schema_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw_start: usize,
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let dirs = const_directives(inp)?;
  let ops = root_operation_types_definition(inp)?;
  let span = SimpleSpan::new(kw_start, inp.span_since(&cursor).end());
  let def = scaffold::SchemaDefinition::new(span, dirs, ops);
  Ok(def)
}

// ─── type-definition dispatch ────────────────────────────────────────────────

/// Dispatches on the leading soft keyword to the matching type-definition body,
/// consuming the keyword through the declining `try_*` atoms, declining to `None`
/// (no tokens consumed) when the next token opens none of the six shapes.
// The `Result<Option<…>, …>` return is inherent to a declining generic dispatch.
#[allow(clippy::type_complexity)]
pub(super) fn try_dispatch_type_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<TypeDefinition<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  if let ParseAttempt::Accept(kw) = try_scalar(inp)? {
    return scalar_after_kw(inp, kw.span().start())
      .map(TypeDefinition::Scalar)
      .map(Some);
  }
  if let ParseAttempt::Accept(kw) = try_type(inp)? {
    return object_after_kw(inp, kw.span().start())
      .map(TypeDefinition::Object)
      .map(Some);
  }
  if let ParseAttempt::Accept(kw) = try_interface(inp)? {
    return interface_after_kw(inp, kw.span().start())
      .map(TypeDefinition::Interface)
      .map(Some);
  }
  if let ParseAttempt::Accept(kw) = try_union(inp)? {
    return union_after_kw(inp, kw.span().start())
      .map(TypeDefinition::Union)
      .map(Some);
  }
  if let ParseAttempt::Accept(kw) = try_enum(inp)? {
    return enum_after_kw(inp, kw.span().start())
      .map(TypeDefinition::Enum)
      .map(Some);
  }
  if let ParseAttempt::Accept(kw) = try_input(inp)? {
    return input_object_after_kw(inp, kw.span().start())
      .map(TypeDefinition::InputObject)
      .map(Some);
  }
  Ok(None)
}

/// Dispatches on the leading soft keyword to the matching type-definition body,
/// erroring (committed) when the next token opens none of the six shapes.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  match try_dispatch_type_definition(inp)? {
    Some(def) => Ok(def),
    None => unexpected(inp),
  }
}

/// Parses a `TypeDefinition` by dispatching on the leading soft keyword
/// (`scalar`/`type`/`interface`/`union`/`enum`/`input`).
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  dispatch_type_definition(inp)
}

/// Parses a `TypeDefinition` preceded by an optional `Description`.
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
{
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let def = dispatch_type_definition(inp)?;
  let span = inp.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, def))
}

/// Declares one keyword-committed described standalone production: description →
/// the committed shape keyword → the shared `*_after_kw` body, mirroring the frozen
/// crate's standalone `parse_described_*` entry roots.
macro_rules! described_standalone {
  ($(#[$meta:meta])* $name:ident => $kw:ident, $body:ident, $out:ident) => {
    $(#[$meta])*
    // The `Result<Described<…>, …>` return is inherent to this generic production.
    #[allow(clippy::type_complexity)]
    pub fn $name<'inp, L, Ctx, Lang>(
      inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
    ) -> Result<$out<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
      SliceOf<'inp, L>: Equivalent<str> + Clone,
      ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
        + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
      <L::Token as Token<'inp>>::Kind: From<Ampersand<(), (), ()>> + From<Pipe<(), (), ()>>,
    {
      let cursor = inp.cursor().clone();
      let desc = description(inp)?;
      let kw = $kw(inp)?;
      let def = $body(inp, kw.span().start())?;
      let span = inp.span_since(&cursor);
      Ok(scaffold::Described::new(span, desc, def))
    }
  };
}

described_standalone!(
  /// Parses a described `ObjectTypeDefinition` (`Description? type Name …`).
  ///
  /// Spec: [ObjectTypeDefinition](https://spec.graphql.org/draft/#ObjectTypeDefinition).
  described_object_type_definition => type_kw, object_after_kw, DescribedObjectTypeDefinition
);

described_standalone!(
  /// Parses a described `InterfaceTypeDefinition` (`Description? interface Name …`).
  ///
  /// Spec: [InterfaceTypeDefinition](https://spec.graphql.org/draft/#InterfaceTypeDefinition).
  described_interface_type_definition => interface, interface_after_kw,
  DescribedInterfaceTypeDefinition
);

described_standalone!(
  /// Parses a described `EnumTypeDefinition` (`Description? enum Name …`).
  ///
  /// Spec: [EnumTypeDefinition](https://spec.graphql.org/draft/#EnumTypeDefinition).
  described_enum_type_definition => enum_kw, enum_after_kw, DescribedEnumTypeDefinition
);

described_standalone!(
  /// Parses a described `InputObjectTypeDefinition` (`Description? input Name …`).
  ///
  /// Spec: [InputObjectTypeDefinition](https://spec.graphql.org/draft/#InputObjectTypeDefinition).
  described_input_object_type_definition => input_kw, input_object_after_kw,
  DescribedInputObjectTypeDefinition
);

#[cfg(test)]
mod tests;
