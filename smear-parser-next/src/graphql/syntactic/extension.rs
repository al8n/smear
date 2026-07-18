//! GraphQL type-system (SDL) extension productions — the seven `extend` shapes
//! (scalar/object/interface/union/enum/input-object type extensions and the schema
//! extension) with their dispatches.
//!
//! Every extension opens with the soft `extend` keyword; the next soft keyword
//! selects the shape. Since a soft keyword cannot be peeked without consuming, the
//! standalone productions consume `extend` + their own keyword committedly, and the
//! dispatches ([`type_extension`], [`type_system_extension`]) consume `extend` then
//! chain the declining `try_*` keyword atoms (the W3/W4 pattern), converging with the
//! standalone productions on shared per-shape bodies.
//!
//! # Spec cardinality (plan Amendment 2, extension audit)
//!
//! Each extension shape is a spec "one of" form — the extension must actually extend
//! *something* — audited variant by variant against the spec grammar:
//!
//! - [`scalar_type_extension`]: `extend scalar Name Directives[Const]` — directives
//!   are REQUIRED (the spec form has no `?`). A bare `extend scalar Name` errors here
//!   where the frozen parser accepted it by fabricating an empty directives list
//!   (a documented deviation, with a rejection regression).
//! - [`object_type_extension`] / [`interface_type_extension`]: at least one of
//!   `ImplementsInterfaces` / `Directives[Const]` / `FieldsDefinition` must be
//!   present (the spec's three forms). The implements-only form (`extend type Name
//!   implements I`) is spec-legal and ACCEPTED here — the frozen parser rejected it
//!   even though the scaffold AST models it (`…ExtensionData::Implements`), the same
//!   spec-correct-relaxation class as W3's operation-name `on` ruling. A bare
//!   `extend type Name` errors (spec + frozen agree).
//! - [`union_type_extension`]: at least one of directives / `UnionMemberTypes`
//!   (frozen parity — already enforced there).
//! - [`enum_type_extension`]: at least one of directives / `EnumValuesDefinition`
//!   (frozen parity).
//! - [`input_object_type_extension`]: at least one of directives /
//!   `InputFieldsDefinition` (frozen parity).
//! - [`schema_extension`]: at least one of directives / `{ RootOperationTypeDefinition+ }`
//!   (frozen parity; the `+` inside the braces is enforced by
//!   [`root_operation_types_definition`] — registered in Wave 4).
//!
//! The brace/paren lists an extension reuses ([`fields_definition`],
//! [`input_fields_definition`], [`enum_values_definition`],
//! [`root_operation_types_definition`]) carry their Wave-4 non-emptiness deviations
//! into the extension positions; `extend enum E { true }` likewise inherits the
//! Wave-4 `enum_value` exclusion (Deviations Register entry 1).
//!
//! # Span note (minor, no accept/reject impact)
//!
//! Extension spans start at the `extend` keyword — the scaffold nodes document their
//! span as covering "the `extend` keyword through all content". The frozen parser
//! anchored the span *after* `extend` (at the shape keyword), contradicting the
//! scaffold contract; parser-next follows the scaffold contract, consistent with the
//! Wave-4 definitions whose spans start at their leading keyword.
//!
//! # Node placement
//!
//! Extensions retro-wrap their kind after the body settles (Amendment 1): the mark is
//! minted before `extend`, so the extension node covers the whole `extend …` region.
//! The dispatches add no wrapper of their own beyond the resolved arm's kind
//! (sum-type convention).

use smear_lexer::{LitBlockStr, LitInlineStr};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  cst::event::EventMark,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  punct::{Ampersand, Pipe},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{
  definition::{
    enum_values_definition, fields_definition, implements, input_fields_definition,
    root_operation_types_definition, union_members,
  },
  directive::const_directives,
  peeks_where,
};
use crate::{
  combinator::{ErrorOf, LiteralValueToken, ParseCtx, SliceOf, ident},
  graphql::{
    ast::{
      EnumTypeExtension, InputObjectTypeExtension, InterfaceTypeExtension, Name,
      ObjectTypeExtension, ScalarTypeExtension, SchemaExtension, TypeExtension,
      TypeSystemExtension, UnionTypeExtension,
    },
    keyword::{
      r#enum as enum_kw, extend, input as input_kw, interface, scalar, schema, try_enum, try_input,
      try_interface, try_scalar, try_schema, try_type, try_union, r#type as type_kw, union,
    },
    kinds::SyntaxKind as K,
  },
};

// ─── shared leaf/error helpers ───────────────────────────────────────────────

/// Parses a bare `Name` (the extended type's name). No node: a name is a leaf token.
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

// ─── extension bodies (after `extend` + the shape keyword) ───────────────────

/// Parses a scalar type extension's body after `extend scalar`, spanning from
/// `ext_start` (the `extend` keyword). No node: the caller retro-wraps the kind.
///
/// Deviation (spec-cardinality audit): the spec form `extend scalar Name
/// Directives[Const]` REQUIRES the directives, so their absence errors here — the
/// frozen parser accepted a bare `extend scalar Name`, fabricating an empty
/// directives list.
fn scalar_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<ScalarTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  // Spec: `extend scalar Name Directives[Const]` — the directives are required.
  let dirs = match const_directives(inp)? {
    Some(dirs) => dirs,
    None => return unexpected(inp),
  };
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  Ok(scaffold::ScalarTypeExtension::new(span, name, dirs))
}

/// Parses an object type extension's body after `extend type`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// At least one of implements/directives/fields must be present (the spec's three
/// "one of" forms); the implements-only form is accepted (spec-correct relaxation —
/// the frozen parser rejected it).
fn object_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<ObjectTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (impls, dirs, fields) {
    (implements, directives, Some(fields)) => scaffold::ObjectTypeExtensionData::Fields {
      implements,
      directives,
      fields,
    },
    (implements, Some(directives), None) => scaffold::ObjectTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => scaffold::ObjectTypeExtensionData::Implements(implements),
    (None, None, None) => return unexpected(inp),
  };
  Ok(scaffold::ObjectTypeExtension::new(span, name, data))
}

/// Parses an interface type extension's body after `extend interface`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// Same "one of" audit as [`object_extension_body`], including the accepted
/// implements-only form.
fn interface_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<InterfaceTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (impls, dirs, fields) {
    (implements, directives, Some(fields)) => scaffold::InterfaceTypeExtensionData::Fields {
      implements,
      directives,
      fields,
    },
    (implements, Some(directives), None) => scaffold::InterfaceTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => scaffold::InterfaceTypeExtensionData::Implements(implements),
    (None, None, None) => return unexpected(inp),
  };
  Ok(scaffold::InterfaceTypeExtension::new(span, name, data))
}

/// Parses a union type extension's body after `extend union`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// At least one of directives/member types must be present (frozen parity).
fn union_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<UnionTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (dirs, members) {
    (directives, Some(members)) => scaffold::UnionTypeExtensionData::Members {
      directives,
      members,
    },
    (Some(directives), None) => scaffold::UnionTypeExtensionData::Directives(directives),
    (None, None) => return unexpected(inp),
  };
  Ok(scaffold::UnionTypeExtension::new(span, name, data))
}

/// Parses an enum type extension's body after `extend enum`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// At least one of directives/values must be present (frozen parity). The values
/// block inherits the Wave-4 `enum_value` exclusion (`extend enum E { true }` errors).
fn enum_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<EnumTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (dirs, values) {
    (directives, Some(values)) => scaffold::EnumTypeExtensionData::Values { directives, values },
    (Some(directives), None) => scaffold::EnumTypeExtensionData::Directives(directives),
    (None, None) => return unexpected(inp),
  };
  Ok(scaffold::EnumTypeExtension::new(span, name, data))
}

/// Parses an input object type extension's body after `extend input`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// At least one of directives/fields must be present (frozen parity).
fn input_object_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<InputObjectTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (dirs, fields) {
    (directives, Some(fields)) => {
      scaffold::InputObjectTypeExtensionData::Fields { directives, fields }
    }
    (Some(directives), None) => scaffold::InputObjectTypeExtensionData::Directives(directives),
    (None, None) => return unexpected(inp),
  };
  Ok(scaffold::InputObjectTypeExtension::new(span, name, data))
}

/// Parses a schema extension's body after `extend schema`, spanning from
/// `ext_start`. No node: the caller retro-wraps the kind.
///
/// At least one of directives/root-operation-types must be present (frozen parity);
/// the `{ RootOperationTypeDefinition+ }` block's non-emptiness rides
/// [`root_operation_types_definition`] (Wave 4).
fn schema_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<SchemaExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let dirs = const_directives(inp)?;
  let ops = if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    Some(root_operation_types_definition(inp)?)
  } else {
    None
  };
  let span = SimpleSpan::new(ext_start, inp.span_since(&cursor).end());
  let data = match (dirs, ops) {
    (directives, Some(definitions)) => scaffold::SchemaExtensionData::Operations {
      directives,
      definitions,
    },
    (Some(directives), None) => scaffold::SchemaExtensionData::Directives(directives),
    (None, None) => return unexpected(inp),
  };
  Ok(scaffold::SchemaExtension::new(span, data))
}

// ─── standalone extension productions ────────────────────────────────────────

/// Parses a `ScalarTypeExtension` (`extend scalar Name Directives[Const]`).
///
/// Deviation (spec-cardinality audit): the directives are required — the spec form
/// has no `?`, so a bare `extend scalar Name` errors here where the frozen parser
/// accepted it by fabricating an empty directives list.
///
/// Spec: [ScalarTypeExtension](https://spec.graphql.org/draft/#ScalarTypeExtension).
pub fn scalar_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ScalarTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  scalar(inp)?;
  let out = scalar_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ScalarTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses an `ObjectTypeExtension` (`extend type Name` + at least one of
/// implements/directives/fields).
///
/// The implements-only form is accepted (spec-correct relaxation; frozen rejected it).
///
/// Spec: [ObjectTypeExtension](https://spec.graphql.org/draft/#ObjectTypeExtension).
pub fn object_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ObjectTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  type_kw(inp)?;
  let out = object_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ObjectTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses an `InterfaceTypeExtension` (`extend interface Name` + at least one of
/// implements/directives/fields).
///
/// The implements-only form is accepted (spec-correct relaxation; frozen rejected it).
///
/// Spec: [InterfaceTypeExtension](https://spec.graphql.org/draft/#InterfaceTypeExtension).
pub fn interface_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InterfaceTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  interface(inp)?;
  let out = interface_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::InterfaceTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses a `UnionTypeExtension` (`extend union Name` + at least one of
/// directives/member types).
///
/// Spec: [UnionTypeExtension](https://spec.graphql.org/draft/#UnionTypeExtension).
pub fn union_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<UnionTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  union(inp)?;
  let out = union_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::UnionTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses an `EnumTypeExtension` (`extend enum Name` + at least one of
/// directives/values).
///
/// Spec: [EnumTypeExtension](https://spec.graphql.org/draft/#EnumTypeExtension).
pub fn enum_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<EnumTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  enum_kw(inp)?;
  let out = enum_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::EnumTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses an `InputObjectTypeExtension` (`extend input Name` + at least one of
/// directives/fields).
///
/// Spec: [InputObjectTypeExtension](https://spec.graphql.org/draft/#InputObjectTypeExtension).
pub fn input_object_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InputObjectTypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  input_kw(inp)?;
  let out = input_object_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::InputObjectTypeExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

/// Parses a `SchemaExtension` (`extend schema` + at least one of directives/root
/// operations).
///
/// Spec: [SchemaExtension](https://spec.graphql.org/draft/#SchemaExtension).
pub fn schema_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<SchemaExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  schema(inp)?;
  let out = schema_extension_body(inp, ext.span().start())?;
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::SchemaExtension.raw());
  emitter.cst_finish();
  Ok(out)
}

// ─── extension dispatches ────────────────────────────────────────────────────

/// Dispatches on the soft keyword after `extend` to the matching type-extension
/// body, consuming the keyword through the declining `try_*` atoms. No node: the
/// caller retro-wraps the resolved arm's kind (sum-type convention).
fn dispatch_type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ext_start: usize,
) -> Result<TypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  if let ParseAttempt::Accept(_kw) = try_scalar(inp)? {
    return scalar_extension_body(inp, ext_start).map(TypeExtension::Scalar);
  }
  if let ParseAttempt::Accept(_kw) = try_type(inp)? {
    return object_extension_body(inp, ext_start).map(TypeExtension::Object);
  }
  if let ParseAttempt::Accept(_kw) = try_interface(inp)? {
    return interface_extension_body(inp, ext_start).map(TypeExtension::Interface);
  }
  if let ParseAttempt::Accept(_kw) = try_union(inp)? {
    return union_extension_body(inp, ext_start).map(TypeExtension::Union);
  }
  if let ParseAttempt::Accept(_kw) = try_enum(inp)? {
    return enum_extension_body(inp, ext_start).map(TypeExtension::Enum);
  }
  if let ParseAttempt::Accept(_kw) = try_input(inp)? {
    return input_object_extension_body(inp, ext_start).map(TypeExtension::InputObject);
  }
  unexpected(inp)
}

/// The `K::…` kind the resolved [`TypeExtension`] arm materializes as.
fn type_extension_kind<S, Ty>(ext: &TypeExtension<S, Ty>) -> u16 {
  match ext {
    TypeExtension::Scalar(_) => K::ScalarTypeExtension.raw(),
    TypeExtension::Object(_) => K::ObjectTypeExtension.raw(),
    TypeExtension::Interface(_) => K::InterfaceTypeExtension.raw(),
    TypeExtension::Union(_) => K::UnionTypeExtension.raw(),
    TypeExtension::Enum(_) => K::EnumTypeExtension.raw(),
    TypeExtension::InputObject(_) => K::InputObjectTypeExtension.raw(),
  }
}

/// Parses a `TypeExtension` (`extend` + one of the six type-extension shapes),
/// dispatching on the soft keyword after `extend`.
///
/// No wrapper node of its own beyond the resolved arm's kind (sum-type convention):
/// the mark — minted before `extend` — is spent as that arm's kind.
///
/// Spec: [TypeExtension](https://spec.graphql.org/draft/#TypeExtension).
pub fn type_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  let out = dispatch_type_extension(inp, ext.span().start())?;
  let kind = type_extension_kind(&out);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind);
  emitter.cst_finish();
  Ok(out)
}

/// Parses a `TypeSystemExtension`'s body after `extend` has been consumed, spending
/// `mark` (minted before `extend`) as the resolved arm's kind.
///
/// Shared by [`type_system_extension`] (which mints the mark and consumes `extend`)
/// and the document-level definition-or-extension dispatches (which consume `extend`
/// as their dispatch and hand the keyword's start and their pre-keyword mark here) —
/// the W3 `fragment_definition_body` convergence pattern.
pub(super) fn type_system_extension_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
  ext_start: usize,
) -> Result<TypeSystemExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  if let ParseAttempt::Accept(_kw) = try_schema(inp)? {
    let ext = schema_extension_body(inp, ext_start)?;
    let emitter = inp.emitter();
    emitter.cst_start_at(mark, K::SchemaExtension.raw());
    emitter.cst_finish();
    return Ok(TypeSystemExtension::Schema(ext));
  }
  let ext = dispatch_type_extension(inp, ext_start)?;
  let kind = type_extension_kind(&ext);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, kind);
  emitter.cst_finish();
  Ok(TypeSystemExtension::Type(ext))
}

/// Parses a `TypeSystemExtension` (`extend` + a schema extension or one of the six
/// type-extension shapes).
///
/// No wrapper node of its own beyond the resolved arm's kind (sum-type convention).
///
/// Spec: [TypeSystemExtension](https://spec.graphql.org/draft/#TypeSystemExtension).
pub fn type_system_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeSystemExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let ext = extend(inp)?;
  type_system_extension_body(inp, mark, ext.span().start())
}

#[cfg(test)]
mod tests;
