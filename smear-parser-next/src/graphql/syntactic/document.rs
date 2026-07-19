//! GraphQL document productions — the top-level `definition` dispatch, the
//! definition-or-extension forks, and the three document roots.
//!
//! [`definition`] dispatches on the leading soft keyword — `schema` / `directive` /
//! the six type-definition keywords — falling through to an executable definition
//! (`fragment`, an operation keyword, or the query-shorthand `{`), exactly the frozen
//! decision order. [`definition_or_extension`] and
//! [`type_system_definition_or_extension`] put `extend` in front of that dispatch and
//! wrap the non-extension arm in an optional leading description
//! ([`Described`](crate::graphql::ast::Described));
//! [`document`] and [`type_system_document`] collect their entry productions to end
//! of input ([`executable_document`](super::executable::executable_document) is the
//! Wave-3 third root).
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! `Document : Definition+` and `TypeSystemDocument : TypeSystemDefinitionOrExtension+`
//! are enforced natively: an EMPTY input errors in both [`document`] and
//! [`type_system_document`] — a documented deviation from the frozen parser, whose
//! unenforced `repeated_while` accepted zero definitions (the same class as Wave 3's
//! `executable_document` entry).

use smear_lexer::{LitBlockStr, LitInlineStr};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::list_of,
  punct::{Ampersand, Pipe},
  token::{IdentifierToken, KeywordToken, PunctuatorToken},
  try_parse_input::ParseAttempt,
};

use super::{
  definition::{
    description, directive_definition_body, schema_definition_body, try_dispatch_type_definition,
  },
  executable::{fragment_definition_body, operation_definition_body},
  extension::type_system_extension_body,
};
use crate::{
  combinator::{Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf},
  graphql::{
    ast::{
      Definition, DefinitionOrExtension, Document, TypeSystemDefinition,
      TypeSystemDefinitionOrExtension, TypeSystemDocument,
    },
    keyword::{try_directive, try_extend, try_fragment, try_schema},
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

/// Parses a `Definition`'s body (every arm routes through a shared `*_body`
/// production, converging with the standalone forms).
///
/// Decision order (frozen parity): `schema` → `directive` → the six type-definition
/// keywords → `fragment` → operation (keyword or query-shorthand).
fn definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Definition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  if let ParseAttempt::Accept(kw) = try_schema(inp)? {
    return schema_definition_body(inp, kw.span().start())
      .map(|d| Definition::TypeSystem(TypeSystemDefinition::Schema(d)));
  }
  if let ParseAttempt::Accept(kw) = try_directive(inp)? {
    return directive_definition_body(inp, kw.span().start())
      .map(|d| Definition::TypeSystem(TypeSystemDefinition::Directive(d)));
  }
  if let Some(def) = try_dispatch_type_definition(inp)? {
    return Ok(Definition::TypeSystem(TypeSystemDefinition::Type(def)));
  }
  if let ParseAttempt::Accept(kw) = try_fragment(inp)? {
    return fragment_definition_body(inp, kw)
      .map(|d| Definition::Executable(scaffold::ExecutableDefinition::Fragment(d)));
  }
  operation_definition_body(inp)
    .map(|d| Definition::Executable(scaffold::ExecutableDefinition::Operation(d)))
}

/// Parses a `Definition` — a type-system definition (schema, directive, or type) or
/// an executable definition (operation or fragment), without a leading description
/// (frozen parity: the description belongs to the document-level described forms).
///
/// Spec: [Definition](https://spec.graphql.org/draft/#Definition).
pub fn definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Definition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  definition_body(inp)
}

/// Parses a `DefinitionOrExtension` — a `TypeSystemExtension` when `extend` leads,
/// otherwise a [`Definition`] with an optional leading description.
///
/// Spec: [Document](https://spec.graphql.org/draft/#Document) (the full-document
/// entry form; extensions per
/// [TypeSystemDefinitionOrExtension](https://spec.graphql.org/draft/#TypeSystemDefinitionOrExtension)).
pub fn definition_or_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<DefinitionOrExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  if let ParseAttempt::Accept(kw) = try_extend(inp)? {
    return type_system_extension_body(inp, kw.span().start())
      .map(DefinitionOrExtension::Extension);
  }
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let def = definition_body(inp)?;
  let span = inp.span_since(&cursor);
  Ok(DefinitionOrExtension::Definition(scaffold::Described::new(
    span, desc, def,
  )))
}

/// Parses a `TypeSystemDefinitionOrExtension` — a `TypeSystemExtension` when
/// `extend` leads, otherwise a described `TypeSystemDefinition` (`schema` /
/// `directive` / a type definition; executable definitions REJECT here).
///
/// Spec: [TypeSystemDefinitionOrExtension](https://spec.graphql.org/draft/#TypeSystemDefinitionOrExtension).
pub fn type_system_definition_or_extension<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeSystemDefinitionOrExtension<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  if let ParseAttempt::Accept(kw) = try_extend(inp)? {
    return type_system_extension_body(inp, kw.span().start())
      .map(TypeSystemDefinitionOrExtension::Extension);
  }
  let cursor = inp.cursor().clone();
  let desc = description(inp)?;
  let def = if let ParseAttempt::Accept(kw) = try_schema(inp)? {
    TypeSystemDefinition::Schema(schema_definition_body(inp, kw.span().start())?)
  } else if let ParseAttempt::Accept(kw) = try_directive(inp)? {
    TypeSystemDefinition::Directive(directive_definition_body(inp, kw.span().start())?)
  } else if let Some(def) = try_dispatch_type_definition(inp)? {
    TypeSystemDefinition::Type(def)
  } else {
    return unexpected(inp);
  };
  let span = inp.span_since(&cursor);
  Ok(TypeSystemDefinitionOrExtension::Definition(
    scaffold::Described::new(span, desc, def),
  ))
}

/// Wraps a described-or-extension entry parser into its document production: the
/// first entry is committed (spec `+`), the rest collect to end of input.
// The fn-pointer parameter spells the full generic parser shape; an alias would
// only move the same generics.
#[allow(clippy::type_complexity)]
fn document_of<'inp, L, Ctx, Lang, T>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  entry: fn(&mut InputRef<'inp, '_, L, Ctx, Lang>) -> Result<T, ErrorOf<'inp, L, Ctx, Lang>>,
) -> Result<scaffold::Document<T>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let cursor = inp.cursor().clone();
  // Spec cardinality (`+`): the first entry is committed, so an empty input errors
  // as end of input; the rest collect until end of input (a document has no closing
  // delimiter, so the stop predicate never fires).
  let first = entry(inp)?;
  let mut defs = list_of(entry, |_: &L::Token| false)(inp)?;
  defs.insert(0, first);
  let span = inp.span_since(&cursor);
  let doc = scaffold::Document::new(span, defs);
  Ok(doc)
}

/// Parses a full `Document` (`Definition+`, each entry a described definition or a
/// type-system extension), running to end of input.
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `Definition+` demands one-or-more, so an empty input errors here where
/// frozen's unenforced `+` accepted it.
///
/// Spec: [Document](https://spec.graphql.org/draft/#Document).
pub fn document<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Document<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  document_of(inp, definition_or_extension)
}

/// Parses a `TypeSystemDocument` (`TypeSystemDefinitionOrExtension+`), running to
/// end of input. Executable definitions REJECT (this root is schema-only).
///
/// Deviation from the frozen parser (spec-cardinality rule, plan Amendment 2): the
/// spec's `TypeSystemDefinitionOrExtension+` demands one-or-more, so an empty input
/// errors here where frozen's unenforced `+` accepted it.
///
/// Spec: [TypeSystemDocument](https://spec.graphql.org/draft/#TypeSystemDocument).
pub fn type_system_document<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeSystemDocument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  document_of(inp, type_system_definition_or_extension)
}

#[cfg(test)]
mod tests;
