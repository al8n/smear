//! GraphQLx `import` productions — the definition, its clause, the brace list,
//! and the named / wildcard specifiers.
//!
//! An import composes schemas across files:
//!
//! ```graphqlx
//! import { User, Post as BlogPost, * as utils } from "./types.graphqlx"
//! import * as helpers from "./helpers.graphqlx"
//! ```
//!
//! [`import_definition`] parses `'import' ImportClause 'from' FilePath`;
//! [`import_clause`] dispatches on one peeked token between the brace
//! [`import_list`] and a bare [`wildcard_specifier`]; the list collects
//! [`named_specifier`] / [`wildcard_specifier`] members (mixed freely, commas are
//! trivia — fixture `0013_complex_import`). Every keyword (`import` / `from` /
//! `as`) is soft, resolved by slice compare through the typed keyword atoms; an
//! `as` alias names a full `::`-[`path`](super::ty::path).
//!
//! The file path is an **inline** string only: the frozen model keys
//! [`ImportDefinition`](crate::graphqlx::ast::ImportDefinition) by
//! [`InlineStringValue`](crate::graphqlx::ast::InlineStringValue), so a block
//! string (`"""…"""`) rejects (Deviations Register: type-shape authority; the
//! fixtures only ever write inline paths).
//!
//! # Spec cardinality (plan Amendment 2)
//!
//! `ImportList : { ImportMember+ }` — an empty `import {} from "…"` rejects. The
//! Wave 6 substrate grammar pins `+` (its `ImportList` rustdoc), overriding the
//! scaffold's stale `importedMember*` comment; the first member commits, the rest
//! collect via `list_of`.
//!
//! # Node placement
//!
//! Specifiers and the definition retro-wrap their kinds after their optional
//! tails settle (Amendment 1: content-dependent spans); the alias path inside a
//! specifier is a [`node`]-bracketed `K::Path` exactly as in type positions.
//! [`import_list`] opens `K::ImportList` up front over the `{ … }` region.
//! [`import_clause`] adds no wrapper node — the committed arm's kind is the
//! clause's (sum-type convention; `K::ImportClause` stays declared-but-unopened
//! exactly like `K::ExecutableDefinition`).

use smear_lexer::keywords::Import;
use tokora::{
  InputRef, Lexer, ParseInput, SimpleSpan, Token,
  cst::event::EventMark,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{braces, list_of, node},
  token::{IdentifierToken, KeywordToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::{peeks_where, ty::path};
use crate::{
  combinator::{ErrorOf, LiteralValueToken, ParseCtx, SliceOf, asterisk, ident},
  graphqlx::{
    ast::{
      ImportClause, ImportDefinition, ImportList, ImportMember, InlineStringValue, Name,
      NamedSpecifier, Path, WildcardSpecifier,
    },
    keyword::{from, import, try_as},
    kinds::SyntaxKind as K,
  },
};

/// Parses the optional `'as' Path` alias tail shared by both specifiers,
/// declining to `None` (no tokens consumed) unless the soft `as` keyword is next.
///
/// The alias is a full `::`-path (`* as helpers`, `User as types::User`), wrapped
/// as a `K::Path` node exactly as in type positions.
// The `Option<Path<…>>` return is inherent to the optional alias.
#[allow(clippy::type_complexity)]
fn try_alias<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Path<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match try_as(inp)? {
    ParseAttempt::Accept(_kw) => Ok(Some(node(K::Path.raw(), path).parse_input(inp)?)),
    ParseAttempt::Decline => Ok(None),
  }
}

/// Parses a `NamedSpecifier` (`Name ('as' Path)?`) — one named import, optionally
/// renamed.
///
/// Grammar: `NamedSpecifier : Name (as Path)?` (frozen GraphQLx model; fixtures
/// `0001`/`0003`).
pub fn named_specifier<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<NamedSpecifier<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let alias = try_alias(inp)?;
  let end = match &alias {
    Some(p) => p.span().end(),
    None => name.span().end(),
  };
  let span = SimpleSpan::new(name.span().start(), end);
  let specifier = NamedSpecifier::new(span, name, alias);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::NamedImportSpecifier.raw());
  emitter.cst_finish();
  Ok(specifier)
}

/// Parses a `WildcardSpecifier` (`'*' ('as' Path)?`) — a whole-module import,
/// optionally renamed.
///
/// Grammar: `WildcardSpecifier : * (as Path)?` (frozen GraphQLx model; fixtures
/// `0002`/`0013`).
pub fn wildcard_specifier<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<WildcardSpecifier<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let star = asterisk(inp)?;
  let alias = try_alias(inp)?;
  let end = match &alias {
    Some(p) => p.span().end(),
    None => star.span().end(),
  };
  let span = SimpleSpan::new(star.span().start(), end);
  let specifier = WildcardSpecifier::new(span, alias);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::WildcardImportSpecifier.raw());
  emitter.cst_finish();
  Ok(specifier)
}

/// Parses one `ImportMember` — a [`wildcard_specifier`] when the next token is
/// `*`, otherwise a [`named_specifier`]. No wrapper node of its own (sum-type
/// convention).
fn import_member<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ImportMember<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_asterisk)? {
    wildcard_specifier(inp).map(ImportMember::Wildcard)
  } else {
    named_specifier(inp).map(ImportMember::Named)
  }
}

/// Parses an `ImportList` (`'{' ImportMember+ '}'`) — named and wildcard members
/// mixed freely, commas insignificant.
///
/// Spec cardinality (Amendment 2): `ImportMember+` per the Wave 6 substrate
/// grammar, so an empty `{}` errors — the first member commits, the rest collect
/// via `list_of`.
pub fn import_list<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ImportList<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  node(
    K::ImportList.raw(),
    braces(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
      // Spec cardinality (`ImportMember+`): the first member is committed, so an
      // empty `{}` errors at the `}` exactly as the committed member reports it.
      let first = import_member(inp)?;
      let mut members = list_of(
        import_member,
        <L::Token as PunctuatorTokenExt>::is_close_brace,
      )(inp)?;
      members.insert(0, first);
      Ok(members)
    }),
  )
  .parse_input(inp)
  .map(|delimited| {
    let (span, _open, _close, members) = delimited.into_components();
    ImportList::new(span, members)
  })
}

/// Parses an `ImportClause` — the brace [`import_list`] when the next token is
/// `{`, a bare [`wildcard_specifier`] when it is `*`, and an error otherwise.
/// No wrapper node of its own (sum-type convention).
///
/// Grammar: `ImportClause : ImportList | WildcardSpecifier`.
pub fn import_clause<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ImportClause<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + KeywordToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_open_brace)? {
    import_list(inp).map(ImportClause::List)
  } else if peeks_where(inp, <L::Token as PunctuatorTokenExt>::is_asterisk)? {
    wildcard_specifier(inp).map(ImportClause::Wildcard)
  } else {
    match inp.next()? {
      Some(spanned) => {
        let (span, token) = spanned.into_components();
        Err(UnexpectedToken::of(span).with_found(token).into())
      }
      None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
    }
  }
}

/// Consumes the next token as the import's file path — an **inline** string
/// literal only (the frozen model keys the definition by `InlineStringValue`, so
/// a block string rejects).
fn import_file_path<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<InlineStringValue<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: LiteralValueToken<'inp, InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  match inp.next()? {
    Some(spanned) => {
      let (span, token) = spanned.into_components();
      match <L::Token as LiteralValueToken>::into_inline_str(token) {
        Ok(lit) => Ok(InlineStringValue::new(span, lit)),
        Err(token) => Err(UnexpectedToken::of(span).with_found(token).into()),
      }
    }
    None => Err(UnexpectedEot::eot_of(inp.offset().clone()).into()),
  }
}

/// Parses the body of an import definition after the `import` keyword has been
/// consumed, spending `mark` as `K::ImportDefinition` over the whole definition.
///
/// Shared by [`import_definition`] (which mints `mark` and consumes the keyword)
/// and the document-level dispatch (which consumes the soft keyword as its
/// dispatch and hands the keyword and its pre-keyword mark here).
pub(super) fn import_definition_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  mark: EventMark,
  kw: Import,
) -> Result<ImportDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<'inp, InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let clause = import_clause(inp)?;
  from(inp)?;
  let file = import_file_path(inp)?;
  let span = SimpleSpan::new(kw.span().start(), file.span().end());
  let def = ImportDefinition::new(span, file, clause);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::ImportDefinition.raw());
  emitter.cst_finish();
  Ok(def)
}

/// Parses an `ImportDefinition` (`'import' ImportClause 'from' FilePath`).
///
/// Grammar: `ImportDefinition : import ImportClause from FilePath` (frozen
/// GraphQLx model; fixtures `0001`–`0003`, `0013`).
pub fn import_definition<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ImportDefinition<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + KeywordToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<'inp, InlineStr = smear_lexer::LitInlineStr<SliceOf<'inp, L>>>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let kw = import(inp)?;
  import_definition_body(inp, mark, kw)
}

#[cfg(test)]
mod tests;
