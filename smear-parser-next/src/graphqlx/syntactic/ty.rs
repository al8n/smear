//! GraphQLx `Type` productions — path types with generics, list / set / map types,
//! and the non-null retro-wrap.
//!
//! [`ty`] is the single recursive dispatcher for every `Type` shape: a peeked
//! identifier commits to a `TypePath` (a `::`-path optionally applied to a `<…>`
//! generic-argument list), a peeked `[` commits to a `ListType`, and a peeked `<`
//! opens either a `SetType` (`<T>`) or a `MapType` (`<K => V>`), disambiguated by
//! whether a `=>` follows the first element. One peek, committed arms — exactly
//! [`value`](super::value::value)'s dispatch discipline.
//!
//! The GraphQLx type nodes each fold a trailing `!` into a `required: bool` field
//! (`DefinitionTypePath` / `ListType` / `SetType` / `MapType` all carry it), so [`ty`]
//! parses the base shape, then the optional `!`, and bakes the flag into the node it
//! already built — no separate `NonNullType` AST node.
//!
//! # The `NonNullType` retro-wrap
//!
//! The CST layer *does* carry a separate `NonNullType` node. [`ty`] mints a mark
//! before dispatching, lets the base shape's own [`node`] bracket (or, for the
//! content-dependent set/map fork, a manual `cst_start_at`) record the base region,
//! and — only when a `!` actually follows — spends the outer mark as
//! `K::NonNullType`, wrapping the base node and the `!` together. A decline (no `!`)
//! leaves the mark unspent. This is content-dependent placement (Amendment 1): the
//! manual `cst_mark`/`cst_start_at`/`cst_finish` retro-wrap, not
//! [`node_at`](tokora::parser::node_at), which cannot decide *whether* to wrap.

use smear_scaffold::ast::{
  ListType, MapType, SetType,
  generic::{DefinitionTypePath, TypeGenerics},
};
use tokora::{
  InputRef, Lexer, ParseInput, SimpleSpan, Token,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{angles, brackets, list_of, node, try_angles},
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use crate::{
  combinator::{ErrorOf, ParseCtx, SliceOf, ident, try_bang, try_fat_arrow, try_path_sep},
  graphqlx::{
    ast::{Name, Path, Type},
    kinds::SyntaxKind as K,
  },
};

/// The classified head of a `Type`: which base shape the one-token peek resolves to.
#[derive(Clone, Copy)]
enum TypeHead {
  /// A `TypePath` — the next token is an identifier.
  Path,
  /// A `ListType` — the next token is `[`.
  List,
  /// A `SetType` or `MapType` — the next token is `<`.
  Angle,
}

/// Peeks the next token (without consuming it) and classifies it into a
/// [`TypeHead`]. `Ok(None)` is end of input, `Ok(Some(None))` is a token that begins
/// no type, `Ok(Some(Some(head)))` is a recognised head with the token still in place
/// for the committed arm.
fn classify_type_head<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Option<TypeHead>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let mut outcome = None;
  inp.try_expect(|spanned| {
    let t: &L::Token = spanned.data;
    let head = if <L::Token as PunctuatorTokenExt>::is_open_bracket(t) {
      Some(TypeHead::List)
    } else if <L::Token as PunctuatorTokenExt>::is_open_angle(t) {
      Some(TypeHead::Angle)
    } else if <L::Token as IdentifierToken>::is_identifier(t) {
      Some(TypeHead::Path)
    } else {
      None
    };
    outcome = Some(head);
    false
  })?;
  Ok(outcome)
}

/// The unwrapped base shape [`ty`] dispatches to, before the trailing-bang decision.
enum TypeCore<S> {
  /// A `TypePath`'s path and optional generic arguments, not yet flagged `required`.
  Path(Path<S>, Option<TypeGenerics<Type<S>>>),
  /// A `ListType`'s already-recursively-resolved element type.
  List(Type<S>),
  /// A `SetType`'s element type.
  Set(Type<S>),
  /// A `MapType`'s key and value types.
  Map(Type<S>, Type<S>),
}

/// Parses a `::`-path — `ident (:: ident)*`, no leading `::` (a type path is always
/// relative). Segments after the first are gathered by a commit-first `try_path_sep`
/// loop, never a separated1 with an identifier follow-set (the W5 bug class).
///
/// The `Path<SliceOf<…>>` return names the shared path node; a caller wraps it in a
/// [`node`] of the kind that owns it (`K::Path`).
pub fn path<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Path<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let (first_span, first_src) = ident(inp)?.into_components();
  let mut segments = std::vec::Vec::from_iter([Name::new(first_span, first_src)]);
  while let ParseAttempt::Accept(_sep) = try_path_sep(inp)? {
    let (seg_span, seg_src) = ident(inp)?.into_components();
    segments.push(Name::new(seg_span, seg_src));
  }
  let span = inp.span_since(&cursor);
  Ok(Path::new(span, segments, false))
}

/// Parses the `path (<…>)?` body of a `TypePath` — the [`node`]-wrapped region covers
/// the path and any generic arguments, never a trailing `!`.
// The nested `Option<TypeGenerics<…>>` return is inherent to the optional generic
// arguments; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
fn type_path_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<
  (
    Path<SliceOf<'inp, L>>,
    Option<TypeGenerics<Type<SliceOf<'inp, L>>>>,
  ),
  ErrorOf<'inp, L, Ctx, Lang>,
>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let path = node(K::Path.raw(), path).parse_input(inp)?;
  let generics = try_type_generics(inp)?;
  Ok((path, generics))
}

/// Parses an optional `<Type, …>` generic-argument list, retro-wrapped as a
/// `TypeGenerics` node when present.
///
/// Declines to `None` (no tokens consumed) unless the next token is `<`. Once the
/// `<` commits, at least one type is required — an empty `<>` is a rejection
/// (Amendment 2: the grammar's `< Type+ >` cardinality is enforced natively; the
/// fixtures never write empty generics). The first element commits, the rest are
/// collected by a commit-first `list_of` loop (never a separated1 with a type /
/// identifier follow-set — the W5 bug class); commas between them are insignificant.
// The `Option<TypeGenerics<…>>` return is inherent to the optional generic arguments.
#[allow(clippy::type_complexity)]
fn try_type_generics<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<TypeGenerics<Type<SliceOf<'inp, L>>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  match try_angles(generic_args_body)(inp)? {
    Some(delimited) => {
      let (span, _open, _close, params) = delimited.into_components();
      let generics = TypeGenerics::new(span, params);
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::TypeGenerics.raw());
      emitter.cst_finish();
      Ok(Some(generics))
    }
    None => Ok(None),
  }
}

/// Parses the `Type (Type)*` body inside a generic-argument `<…>` region — one or
/// more types (Amendment 2). The first element commits (so an empty `<>` errors on
/// the closing `>`), and the rest are gathered by a commit-first `list_of` loop.
// The `Vec<Type<…>>` return is inherent to the generic-argument container.
#[allow(clippy::type_complexity)]
fn generic_args_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<std::vec::Vec<Type<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let first = ty(inp)?;
  let mut params = list_of(ty, <L::Token as PunctuatorTokenExt>::is_close_angle)(inp)?;
  params.insert(0, first);
  Ok(params)
}

/// Parses the `[ Type ]` region of a `ListType` — the [`node`]-wrapped region covers
/// the brackets and the recursively-parsed element, never a trailing `!`.
fn list_type_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Type<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let delimited = brackets(ty)(inp)?;
  let (_span, _open, _close, inner) = delimited.into_components();
  Ok(inner)
}

/// Parses the `Type (=> Type)?` body inside a `<…>` region: a lone type is a set
/// element, a `Type => Type` pair is a map's key and value.
fn angle_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<AngleBody<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let key = ty(inp)?;
  match try_fat_arrow(inp)? {
    ParseAttempt::Accept(_arrow) => {
      let val = ty(inp)?;
      Ok(AngleBody::Map(key, val))
    }
    ParseAttempt::Decline => Ok(AngleBody::Set(key)),
  }
}

/// The two shapes a `<…>` region resolves to.
enum AngleBody<S> {
  /// `<T>` — a set element type.
  Set(Type<S>),
  /// `<K => V>` — a map's key and value types.
  Map(Type<S>, Type<S>),
}

/// The shared error tail: reports the offending token as unexpected, or end of input.
fn unexpected_type<'inp, L, Ctx, T, Lang>(
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

/// Parses a `Type`: a `TypePath` (`a::b::C<…>`), a `ListType` (`[T]`), a `SetType`
/// (`<T>`), or a `MapType` (`<K => V>`), each optionally made non-null by a trailing
/// `!`.
///
/// One peek, committed arms; the trailing `!` folds into the resolved node's
/// `required` flag and retro-wraps the CST region as `NonNullType`.
///
/// Spec: [Type](https://spec.graphql.org/draft/#sec-Type-References) (GraphQLx).
pub fn ty<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Type<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  let core = match classify_type_head(inp)? {
    Some(Some(TypeHead::Path)) => {
      let (path, generics) = node(K::TypePath.raw(), type_path_body).parse_input(inp)?;
      TypeCore::Path(path, generics)
    }
    Some(Some(TypeHead::List)) => {
      TypeCore::List(node(K::ListType.raw(), list_type_body).parse_input(inp)?)
    }
    Some(Some(TypeHead::Angle)) => {
      // Content-dependent kind (set vs map): a manual retro-wrap, chosen after the
      // body reveals whether a `=>` was present (Amendment 1).
      let inner_mark = inp.emitter().cst_mark();
      let delimited = angles(angle_body)(inp)?;
      let (_span, _open, _close, body) = delimited.into_components();
      let kind = match &body {
        AngleBody::Set(_) => K::SetType,
        AngleBody::Map(_, _) => K::MapType,
      };
      let emitter = inp.emitter();
      emitter.cst_start_at(inner_mark, kind.raw());
      emitter.cst_finish();
      match body {
        AngleBody::Set(inner) => TypeCore::Set(inner),
        AngleBody::Map(key, val) => TypeCore::Map(key, val),
      }
    }
    _ => return unexpected_type(inp),
  };
  let required = matches!(try_bang(inp)?, ParseAttempt::Accept(_bang));
  let span = inp.span_since(&cursor);
  let out = match core {
    TypeCore::Path(path, generics) => {
      Type::Path(DefinitionTypePath::new(span, required, path, generics))
    }
    TypeCore::List(inner) => Type::from(ListType::new(span, inner, required)),
    TypeCore::Set(inner) => Type::from(SetType::new(span, inner, required)),
    TypeCore::Map(key, val) => Type::from(MapType::new(span, key, val, required)),
  };
  if required {
    let emitter = inp.emitter();
    emitter.cst_start_at(mark, K::NonNullType.raw());
    emitter.cst_finish();
  }
  Ok(out)
}

#[cfg(test)]
mod tests;
