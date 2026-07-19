//! GraphQL `Type` productions — named types, list types, and the trailing bang.
//!
//! [`ty`] is the single recursive dispatcher for every `Type` shape: a peeked `[`
//! commits to the `ListType` body (recursing into `ty` for the element), a peeked
//! identifier commits to the `NamedType` body — one peek, committed arms, exactly
//! [`value`](super::value::value)'s dispatch discipline. Neither shape carries a
//! separate AST node for a trailing `!`: the frozen [`Type`] enum folds it into a
//! `required: bool` field on the same `NamedType`/`ListType`, so [`ty`] parses the
//! bang after the base shape and bakes it into the node it already built.

use std::boxed::Box;

use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::brackets,
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use crate::{
  combinator::{ErrorOf, ParseCtx, SliceOf, ident, try_bang},
  graphql::ast::{Name, Type},
};

/// The classified head of a `Type`: which base shape the one-token peek resolves to.
#[derive(Clone, Copy)]
enum TypeHead {
  /// A `NamedType` — the next token is an identifier.
  Name,
  /// A `ListType` — the next token is `[`.
  List,
}

/// Peeks the next token (without consuming it) and classifies it into a
/// [`TypeHead`]. Mirrors `value`'s `classify_value_head`: `Ok(None)` is end of
/// input, `Ok(Some(None))` is a token that begins no type, `Ok(Some(Some(head)))`
/// is a recognised head, with the token still in place for the committed arm.
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
    } else if <L::Token as IdentifierToken>::is_identifier(t) {
      Some(TypeHead::Name)
    } else {
      None
    };
    outcome = Some(head);
    false
  })?;
  Ok(outcome)
}

/// The unwrapped base shape [`ty`] dispatches to, before the trailing-bang
/// decision: either a bare `Name` (a `NamedType` in waiting) or a fully resolved
/// element `Type` (a `ListType` in waiting).
enum TypeCore<S> {
  /// A `NamedType`'s name, not yet wrapped with its `required` flag.
  Name(Name<S>),
  /// A `ListType`'s already-recursively-resolved element type.
  List(Type<Name<S>>),
}

/// Parses just the `Name` of a `NamedType` — never a trailing `!` ([`ty`] folds
/// that into the node it builds).
fn named_type_name<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Name<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let (span, src) = ident(inp)?.into_components();
  Ok(Name::new(span, src))
}

/// Parses the `[ Type ]` region of a `ListType` — the brackets and the
/// recursively-parsed element, never a trailing `!`.
// The nested `Type<Name<SliceOf<…>>>` return is inherent to the recursive `Type`
// shape; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
fn list_type_body<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Type<Name<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let delimited = brackets(ty)(inp)?;
  let (_span, _open, _close, inner) = delimited.into_components();
  Ok(inner)
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

/// Parses a `Type`: a `NamedType`, a `ListType`, or either wrapped in a
/// `NonNullType` by a trailing `!`.
///
/// One peek, committed arms — `[` → `ListType` (recursing into `ty` for the
/// element), an identifier → `NamedType` — then an optional trailing `!` folds
/// into the resolved node's `required` flag.
///
/// Spec: [Type](https://spec.graphql.org/draft/#sec-Type-References).
// The nested `Type<Name<SliceOf<…>>>` return is inherent to the recursive `Type`
// shape; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn ty<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Type<Name<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let core = match classify_type_head(inp)? {
    Some(Some(TypeHead::Name)) => TypeCore::Name(named_type_name(inp)?),
    Some(Some(TypeHead::List)) => TypeCore::List(list_type_body(inp)?),
    _ => return unexpected_type(inp),
  };
  match try_bang(inp)? {
    ParseAttempt::Accept(_bang) => {
      let span = inp.span_since(&cursor);
      let out = match core {
        TypeCore::Name(name) => Type::Name(scaffold::NamedType::new(span, name, true)),
        TypeCore::List(inner) => Type::List(Box::new(scaffold::ListType::new(span, inner, true))),
      };
      Ok(out)
    }
    ParseAttempt::Decline => {
      let span = inp.span_since(&cursor);
      Ok(match core {
        TypeCore::Name(name) => Type::Name(scaffold::NamedType::new(span, name, false)),
        TypeCore::List(inner) => Type::List(Box::new(scaffold::ListType::new(span, inner, false))),
      })
    }
  }
}

#[cfg(test)]
mod tests;
