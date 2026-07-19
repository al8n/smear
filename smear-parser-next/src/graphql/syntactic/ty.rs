//! GraphQL `Type` productions — named types, list types, and the trailing bang.
//!
//! [`ty`] is the single recursive dispatcher for every `Type` shape. The base-shape
//! choice is spelled the house way — a tuple of committed arms behind a
//! [`ParseChoice`] peek dispatch (plan Amendment 7): a peeked `[` selects the
//! `ListType` body (recursing into `ty` for the element), a peeked identifier
//! selects the `NamedType` body, and any other head declines the choice and falls
//! through to `unexpected_type` for the same unexpected-token / end-of-input error
//! the hand-rolled classifier raised. Neither shape carries a separate AST node for
//! a trailing `!`: the frozen [`Type`] enum folds it into a `required: bool` field
//! on the same `NamedType`/`ListType`, so [`ty`] parses the bang after the base
//! shape and bakes it into the node it already built.

use std::boxed::Box;

use smear_scaffold::ast as scaffold;
use tokora::{
  Branch, InputRef, Lexer, ParseChoice, SimpleSpan, Token, TryParseInput,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::brackets,
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::{IntoComponents, typenum::U1},
};

use crate::{
  combinator::{ErrorOf, ParseCtx, SliceOf, ident, try_bang},
  graphql::ast::{Name, Type},
};

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

/// The `NamedType` arm of [`ty`]'s base-shape dispatch: the bare name lifted into
/// [`TypeCore::Name`] so both arms share the choice's uniform output. A plain fn
/// item (not a `.map`ped parser) because `Map`'s `ParseInput` impl requires a
/// `Sized` `Lang`, which these `Lang: ?Sized` productions do not have.
fn named_type_core<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeCore<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  Ok(TypeCore::Name(named_type_name(inp)?))
}

/// The `ListType` arm of [`ty`]'s base-shape dispatch: the bracketed element type
/// lifted into [`TypeCore::List`]. A plain fn item for the same reason as
/// [`named_type_core`].
// The nested `Type<Name<SliceOf<…>>>` the arm carries is inherent to the recursive
// `Type` shape; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
fn list_type_core<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<TypeCore<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  Ok(TypeCore::List(list_type_body(inp)?))
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
  // House-way base-shape dispatch: a tuple of committed arms behind a `ParseChoice`
  // one-token peek. An identifier selects the `NamedType` arm, `[` the `ListType`
  // arm; any other head (or end of input) declines the choice and falls through to
  // `unexpected_type`, preserving the classifier's exact unexpected-token / EOT
  // error. Same accept set as the hand-rolled peek: `is_identifier`/`is_open_bracket`
  // stay the classifiers, so soft keywords (`type`, `true`, …) remain named types.
  let core = match (
    named_type_core::<L, Ctx, Lang>,
    list_type_core::<L, Ctx, Lang>,
  )
    .peek_then_try_choice::<_, U1>(|mut peeked: Peeked<'_, 'inp, L, U1>, _emitter| {
      Ok(match peeked.pop_front() {
        None => None,
        Some(tok) => {
          let t = tok.token();
          if t.is_identifier() {
            Some(Branch::B0)
          } else if t.is_open_bracket() {
            Some(Branch::B1)
          } else {
            None
          }
        }
      })
    })
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(core) => core,
    ParseAttempt::Decline => return unexpected_type(inp),
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
