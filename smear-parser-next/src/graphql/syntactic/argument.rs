//! GraphQL `Argument`/`Arguments` productions — executable and constant.
//!
//! [`argument`] and [`const_argument`] each parse one committed `Name ':' Value`
//! pair. [`arguments`] and [`const_arguments`] parse the optional parenthesised
//! argument list a field or directive may carry, declining to `None` (no tokens
//! consumed) unless the next token is `(`.
//!
//! The list body is the decision-driven json-example shape (plan Amendments 6–7):
//! the committed [`argument`] item, driven by
//! [`repeated_while`](tokora::ParseInput::repeated_while) over a two-token decision —
//! an argument's FIRST set is `Name ':'`, so the list continues only while the next
//! two tokens are an identifier followed by a colon. The decision, not an
//! attempt-then-rewind, is what ends the list, so there is no per-item backtrack and
//! a bare identifier without a colon — `( foo )` — stops the list and is reported by
//! the closing-paren check rather than entering an argument that then fails on the
//! missing colon.
//!
//! The parentheses frame the body through [`try_lparen`] (the declining opener — its
//! decline is the whole production's decline) and a committed [`rparen`] close, and
//! are then dropped: parser-next's AST keeps no delimiter tokens, only the whole-list
//! span (plan Amendment 7). This is deliberately *not* the many-builder's
//! `.delimited::<Paren>()`: over `repeated_while` that stage tolerates a missing
//! closer at end of input (it returns the collected elements instead of erroring),
//! which would accept an unterminated `( … ` against the spec and frozen parity — so
//! the strict `rparen` close is used instead.
//!
//! Empty `()` is accepted (zero-or-more): a leniency deviation from the spec's
//! `Arguments : ( Argument+ )` (plan Amendment 5 — frozen parity; the spec demands
//! one-or-more, but the frozen parser's `+` is unenforced and parser-next matches
//! it). The const twins thread [`const_value`] instead of [`value`], exactly
//! mirroring the frozen crate's split.

use std::vec::Vec;

use smear_lexer::{LitBlockStr, LitInlineStr};
use smear_scaffold::ast as scaffold;
use tokora::{
  Accumulator, InputRef, Lexer, ParseInput, SimpleSpan, Token,
  cache::{Peeked, PeekedTokenExt},
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  span::AsSpan,
  token::{IdentifierToken, LitToken, PunctuatorToken, PunctuatorTokenExt},
  try_parse_input::ParseAttempt,
  utils::{IntoComponents, typenum::U3},
};

use super::value::{const_value, value, value_head_kind};
use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, ident, rparen, try_lparen,
  },
  graphql::ast::{Argument, ArgumentList, Arguments, ConstArgument, ConstArguments, Name},
};

/// Parses an `Argument` (`Name ':' Value`).
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument).
pub fn argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Argument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let arg = scaffold::Argument::new(span, name, value);
  Ok(arg)
}

/// Parses a constant `Argument` (`Name ':' ConstValue`).
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument) (const context).
pub fn const_argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstArgument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
{
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = const_value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let arg = scaffold::Argument::new(span, name, value);
  Ok(arg)
}

/// The list-continuation decision for [`arguments`]/[`const_arguments`]: an
/// `Argument`'s FIRST set is `Name ':' Value`, so the list continues
/// ([`Action::Continue`]) only while the next three tokens are an identifier, a
/// colon, and a value head (plan Amendment 7, U3 name-colon-value window), and stops
/// ([`Action::Stop`]) otherwise — at the closing `)`, at end of input, or on any
/// other head (a bare identifier without a colon included, so `( foo )` stops the
/// list and is reported by the closing-paren check). The value-head classification
/// is shared with the value dispatcher ([`value_head_kind`](super::value::value_head_kind)),
/// one source of truth.
fn decide_argument_head<'inp, L, Ctx, Lang>(
  mut peeked: Peeked<'_, 'inp, L, U3>,
  _: &mut Ctx::Emitter,
) -> Result<Action, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: LitToken<'inp> + IdentifierToken<'inp> + PunctuatorToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
{
  let Some(head) = peeked.pop_front() else {
    return Ok(Action::Stop);
  };
  if !head.token().is_identifier() {
    return Ok(Action::Stop);
  }
  let Some(colon) = peeked.pop_front() else {
    return Ok(Action::Stop);
  };
  if !colon.token().is_colon() {
    return Ok(Action::Stop);
  }
  Ok(match peeked.pop_front() {
    Some(value) if value_head_kind::<L>(value.token()).is_some() => Action::Continue,
    _ => Action::Stop,
  })
}

/// Parses an optional `Arguments` list (`'(' Argument* ')'`), declining to `None`
/// (no tokens consumed) unless the next token is `(`.
///
/// The declining [`try_lparen`] opener is the production's decline; once `(` is
/// consumed the body is the committed [`argument`] item driven by
/// `decide_argument_head`, and a committed [`rparen`] requires the close (an
/// unterminated `( …` errors). The parens are dropped — the AST keeps only the
/// whole-list span, no delimiter tokens (plan Amendment 7); empty `()` yields an
/// empty list (leniency deviation, plan Amendment 5 — frozen parity).
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn arguments<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Arguments<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
{
  let cursor = inp.cursor().clone();
  match try_lparen(inp)? {
    ParseAttempt::Decline => Ok(None),
    ParseAttempt::Accept(_open) => {
      let items: Vec<Argument<SliceOf<'inp, L>>> = argument
        .repeated_while::<_, U3>(decide_argument_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rparen(inp)?;
      let span = inp.span_since(&cursor);
      Ok(Some(ArgumentList::new(span, items)))
    }
  }
}

/// The const twin of [`arguments`]: an optional `ConstArguments` list.
///
/// Same declining [`try_lparen`] opener, `decide_argument_head`-driven body, and
/// committed [`rparen`] close as [`arguments`], with the committed [`const_argument`]
/// item.
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments) (const context).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn const_arguments<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ConstArguments<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
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
{
  let cursor = inp.cursor().clone();
  match try_lparen(inp)? {
    ParseAttempt::Decline => Ok(None),
    ParseAttempt::Accept(_open) => {
      let items: Vec<ConstArgument<SliceOf<'inp, L>>> = const_argument
        .repeated_while::<_, U3>(decide_argument_head::<L, Ctx, Lang>)
        .collect()
        .parse_input(inp)?;
      rparen(inp)?;
      let span = inp.span_since(&cursor);
      Ok(Some(ArgumentList::new(span, items)))
    }
  }
}

#[cfg(test)]
mod tests;
