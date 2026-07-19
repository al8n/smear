//! GraphQL `Argument`/`Arguments` productions — executable and constant.
//!
//! [`argument`] and [`const_argument`] parse one `Name ':' Value` pair; [`try_argument`]
//! and [`try_const_argument`] are their declining twins — decline (no tokens consumed)
//! unless the next token is an identifier, otherwise commit to the same `':' Value` tail.
//! [`arguments`] and [`const_arguments`] parse the optional parenthesised argument list a
//! field or directive may carry, declining to `None` (no tokens consumed) unless the next
//! token is `(`. The spec grammar is `Arguments : ( Argument+ )`, but parser-next
//! stays lenient and accepts an empty `()` (user ruling, plan Amendment 5 — frozen
//! parity; see the fns' doc notes). The const twins thread [`const_value`] instead
//! of [`value`], exactly mirroring the frozen crate's split.

use std::vec::Vec;

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  punctuator::{LParen, RParen},
};
use smear_scaffold::ast as scaffold;
use tokora::{
  Accumulator, InputRef, Lexer, SimpleSpan, Token, TryParseInput,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::try_parens,
  span::{AsSpan, IntoSpan},
  token::{IdentifierToken, PunctuatorToken},
  try_parse_input::ParseAttempt,
  utils::IntoComponents,
};

use super::value::{const_value, value};
use crate::{
  combinator::{
    Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, ident, try_ident,
  },
  graphql::ast::{Argument, Arguments, ConstArgument, ConstArguments, Name},
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

/// Declines (no tokens consumed) unless the next token is an identifier, in which
/// case it commits to the following `':' Value` tail exactly as [`argument`] does.
///
/// The attempt boundary is the leading identifier alone: once consumed, a missing
/// `:` or value is an error, never a decline. [`arguments`]' builder chain repeats
/// over this atom — its own decline is what ends the list, so the chain needs no
/// separate until-predicate.
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument).
// The `Result<ParseAttempt<…>, …>` return is inherent to a declining generic
// production; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn try_argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<Argument<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  match try_ident(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(id) => {
      let (name_span, name_src) = id.into_components();
      let name = Name::new(name_span, name_src);
      colon(inp)?;
      let value = value(inp)?;
      let span = SimpleSpan::new(name.span().start(), value.as_span().end());
      Ok(ParseAttempt::Accept(scaffold::Argument::new(
        span, name, value,
      )))
    }
  }
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

/// The const twin of [`try_argument`]: declines (no tokens consumed) unless the
/// next token is an identifier, in which case it commits to the following
/// `':' ConstValue` tail exactly as [`const_argument`] does.
///
/// [`const_arguments`]' builder chain repeats over this atom the same way
/// [`arguments`]' repeats over [`try_argument`].
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument) (const context).
// The `Result<ParseAttempt<…>, …>` return is inherent to a declining generic
// production; factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn try_const_argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<ConstArgument<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  match try_ident(inp)? {
    ParseAttempt::Decline => Ok(ParseAttempt::Decline),
    ParseAttempt::Accept(id) => {
      let (name_span, name_src) = id.into_components();
      let name = Name::new(name_span, name_src);
      colon(inp)?;
      let value = const_value(inp)?;
      let span = SimpleSpan::new(name.span().start(), value.as_span().end());
      Ok(ParseAttempt::Accept(scaffold::Argument::new(
        span, name, value,
      )))
    }
  }
}

/// Parses an optional `Arguments` list (`'(' Argument* ')'`), declining to `None`
/// (no tokens consumed) unless the next token is `(`.
///
/// Leniency deviation from the spec (user ruling, plan Amendment 5 — REVERSES the
/// Amendment-2 entry for this site): the spec's `Arguments : ( Argument+ )` demands
/// one-or-more, but parser-next stays lenient and accepts an empty `()` here,
/// matching the frozen parser's unenforced `+` (frozen parity). All other
/// Amendment-2 cardinality sites keep their spec enforcement.
///
/// Composition: [`try_parens`] supplies the declining outer region (the many-builder
/// surface's own `.repeated().delimited::<D>()` has no declining twin — a missing
/// opener there is a hard error, not a decline — so the single-region attempt shape
/// is still the right tool for the paren frame). Inside it, [`try_argument`]
/// `.repeated().collect()` gathers zero-or-more arguments — the tokora json-example
/// builder pattern, minus the separator stage: GraphQL argument lists have no
/// separator token (commas are trivia in this lexer), so `try_argument`'s own
/// decline is what ends the list, with no `is_close_paren`-style until-predicate to
/// thread through a second combinator.
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
  match try_parens::<_, _, _, _, Vec<Argument<SliceOf<'inp, L>>>, _>(
    try_argument.repeated().collect_with(Vec::new()),
  )(inp)?
  {
    Some(delimited) => {
      let (span, open, close, items) = delimited.into_components();
      let args = scaffold::Arguments::new(
        span,
        LParen::new(open.into_span()),
        items,
        RParen::new(close.into_span()),
      );
      Ok(Some(args))
    }
    None => Ok(None),
  }
}

/// The const twin of [`arguments`]: an optional `ConstArguments` list.
///
/// Leniency deviation from the spec (user ruling, plan Amendment 5 — REVERSES the
/// Amendment-2 entry for this site): the spec's `Arguments : ( Argument+ )` demands
/// one-or-more, but parser-next stays lenient and accepts an empty `()` here,
/// matching the frozen parser's unenforced `+` (frozen parity). All other
/// Amendment-2 cardinality sites keep their spec enforcement.
///
/// Same [`try_parens`] + [`try_const_argument`]`.repeated().collect()` composition
/// as [`arguments`].
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
  match try_parens::<_, _, _, _, Vec<ConstArgument<SliceOf<'inp, L>>>, _>(
    try_const_argument.repeated().collect_with(Vec::new()),
  )(inp)?
  {
    Some(delimited) => {
      let (span, open, close, items) = delimited.into_components();
      let args = scaffold::Arguments::new(
        span,
        LParen::new(open.into_span()),
        items,
        RParen::new(close.into_span()),
      );
      Ok(Some(args))
    }
    None => Ok(None),
  }
}

#[cfg(test)]
mod tests;
