//! GraphQL `Argument`/`Arguments` productions — executable and constant.
//!
//! [`argument`] and [`const_argument`] parse one `Name ':' Value` pair; [`arguments`]
//! and [`const_arguments`] parse the optional parenthesised argument list a field or
//! directive may carry, declining to `None` (no tokens consumed) unless the next
//! token is `(`. The spec grammar is `Arguments : ( Argument+ )`, but parser-next
//! stays lenient and accepts an empty `()` (user ruling, plan Amendment 5 — frozen
//! parity; see the fns' doc notes). The const twins thread [`const_value`] instead
//! of [`value`], exactly mirroring the frozen crate's split.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  punctuator::{LParen, RParen},
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{list_of, try_parens},
  span::{AsSpan, IntoSpan},
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  utils::IntoComponents,
};

use super::value::{const_value, value};
use crate::{
  combinator::{Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, ident},
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
/// is the right tool here); [`list_of`] collects zero-or-more arguments up to the
/// `)`, itself a `repeated_while(..).collect()` composition over the element parser.
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
  match try_parens(list_of(
    argument,
    <L::Token as PunctuatorTokenExt>::is_close_paren,
  ))(inp)?
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
/// Same [`try_parens`] + [`list_of`] composition as [`arguments`].
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
  match try_parens(list_of(
    const_argument,
    <L::Token as PunctuatorTokenExt>::is_close_paren,
  ))(inp)?
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
