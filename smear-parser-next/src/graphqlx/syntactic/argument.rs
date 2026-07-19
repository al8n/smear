//! GraphQLx `Argument`/`Arguments` productions — executable and constant.
//!
//! [`argument`] and [`const_argument`] parse one `Name ':' Value` pair;
//! [`arguments`] and [`const_arguments`] parse the optional parenthesised
//! `Argument+` list a field or directive may carry, declining to `None` (no
//! tokens consumed) unless the next token is `(`. The const twins thread
//! [`const_value`] instead of [`value`], exactly mirroring the GraphQL
//! productions — the argument shape is dialect-shared; only the value family
//! underneath differs (radix-preserving literals, `set`/`map` composites,
//! `::`-path enums).
//!
//! # Node placement
//!
//! `Argument`'s kind is known up front, but its span is not settled until the
//! value is parsed, so — like `value`'s `object_field` — it uses the manual
//! `cst_mark`/`cst_start_at`/`cst_finish` retro-wrap rather than
//! [`node`](tokora::parser::node). `Arguments` is optional (Amendment 1: optional
//! node kinds retro-wrap manually, not [`node_opt`](tokora::parser::node_opt)):
//! the mark is minted before the attempt and spent only when the parens are
//! actually present.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  graphqlx::{LitFloat, LitInt},
  punctuator::{LParen, RParen},
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  emitter::CstEmitter,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::{list_of, try_parens},
  span::{AsSpan, IntoSpan},
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  utils::IntoComponents,
};

use super::value::{const_value, value};
use crate::{
  combinator::{Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, colon, ident},
  graphqlx::{
    ast::{Argument, Arguments, ConstArgument, ConstArguments, Name},
    kinds::SyntaxKind as K,
  },
};

/// Parses an `Argument` (`Name ':' Value`).
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument) (GraphQLx values).
pub fn argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Argument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let arg = scaffold::Argument::new(span, name, value);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::Argument.raw());
  emitter.cst_finish();
  Ok(arg)
}

/// Parses a constant `Argument` (`Name ':' ConstValue`).
///
/// Spec: [Argument](https://spec.graphql.org/draft/#Argument) (GraphQLx, const
/// context).
pub fn const_argument<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstArgument<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp, Span = SimpleSpan>,
  L::Token: IdentifierToken<'inp>
    + PunctuatorToken<'inp>
    + LiteralValueToken<
      'inp,
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  colon(inp)?;
  let value = const_value(inp)?;
  let span = SimpleSpan::new(name.span().start(), value.as_span().end());
  let arg = scaffold::Argument::new(span, name, value);
  let emitter = inp.emitter();
  emitter.cst_start_at(mark, K::Argument.raw());
  emitter.cst_finish();
  Ok(arg)
}

/// Parses an optional `Arguments` list (`'(' Argument+ ')'`), declining to `None`
/// (no tokens consumed) unless the next token is `(`.
///
/// Spec cardinality (Amendment 2): `( Argument+ )` demands one-or-more, so an
/// empty `()` errors (the same known site as GraphQL's W2 retrofit).
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments) (GraphQLx values).
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
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_parens(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`Argument+`): the first argument is committed, so an empty
    // `()` errors at the `)` exactly as the committed ident atom reports it.
    let first = argument(inp)?;
    let mut items = list_of(argument, <L::Token as PunctuatorTokenExt>::is_close_paren)(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, open, close, items) = delimited.into_components();
      let args = scaffold::Arguments::new(
        span,
        LParen::new(open.into_span()),
        items,
        RParen::new(close.into_span()),
      );
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::Arguments.raw());
      emitter.cst_finish();
      Ok(Some(args))
    }
    None => Ok(None),
  }
}

/// The const twin of [`arguments`]: an optional `ConstArguments` list.
///
/// Spec cardinality (Amendment 2): `( Argument+ )` demands one-or-more, so an
/// empty `()` errors.
///
/// Spec: [Arguments](https://spec.graphql.org/draft/#Arguments) (GraphQLx, const
/// context).
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
      Int = LitInt<SliceOf<'inp, L>>,
      Float = LitFloat<SliceOf<'inp, L>>,
      InlineStr = LitInlineStr<SliceOf<'inp, L>>,
      BlockStr = LitBlockStr<SliceOf<'inp, L>>,
    >,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  Ctx::Emitter: CstEmitter<'inp, L, Lang>,
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let mark = inp.emitter().cst_mark();
  match try_parens(|inp: &mut InputRef<'inp, '_, L, Ctx, Lang>| {
    // Spec cardinality (`Argument+`): the first argument is committed, so an empty
    // `()` errors at the `)` exactly as the committed ident atom reports it.
    let first = const_argument(inp)?;
    let mut items = list_of(
      const_argument,
      <L::Token as PunctuatorTokenExt>::is_close_paren,
    )(inp)?;
    items.insert(0, first);
    Ok(items)
  })(inp)?
  {
    Some(delimited) => {
      let (span, open, close, items) = delimited.into_components();
      let args = scaffold::Arguments::new(
        span,
        LParen::new(open.into_span()),
        items,
        RParen::new(close.into_span()),
      );
      let emitter = inp.emitter();
      emitter.cst_start_at(mark, K::Arguments.raw());
      emitter.cst_finish();
      Ok(Some(args))
    }
    None => Ok(None),
  }
}

#[cfg(test)]
mod tests;
