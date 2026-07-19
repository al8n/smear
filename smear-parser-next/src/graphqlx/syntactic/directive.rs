//! GraphQLx `Directive`/`Directives` productions — executable and constant.
//!
//! [`directive`] parses `'@' Name Arguments?`; [`directives`] parses the greedy
//! `Directive+` run a field, selection, or definition may carry, declining to
//! `None` (no tokens consumed) when the next token is not `@`. The const twins
//! thread [`const_arguments`] instead of [`arguments`], exactly mirroring the
//! GraphQL productions over the GraphQLx value family.

use smear_lexer::{
  LitBlockStr, LitInlineStr,
  graphqlx::{LitFloat, LitInt},
};
use smear_scaffold::ast as scaffold;
use tokora::{
  InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  parser::list_of,
  token::{IdentifierToken, PunctuatorToken, PunctuatorTokenExt},
  utils::IntoComponents,
};

use super::argument::{arguments, const_arguments};
use crate::{
  combinator::{Equivalent, ErrorOf, LiteralValueToken, ParseCtx, SliceOf, at, ident},
  graphqlx::ast::{ConstDirective, ConstDirectives, Directive, Directives, Name},
};

/// Parses a `Directive` (`'@' Name Arguments?`).
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive) (GraphQLx values).
pub fn directive<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Directive<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  at(inp)?;
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let args = arguments(inp)?;
  let span = inp.span_since(&cursor);
  let directive = scaffold::Directive::new(span, name, args);
  Ok(directive)
}

/// Parses a constant `Directive` (`'@' Name ConstArguments?`).
///
/// Spec: [Directive](https://spec.graphql.org/draft/#Directive) (GraphQLx, const
/// context).
pub fn const_directive<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ConstDirective<SliceOf<'inp, L>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  at(inp)?;
  let (name_span, name_src) = ident(inp)?.into_components();
  let name = Name::new(name_span, name_src);
  let args = const_arguments(inp)?;
  let span = inp.span_since(&cursor);
  let directive = scaffold::Directive::new(span, name, args);
  Ok(directive)
}

/// Parses an optional `Directives` run (`Directive+`), declining to `None` (no
/// tokens consumed) when the next token is not `@`.
///
/// Spec: [Directives](https://spec.graphql.org/draft/#Directives) (GraphQLx
/// values).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn directives<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Directives<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let ds = list_of(directive, |t: &L::Token| {
    !<L::Token as PunctuatorTokenExt>::is_at(t)
  })(inp)?;
  if ds.is_empty() {
    return Ok(None);
  }
  let span = inp.span_since(&cursor);
  let directives = scaffold::Directives::new(span, ds);
  Ok(Some(directives))
}

/// The const twin of [`directives`]: an optional `ConstDirectives` run.
///
/// Spec: [Directives](https://spec.graphql.org/draft/#Directives) (GraphQLx,
/// const context).
// The `Result<Option<…>, …>` return is inherent to an optional generic production;
// factoring it into an alias would only move the same generics.
#[allow(clippy::type_complexity)]
pub fn const_directives<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<ConstDirectives<SliceOf<'inp, L>>>, ErrorOf<'inp, L, Ctx, Lang>>
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
  SliceOf<'inp, L>: Equivalent<str> + Clone,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  let cursor = inp.cursor().clone();
  let ds = list_of(const_directive, |t: &L::Token| {
    !<L::Token as PunctuatorTokenExt>::is_at(t)
  })(inp)?;
  if ds.is_empty() {
    return Ok(None);
  }
  let span = inp.span_since(&cursor);
  let directives = scaffold::Directives::new(span, ds);
  Ok(Some(directives))
}

#[cfg(test)]
mod tests;
