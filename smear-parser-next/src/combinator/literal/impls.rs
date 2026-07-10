//! The GraphQL dialect shim for [`LiteralValueToken`](super::LiteralValueToken).
//!
//! This module names the concrete GraphQL [`SyntacticToken`] and its string
//! carriers, so it is per-dialect glue rather than part of the generic atom
//! surface — the Lego rule binds the atoms, not this capability implementation.

use smear_lexer::{LitBlockStr, LitInlineStr, graphql::syntactic::SyntacticToken};
use tokora::Slice;

use super::LiteralValueToken;

impl<'inp, S> LiteralValueToken<'inp> for SyntacticToken<S>
where
  S: Slice<'inp> + Clone + 'inp,
{
  type Int = S;
  type Float = S;
  type InlineStr = LitInlineStr<S>;
  type BlockStr = LitBlockStr<S>;

  #[inline]
  fn into_int(self) -> Result<S, Self> {
    match self {
      Self::LitInt(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline]
  fn into_float(self) -> Result<S, Self> {
    match self {
      Self::LitFloat(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline]
  fn into_inline_str(self) -> Result<LitInlineStr<S>, Self> {
    match self {
      Self::LitInlineStr(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline]
  fn into_block_str(self) -> Result<LitBlockStr<S>, Self> {
    match self {
      Self::LitBlockStr(value) => Ok(value),
      other => Err(other),
    }
  }
}
