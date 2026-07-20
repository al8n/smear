//! The dialect shims for [`LiteralValueToken`](super::LiteralValueToken).
//!
//! Each block names a concrete dialect `SyntacticToken` and its payload/string
//! carriers, so it is per-dialect glue rather than part of the generic atom
//! surface — the Lego rule binds the atoms, not this capability implementation.
//! GraphQL yields the raw source slice for ints and floats (one radix each);
//! GraphQLx preserves the radix by yielding the `LitInt`/`LitFloat` payload enums.

use smear_lexer::{LitBlockStr, LitInlineStr};
use tokora::Slice;

use super::LiteralValueToken;

/// GraphQL: ints and floats are the raw source slice (a single radix each).
#[cfg(feature = "graphql")]
impl<'inp, S> LiteralValueToken<'inp> for smear_lexer::graphql::syntactic::SyntacticToken<S>
where
  S: Slice<'inp> + Clone + 'inp,
{
  type Int = S;
  type Float = S;
  type InlineStr = LitInlineStr<S>;
  type BlockStr = LitBlockStr<S>;

  #[inline(always)]
  fn into_int(self) -> Result<S, Self> {
    match self {
      Self::LitInt(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline(always)]
  fn into_float(self) -> Result<S, Self> {
    match self {
      Self::LitFloat(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline(always)]
  fn into_inline_str(self) -> Result<LitInlineStr<S>, Self> {
    match self {
      Self::LitInlineStr(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline(always)]
  fn into_block_str(self) -> Result<LitBlockStr<S>, Self> {
    match self {
      Self::LitBlockStr(value) => Ok(value),
      other => Err(other),
    }
  }

  #[inline(always)]
  fn is_literal(&self) -> bool {
    matches!(
      self,
      Self::LitInt(_) | Self::LitFloat(_) | Self::LitInlineStr(_) | Self::LitBlockStr(_)
    )
  }
}

/// GraphQLx: ints and floats preserve their radix via the `LitInt`/`LitFloat`
/// payload enums (decimal/hex/binary/octal ints, decimal/hex floats); the string
/// carriers are shared with GraphQL.
#[cfg(feature = "graphqlx")]
#[cfg_attr(docsrs, doc(cfg(feature = "graphqlx")))]
const _: () = {
  use smear_lexer::graphqlx::{LitFloat, LitInt, syntactic::SyntacticToken};

  impl<'inp, S> LiteralValueToken<'inp> for SyntacticToken<S>
  where
    S: Slice<'inp> + Clone + 'inp,
  {
    type Int = LitInt<S>;
    type Float = LitFloat<S>;
    type InlineStr = LitInlineStr<S>;
    type BlockStr = LitBlockStr<S>;

    #[inline(always)]
    fn into_int(self) -> Result<LitInt<S>, Self> {
      match self {
        Self::LitInt(value) => Ok(value),
        other => Err(other),
      }
    }

    #[inline(always)]
    fn into_float(self) -> Result<LitFloat<S>, Self> {
      match self {
        Self::LitFloat(value) => Ok(value),
        other => Err(other),
      }
    }

    #[inline(always)]
    fn into_inline_str(self) -> Result<LitInlineStr<S>, Self> {
      match self {
        Self::LitInlineStr(value) => Ok(value),
        other => Err(other),
      }
    }

    #[inline(always)]
    fn into_block_str(self) -> Result<LitBlockStr<S>, Self> {
      match self {
        Self::LitBlockStr(value) => Ok(value),
        other => Err(other),
      }
    }

    #[inline(always)]
    fn is_literal(&self) -> bool {
      matches!(
        self,
        Self::LitInt(_) | Self::LitFloat(_) | Self::LitInlineStr(_) | Self::LitBlockStr(_)
      )
    }
  }
};
