//! Literal atoms: declining parsers that classify a GraphQL-family literal token
//! and hand back its extracted payload.
//!
//! tokora's [`LitToken`] classifies a token as int/float/string but stops short of
//! surfacing the payload. [`LiteralValueToken`] closes that gap: it extends
//! `LitToken` with by-value `into_*` extractors, so an atom can classify with the
//! `LitToken` predicate and then peel off the payload from the consumed token.
//! Each atom ([`try_int`], [`try_float`], [`try_inline_str`], [`try_block_str`],
//! [`try_string`]) declines without consuming when the next token is not its
//! literal, and otherwise returns the `(payload, span)` an assembly wraps into an
//! AST node.
//!
//! The atoms stay Lego-clean — generic over the [`LiteralValueToken`] capability
//! alone, naming no concrete dialect token. The per-dialect glue that actually
//! implements the capability for a concrete token lives in the dialect-gated
//! [`impls`] shim (GraphQL and GraphQLx); because it names dialect types it is
//! not part of the atom surface.

use tokora::{
  InputRef, Lexer, error::UnexpectedEot, token::LitToken, try_parse_input::ParseAttempt,
};

use super::{ErrorOf, ParseCtx};

#[cfg(any(feature = "graphql", feature = "graphqlx"))]
mod impls;

/// Payload extraction for GraphQL-family literal tokens.
///
/// tokora's [`LitToken`] supertrait answers *which* literal a token is; this
/// capability adds the by-value extractors that hand back the payload. Every
/// `into_*` returns `Err(self)` unchanged when the token is not that literal, so
/// a caller that has already classified the token can extract in one move and, on
/// a miss, recover the token untouched (which is how [`try_string`] tries the
/// inline carrier and falls back to the block carrier).
pub trait LiteralValueToken<'inp>: LitToken<'inp> {
  /// The extracted int payload (GraphQL yields the source slice).
  type Int;
  /// The extracted float payload (GraphQL yields the source slice).
  type Float;
  /// The extracted inline (single-line) string payload.
  type InlineStr;
  /// The extracted block (triple-quoted) string payload.
  type BlockStr;

  /// Extracts the int payload, returning `Err(self)` unchanged when the token is
  /// not an int literal.
  fn into_int(self) -> Result<Self::Int, Self>
  where
    Self: Sized;

  /// Extracts the float payload, returning `Err(self)` unchanged when the token
  /// is not a float literal.
  fn into_float(self) -> Result<Self::Float, Self>
  where
    Self: Sized;

  /// Extracts the inline string payload, returning `Err(self)` unchanged when the
  /// token is not an inline string literal.
  fn into_inline_str(self) -> Result<Self::InlineStr, Self>
  where
    Self: Sized;

  /// Extracts the block string payload, returning `Err(self)` unchanged when the
  /// token is not a block string literal.
  fn into_block_str(self) -> Result<Self::BlockStr, Self>
  where
    Self: Sized;
}

/// The inline-or-block payload [`try_string`] yields: the inline carrier or the
/// block carrier of a matched string-literal token.
#[derive(
  Debug,
  Clone,
  Copy,
  PartialEq,
  Eq,
  Hash,
  derive_more::IsVariant,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringLiteral<Inline, Block> {
  /// An inline (single-line) string payload.
  Inline(Inline),
  /// A block (triple-quoted) string payload.
  Block(Block),
}

/// The int payload the literal token `L` yields.
pub type IntOf<'inp, L> = <<L as Lexer<'inp>>::Token as LiteralValueToken<'inp>>::Int;

/// The float payload the literal token `L` yields.
pub type FloatOf<'inp, L> = <<L as Lexer<'inp>>::Token as LiteralValueToken<'inp>>::Float;

/// The inline string payload the literal token `L` yields.
pub type InlineStrOf<'inp, L> = <<L as Lexer<'inp>>::Token as LiteralValueToken<'inp>>::InlineStr;

/// The block string payload the literal token `L` yields.
pub type BlockStrOf<'inp, L> = <<L as Lexer<'inp>>::Token as LiteralValueToken<'inp>>::BlockStr;

/// The inline-or-block string payload [`try_string`] yields for the literal token
/// `L`.
pub type StringOf<'inp, L> = StringLiteral<InlineStrOf<'inp, L>, BlockStrOf<'inp, L>>;

/// The result a declining literal atom returns: an attempt at a `(payload, span)`
/// pair carrying payload `T`, or the context's emitted error.
pub type LitAttempt<'inp, L, Ctx, Lang, T> =
  Result<ParseAttempt<(T, <L as Lexer<'inp>>::Span)>, ErrorOf<'inp, L, Ctx, Lang>>;

/// Declines (no tokens consumed) unless the next token is an int literal, whose
/// payload and span it then returns.
#[inline]
pub fn try_int<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> LitAttempt<'inp, L, Ctx, Lang, IntOf<'inp, L>>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  inp
    .try_expect(|t| t.into_data().is_integer_literal())
    .map(|opt| {
      opt
        .map(|tok| {
          let (span, token) = tok.into_components();
          let value = match token.into_int() {
            Ok(value) => value,
            Err(_) => unreachable!("is_integer_literal implies into_int succeeds"),
          };
          (value, span)
        })
        .into()
    })
}

/// Declines (no tokens consumed) unless the next token is a float literal, whose
/// payload and span it then returns.
#[inline]
pub fn try_float<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> LitAttempt<'inp, L, Ctx, Lang, FloatOf<'inp, L>>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  inp
    .try_expect(|t| {
      // tokora keeps `is_hex_float_literal` separate from `is_float_literal`, so a
      // float atom must accept both to reach GraphQLx hex floats (`0x1.8p3`). The
      // GraphQL impl leaves `is_hex_float_literal` default-false, so it is
      // unaffected.
      let data = t.into_data();
      data.is_float_literal() || data.is_hex_float_literal()
    })
    .map(|opt| {
      opt
        .map(|tok| {
          let (span, token) = tok.into_components();
          let value = match token.into_float() {
            Ok(value) => value,
            Err(_) => {
              unreachable!("is_float_literal or is_hex_float_literal implies into_float succeeds")
            }
          };
          (value, span)
        })
        .into()
    })
}

/// Declines (no tokens consumed) unless the next token is an inline (single-line)
/// string literal, whose payload and span it then returns.
#[inline]
pub fn try_inline_str<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> LitAttempt<'inp, L, Ctx, Lang, InlineStrOf<'inp, L>>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  inp
    .try_expect(|t| t.into_data().is_inline_string_literal())
    .map(|opt| {
      opt
        .map(|tok| {
          let (span, token) = tok.into_components();
          let value = match token.into_inline_str() {
            Ok(value) => value,
            Err(_) => unreachable!("is_inline_string_literal implies into_inline_str succeeds"),
          };
          (value, span)
        })
        .into()
    })
}

/// Declines (no tokens consumed) unless the next token is a block (triple-quoted)
/// string literal, whose payload and span it then returns.
#[inline]
pub fn try_block_str<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> LitAttempt<'inp, L, Ctx, Lang, BlockStrOf<'inp, L>>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  inp
    .try_expect(|t| t.into_data().is_multiline_string_literal())
    .map(|opt| {
      opt
        .map(|tok| {
          let (span, token) = tok.into_components();
          let value = match token.into_block_str() {
            Ok(value) => value,
            Err(_) => unreachable!("is_multiline_string_literal implies into_block_str succeeds"),
          };
          (value, span)
        })
        .into()
    })
}

/// Declines (no tokens consumed) unless the next token is a string literal —
/// inline or block — whose payload (tagged [`StringLiteral::Inline`] or
/// [`StringLiteral::Block`]) and span it then returns.
#[inline]
pub fn try_string<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> LitAttempt<'inp, L, Ctx, Lang, StringOf<'inp, L>>
where
  L: Lexer<'inp>,
  L::Token: LiteralValueToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  inp
    .try_expect(|t| t.into_data().is_string_literal())
    .map(|opt| {
      opt
        .map(|tok| {
          let (span, token) = tok.into_components();
          let value = match token.into_inline_str() {
            Ok(inline) => StringLiteral::Inline(inline),
            Err(token) => match token.into_block_str() {
              Ok(block) => StringLiteral::Block(block),
              Err(_) => unreachable!("is_string_literal implies an inline or block string"),
            },
          };
          (value, span)
        })
        .into()
    })
}

#[cfg(test)]
mod tests;
