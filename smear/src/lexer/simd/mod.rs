//! Dialect-agnostic SIMD-lexer primitives shared by the GraphQL and GraphQLx
//! SIMD lexers.
//!
//! These are the byte-category fast paths (trivia, identifiers), the
//! source-abstraction traits ([`AsBytes`]) that let a lexer run
//! over `str`, `[u8]`, and the owned/shared source wrappers alike, and the
//! [`delegate_to_logos`] slow-path fallback that hands an unrecognized token to
//! a per-dialect Logos lexer. The dialect-specific dispatch loop (punctuation,
//! numbers, strings) lives in each dialect's own `syntactic` module and calls into
//! these.

use tokora::{
  Lexer, SimpleSpan, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::RecursionLimiter,
};

/// Maximum byte recursion depth — matches the default in
/// [`tokora::state::recursion_tracker::RecursionLimiter`].
pub const DEFAULT_RECURSION_LIMIT: usize = 500;

pub(crate) type LogosSourceOf<'inp, T> =
  <<T as FromLogos<'inp>>::Logos as tokora::logos::Logos<'inp>>::Source;

/// Outcome of delegating one token to a Logos lexer via [`delegate_to_logos`].
///
/// The two variants carry exactly what the caller must fold back into its own
/// state: on a token, the post-lex recursion state is propagated; on an error,
/// it deliberately is not (the `Error` variant has no `state` field), matching
/// the pre-SIMD lexer, which never advances recursion state across an error.
///
/// Bounded by `Token<'inp>` rather than `FromLogos<'inp>`: the `Error` variant
/// is the only field that constrains `T` (it names `<T as Token<'inp>>::Error`),
/// and `FromLogos<'inp>: Token<'inp>` is strictly stronger than this type
/// itself needs. `delegate_to_logos` — the sole constructor — still requires
/// `T: FromLogos<'inp>` to build the `LogosLexer`, so every actual caller
/// satisfies this bound already.
pub(crate) enum Delegated<'inp, T: Token<'inp>> {
  Token {
    token: T,
    end: usize,
    state: RecursionLimiter,
  },
  Error {
    error: <T as Token<'inp>>::Error,
    span: SimpleSpan,
  },
}

/// Delegate the token starting at `cursor` to a fresh Logos lexer over
/// `scan_primitive`, seeded with `state`.
///
/// This is the shared slow-path fallback for both dialects' SIMD lexers: the
/// dispatch `_` arm uses it for every byte the fast paths don't claim, and the
/// number/`.` fast paths use it for any anomaly they detect. Logos re-derives
/// the token — or the exact error — from scratch, so parity with the pre-SIMD
/// lexer holds by construction: nothing here constructs an error itself. The
/// caller supplies the token type `T` (its own `SyntacticToken`) and folds the
/// returned [`Delegated`] outcome back into its cursor/span/state.
#[inline(always)]
pub(crate) fn delegate_to_logos<'inp, T>(
  scan_primitive: &'inp LogosSourceOf<'inp, T>,
  cursor: usize,
  state: RecursionLimiter,
) -> Option<Delegated<'inp, T>>
where
  T: FromLogos<'inp>,
  LogosLexer<'inp, T>: Lexer<
      'inp,
      State = RecursionLimiter,
      Source = LogosSourceOf<'inp, T>,
      Token = T,
      Offset = usize,
    >,
{
  let mut logos = LogosLexer::with_state(scan_primitive, state);
  logos.bump(&cursor);
  match logos.lex()? {
    Ok(token) => {
      let end = logos.inner().span().end;
      Some(Delegated::Token {
        token,
        end,
        state: *logos.state(),
      })
    }
    Err(error) => {
      // Carry the delegated lexer's own error-token span (absolute offsets)
      // so the caller's `span()`/`slice()` report the error token exactly like
      // `LogosLexer`.
      let span: SimpleSpan = logos.inner().span().into();
      Some(Delegated::Error { error, span })
    }
  }
}

/// Length of the identifier run starting at `bytes[0]`.
///
/// The caller has already proved `bytes[0]` is an identifier-start byte
/// (`[a-zA-Z_]`), so the run is non-empty by construction and scanning starts
/// at index 1. Splitting on the remaining length keeps short idents (the common
/// case — most identifiers are < 16 bytes) on a tight inlined scalar loop,
/// while long idents (e.g. enum value names in schemas) get the SIMD
/// dispatcher. The caller advances its own cursor by the returned length.
#[inline(always)]
pub(crate) fn scan_identifier(bytes: &[u8]) -> usize {
  let total = bytes.len();
  let mut end = 1;

  if total - end >= 32 {
    // Long-ident path: amortize the SIMD dispatcher across enough bytes to
    // pay for itself.
    end += memspan::skip::skip_ident(&bytes[end..]);
  } else {
    // Short-ident path: a tight branchy loop with a known-tiny upper bound.
    // LLVM keeps this in the icache and the per-iteration cost beats any
    // function call dispatch for ≤ ~24-byte idents.
    while end < total && is_ident_continue(bytes[end]) {
      end += 1;
    }
  }

  end
}

#[inline(always)]
fn is_ident_continue(b: u8) -> bool {
  matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

// Macro-generated SIMD-accelerated skipper for the four whitespace bytes plus
// comma. BOM and comments are handled separately because they're multi-byte
// sequences that don't fit a single-byte class.
memspan::skip_class! {
  pub(crate) fn skip_ws_and_comma(bytes = [b' ', b'\t', b'\r', b'\n', b',']);
}

/// Find the position of the next `\n` or `\r` in `input`, used for terminating
/// `#`-style comments.
#[inline(always)]
pub(crate) fn memchr_newline(input: &[u8]) -> Option<usize> {
  // Trivial scalar fallback first (most comments are short and contain no
  // special bytes between `#` and EOL). The branchy scalar loop is hard to
  // beat for short tails.
  if input.len() < 32 {
    return input.iter().position(|&b| b == b'\n' || b == b'\r');
  }
  // For longer comments we hand off to memchr2 which has the same
  // SIMD-saturation guarantees as memchr — a single `vceqq + vorrq +
  // shrn-extract` loop on aarch64.
  ::memspan::skip::skip_until(input, b"\n\r")
}

/// The dialect-agnostic validity-only fast scanner for **decimal** numeric
/// literals (`Int` / `Float`), shared by both dialects' SIMD lexers. Radix and
/// hex-float forms (GraphQLx) are never fast-pathed here — they return `None`
/// and delegate to the per-dialect `NumberToken` grammar.
pub(crate) mod scan;

pub(crate) use scan::{NumberKind, scan_number};
