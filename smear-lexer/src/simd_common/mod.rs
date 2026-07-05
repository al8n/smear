//! Dialect-agnostic SIMD-lexer primitives shared by the GraphQL and GraphQLx
//! SIMD lexers.
//!
//! These are the byte-category fast paths (trivia, identifiers), the
//! source-abstraction traits ([`AsBytes`], [`ScanSource`]) that let a lexer run
//! over `str`, `[u8]`, and the owned/shared source wrappers alike, and the
//! [`delegate_to_logos`] slow-path fallback that hands an unrecognized token to
//! a per-dialect Logos lexer. The dialect-specific dispatch loop (punctuation,
//! numbers, strings) lives in each dialect's own `simd` module and calls into
//! these.

use tokit::{
  Lexer, Source, Token,
  lexer::{FromLogos, LogosLexer},
  state::recursion_tracker::RecursionLimiter,
};

/// Maximum byte recursion depth — matches the default in
/// [`tokit::state::recursion_tracker::RecursionLimiter`].
pub const DEFAULT_RECURSION_LIMIT: usize = 500;

/// Borrow a source slice as raw bytes for SIMD scanning.
///
/// The SIMD fast paths operate on `&[u8]` regardless of a source's primitive
/// type, so every source-slice type the lexer accepts implements this as a
/// zero-cost deref to its underlying bytes.
pub trait AsBytes {
  /// Returns the underlying bytes of this slice.
  fn as_bytes(&self) -> &[u8];
}

impl<T: ?Sized> AsBytes for &T
where
  T: AsBytes,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    (*self).as_bytes()
  }
}

impl AsBytes for str {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    str::as_bytes(self)
  }
}

impl AsBytes for [u8] {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

// Each of these is `tokit::Source::Slice` for an owned or shared source type
// (`bytes::Bytes`, `bstr::BStr`, `hipstr::HipStr`, `hipstr::HipByt` — see
// tokit's `src/source/{bytes_1,bstr_1,hipstr_0_8}.rs`), so implementing
// `AsBytes` for them is what lets a SIMD lexer run over those source types,
// exactly as it already does for `str`/`[u8]` above. Every impl is a trivial
// deref to `&[u8]` — no allocation, no copying.

#[cfg(feature = "bytes")]
impl AsBytes for bytes::Bytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "bstr")]
impl AsBytes for bstr::BStr {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "hipstr")]
impl AsBytes for hipstr::HipStr<'_> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    // `HipStr` derefs to `str`, not `[u8]` directly, so route through
    // `str::as_bytes` (same call as the `str` impl above; the `&HipStr ->
    // &str` coercion happens at the argument site).
    str::as_bytes(self)
  }
}

#[cfg(feature = "hipstr")]
impl AsBytes for hipstr::HipByt<'_> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_bytes(&self) -> &[u8] {
    self
  }
}

/// The primitive `str`/`[u8]` source that Logos actually scans for a source
/// `S`, plus how to borrow `S` as it.
///
/// The `token!`-generated Logos enums always scan `str`/`[u8]` and convert each
/// matched primitive slice up to `S::Slice` via `IntoEquivalent`, so
/// `LogosLexer`'s `Source` is always the primitive — never the owned/shared
/// wrapper. `delegate_to_logos` builds its `LogosLexer` over this primitive
/// view; the tokens it produces are still parameterized over `S::Slice` and
/// compare content-equal to the fast path's.
pub trait ScanSource: Source<usize> {
  /// The `str`/`[u8]` primitive Logos scans for this source. Identity for
  /// `str`/`[u8]`; the wrapper's deref target otherwise.
  type ScanPrimitive: Source<usize> + ?Sized;

  /// Borrow this source as the primitive Logos scans.
  fn scan_primitive(&self) -> &Self::ScanPrimitive;
}

impl ScanSource for str {
  type ScanPrimitive = str;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &str {
    self
  }
}

impl ScanSource for [u8] {
  type ScanPrimitive = [u8];

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "bytes")]
impl ScanSource for bytes::Bytes {
  type ScanPrimitive = [u8];

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "bstr")]
impl ScanSource for bstr::BStr {
  type ScanPrimitive = [u8];

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &[u8] {
    self
  }
}

#[cfg(feature = "hipstr")]
impl ScanSource for hipstr::HipStr<'_> {
  type ScanPrimitive = str;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &str {
    self
  }
}

#[cfg(feature = "hipstr")]
impl ScanSource for hipstr::HipByt<'_> {
  type ScanPrimitive = [u8];

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn scan_primitive(&self) -> &[u8] {
    self
  }
}

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
    end: usize,
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
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn delegate_to_logos<'inp, T>(
  scan_primitive: &'inp <LogosLexer<'inp, T> as Lexer<'inp>>::Source,
  cursor: usize,
  state: RecursionLimiter,
) -> Option<Delegated<'inp, T>>
where
  T: FromLogos<'inp>,
  LogosLexer<'inp, T>: Lexer<'inp, State = RecursionLimiter, Token = T, Offset = usize>,
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
      let end = logos.inner().span().end;
      Some(Delegated::Error { error, end })
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
#[cfg_attr(not(tarpaulin), inline(always))]
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

#[cfg(test)]
mod tests;
