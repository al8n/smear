//! Owned text that is cheap to clone where the target has pointer-width compare-and-swap, and a
//! plain box where it has not, behind one API.
//!
//! # Why one seam, and why here
//!
//! A text the workspace builds once and then clones with its owner — a map key, an error's
//! message — is cheapest as smol-bytes 0.2's shared `Utf8Bytes`: up to 62 bytes inline, and one
//! reference-counted buffer past that, so a clone copies a few words or bumps a counter and never
//! allocates. That buffer needs pointer-width atomics, which `thumbv6m-none-eabi` and its kind do
//! not have. So the choice of representation is a function of the target, and it is made **once**,
//! here, rather than in every crate that holds such a text.
//!
//! This is the lowest crate its users reach: `graphql-proto`, through its reader tier. It sits
//! outside `repr/`, which `smear-noatomic` compiles with no dependency at all.
//!
//! # The two arms
//!
//! | target | [`FrozenStr`] | [`StrBuf`] | a clone |
//! |---|---|---|---|
//! | `target_has_atomic = "ptr"` | smol-bytes' shared `Utf8Bytes` | smol-bytes' `Utf8BytesMut` | inline copy or a counter bump |
//! | otherwise | `Box<str>` | `String` | a copy of the text |
//!
//! Equality, ordering and hashing are the text's in both arms, written through `as_str` rather
//! than derived, so a map keyed by a [`FrozenStr`] is searched with a `&str` and finds the same
//! entry whichever arm built it.
//!
//! `--cfg smear_seam_fallback` selects the second arm on a target that has the atomics, which is
//! how CI compiles it on a host.
//!
//! # Not API
//!
//! Both types are re-exported `#[doc(hidden)]`, and so are their inherent methods: this is a seam
//! between the workspace's crates, not a promise to anyone outside it.

use core::{
  borrow::Borrow,
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  ops::Deref,
};

#[cfg(all(target_has_atomic = "ptr", not(smear_seam_fallback)))]
type Frozen = smol_bytes_0_2::Utf8Bytes;
#[cfg(any(not(target_has_atomic = "ptr"), smear_seam_fallback))]
type Frozen = std::boxed::Box<str>;

#[cfg(all(target_has_atomic = "ptr", not(smear_seam_fallback)))]
type Buf = smol_bytes_0_2::Utf8BytesMut;
#[cfg(any(not(target_has_atomic = "ptr"), smear_seam_fallback))]
type Buf = std::string::String;

/// Hands a finished buffer over as frozen text: the same allocation, where the target has atomics.
#[cfg(all(target_has_atomic = "ptr", not(smear_seam_fallback)))]
#[inline]
fn frozen(buf: Buf) -> Frozen {
  buf.freeze()
}

/// Hands a finished buffer over as frozen text: `String::into_boxed_str`, which reallocates only
/// when the buffer holds more room than text.
#[cfg(any(not(target_has_atomic = "ptr"), smear_seam_fallback))]
#[inline]
fn frozen(buf: Buf) -> Frozen {
  buf.into_boxed_str()
}

/// Owned, immutable text.
///
/// smol-bytes 0.2's shared `Utf8Bytes` where the target has pointer-width atomics: up to 62 bytes
/// inline, and a shared reference-counted buffer past that. A `Box<str>` where it has not. The
/// module header has why the choice is the target's and why it is made here.
#[derive(Clone)]
pub struct FrozenStr(Frozen);

#[doc(hidden)]
impl FrozenStr {
  /// Returns the text.
  #[inline]
  pub fn as_str(&self) -> &str {
    &self.0
  }

  /// Returns the text's length in bytes.
  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  /// Returns whether the text is empty.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

impl From<&str> for FrozenStr {
  #[inline]
  fn from(text: &str) -> Self {
    Self(Frozen::from(text))
  }
}

impl From<std::string::String> for FrozenStr {
  #[inline]
  fn from(text: std::string::String) -> Self {
    Self(Frozen::from(text))
  }
}

impl Deref for FrozenStr {
  type Target = str;

  #[inline]
  fn deref(&self) -> &str {
    self.as_str()
  }
}

impl AsRef<str> for FrozenStr {
  #[inline]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl Borrow<str> for FrozenStr {
  #[inline]
  fn borrow(&self) -> &str {
    self.as_str()
  }
}

impl PartialEq for FrozenStr {
  #[inline]
  fn eq(&self, other: &Self) -> bool {
    self.as_str() == other.as_str()
  }
}

impl Eq for FrozenStr {}

impl PartialOrd for FrozenStr {
  #[inline]
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for FrozenStr {
  #[inline]
  fn cmp(&self, other: &Self) -> Ordering {
    self.as_str().cmp(other.as_str())
  }
}

impl Hash for FrozenStr {
  /// Hashes the text exactly as `str` does, which is what `Borrow<str>` requires of it.
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_str().hash(state);
  }
}

impl fmt::Debug for FrozenStr {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self.as_str(), f)
  }
}

impl fmt::Display for FrozenStr {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.as_str(), f)
  }
}

/// A text being built, which [`freeze`](StrBuf::freeze) turns into a [`FrozenStr`].
///
/// smol-bytes 0.2's `Utf8BytesMut` where the target has pointer-width atomics, and the freeze
/// hands its buffer over without a copy. A `String` where it has not.
pub struct StrBuf(Buf);

#[doc(hidden)]
impl StrBuf {
  /// Creates an empty buffer, which allocates nothing.
  #[inline]
  pub const fn new() -> Self {
    Self(Buf::new())
  }

  /// Creates an empty buffer with room for `capacity` bytes.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(Buf::with_capacity(capacity))
  }

  /// Appends `text`.
  #[inline]
  pub fn push_str(&mut self, text: &str) {
    self.0.push_str(text);
  }

  /// Returns the text built so far.
  #[inline]
  pub fn as_str(&self) -> &str {
    &self.0
  }

  /// Returns the length in bytes of the text built so far.
  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  /// Returns whether nothing has been written yet.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Returns the text, frozen: the same buffer where the target has atomics, so nothing is copied.
  #[inline]
  pub fn freeze(self) -> FrozenStr {
    FrozenStr(frozen(self.0))
  }
}

impl Default for StrBuf {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl fmt::Write for StrBuf {
  #[inline]
  fn write_str(&mut self, text: &str) -> fmt::Result {
    self.push_str(text);
    Ok(())
  }
}

// Both arms are shareable across threads, and the owners of these texts — an error, a map of a
// response — are sent and shared as values. Asserted rather than assumed, so an arm that lost it
// would fail here instead of at a distant `Send` bound.
const _: () = {
  const fn shareable<T: Send + Sync>() {}
  shareable::<FrozenStr>();
  shareable::<StrBuf>();
};
