//! The one seam between the validator and the caller's diagnostic storage.
//!
//! # The validator owns nothing
//!
//! There is no diagnostic buffer inside `validate_executable`. The caller passes a [`Sink`], the
//! validator hands it values, and the sink decides what happens to them — the obligation pattern
//! `hick`/`mdns-proto` uses for I/O, applied to reporting. That is what makes the fast path free:
//! with [`First`] the compiler can see that no collection is ever built, and a server that only
//! needs a verdict allocates nothing at all.
//!
//! # One method, not a trait family
//!
//! `tokora`'s parser has an emitter bundle per error kind because parsing has many kinds with
//! different recovery decisions. Validation has one diagnostic family and exactly one decision —
//! keep going, or stop — so the whole seam is one method returning [`ControlFlow`]. The four
//! provided implementations mirror the *semantics* of `tokora`'s `Fatal`, `Verbose`, `Silent` and
//! `Ignored` bundles:
//!
//! | this module | `tokora` | behaviour |
//! |---|---|---|
//! | [`First`] | `Fatal` | keep the first diagnostic, stop validating |
//! | [`Collect`] | `Verbose` | append to the caller's `Vec`, keep going |
//! | [`Count`] | `Silent` | count, keep nothing, keep going |
//! | [`Ignore`] | `Ignored` | discard, keep going |

use core::ops::ControlFlow;

use std::vec::Vec;

use super::Diagnostic;

/// Caller-provided diagnostic storage.
///
/// Returning [`ControlFlow::Break`] stops validation; the entry point then reports that it
/// stopped early through [`Invalid::stopped`](super::Invalid::stopped). Returning
/// [`ControlFlow::Continue`] asks for the rest of the document's diagnostics.
pub trait Sink<S> {
  /// Accepts one diagnostic, and says whether validation should continue.
  fn diagnostic(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()>;
}

impl<S, K> Sink<S> for &mut K
where
  K: Sink<S> + ?Sized,
{
  #[inline]
  fn diagnostic(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    (**self).diagnostic(diagnostic)
  }
}

/// Keeps the first diagnostic and stops — the server default.
///
/// This is the sink the zero-allocation path is built around: it holds one `Option`, never a
/// collection, and it breaks immediately, so an invalid document costs one partial traversal
/// rather than a full one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct First<S> {
  first: Option<Diagnostic<S>>,
}

impl<S> First<S> {
  /// Creates an empty sink.
  #[inline]
  pub const fn new() -> Self {
    Self { first: None }
  }

  /// Returns the diagnostic that stopped validation, if any.
  #[inline]
  pub const fn get(&self) -> Option<&Diagnostic<S>> {
    self.first.as_ref()
  }

  /// Takes the diagnostic out, leaving the sink empty and reusable.
  #[inline]
  pub fn take(&mut self) -> Option<Diagnostic<S>> {
    self.first.take()
  }

  /// Consumes the sink and returns the diagnostic.
  #[inline]
  pub fn into_inner(self) -> Option<Diagnostic<S>> {
    self.first
  }

  /// Empties the sink so it can be reused for the next request.
  #[inline]
  pub fn clear(&mut self) {
    self.first = None;
  }
}

impl<S> Default for First<S> {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl<S> Sink<S> for First<S> {
  #[inline]
  fn diagnostic(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    if self.first.is_none() {
      self.first = Some(diagnostic);
    }
    ControlFlow::Break(())
  }
}

/// Appends to a `Vec` the caller owns — the tooling default.
///
/// The borrow is what keeps the storage the caller's: capacity survives between requests because
/// the `Vec` does, and a caller who pre-sizes it keeps the whole path allocation-free.
#[derive(Debug, PartialEq, Eq)]
pub struct Collect<'a, S> {
  into: &'a mut Vec<Diagnostic<S>>,
}

impl<'a, S> Collect<'a, S> {
  /// Wraps the caller's storage.
  #[inline]
  pub fn new(into: &'a mut Vec<Diagnostic<S>>) -> Self {
    Self { into }
  }

  /// Returns what has been collected so far.
  #[inline]
  pub fn diagnostics(&self) -> &[Diagnostic<S>] {
    self.into
  }
}

impl<'a, S> From<&'a mut Vec<Diagnostic<S>>> for Collect<'a, S> {
  #[inline]
  fn from(into: &'a mut Vec<Diagnostic<S>>) -> Self {
    Self::new(into)
  }
}

impl<S> Sink<S> for Collect<'_, S> {
  #[inline]
  fn diagnostic(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    self.into.push(diagnostic);
    ControlFlow::Continue(())
  }
}

/// Counts diagnostics and keeps none.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Count {
  count: u32,
}

impl Count {
  /// Creates a sink at zero.
  #[inline]
  pub const fn new() -> Self {
    Self { count: 0 }
  }

  /// Returns how many diagnostics have been accepted.
  #[inline]
  pub const fn get(&self) -> u32 {
    self.count
  }

  /// Resets the counter so the sink can be reused.
  #[inline]
  pub const fn clear(&mut self) {
    self.count = 0;
  }
}

impl<S> Sink<S> for Count {
  #[inline]
  fn diagnostic(&mut self, _: Diagnostic<S>) -> ControlFlow<()> {
    self.count = self.count.saturating_add(1);
    ControlFlow::Continue(())
  }
}

/// Discards every diagnostic and keeps validating.
///
/// The verdict still comes back: `validate_executable` counts what it emitted regardless of what
/// the sink did with it, so `Ignore` reports "invalid, and I am not telling you why" — useful for
/// a warm-up pass or a conformance harness that only wants the boolean.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Ignore;

impl<S> Sink<S> for Ignore {
  #[inline]
  fn diagnostic(&mut self, _: Diagnostic<S>) -> ControlFlow<()> {
    ControlFlow::Continue(())
  }
}
