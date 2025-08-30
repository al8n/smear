use chumsky::span;

/// The span
#[derive(Debug, Clone, Copy)]
pub struct Span<S> {
  start: usize,
  end: usize,
  state: S,
}

impl<S> Span<S> {
  /// Creates a new span.
  ///
  /// The start is inclusive and the end is exclusive.
  #[inline(always)]
  pub const fn new(start: usize, end: usize, state: S) -> Self {
    Self { start, end, state }
  }

  /// Returns the start of the span.
  #[inline(always)]
  pub const fn start(&self) -> usize {
    self.start
  }

  /// Returns the end of the span.
  #[inline(always)]
  pub const fn end(&self) -> usize {
    self.end
  }
}

impl<S: Copy> span::Span for Span<S> {
  type Context = S;

  type Offset = usize;

  #[inline(always)]
  fn new(ctx: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
    Self::new(range.start, range.end, ctx)
  }

  #[inline(always)]
  fn context(&self) -> Self::Context {
    self.state
  }

  #[inline(always)]
  fn start(&self) -> Self::Offset {
    self.start
  }

  #[inline(always)]
  fn end(&self) -> Self::Offset {
    self.end
  }
}
