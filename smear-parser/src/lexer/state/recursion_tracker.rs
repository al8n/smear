/// An error that occurs when the recursion limit is exceeded.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("recursion limit exceeded: depth {}, maximum {}", .0.depth(), .0.limitation())]
pub struct RecursionLimitExceeded(RecursionTracker);

impl RecursionLimitExceeded {
  /// Returns the recursion tracker that caused the error.
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.0.depth()
  }

  /// Returns the maximum number of recursion limitation.
  #[inline(always)]
  pub const fn limitation(&self) -> usize {
    self.0.limitation()
  }
}

/// A recursion tracker which tracks the depth of the recursion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecursionTracker {
  max: usize,
  current: usize,
}

impl Default for RecursionTracker {
  #[inline(always)]
  fn default() -> Self {
    Self::new()
  }
}

impl RecursionTracker {
  /// Creates a new recursion tracker.
  ///
  /// Defaults to a maximum depth of 500.
  #[inline(always)]
  pub const fn new() -> Self {
    Self {
      max: 500,
      current: 0,
    }
  }

  /// Creates a new recursion tracker with the given maximum depth.
  #[inline(always)]
  pub const fn with_limitation(max: usize) -> Self {
    Self { max, current: 0 }
  }

  /// Returns the current depth of the recursion.
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.current
  }

  /// Returns the maximum depth of the recursion.
  #[inline(always)]
  pub const fn limitation(&self) -> usize {
    self.max
  }

  /// Increase the current depth of the recursion.
  #[inline(always)]
  pub const fn increase(&mut self) {
    self.current += 1;
  }

  /// Decrease the current depth of the recursion.
  #[inline(always)]
  pub const fn decrease(&mut self) {
    self.current = self.current.saturating_sub(1);
  }
}

impl super::State for RecursionTracker {
  type Error = RecursionLimitExceeded;

  #[inline(always)]
  fn increase_recursion(&mut self) {
    self.increase();
  }

  #[inline(always)]
  fn decrease_recursion(&mut self) {
    self.decrease();
  }

  #[inline(always)]
  fn increase_token(&mut self) {}

  #[inline(always)]
  fn increase_column_number(&mut self, _: usize) {}

  #[inline(always)]
  fn increase_line_number(&mut self, _: usize) {}

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    if self.depth() > self.limitation() {
      Err(RecursionLimitExceeded(*self))
    } else {
      Ok(())
    }
  }
}
