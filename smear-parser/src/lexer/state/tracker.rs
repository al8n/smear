use super::{Position, RecursionLimitExceeded, RecursionTracker, TokenLimitExceeded, TokenTracker};

/// The limit exceeded error.
#[derive(
  Debug,
  Clone,
  Copy,
  PartialEq,
  Eq,
  thiserror::Error,
  derive_more::IsVariant,
  derive_more::Unwrap,
  derive_more::TryUnwrap,
)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum LimitExceeded {
  /// The token limit has been exceeded.
  #[error(transparent)]
  Token(#[from] TokenLimitExceeded),
  /// The recursion limit has been exceeded.
  #[error(transparent)]
  Recursion(#[from] RecursionLimitExceeded),
}

/// Trackers for tracking number of tokens, recursion depth and the position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tracker {
  token_tracker: TokenTracker,
  recursion_tracker: RecursionTracker,
  position: Position,
}

impl Default for Tracker {
  #[inline(always)]
  fn default() -> Self {
    Self::new()
  }
}

impl Tracker {
  /// Creates a new tracker.
  #[inline(always)]
  pub const fn new() -> Self {
    Self::with_trackers(TokenTracker::new(), RecursionTracker::new())
  }

  /// Creates a new tracker with the given token tracker
  #[inline(always)]
  pub const fn with_token_tracker(token_tracker: TokenTracker) -> Self {
    Self::with_trackers(token_tracker, RecursionTracker::new())
  }

  /// Creates a new tracker with the given recursion tracker.
  #[inline(always)]
  pub const fn with_recursion_tracker(recursion_tracker: RecursionTracker) -> Self {
    Self::with_trackers(TokenTracker::new(), recursion_tracker)
  }

  /// Creates a new tracker with the given token and recursion trackers.
  #[inline(always)]
  pub const fn with_trackers(
    token_tracker: TokenTracker,
    recursion_tracker: RecursionTracker,
  ) -> Self {
    Self {
      token_tracker,
      recursion_tracker,
      position: Position::new(),
    }
  }

  /// Returns the token tracker.
  #[inline(always)]
  pub const fn token(&self) -> &TokenTracker {
    &self.token_tracker
  }

  /// Returns the token tracker
  #[inline(always)]
  pub const fn token_mut(&mut self) -> &mut TokenTracker {
    &mut self.token_tracker
  }

  /// Returns the recursion tracker.
  #[inline(always)]
  pub const fn recursion(&self) -> &RecursionTracker {
    &self.recursion_tracker
  }

  /// Returns the recursion tracker.
  #[inline(always)]
  pub const fn recursion_mut(&mut self) -> &mut RecursionTracker {
    &mut self.recursion_tracker
  }

  /// Returns the position.
  #[inline(always)]
  pub const fn position(&self) -> &Position {
    &self.position
  }

  /// Returns the position.
  #[inline(always)]
  pub const fn position_mut(&mut self) -> &mut Position {
    &mut self.position
  }
}

impl super::State for Tracker {
  type Error = LimitExceeded;

  #[inline(always)]
  fn increase_token(&mut self) {
    self.token_mut().increase();
  }

  #[inline(always)]
  fn increase_column_number(&mut self, num: usize) {
    self.position_mut().increase_column_number(num);
  }

  #[inline(always)]
  fn increase_line_number(&mut self, num: usize) {
    self.position_mut().increase_line_number(num);
  }

  #[inline(always)]
  fn increase_recursion(&mut self) {
    self.recursion_mut().increase();
  }

  #[inline(always)]
  fn decrease_recursion(&mut self) {
    self.recursion_mut().decrease();
  }

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    self
      .recursion_tracker
      .check()
      .map_err(LimitExceeded::from)?;
    self.token_tracker.check().map_err(LimitExceeded::from)?;
    Ok(())
  }

  #[inline(always)]
  fn tab_width(&self) -> usize {
    self.position.tab_width()
  }
}
