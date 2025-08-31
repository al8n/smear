use super::{Position, RecursionLimitExceeded, RecursionLimiter, TokenLimitExceeded, TokenLimiter};

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
  token_tracker: TokenLimiter,
  recursion_tracker: RecursionLimiter,
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
    Self::with_trackers(TokenLimiter::new(), RecursionLimiter::new())
  }

  /// Creates a new tracker with the given token tracker
  #[inline(always)]
  pub const fn with_token_tracker(token_tracker: TokenLimiter) -> Self {
    Self::with_trackers(token_tracker, RecursionLimiter::new())
  }

  /// Creates a new tracker with the given recursion tracker.
  #[inline(always)]
  pub const fn with_recursion_tracker(recursion_tracker: RecursionLimiter) -> Self {
    Self::with_trackers(TokenLimiter::new(), recursion_tracker)
  }

  /// Creates a new tracker with the given token and recursion trackers.
  #[inline(always)]
  pub const fn with_trackers(
    token_tracker: TokenLimiter,
    recursion_tracker: RecursionLimiter,
  ) -> Self {
    Self {
      token_tracker,
      recursion_tracker,
      position: Position::new(),
    }
  }

  /// Returns the token tracker.
  #[inline(always)]
  pub const fn token(&self) -> &TokenLimiter {
    &self.token_tracker
  }

  /// Returns the token tracker
  #[inline(always)]
  pub const fn token_mut(&mut self) -> &mut TokenLimiter {
    &mut self.token_tracker
  }

  /// Returns the recursion tracker.
  #[inline(always)]
  pub const fn recursion(&self) -> &RecursionLimiter {
    &self.recursion_tracker
  }

  /// Returns the recursion tracker.
  #[inline(always)]
  pub const fn recursion_mut(&mut self) -> &mut RecursionLimiter {
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
