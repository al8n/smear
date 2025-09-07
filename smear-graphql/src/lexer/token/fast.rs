use smear_parser::lexer::state::{RecursionLimiter, TokenLimiter};

/// A token based on `str` input.
pub mod str;

#[derive(Default, Clone, Copy, Eq, PartialEq)]
pub struct TokenOptions {
  token_limiter: TokenLimiter,
  recursion_limiter: RecursionLimiter,
  fast_error: bool,
}

impl TokenOptions {
  /// Create a new `TokenOptions` with default values.
  #[inline]
  pub const fn new() -> Self {
    Self {
      token_limiter: TokenLimiter::new(),
      recursion_limiter: RecursionLimiter::new(),
      fast_error: false,
    }
  }

  /// Set the token limiter.
  #[inline]
  pub const fn with_token_limiter(mut self, token_limiter: TokenLimiter) -> Self {
    self.token_limiter = token_limiter;
    self
  }

  /// Set the recursion limiter.
  #[inline]
  pub const fn with_recursion_limiter(mut self, recursion_limiter: RecursionLimiter) -> Self {
    self.recursion_limiter = recursion_limiter;
    self
  }

  /// Enable or disable fast error mode.
  ///
  /// When fast error mode is enabled, the lexer will not attempt to give a detailed
  /// error on invalid tokens.
  #[inline]
  pub const fn with_fast_error(mut self, fast_error: bool) -> Self {
    self.fast_error = fast_error;
    self
  }

  /// Get the token limiter.
  #[inline]
  pub const fn token_limiter(&self) -> &TokenLimiter {
    &self.token_limiter
  }

  /// Get the recursion limiter.
  #[inline]
  pub const fn recursion_limiter(&self) -> &RecursionLimiter {
    &self.recursion_limiter
  }

  /// Check if fast error mode is enabled.
  #[inline]
  pub const fn fast_error(&self) -> bool {
    self.fast_error
  }
}
