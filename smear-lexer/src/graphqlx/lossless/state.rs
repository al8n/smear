use tokora::state::{LimitExceeded, State, TokenLimiter};

use crate::{handlers::LosslessExtras, limits::LosslessLimits};

/// The state GraphQLx's lossless lexer runs under: the Logos `Extras` of its grammar, and its
/// [`Lexer::State`](tokora::Lexer::State).
///
/// It holds the lexer's [`LosslessLimits`] and answers their eight reads, each with the limits'
/// own signature. [`Default`] holds the limits' default, which is what a lossless parse handed no
/// limits is seeded with. Every lossless door takes [`LosslessLimits`] and builds the state from
/// them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LosslessState {
  limits: LosslessLimits,
}

// tokora copies the state into every token it caches, so a word added here is a word per cached
// token. The state is its limits and nothing more.
const _: () = assert!(
  core::mem::size_of::<LosslessState>() == core::mem::size_of::<LosslessLimits>(),
  "LosslessState must stay exactly its LosslessLimits: every cached lossless token carries one."
);

impl LosslessState {
  /// A state that holds `limits`.
  #[inline(always)]
  pub const fn new(limits: LosslessLimits) -> Self {
    Self { limits }
  }

  /// The limits this state holds, with the counts the lexer has stepped.
  #[inline(always)]
  pub const fn limits(&self) -> &LosslessLimits {
    &self.limits
  }

  /// The limits' [`brace_depth`](LosslessLimits::brace_depth).
  #[inline(always)]
  pub const fn brace_depth(&self) -> usize {
    self.limits.brace_depth()
  }

  /// The limits' [`depth`](LosslessLimits::depth).
  #[inline(always)]
  pub const fn depth(&self) -> usize {
    self.limits.depth()
  }

  /// The limits' [`token`](LosslessLimits::token).
  #[inline(always)]
  pub const fn token(&self) -> &TokenLimiter {
    self.limits.token()
  }

  /// The limits' [`max_tokens`](LosslessLimits::max_tokens).
  #[inline(always)]
  pub const fn max_tokens(&self) -> usize {
    self.limits.max_tokens()
  }

  /// The limits' [`max_nesting_depth`](LosslessLimits::max_nesting_depth).
  #[inline(always)]
  pub const fn max_nesting_depth(&self) -> usize {
    self.limits.max_nesting_depth()
  }

  /// The limits' [`parse_ceiling`](LosslessLimits::parse_ceiling).
  #[inline(always)]
  pub const fn parse_ceiling(&self) -> usize {
    self.limits.parse_ceiling()
  }

  /// The limits' [`max_produce_events`](LosslessLimits::max_produce_events).
  #[inline(always)]
  pub const fn max_produce_events(&self) -> usize {
    self.limits.max_produce_events()
  }

  /// The limits' [`max_diagnostics`](LosslessLimits::max_diagnostics).
  #[inline(always)]
  pub const fn max_diagnostics(&self) -> usize {
    self.limits.max_diagnostics()
  }
}

impl From<LosslessLimits> for LosslessState {
  #[inline(always)]
  fn from(limits: LosslessLimits) -> Self {
    Self::new(limits)
  }
}

impl State for LosslessState {
  type Error = LimitExceeded;

  /// The limits' own check: the nesting ceiling, and then the token ceiling.
  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    self.limits.check()
  }
}

impl LosslessExtras for LosslessState {
  #[inline(always)]
  fn lossless_limits(&self) -> &LosslessLimits {
    &self.limits
  }

  #[inline(always)]
  fn lossless_limits_mut(&mut self) -> &mut LosslessLimits {
    &mut self.limits
  }
}
