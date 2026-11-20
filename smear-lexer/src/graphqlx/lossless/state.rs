use tokora::state::{LimitExceeded, State, TokenLimiter};

use crate::{handlers::LosslessExtras, limits::LosslessLimits};

/// The state GraphQLx's lossless lexer runs under: the Logos `Extras` of its grammar, and its
/// [`Lexer::State`](tokora::Lexer::State).
///
/// It holds the lexer's [`LosslessLimits`] and answers their eight reads, each with the limits'
/// own signature, and whether the lexer is inside a `const { … }` block, with how many `<` are
/// open there. [`Default`] holds the limits' default, outside a block, which is what a lossless
/// parse handed no limits is seeded with. Every lossless door takes [`LosslessLimits`] and builds
/// the state from them.
///
/// Inside a block a comma is the token `Separator` while no `<` is open, `-` is always `Minus`
/// and no number carries a sign, `>>` is the token `Shr` where no `<` is open, and a name touching
/// a quote is a prefixed string or a reserved prefix. The lexer never enters or leaves a block by
/// what it reads: its parser does, through [`enter_const_block`](Self::enter_const_block) and
/// [`leave_const_block`](Self::leave_const_block).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LosslessState {
  limits: LosslessLimits,
  /// Zero outside a block, and inside one, one more than the count of open `<`.
  block: usize,
}

// tokora copies the state into every token it caches, so a word added here is a word per cached
// token. The state is its limits and the one word of a block.
const _: () = assert!(
  core::mem::size_of::<LosslessState>()
    == core::mem::size_of::<LosslessLimits>() + core::mem::size_of::<usize>(),
  "LosslessState must stay its LosslessLimits and one word: every cached lossless token carries \
   one."
);

impl LosslessState {
  /// A state that holds `limits`, outside a block.
  #[inline(always)]
  pub const fn new(limits: LosslessLimits) -> Self {
    Self { limits, block: 0 }
  }

  /// Whether the lexer is inside a `const { … }` block, where a comma is a token, `-` is always an
  /// operator, `>>` is a shift where no `<` is open, and a name touching a quote is a prefix.
  #[inline(always)]
  pub const fn in_const_block(&self) -> bool {
    self.block != 0
  }

  /// How many `<` are open in the block: zero outside one. Nothing refuses on it, it stays at its
  /// ceiling once it reaches it, and a `{` or a `}` sets it to zero.
  #[inline(always)]
  pub const fn open_angles(&self) -> usize {
    self.block.saturating_sub(1)
  }

  /// Enters a block, with no `<` open. A parser calls it right after the block's `{`. It touches
  /// none of the limits.
  #[inline(always)]
  pub const fn enter_const_block(&mut self) {
    self.block = 1;
  }

  /// Leaves the block. A parser calls it right after the block's `}`. It touches none of the
  /// limits.
  #[inline(always)]
  pub const fn leave_const_block(&mut self) {
    self.block = 0;
  }

  /// Counts a `<` that opens, inside a block; nothing outside one.
  #[inline(always)]
  pub(crate) const fn open_angle(&mut self) {
    if self.block != 0 {
      self.block = self.block.saturating_add(1);
    }
  }

  /// Uncounts a `>` that closes a `<` the block opened; nothing when none is open.
  #[inline(always)]
  pub(crate) const fn close_angle(&mut self) {
    if self.block > 1 {
      self.block -= 1;
    }
  }

  /// A brace's clear: no `<` is open after it. Outside a block this is the identity, so no brace
  /// enters one.
  #[inline(always)]
  pub(crate) const fn clear_angles(&mut self) {
    if self.block > 1 {
      self.block = 1;
    }
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
