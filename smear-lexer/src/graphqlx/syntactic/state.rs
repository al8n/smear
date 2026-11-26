use core::fmt;

use tokora::state::{RecursionLimitExceeded, RecursionLimiter, State};

use crate::limits::SyntacticLimits;

/// The state GraphQLx's syntactic lexer runs under: its [`Lexer::State`](tokora::Lexer::State).
///
/// It holds the lexer's nesting ceiling and the count of open brackets checked against it, which
/// [`limits`](Self::limits) answers as a [`SyntacticLimits`], and whether the lexer is inside a
/// `const { … }` block, with how many `<` are open there. [`Default`] holds the limits' default,
/// outside a block, which is what `Parser::with_parser(…).parse_str(src)` seeds a parse with. A
/// caller who wants other limits builds the state from them, with [`new`](Self::new) or [`From`].
///
/// The ceiling is held in 32 bits, so a state refuses past 4,294,967,295 open brackets at the
/// most. A state built from a higher ceiling, [`SyntacticLimits::unlimited`]'s among them, holds
/// 4,294,967,295, and [`limits`](Self::limits) reads that back.
///
/// Inside a block a comma is the token `Separator` while no `<` is open, `-` is always `Minus`
/// and no number carries a sign, `>>` is the token `Shr` where no `<` is open, and a name touching
/// a quote is a prefixed string or a reserved prefix. The lexer never enters or leaves a block by
/// what it reads: its parser does, through [`enter_const_block`](Self::enter_const_block) and
/// [`leave_const_block`](Self::leave_const_block).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntacticState {
  /// The nesting ceiling in the low half, and the block's word in the high half: zero outside a
  /// block, and inside one, one more than the count of open `<`.
  packed: u64,
  /// The count of open brackets.
  depth: usize,
}

// tokora copies the state into every token it caches, so a word added here is a word per cached
// token. The state is its packed word and its depth: two words on a 64-bit target. A 32-bit target
// that aligns a `u64` to four bytes holds it in 12, and one that aligns it to eight pads it to 16.
const _: () = assert!(
  core::mem::size_of::<SyntacticState>() == core::mem::size_of::<(u64, usize)>(),
  "SyntacticState must stay its packed word and its depth: every cached GraphQLx token carries \
   one."
);

#[cfg(target_pointer_width = "64")]
const _: () = assert!(
  core::mem::size_of::<SyntacticState>() == 2 * core::mem::size_of::<usize>(),
  "SyntacticState must stay two words on a 64-bit target: every cached GraphQLx token carries one."
);

/// The packed word's low half, where the ceiling is. As a number it is the highest ceiling a state
/// holds: 4,294,967,295 levels.
const LOW_HALF: u64 = u32::MAX as u64;

/// The shift to the packed word's high half, where the block's word is.
const HIGH_HALF: u32 = u32::BITS;

impl SyntacticState {
  /// A state that holds `limits`, outside a block. A ceiling above 4,294,967,295 levels,
  /// [`SyntacticLimits::unlimited`]'s among them, is held as 4,294,967,295.
  #[inline(always)]
  pub const fn new(limits: SyntacticLimits) -> Self {
    let ceiling = limits.max_nesting_depth() as u64;
    Self {
      packed: if ceiling < LOW_HALF {
        ceiling
      } else {
        LOW_HALF
      },
      depth: limits.depth(),
    }
  }

  /// The nesting ceiling: the packed word's low half.
  #[inline(always)]
  const fn ceiling(&self) -> usize {
    (self.packed & LOW_HALF) as usize
  }

  /// The block's word: the packed word's high half.
  #[inline(always)]
  const fn block(&self) -> u32 {
    (self.packed >> HIGH_HALF) as u32
  }

  /// Writes the block's word, and leaves the ceiling as it is.
  #[inline(always)]
  const fn set_block(&mut self, block: u32) {
    self.packed = (self.packed & LOW_HALF) | ((block as u64) << HIGH_HALF);
  }

  /// Whether the lexer is inside a `const { … }` block, where a comma is a token, `-` is always an
  /// operator, `>>` is a shift where no `<` is open, and a name touching a quote is a prefix.
  #[inline(always)]
  pub const fn in_const_block(&self) -> bool {
    // The high half against zero, and no other bit of the word: where this load feeds nothing
    // else, the backend narrows it to that half's 32 bits and folds it into the compare, one
    // compare with memory.
    self.block() != 0
  }

  /// How many `<` are open in the block: zero outside one. Nothing refuses on it, it stays at
  /// 4,294,967,294 once it reaches it, and a `{` or a `}` sets it to zero.
  #[inline(always)]
  pub const fn open_angles(&self) -> usize {
    self.block().saturating_sub(1) as usize
  }

  /// Enters a block, with no `<` open. A parser calls it right after the block's `{`. It touches
  /// none of the limits.
  #[inline(always)]
  pub const fn enter_const_block(&mut self) {
    self.set_block(1);
  }

  /// Leaves the block. A parser calls it right after the block's `}`. It touches none of the
  /// limits.
  #[inline(always)]
  pub const fn leave_const_block(&mut self) {
    self.set_block(0);
  }

  /// Counts a `<` that opens, inside a block; nothing outside one.
  #[inline(always)]
  pub(crate) const fn open_angle(&mut self) {
    let block = self.block();
    if block != 0 {
      self.set_block(block.saturating_add(1));
    }
  }

  /// Uncounts a `>` that closes a `<` the block opened; nothing when none is open.
  #[inline(always)]
  pub(crate) const fn close_angle(&mut self) {
    let block = self.block();
    if block > 1 {
      self.set_block(block - 1);
    }
  }

  /// A brace's clear: no `<` is open after it. Outside a block this is the identity, so no brace
  /// enters one.
  #[inline(always)]
  pub(crate) const fn clear_angles(&mut self) {
    if self.block() > 1 {
      self.set_block(1);
    }
  }

  /// Opens one level of brackets, saturating as [`SyntacticLimits::increase`] does. It refuses
  /// nothing: the [`check`](Self::check) after it does.
  #[inline(always)]
  pub(crate) const fn increase_depth(&mut self) {
    self.depth = self.depth.saturating_add(1);
  }

  /// Closes one level of brackets, saturating at zero as [`SyntacticLimits::decrease`] does.
  #[inline(always)]
  pub(crate) const fn decrease_depth(&mut self) {
    self.depth = self.depth.saturating_sub(1);
  }

  /// Sets the count of open brackets: a delegated number's lexer gives the depth back through it.
  #[inline(always)]
  pub(crate) const fn set_depth(&mut self, depth: usize) {
    self.depth = depth;
  }

  /// tokora's own tracker at this state's ceiling and depth. Only a refusal and the limits handed
  /// out build one.
  #[inline(always)]
  const fn limiter(&self) -> RecursionLimiter {
    RecursionLimiter::with_limitation(self.ceiling()).with_depth(self.depth)
  }

  /// Whether the brackets open are within the nesting ceiling: the depth against the ceiling's
  /// half of the word. A refusal is the tracker's own, from [`RecursionLimiter::check`] at this
  /// ceiling and depth.
  #[inline(always)]
  pub(crate) const fn check(&self) -> Result<(), RecursionLimitExceeded> {
    if self.depth <= self.ceiling() {
      Ok(())
    } else {
      self.limiter().check()
    }
  }

  /// The limits this state holds, with the count of open brackets the lexer has stepped. The
  /// ceiling is the one the state holds: 4,294,967,295 at the most.
  #[inline(always)]
  pub fn limits(&self) -> SyntacticLimits {
    SyntacticLimits::from(self.limiter())
  }
}

impl Default for SyntacticState {
  /// The limits' default, outside a block.
  #[inline(always)]
  fn default() -> Self {
    Self::new(SyntacticLimits::default())
  }
}

// By hand, so that a state prints as the limits it holds and the block's word, not as the packed
// word.
impl fmt::Debug for SyntacticState {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("SyntacticState")
      .field("limits", &self.limits())
      .field("block", &self.block())
      .finish()
  }
}

impl From<SyntacticLimits> for SyntacticState {
  #[inline(always)]
  fn from(limits: SyntacticLimits) -> Self {
    Self::new(limits)
  }
}

impl State for SyntacticState {
  type Error = RecursionLimitExceeded;

  /// Whether the brackets open are within the nesting ceiling. A refusal is the one tokora's own
  /// tracker makes at this state's ceiling and depth.
  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Self::check(self)
  }
}

#[cfg(test)]
mod tests;
