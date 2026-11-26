use tokora::state::{RecursionLimitExceeded, State};

use crate::limits::SyntacticLimits;

/// The state GraphQLx's syntactic lexer runs under: its [`Lexer::State`](tokora::Lexer::State).
///
/// It holds the lexer's [`SyntacticLimits`], the nesting ceiling and the count of open brackets
/// checked against it, and whether the lexer is inside a `const { … }` block, with how many `<`
/// are open there. [`Default`] holds the limits' default, outside a block, which is what
/// `Parser::with_parser(…).parse_str(src)` seeds a parse with. A caller who wants other limits
/// builds the state from them, with [`new`](Self::new) or [`From`].
///
/// Inside a block a comma is the token `Separator` while no `<` is open, `-` is always `Minus`
/// and no number carries a sign, `>>` is the token `Shr` where no `<` is open, and a name touching
/// a quote is a prefixed string or a reserved prefix. The lexer never enters or leaves a block by
/// what it reads: its parser does, through [`enter_const_block`](Self::enter_const_block) and
/// [`leave_const_block`](Self::leave_const_block).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SyntacticState {
  limits: SyntacticLimits,
  /// Zero outside a block, and inside one, one more than the count of open `<`.
  block: usize,
}

// tokora copies the state into every token it caches, so a word added here is a word per cached
// token. The state is its limits and the one word of a block.
const _: () = assert!(
  core::mem::size_of::<SyntacticState>()
    == core::mem::size_of::<SyntacticLimits>() + core::mem::size_of::<usize>(),
  "SyntacticState must stay its SyntacticLimits and one word: every cached GraphQLx token carries \
   one."
);

impl SyntacticState {
  /// A state that holds `limits`, outside a block.
  #[inline(always)]
  pub const fn new(limits: SyntacticLimits) -> Self {
    Self { limits, block: 0 }
  }

  /// Whether the lexer is inside a `const { … }` block, where a comma is a token, `-` is always an
  /// operator, `>>` is a shift where no `<` is open, and a name touching a quote is a prefix.
  #[inline(always)]
  pub const fn in_const_block(&self) -> bool {
    // PROBE F.Q: never in a block, so the loop's tests of it fold away. Measurement only.
    false
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

  /// The limits this state holds, with the count of open brackets the lexer has stepped.
  #[inline(always)]
  pub const fn limits(&self) -> &SyntacticLimits {
    &self.limits
  }

  /// The limits, for the lexer to step.
  #[inline(always)]
  pub(crate) const fn limits_mut(&mut self) -> &mut SyntacticLimits {
    &mut self.limits
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

  /// The limits' own check: whether the brackets open are within the nesting ceiling.
  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    self.limits.check()
  }
}
