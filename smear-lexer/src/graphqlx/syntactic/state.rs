use tokora::state::{RecursionLimitExceeded, State};

use crate::limits::SyntacticLimits;

/// The state GraphQLx's syntactic lexer runs under: its [`Lexer::State`](tokora::Lexer::State).
///
/// It holds the lexer's [`SyntacticLimits`], the nesting ceiling and the count of open brackets
/// checked against it. [`Default`] holds the limits' default, which is what
/// `Parser::with_parser(…).parse_str(src)` seeds a parse with. A caller who wants other limits
/// builds the state from them, with [`new`](Self::new) or [`From`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SyntacticState {
  limits: SyntacticLimits,
}

// tokora copies the state into every token it caches, so a word added here is a word per cached
// token. The state is its limits and nothing more.
const _: () = assert!(
  core::mem::size_of::<SyntacticState>() == core::mem::size_of::<SyntacticLimits>(),
  "SyntacticState must stay exactly its SyntacticLimits: every cached GraphQLx token carries one."
);

impl SyntacticState {
  /// A state that holds `limits`.
  #[inline(always)]
  pub const fn new(limits: SyntacticLimits) -> Self {
    Self { limits }
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
