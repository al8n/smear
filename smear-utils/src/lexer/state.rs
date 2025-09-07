pub use position::*;
pub use recursion_tracker::*;
pub use token_tracker::*;
pub use tracker::*;

mod position;
mod recursion_tracker;
mod token_tracker;
mod tracker;

/// The state of the lexer.
pub trait State: Copy + core::fmt::Debug {
  /// The error type of the state
  type Error: core::error::Error + Clone;

  /// Increases the token count.
  fn increase_token(&mut self);

  /// Increases the column number.
  fn increase_column_number(&mut self, num: usize);

  /// Increases the line number.
  ///
  /// **Notes:** When the implementors implement this fn, do not forget to reset the column number to the beginning.
  fn increase_line_number(&mut self, lines: usize);

  /// Increases the recursion depth
  fn increase_recursion(&mut self);

  /// Decreases the recursion depth.
  fn decrease_recursion(&mut self);

  /// Checks the state, returning an error if the state is invalid.
  ///
  /// This function will be automatically invoked when yielding every token,
  /// if an error is returned than a state error token will be created by
  /// [`Token::from_state_error`](super::Token::from_state_error)
  fn check(&self) -> Result<(), Self::Error>;

  /// How many characters width should tab characters be?
  ///
  /// If unspecified, this defaults to `4`.
  #[inline(always)]
  fn tab_width(&self) -> usize {
    4
  }
}

impl State for () {
  type Error = core::convert::Infallible;

  #[inline(always)]
  fn increase_token(&mut self) {}

  #[inline(always)]
  fn increase_column_number(&mut self, _: usize) {}

  #[inline(always)]
  fn increase_line_number(&mut self, _: usize) {}

  #[inline(always)]
  fn increase_recursion(&mut self) {}

  #[inline(always)]
  fn decrease_recursion(&mut self) {}

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Ok(())
  }
}
