/// The position of the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
  line: usize,
  column: usize,
  tab_width: usize,
}

impl Default for Position {
  #[inline(always)]
  fn default() -> Self {
    Self::new()
  }
}

impl Position {
  /// Creates a new position.
  #[inline(always)]
  pub const fn new() -> Self {
    Self::with_tab_width(4)
  }

  /// Creates a new position with the tab width
  #[inline(always)]
  pub const fn with_tab_width(tab_width: usize) -> Self {
    Self { line: 1, column: 1, tab_width }
  }

  /// Returns the line number (1-based).
  #[inline(always)]
  pub const fn line(&self) -> usize {
    self.line
  }

  /// Increase the line number
  #[inline(always)]
  pub const fn increase_line_number(&mut self, lines: usize) {
    self.line += lines;
    // when increasing the line, the column should reset
    self.column = 1;
  }

  /// Returns the column number (1-based).
  #[inline(always)]
  pub const fn column(&self) -> usize {
    self.column
  }

  /// Increase the column number
  #[inline(always)]
  pub const fn increase_column_number(&mut self, cols: usize) {
    self.column += cols;
  }
}

impl super::State for Position {
  type Error = core::convert::Infallible;

  #[inline(always)]
  fn increase_column_number(&mut self, num: usize) {
    self.increase_column_number(num);
  }

  #[inline(always)]
  fn increase_line_number(&mut self, lines: usize) {
    self.increase_line_number(lines);
  }

  #[inline(always)]
  fn increase_recursion(&mut self) {}

  #[inline(always)]
  fn decrease_recursion(&mut self) {}

  #[inline(always)]
  fn increase_token(&mut self) {}

  #[inline(always)]
  fn check(&self) -> Result<(), Self::Error> {
    Ok(())
  }

  #[inline(always)]
  fn tab_width(&self) -> usize {
    self.tab_width
  }
}
