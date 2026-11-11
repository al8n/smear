//! Names and text: the model's two arenas, read by symbol and by run.

use crate::graphqlx::id::Sym;

use super::Model;

impl Model {
  /// The name `sym` interned.
  ///
  /// # Panics
  ///
  /// If `sym` is not one of this model's names: a symbol belongs to the analysis that issued it.
  #[inline]
  pub fn name(&self, sym: Sym) -> &str {
    assert!(
      (sym.get() as usize) < self.spans.len(),
      "name {} is past the {} names of this model",
      sym.get(),
      self.spans.len()
    );
    let (start, end) = self.spans[sym.get() as usize];
    self.names.get(start as usize..end as usize).unwrap_or("")
  }
}
