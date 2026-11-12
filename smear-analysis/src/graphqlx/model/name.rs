//! Names and text: the model's two arenas, read by symbol and by run.

use tokora::diagnostic::Location;

use crate::graphqlx::id::Sym;

use super::{Model, table::DescriptionRow};

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

/// A description: its text, cooked, and where it is written.
///
/// An item with no description answers `None`, and one with an empty description answers a
/// `Description` whose text is `""`: the two are kept apart.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Description<'a> {
  text: &'a str,
  location: Location,
}

impl<'a> Description<'a> {
  /// The description `row` holds in `model`, if any.
  pub(super) fn of(model: &'a Model, row: Option<DescriptionRow>) -> Option<Self> {
    let row = row?;
    Some(Self {
      text: row.text.text(&model.text),
      location: row.location,
    })
  }

  /// Its text, cooked: escapes decoded, and a block string's indentation removed.
  #[inline]
  pub const fn text(&self) -> &'a str {
    self.text
  }

  /// Where it is written.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }
}
