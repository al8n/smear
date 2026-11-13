//! What the phases after coercion write, and what the hand-built models of the tests write in one
//! step: each definition's externs; the tiers of definitions and types; the values coercion does
//! not make yet, type values; a value written whole, each over coercion's own writers; and marks.
//!
//! GATED ON HAVING A CALLER. The phases still to come are this module's callers, and until each
//! lands the test-support door is its only caller: a method that no shipped build calls is dead
//! code. A phase moves the methods it calls out of this module, as the module tree, item
//! collection, lowering, instantiation and coercion moved theirs.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, Sym, TyId, ValueId},
  model::{
    table::{ExternRow, VALUE_TYPE},
    ty::PathRoot,
    value::LiteralKind,
  },
  package::Tier,
};

use super::{ModelBuilder, grow, push_text};

impl ModelBuilder {
  /// Adds an `@extern` to `def`, after those it has: its `lang` and `type`, and its effective
  /// tier.
  pub fn push_extern(&mut self, def: DefId, lang: &str, ty: &str, tier: Tier) {
    let lang = push_text(&mut self.model.text, lang);
    let ty = push_text(&mut self.model.text, ty);
    let row = &mut self.model.defs[def.get() as usize];
    grow(
      &mut self.model.externs,
      &mut row.externs,
      ExternRow { lang, ty, tier },
    );
  }

  /// Gives `ty` the tier it requires.
  pub fn set_ty_tier(&mut self, ty: TyId, tier: Tier) {
    self.model.tys[ty.get() as usize].tier = tier;
  }

  /// Makes `value` an integer: negative when `negative`, of absolute value `magnitude`,
  /// little-endian, written as `text`. Trailing zero bytes of `magnitude` are not kept, so zero is
  /// no bytes, and zero is never negative.
  pub fn set_int(&mut self, value: ValueId, negative: bool, magnitude: &[u8], text: &str) {
    let text = self.keep_text(text);
    let int = self.push_int(negative, magnitude, text);
    self.give_int(value, int);
  }

  /// Makes `value` the float `float`, written as `text`.
  pub fn set_float(&mut self, value: ValueId, float: f64, text: &str) {
    let text = self.keep_text(text);
    let row = self.push_float(float, text);
    self.give_float(value, row);
  }

  /// Makes `value` the string `text`, cooked.
  pub fn set_string(&mut self, value: ValueId, text: &str) {
    let text = self.keep_text(text);
    self.give_string(value, text);
  }

  /// Makes `value` a literal of `kind`, kept as written: `text`.
  pub fn set_literal(&mut self, value: ValueId, kind: LiteralKind, text: &str) {
    let text = self.keep_text(text);
    let literal = self.push_literal(kind, text);
    self.give_literal(value, literal);
  }

  /// Records the path an enum or type value writes: from `root`, through `segments`.
  pub fn set_value_path(&mut self, value: ValueId, root: PathRoot, segments: &[Sym]) {
    let segments = self.keep_segments(segments);
    self.give_path(value, root, segments);
  }

  /// Makes `value` the type value naming `def`.
  pub fn set_type_value(&mut self, value: ValueId, def: DefId) {
    let row = self.value_row(value);
    row.shape = VALUE_TYPE;
    row.def = def;
  }

  /// Marks, as an error reported at `at` does, every contribution and member whose extent holds
  /// `at`. Marks are applied when the model is finished.
  pub fn mark(&mut self, at: Location) {
    self.marks.push(at);
  }
}
