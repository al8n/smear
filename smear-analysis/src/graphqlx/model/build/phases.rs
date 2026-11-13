//! What the phases after coercion write, and what the hand-built models of the tests write in one
//! step: each definition's externs; the tiers of definitions and types; the values coercion does
//! not make yet, sets, maps, input objects, enum values and type values; a value written whole,
//! each over coercion's own writers; and marks.
//!
//! GATED ON HAVING A CALLER. The phases still to come are this module's callers, and until each
//! lands the test-support door is its only caller: a method that no shipped build calls is dead
//! code. A phase moves the methods it calls out of this module, as the module tree, item
//! collection, lowering, instantiation and coercion moved theirs.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, MemberId, Sym, TyId, ValueId, dense},
  model::{
    table::{ExternRow, Run, VALUE_ENUM, VALUE_MAP, VALUE_OBJECT, VALUE_SET, VALUE_TYPE},
    ty::PathRoot,
    value::{LiteralKind, ObjectEntry},
  },
  package::Tier,
};

use super::{ModelBuilder, append, grow, push_text};

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

  /// Makes `value` the enum value `member` of the enum `def`.
  pub fn set_enum(&mut self, value: ValueId, def: DefId, member: MemberId) {
    let row = self.value_row(value);
    row.shape = VALUE_ENUM;
    row.def = def;
    row.member = member;
  }

  /// Makes `value` the type value naming `def`.
  pub fn set_type_value(&mut self, value: ValueId, def: DefId) {
    let row = self.value_row(value);
    row.shape = VALUE_TYPE;
    row.def = def;
  }

  /// Makes `value` the set of `elements`, each appended after it.
  pub fn set_set(&mut self, value: ValueId, elements: &[ValueId]) {
    self.set_items(value, VALUE_SET, elements);
  }

  /// Makes `value` the map of `entries`, each a key and a value, each appended after it.
  pub fn set_map(&mut self, value: ValueId, entries: &[(ValueId, ValueId)]) {
    debug_assert!(
      entries
        .iter()
        .all(|&(key, item): &(ValueId, ValueId)| key > value && item > value),
      "what a value holds is appended after it"
    );
    let run = append(&mut self.model.map_entries, entries);
    let row = self.value_row(value);
    row.shape = VALUE_MAP;
    row.run = run;
  }

  /// Makes `value` the input object of `entries`: each the input field it sets, where the field's
  /// name is written (`None` for a field a default filled), and the field's value, appended after
  /// `value`.
  pub fn set_object(&mut self, value: ValueId, entries: &[(MemberId, Option<Location>, ValueId)]) {
    let start = dense(self.model.object_entries.len());
    for &(field, name, item) in entries {
      debug_assert!(item > value, "what a value holds is appended after it");
      self.model.object_entries.push(ObjectEntry {
        field,
        name,
        value: item,
      });
    }
    let row = self.value_row(value);
    row.shape = VALUE_OBJECT;
    row.run = Run {
      start,
      len: dense(entries.len()),
    };
  }

  /// Marks, as an error reported at `at` does, every contribution and member whose extent holds
  /// `at`. Marks are applied when the model is finished.
  pub fn mark(&mut self, at: Location) {
    self.marks.push(at);
  }
}
