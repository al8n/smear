//! What the phases after instantiation write: each definition's externs and the satisfactions;
//! the tiers of definitions and types; the shapes and types coercion gives values; and marks.
//!
//! GATED ON HAVING A CALLER. The phases still to come are this module's callers, and until each
//! lands the test-support door is its only caller: a method that no shipped build calls is dead
//! code. A phase moves the methods it calls out of this module, as the module tree, item
//! collection, lowering and instantiation moved theirs.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, MemberId, TyId, ValueId, dense},
  model::{
    table::{
      ExternRow, FloatRow, IntRow, Run, VALUE_BOOL, VALUE_ENUM, VALUE_FLOAT, VALUE_INT, VALUE_LIST,
      VALUE_MAP, VALUE_NULL, VALUE_OBJECT, VALUE_SET, VALUE_STRING, VALUE_TYPE,
    },
    value::ObjectEntry,
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

  /// Records that `ty` satisfies `interface`, declared or intrinsic. Satisfactions are added in
  /// any order, and each is kept once.
  pub fn push_satisfaction(&mut self, ty: TyId, interface: DefId) {
    self.model.satisfactions.push((ty, interface));
  }

  /// Gives `ty` the tier it requires.
  pub fn set_ty_tier(&mut self, ty: TyId, tier: Tier) {
    self.model.tys[ty.get() as usize].tier = tier;
  }

  /// Makes `value` `null`.
  pub fn set_null(&mut self, value: ValueId) {
    self.value_row(value).shape = VALUE_NULL;
  }

  /// Makes `value` the boolean `flag`.
  pub fn set_bool(&mut self, value: ValueId, flag: bool) {
    let row = self.value_row(value);
    row.shape = VALUE_BOOL;
    row.payload = u32::from(flag);
  }

  /// Makes `value` an integer: negative when `negative`, of absolute value `magnitude`,
  /// little-endian, written as `text`. Trailing zero bytes of `magnitude` are not kept, so zero is
  /// no bytes, and zero is never negative.
  pub fn set_int(&mut self, value: ValueId, negative: bool, magnitude: &[u8], text: &str) {
    let len = magnitude
      .iter()
      .rposition(|byte: &u8| *byte != 0)
      .map_or(0, |last: usize| last + 1);
    let kept = &magnitude[..len];
    let int = IntRow {
      negative: negative && !kept.is_empty(),
      magnitude: append(&mut self.model.magnitudes, kept),
      text: push_text(&mut self.model.text, text),
    };
    let payload = dense(self.model.ints.len());
    self.model.ints.push(int);
    let row = self.value_row(value);
    row.shape = VALUE_INT;
    row.payload = payload;
  }

  /// Makes `value` the float `float`, written as `text`.
  pub fn set_float(&mut self, value: ValueId, float: f64, text: &str) {
    let text = push_text(&mut self.model.text, text);
    let payload = dense(self.model.floats.len());
    self.model.floats.push(FloatRow { value: float, text });
    let row = self.value_row(value);
    row.shape = VALUE_FLOAT;
    row.payload = payload;
  }

  /// Makes `value` the string `text`, cooked.
  pub fn set_string(&mut self, value: ValueId, text: &str) {
    let run = push_text(&mut self.model.text, text);
    let row = self.value_row(value);
    row.shape = VALUE_STRING;
    row.run = run;
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

  /// Makes `value` the list of `elements`, each appended after it.
  pub fn set_list(&mut self, value: ValueId, elements: &[ValueId]) {
    self.set_items(value, VALUE_LIST, elements);
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

  /// Gives `value` the type it was coerced against.
  pub fn set_value_ty(&mut self, value: ValueId, ty: TyId) {
    debug_assert!(
      (ty.get() as usize) < self.model.tys.len(),
      "a type is interned before a value is given it"
    );
    self.value_row(value).ty = ty;
  }

  /// Marks, as an error reported at `at` does, every contribution and member whose extent holds
  /// `at`. Marks are applied when the model is finished.
  pub fn mark(&mut self, at: Location) {
    self.marks.push(at);
  }

  /// Makes `value` a list or a set, `shape`, of `elements`, each appended after it.
  fn set_items(&mut self, value: ValueId, shape: u8, elements: &[ValueId]) {
    debug_assert!(
      elements.iter().all(|item: &ValueId| *item > value),
      "what a value holds is appended after it"
    );
    let run = append(&mut self.model.value_items, elements);
    let row = self.value_row(value);
    row.shape = shape;
    row.run = run;
  }
}
