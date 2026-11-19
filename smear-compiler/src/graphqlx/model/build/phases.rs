//! What the hand-built models of the tests write in one step: an extern whose texts are given, as
//! a hand-built model holds one; a value written whole, each over coercion's own writers; and
//! marks.
//!
//! GATED ON HAVING A CALLER. The test-support door is this module's only caller: a method that no
//! shipped build calls is dead code. A phase moves the methods it calls out of this module, as the
//! module tree, item collection, lowering, instantiation, coercion, binding and the tier phase
//! moved theirs. Binding writes an extern with `bind_extern`, over the texts coercion kept;
//! `push_extern` keeps its own, through the same helper.

use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{DefId, Sym, ValueId},
  model::{ty::PathRoot, value::LiteralKind},
  package::Tier,
};

use super::ModelBuilder;

impl ModelBuilder {
  /// Adds an `@extern` to `def`, after those it has: its `lang` and `type`, kept as texts of its
  /// own, and its effective tier. It is bound from no application.
  pub fn push_extern(&mut self, def: DefId, lang: &str, ty: &str, tier: Tier) {
    let lang = self.keep_text(lang);
    let ty = self.keep_text(ty);
    self.append_extern(def, lang, ty, tier, None);
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

  /// Marks, as an error reported at `at` does, every contribution and member whose extent holds
  /// `at`. Marks are applied when the model is finished.
  pub fn mark(&mut self, at: Location) {
    self.marks.push(at);
  }
}
