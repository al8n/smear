//! The packed state against tokora's own `RecursionLimiter`, the tracker `SyntacticLimits` wraps:
//! a state refuses where that tracker refuses, with the tracker's own error, at every depth a walk
//! across a ceiling reaches, except where the clamp lowers a ceiling above 4,294,967,295. The
//! block's word shares the ceiling's word, saturates at the top of its half, and moves neither the
//! ceiling nor the depth.

use tokora::state::RecursionLimiter;

use super::SyntacticState;
use crate::limits::SyntacticLimits;

/// The highest ceiling a state holds, stated here and not read from the state.
const HELD: usize = 4_294_967_295;

/// Zero, one, the highest ceiling a state holds, the lowest above it and `usize::MAX`: the last
/// two are clamped. A 32-bit target has no `1 << 32`, and its `usize::MAX` is the highest held.
const CEILINGS: [u64; 5] = [0, 1, (1 << 32) - 1, 1 << 32, usize::MAX as u64];

/// A walk from two below a depth, `true` for a bracket that opens: one down, five up and five
/// down. It crosses that depth both ways, and saturates where the depth is zero or `usize::MAX`.
const WALK: [bool; 11] = [
  false, true, true, true, true, true, false, false, false, false, false,
];

/// What `state` answers against `old`, the unclamped tracker stepped alike, at `ceiling`.
fn assert_alike(state: &SyntacticState, old: &RecursionLimiter, ceiling: usize) {
  let held = ceiling.min(HELD);
  let depth = old.depth();
  let at = RecursionLimiter::with_limitation(held).with_depth(depth);
  assert_eq!(
    state.check(),
    at.check(),
    "ceiling {ceiling}, depth {depth}: the tracker at the held ceiling"
  );
  if held == ceiling || depth <= held {
    assert_eq!(
      state.check(),
      old.check(),
      "ceiling {ceiling}, depth {depth}: as the unclamped tracker answers"
    );
  } else {
    assert!(
      state.check().is_err(),
      "ceiling {ceiling}, depth {depth}: the clamp refuses"
    );
  }
  assert_eq!(
    state.limits(),
    SyntacticLimits::from(at),
    "ceiling {ceiling}, depth {depth}: the limits read back"
  );
  assert!(
    !state.in_const_block() && state.open_angles() == 0,
    "ceiling {ceiling}, depth {depth}: the depth moved the block's word"
  );
}

#[test]
fn a_packed_state_refuses_where_its_tracker_did_and_its_block_word_saturates() {
  for ceiling in CEILINGS
    .into_iter()
    .filter_map(|ceiling| usize::try_from(ceiling).ok())
  {
    let held = ceiling.min(HELD);
    for across in [0, held, ceiling, usize::MAX] {
      let mut old = RecursionLimiter::with_limitation(ceiling).with_depth(across.saturating_sub(2));
      let mut state = SyntacticState::new(SyntacticLimits::from(old));
      assert_alike(&state, &old, ceiling);
      for opens in WALK {
        if opens {
          old.increase();
          state.increase_depth();
        } else {
          old.decrease();
          state.decrease_depth();
        }
        assert_alike(&state, &old, ceiling);
      }
    }

    // The block's word at the top of its half, inside a block, at this ceiling.
    let outside = SyntacticState::new(SyntacticLimits::with_max_nesting_depth(ceiling));
    let limits = outside.limits();
    let mut state = outside;
    state.enter_const_block();
    state.set_block(u32::MAX);
    assert_eq!(state.open_angles(), 4_294_967_294, "ceiling {ceiling}");
    state.open_angle();
    assert_eq!(
      state.open_angles(),
      4_294_967_294,
      "ceiling {ceiling}: a `<` at the top leaves the word"
    );
    state.increase_depth();
    state.decrease_depth();
    assert_eq!(
      (state.in_const_block(), state.open_angles(), state.limits()),
      (true, 4_294_967_294, limits),
      "ceiling {ceiling}: the word and the depth move apart"
    );
    state.close_angle();
    assert_eq!(state.open_angles(), 4_294_967_293, "ceiling {ceiling}");
    state.clear_angles();
    state.close_angle();
    assert!(
      state.in_const_block() && state.open_angles() == 0,
      "ceiling {ceiling}"
    );
    state.leave_const_block();
    state.open_angle();
    assert_eq!(
      state, outside,
      "ceiling {ceiling}: outside a block a `<` counts nothing"
    );
  }

  // The clamp itself, from the public door: `unlimited()` reads back as the held ceiling, and on a
  // 64-bit target the first depth above it is refused where the unclamped tracker let it through.
  let unlimited = SyntacticState::new(SyntacticLimits::unlimited());
  assert_eq!(unlimited.limits().max_nesting_depth(), HELD);
  assert_eq!(
    unlimited,
    SyntacticState::new(SyntacticLimits::with_max_nesting_depth(HELD))
  );
  if let Ok(above) = usize::try_from(1u64 << 32) {
    let limits = SyntacticLimits::from(RecursionLimiter::unlimited().with_depth(above));
    assert!(limits.check().is_ok());
    let refusal = SyntacticState::new(limits).check().unwrap_err();
    assert_eq!((refusal.depth(), refusal.limitation()), (above, HELD));
  }
}
