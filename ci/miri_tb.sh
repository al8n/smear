#!/bin/bash
set -e

# Check if TARGET is provided, otherwise panic
if [ -z "$1" ]; then
  echo "Error: TARGET is not provided"
  exit 1
fi

TARGET=$1

# Prove the scope guard can fail, BEFORE `rustup`, before `cargo miri setup`, before anything
# that costs minutes. `ci/miri_scope.py` is what makes this cell's coverage claim checkable
# rather than asserted, and a guard that has quietly stopped checking is worse than no guard —
# it is the exact shape of #73. Sub-second, and it reads the real `smear/tests/` partition, so
# it also fails if that tree stops having both gated and un-gated targets to distinguish.
python3 ci/miri_scope.py --selftest

rustup toolchain install nightly --component miri
rustup override set nightly
cargo miri setup

MIRIFLAGS="-Zmiri-strict-provenance -Zmiri-disable-isolation -Zmiri-symbolic-alignment-check -Zmiri-tree-borrows"

# 32-bit targets simulate a 4 GiB address space, and Miri does not
# aggressively reuse freed addresses by default; a long allocation-heavy
# test binary can exhaust it cumulatively ("no more free addresses in the
# address space"). i686-unknown-linux-gnu is the only 32-bit target in the
# CI matrix.
#
# -Zmiri-address-reuse-rate=1.0 stays on for i686: it is cheap and correct
# in intent. But it was measured *insufficient alone* across three
# consecutive CI runs, identically under this aliasing model (Tree Borrows)
# and under Stacked Borrows (ci/miri_sb.sh): the `smear` lib tests and
# tests/oracle.rs pass in full, then tests/tokora_conformance.rs still
# exhausts the address space partway through graphqlx::lossless_conformance.
# cargo already runs each test binary as its own process, so every binary
# starts with a fresh 4 GiB space — the exhaustion accrues *within* one
# binary, so the only further lever is not running that binary at all under
# this target.
#
# So i686 alone drops `--tests` and runs the lib unit tests only. This is a
# deliberate, scoped reduction, not a silent one: i686's unique value in
# this matrix is 32-bit pointer width on a SIMD byte-level lexer, and the
# lib tests exercise exactly that, under both aliasing models. Measured
# 2026-08-07 under the `-p smear` selection below: 473 lib unit tests, the
# lexer's and the parser's in one binary since the crates merged. What is
# given up is "integration-suite coverage at 32-bit", not "32-bit
# coverage" — tests/oracle.rs and tests/tokora_conformance.rs still run
# under Miri on x86_64 and powerpc64-unknown-linux-gnu, so the scenarios
# themselves stay covered, just not at this pointer width.

# ── A WALL-CLOCK RISK THIS SELECTION CARRIES, STATED BEFORE IT BITES ────────────────────────
#
# `tests/syntactic_span_extent.rs` and `tests/syntactic_x_span_extent.rs` are in this cell and
# have almost certainly never completed in one. They arrived in #72 and grew in #80; they are
# not `rowan`-gated, so they were always SELECTED — but cargo runs test binaries in name order,
# `lossless_*` sorts before `syntactic_*`, and since #70 every Miri cell aborted inside the
# lossless tower before reaching them. `miri.yml` has no `success` conclusion in its last 40
# runs either, so nothing has covered them.
#
# They are also the most expensive thing here by a wide margin. Measured 2026-08-07 on one
# aarch64-apple-darwin core, this model, nothing else on that core: ONE of the six tests in
# `syntactic_span_extent.rs` — `trivia_injection_leaves_every_span_on_its_own_tokens` — had NOT
# finished after 36 minutes. Its sibling sweeps 90 corpus entries where that one sweeps 56. For scale,
# the whole lib suite is 183s and `tests/oracle.rs` is 180s.
#
# The number that matters and is NOT measurable from here is the GitHub runner's. `miri.yml`'s
# header records 4h20m for the slowest cell of run 30963710061 — created before #72, so without
# these two targets. A GitHub job is killed at 6 hours. If a cell starts timing out, this is the
# reason, and the remedy is the one the i686 block above already demonstrates: reduce the
# selection deliberately, in writing, naming what is given up. `--test`-level exclusion of these
# two is the obvious first cut, and it costs less than it looks — Miri finds UB in code PATHS,
# and these sweeps vary the INPUT over paths the lib tests, `oracle.rs` and
# `tokora_conformance.rs` already interpret.
#
# It is not cut pre-emptively, because dropping coverage on an estimate is the mistake in the
# other direction, and nothing here has a CI measurement yet.
if [ "$TARGET" = "i686-unknown-linux-gnu" ]; then
  MIRIFLAGS="$MIRIFLAGS -Zmiri-address-reuse-rate=1.0"
  TEST_ARGS=""
  TESTS_SELECTED=0
else
  TEST_ARGS="--tests"
  TESTS_SELECTED=1
fi

export MIRIFLAGS

# ── WHY `-p smear`, AND WHAT IT COSTS ───────────────────────────────────────────────────────
#
# This line said `cargo miri test $TEST_ARGS --target $TARGET --lib` until #77 — no `-p`, so it
# selected every workspace member, and cargo unified their features. `smear` does NOT default to
# `rowan`, but `benchmarks` and `smear-smoke` both enable it, so the resolve turned it on and the
# whole lossless CST tower entered this cell. Nobody widened the scope; #70 and then #84 added a
# member and the arithmetic did it. See the note in the root `Cargo.toml`.
#
# The widening was not survivable, and that is the finding rather than an inconvenience.
# `rowan 0.16.1` has undefined behaviour reachable from its ordinary public API, under BOTH
# aliasing models and in two independent places:
#
#   * Tree Borrows — THIS model — `src/cursor.rs:219`, `rowan::cursor::free` deallocating a
#     `Box<NodeData>` through a tag an ancestor's `Cell` still holds frozen. Reached from
#     dropping any red-tree `SyntaxNode`; `smear/tests/lossless_roundtrip.rs` trips it on
#     `parse_document(src).syntax().text().to_string()`, which is as ordinary as this API gets.
#   * Stacked Borrows — `src/arc.rs:260`, a `&HeaderSlice<H, [T]>` forged over the whole slice
#     out of a `&self` whose retag covers only the header. Reached from
#     `GreenNodeBuilder::finish_node`, i.e. from building ANY tree. Tree Borrows accepts that
#     one — upstream's position is that Stacked Borrows does not support the pattern rowan
#     needs — which is why the two models fail in different places rather than the same one.
#
# Both were reproduced on 2026-08-07 by two standalone programs that name nothing but rowan's
# public API, against `0.15.19`, `0.16.1`, `0.16.2` (yanked) and `0.17.0` — the newest release,
# published 2026-08-02. The construct is byte-identical in all four, so THERE IS NO VERSION TO
# BUMP TO. Upstream has had it reported since 2021 (rust-analyzer/rowan#108, and #163, #192);
# the only fix attempt, PR #211, is a conflicting draft whose own description says the mutable
# path still fails under Tree Borrows.
#
# So `-p smear` is not a cost/benefit trade about minutes. The lossless tower CANNOT be
# interpreted, at any price, until rowan is fixed — and the 33 `#![cfg(feature = "rowan")]`
# targets in `smear/tests/` are therefore excluded here rather than left to fail. `-p smear` is
# the mechanism because it is the one that a future workspace member cannot undo: feature
# unification is over the SELECTED packages, and this selects one.
#
# What that leaves covered is the half where this project's own `unsafe` lives — the SIMD lexer
# and the syntactic parser, through `tokora`'s substrate — and `ci/miri_scope.py` below asserts
# that the covered/excluded split is exactly the one written here, in both directions. Read its
# header before changing any of this.
LOG="$(mktemp)"
set +e
cargo miri test -p smear $TEST_ARGS --target "$TARGET" --lib 2>&1 | tee "$LOG"
STATUS=${PIPESTATUS[0]}
set -e

# Always run the scope guard, including after a failed run: the excluded set it prints is the
# statement of what this cell does not cover, and that is worth reading either way. Its own
# verdict is folded in below rather than allowed to mask Miri's.
SCOPE=0
python3 ci/miri_scope.py --log "$LOG" --tests-selected "$TESTS_SELECTED" \
  --miri-status "$STATUS" || SCOPE=$?
rm -f "$LOG"

if [ "$STATUS" -ne 0 ]; then
  exit "$STATUS"
fi
exit "$SCOPE"

