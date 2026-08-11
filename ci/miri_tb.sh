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
# it is the exact shape of #73. Sub-second, and it reads the real `smear/tests/` partition and
# the real `smear/Cargo.toml` feature table, so it also fails if that tree stops having both
# excluded and compiled targets to distinguish, or if `rowan` starts resolving on from defaults.
# PYTHON >= 3.11, checked here rather than discovered as a traceback twenty minutes in.
# `ci/miri_scope.py` parses `smear/Cargo.toml` with `tomllib`, which entered the standard library
# in 3.11; on macOS `/usr/bin/python3` is 3.9 and would fail on the import alone. The other Python
# gates in `ci/` are deliberately 3.9-safe — this is the only floor in the directory.
if ! python3 -c 'import sys; raise SystemExit(0 if sys.version_info >= (3, 11) else 1)'; then
  echo "FAIL: ci/miri_scope.py needs Python >= 3.11 (tomllib); \`python3\` here is $(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')" >&2
  exit 1
fi

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
# 2026-08-07 under what was then a `-p smear` selection: 478 lib unit tests, the
# lexer's and the parser's in one binary while the crates were merged. The split redistributed
# those across four binaries — 505 tests at this cell's feature set — and this timing has not been
# re-measured on the derived selection. What is
# given up is "integration-suite coverage at 32-bit", not "32-bit
# coverage" — tests/oracle.rs and tests/tokora_conformance.rs still run
# under Miri on x86_64 and powerpc64-unknown-linux-gnu, so the scenarios
# themselves stay covered, just not at this pointer width.

# ── THE WALL-CLOCK CUT, AND WHAT IT COSTS ───────────────────────────────────────────────────
#
# The previous revision of this comment predicted that `tests/syntactic_span_extent.rs` and
# `tests/syntactic_x_span_extent.rs` would be what stopped a cell finishing, and declined to cut
# them because nothing had a CI measurement yet. The measurement arrived. Run 31318425279,
# `miri-tb-x86_64-unknown-linux-gnu`, timestamps from its own log:
#
#   14:28:25Z  lib unit tests, 478 of them            -> 14:38:41Z   10m16s
# (Those 478 are one binary, from when the crates were merged. Since the split the selection is
#  four binaries — 125 + 353 + 12 + 15 = 505 — and this timing has not been re-measured on it.)
#   14:38:42Z  46 excluded targets, empty harnesses   -> 14:39:10Z      28s
#   14:39:10Z  tests/oracle.rs, 8 tests               -> 14:47:11Z    8m01s
#   14:47:15Z  tests/syntactic_span_extent.rs         -> killed 20:27Z, STILL IN IT
#
# Five hours forty minutes inside one test, `trivia_injection_leaves_every_span_on_its_own_tokens`,
# with two targets still queued behind it. That is not a slow cell, it is a cell that cannot
# finish: five consecutive runs on `feat/proto-collect-substrate` were each cancelled by the next
# push, and of the ten cells the ONLY two that have ever concluded are the i686 pair — which run
# `--lib` alone, and therefore never build these two targets at all.
#
# The cut is PER TEST, not per file, and it is written in the two files rather than here. Each
# carries `#[cfg_attr(miri, ignore)]` on its two corpus sweeps and on nothing else: the four
# witnesses in each are hand-written inputs that cost an interpreter nothing, and excluding a free
# test buys this cell nothing. Confirmed locally on 2026-08-10, `--test-threads=1` on
# `aarch64-apple-darwin`: five of `syntactic_span_extent.rs`'s six tests finish and
# `trivia_injection_leaves_every_span_on_its_own_tokens` does not return.
#
# In the files because `cargo test` is then untouched and still runs all six on every push;
# because `ci/miri_scope.py` DERIVES the count from those attributes and fails the cell if the
# reported `ignored` disagrees with it, or if every test in a target ends up ignored; and because
# a `--test`-level list in this script would be a second statement of the same fact, free to
# drift from the first.
#
# The derived count does not stand alone, because on its own it ratifies additions: a fifth
# `#[cfg_attr(miri, ignore)]` moves the source and the reported `ignored` together, they agree,
# and the cell stays green over a coverage cut nobody chose. So `ci/miri_scope.py` also holds
# `MIRI_IGNORE_BUDGET`, a frozen total that fails the cell in EITHER direction and requires
# `.github/workflows/miri.yml` to keep stating the same number. Adding or removing one of these
# ignores is a decision, and it is meant to show up as a diff to that literal.
#
# What it costs is small for the question Miri answers. Miri decides whether an execution path has
# undefined behaviour; those four sweeps vary the INPUT — 56 and 90 corpus entries times eight
# ignorable forms — over paths the lib tests, `tests/oracle.rs` and `tests/tokora_conformance.rs`
# interpret in this same cell, and they spend most of their time in `format!("{:#?}")` and a
# string walk that are the tests' own safe code. Their headers carry the full argument, including
# why this is a cost model incompatible with an interpreter rather than a defect: the sibling
# sweep over the smaller `invalid_` corpus runs the same machinery and terminates.
#
# ── AND A SECOND THING THAT WOULD HAVE KEPT THE CELL RED AT ZERO WALL CLOCK ─────────────────
#
# Nine of the targets this selection builds are gated on `validator`, `proto` or `introspection`,
# which `smear` does not default to and this script does not pass — so they compile to empty
# harnesses, exactly like the 37 `rowan`-gated ones. `ci/miri_scope.py` used to treat "not
# `rowan`-gated and ran zero tests" as #73's silence and fail the cell for it, and no cell had
# ever run far enough to find out. It now derives the cell's whole feature set from
# `smear/Cargo.toml` and evaluates each target's crate-level `#![cfg(...)]` against it, so those
# nine are declared exclusions with a printed reason rather than a verdict nobody could reach.

if [ "$TARGET" = "i686-unknown-linux-gnu" ]; then
  MIRIFLAGS="$MIRIFLAGS -Zmiri-address-reuse-rate=1.0"
  TEST_ARGS=""
  TESTS_SELECTED=0
else
  TEST_ARGS="--tests"
  TESTS_SELECTED=1
fi

export MIRIFLAGS

# ── WHY A NAMED SELECTION, AND WHAT IT COSTS ────────────────────────────────────────────────
#
# This line said `cargo miri test $TEST_ARGS --target $TARGET --lib` until #77 — no `-p`, so it
# selected every workspace member, and cargo unified their features. `smear` does NOT default to
# `rowan`, but `benchmarks` and `smear-smoke` both enabled it, so the resolve turned it on and the
# whole lossless CST tower entered this cell. Nobody widened the scope; #70 and then #84 added a
# member and the arithmetic did it. See the note in the root `Cargo.toml`.
#
# `benchmarks` has since been dissolved into `smear` and `smear-smoke` is the only member left
# that enables `rowan`, which narrows the accident by one member and removes none of the reason:
# a bare selection is still a resolve rather than a decision, and `smear`'s own `[[bench]]`
# targets now ask for `rowan` and `validator` through `required-features` — which `-p smear`
# leaves unsatisfied, so they are skipped here rather than interpreted.
#
# The widening was not survivable, and that is the finding rather than an inconvenience.
# `rowan 0.17.0` has undefined behaviour reachable from its ordinary public API, under BOTH
# aliasing models and in two independent places:
#
#   * Tree Borrows — THIS model — `src/cursor.rs:136`, `rowan::cursor::free` deallocating a
#     `Box<NodeData>` through a tag an ancestor's `Cell` still holds frozen. Reached from
#     dropping any red-tree `SyntaxNode`; `smear/tests/lossless_roundtrip.rs` trips it on
#     `parse_document(src).syntax().text().to_string()`, which is as ordinary as this API gets.
#   * Stacked Borrows — `src/arc.rs:264`, a `&HeaderSlice<H, [T]>` forged over the whole slice
#     out of a `&self` whose retag covers only the header. Reached from
#     `GreenNodeBuilder::finish_node`, i.e. from building ANY tree. Tree Borrows accepts that
#     one — upstream's position is that Stacked Borrows does not support the pattern rowan
#     needs — which is why the two models fail in different places rather than the same one.
#
# Both were reproduced on 2026-08-07 by two standalone programs that name nothing but rowan's
# public API, against `0.15.19`, `0.16.1`, `0.16.2` (yanked) and `0.17.0` — the newest release,
# published 2026-08-02. The construct is byte-identical in all four, so THERE IS NO VERSION TO
# BUMP TO. That prediction has since been tested rather than left standing: this workspace moved
# to `0.17.0`, for an unrelated reason (rowan is in tokora's public API, so its major is tokora's
# — al8n/tokora#237), and both sites were re-measured on it. Both survived. The line numbers
# above are 0.17.0's; on 0.16.1 they were `arc.rs:260` and `cursor.rs:219`. Upstream has had it reported since 2021 (rust-analyzer/rowan#108, and #163, #192);
# the only fix attempt, PR #211, is a conflicting draft whose own description says the mutable
# path still fails under Tree Borrows.
#
# So a named selection is not a cost/benefit trade about minutes. The lossless tower CANNOT be
# interpreted, at any price, until rowan is fixed — and the 37 `#![cfg(feature = "rowan")]`
# targets in `smear/tests/` are therefore excluded here rather than left to fail. Naming the
# packages is the mechanism because it is the one that a future workspace member cannot undo:
# feature unification is over the SELECTED packages, and this selects exactly these.
#
# WHAT THIS CELL COVERS, and it is measured rather than claimed. The half where this project's own
# `unsafe` lives — the SIMD lexer and the syntactic parser, through `tokora`'s substrate — and all
# four of those `unsafe` sites are in `smear-lexer/src/string_lexer/`. The selection is
# `smear-lexer`, `smear-parser`, `smear-compiler` and `graphql-proto`: 125 + 353 + 12 + 15 lib unit
# tests, counted with `cargo test -p <m> --lib -- --list` at this cell's feature set.
#
# `smear` AND `smear-schema` ARE DELIBERATELY OUT, which is not a narrowing. `smear/src` is one
# file of re-exports and its lib carries no unit tests at any feature set; `smear-schema`'s three
# are behind `build`, which these scripts do not enable. Selecting either would produce a harness
# with 0 tests, and `ci/miri_scope.py` fails that on purpose — "a target that compiles to an empty
# harness passes without testing anything" is #73's mechanism. The reasons live beside the names,
# in `MIRI_NOT_SELECTED`, and `ci/feature_reachability.py` requires each to still match a real
# publishable member.
#
# THE SELECTION BELOW IS NOT WRITTEN HERE. It is read from `MIRI_PACKAGES`. Those were two
# hand-maintained lists and they drifted: the constant grew to six members through the split while
# this script still passed two, so the guard was checking a declaration the run did not execute and
# the cell failed late, after the expensive part. Read `ci/miri_scope.py`'s header before changing
# any of this.
LOG="$(mktemp)"
set +e
# BUILT FROM `MIRI_PACKAGES`, not written out beside it. These two lists were hard-coded
# separately and they drifted: the constant grew to six members as the split extracted them while
# this line still passed two, so `ci/miri_scope.py` — which reads the constant as its expectation —
# was checking a declaration the workflow did not execute, and the cell failed late, after the
# expensive run. Deriving removes the second list rather than adding a check that compares them.
#
# Read into an array so each token is one argument, which `-p smear-lexer` needs and word-splitting
# an unquoted string would only get right by accident.
MIRI_SELECTION=()
while IFS= read -r token; do MIRI_SELECTION+=("$token"); done < <(python3 ci/miri_scope.py --print-packages)
if [ "${#MIRI_SELECTION[@]}" -eq 0 ]; then
  echo "FAIL: ci/miri_scope.py printed no package selection." >&2
  exit 1
fi
echo "selection: ${MIRI_SELECTION[*]}"

cargo miri test "${MIRI_SELECTION[@]}" $TEST_ARGS --target "$TARGET" --lib 2>&1 | tee "$LOG"
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

