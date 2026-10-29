#!/bin/bash
set -uo pipefail

# The feature-matrix gate, as ONE string that CI and a local run share.
#
# ## Why this is a script and not two lines in a workflow
#
# `.github/workflows/ci.yml` sets `RUSTFLAGS: -Dwarnings` for every job in the file. A local
# `cargo hack clippy --each-feature` does not, so a rustc lint is an ERROR there and a WARNING
# here — and the two runs are then different questions wearing the same name. That gap cleared a
# tree twice on this branch, both times on the alloc-as-std alias, and both times the local run
# was reported green:
#
#   * commit 1 of the split — the local gate was `cargo hack CHECK`, which runs no clippy lints at
#     all, so it never saw `unused_extern_crates` on `smear`'s alias;
#   * `699fb9a` — the local gate was `cargo hack CLIPPY` and did see it, as a warning, because
#     `RUSTFLAGS` was not exported. The failure needs BOTH `-D warnings` in force AND a feature set
#     with `std` off and no dialect; the `--all-features` runs had the first and not the second,
#     and the `--each-feature` run had the second and not the first. Neither gate was wrong on its
#     own axis. It was found by a branch stacked on this one.
#
# So the invocation lives here, the workflow calls this file, and there is nothing left to drift.
# Run it before pushing; it is what CI runs.
#
# `RUSTFLAGS` is composed rather than overwritten if the caller already set it, so CI's
# `-C debuginfo=0 -C codegen-units=16` survive and a local run gets the deny it would otherwise
# lack.
#
# Usage: ci/hack.sh
#   CARGO and CARGO_TARGET_DIR are honoured. `CARGO_HACK_TOOLCHAIN=+stable` picks a toolchain;
#   CI's matrix uses stable, and nightly is worth running locally because its lint set is wider.

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CARGO="${CARGO:-cargo}"
TOOLCHAIN="${CARGO_HACK_TOOLCHAIN:-}"

case "${RUSTFLAGS:-}" in
  *-Dwarnings*|*"-D warnings"*) ;;
  *) export RUSTFLAGS="-Dwarnings ${RUSTFLAGS:-}" ;;
esac
echo "RUSTFLAGS=$RUSTFLAGS"

cd "$REPO_ROOT" || exit 1
status=0

# Pass 1 — every feature of every member, one at a time, plus none and all.
#
# `--tests` is on both passes, and it is what makes each row check the member's TEST code too: unit
# tests and `tests/*.rs`, built with the row's features and nothing else. Without it a row compiles
# only the library, so a test that names `std` while its row is `no_std` — `to_string()` with no
# `alloc` import, `thread_local!`, `std::thread` — was checked in no row but the full-feature
# ones. al8n/smear#140. It is not `--all-targets`: examples and benches are built by their own
# steps, and a bench pulls the criterion stack into every row.
#
# `--keep-going` is on both passes: one run must report EVERY failing row, not the first. cargo-hack
# stops at the first row that fails, and with test targets in every row the first failure hides the
# rest, which costs one CI round trip per row. It still exits non-zero when any row fails, and
# `|| status=1` below carries that out of each pass, so the script fails if either pass did.
#
# The cost is wall-clock, a test target compiled once per row, and the first CI run of this change
# is its measurement. If it proves too dear the answer is a smaller set of chosen no-`std` rows with
# `--tests`, not dropping it again.
echo "== pass 1: cargo hack clippy --each-feature --tests =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy --each-feature --tests --keep-going || status=1

# Pass 2 — the pairwise isolation cells the crate split used to provide by accident.
# `smear-parser`'s own `--each-feature` run was implicitly "parser x feature", because its build
# always contained the parser; a single-crate `--each-feature` never builds `parser` together with
# exactly one other feature. This restores those pairs deliberately, and the two that matter are
# `parser` x `bytes` (60 parser cfg sites) and `parser` x dialect.
#
# `smear-lexer` needs no pass of its own, and that is measured rather than assumed: not one of its
# features implies another — every entry in its `[features]` table is either empty or a forward to
# a dependency — so pass 1 already builds each of them in the only combination the old
# single-crate run gave it.
#
# `rowan`, `lossless-coverage`, `validator`, `introspection` and `proto` are excluded because they
# already imply `parser`, so their pass-1 cells ARE pairs; `default`, `--no-default-features` and
# `--all-features` are excluded because those three runs would duplicate pass 1 exactly.
#
# `parser` itself is NOT in the exclude list: cargo-hack rejects a feature named by both
# `--features` and `--exclude-features` (verified against 0.6.43, "specified by both"), and it does
# not need to be there — it already skips the `parser,parser` cell on its own. Nine cells, one per
# remaining feature, and a feature added to the manifest later joins them without anyone editing
# this file.
#
# `--tests` here for the reason pass 1 gives: these are the cells whose test targets no other row
# builds.
echo "== pass 2: the parser x feature pairs =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy -p smear --each-feature --features parser --tests --keep-going \
  --exclude-features rowan,lossless-coverage,validator,introspection,proto,default \
  --exclude-no-default-features --exclude-all-features || status=1

exit "$status"
