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
# Its rows are the CORE tier — no allocator, each crate exporting only what needs none — except
# the rows that name a tier: `std`, `default` and `--all-features` reach `std`, and `alloc` reaches
# `alloc`. No other feature implies a tier, so a dependency feature's row is a core row too: it
# compiles the dependency in and exposes nothing that needs `std` or `alloc`.
#
# THE UNVERSIONED DEPENDENCY NAMES ARE NOT ROWS. `bytes`, `bstr`, `hipstr`, `smol-bytes` and
# `rowan` are pure aliases of the versioned features (`bytes = ["bytes_1"]`), so
# each of their rows would build what the versioned row builds and double the matrix. They are
# excluded here and in every pass below. `--exclude-features` also drops cargo-hack's
# `--all-features` row, so the second command asks for it by name.
#
# `--tests` is on every pass, and it is what makes each row check the member's TEST code too: unit
# tests and `tests/*.rs`, built with the row's features and nothing else. Without it a row compiles
# only the library, so a test that names `std` while its row is `no_std` — `to_string()` with no
# `alloc` import, `thread_local!`, `std::thread` — was checked in no row but the full-feature
# ones. al8n/smear#140. It is not `--all-targets`: examples and benches are built by their own
# steps, and a bench pulls the criterion stack into every row.
#
# `--keep-going` is on every pass: one run must report EVERY failing row, not the first. cargo-hack
# stops at the first row that fails, and with test targets in every row the first failure hides the
# rest, which costs one CI round trip per row. It still exits non-zero when any row fails, and
# `|| status=1` below carries that out of each pass, so the script fails if any pass did.
#
# The cost is wall-clock, a test target compiled once per row, and the first CI run of this change
# is its measurement. If it proves too dear the answer is a smaller set of chosen no-`std` rows with
# `--tests`, not dropping it again.
echo "== pass 1: the core tier, cargo hack clippy --each-feature --tests =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy --each-feature --tests --keep-going \
  --exclude-features bytes,bstr,hipstr,smol-bytes,rowan || status=1
$CARGO $TOOLCHAIN hack clippy --all-features --tests --keep-going || status=1

# Pass 2 — the alloc tier: every feature, one at a time, on top of `alloc`.
#
# This is what pass 1 covered until `alloc` became a feature of its own: a build with an allocator
# and no `std`. `alloc` is NOT in the exclude list, because cargo-hack refuses a feature named by
# both `--features` and `--exclude-features`; it skips the `alloc,alloc` cell on its own. The
# no-default row stays on purpose: with `--features alloc` it is "alloc alone". `rowan_0_17` and
# `lossless-coverage` are rows here as well: they imply no tier, so with `alloc` each builds the
# rowan crate and leaves the tower, whose tier is `std`, compiled out.
echo "== pass 2: the alloc tier =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy --each-feature --features alloc --ignore-unknown-features --tests \
  --keep-going --exclude-features std,default,rowan,bytes,bstr,hipstr,smol-bytes \
  --exclude-all-features || status=1

# Pass 2b — `rowan_0_17` on `alloc` and without `std`, with each dialect and with the validator:
# every rowan site of both dialects and every lossless test, compiled where the tower is absent.
#
# The tower is gated on `std`, so here its modules are compiled out while the dialects, the
# validator and the tests beside them are not. A rowan site that is missing its `std` names a
# module that does not exist, or leaves an import or a constructor only the tower used dead under
# `-D warnings`; no other row of any pass builds this configuration with a dialect on. `smear` and
# `smear-parser` are the crates whose rowan sites sit beside dialect code, and `--include-features`
# keeps each to its dialect rows, and `smear` to its validator row as well.
echo "== pass 2b: rowan_0_17 on the alloc tier, with each dialect and the validator =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy -p smear --each-feature --features alloc,rowan_0_17 --tests \
  --keep-going --include-features graphql,graphqlx,validator \
  --exclude-no-default-features --exclude-all-features || status=1
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy -p smear-parser --each-feature --features alloc,rowan_0_17 --tests \
  --keep-going --include-features graphql,graphqlx \
  --exclude-no-default-features --exclude-all-features || status=1

# Pass 3 — the pairwise isolation cells the crate split used to provide by accident.
# `smear-parser`'s own `--each-feature` run was implicitly "parser x feature", because its build
# always contained the parser; a single-crate `--each-feature` never builds `parser` together with
# exactly one other feature. This restores those pairs deliberately, and the two that matter are
# `parser` x `bytes_1` (60 parser cfg sites) and `parser` x dialect. They are alloc-tier cells,
# because the parser needs an allocator: `--features parser,alloc`.
#
# `smear-lexer` needs no pass of its own, and that is measured rather than assumed: not one of its
# features implies another capability — every entry in its `[features]` table is a tier, an empty
# capability or a forward to a dependency — so pass 2 already builds each of them in the only
# combination the old single-crate run gave it.
#
# `rowan_0_17`, `lossless-coverage`, `validator`, `introspection`, `proto` and `analysis` are
# excluded because they already imply `parser`, so their pass-2 cells ARE pairs; `default`,
# `std` and `--all-features` are excluded because the `std` tier is pass 1's and pass 4's, and
# `--no-default-features` because `parser,alloc` is already pass 2's `parser` row. The aliases are
# excluded as in pass 1.
#
# `parser` and `alloc` are NOT in the exclude list: cargo-hack rejects a feature named by both
# `--features` and `--exclude-features` (verified against 0.6.43, "specified by both"), and it
# does not need them there — it already skips the `parser,parser` and `alloc,alloc` cells on its
# own. Eight cells, one per remaining feature, and a feature added to the manifest later joins
# them without anyone editing this file.
#
# `--tests` here for the reason pass 1 gives: these are the cells whose test targets no other row
# builds.
echo "== pass 3: the parser x feature pairs, on the alloc tier =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy -p smear --each-feature --features parser,alloc --tests --keep-going \
  --exclude-features \
  rowan,rowan_0_17,lossless-coverage,validator,introspection,proto,analysis,default,std,bytes,bstr,hipstr,smol-bytes \
  --exclude-no-default-features --exclude-all-features || status=1

# Pass 4 — the std tier, without alloc: `std,alloc` and every dependency feature on `std`.
#
# `std` does not imply `alloc`, and `std` alone is the default tier, whose row is pass 1's. This
# pass adds the rows on top of it that no other pass builds: `std,alloc`, both tiers at once, and
# each dependency feature with `std` and nothing else. `rowan_0_17` and `lossless-coverage` are NOT
# excluded: their pass-1 rows are core rows, so `std,rowan_0_17` and `std,lossless-coverage` are the
# only rows of the matrix that compile the tower on its own, with the tier it needs and no more.
#
# The capabilities are excluded with the aliases: `std` and a capability re-check what the default
# rows, the cross-crate rows and `--all-features` already compile. `--exclude-no-default-features`
# drops the `std`-alone row, which repeats pass 1's. `--include-features` cannot express this
# workspace-wide: with `--ignore-unknown-features` it still emits rows for a feature a package
# lacks.
echo "== pass 4: the std tier, without alloc =="
# shellcheck disable=SC2086
$CARGO $TOOLCHAIN hack clippy --each-feature --features std --ignore-unknown-features --tests \
  --keep-going --exclude-features \
  default,bytes,bstr,hipstr,smol-bytes,rowan,graphql,graphqlx,parser,validator,proto,introspection,analysis,materialized-numbers,test-support,build \
  --exclude-no-default-features --exclude-all-features || status=1

exit "$status"
