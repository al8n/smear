#!/bin/bash
set -e

# Check if TARGET is provided, otherwise panic
if [ -z "$1" ]; then
  echo "Error: TARGET is not provided"
  exit 1
fi

TARGET=$1

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
# and under Stacked Borrows (ci/miri_sb.sh): the smear-lexer lib tests and
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
# lib tests (115 passing at last measurement) exercise exactly that, under
# both aliasing models. What is given up is "integration-suite coverage at
# 32-bit", not "32-bit coverage" — tests/oracle.rs and
# tests/tokora_conformance.rs still run under Miri on x86_64 and
# powerpc64-unknown-linux-gnu, so the scenarios themselves stay covered,
# just not at this pointer width.
if [ "$TARGET" = "i686-unknown-linux-gnu" ]; then
  MIRIFLAGS="$MIRIFLAGS -Zmiri-address-reuse-rate=1.0"
  TEST_ARGS=""
else
  TEST_ARGS="--tests"
fi

export MIRIFLAGS

cargo miri test $TEST_ARGS --target $TARGET --lib

