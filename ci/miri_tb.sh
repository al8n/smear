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
# CI matrix. Raising the reuse rate is a known lever for this, but whether
# it is *sufficient* to avoid exhaustion for this suite is unproven until a
# CI run confirms it.
if [ "$TARGET" = "i686-unknown-linux-gnu" ]; then
  MIRIFLAGS="$MIRIFLAGS -Zmiri-address-reuse-rate=1.0"
fi

export MIRIFLAGS

cargo miri test --tests --target $TARGET --lib

