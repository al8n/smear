#!/bin/bash
set -euo pipefail

# Does the core tier link no allocator?
#
# `smear` with every capability and no tier feature is meant to compile, and to export only what
# needs no allocation. That is a claim about the whole dependency graph, which `cargo check` on a
# host with `std` cannot see: an `alloc` pulled in by any crate is invisible there. `ci/noalloc/`
# is a `staticlib` with no `#[global_allocator]`, and rustc refuses a non-rlib artifact whose graph
# links `alloc` unless a global allocator exists. So a green build is the proof.
#
# ## The steps
#
#   (a) the fixture lists exactly the `smear` features that imply no tier — derived, not remembered
#   (b) the fixture builds for TARGET                                    must COMPILE
#   (c) the same build with the fixture's `alloc` feature                must FAIL, on the missing
#       allocator. This is the control: it is what stops (b) being vacuous, because a build that
#       cannot fail proves nothing. The fixture's `extern crate smear;` is what makes it fail.
#
# Usage: ci/noalloc.sh
#   TARGET picks the target (default `thumbv7em-none-eabihf`), which must be installed:
#   `rustup target add "$TARGET"`. It needs compare-and-swap, so `Arc` is not the question being
#   asked; `thumbv6m` stays with the `noatomic` job. CARGO and CARGO_TARGET_DIR are honoured.

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CARGO="${CARGO:-cargo}"
TARGET="${TARGET:-thumbv7em-none-eabihf}"
FIXTURE="ci/noalloc/Cargo.toml"

cd "$REPO_ROOT"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "== (a) the fixture asks smear for exactly the features that imply no tier =="
$CARGO metadata --no-deps --format-version 1 > "$tmp/workspace.json"
$CARGO metadata --manifest-path "$FIXTURE" --no-deps --format-version 1 > "$tmp/fixture.json"
python3 - "$tmp/workspace.json" "$tmp/fixture.json" <<'PY'
import json
import sys

with open(sys.argv[1]) as handle:
    workspace = json.load(handle)
with open(sys.argv[2]) as handle:
    fixture = json.load(handle)

table = next(p for p in workspace["packages"] if p["name"] == "smear")["features"]


def closure(name):
    # Local features only: `dep:x`, `x/y` and `x?/y` leave the crate.
    seen, todo = set(), [name]
    while todo:
        feature = todo.pop()
        if feature in seen:
            continue
        seen.add(feature)
        todo.extend(e for e in table.get(feature, []) if "/" not in e and not e.startswith("dep:"))
    return seen


expected = {f for f in table if f != "default" and not {"alloc", "std"} & closure(f)}
asked = set(next(
    d for p in fixture["packages"] for d in p["dependencies"] if d["name"] == "smear"
)["features"])

if asked != expected:
    sys.exit(
        "FAIL: ci/noalloc/Cargo.toml asks smear for the wrong features.\n"
        f"  missing (imply no tier, not asked for): {sorted(expected - asked)}\n"
        f"  extra (imply `alloc` or `std`, or are not features): {sorted(asked - expected)}"
    )
print(f"ok: {len(expected)} features: {', '.join(sorted(expected))}")
PY

echo "== (b) the core tier builds for $TARGET and links no allocator =="
$CARGO build --manifest-path "$FIXTURE" --target "$TARGET"

echo "== (c) the control: with \`alloc\` the same build must fail on the missing allocator =="
# `if` keeps the expected failure from tripping `set -e`.
if $CARGO build --manifest-path "$FIXTURE" --target "$TARGET" --features alloc \
  > "$tmp/control.log" 2>&1; then
  echo "FAIL: the control built. With \`alloc\` the fixture links an allocator and has none, so" >&2
  echo "      the build in (b) proves nothing about the core tier." >&2
  cat "$tmp/control.log" >&2
  exit 1
fi
if ! grep -q 'no global memory allocator found' "$tmp/control.log"; then
  echo "FAIL: the control failed, but not on the missing allocator. Its output:" >&2
  cat "$tmp/control.log" >&2
  exit 1
fi
echo "ok: the control failed with \`no global memory allocator found\`"
