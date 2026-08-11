#!/usr/bin/env bash
# Run the GraphQL lexer benchmarks with --quick so the suite finishes fast.
# Usage:
#   ./bench.sh                  # runs syntactic + simd_phase1
#   ./bench.sh lossless         # also includes the lossless group
#   ./bench.sh all              # runs all three groups
#
# Lives in the directory whose benches it drives, and `cd`s there, so the bare
# `cargo bench --bench lex_baseline` below resolves against the nearest manifest above it —
# `smear/Cargo.toml`, which is the package that declares the target. That coupling is the whole
# reason this file travels with the benches: it has already been in two other places (`smear/`,
# then the `smear-benches` member), and each time leaving it behind would have produced a script
# that still ran, still exited 0, and selected a package with no `[[bench]]` target in it.
#
# `lex_baseline` needs no `--features`: it measures the crate's default feature set, which is the
# point of it. The two apollo comparisons do — see `smear/Cargo.toml` — and are not driven here.

set -euo pipefail
cd "$(dirname "$0")"

GROUPS=("graphql/lex/syntactic" "graphql/lex/simd_phase1")

if [[ "${1-}" == "lossless" || "${1-}" == "all" ]]; then
  GROUPS+=("graphql/lex/lossless")
fi

for group in "${GROUPS[@]}"; do
  echo "=== $group ==="
  cargo bench --bench lex_baseline -- --quick "$group"
  echo
done
