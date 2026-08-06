#!/usr/bin/env bash
# Run the GraphQL lexer benchmarks with --quick so the suite finishes fast.
# Usage:
#   ./bench.sh                  # runs syntactic + simd_phase1
#   ./bench.sh lossless         # also includes the lossless group
#   ./bench.sh all              # runs all three groups
#
# Lives beside the benches it drives. It was `smear/bench.sh` and `cd`d to its own directory, so
# the bare `cargo bench --bench lex_baseline` below resolved against `smear`'s manifest; moving
# the benches to this member without moving this file would have left a script that still ran
# and still exited 0 while selecting a package with no `[[bench]]` target left in it.

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
