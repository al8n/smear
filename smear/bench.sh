#!/usr/bin/env bash
# Run the GraphQL lexer benchmarks with --quick so the suite finishes fast.
# Usage:
#   ./bench.sh                  # runs syntactic + simd_phase1
#   ./bench.sh lossless         # also includes the lossless group
#   ./bench.sh all              # runs all three groups

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
