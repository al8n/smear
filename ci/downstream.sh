#!/bin/bash
set -u

# Can a SECOND DEPENDENCY reach a `smear::…` capability the consumer never enabled?
#
# `smear` re-exports whole member crates, so a feature of `smear` gates what it claims only where
# `smear/src/lib.rs` writes the `#[cfg]`. Where it forwards to a member feature instead, the `#[cfg]`
# is inside the member and cargo unifies that member's features across the entire graph.
#
# EVERY OTHER GATE IN THIS REPOSITORY BUILDS THIS WORKSPACE, where nothing else requests those member
# features — so none of them can see it. The leak needs a second requester downstream. `ci/downstream/`
# is that downstream, and its compile is the property.
#
# Ported from `pql/ci/downstream.sh`, which exists because pql hit the sibling of this problem and
# spent six review rounds converging on the shape: flags composed on the command line, a committed
# lock read with `--locked`, POS legs that must compile, negative legs that must fail ON A NAMED
# REASON rather than merely fail, and a fixture with no dev-dependencies.
#
# ## The legs
#
#   EQ-POS(m/f)     smear/std[,…], smear/f, m/f       must COMPILE   — agreement is allowed
#   EQ-LEAK(m/f)    smear/std[,…],          m/f       must FAIL      — on the equivalence assertion
#   CTL(pair)       uses-pair, smear/std[,…]          must FAIL      — on an unresolved path
#   POS(pair)       uses-pair, smear/std[,…], smear/f must COMPILE
#   LEAK(pair)      uses-pair, smear/std[,…], m/f     must FAIL      — on the equivalence assertion
#
# EQ is total over every member feature `cargo metadata` reports and needs no consumer code: the
# assertion is in `smear`, so a disagreeing graph fails on its own. CTL/POS/LEAK are the smaller
# family with a real path behind them, and CTL is what stops POS being vacuous — a `uses-` module
# that stopped naming its path would make CTL compile, which is a hard red.
#
# ## What a red here means
#
# A LEAK leg that COMPILES is the defect itself: a consumer's build contains a capability it did not
# ask for. Measured before the repair, all ten did.
#
# Usage: ci/downstream.sh
#   CARGO and CARGO_TARGET_DIR are honoured and passed to every leg, so all legs share one target
#   directory. `--locked` is used throughout, so the fixture's committed lock is what resolves.

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIXTURE="$REPO_ROOT/ci/downstream"
MANIFEST="$FIXTURE/Cargo.toml"
WORKSPACE_MANIFEST="$REPO_ROOT/Cargo.toml"
PLANNER="$REPO_ROOT/ci/downstream_pairs.py"
CARGO="${CARGO:-cargo}"

started="$SECONDS"

if [ ! -f "$MANIFEST" ]; then
  echo "FAIL: no consumer fixture at $MANIFEST" >&2
  exit 1
fi
# Any `python3` will do, and that is checked rather than hoped: `ci/downstream_pairs.py` is stdlib
# only and 3.9-safe, verified on `/usr/bin/python3` 3.9.6. The one gate in `ci/` with an interpreter
# floor is `miri_scope.py`, which needs 3.11 for `tomllib` and says so itself.
if ! command -v python3 >/dev/null 2>&1; then
  echo "FAIL: python3 is required to read cargo's JSON." >&2
  exit 1
fi

status=0
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# ── Certify the frame, and receive the legs ──────────────────────────────────────────────────
#
# A failure here is not a weaker signal than a failed build: it means the builds below would have
# been evidence about something other than the property.
echo "== certifying the pair list, the probes and the fixture's graph =="
if ! python3 "$PLANNER" plan "$WORKSPACE_MANIFEST" "$MANIFEST" \
     > "$tmp/plan" 2> "$tmp/certify.err"; then
  cat "$tmp/certify.err" >&2
  echo "FAIL: the gate's own frame is not certified, so its builds would prove nothing." >&2
  exit 1
fi
cat "$tmp/certify.err" >&2
if [ ! -s "$tmp/plan" ]; then
  echo "FAIL: the planner printed no legs." >&2
  exit 1
fi
echo

pass=0
fail=0
na=0

# `cargo check` and not `build`: the equivalence is a const assertion and the paths are `use`
# statements, so both are decided before codegen. Measured on this tree, the whole gate is 30-70s
# warm; `build` roughly triples it and decides nothing more.
run_leg() {
  local kind="$1" label="$2" features="$3" out="$tmp/${kind}-${label//\//_}"
  $CARGO check --locked --quiet --manifest-path "$MANIFEST" --no-default-features \
    --features "$features" --message-format=json > "$out.json" 2> "$out.err"
}

while IFS=$'\t' read -r kind label features; do
  if [ -z "${kind:-}" ] || [ -z "${label:-}" ] || [ -z "${features:-}" ]; then
    echo "FAIL: malformed leg from the planner: '$kind' '$label' '$features'" >&2
    status=1
    continue
  fi
  out="$tmp/${kind}-${label//\//_}"

  case "$kind" in
    EQ-POS|POS)
      run_leg "$kind" "$label" "$features"
      if [ $? -eq 0 ]; then
        pass=$((pass + 1))
      else
        echo "== $kind($label): --features $features"
        sed -n '1,30p' "$out.err" >&2
        python3 -c "
import json,sys
for line in open(sys.argv[1]):
    try: m=json.loads(line)
    except Exception: continue
    if m.get('reason')=='compiler-message' and m['message'].get('level')=='error':
        print(m['message'].get('rendered',''), file=sys.stderr)
" "$out.json" >&2
        echo "FAIL: $kind($label) must compile and did not." >&2
        if [ "$kind" = "POS" ]; then
          echo "      Enabling smear/<f> no longer makes the path it advertises reachable." >&2
        else
          echo "      A graph where smear and the member AGREE must build; the equivalence" >&2
          echo "      assertion is meant to reject disagreement only." >&2
        fi
        status=1; fail=$((fail + 1))
      fi
      ;;

    EQ-LEAK|LEAK|CTL)
      run_leg "$kind" "$label" "$features"
      if [ $? -eq 0 ]; then
        echo "== $kind($label): --features $features"
        if [ "$kind" = "CTL" ]; then
          echo "FAIL: the control COMPILED without smear/<f>." >&2
          echo "      The probe module has stopped naming the path it exists to name, so the" >&2
          echo "      positive leg beside it proves nothing. This is the vacuity the control" >&2
          echo "      exists for." >&2
        else
          echo "FAIL: THE LEAK IS BACK. A consumer that did not enable this smear feature built" >&2
          echo "      anyway, because a second dependency asked the member for it directly." >&2
          echo "      Either the equivalence assertion in smear/src/lib.rs lost this pair, or" >&2
          echo "      the member stopped publishing its __features constant." >&2
        fi
        status=1; fail=$((fail + 1))
      elif ! python3 "$PLANNER" judge "$kind" "$label" "$out.json"; then
        sed -n '1,20p' "$out.err" >&2
        echo "FAIL: $kind($label) failed, but not for the reason it must." >&2
        status=1; fail=$((fail + 1))
      else
        pass=$((pass + 1))
      fi
      ;;

    UNION)
      # Coexistence, plus the formatting and lints the root's own `--all`/`--workspace` commands
      # cannot reach past the `exclude`.
      echo "== UNION: every probe at once, formatted and linted"
      if ! $CARGO fmt --manifest-path "$MANIFEST" --all -- --check; then
        echo "FAIL: ci/downstream is not formatted." >&2
        status=1; fail=$((fail + 1))
      elif ! $CARGO clippy --locked --quiet --manifest-path "$MANIFEST" --no-default-features \
            --features "$features" --all-targets -- -D warnings; then
        echo "FAIL: ci/downstream does not pass clippy with every probe on." >&2
        status=1; fail=$((fail + 1))
      else
        pass=$((pass + 1))
      fi
      ;;

    EQ-LEAK-NA)
      # A pair the fixture cannot put into disagreement: the member's presence in the graph already
      # implies the feature, so there is no graph to build. Counted and printed rather than dropped,
      # because a silently skipped leg is a coverage cut nobody chose.
      na=$((na + 1))
      ;;

    *)
      echo "FAIL: unknown leg kind '$kind' — a leg this script cannot run went unchecked." >&2
      status=1
      ;;
  esac
done < "$tmp/plan"

echo
echo "── downstream: $pass legs as required, $fail wrong, $na not applicable, $((SECONDS - started))s"
if [ "$status" -ne 0 ]; then
  echo "FAIL: smear's features do not gate what they advertise in a graph with a second requester." >&2
fi
exit "$status"
