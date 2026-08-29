#!/usr/bin/env bash
#
# The two performance gates: this branch's peak allocation and wall clock against its merge-base's.
#
# ── THE ONE DECISION THIS SCRIPT IS ─────────────────────────────────────────────────────────
#
# Both sides are built and measured HERE, in one invocation, on one machine, with one toolchain.
# There is no committed baseline file, and adding one would break the gate rather than speed it up.
#
# A peak-byte figure is deterministic (see below) but it is deterministic GIVEN A BUILD. A rustc
# upgrade, an inlining decision, a `Vec`'s growth policy in std, a dependency bump — each moves it,
# by amounts far larger than the regression this gate exists to see. `tokora` is additionally a
# **git dependency on a moving branch** here, and this workspace commits no `Cargo.lock`, so two
# builds of the same smear commit can compile different tokora. A committed baseline would turn
# every one of those into a red gate for a change nobody made, and a gate that reds for reasons
# nobody caused is a gate that gets disabled. A same-run A/B cancels all of it, and the lock is
# resolved ONCE and copied to both sides so the two cannot disagree about tokora.
#
# ── AND THE TRICK THAT MAKES IT WORK ────────────────────────────────────────────────────────
#
# The merge-base does not contain this instrument: `benches/solo/peak_alloc.rs`,
# `benches/solo/wall_clock.rs` and their corpus are new on this branch, and on the next branch the
# workloads will have been edited. So the base side is NOT a checkout of the merge-base. It is THIS
# branch's tree with **every member's `src/` replaced by the merge-base's**: the same harness, the
# same fixtures, the same sizes, the same manifests, compiled against the older library. The
# instrument is the constant and the library is the variable, which is the only arrangement in
# which the difference means what it is read to mean.
#
# `smear/Cargo.toml` therefore comes from the HEAD side, not the base — it is where the two
# `[[bench]]` targets are declared, and a base-side manifest would declare neither. The
# consequence, stated because it is a real bound: if this branch changes a member's manifest in a
# way the base's `src/` cannot satisfy, the base side fails to BUILD and this script reds saying
# so. That is correct and is not a false positive — it means the two revisions are not comparable
# by this instrument.
#
# ── THE TWO GATES, AND WHY BOTH ─────────────────────────────────────────────────────────────
#
# **Peak allocation is the sharp one and the one this repository needs.** Every allocation defect
# found in this tree was invisible to a clock: `compute_closures` over the refused population at
# 3.17 GB, the `MissingInterfaceField` family at 6.03 GB, `flatten`'s possible-set table at 128 MB,
# the field-coverage name rendering at 406.91 MB. A wall-clock gate at any threshold a CI runner
# supports would have caught none of them.
#
# **Wall clock is the coarse second layer**, for the class the first is blind to: cache locality,
# branch prediction, data movement. Expect it to see roughly a 10% regression and no better — see
# the threshold derivation below, which is a measurement and not a preference.
#
# Both read the SAME workloads at TWO sizes and gate on the ratio as well as the absolute; see
# `smear/benches/solo/perf/mod.rs` for why that is the design and not a nicety.
#
# ── USAGE ───────────────────────────────────────────────────────────────────────────────────
#
#     ci/perf/run.sh <base-ref>   # compare HEAD against its merge-base with <base-ref>
#     ci/perf/run.sh --self       # compare HEAD against ITSELF: the gates' own noise floor
#
# `--self` is the thing to run first on a new host. It says what that machine's floor is before any
# number from it is trusted, and for the allocation gate it is also the reproducibility check: two
# builds of identical source, in two directories, into two target directories, measured
# independently.
#
# Environment: PERF_ALLOC_THRESHOLD, PERF_ALLOC_RATIO, PERF_WALL_THRESHOLD, PERF_WALL_RATIO,
# PERF_ROUNDS, PERF_INNER_ROUNDS, PERF_WORK, PERF_ONLY, PERF_LOCK.

set -euo pipefail

# ── The allocation threshold ────────────────────────────────────────────────────────────────
#
# THE CLAIM A CALLER RELIES ON: **this gate can see a 1% change in any workload's peak, at either
# size, and any movement in its per-doubling ratio.**
#
# IS THE READING REPRODUCIBLE? MEASURED, NOT ASSUMED. Three invocations of the same binary on
# `aarch64-apple-darwin`, release, produced **byte-identical JSON** — not "within noise", identical,
# across all nine workloads at both sizes. Nothing on these paths is hash-seeded or
# address-dependent in a way that reaches an allocation size, and a machine under load returns the
# same number as an idle one. `--self` re-checks the stronger form of the same claim on whatever
# host it runs on — two independent BUILDS of identical source rather than two runs of one binary —
# and the `perf-floor` job in `.github/workflows/perf.yml` runs it on every push to the trunk.
#
# So this number is not a noise allowance; there is no noise. It is the allowance for changes that
# legitimately move a peak without being a performance regression — a widened struct, an added
# match arm, a `Vec` that now reserves one more element. On the replayed population below the
# largest such incidental movement was **0.00%**: five recent merged commits, replayed against
# their own parents with this instrument on top, moved every workload by exactly nothing except
# where they moved the subject. 1% is therefore a wide margin over a measured zero, and it is set
# there rather than at zero for one reason — a legitimate representation change is a real category
# and reading its diff should not require an acceptance trailer for a rounding difference.
: "${PERF_ALLOC_THRESHOLD:=1.0}"

# The ratio gate, in ABSOLUTE units of the ratio itself rather than per cent. A workload that read
# 1.97 per doubling and now reads 2.07 has not changed law; one that reads 3.9 has. 0.15 is a
# fifteenth of the distance between linear and quadratic, and on the replayed population every row
# moved its ratio by less than 0.001.
: "${PERF_ALLOC_RATIO:=0.15}"

# ── The wall-clock threshold ────────────────────────────────────────────────────────────────
#
# Derived from a self-comparison on the host, not chosen. `--self` builds the identical source
# twice and interleaves the two binaries, so whatever it reports is the floor: two programs that do
# exactly the same thing, differing only in which of them the runner was kinder to.
#
# The number below is deliberately coarse and the header of `benches/solo/wall_clock.rs` says why
# at length. The short form: the sibling instrument in `tokora` measured 4.3-4.8% run-to-run on a
# DEDICATED machine and saw one run move nine unrelated benches +82% in lockstep, and a
# GitHub-hosted runner is a shared machine. **Do not read this gate as seeing better than about a
# 10% regression.** Re-derive it on the runner with `--self` — the `perf-floor` job does exactly
# that on every push to the trunk, and if that job's floor ever exceeds this number the honest
# response is to widen it or to retire the gate, not to re-run until it passes.
: "${PERF_WALL_THRESHOLD:=12.0}"
: "${PERF_WALL_RATIO:=0.40}"

# How many times the two wall-clock binaries are INTERLEAVED. Interleaving rather than running all
# of one side then all of the other is the half that matters: a runner whose throughput drifts over
# the job's lifetime otherwise charges the whole drift to whichever side ran second.
: "${PERF_ROUNDS:=4}"

# Batches inside one invocation, each reported as its own fastest. See `wall_clock.rs` on why the
# reduction is min and not mean.
: "${PERF_INNER_ROUNDS:=5}"

: "${PERF_WORK:=${RUNNER_TEMP:-/tmp}/smear-perf}"

# `alloc`, `wall`, or `both`. The allocation gate does not depend on the wall-clock one and is
# worth running alone on a host whose timing is not worth reading.
: "${PERF_ONLY:=both}"

repo="$(git rev-parse --show-toplevel)"
cd "$repo"

if [ "$#" -ne 1 ]; then
  # Both patterns are anchored at `^# ` so that THIS line — which contains the start pattern as
  # data — cannot re-open the range and print the rest of the script.
  sed -n '/^# ── USAGE/,/^# PERF_ROUNDS/p' "$0" | sed 's/^# \{0,1\}//'
  exit 2
fi

if [ "$1" = "--self" ]; then
  base="$(git rev-parse HEAD)"
  mode="self-comparison"
else
  base="$(git merge-base HEAD "$(git rev-parse "$1")")"
  mode="vs merge-base"
fi

head_sha="$(git rev-parse HEAD)"
echo "perf: head $head_sha"
echo "perf: base $base ($mode)"

rm -rf "$PERF_WORK/head" "$PERF_WORK/base"
mkdir -p "$PERF_WORK/head" "$PERF_WORK/base"

# `git archive` rather than `git worktree add`: it materialises a tree with no `.git`, no `target/`
# and no bookkeeping to clean up, and it takes the tree from the OBJECT rather than from the
# working directory — so an uncommitted edit cannot leak into a side.
git archive "$head_sha" | tar -x -C "$PERF_WORK/head"
git archive "$head_sha" | tar -x -C "$PERF_WORK/base"

# Every member's `src/`, from whichever side declares it. Derived from the two trees rather than
# listed here: a hard-coded list of crates is a list that stops matching the day a member is added,
# and stops SILENTLY — the new crate would simply be measured at the head's revision on both sides,
# which reads as "this change costs nothing" for the one crate the change is in.
src_dirs="$( { git ls-tree -r --name-only "$head_sha"; git ls-tree -r --name-only "$base"; } \
  | sed -n 's#^\([^/][^/]*\)/src/.*#\1/src#p' | sort -u )"
echo "perf: replacing these with the base's:"
printf '%s\n' "$src_dirs" | sed 's/^/perf:   /'

for dir in $src_dirs; do
  rm -rf "${PERF_WORK:?}/base/$dir"
  if git ls-tree -d --name-only "$base" -- "$dir" | grep -q .; then
    git archive "$base" -- "$dir" | tar -x -C "$PERF_WORK/base"
  else
    echo "perf: note: $dir does not exist at the base; the base side will not have it"
  fi
done

# ── ONE LOCK FOR BOTH SIDES, AND WHERE IT COMES FROM ────────────────────────────────────────
#
# `Cargo.lock` is gitignored here, so each side would otherwise resolve its own — and with `tokora`
# on `branch = "main"` two resolves seconds apart can pick different commits, which this gate would
# then read as a regression in smear. One lock is resolved and both sides get it; whether cargo had
# to re-resolve for the base manifest is PRINTED below rather than assumed either way.
#
# WHICH lock, in order of preference, and the middle one was added after it bit:
#
#   1. `$PERF_LOCK`, if the caller names one. This is how a run is made reproducible across days.
#   2. The repository's own `Cargo.lock`, if the working tree has one. A developer's tree has been
#      resolved by whatever they last built, and that is the tokora this branch was written
#      against. **MEASURED**: a replay run minutes after a green one failed to build the HEAD side
#      with 34 errors in `smear-parser`, because `cargo generate-lockfile` had moved `tokora` from
#      `3d5262a` to `79967b6` in between and the newer one had changed a bound. Nothing about smear
#      had changed. The root `Cargo.toml` records this exposure as a known unpaid cost of the git
#      edge; what it costs THIS gate is that a run can become unavailable for a reason on another
#      repository's `main`, and preferring an already-resolved lock is the cheap half of the repair.
#   3. A fresh resolve. This is what CI does, because the checkout has no lock — the same exposure
#      every other job in this repository already carries, and not one this script can fix.
if [ -n "${PERF_LOCK:-}" ] && [ -r "${PERF_LOCK}" ]; then
  echo "perf: using the caller's lock: $PERF_LOCK"
  cp "$PERF_LOCK" "$PERF_WORK/head/Cargo.lock"
elif [ -r "$repo/Cargo.lock" ]; then
  echo "perf: using the working tree's already-resolved Cargo.lock"
  cp "$repo/Cargo.lock" "$PERF_WORK/head/Cargo.lock"
else
  echo "perf: no lock to start from; resolving one"
fi
( cd "$PERF_WORK/head" && cargo generate-lockfile --quiet )
cp "$PERF_WORK/head/Cargo.lock" "$PERF_WORK/base/Cargo.lock"
grep -A2 '^name = "tokora"' "$PERF_WORK/head/Cargo.lock" | sed -n 's/^source = /perf: tokora /p' || true

for side in head base; do
  dir="$PERF_WORK/$side"
  # `--remap-path-prefix` so the two builds embed identical source paths. Panic locations and
  # `file!()` strings are baked into the binary, and two sides whose strings differ in LENGTH are
  # two binaries whose allocation of a panic message would differ for a reason nothing here
  # changed. Both map to the same name.
  #
  # `--features validator` and both `--bench` names are spelled out. Naming a target is LOUD when
  # its `required-features` are unmet (`exit 101, "requires the features: ..."`) where a filter
  # that matches nothing warns and exits 0; `smear/Cargo.toml`'s bench header carries the four
  # measured cases.
  ( cd "$dir" \
    && CARGO_TARGET_DIR="$PERF_WORK/target-$side" \
       RUSTFLAGS="${RUSTFLAGS:-} --remap-path-prefix=$dir=/smear" \
       cargo build -p smear --features validator \
         --bench peak_alloc --bench wall_clock \
         --profile bench --message-format=json-render-diagnostics \
         > "$PERF_WORK/$side.build.json" ) || {
    echo "::error::the $side side did not build."
    if [ "$side" = base ]; then
      echo "::error::That means this branch's instrument cannot be compiled against the"
      echo "::error::merge-base's library sources, so the two revisions are not comparable by it."
      echo "::error::Usually this is an API change under some member's \`src/\` that"
      echo "::error::\`benches/solo/perf/mod.rs\` names; the fix is in the instrument, not here."
    fi
    exit 1
  }
done

if cmp -s "$PERF_WORK/head/Cargo.lock" "$PERF_WORK/base/Cargo.lock"; then
  echo "perf: lock parity ok — both sides resolved the same dependency graph"
else
  echo "perf: the two sides' locks DIFFER; cargo had to re-resolve for the base manifests:"
  diff -u "$PERF_WORK/base/Cargo.lock" "$PERF_WORK/head/Cargo.lock" \
    | grep -E '^[-+](name|version|source)' | sort -u | sed 's/^/perf:   /' || true
  echo "perf: a delta below may therefore be a dependency's, not this branch's."
fi

alloc_head="$(python3 ci/perf/exe_path.py "$PERF_WORK/head.build.json" peak_alloc)"
alloc_base="$(python3 ci/perf/exe_path.py "$PERF_WORK/base.build.json" peak_alloc)"
wall_head="$(python3 ci/perf/exe_path.py "$PERF_WORK/head.build.json" wall_clock)"
wall_base="$(python3 ci/perf/exe_path.py "$PERF_WORK/base.build.json" wall_clock)"

# The acceptances, harvested from THIS BRANCH'S OWN COMMITS. That is what makes an acceptance
# one-shot: `$base..HEAD` is this change and nothing else, so a trailer licenses the commits it
# travels with and is gone from the next branch's range. A file checked into the tree would instead
# sit there licensing every future drift in the same workload, which — against a merge-base
# comparison that resets each time — is an unbounded allowance written as a bounded one. The
# spelling is `tokora`'s, deliberately: one convention across the two repositories, not two.
git log --format=%B "$base..HEAD" | grep -E '^\s*Perf-accept:' > "$PERF_WORK/accept.txt" || true
if [ -s "$PERF_WORK/accept.txt" ]; then
  echo "perf: this branch carries acceptances:"
  sed 's/^/perf:   /' "$PERF_WORK/accept.txt"
fi

status=0

if [ "$PERF_ONLY" != "wall" ]; then
  echo
  echo "perf: === peak allocation ==="

  # The head side is read TWICE before anything is compared. A gate whose threshold is a fraction
  # of a per cent is only honest if the instrument repeats, and the cheapest possible statement of
  # that is two readings of the same binary — which costs half a second and is the difference
  # between "the reading did not move" and "the reading is capable of not moving".
  "$alloc_head" --json "$PERF_WORK/head.alloc.json" > "$PERF_WORK/head.alloc.log"
  "$alloc_head" --json "$PERF_WORK/head.alloc.repeat.json" > /dev/null
  if cmp -s "$PERF_WORK/head.alloc.json" "$PERF_WORK/head.alloc.repeat.json"; then
    echo "perf: peak bytes is bit-reproducible on this host (two readings, identical)"
  else
    echo "::warning::peak bytes is NOT bit-reproducible on this host. The threshold below was"
    echo "::warning::derived on the assumption that it is; find out what is order- or hash-seeded"
    echo "::warning::on these paths before trusting a delta smaller than the spread. Diff:"
    diff -u "$PERF_WORK/head.alloc.json" "$PERF_WORK/head.alloc.repeat.json" | sed 's/^/perf:   /' || true
  fi

  "$alloc_base" --json "$PERF_WORK/base.alloc.json" > "$PERF_WORK/base.alloc.log"

  python3 ci/perf/compare.py \
    --kind alloc \
    --base "$PERF_WORK/base.alloc.json" \
    --head "$PERF_WORK/head.alloc.json" \
    --threshold "$PERF_ALLOC_THRESHOLD" \
    --ratio-tolerance "$PERF_ALLOC_RATIO" \
    --accept-file "$PERF_WORK/accept.txt" \
    ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"} || status=1
fi

if [ "$PERF_ONLY" != "alloc" ]; then
  echo
  echo "perf: === wall clock ==="

  base_files=""
  head_files=""
  for round in $(seq 1 "$PERF_ROUNDS"); do
    # Interleaved, base then head, one round at a time. See the header.
    "$wall_base" --rounds "$PERF_INNER_ROUNDS" --json "$PERF_WORK/base.wall.$round.json" > /dev/null
    "$wall_head" --rounds "$PERF_INNER_ROUNDS" --json "$PERF_WORK/head.wall.$round.json" > /dev/null
    base_files="${base_files:+$base_files,}$PERF_WORK/base.wall.$round.json"
    head_files="${head_files:+$head_files,}$PERF_WORK/head.wall.$round.json"
  done

  python3 ci/perf/compare.py \
    --kind wall \
    --base "$base_files" \
    --head "$head_files" \
    --threshold "$PERF_WALL_THRESHOLD" \
    --ratio-tolerance "$PERF_WALL_RATIO" \
    --accept-file "$PERF_WORK/accept.txt" \
    ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"} || status=1
fi

exit "$status"
