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
# by amounts far larger than the regression this gate exists to see. This workspace commits no
# `Cargo.lock`, so two builds of the same smear commit can compile different releases of a
# dependency (`tokora` was a git dependency on a moving branch when this was written, which made
# the gap seconds; #209 moved it to the registry). A committed baseline would turn every one of
# those into a red gate for a change nobody made, and a gate that reds for reasons nobody caused is
# a gate that gets disabled. A same-run A/B cancels all of it, and both sides start from ONE lock,
# so they differ in a dependency only where their own manifests ask for different ones.
#
# ── WHAT EACH SIDE IS ───────────────────────────────────────────────────────────────────────
#
# The HEAD side is the head's tree. The BASE side is the MERGE-BASE'S OWN TREE — its manifests and
# its `src/`, so its features, its defaults and the dependency versions it asks for — with one thing
# laid over it from the head: the INSTRUMENT. That is the two bench targets this script builds and
# every file they read at compile time: their crate roots, the modules those declare (`perf/`, and
# `tests/support/counting.rs` through `#[path]`) and the corpus fixtures `perf/mod.rs` reaches
# through `include_str!`. Nothing here lists those files. `sides.py instrument` derives the set from
# the head's bench sources on every run, and it refuses — the run stops and says where — on any way
# of reaching a file it does not follow. A list would go stale silently: a fixture added to the
# corpus and missing from the list would be read from the merge-base's tree on one side and from the
# head's on the other, and the difference would be reported as the library's. The log names every
# file laid over and whether the merge-base's own copy differed from it.
#
# So the instrument is the constant and the REVISION is the variable — sources, manifests, features
# and dependencies together, which is what a pull request is. Until #272 the base side was the
# head's tree with each member's `src/` replaced by the merge-base's: the merge-base's sources,
# compiled under the HEAD'S manifests. That measured a library the base branch never shipped
# whenever the head changed a manifest in a way the old sources could still satisfy, and #272 was
# the case that showed it. It renamed the feature `smallvec` to `smallvec_1`; the merge-base's
# lexers test `cfg(feature = "smallvec")`, so on the base side that cfg went off, silently — the
# build was green and the two locks were identical — and the parse family read 10 to 22% slower
# than a base built in a configuration nobody ships. The same arrangement could not build the base
# side at all once the head added a workspace member (#273: the member's manifest stayed and its
# `src/` did not).
#
# WHAT IT SEES: a change to any member's sources; a manifest change that moves a feature, a default
# or a dependency requirement, because each side resolves its own; a member added or removed. The
# log prints the features each side compiled its own packages with, and the packages the two locks
# resolved differently, so a delta that a dependency or a feature explains can be read as such.
#
# WHAT IT DOES NOT SEE, by construction: a change to the instrument itself, because both sides run
# the head's, which is what makes their readings comparable at all (`compare.py` refuses two
# different workload sets); a change to the build profile, because both sides are compiled with the
# one pinned below, so a profile edit is neither measured nor allowed to differ between them; and
# RUSTFLAGS, the toolchain and the runner, which are one for both by being one run.
#
# WHEN IT DECLINES. The run measures nothing, exits 1, and says in its last line and at the top of
# the job's summary that the two revisions are not comparable by this instrument, when: the
# merge-base does not declare the two bench targets, or declares them at another path (the head's
# manifest is never borrowed to give it them); the two trees declare a build-profile setting the pin
# cannot override; or the base side does not build, which usually means the head's instrument does
# not compile against the merge-base's library. Each is a statement about the two revisions and the
# instrument, not a performance finding.
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
# It is not a round number picked for comfort. Four populations were measured on
# `aarch64-apple-darwin`, release, before it was chosen.
#
#   1. **IS THE READING REPRODUCIBLE AT ALL? MEASURED, NOT ASSUMED.** Three invocations of the same
#      binary produced **byte-identical JSON** — not "within noise", identical, across all nine
#      workloads at both sizes. Then the stronger form: `--self` builds the identical source TWICE,
#      in two directories, into two target directories, and measures both, and all nine came back
#      at **exactly +0.000% at both sizes with the ratio unchanged to three decimals**. Nothing on
#      these paths is hash-seeded or address-dependent in a way that reaches an allocation size,
#      and a machine under load returns the same number as an idle one — the property the criterion
#      benches in this repository do not have and cannot be given. `run.sh` re-checks the cheap
#      half of this on every run (below) and the `perf-floor` job re-checks the whole of it on
#      every push to the trunk.
#
#   2. **A prose-only change.** A commit adding one `///` line to `SchemaBuilder::compute_closures`
#      and one `//` comment inside its hottest loop: all nine at **+0.000%**, both sizes, ratio
#      unmoved. (The same commit read -4.63% to +2.46% on the wall-clock gate, which is that gate's
#      floor and not a finding.)
#
#   3. **Real merged commits**, replayed against their own parents with this instrument on top —
#      the population this gate will actually meet.
#
#      * `fcd7f5e` (#206, `fix(schema)!`, the refused population's closure walk) moved
#        `schema_iface_mismatch` by **-72.12% / -92.25%** and `schema_transitive` by
#        **-48.55% / -85.01%**, and took both of their ratios from **~3.9 to ~1.1**. That is this
#        repository's own repair, retro-detected, and it is the strongest available statement that
#        the two ceiling-shaped workloads read what they were built to read. Every other row was
#        `+0.000%`; the largest INCIDENTAL movement the commit produced anywhere was **0.025%**, on
#        `schema_real` at the small size, with its ratio moving 1.360 -> 1.361.
#      * `0168942` (#202, `fix!`, eight walks over one structure) moved `schema_valid` by
#        **-3.12% / -6.17%** — an improvement, in the axis it changed — and left **all eight other
#        workloads at exactly +0.000%**.
#
#      A BOUND ON THAT POPULATION, recorded rather than glossed. It is two commits and not five,
#      because the replay is limited by the `tokora` git edge: `8b73965` (#198) and everything
#      before `bb279ea` (#203) needs an OLDER tokora than any current pin, and the base side fails
#      to compile with `E0308` on `tokora::types::recovery::Components`. This script reports that
#      as "the two revisions are not comparable by this instrument" and declines to answer, which
#      is the correct behaviour and is why the number below is not derived downward from a
#      population of two.
#
#   4. **A planted regression, at the shape the ceiling exists against.** Setting
#      `MAX_INTERFACE_IMPLEMENTATION_MISMATCHES` to `usize::MAX` — the state before the repair —
#      read `schema_iface_mismatch` at **+258.56% / +1190.72%**, ratio **1.099 -> 3.955**, with
#      every other workload at `+0.000%` and the `parse` control flat, so the harness said by
#      itself that the cost was inside the builder. And the converse plant, a linear scan in front
#      of `SchemaBuilder::type_index`, is **invisible here** at +0.000% on every row while the
#      wall-clock gate reads it at +26.66%: that pair is the whole argument for running two gates
#      rather than one.
#
# So the margin is not for noise; there is none. It is for the changes that legitimately move a
# peak without being a performance regression — a widened struct, an added match arm, a `Vec` that
# reserves one more element — and population 3 says those cost hundredths of a per cent, not one.
: "${PERF_ALLOC_THRESHOLD:=1.0}"

# The ratio gate, in ABSOLUTE units of the ratio itself rather than per cent. A workload that read
# 1.97 per doubling and now reads 2.07 has not changed law; one that reads 3.9 has. Linear is 2 and
# quadratic is 4, so 0.15 is under a tenth of the distance between them — and on the replayed
# population above, every row that was NOT the commit's subject moved its ratio by less than 0.002.
# The subjects moved theirs by 2.8.
: "${PERF_ALLOC_RATIO:=0.15}"

# ── The wall-clock threshold ────────────────────────────────────────────────────────────────
#
# Derived from self-comparisons, not chosen. `--self` builds the identical source twice and
# interleaves the two binaries, so whatever it reports is the floor: two programs that do exactly
# the same thing, differing only in which of them the machine was kinder to.
#
# Measured on `aarch64-apple-darwin`, four interleaved rounds of five batches each, on a machine
# carrying ~40 load average from unrelated work:
#
#   * `--self`, identical source both sides: **-1.88% to +2.19%**, worst ratio movement +0.077.
#   * the prose-only commit of population 2 above: **-4.63% to +2.46%**, worst ratio movement
#     +0.049.
#   * this branch against its own merge-base, where the allocation gate reads +0.000% on every row
#     and there is by construction nothing for the clock to find: **-8.16% to +6.41%**. That run is
#     the one that matters most for the number below, and it is the reason the promotion criterion
#     in `.github/workflows/perf.yml` is written as it is — **+6.41% is more than half of 12%, so
#     on this evidence the wall-clock gate does NOT qualify for promotion and should not be made
#     required.** It is recorded here rather than dropped because a floor measured on a quiet
#     minute is not the floor.
#   * two source plants that the allocation gate reads at exactly zero: a lost inline on the
#     lexer's per-byte identifier predicate and a lost SIMD identifier skip, **both under +3.2%** —
#     which is a finding about where this corpus spends its time, not about the gate.
#   * the plant that does move: a linear scan in front of `SchemaBuilder::type_index`, at
#     **+8.99% / +26.66%** on `schema_iface_mismatch` and **+5.54% / +18.04%** on
#     `schema_transitive`, with the `parse` control family flat.
#
# **THESE NUMBERS ARE A FLOOR OBSERVATION AND NOT A RUNNER CALIBRATION, and the difference decides
# how the threshold is set.** They were taken on a developer's machine; the gate runs on a shared
# GitHub-hosted runner, where the sibling instrument in `tokora` measured 4.3-4.8% run-to-run on a
# DEDICATED machine and saw one run move nine unrelated benches +82% in lockstep. So the number
# below is deliberately far above the local floor. **Do not read this gate as seeing better than
# about a 10% regression.** The `perf-floor` job re-derives the real floor with `--self` on every
# push to the trunk, and if that job's floor ever approaches this number the honest response is to
# widen it or to retire the gate, not to re-run until it passes.
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

# ── THE INSTRUMENT, AS CARGO KNOWS IT ───────────────────────────────────────────────────────
#
# Two bench targets of one package, built with the feature their `required-features` names. The
# merge-base must declare both, at the same paths; every file they read is derived from their
# sources (see the header), so nothing in this script names one.
perf_package=smear
perf_features=validator
perf_targets=(peak_alloc wall_clock)

# ── THE BUILD PROFILE, PINNED FOR BOTH SIDES ────────────────────────────────────────────────
#
# Each side brings its own manifests, so each would also bring its own `[profile.*]`, and a pull
# request that edited the profile would be measured against a base compiled another way: a
# difference in how the code was compiled, read as a difference in the code. So the profile is
# taken from neither tree. Every setting below reaches both builds as `CARGO_PROFILE_BENCH_<KEY>`,
# which cargo ranks above `.cargo/config.toml` and both above `Cargo.toml`, for `[profile.bench]`
# and for the `[profile.release]` it inherits from alike; and `CARGO_INCREMENTAL=0` because that
# variable, when a caller's environment sets it, overrides the profile's `incremental`.
#
# THE VALUES ARE TODAY'S, NOT A CHOICE. They are cargo's built-in `release` settings, which `bench`
# inherits, and they are what this job has always compiled with: no manifest in this workspace
# declares a `[profile.*]` table and there is no `.cargo/config.toml`. Pinning them changes no
# number this job reports. Whether the gate should compile another way — one codegen unit, say, for
# a narrower floor — is a separate decision, and one that needs its own `perf-floor` runs.
#
# Pinned: every key that can change the code rustc generates — optimisation, codegen partitioning,
# LTO, debug info, the two runtime checks, unwinding, incremental compilation. Not pinned: `strip`,
# `split-debuginfo` and `rpath`, which change only what is written into the executable beside that
# code, and whose defaults depend on the platform and the cargo release, so a guessed value could
# change today's binary. What the environment cannot pin at all is a per-package table,
# `[profile.<name>.package.<pkg>]`, for which cargo reads no variable. Both gaps are closed the same
# way: `sides.py profile` compares the two trees' declarations before anything is built, prints any
# difference, and stops the run as not comparable when the difference is not in a pinned key.
profile_pin=(
  opt-level=3
  codegen-units=16
  lto=false
  debug=false
  debug-assertions=false
  overflow-checks=false
  panic=unwind
  incremental=false
)

perf_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(git rev-parse --show-toplevel)"
cd "$repo"

# `opt-level=3` becomes `CARGO_PROFILE_BENCH_OPT_LEVEL=3` for the two builds and the key `opt-level`
# for `sides.py profile`, so the pin and the check of what it cannot reach read one list.
pin_env=(CARGO_INCREMENTAL=0)
pin_keys=()
for setting in "${profile_pin[@]}"; do
  key="${setting%%=*}"
  pin_keys+=("$key")
  key="${key//-/_}"
  pin_env+=("CARGO_PROFILE_BENCH_$(printf '%s' "$key" | tr '[:lower:]' '[:upper:]')=${setting#*=}")
done
bench_args=()
for target in "${perf_targets[@]}"; do
  bench_args+=(--bench "$target")
done

# ── WHAT THE RUN SAYS LAST ──────────────────────────────────────────────────────────────────
#
# One line, whatever happened: what was measured and how it read, or why nothing was. It is the
# last line of the log and the first of the job's summary, and it exists because the job is
# `continue-on-error`: its run concludes success whatever this script does, so a base side that did
# not build — every run #273 had — read as a green run with no table, and nothing on the summary
# page said that nothing had been compared. Each stage sets it to what stopping there would mean.
outcome="NOT MEASURED: the run stopped before it compared anything; the log above says where"

headline() {
  local code=$?
  echo
  if [ "$code" -eq 0 ]; then
    echo "perf: $outcome"
  else
    echo "::error::perf: $outcome"
  fi
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      printf '**perf: %s**\n\n' "$outcome"
      if [ -s "$PERF_WORK/summary.md" ]; then
        cat "$PERF_WORK/summary.md"
      fi
    } >> "$GITHUB_STEP_SUMMARY"
  fi
}

# Stop without measuring. `not_comparable` is the instrument declining to answer, because the two
# revisions cannot be read by one instrument; `stop` is anything else that leaves nothing to read.
not_comparable() {
  outcome="NOT MEASURED: the two revisions are not comparable by this instrument: $1"
  exit 1
}

stop() {
  outcome="NOT MEASURED: $1"
  exit 1
}

# ── THE TWO TREES ───────────────────────────────────────────────────────────────────────────
#
# `materialise <head-sha> <base-sha>` puts the head's tree in `$PERF_WORK/head` and the
# merge-base's in `$PERF_WORK/base`, checks that one instrument can read both, and lays the head's
# instrument over the base. Nothing in it compiles, which is what lets a harness source this file
# and call it.
materialise() {
  local head_sha="$1" base_sha="$2" side file note status=0

  rm -rf "$PERF_WORK/head" "$PERF_WORK/base"
  mkdir -p "$PERF_WORK/head" "$PERF_WORK/base"

  # `git archive` rather than `git worktree add`: it materialises a tree with no `.git`, no
  # `target/` and no bookkeeping to clean up, and it takes the tree from the OBJECT rather than
  # from the working directory — so an uncommitted edit cannot leak into a side.
  git archive "$head_sha" | tar -x -C "$PERF_WORK/head"
  git archive "$base_sha" | tar -x -C "$PERF_WORK/base"

  # What each side's manifests declare, as cargo reads them. `--no-deps` resolves nothing.
  for side in head base; do
    if ! ( cd "$PERF_WORK/$side" && cargo metadata --no-deps --format-version 1 ) \
      > "$PERF_WORK/$side.metadata.json"; then
      if [ "$side" = base ]; then
        not_comparable "cargo cannot read the merge-base's manifests"
      fi
      stop "cargo cannot read the head's manifests"
    fi
  done

  # The instrument. Refused with status 3 when the merge-base does not declare the two targets
  # where the head does, and with 1 when the head's sources reach a file in a way not followed.
  python3 "$perf_dir/sides.py" instrument \
    "$PERF_WORK/head" "$PERF_WORK/head.metadata.json" \
    "$PERF_WORK/base" "$PERF_WORK/base.metadata.json" \
    "$perf_package" "${perf_targets[@]}" > "$PERF_WORK/instrument.txt" || status=$?
  if [ "$status" -ne 0 ]; then
    cat "$PERF_WORK/instrument.txt"
    if [ "$status" -eq 3 ]; then
      not_comparable "the merge-base does not declare the two bench targets where the head does"
    fi
    stop "the instrument's files could not be derived from the head's bench sources"
  fi

  echo "perf: the base side is the merge-base's own tree, and laid over it from the head:"
  while IFS= read -r file; do
    if [ ! -e "$PERF_WORK/base/$file" ]; then
      note="added    "
    elif cmp -s "$PERF_WORK/head/$file" "$PERF_WORK/base/$file"; then
      note="identical"
    else
      note="replaced "
    fi
    mkdir -p "$PERF_WORK/base/$(dirname "$file")"
    cp "$PERF_WORK/head/$file" "$PERF_WORK/base/$file"
    echo "perf:   $note  $file"
  done < "$PERF_WORK/instrument.txt"

  # The profile. A difference between the two trees that the pin does not override stops the run.
  status=0
  python3 "$perf_dir/sides.py" profile "$PERF_WORK/base" "$PERF_WORK/head" "${pin_keys[@]}" \
    || status=$?
  if [ "$status" -eq 3 ]; then
    not_comparable "the two trees declare a build-profile setting the pin cannot override"
  elif [ "$status" -ne 0 ]; then
    stop "the two trees' profile declarations could not be read"
  fi
}

# ── ONE LOCK TO START FROM, AND WHERE IT COMES FROM ─────────────────────────────────────────
#
# `Cargo.lock` is gitignored here, so two sides resolved independently would each take whatever
# release of every dependency was newest at that moment, and a release published between the two
# resolves would be read by this gate as a change in smear. So both start from ONE lock: it is
# completed for the head's manifests, copied to the base side, and there cargo re-resolves only
# what the merge-base's own manifests ask for differently. What is left different between the two
# locks is therefore what the two revisions' manifests ask for — a dependency bumped, added or
# dropped, a member added — and that is part of what a pull request is, so it is PRINTED, package
# by package, and a delta below may be its. Both resolutions are `cargo metadata`, which resolves
# as a build does, writes the lock and compiles nothing, and both builds then run `--locked`: the
# locks printed are the locks built.
#
# WHICH lock the head starts from, in order of preference, and the middle one was added after it
# bit:
#
#   1. `$PERF_LOCK`, if the caller names one. This is how a run is made reproducible across days.
#   2. The repository's own `Cargo.lock`, if the working tree has one. A developer's tree has been
#      resolved by whatever they last built, and that is the dependency set this branch was written
#      against. **MEASURED** while `tokora` was a git dependency: a replay run minutes after a green
#      one failed to build the HEAD side with 34 errors in `smear-parser`, because
#      `cargo generate-lockfile` had moved `tokora` from `3d5262a` to `79967b6` in between and the
#      newer one had changed a bound. Nothing about smear had changed. A registry release can do the
#      same, only more slowly; what it costs THIS gate is that a run can become unavailable for a
#      reason in another repository, and preferring an already-resolved lock is the cheap half of
#      the repair.
#   3. A fresh resolve. This is what CI does, because the checkout has no lock — the same exposure
#      every other job in this repository already carries, and not one this script can fix.
#
# `cargo generate-lockfile` is run ONLY when there is nothing to start from, and that is not a
# micro-optimisation. Its documented behaviour is to rebuild an existing lock "with the latest
# available version of every package", so calling it after copying a good lock in undoes the copy.
# That is exactly how the failure above reproduced a second time.
resolve_locks() {
  local host
  host="$(rustc -vV | sed -n 's/^host: //p')"
  if [ -n "${PERF_LOCK:-}" ] && [ -r "${PERF_LOCK}" ]; then
    echo "perf: using the caller's lock: $PERF_LOCK"
    cp "$PERF_LOCK" "$PERF_WORK/head/Cargo.lock"
  elif [ -r "$repo/Cargo.lock" ]; then
    echo "perf: using the working tree's already-resolved Cargo.lock"
    cp "$repo/Cargo.lock" "$PERF_WORK/head/Cargo.lock"
  else
    echo "perf: no lock to start from; resolving one"
    ( cd "$PERF_WORK/head" && cargo generate-lockfile --quiet ) \
      || stop "no lock could be resolved for the head's manifests"
  fi
  # `--filter-platform` only spares downloading crates for other targets: a lock is one for all.
  ( cd "$PERF_WORK/head" \
    && cargo metadata --format-version 1 --filter-platform "$host" > /dev/null ) \
    || stop "the head's dependencies could not be resolved"
  cp "$PERF_WORK/head/Cargo.lock" "$PERF_WORK/base/Cargo.lock"
  ( cd "$PERF_WORK/base" \
    && cargo metadata --format-version 1 --filter-platform "$host" > /dev/null ) \
    || stop "the merge-base's dependencies could not be resolved from the head's lock"
  grep -A2 '^name = "tokora"' "$PERF_WORK/head/Cargo.lock" \
    | sed -n 's/^source = /perf: tokora /p' || true
  python3 "$perf_dir/sides.py" lock "$PERF_WORK/base/Cargo.lock" "$PERF_WORK/head/Cargo.lock"
}

# `build_side head|base`, each into its own target directory, with the pinned profile.
#
# `--remap-path-prefix` so the two builds embed identical source paths. Panic locations and
# `file!()` strings are baked into the binary, and two sides whose strings differ in LENGTH are two
# binaries whose allocation of a panic message would differ for a reason nothing here changed. Both
# map to the same name.
#
# `--features validator` and both `--bench` names are spelled out. Naming a target is LOUD when its
# `required-features` are unmet (`exit 101, "requires the features: ..."`) where a filter that
# matches nothing warns and exits 0; `smear/Cargo.toml`'s bench header carries the four measured
# cases.
build_side() {
  local dir="$PERF_WORK/$1"
  ( cd "$dir" \
    && env CARGO_TARGET_DIR="$PERF_WORK/target-$1" \
       RUSTFLAGS="${RUSTFLAGS:-} --remap-path-prefix=$dir=/smear" \
       "${pin_env[@]}" \
       cargo build --locked -p "$perf_package" --features "$perf_features" "${bench_args[@]}" \
         --profile bench --message-format=json-render-diagnostics \
         > "$PERF_WORK/$1.build.json" )
}

# Sourced rather than run — by a harness that wants the two trees, the profile check and the locks
# without a build — the file stops here, having defined its stages and run none of them.
if [ "${BASH_SOURCE[0]}" != "$0" ]; then
  return 0
fi

if [ "$#" -ne 1 ]; then
  # Both patterns are anchored at `^# ` so that THIS line — which contains the start pattern as
  # data — cannot re-open the range and print the rest of the script.
  sed -n '/^# ── USAGE/,/^# PERF_ROUNDS/p' "$0" | sed 's/^# \{0,1\}//'
  exit 2
fi

mkdir -p "$PERF_WORK"
rm -f "$PERF_WORK/summary.md"
trap headline EXIT

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

materialise "$head_sha" "$base"
echo "perf: the build profile, pinned for both sides: ${pin_env[*]}"
resolve_locks

outcome="NOT MEASURED: the head side did not build"
build_side head || {
  echo "::error::the head side did not build."
  # A HEAD-side failure is not a perf finding and should not be read as one. The tree does not
  # compile, so every other job in this repository is failing too and this one has nothing to
  # measure. Saying which of the two it is matters, because they license opposite responses:
  # a base-side failure is fixed in the instrument, a head-side failure is fixed in the tree.
  echo "::error::That is a statement about THIS TREE, not about performance: the branch does"
  echo "::error::not compile, so there is nothing to measure and every other job is red too."
  echo "::error::One cause is specific to this repository and worth checking first: no"
  echo "::error::\`Cargo.lock\` is committed, so a build here can pick up a dependency release this"
  echo "::error::tree has never compiled against. The lock this run used is printed above;"
  echo "::error::\`PERF_LOCK\` pins it."
  exit 1
}

outcome="NOT MEASURED: the base side did not build"
outcome="$outcome, so the two revisions are not comparable by this instrument"
build_side base || {
  echo "::error::the base side did not build."
  echo "::error::It is the merge-base's own tree with this branch's instrument laid over it, so"
  echo "::error::the instrument cannot be compiled against the merge-base's library and the two"
  echo "::error::revisions are not comparable by it. Usually this is an API change under some"
  echo "::error::member's \`src/\` that \`benches/solo/perf/mod.rs\` names; the fix is in the"
  echo "::error::instrument, not here."
  exit 1
}

python3 "$perf_dir/sides.py" features \
  "$PERF_WORK/base" "$PERF_WORK/base.build.json" "$PERF_WORK/head" "$PERF_WORK/head.build.json"

outcome="NOT MEASURED: both sides built, but a binary was not found or did not run to the end"
alloc_head="$(python3 "$perf_dir/exe_path.py" "$PERF_WORK/head.build.json" peak_alloc)"
alloc_base="$(python3 "$perf_dir/exe_path.py" "$PERF_WORK/base.build.json" peak_alloc)"
wall_head="$(python3 "$perf_dir/exe_path.py" "$PERF_WORK/head.build.json" wall_clock)"
wall_base="$(python3 "$perf_dir/exe_path.py" "$PERF_WORK/base.build.json" wall_clock)"

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

  python3 "$perf_dir/compare.py" \
    --kind alloc \
    --base "$PERF_WORK/base.alloc.json" \
    --head "$PERF_WORK/head.alloc.json" \
    --threshold "$PERF_ALLOC_THRESHOLD" \
    --ratio-tolerance "$PERF_ALLOC_RATIO" \
    --accept-file "$PERF_WORK/accept.txt" \
    --summary "$PERF_WORK/summary.md" || status=1
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

  python3 "$perf_dir/compare.py" \
    --kind wall \
    --base "$base_files" \
    --head "$head_files" \
    --threshold "$PERF_WALL_THRESHOLD" \
    --ratio-tolerance "$PERF_WALL_RATIO" \
    --accept-file "$PERF_WORK/accept.txt" \
    --summary "$PERF_WORK/summary.md" || status=1
fi

outcome="measured ${head_sha:0:12} against ${base:0:12} ($mode)"
if [ "$status" -eq 0 ]; then
  outcome="$outcome: no workload over its threshold"
else
  outcome="$outcome: a gate failed, and the tables and errors above say which"
fi
exit "$status"
