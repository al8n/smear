#!/usr/bin/env bash
#
# The performance gates: this branch's peak allocation, wall clock and instruction counts against
# its merge-base's.
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
# file laid over and whether the merge-base's own copy differed from it. With `--against` (see
# USAGE) the base side is the named revision's own tree instead, built the same way.
#
# So the instrument is the constant and the REVISION is the variable — sources, manifests, features
# and dependencies together, which is what a pull request is. Until #272 the base side was the
# head's tree with each member's `src/` replaced by the merge-base's: the merge-base's sources,
# compiled under the HEAD'S manifests. That measured a library the base branch never shipped
# whenever the head changed a manifest in a way the old sources could still satisfy, and #272 was
# the case that showed it. It renamed the feature `smallvec` to `smallvec_1`; the merge-base's
# lexers tested `cfg(feature = "smallvec")`, so on the base side that cfg went off, silently — the
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
# A side must also compile its own packages in the run that measures it, because a side that cargo
# takes for fresh is measured from whatever its target directory held before, so `sides.py
# compiled` stops the run the same way, with nothing measured, when any of a side's own artifacts or
# either bench target comes back fresh.
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
# ── THE THIRD READING: INSTRUCTION COUNTS ───────────────────────────────────────────────────
#
# **A count is exact, like the allocation figure, and sees what the clock sees.** callgrind counts
# what a binary executes, which is a property of the binary and not of the runner, so a count reads
# a lost inline, a longer path through the lexer or a loop that does more work, without the clock's
# spread: about a per cent, not ten. It reads the same workloads at the same two sizes, out of
# `wall_clock`'s count mode, so the ratio is gated here too: `ci/perf/icount.py` runs each row at
# each size at n, 2n and 3n iterations under callgrind, and the difference of two totals is the
# cost of n. It is the last stage, after the wall clock, so that its runs, which go in parallel,
# cannot load a timed one; `PERF_ONLY` names it `icount`. It needs valgrind, which runs on Linux and
# not on macOS, and in CI it runs alone, in a job of its own on two Linux images —
# `.github/workflows/perf.yml` says why.
#
# EVERY WAY THE STAGE CAN FAIL TO READ A ROW IS A REFUSAL, NOT A NUMBER. The stage reads NOT
# MEASURED, and the table and the last line say on which side and why, when:
#
#   * valgrind is absent — and a run that asked for counts alone stops before it builds anything;
#   * a callgrind run on either side exits non-zero, or outlives `PERF_ICOUNT_TIMEOUT`;
#   * a row's two runs at n differ, or its count does not grow from n to 2n and from 2n to 3n;
#   * the two halves of a row's reading disagree by more than `PERF_ICOUNT_SPREAD` of the finest
#     line it is read against, and still do when the row is read again at four times n (see that
#     setting);
#   * the two sides were read at different n.
#
# A row so refused fails the stage and stays in the table. Dropped, it would be a row the gate had
# stopped watching without anyone deciding that it should.
#
# ── USAGE ───────────────────────────────────────────────────────────────────────────────────
#
#     ci/perf/run.sh <base-ref>         # compare HEAD against its merge-base with <base-ref>
#     ci/perf/run.sh --against <rev>    # compare HEAD against <rev> ITSELF, not a merge-base
#     ci/perf/run.sh --self             # compare HEAD against ITSELF: the gates' own noise floor
#
# `--against` asks what no pull request asks: this tree against a revision as it is. CI runs it
# for a dispatch of `perf.yml` with the input `base`, `gh workflow run perf.yml --ref <branch> -f
# base=<rev>`; <rev> is a hash, a tag or a branch, tried as given and then as `origin/<rev>`.
#
# `--self` is the thing to run first on a new host. It says what that machine's floor is before any
# number from it is trusted, and for the allocation gate it is also the reproducibility check: two
# builds of identical source, in two directories, into two target directories, measured
# independently.
#
# `PERF_ONLY=icount ci/perf/run.sh --self` reads the counts alone, on a Linux host with valgrind.
#
# Environment: PERF_ALLOC_THRESHOLD, PERF_ALLOC_RATIO, PERF_WALL_THRESHOLD, PERF_WALL_RATIO,
# PERF_ICOUNT_THRESHOLD, PERF_ICOUNT_RATIO, PERF_ICOUNT_J1, PERF_ICOUNT_SPREAD, PERF_ICOUNT_N,
# PERF_ICOUNT_TIMEOUT, PERF_ICOUNT_JOBS, PERF_ICOUNT_DATA, PERF_RUNNER_IMAGE,
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

# ── The instruction-count threshold ─────────────────────────────────────────────────────────
#
# THE CLAIM A CALLER RELIES ON: **this gate sees a 1% change in any workload's instructions per
# iteration, at either size, and a move of its per-doubling ratio past the tolerance below.**
#
# A count does not vary from run to run: two runs of one binary execute the same instructions, and
# `icount.py` refuses a row whose repeated run says otherwise. So `--self` reads exactly zero on
# every row by construction — it builds identical source twice, and one source gives one binary —
# which proves the reading deterministic and calibrates nothing. **What the number rests on is
# codegen drift**: a pull request moves inlining and partitioning in code it never touched, and a
# row whose source did not change moves with it — a delta that is the compiler's and reads as the
# pull request's.
#
# THE DRIFT, MEASURED. The lexer-dispatch campaign's calibration read it by count on GitHub's x86
# runners: 26 steps along that campaign's chain of commits, each a build whose binary differed from
# the one before, read on GraphQL's own two syntactic rows. The chain was GraphQLx's and changed
# nothing that is GraphQL's alone. The 95th percentile of a step was **0.39% and 0.32%** of
# instructions on the two rows, the largest **0.49%**, and **0.92%** at the one step that changed
# code the two dialects share. +1% is above every one of them, and a row that moves past it has
# moved further than any step of that sample did.
#
# Three bounds on that sample, recorded rather than glossed:
#
#   * **it is x86 alone.** No step has been read on ARM. Until merged commits are replayed against
#     their parents on that image (a `perf.yml` dispatch with `base`), ARM's +1% is x86's figure,
#     and the ARM table says it has no drift sample of its own;
#   * **it is two rows.** No schema or validate row's drift has been sampled by count;
#   * **0.92% is close.** A change to code every row shares can read most of a per cent with nothing
#     wrong. An `icount/` trailer (see `compare.py`) is how such a change gets through.
: "${PERF_ICOUNT_THRESHOLD:=1.0}"

# The count's ratio gate, in the ratio's own units as `PERF_ALLOC_RATIO` is, and at its value for
# its reason: linear is 2 and quadratic is 4, so 0.15 is under a tenth of the distance between
# them. A ratio moves only as far as a row's two sizes part, and drift moves both sizes of a row
# together; the worst step above, landing on one size alone, would move a ratio of 2 by 0.018. A
# trade an `icount/` trailer accepts may land on one size more than the other, and 0.15 leaves it
# room that a change of law does not need. What no count has yet is the planted subject the
# allocation ratio was read against, so this rests on that arithmetic, not on a measured plant.
: "${PERF_ICOUNT_RATIO:=0.15}"

# ── J1: a second verdict on the GraphQLx syntactic rows ─────────────────────────────────────
#
# The owner's rule for the lexer-dispatch work, J1: each of the three GraphQLx syntactic rows
# executes no more than the base's instructions and an allowance, which the owner put at about
# half a per cent. The +1% line alone misleads there — the campaign's head after its first repair
# read +0.83% and +0.87% on two of these rows, which +1% passes and J1 fails — so they carry a
# second verdict at 0.5%, printed in the table and raised as a warning, and never shown as `ok`.
#
# It fails nothing, with one exception that is not a verdict: a J1 row whose halves read more than
# the spread bound below apart at n and again at 4n is NOT MEASURED, and that fails the stage at any
# delta. A red J1 leg of that kind means the row was not measured, not that it regressed.
#
# On the executable row, single inlining decisions that moved with unrelated commits were each
# about the size of the allowance, so a red at 0.5% would fire on pull requests that never touched
# the parser. The lexer row's own figure is the owner's to set; it takes the
# parse rows' half per cent until they do. `lex_graphql_syntactic` is not here: it is GraphQL's own
# row, and belongs with the drift sample, not with J1. No verdict of any kind reads reads and
# writes: their 95th-percentile step in the sample above was 0.47% and 0.65%.
: "${PERF_ICOUNT_J1:=0.5}"
icount_j1_rows=lex_graphqlx_syntactic,parse_graphqlx_type_system,parse_graphqlx_executable

# ── A reading's spread, and how much of it a line can bear ──────────────────────────────────
#
# A count reads one iteration's cost over iterations n+1 to 3n, and its SPREAD is how far apart the
# two halves of that baseline read, as a per cent of one half (`icount.py`). A cost per iteration
# that is constant has none. MEASURED on the first runs of this stage, both images, every row and
# size on each side:
#
#   * the lexer and validate rows, exactly 0 — one allocation of a known size an iteration, or
#     buffers kept across iterations;
#   * the executable and deep-value parse rows, under 0.007%;
#   * the GraphQLx syntactic rows, J1's three, at most 0.050% (`parse_graphqlx_type_system` at hi
#     on x86);
#   * the rows that build and free many heap objects in each iteration, the schema family and the
#     type-system parses, up to 0.45% on x86 and 0.52% on aarch64, of either sign.
#
# THAT LAST IS NOT A WARM-UP, and more warm-up iterations would not remove it. The first n
# iterations already cancel in every difference, and both halves come after them. A warm-up still
# settling would move a row's two halves one way at both of its sizes; of the eleven x86 readings
# over 0.01%, six rise and five fall, and three of the five rows that spread go opposite ways at
# their two sizes. Nothing grows across iterations either: no cache, interner or kept `Vec` on these
# paths, and the two validate rows, which do keep their buffers, read exactly 0. What one of these
# iterations costs depends on where the allocator's free lists left the last one, and nothing shows
# it settling.
#
# SO A SPREAD IS BOUNDED, AGAINST THE LINE IT HAS TO SUPPORT. The reading sits between its two
# halves, and a head-against-base delta takes two readings, so a delta can carry about one spread
# of allocator placement that is not the change. A quarter of the finest line a workload is read
# against keeps a delta that reaches that line at four spreads or more of it: at most 0.25% for the
# +1% threshold, and 0.125% for J1's 0.5% on its three rows. Every table shows each workload's
# widest spread, and the n it was read at. On the first runs every J1 row read with room, and two
# rows missed: `schema_iface_mismatch` (0.450% at lo) on x86 and `schema_valid` (0.519% at lo) on
# aarch64.
#
# A MISS IS READ AGAIN, NOT REFUSED AT ONCE. A workload past its bound on either side, at either
# size, is read again on both sides at four times n — 40, 80 and 120 — and that reading replaces
# the first; past the bound there too, it is NOT MEASURED. If the offset placement puts on a half
# is bounded, a half four times as long carries a quarter of the spread; if each iteration's
# placement is independent of the last, half of it. One length of half cannot tell those apart, so
# the first runs predict a range: x86's miss would read 0.112% or 0.225%, under its 0.25% either
# way, and aarch64's 0.130% or 0.259%, under it by the first and just over it by the second. The
# second reading is 24n iterations a size and side against the first's 7n, and only rows that miss
# pay for it: at the first runs' speeds, about 600 s of callgrind for x86's miss and 140 s for
# aarch64's, under four minutes and under one at four runs at a time.
: "${PERF_ICOUNT_SPREAD:=0.25}"

# How many iterations a count is read at: n, 2n and 3n, and n again for the repeat. The first n
# cancel in every difference, so they are the warm-up — the allocator's bins and `Scratch` grow over
# the first iterations and are reused after, and tokora's gate takes 8 for the same reason. Every
# run's `--iterations` is zero-padded to one width, a second reading's 12n included, so argv is the
# same length in every run. 16 would serve as well as 10, at 60% more callgrind time.
: "${PERF_ICOUNT_N:=10}"

# Seconds one callgrind run may take before its row reads NOT MEASURED. A bound, not an estimate:
# the dearest run is 3n iterations of the dearest row at its large size, and the probe branch made
# two calls of every row's region at both sizes, on both sides and with branch simulation as well,
# in one CI step of 75 s that included installing valgrind. It is here so that a run which hangs is
# a refusal on its row and not the job's timeout. A second reading's runs are four times as long
# and are given four times this.
: "${PERF_ICOUNT_TIMEOUT:=600}"

# How many callgrind runs go at once; empty is one per CPU. The count stage runs after the wall
# clock, so nothing it does can load a timed run, and a count does not depend on load.
: "${PERF_ICOUNT_JOBS:=}"

# `yes` reads data reads and writes beside the instructions, through callgrind's cache simulator,
# which costs time; `no` reads instructions only, and the table says so. Reads and writes are
# advisory either way.
: "${PERF_ICOUNT_DATA:=yes}"

# How many times the two wall-clock binaries are INTERLEAVED. Interleaving rather than running all
# of one side then all of the other is the half that matters: a runner whose throughput drifts over
# the job's lifetime otherwise charges the whole drift to whichever side ran second.
: "${PERF_ROUNDS:=4}"

# Batches inside one invocation, each reported as its own fastest. See `wall_clock.rs` on why the
# reduction is min and not mean.
: "${PERF_INNER_ROUNDS:=5}"

: "${PERF_WORK:=${RUNNER_TEMP:-/tmp}/smear-perf}"

# Which stages run: `alloc`, `wall` and `icount`, comma-separated. They run in that order whatever
# the list says, counts last. The allocation gate does not depend on the wall-clock one and is worth
# running alone on a host whose timing is not worth reading, and the counts need a host with
# valgrind. `both` is the spelling from before the third stage and means what it meant, `alloc,wall`:
# it does not include the counts, and the run says so. Any other word is refused rather than read as
# some subset, because a stage a typo left out is a stage nobody sees missing.
: "${PERF_ONLY:=alloc,wall}"

# ── THE INSTRUMENT, AS CARGO KNOWS IT ───────────────────────────────────────────────────────
#
# Two bench targets of one package, built with the features their `required-features` names. The
# merge-base must declare both, at the same paths; every file they read is derived from their
# sources (see the header), so nothing in this script names one.
perf_package=smear
perf_features=graphqlx,validator
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
# Pinned: every key that can change the code rustc generates for a bench — optimisation, codegen
# partitioning, LTO, debug info, the two runtime checks, incremental compilation. Not pinned:
# `panic`, which cargo ignores for benchmarks ("Tests, benchmarks, build scripts, and proc macros
# ignore the panic setting"), so a bench always unwinds and pinning the key pinned nothing; and
# `strip`, `split-debuginfo` and `rpath`, which change only what is written into the executable
# beside that code, and whose defaults depend on the platform and the cargo release, so a guessed
# value could change today's binary. What the environment cannot pin at all is a per-package
# table, `[profile.<name>.package.<pkg>]`, for which cargo reads no variable. Both gaps are closed
# the same way: `sides.py profile` compares the two trees' declarations before anything is built,
# prints any difference, and stops the run as not comparable when the difference is neither in a
# pinned key nor in `panic`, which no bench build reads.
profile_pin=(
  opt-level=3
  codegen-units=16
  lto=false
  debug=false
  debug-assertions=false
  overflow-checks=false
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
#
# Once both sides are built, the line names every stage that ran and what it found —
# `alloc: …; wall: …; icount: …` — each part the one line that stage's comparator left in
# `$PERF_WORK/<stage>.outcome`. A stage NOT MEASURED says so in its part, and when every stage that
# ran is NOT MEASURED the line starts with it, so a run that compared nothing still cannot read as
# one that did.
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
# `materialise <head-sha> <base-sha>` puts the head's tree in `$PERF_WORK/head` and the base's —
# the merge-base, or the revision `--against` names — in `$PERF_WORK/base`, checks that one
# instrument can read both, and lays the head's instrument over the base. Nothing in it compiles,
# which is what lets a harness source this file and call it.
materialise() {
  local head_sha="$1" base_sha="$2" side file note status=0

  rm -rf "$PERF_WORK/head" "$PERF_WORK/base"
  mkdir -p "$PERF_WORK/head" "$PERF_WORK/base"

  # `git archive` rather than `git worktree add`: it materialises a tree with no `.git`, no
  # `target/` and no bookkeeping to clean up, and it takes the tree from the OBJECT rather than
  # from the working directory — so an uncommitted edit cannot leak into a side.
  #
  # `tar -m`, so every file carries the time it was extracted. The archive of a COMMIT stamps each
  # file with the commit's time, and cargo takes a path package for fresh when its artifacts are
  # newer than its sources: a cache restored from a later build of another revision with the same
  # manifests would stand in for this side unseen. With `-m` the sources are newer than anything
  # cached, so both sides' members are compiled on every run, as in every sound run so far, and only
  # the registry dependencies come from the cache. GNU tar (`--touch`) and bsdtar both read `-m` as
  # "do not extract the modification time".
  git archive "$head_sha" | tar -x -m -C "$PERF_WORK/head"
  git archive "$base_sha" | tar -x -m -C "$PERF_WORK/base"

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
# `--features graphqlx,validator` and both `--bench` names are spelled out. Naming a target is LOUD
# when its `required-features` are unmet (`exit 101, "requires the features: ..."`) where a filter
# that matches nothing warns and exits 0; `smear/Cargo.toml`'s bench header carries the four
# measured cases.
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

# `compiled head|base`, after that side's build: the run goes on only if the build compiled the
# side's own packages and both bench targets (see the header). `sides.py compiled` reads the
# messages `build_side` saved and names every artifact cargo took for fresh.
compiled() {
  local status=0
  python3 "$perf_dir/sides.py" compiled "$1" "$PERF_WORK/$1" "$PERF_WORK/$1.build.json" \
    "${perf_targets[@]}" || status=$?
  if [ "$status" -eq 3 ]; then
    stop "the $1 side did not compile its own packages, so nothing was measured"
  elif [ "$status" -ne 0 ]; then
    stop "the $1 side's build messages could not be read"
  fi
}

# `resolve_against <rev>` sets `base` to the commit <rev> names, once: as given (a hash, a tag, a
# local branch), then as `origin/<rev>`, which is where a CI checkout keeps every other branch. It
# stops the run with a plain error when neither names a commit. An argument not spelled like a
# revision is refused before git reads it, and is not echoed.
resolve_against() {
  case "$1" in
    '' | -* | *[!A-Za-z0-9._/@^~{}+-]*)
      stop "--against takes a revision, a hash, a tag or a branch, and this is not spelled like one"
      ;;
  esac
  base="$(git rev-parse --verify --quiet "$1^{commit}" \
    || git rev-parse --verify --quiet "refs/remotes/origin/$1^{commit}" || true)"
  if [ -z "$base" ]; then
    stop "\`$1\` names no commit in this checkout, as given or as origin/$1"
  fi
}

# Sourced rather than run — by a harness that wants the two trees, the profile check and the locks
# without a build — the file stops here, having defined its stages and run none of them.
if [ "${BASH_SOURCE[0]}" != "$0" ]; then
  return 0
fi

if [ "${1:-}" = "--against" ]; then
  arguments=2
else
  arguments=1
fi
if [ "$#" -ne "$arguments" ]; then
  # Both patterns are anchored at `^# ` so that THIS line — which contains the start pattern as
  # data — cannot re-open the range and print the rest of the script.
  sed -n '/^# ── USAGE/,/^# PERF_ROUNDS/p' "$0" | sed 's/^# \{0,1\}//'
  exit 2
fi

mkdir -p "$PERF_WORK"
rm -f "$PERF_WORK/summary.md" "$PERF_WORK/alloc.outcome" "$PERF_WORK/wall.outcome" \
  "$PERF_WORK/icount.outcome"
trap headline EXIT

# The stages asked for. See `PERF_ONLY` above for why `both` stays and anything unknown does not.
want_alloc=0
want_wall=0
want_icount=0
if [ "$PERF_ONLY" = both ]; then
  echo "perf: PERF_ONLY=both is alloc,wall, as it was before the count stage; counts run only where"
  echo "perf: \`icount\` is named"
  PERF_ONLY=alloc,wall
fi
stages_left="$PERF_ONLY,"
while [ -n "$stages_left" ]; do
  stage="${stages_left%%,*}"
  stages_left="${stages_left#*,}"
  case "$stage" in
    alloc) want_alloc=1 ;;
    wall) want_wall=1 ;;
    icount) want_icount=1 ;;
    *) stop "PERF_ONLY=$PERF_ONLY is not a list of the stages alloc, wall and icount" ;;
  esac
done
stages_asked=""
for stage in alloc wall icount; do
  case "$stage" in
    alloc) wanted="$want_alloc" ;;
    wall) wanted="$want_wall" ;;
    icount) wanted="$want_icount" ;;
  esac
  if [ "$wanted" -eq 1 ]; then
    stages_asked="${stages_asked:+$stages_asked, }$stage"
  fi
done
echo "perf: stages: $stages_asked"

# valgrind is looked for before anything is built: a run that asked for counts alone has nothing
# else to do without it, and one that asked for more still reads the others and says the counts
# were NOT MEASURED.
icount_unavailable=""
if [ "$want_icount" -eq 1 ] && ! command -v valgrind > /dev/null 2>&1; then
  icount_unavailable="valgrind is not installed on this host"
  if [ "$want_alloc" -eq 0 ] && [ "$want_wall" -eq 0 ]; then
    stop "the count stage was the only one asked for, and $icount_unavailable"
  fi
  echo "::warning::perf: the count stage will read NOT MEASURED: $icount_unavailable"
fi

if [ "$1" = "--self" ]; then
  base="$(git rev-parse HEAD)"
  mode="self-comparison"
elif [ "$1" = "--against" ]; then
  resolve_against "$2"
  mode="against $2 itself, not a merge-base"
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
compiled head

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
compiled base

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
# spelling is `tokora`'s, deliberately: one convention across the two repositories, not two. Each
# trailer is scoped to the instrument it names, and `compare.py` reads only its own stage's: a bare
# workload name licenses the allocation and wall-clock gates, and `icount/<workload>` the count.
git log --format=%B "$base..HEAD" | grep -E '^\s*Perf-accept:' > "$PERF_WORK/accept.txt" || true
if [ -s "$PERF_WORK/accept.txt" ]; then
  echo "perf: this branch carries acceptances:"
  sed 's/^/perf:   /' "$PERF_WORK/accept.txt"
fi

status=0

if [ "$want_alloc" -eq 1 ]; then
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
    --summary "$PERF_WORK/summary.md" \
    --outcome "$PERF_WORK/alloc.outcome" || status=1
fi

if [ "$want_wall" -eq 1 ]; then
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
    --summary "$PERF_WORK/summary.md" \
    --outcome "$PERF_WORK/wall.outcome" || status=1
fi

if [ "$want_icount" -eq 1 ]; then
  echo
  echo "perf: === instruction counts ==="

  # Last, after the clock, so that the parallel callgrind runs load no timed one. Both binaries are
  # the ones `build_side` built and `compiled` vetted, so every refusal of the two revisions as not
  # comparable has already been made before a count is taken.
  if [ -n "$icount_unavailable" ]; then
    echo "::error::perf: the count stage did not run: $icount_unavailable"
    echo "NOT MEASURED: $icount_unavailable" > "$PERF_WORK/icount.outcome"
    status=1
  else
    rm -rf "$PERF_WORK/icount"
    icount_status=0
    python3 "$perf_dir/icount.py" \
      --head "$wall_head" \
      --base "$wall_base" \
      --out "$PERF_WORK/icount" \
      --outcome "$PERF_WORK/icount.outcome" \
      --iterations "$PERF_ICOUNT_N" \
      --timeout "$PERF_ICOUNT_TIMEOUT" \
      --jobs "${PERF_ICOUNT_JOBS:-0}" \
      --data "$PERF_ICOUNT_DATA" \
      --threshold "$PERF_ICOUNT_THRESHOLD" \
      --j1-rows "$icount_j1_rows" \
      --j1-threshold "$PERF_ICOUNT_J1" \
      --spread-share "$PERF_ICOUNT_SPREAD" || icount_status=$?
    if [ "$icount_status" -eq 0 ]; then
      python3 "$perf_dir/compare.py" \
        --kind icount \
        --base "$PERF_WORK/icount/base.json" \
        --head "$PERF_WORK/icount/head.json" \
        --threshold "$PERF_ICOUNT_THRESHOLD" \
        --ratio-tolerance "$PERF_ICOUNT_RATIO" \
        --j1-rows "$icount_j1_rows" \
        --j1-threshold "$PERF_ICOUNT_J1" \
        --spread-share "$PERF_ICOUNT_SPREAD" \
        --accept-file "$PERF_WORK/accept.txt" \
        --summary "$PERF_WORK/summary.md" \
        --outcome "$PERF_WORK/icount.outcome" || status=1
    else
      if [ ! -s "$PERF_WORK/icount.outcome" ]; then
        echo "NOT MEASURED: icount.py stopped with status $icount_status; the log above says why" \
          > "$PERF_WORK/icount.outcome"
      fi
      status=1
    fi
  fi
fi

# The last line: each stage that ran, and the one line it left about itself. A stage whose
# comparator stopped before it wrote one is NOT MEASURED; its error is in the log above.
stages_said=""
measured_any=0
for stage in alloc wall icount; do
  case "$stage" in
    alloc) wanted="$want_alloc" ;;
    wall) wanted="$want_wall" ;;
    icount) wanted="$want_icount" ;;
  esac
  if [ "$wanted" -ne 1 ]; then
    continue
  fi
  said="NOT MEASURED: its comparison stopped before it decided; the log above says why"
  if [ -s "$PERF_WORK/$stage.outcome" ]; then
    said="$(head -n 1 "$PERF_WORK/$stage.outcome")"
  fi
  case "$said" in
    "NOT MEASURED"*) ;;
    *) measured_any=1 ;;
  esac
  stages_said="${stages_said:+$stages_said; }$stage: $said"
done
if [ "$measured_any" -eq 1 ]; then
  outcome="measured ${head_sha:0:12} against ${base:0:12} ($mode); $stages_said"
else
  outcome="NOT MEASURED: ${head_sha:0:12} against ${base:0:12} ($mode); $stages_said"
fi
exit "$status"
