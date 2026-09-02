//! **Wall clock**, per workload, at two sizes — the coarse layer over the same corpus.
//!
//! # What this sees that `peak_alloc` cannot
//!
//! Cache locality, branch prediction and data movement. A change that keeps the same allocations
//! and arranges them worse — a hot field pushed past a cache line, a loop whose branch stopped
//! being predictable, a copy that became a copy of the same bytes in a worse order — costs a clock
//! and costs an allocator counter nothing.
//!
//! # And what it cannot see, stated plainly rather than implied
//!
//! **Roughly a 10% regression, and no better.** That is not a property of this harness; it is what
//! a shared CI runner's spread allows. The sibling instrument in `tokora` measured 4.3-4.8%
//! run-to-run on a *dedicated* machine, and one run there moved nine unrelated benches by +82% in
//! lockstep. A threshold under about 10% on a GitHub-hosted runner is a threshold that reds on
//! runner weather, and a gate that reds for reasons nobody caused is a gate that gets switched
//! off.
//!
//! So this gate is deliberately the second layer. `peak_alloc` is the sharp one; this one exists
//! for the class the sharp one is blind to, and its threshold is derived from a self-comparison on
//! the runner that will actually run it — see `ci/perf/run.sh`'s header for the measurement.
//!
//! # Min-of-N, not mean
//!
//! Noise on a shared runner is one-directional: a co-tenant can only make a measurement slower.
//! The fastest observation of an identical computation is therefore the one least contaminated,
//! and averaging mixes the signal with however much of the runner somebody else was using. Each
//! size is timed in `--rounds` batches and the fastest batch is what is reported.
//!
//! `ci/perf/run.sh` adds the other half: it **interleaves** whole invocations of the base and head
//! binaries rather than running all of one and then all of the other, so a drift in the runner's
//! throughput over the job's lifetime lands on both sides instead of on whichever ran second.
//!
//! # Usage
//!
//!     cargo bench -p smear --features validator --bench wall_clock
//!     cargo bench -p smear --features validator --bench wall_clock -- --rounds 7 --json out.json

mod perf;

use std::{
  hint::black_box,
  time::{Duration, Instant},
};

use perf::{Reading, WORKLOADS};

/// How long one timed batch should take. Long enough that `Instant`'s own resolution and the
/// call overhead are noise beside it, short enough that the whole binary is a couple of seconds.
const BATCH: Duration = Duration::from_millis(20);

/// The ceiling on the calibrated iteration count, so a workload that turns out to be far cheaper
/// than expected cannot turn one batch into a minute.
const MAX_ITERS: u64 = 100_000;

fn main() {
  let args: Vec<String> = std::env::args().skip(1).collect();
  let destination = perf::json_destination(&args);
  let rounds = perf::rounds(&args, 5);

  perf::warm_up();

  let readings: Vec<Reading> = WORKLOADS
    .iter()
    .map(|workload| {
      assert_eq!(
        workload.hi,
        workload.lo * 2,
        "`{}` declares sizes that are not a doubling, so its ratio would not be a per-doubling \
         reading",
        workload.name
      );
      Reading {
        name: workload.name,
        family: workload.family,
        lo_size: workload.lo,
        hi_size: workload.hi,
        lo: measure(workload.run, workload.lo, rounds),
        hi: measure(workload.run, workload.hi, rounds),
      }
    })
    .collect();

  perf::print_table("ns/iter", &readings);

  if let Some(path) = destination {
    let json = perf::emit_json("wall_clock", "ns", rounds, &readings);
    std::fs::write(&path, json).unwrap_or_else(|error| panic!("cannot write {path}: {error}"));
    println!("wall_clock: wrote {path}");
  }
}

/// Nanoseconds per run of the measured region: the fastest of `rounds` batches.
///
/// The iteration count is calibrated from one untimed run rather than fixed, because the workloads
/// here span four orders of magnitude and a count that suits the cheapest would take minutes on
/// the dearest. The count is per (workload, size) and is reported per iteration, so the base and
/// head sides remain comparable even where they calibrate to different counts.
fn measure(run: fn(usize, &mut perf::Region<'_>), size: usize, rounds: usize) -> f64 {
  let mut best = f64::INFINITY;
  run(size, &mut |body: &mut dyn FnMut()| {
    let probe = Instant::now();
    body();
    let single = probe.elapsed().as_nanos().max(1) as u64;

    let iters = (BATCH.as_nanos() as u64 / single).clamp(1, MAX_ITERS);

    for _ in 0..rounds {
      let started = Instant::now();
      for _ in 0..iters {
        body();
      }
      let elapsed = started.elapsed();
      black_box(&elapsed);
      let per_iter = elapsed.as_nanos() as f64 / iters as f64;
      if per_iter < best {
        best = per_iter;
      }
    }
  });
  best
}
