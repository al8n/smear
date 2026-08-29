//! **Peak live bytes**, per workload, at two sizes — the sharper of this repository's two perf
//! gates and the one it most needs.
//!
//! # Why allocation and not time
//!
//! Every allocation defect found in this tree was invisible to a clock and loud to an allocator,
//! and the repairs are the record of it:
//!
//! | defect | peak, before -> after |
//! |---|---|
//! | `compute_closures` over the refused population | 3.17 GB -> 4.85 MB |
//! | field-coverage name rendering | 406.91 MB -> 4.25 MB |
//! | the `MissingInterfaceField` family | 6.03 GB -> 9.5 MB |
//! | `flatten`'s possible-set table | 128 MB -> 517 KB |
//! | the iterative rewrite's heap abort | 3.18x the caller's tree -> 1.08x |
//!
//! Not one of them would have been caught by a wall-clock gate at any threshold a runner can
//! support, and every one of them is a difference an allocator counter reads exactly.
//!
//! # Exactly, and that is the design
//!
//! A peak-byte reading is a property of what the program *does*, not of the machine it does it
//! on. It has no run-to-run spread to clear, so the threshold in `ci/perf/run.sh` is set from a
//! measured self-comparison rather than from a noise allowance, and a machine under load returns
//! the same number as an idle one. That is the property the criterion benches in this directory do
//! not have and cannot be given.
//!
//! The claim is checked rather than asserted: `ci/perf/run.sh` runs this binary twice on the
//! identical tree and fails if the two JSON files differ. See that script's header for what the
//! check found.
//!
//! # Usage
//!
//!     cargo bench -p smear --features validator --bench peak_alloc
//!     cargo bench -p smear --features validator --bench peak_alloc -- --json out.json
//!
//! `benches/solo/perf/mod.rs` carries the workloads, the two sizes each declares, and why the
//! ratio between them is the reading that matters.

// The instrument, shared with `tests/validator_allocation.rs` so the two cannot drift into
// different definitions of what an allocation is. Its header carries what the sharing costs.
#[path = "../../tests/support/counting.rs"]
mod counting;

mod perf;

use counting::peak_bytes;
use perf::{Reading, WORKLOADS};

fn main() {
  let args: Vec<String> = std::env::args().skip(1).collect();
  let destination = perf::json_destination(&args);

  // Charge the once-per-process built-in parse to nobody, then let the thread-local counters
  // themselves be touched once before any window opens.
  perf::warm_up();
  let _ = peak_bytes(|| {});

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
      let lo = measure(workload.run, workload.lo);
      let hi = measure(workload.run, workload.hi);
      Reading {
        name: workload.name,
        family: workload.family,
        lo_size: workload.lo,
        hi_size: workload.hi,
        lo: lo as f64,
        hi: hi as f64,
      }
    })
    .collect();

  perf::print_table("bytes", &readings);

  if let Some(path) = destination {
    let json = perf::emit_json("peak_alloc", "bytes", 1, &readings);
    std::fs::write(&path, json).unwrap_or_else(|error| panic!("cannot write {path}: {error}"));
    println!("peak_alloc: wrote {path}");
  }
}

/// Runs one workload at one size and returns the high-water mark of its measured region.
///
/// The region runs **once**. Repeating it would not raise the peak — a high-water mark is a
/// maximum, not a sum — and would only add the chance that a second run reuses an allocation the
/// first one freed, which is a property of the allocator rather than of the subject.
fn measure(run: fn(usize, &mut perf::Region<'_>), size: usize) -> usize {
  let mut peak = 0usize;
  run(size, &mut |body: &mut dyn FnMut()| {
    peak = peak_bytes(body);
  });
  peak
}
