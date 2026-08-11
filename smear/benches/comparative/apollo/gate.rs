//! Run the correctness gate on its own and exit non-zero if any corpus entry is excluded.
//!
//! The bench runs the same gate in-process before timing; this exists so the gate's table and its
//! verdict can be captured without a full criterion run, and so CI or a reviewer can ask the
//! question "are both parsers doing the same job on this corpus?" and get an exit code:
//!
//! ```text
//! cargo run -p smear --features rowan,validator --example gate --release          # table + verdict
//! cargo run -p smear --features rowan,validator --example gate --release -- kinds # + node-kind histograms
//! ```
//!
//! **An example rather than a `[[bin]]`, deliberately.** `smear` declares no `[[bin]]`, so its CI
//! `cross` job — `cargo build -p smear --target <t>` over fifteen targets, with nothing installed
//! but `rustup target add` — has never had to *link an executable*. A `[[bin]]` here would be its
//! first, and `cargo build` builds bins by default: the job then
//! fails at link time on every target whose cross linker the runner lacks (verified locally:
//! `aarch64-linux-android` and `x86_64-pc-windows-gnu` both fail with
//! `could not compile ... (bin "gate")`). `cargo build` does **not** build examples, so as an
//! example this file leaves the cross job building exactly what it built before.

// The shared harness. It is a module compiled into this target rather than a library
// linked into it; `support/mod.rs` carries why, and why this file sits under `benches/`.
mod support;

use crate::support::{CORPUS, kind_histograms, print_gate, run_gate};

fn main() -> std::process::ExitCode {
  // `gate kinds` additionally dumps each side's node-kind histogram, which is what turns gate
  // 3's node-count difference from a bare number into an explanation.
  let want_kinds = std::env::args().any(|a| a == "kinds");

  println!("## Corpus");
  println!();
  println!("| entry | bytes | tier-2 shape | provenance |");
  println!("|---|---:|---|---|");
  for entry in CORPUS {
    println!(
      "| {} | {} | {:?} | {} |",
      entry.name,
      entry.len(),
      entry.shape,
      entry.provenance
    );
  }
  println!();

  let rows = run_gate();
  print_gate(&rows);

  if want_kinds {
    for entry in CORPUS {
      let (smear, apollo) = kind_histograms(entry);
      println!("### node kinds — {}", entry.name);
      println!();
      println!("| rank | smear kind | count | apollo kind | count |");
      println!("|---:|---|---:|---|---:|");
      let rows = smear.len().max(apollo.len());
      for i in 0..rows {
        let (sk, sn) = smear
          .get(i)
          .map(|(k, n)| (k.as_str(), n.to_string()))
          .unwrap_or(("", String::new()));
        let (ak, an) = apollo
          .get(i)
          .map(|(k, n)| (k.as_str(), n.to_string()))
          .unwrap_or(("", String::new()));
        println!("| {} | {} | {} | {} | {} |", i + 1, sk, sn, ak, an);
      }
      println!();
    }
  }

  if rows.iter().all(|r| r.eligible()) {
    std::process::ExitCode::SUCCESS
  } else {
    std::process::ExitCode::FAILURE
  }
}
