//! What the run prints, and what makes it fail.
//!
//! Every check states its own reach before its findings — rows read, accessor bodies read,
//! inventories found — because a check with nothing to check passes for the wrong reason, and
//! that is the failure mode this whole tool exists about. All four therefore red on a reach of
//! zero as loudly as they red on a finding.

use super::{Report, exempt, probe::Outcome};

const RULE: &str =
  "────────────────────────────────────────────────────────────────────────────────────────────";

/// The header rule, counted in characters. Byte-slicing a run of `─` lands mid-codepoint.
fn header(title: &str) -> String {
  let width = RULE.chars().count();
  let used = title.chars().count();
  format!("{title}{}", "─".repeat(width.saturating_sub(used)))
}

pub fn render(report: &Report, probe: Option<&Outcome>, verbose: bool) {
  println!();
  println!("{}", header("── Diagnostic contract census ─"));
  println!(
    "`{}` resolves through {} public {}: {}",
    super::TRAIT,
    report.trait_paths.len(),
    plural(report.trait_paths.len(), "spelling", "spellings"),
    if report.trait_paths.is_empty() {
      "none".to_string()
    } else {
      report.trait_paths.join(", ")
    }
  );

  println!();
  println!(
    "D1 — public error types the contract has something to say about: {} ({} answer it, {} \
     recorded)",
    report.rows.len(),
    report.contract.len(),
    report.recorded.len(),
  );
  for index in &report.contract {
    let row = &report.rows[*index];
    println!("  = [CONTRACT] {}", row.path);
    println!("      {}", row.location);
    for origin in &row.origins {
      println!("      {origin}");
    }
    if verbose && let Some(spelling) = &row.spelling {
      println!("      probe: {}", spelling.ty);
    }
  }
  for (index, exemption) in &report.recorded {
    let row = &report.rows[*index];
    println!("  ! [{}] {}", exemption.label(), row.path);
    println!("      {}", row.location);
    for origin in &row.origins {
      println!("      {origin}");
    }
    println!("      reason: {}", squash(exemption.reason));
  }
  if !report.folded_aliases.is_empty() {
    println!(
      "  public aliases folded into the row their target is: {}",
      report.folded_aliases.join(", ")
    );
  }
  if !report.foreign_err_types.is_empty() {
    println!(
      "  public `Result`s whose `Err` another crate declares, which this one cannot make answer: \
       {}",
      report.foreign_err_types.len()
    );
    for entry in &report.foreign_err_types {
      println!("      {entry}");
    }
  }

  println!();
  match probe {
    None => println!("D2 — not run"),
    Some(outcome) if outcome.skipped.is_some() => println!(
      "D2 — SKIPPED: {}",
      outcome.skipped.as_deref().unwrap_or_default()
    ),
    Some(outcome) => {
      println!(
        "D2 — {} {} asserted through {} trait {}, checked by rustc at {}: {}",
        outcome.rows,
        plural(outcome.rows, "row", "rows"),
        outcome.spellings,
        plural(outcome.spellings, "spelling", "spellings"),
        outcome.dir.display(),
        match outcome.status {
          Some(0) => "every one holds".to_string(),
          Some(code) => format!("cargo check exited {code}"),
          None => "cargo check did not run to completion".to_string(),
        }
      );
    }
  }

  println!();
  println!(
    "D3 — wildcard arms in the {} accessor {} the registry names, over {} {}: {}",
    report.accessors_read,
    plural(report.accessors_read, "body", "bodies"),
    report.matches_read,
    plural(report.matches_read, "match", "matches"),
    report.wildcards.len(),
  );
  for wildcard in &report.wildcards {
    println!(
      "  x {}  `{} => …`  ({})",
      wildcard.function, wildcard.pattern, wildcard.location
    );
  }

  println!();
  println!(
    "D4 — `ALL`-style inventories found by shape: {}",
    report.inventories.len()
  );
  for inventory in &report.inventories {
    let verdict = if inventory.missing.is_empty() && inventory.spurious.is_empty() {
      "complete".to_string()
    } else {
      let mut parts = Vec::new();
      if !inventory.missing.is_empty() {
        parts.push(format!("MISSING {}", inventory.missing.join(", ")));
      }
      if !inventory.spurious.is_empty() {
        parts.push(format!("SPURIOUS {}", inventory.spurious.join(", ")));
      }
      parts.join("; ")
    };
    println!(
      "  {} {}  {}/{} variants — {verdict}",
      if inventory.missing.is_empty() && inventory.spurious.is_empty() {
        "."
      } else {
        "x"
      },
      inventory.name,
      inventory.listed,
      inventory.declared,
    );
    if verbose || !inventory.missing.is_empty() || !inventory.spurious.is_empty() {
      println!(
        "      {}  (enum at {})",
        inventory.location, inventory.enum_location
      );
    }
  }
  for problem in &report.unresolved_inventories {
    println!("  ? {problem}");
  }
  println!("{RULE}");
  println!();
}

/// Prints every reason the run fails, and returns whether it passed.
pub fn verdict(report: &Report, probe: Option<&Outcome>) -> bool {
  let mut ok = true;

  for problem in &report.table_problems {
    error(&format!("the exemption table is not usable: {problem}"));
    ok = false;
  }

  if report.rows.is_empty() {
    error(
      "D1 read no public error types at all. It would then pass by having nothing to check, \
       which is the defect it exists to catch — the reader has stopped finding the crate's \
       surface",
    );
    ok = false;
  }

  if report.trait_paths.is_empty() {
    error(&format!(
      "no public spelling of `{}` was found. Either the trait is gone, or it moved to another \
       crate and this one no longer re-exports it — and in that second case D2 has been asserting \
       nothing",
      super::TRAIT
    ));
    ok = false;
  }

  for problem in &report.broken_aggregates {
    error(&format!(
      "a record that delegates the contract delegates it to nothing: {problem}"
    ));
    ok = false;
  }

  if !report.stale.is_empty() {
    error(&format!(
      "{} {} in the contract exemption table {} nothing in the crate",
      report.stale.len(),
      plural(report.stale.len(), "entry", "entries"),
      plural(report.stale.len(), "matches", "match"),
    ));
    for exemption in &report.stale {
      println!("  - {}", exemption.path);
    }
    println!(
      "  A stale exemption is not harmless: it is a type recorded as not answering the contract \
       that no longer exists, or a reader that has stopped seeing it. If the type now implements \
       `Diagnose`, delete the line — that is how this table is made to shrink. If it does not, \
       the census is no longer reading what it thinks it is."
    );
    ok = false;
  }

  match probe {
    None => {}
    Some(outcome) if outcome.skipped.is_some() => {
      error(
        "D2 did not run, so nothing proved that the contract rows implement the contract. This \
         is a local convenience and CI must not use it",
      );
      ok = false;
    }
    Some(outcome) => {
      if outcome.rows == 0 {
        error(
          "D2 asserted nothing: every row of the D1 inventory is exempt. A probe with no \
           assertions in it compiles, and a check that passes because it is empty is the shape \
           this tool exists to refuse",
        );
        ok = false;
      }
      if outcome.status != Some(0) {
        error(&format!(
          "D2's probe crate does not compile, so at least one type the contract covers does not \
           implement `{}`. `rustc` names it below; the census does not, because the census is \
           only the inventory",
          super::TRAIT
        ));
        for line in outcome.output.lines() {
          println!("  | {line}");
        }
        println!(
          "  Either implement the contract on the type — on the resolved view, if its spans mean \
           nothing without a schema — or record it in ci/source_census/src/diagnose/exempt.rs \
           with a reason and, if it is a family waiting on a phase, the issue that owns it."
        );
        ok = false;
      }
    }
  }

  if report.matches_read == 0 {
    error(
      "D3 read no matches at all. The per-variant enforcement is those matches, so a check that \
       found none of them is not checking the enforcement — the accessors have been renamed, or \
       the reader no longer reaches the files they are in",
    );
    ok = false;
  }

  if !report.wildcards.is_empty() {
    error(&format!(
      "{} wildcard {} in an accessor the contract's per-variant enforcement depends on",
      report.wildcards.len(),
      plural(report.wildcards.len(), "arm", "arms"),
    ));
    for wildcard in &report.wildcards {
      println!(
        "  - {}  `{} => …`  ({})",
        wildcard.function, wildcard.pattern, wildcard.location
      );
    }
    println!(
      "  A wildcard-free match IS the enforcement: it is what makes a new variant a hard `E0004` \
       until it answers with a code, a severity, a help line and a related-span phrase. One `_ \
       =>` turns that compile error into a silently wrong answer for every variant added \
       afterwards, and nothing else in the build says so."
    );
    ok = false;
  }

  if report.inventories.is_empty() {
    error(
      "D4 found no `ALL`-style inventory. There are several in this crate, so finding none means \
       the shape this check derives from has changed and the check now covers nothing",
    );
    ok = false;
  }

  if !report.unresolved_inventories.is_empty() {
    error(&format!(
      "{} {} the shape of an inventory and could not be resolved to an enum",
      report.unresolved_inventories.len(),
      plural(
        report.unresolved_inventories.len(),
        "constant has",
        "constants have"
      ),
    ));
    for problem in &report.unresolved_inventories {
      println!("  - {problem}");
    }
    ok = false;
  }

  let incomplete: Vec<_> = report
    .inventories
    .iter()
    .filter(|inventory| !inventory.missing.is_empty() || !inventory.spurious.is_empty())
    .collect();
  if !incomplete.is_empty() {
    error(&format!(
      "{} {} does not enumerate its enum",
      incomplete.len(),
      plural(incomplete.len(), "inventory", "inventories"),
    ));
    for inventory in &incomplete {
      println!(
        "  - {}  ({}) — {} of {} variants",
        inventory.name, inventory.location, inventory.listed, inventory.declared
      );
      if !inventory.missing.is_empty() {
        println!("    missing: {}", inventory.missing.join(", "));
      }
      if !inventory.spurious.is_empty() {
        println!("    spurious: {}", inventory.spurious.join(", "));
      }
    }
    println!(
      "  Nothing in the compiler ties a hand-written array to the enum above it — al8n/smear#126 \
       measured that directly: planting a variant on `SchemaErrorKind` reddened five wildcard-free \
       matches and left `ALL` green. An inventory that misses a variant is a rule no fixture \
       exercises, or a directive location the schema cannot name."
    );
    ok = false;
  }

  if ok {
    println!(
      "diagnostic-census OK: {} public error {}, {} answering the contract and proven by rustc, \
       {} recorded ({} aggregates, {} resolved elsewhere, {} verdicts, {} tracked, {} where the \
       pattern misfired); {} accessor {} with no wildcard; {} {} complete",
      report.rows.len(),
      plural(report.rows.len(), "type", "types"),
      report.contract.len(),
      report.recorded.len(),
      count(report, exempt::Kind::Aggregate),
      count(report, exempt::Kind::Unresolved),
      count(report, exempt::Kind::Verdict),
      count(report, exempt::Kind::Tracked),
      count(report, exempt::Kind::NotDiagnostic),
      report.accessors_read,
      plural(report.accessors_read, "body", "bodies"),
      report.inventories.len(),
      plural(report.inventories.len(), "inventory", "inventories"),
    );
  }
  ok
}

fn count(report: &Report, kind: exempt::Kind) -> usize {
  report
    .recorded
    .iter()
    .filter(|(_, exemption)| exemption.kind == kind)
    .count()
}

fn plural(n: usize, one: &str, many: &str) -> String {
  if n == 1 { one } else { many }.to_string()
}

fn squash(text: &str) -> String {
  text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn error(message: &str) {
  if std::env::var_os("GITHUB_ACTIONS").is_some() {
    println!("::error::source-census: {message}");
  } else {
    println!("error: source-census: {message}");
  }
}
