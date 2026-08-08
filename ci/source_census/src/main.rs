//! Fails a build when a public entry takes source text at a concrete type.
//!
//! `smear` ships five source-representation integrations so a consumer can pick one. §3, syntactic
//! §5 and §6/§7 honour that with `S: AsRef<[u8]>`. Nothing fails when a new public entry spells the
//! same parameter `&str` instead: it compiles, it passes every test, it reads naturally, and the
//! narrowing is discovered by a consumer at a call site. That is al8n/smear#122, and this is the
//! mechanism it asks for.
//!
//! Run from the repository root:
//!
//! ```text
//! cargo run -p source-census --                # census `smear`, exit 1 on an unexplained finding
//! cargo run -p source-census -- --selftest     # prove every branch of the verdict can fail
//! cargo run -p source-census -- --verbose      # also list the parameters that cost nothing
//! ```
//!
//! Three modules carry the design and each states its own reasoning:
//!
//! * [`surface`] — which entries a consumer can reach, derived from the code rather than listed.
//! * [`rule`] — what counts as a parameter carrying source text, which is the whole judgement.
//! * [`exempt`] — the recorded narrowings, and what makes an unexplained one fail.
//!
//! # What makes this gate non-vacuous
//!
//! `ci/miri_scope.py` fails a Miri cell when the interpreted set is not what the scripts claim.
//! The same idea applies here in three places:
//!
//! * `--selftest` runs the verdict against synthetic crates and requires every branch to fire.
//! * An exemption that matches nothing is a failure, so the table is a live canary: if the reader
//!   ever stops finding the surface — a moved file, a `mod` shape it cannot walk — the table goes
//!   stale in the same instant and the run goes red, rather than reporting a clean crate.
//! * A run that reads zero public entries, or examines zero parameters, fails outright.

mod census;
mod exempt;
mod rule;
mod selftest;
mod surface;

#[cfg(test)]
mod tests;

use std::{
  path::{Path, PathBuf},
  process::ExitCode,
};

use census::{Report, Verdict};
use rule::Cost;

const DEFAULT_ROOT: &str = "smear/src/lib.rs";
const DEFAULT_CRATE: &str = "smear";

fn main() -> ExitCode {
  let mut root = PathBuf::from(DEFAULT_ROOT);
  let mut crate_name = DEFAULT_CRATE.to_string();
  let mut verbose = false;
  let mut run_selftest = false;

  let mut args = std::env::args().skip(1);
  while let Some(arg) = args.next() {
    match arg.as_str() {
      "--selftest" => run_selftest = true,
      "--verbose" => verbose = true,
      "--crate-root" => match args.next() {
        Some(value) => root = PathBuf::from(value),
        None => return fail("--crate-root needs a path to a crate's lib.rs"),
      },
      "--crate-name" => match args.next() {
        Some(value) => crate_name = value,
        None => return fail("--crate-name needs a name"),
      },
      "--help" | "-h" => {
        println!(
          "source-census [--crate-root {DEFAULT_ROOT}] [--crate-name {DEFAULT_CRATE}] \
           [--verbose] [--selftest]"
        );
        return ExitCode::SUCCESS;
      }
      other => return fail(&format!("unknown argument {other:?}")),
    }
  }

  if run_selftest {
    return match selftest::run() {
      Ok(cases) => {
        println!("source-census selftest OK: {cases} cases");
        ExitCode::SUCCESS
      }
      Err(problems) => {
        error("the selftest did not behave as `rule`'s header claims");
        for problem in problems {
          println!("  - {problem}");
        }
        ExitCode::FAILURE
      }
    };
  }

  // A gate that only works from one directory is a gate someone runs from the wrong one and
  // reads the error as noise. The default path is tried against the working directory first and
  // then against this crate's own location, which is fixed relative to the repository.
  if !root.is_file() && root == Path::new(DEFAULT_ROOT) {
    let beside = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("..")
      .join("..")
      .join(DEFAULT_ROOT);
    if beside.is_file() {
      root = beside;
    }
  }
  if !root.is_file() {
    return fail(&format!(
      "no such crate root: {} — run this from the repository root, or pass --crate-root",
      root.display()
    ));
  }

  let mut report = match census::detect(&root, &crate_name) {
    Ok(report) => report,
    Err(message) => return fail(&message),
  };
  census::reconcile(&mut report);
  render(&report, &root, verbose);

  if verdict(&report) {
    ExitCode::SUCCESS
  } else {
    ExitCode::FAILURE
  }
}

const RULE: &str =
  "────────────────────────────────────────────────────────────────────────────────────────────";

fn render(report: &Report, root: &Path, verbose: bool) {
  println!();
  println!("── Source genericity census ─{}", &RULE[..RULE.len() - 84]);
  println!("crate root: {}", root.display());
  println!(
    "read: {} files, {} publicly reachable modules, {} public entries, {} parameters",
    report.files_read,
    report.public_modules.len(),
    report.entries_read,
    report.params_read,
  );
  println!(
    "source positions derived from the crate ({}): {}",
    report.source_positions.len(),
    render_positions(report),
  );
  println!(
    "item-position macro invocations, whose expansions this tool cannot see: {}",
    report.macro_invocations,
  );
  render_macro_templates(report);

  section(
    report,
    Verdict::Neutral,
    "NEUTRAL",
    "a byte view; `S: AsRef<[u8]>` already produces one",
    verbose,
  );
  section(
    report,
    Verdict::Datum,
    "DATUM",
    "concrete text that is not the document",
    true,
  );

  let narrowed: Vec<usize> = report
    .observations
    .iter()
    .enumerate()
    .filter(|(_, o)| o.verdict == Verdict::Narrowed)
    .map(|(i, _)| i)
    .collect();
  println!();
  println!(
    "NARROWED — concrete text standing where the crate elsewhere puts a source type: {}",
    narrowed.len()
  );
  for (index, exemption) in &report.recorded {
    let o = &report.observations[*index];
    println!(
      "  ! [{}] {}::{}  {}: {} <{}>{}",
      exemption.label(),
      o.module,
      o.entry,
      o.param,
      o.ty,
      cost_tag(o.cost),
      inner_text(o),
    );
    println!("      {}", o.location);
    println!("      {}", o.why);
    println!("      reason: {}", squash(exemption.reason));
  }
  for index in &report.unexplained {
    let o = &report.observations[*index];
    println!(
      "  x [UNEXPLAINED] {}::{}  {}: {} <{}>{}",
      o.module,
      o.entry,
      o.param,
      o.ty,
      cost_tag(o.cost),
      inner_text(o)
    );
    println!("      {}", o.location);
    println!("      {}", o.why);
  }
  println!("{RULE}");
  println!();
}

fn section(report: &Report, want: Verdict, title: &str, gloss: &str, show: bool) {
  let rows: Vec<_> = report
    .observations
    .iter()
    .filter(|o| o.verdict == want)
    .collect();
  println!();
  println!("{title} — {gloss}: {}", rows.len());
  if !show {
    println!("  (listed with --verbose)");
    return;
  }
  for o in rows {
    println!(
      "  . {}::{}  {}: {}{}",
      o.module,
      o.entry,
      o.param,
      o.ty,
      inner_text(o)
    );
    println!("      {}", o.why);
  }
}

/// What step 1 of the rule charged this parameter.
fn cost_tag(cost: Cost) -> &'static str {
  match cost {
    Cost::None => "bytes",
    Cost::Utf8 => "utf-8",
    Cost::Representation => "owned",
  }
}

/// `  [via Cow<'static, str>]` when the text type is nested inside a larger parameter type.
fn inner_text(o: &census::Observation) -> String {
  if o.text == o.ty {
    String::new()
  } else {
    format!("  [via {}]", o.text)
  }
}

/// The one place the census cannot see, measured rather than described.
fn render_macro_templates(report: &Report) {
  let unguarded: Vec<_> = report
    .macro_templates
    .iter()
    .filter(|t| !t.konstant && !t.doc_hidden)
    .collect();
  println!(
    "`pub fn` templates inside `macro_rules!` taking concrete text: {} ({} constant, {} \
     `doc(hidden)`, {} neither)",
    report.macro_templates.len(),
    report.macro_templates.iter().filter(|t| t.konstant).count(),
    report
      .macro_templates
      .iter()
      .filter(|t| !t.konstant && t.doc_hidden)
      .count(),
    unguarded.len(),
  );
  for template in &report.macro_templates {
    let verdict = if template.konstant {
      "constant"
    } else if template.doc_hidden {
      "`doc(hidden)`, so not API"
    } else {
      "NOT GUARDED"
    };
    println!(
      "    {}! {}: {}  ({}:{}) — {verdict}",
      template.macro_name, template.param, template.rendered, template.file, template.line
    );
  }
}

fn render_positions(report: &Report) -> String {
  let mut out: Vec<String> = report
    .source_positions
    .iter()
    .map(|(family, indices)| {
      let list: Vec<String> = indices.iter().map(usize::to_string).collect();
      format!("{family}<{}>", list.join(","))
    })
    .collect();
  out.sort();
  if out.is_empty() {
    return "none".to_string();
  }
  out.join(" ")
}

/// Prints every reason the run fails, and returns whether it passed.
fn verdict(report: &Report) -> bool {
  let mut ok = true;

  for problem in &report.table_problems {
    error(&format!("the exemption table is not usable: {problem}"));
    ok = false;
  }

  if report.entries_read == 0 || report.params_read == 0 {
    error(
      "the census read no public entries, or no parameters. It would then pass by having nothing \
       to check, which is the defect it exists to catch — the reader has stopped finding the \
       crate's surface",
    );
    ok = false;
  }

  if !report.unexplained.is_empty() {
    error(&format!(
      "{} public {} source text at a concrete type and {} not recorded",
      report.unexplained.len(),
      plural(
        report.unexplained.len(),
        "parameter takes",
        "parameters take"
      ),
      plural(report.unexplained.len(), "is", "are"),
    ));
    for index in &report.unexplained {
      let o = &report.observations[*index];
      println!(
        "  - {}::{}  {}: {}  ({})",
        o.module, o.entry, o.param, o.ty, o.location
      );
      println!("    {}", o.why);
    }
    println!(
      "  Either make the entry generic over the source — `S: AsRef<[u8]>`, as §3, syntactic §5 \
       and §6/§7 already are — or record it in ci/source_census/src/exempt.rs with a reason and \
       the issue that owns it. Deleting the parameter from the census is not one of the options: \
       there is nowhere to delete it from."
    );
    ok = false;
  }

  let unguarded: Vec<_> = report
    .macro_templates
    .iter()
    .filter(|t| !t.konstant && !t.doc_hidden)
    .collect();
  if !unguarded.is_empty() {
    error(&format!(
      "{} `pub fn` {} inside a `macro_rules!` source text at a concrete type",
      unguarded.len(),
      plural(unguarded.len(), "template takes", "templates take"),
    ));
    for template in &unguarded {
      println!(
        "  - {}! {}: {}  ({}:{})",
        template.macro_name, template.param, template.rendered, template.file, template.line
      );
    }
    println!(
      "  A macro is where a narrowing multiplies: `lossless_drivers!` alone expands into fourteen \
       modules. The census cannot read the items a macro produces, so it reads the template \
       instead — make the parameter generic over the source, or make the expansion \
       `#[doc(hidden)]` if it is not API, or `&'\''static str` if it is a constant."
    );
    ok = false;
  }

  if !report.stale.is_empty() {
    error(&format!(
      "{} {} in the exemption table {} nothing in the crate",
      report.stale.len(),
      plural(report.stale.len(), "entry", "entries"),
      plural(report.stale.len(), "matches", "match"),
    ));
    for exemption in &report.stale {
      println!(
        "  - {}::{}  {}",
        exemption.module, exemption.entry, exemption.param
      );
    }
    println!(
      "  A stale exemption is not harmless: it is a recorded narrowing that no longer exists, or \
       a reader that has stopped seeing the entry. If the door was widened, delete the line — \
       that is how this table is made to shrink. If it was not, the census is no longer reading \
       what it thinks it is."
    );
    ok = false;
  }

  if ok {
    let misfires = report
      .recorded
      .iter()
      .filter(|(_, e)| e.kind == exempt::Kind::NotSource)
      .count();
    println!(
      "source-census OK: {} public entries, {} parameters, {} narrowed and all recorded with a \
       reason ({} tracked, {} structural, {misfires} where the rule misfired)",
      report.entries_read,
      report.params_read,
      report.recorded.len(),
      report
        .recorded
        .iter()
        .filter(|(_, e)| e.kind == exempt::Kind::Tracked)
        .count(),
      report
        .recorded
        .iter()
        .filter(|(_, e)| e.kind == exempt::Kind::Structural)
        .count(),
    );
  }
  ok
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

fn fail(message: &str) -> ExitCode {
  error(message);
  ExitCode::FAILURE
}
