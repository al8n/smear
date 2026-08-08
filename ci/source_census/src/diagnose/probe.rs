//! D2: the inventory is derived by `syn`, and the property is decided by `rustc`.
//!
//! # Why `syn` is not allowed to answer this one
//!
//! Everything else in this tool is a judgement about source text, and source text is what `syn`
//! reads. "Does `T` implement `Diagnose`" is not that kind of question. `impl Diagnose for
//! DiagnosticDisplay<'_, S> where S: AsRef<[u8]>` is a token sequence that says the contract holds
//! for *some* `S`; whether it holds at a call site depends on the bound, on a `cfg`, on a
//! coherence rule, and on a blanket impl in another crate that no reader of this crate's tokens
//! can see. A tool that greps for `impl Diagnose for X` has learnt that somebody typed it.
//!
//! So the census writes a crate. One assertion per contract row, each under the impl's own
//! generics and `where` clause — never under a `T: Diagnose` bound, which would make the whole
//! thing vacuous — and `cargo check` decides. A row nobody implemented is `E0277` naming the type,
//! which is the transcript al8n/smear#126 asks the calibration to produce.
//!
//! # Both spellings of the trait
//!
//! al8n/tokora#240 moves `Diagnose` into `tokora` and leaves `smear` re-exporting it. The probe
//! therefore asserts through *every* public spelling the crate offers, derived rather than
//! written down: the declaration's own path while the trait lives here, and the `pub use`'s path
//! once it does not. Both resolve to the same trait today because there is one; the day there are
//! two paths, each is asserted separately and a row that answers only one of them reds.

use std::{
  path::{Path, PathBuf},
  process::Command,
};

use super::Report;

/// What the generated crate was, and what `cargo` said about it.
pub struct Outcome {
  pub dir: PathBuf,
  /// Rows asserted, and spellings of the trait each was asserted through.
  pub rows: usize,
  pub spellings: usize,
  /// `None` when `cargo` could not be run at all, which is a failure and not a pass.
  pub status: Option<i32>,
  pub output: String,
  /// Set when the probe was deliberately not run, which the run then says out loud.
  pub skipped: Option<String>,
}

impl Outcome {
  /// A run that did not happen, which the verdict treats as a failure and says so.
  pub fn skipped(why: &str) -> Self {
    Self {
      dir: PathBuf::new(),
      rows: 0,
      spellings: 0,
      status: None,
      output: String::new(),
      skipped: Some(why.to_string()),
    }
  }
}

/// Writes the probe crate and `cargo check`s it.
pub fn run(
  report: &Report,
  crate_dir: &Path,
  dir: &Path,
  jobs: Option<usize>,
) -> Result<Outcome, String> {
  let rows: Vec<&super::Row> = report
    .contract
    .iter()
    .map(|index| &report.rows[*index])
    .collect();

  std::fs::create_dir_all(dir.join("src")).map_err(|e| format!("{}: {e}", dir.display()))?;
  let manifest = manifest(crate_dir)?;
  let lib = library(&rows, &report.trait_paths);
  write_if_changed(&dir.join("Cargo.toml"), &manifest)?;
  write_if_changed(&dir.join("src").join("lib.rs"), &lib)?;

  let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
  let mut command = Command::new(cargo);
  command
    .arg("check")
    .arg("--manifest-path")
    .arg(dir.join("Cargo.toml"))
    // Its own target directory, and not the one this binary was built into: the parent is locked
    // for the duration of the `cargo run` that produced this process, and a nested build sharing
    // it would wait on a lock nothing is going to release.
    .arg("--target-dir")
    .arg(dir.join("target"))
    .env_remove("RUSTFLAGS")
    .env_remove("CARGO_BUILD_JOBS");
  if let Some(jobs) = jobs {
    command.arg("--jobs").arg(jobs.to_string());
  }

  let result = command
    .output()
    .map_err(|e| format!("could not run `cargo check` on the probe crate: {e}"))?;
  let mut output = String::from_utf8_lossy(&result.stderr).into_owned();
  output.push_str(&String::from_utf8_lossy(&result.stdout));

  Ok(Outcome {
    dir: dir.to_path_buf(),
    rows: rows.len(),
    spellings: report.trait_paths.len(),
    status: result.status.code(),
    output,
    skipped: None,
  })
}

/// The probe crate's manifest, with every feature the crate under census declares.
///
/// All of them, because a `Diagnose` impl can be written under any one and a probe compiled with
/// fewer would be a check narrower than what it claims to cover. The list is read out of the
/// crate's own manifest rather than restated, so a feature added tomorrow is asserted tomorrow.
fn manifest(crate_dir: &Path) -> Result<String, String> {
  let manifest_path = crate_dir.join("Cargo.toml");
  let text = std::fs::read_to_string(&manifest_path)
    .map_err(|e| format!("{}: {e}", manifest_path.display()))?;
  let features = declared_features(&text);
  if features.is_empty() {
    return Err(format!(
      "{} declares no features, which the probe reads to know what to compile — either the \
       manifest moved or the reader stopped finding its `[features]` table",
      manifest_path.display()
    ));
  }
  let name = crate_dir
    .file_name()
    .and_then(|name| name.to_str())
    .ok_or_else(|| format!("{} has no directory name", crate_dir.display()))?;
  let list = features
    .iter()
    .map(|feature| format!("  \"{feature}\","))
    .collect::<Vec<_>>()
    .join("\n");

  Ok(format!(
    "# Generated by ci/source_census. Edits are overwritten on every run.\n\
     #\n\
     # `[workspace]` is not decoration: this crate is written under a target directory that sits\n\
     # inside the repository, so cargo would otherwise walk up, find the workspace root, fail to\n\
     # find this package in its members, and refuse to build at all.\n\
     [package]\n\
     name = \"source-census-probe\"\n\
     version = \"0.0.0\"\n\
     edition = \"2024\"\n\
     publish = false\n\
     \n\
     [workspace]\n\
     \n\
     [lib]\n\
     path = \"src/lib.rs\"\n\
     \n\
     [dependencies.{name}]\n\
     path = \"{}\"\n\
     default-features = false\n\
     features = [\n{list}\n]\n",
    crate_dir.display(),
  ))
}

/// The feature names in a manifest's `[features]` table.
///
/// A five-line reader instead of a `toml` dependency: the table is a flat list of `name = [...]`
/// at the start of a line, this crate has no other use for a TOML parser, and a wrong answer here
/// is loud — the probe fails to compile with `unknown feature`.
fn declared_features(manifest: &str) -> Vec<String> {
  let mut out = Vec::new();
  let mut inside = false;
  for line in manifest.lines() {
    let trimmed = line.trim_end();
    if trimmed.starts_with('[') {
      inside = trimmed == "[features]";
      continue;
    }
    if !inside {
      continue;
    }
    // Only a line that starts in column zero declares a feature; a continuation of a multi-line
    // array is indented, and `default` is excluded because the probe names every feature anyway.
    let Some((name, rest)) = trimmed.split_once('=') else {
      continue;
    };
    if name.starts_with(char::is_whitespace) || !rest.trim_start().starts_with('[') {
      continue;
    }
    let name = name.trim();
    if name.is_empty() || name == "default" || !name.chars().all(|c| c.is_ascii_graphic()) {
      continue;
    }
    out.push(name.to_string());
  }
  out
}

/// The generated assertions.
fn library(rows: &[&super::Row], trait_paths: &[String]) -> String {
  let mut out = String::new();
  out.push_str(
    "// Generated by ci/source_census. Edits are overwritten on every run.\n\
     //\n\
     // One assertion per row of the diagnostic census's D1 inventory, through every public\n\
     // spelling of the contract trait. Nothing here is called: `cargo check` deciding that it\n\
     // type-checks IS the assertion.\n\
     #![allow(warnings)]\n\n",
  );

  for (index, path) in trait_paths.iter().enumerate() {
    out.push_str(&format!(
      "/// `{path}`\nfn is_diagnose_{index}<T: ?Sized + {path}>() {{}}\n\n"
    ));
  }

  for (index, row) in rows.iter().enumerate() {
    let Some(spelling) = &row.spelling else {
      out.push_str(&format!(
        "// row {index}: `{}` — the census could not spell an instantiation of it.\n\n",
        row.ident
      ));
      continue;
    };
    out.push_str(&format!("// {} ({})\n", row.ident, row.location));
    out.push_str(&format!("fn row_{index}{}()\n", spelling.generics));
    if !spelling.where_clause.is_empty() {
      out.push_str(&format!("{}\n", spelling.where_clause));
    }
    out.push_str("{\n");
    for (via, _) in trait_paths.iter().enumerate() {
      out.push_str(&format!(
        "  is_diagnose_{via}::<{}>();\n",
        spelling.ty.replace("$crate", "smear")
      ));
    }
    out.push_str("}\n\n");
  }
  out
}

/// Writes only when the bytes differ, so an unchanged probe is not rebuilt on every run.
fn write_if_changed(path: &Path, contents: &str) -> Result<(), String> {
  if std::fs::read_to_string(path).is_ok_and(|existing| existing == contents) {
    return Ok(());
  }
  std::fs::write(path, contents).map_err(|e| format!("{}: {e}", path.display()))
}

/// Where the probe crate goes: under the target directory this binary was built into.
///
/// Under the target directory rather than in the source tree because it is a build artifact — it
/// is regenerated from the crate on every run and nothing should ever edit it — and because
/// `target/` is what CI already caches and `.gitignore` already covers.
pub fn default_dir() -> Result<PathBuf, String> {
  let exe = std::env::current_exe().map_err(|e| format!("cannot locate this binary: {e}"))?;
  let mut dir = exe
    .parent()
    .ok_or_else(|| format!("{} has no parent", exe.display()))?
    .to_path_buf();
  // `<target>/debug/source-census` normally, `<target>/debug/deps/source-census-<hash>` when
  // cargo runs it as a test binary.
  if dir.file_name().is_some_and(|name| name == "deps") {
    dir.pop();
  }
  dir.pop();
  Ok(dir.join("source-census-probe"))
}
