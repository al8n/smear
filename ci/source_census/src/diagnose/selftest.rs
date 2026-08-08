//! Proves each of D1, D3 and D4 can fire, against synthetic crates.
//!
//! A check that has only ever been green proves nothing, and this repository has shipped several
//! (al8n/smear#73, #122, #126). The cases below run through the same [`super::detect`] a real run
//! uses, so nothing is stubbed: if the reader stops finding declarations, `impl` blocks or `match`
//! arms, these fail before the crate under census does.
//!
//! D2 is not here. Its verdict is `cargo check`'s, and building a crate per case would make the
//! selftest cost minutes; what is proved here is the inventory D2 consumes and the spelling it
//! writes. The compiler half is calibrated by planting a type in the real crate, which is what
//! al8n/smear#126 asks for and what the pull request records.

use std::{
  fs,
  path::{Path, PathBuf},
};

use super::exempt;
use crate::surface;

struct Case {
  name: &'static str,
  source: &'static str,
  check: fn(&super::Report) -> Vec<String>,
}

/// A miniature of the contract: the trait, one type answering it, one that does not.
const SUBSTRATE: &str = r#"
pub mod diagnostic {
  pub trait Diagnose: core::fmt::Display {
    fn code(&self) -> u32;
    fn severity(&self) -> u32;
    fn help(&self) -> Option<&'static str>;
  }
}
"#;

const CASES: &[Case] = &[
  Case {
    name: "D1: a public error type with no impl is a contract row",
    source: r#"
      pub mod door {
        pub struct DgUnansweredError { pub at: u32 }
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      match row(report, "DgUnansweredError") {
        None => problems.push("`DgUnansweredError` was not in the inventory at all".to_string()),
        Some(index) => {
          if !report.is_contract(index) {
            problems.push("`DgUnansweredError` was not a contract row".to_string());
          }
          if report.rows[index].path != "probe::door::DgUnansweredError" {
            problems.push(format!(
              "`DgUnansweredError` resolved to `{}`, wanted `probe::door::DgUnansweredError`",
              report.rows[index].path
            ));
          }
        }
      }
      problems
    },
  },
  Case {
    name: "D1: a private error type is not public surface and is not a row",
    source: r#"
      mod hidden {
        pub struct DgHiddenError { pub at: u32 }
      }
      pub mod visible {
        #[doc(hidden)]
        pub struct DgDocHiddenError { pub at: u32 }
      }
    "#,
    check: |report| {
      ["DgHiddenError", "DgDocHiddenError"]
        .iter()
        .filter(|ident| row(report, ident).is_some())
        .map(|ident| format!("`{ident}` was reported, and it is not public surface"))
        .collect()
    },
  },
  Case {
    name: "D1: a row is found through a re-export chain and named by the path a consumer writes",
    source: r#"
      pub mod outer {
        mod inner;
        pub use inner::DgChainedError;
      }
    "#,
    check: |report| match row(report, "DgChainedError") {
      None => vec!["`DgChainedError` was not found behind the re-export".to_string()],
      Some(index) if report.rows[index].path != "probe::outer::DgChainedError" => vec![format!(
        "`DgChainedError` resolved to `{}`, wanted the path the `pub use` publishes",
        report.rows[index].path
      )],
      Some(_) => Vec::new(),
    },
  },
  Case {
    name: "D1: an alias of a row is folded into it rather than counted twice",
    source: r#"
      pub mod door {
        pub struct DgBaseError<Char = char> { pub at: Char }
        pub type DgAliasError = DgBaseError<char>;
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      if row(report, "DgAliasError").is_some() {
        problems.push("`DgAliasError` was counted as a row of its own".to_string());
      }
      if !report
        .folded_aliases
        .iter()
        .any(|folded| folded.contains("DgAliasError"))
      {
        problems.push("`DgAliasError` was dropped rather than reported as folded".to_string());
      }
      if row(report, "DgBaseError").is_none() {
        problems.push("`DgBaseError` itself went missing".to_string());
      }
      problems
    },
  },
  Case {
    name: "D1: an `Err` type that is not named like an error is still a row",
    source: r#"
      pub mod door {
        pub struct DgRefusal { pub at: u32 }
        pub fn build() -> Result<u32, DgRefusal> { Ok(0) }
      }
    "#,
    check: |report| match row(report, "DgRefusal") {
      None => vec!["a public `Result`'s `Err` type was not in the inventory".to_string()],
      Some(index)
        if !report.rows[index]
          .origins
          .iter()
          .any(|origin| origin.contains("`Err`")) =>
      {
        vec![format!(
          "`DgRefusal` was a row for the wrong reason: {:?}",
          report.rows[index].origins
        )]
      }
      Some(_) => Vec::new(),
    },
  },
  Case {
    name: "D1: the contract trait resolves through a re-export of another crate's trait",
    source: r#"
      pub mod relay {
        pub use tokora::diagnostic::Diagnose;
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      // The substrate declares the trait, so both spellings must be present at once — which is
      // the state al8n/tokora#240 passes through, and the one where asserting through only the
      // first would silently stop covering the second.
      for want in ["probe::diagnostic::Diagnose", "probe::relay::Diagnose"] {
        if !report.trait_paths.iter().any(|path| path == want) {
          problems.push(format!(
            "`{want}` is not among the spellings the probe would assert through: {:?}",
            report.trait_paths
          ));
        }
      }
      problems
    },
  },
  Case {
    name: "D3: a `_ =>` arm in a registered accessor is a finding",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B }
        impl DgKind {
          pub const fn code(&self) -> u32 {
            match self {
              Self::A => 1,
              _ => 0,
            }
          }
        }
      }
    "#,
    check: |report| match report.wildcards.first() {
      None => vec!["the `_ =>` arm was not reported".to_string()],
      Some(found) if found.pattern != "_" || !found.function.ends_with("::code") => {
        vec![format!(
          "the wildcard was reported as `{}` in `{}`",
          found.pattern, found.function
        )]
      }
      Some(_) => Vec::new(),
    },
  },
  Case {
    name: "D3: a bare binding arm is a catch-all too, and an unlisted accessor is not read",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B }
        impl DgKind {
          pub const fn help(&self) -> Option<&'static str> {
            match self {
              Self::A => Some("a"),
              other => None,
            }
          }
          pub const fn message(&self) -> &'static str {
            match self {
              Self::A => "a",
              _ => "b",
            }
          }
        }
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      if !report
        .wildcards
        .iter()
        .any(|w| w.pattern == "other" && w.function.ends_with("::help"))
      {
        problems.push(format!(
          "the binding arm in `help` was not reported: {:?}",
          report
            .wildcards
            .iter()
            .map(|w| (&w.function, &w.pattern))
            .collect::<Vec<_>>()
        ));
      }
      if report
        .wildcards
        .iter()
        .any(|w| w.function.ends_with("::message"))
      {
        problems.push(
          "`message` is not an accessor the contract's enforcement runs through, and it was read \
           anyway"
            .to_string(),
        );
      }
      problems
    },
  },
  Case {
    name: "D3: a wildcard-free match over every variant is not a finding",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B }
        impl DgKind {
          pub const fn severity(&self) -> u32 {
            match self {
              Self::A => 1,
              Self::B => 2,
            }
          }
        }
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      if !report.wildcards.is_empty() {
        problems.push(format!(
          "an exhaustive match was reported as a wildcard: {:?}",
          report
            .wildcards
            .iter()
            .map(|w| &w.pattern)
            .collect::<Vec<_>>()
        ));
      }
      if report.matches_read == 0 {
        problems.push("the accessor's match was not read at all, so nothing was checked".into());
      }
      problems
    },
  },
  Case {
    name: "D4: an inventory that omits a variant is a finding, and names it",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B, C }
        impl DgKind {
          pub const ALL: [Self; 2] = [Self::A, Self::B];
        }
      }
    "#,
    check: |report| match report
      .inventories
      .iter()
      .find(|inventory| inventory.name == "DgKind::ALL")
    {
      None => vec!["the inventory was not found by shape at all".to_string()],
      Some(inventory) if inventory.missing != ["C"] => vec![format!(
        "the omission was reported as {:?}, wanted `C`",
        inventory.missing
      )],
      Some(_) => Vec::new(),
    },
  },
  Case {
    name: "D4: a name the enum does not declare, and one listed twice, are findings",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B }
        impl DgKind {
          pub const EVERY: &'static [DgKind] = &[DgKind::A, DgKind::A, DgKind::B];
        }
      }
    "#,
    check: |report| match report
      .inventories
      .iter()
      .find(|inventory| inventory.name == "DgKind::EVERY")
    {
      None => vec![
        "an inventory not named `ALL` was not found — D4 derives by shape, so the name must not \
         matter"
          .to_string(),
      ],
      Some(inventory) if inventory.spurious != ["A (listed 2 times)"] => vec![format!(
        "the duplicate was reported as {:?}",
        inventory.spurious
      )],
      Some(_) => Vec::new(),
    },
  },
  Case {
    name: "D4: a complete inventory passes, and a constant that is not one is not read",
    source: r#"
      pub mod kind {
        pub enum DgKind { A, B }
        impl DgKind {
          pub const ALL: [Self; 2] = [Self::A, Self::B];
          pub const FIRST: Self = Self::A;
          pub const NAMES: [&'static str; 2] = ["a", "b"];
        }
      }
    "#,
    check: |report| {
      let mut problems = Vec::new();
      match report
        .inventories
        .iter()
        .find(|inventory| inventory.name == "DgKind::ALL")
      {
        None => problems.push("the complete inventory was not found".to_string()),
        Some(inventory) if !inventory.missing.is_empty() || !inventory.spurious.is_empty() => {
          problems.push(format!(
            "a complete inventory was reported incomplete: missing {:?}, spurious {:?}",
            inventory.missing, inventory.spurious
          ));
        }
        Some(_) => {}
      }
      if report.inventories.len() != 1 {
        problems.push(format!(
          "a constant that enumerates nothing was read as an inventory: {:?}",
          report
            .inventories
            .iter()
            .map(|inventory| &inventory.name)
            .collect::<Vec<_>>()
        ));
      }
      problems
    },
  },
];

/// The extra file the re-export case needs.
const INNER_RS: &str = r#"
pub struct DgChainedError { pub at: u32 }
"#;

/// How many checks [`table_cases`] makes, for the count the selftest prints.
const TABLE_CASES: usize = 4;

pub fn run() -> Result<usize, Vec<String>> {
  let mut problems = Vec::new();
  let root = scratch_dir();

  for (index, case) in CASES.iter().enumerate() {
    let dir = root.join(format!("case{index}"));
    if let Err(message) = lay_out(&dir, case) {
      problems.push(format!("{}: {message}", case.name));
      continue;
    }
    let surface = match surface::load(&dir.join("lib.rs"), "probe") {
      Ok(surface) => surface,
      Err(message) => {
        problems.push(format!("{}: could not read it: {message}", case.name));
        continue;
      }
    };
    let report = super::detect(&surface);
    problems.extend(
      (case.check)(&report)
        .into_iter()
        .map(|problem| format!("{}: {problem}", case.name)),
    );
  }

  problems.extend(table_cases());
  let _ = fs::remove_dir_all(&root);

  if problems.is_empty() {
    Ok(CASES.len() + TABLE_CASES)
  } else {
    Err(problems)
  }
}

/// The table's own guards, exercised against deliberately broken records.
fn table_cases() -> Vec<String> {
  let mut problems = Vec::new();

  if !exempt::validate().is_empty() {
    problems.push(format!(
      "the shipped contract exemption table does not pass its own validation: {:?}",
      exempt::validate()
    ));
  }

  let short = exempt::Exemption {
    path: "probe::T",
    kind: exempt::Kind::Verdict,
    element: None,
    issue: None,
    reason: "a verdict",
  };
  if exempt::check_one(&short, 0).is_empty() {
    problems.push(
      "a two-word reason passed validation — a type could then be recorded as outside the \
       contract without an argument, which is the omission this table exists to prevent"
        .to_string(),
    );
  }

  let unnamed = exempt::Exemption {
    path: "probe::T",
    kind: exempt::Kind::Aggregate,
    element: None,
    issue: None,
    reason: "A collection of diagnostics, whose elements answer the contract instead of it.",
  };
  if exempt::check_one(&unnamed, 0).is_empty() {
    problems.push(
      "an aggregate with no element passed validation — the record would then claim its elements \
       answer without saying which elements"
        .to_string(),
    );
  }

  let untracked = exempt::Exemption {
    path: "probe::T",
    kind: exempt::Kind::Tracked,
    element: None,
    issue: None,
    reason: "A family that will join the contract at some point, once somebody gets to it.",
  };
  if exempt::check_one(&untracked, 0).is_empty() {
    problems.push(
      "a tracked family with no issue passed validation — that is debt with nowhere to be paid"
        .to_string(),
    );
  }

  problems
}

fn row(report: &super::Report, ident: &str) -> Option<usize> {
  report.rows.iter().position(|row| row.ident == ident)
}

fn lay_out(dir: &Path, case: &Case) -> Result<(), String> {
  let _ = fs::remove_dir_all(dir);
  fs::create_dir_all(dir).map_err(|e| e.to_string())?;
  let lib = format!("{SUBSTRATE}\n{}\n", case.source);
  fs::write(dir.join("lib.rs"), lib).map_err(|e| e.to_string())?;
  if case.source.contains("mod inner;") {
    fs::create_dir_all(dir.join("outer")).map_err(|e| e.to_string())?;
    fs::write(dir.join("outer").join("inner.rs"), INNER_RS).map_err(|e| e.to_string())?;
  }
  Ok(())
}

fn scratch_dir() -> PathBuf {
  let stamp = std::time::SystemTime::now()
    .duration_since(std::time::UNIX_EPOCH)
    .map(|d| d.as_nanos())
    .unwrap_or_default();
  let dir = std::env::temp_dir().join(format!(
    "diagnose-census-selftest-{stamp}-{}",
    std::process::id()
  ));
  let _ = fs::create_dir_all(&dir);
  dir
}
