//! Proves every branch of the verdict can fire, against synthetic crates.
//!
//! A check that has only ever been green proves nothing, and this repository has shipped several
//! (al8n/smear#73, #122). So the cases below are written as crates and run through the same
//! [`crate::census::detect`] a real run uses — the file walk, the re-export
//! resolution, the source-position derivation and the four tests, end to end. Nothing is stubbed;
//! if the reader stops finding public entries, these fail first.
//!
//! Two of the cases are the discrimination al8n/smear#122 asks for by name: `Executor::start`'s
//! `operation: Option<&str>` must come out a datum and the three narrowed doors must come out
//! findings, in signatures shaped like the real ones.

use std::{
  fs,
  path::{Path, PathBuf},
};

use crate::{
  census::{self, Verdict},
  exempt,
};

/// `(entry, parameter, verdict, a fragment the explanation must contain)`.
///
/// The fragment is what keeps a case from passing for the wrong reason: several of these could
/// come out with the right verdict off the wrong test, and a rule that is right by accident is
/// one refactor away from being wrong in silence.
type Want = (&'static str, &'static str, Verdict, &'static str);

struct Case {
  name: &'static str,
  source: &'static str,
  wants: &'static [Want],
  /// Entries the census must NOT report at all, at any verdict.
  absent: &'static [&'static str],
}

/// A miniature of the crate's real shape: a source-generic substrate, a source-generic executor,
/// and doors on top of it.
const SUBSTRATE: &str = r#"
pub mod ast {
  pub struct ExecutableDocument<S> { pub source: S }
  pub struct Parse { pub green: u32 }
  pub struct Schema { pub names: u32 }
}
pub mod sink {
  pub trait Sink<S> { fn emit(&mut self, value: S); }
}
pub mod syntactic {
  use super::ast::{ExecutableDocument, Schema};
  use super::sink::Sink;
  pub fn validate_executable<S, K>(schema: &Schema, document: &ExecutableDocument<S>, sink: &mut K)
  where
    S: AsRef<[u8]> + Clone,
    K: Sink<S>,
  { let _ = (schema, document, sink); }
}
"#;

const CASES: &[Case] = &[
  Case {
    name: "a free entry taking the document at `&str` is a narrowing",
    source: r#"
      pub mod door {
        use crate::ast::Parse;
        pub fn validate_schema_lossless(parse: &Parse, source: &str) -> u32 {
          let _ = (parse, source);
          0
        }
      }
    "#,
    wants: &[(
      "validate_schema_lossless",
      "source",
      Verdict::Narrowed,
      "test 2",
    )],
    absent: &[],
  },
  Case {
    name: "a source-generic family applied to a concrete text type is a narrowing (test 1)",
    source: r#"
      pub mod door {
        use crate::ast::Parse;
        use crate::sink::Sink;
        pub fn validate_executable_lossless<'src, K>(parse: &Parse, buffer: &'src str, sink: &mut K)
        where
          K: Sink<&'src str>,
        { let _ = (parse, buffer, sink); }
      }
    "#,
    // `buffer` is in no lexicon and the entry holds nothing generically, so only the
    // `Sink<&'src str>` bound can convict it. That is test 1 on its own.
    wants: &[(
      "validate_executable_lossless",
      "buffer",
      Verdict::Narrowed,
      "test 1 — `Sink` carries a source position",
    )],
    absent: &[],
  },
  Case {
    name: "a lookup key on a source-generic receiver is a datum (test 3, `Executor::start`)",
    source: r#"
      pub mod exec {
        use crate::ast::{ExecutableDocument, Schema};
        pub struct Executor<'a, S, V> {
          pub schema: &'a Schema,
          pub document: &'a ExecutableDocument<S>,
          pub values: core::marker::PhantomData<V>,
        }
        impl<'a, S, V> Executor<'a, S, V>
        where
          S: AsRef<[u8]>,
        {
          pub fn start(&mut self, operation: Option<&str>, root: u32) -> Result<(), u32> {
            let _ = (operation, root);
            Ok(())
          }
          pub fn handle_field_error(&mut self, id: u32, message: &str) { let _ = (id, message); }
        }
      }
    "#,
    wants: &[
      ("Executor::start", "operation", Verdict::Datum, "test 3"),
      (
        "Executor::handle_field_error",
        "message",
        Verdict::Datum,
        "test 3",
      ),
    ],
    absent: &[],
  },
  Case {
    name: "the lexicon convicts a document handed to a source-generic receiver (test 2 beats 3)",
    source: r#"
      pub mod exec {
        use crate::ast::{ExecutableDocument, Schema};
        pub struct Executor<'a, S, V> {
          pub schema: &'a Schema,
          pub document: &'a ExecutableDocument<S>,
          pub values: core::marker::PhantomData<V>,
        }
        impl<'a, S, V> Executor<'a, S, V>
        where
          S: AsRef<[u8]>,
        {
          pub fn parse_extension(&mut self, sdl: &str) { let _ = sdl; }
        }
      }
    "#,
    wants: &[(
      "Executor::parse_extension",
      "sdl",
      Verdict::Narrowed,
      "test 2",
    )],
    absent: &[],
  },
  Case {
    name: "a byte view imposes nothing and is neutral",
    source: r#"
      pub mod repr {
        pub struct Table { pub n: u32 }
        impl Table {
          pub fn sym(&self, bytes: &[u8]) -> Option<u32> { let _ = bytes; None }
        }
      }
    "#,
    wants: &[("Table::sym", "bytes", Verdict::Neutral, "byte view")],
    absent: &[],
  },
  Case {
    name: "an owned representation is a narrowing even though it is not `&str`",
    source: r#"
      pub mod door {
        pub fn load(payload: alloc_shim::Vec<u8>) -> u32 { let _ = payload; 0 }
      }
      pub mod alloc_shim { pub use std::vec::Vec; }
    "#,
    wants: &[("load", "payload", Verdict::Narrowed, "test 2")],
    absent: &[],
  },
  Case {
    name: "`&mut` is an output buffer and `'static` is a constant",
    source: r#"
      pub mod door {
        pub fn render(out: &mut String, space: &'static str) { let _ = (out, space); }
      }
    "#,
    wants: &[
      ("render", "out", Verdict::Datum, "output buffer"),
      ("render", "space", Verdict::Datum, "'static"),
    ],
    absent: &[],
  },
  Case {
    name: "a private entry and a `#[doc(hidden)]` entry are not public surface",
    source: r#"
      mod hidden_module {
        pub fn invisible(source: &str) { let _ = source; }
      }
      pub mod visible {
        #[doc(hidden)]
        pub fn also_invisible(source: &str) { let _ = source; }
        pub(crate) fn not_public(source: &str) { let _ = source; }
      }
    "#,
    wants: &[],
    absent: &["invisible", "also_invisible", "not_public"],
  },
  Case {
    name: "a `pub use` of a private module's item makes that item public surface",
    source: r#"
      pub mod outer {
        mod inner;
        pub use inner::reexported;
      }
    "#,
    wants: &[("reexported", "source", Verdict::Narrowed, "test 2")],
    absent: &[],
  },
  Case {
    name: "an entry generic over the source reports nothing at all",
    source: r#"
      pub mod door {
        use crate::ast::ExecutableDocument;
        pub fn validate<S: AsRef<[u8]>>(document: &ExecutableDocument<S>, name: &[u8]) -> u32 {
          let _ = (document, name);
          0
        }
      }
    "#,
    wants: &[("validate", "name", Verdict::Neutral, "byte view")],
    absent: &[],
  },
];

/// The extra file the `pub use` case needs, keyed by the case's module layout.
const INNER_RS: &str = r#"
pub fn reexported(source: &str) -> u32 { let _ = source; 0 }
pub fn not_reexported(source: &str) -> u32 { let _ = source; 0 }
"#;

/// Runs every case and returns how many ran, or the list of disagreements.
pub fn run() -> Result<usize, Vec<String>> {
  let mut problems = Vec::new();
  let root = scratch_dir();

  for (index, case) in CASES.iter().enumerate() {
    let dir = root.join(format!("case{index}"));
    if let Err(message) = lay_out(&dir, case) {
      problems.push(format!("{}: {message}", case.name));
      continue;
    }
    let report = match census::detect(&dir.join("lib.rs"), "probe") {
      Ok(report) => report,
      Err(message) => {
        problems.push(format!(
          "{}: the census could not read it: {message}",
          case.name
        ));
        continue;
      }
    };
    for (entry, param, want, because) in case.wants {
      let found = report
        .observations
        .iter()
        .find(|o| o.entry == *entry && o.param == *param);
      match found {
        None => problems.push(format!(
          "{}: `{entry}`'s `{param}` was not reported at all — the reader did not reach it, so \
           the case tested nothing",
          case.name
        )),
        Some(o) if o.verdict != *want => problems.push(format!(
          "{}: `{entry}`'s `{param}` came out {:?}, wanted {want:?} ({})",
          case.name, o.verdict, o.why
        )),
        Some(o) if !o.why.contains(because) => problems.push(format!(
          "{}: `{entry}`'s `{param}` came out {want:?} for the wrong reason — wanted {because:?}, \
           got {:?}",
          case.name, o.why
        )),
        Some(_) => {}
      }
    }
    for entry in case.absent {
      if report.observations.iter().any(|o| o.entry == *entry) {
        problems.push(format!(
          "{}: `{entry}` was reported, and it is not public surface",
          case.name
        ));
      }
    }
  }

  problems.extend(table_cases());
  let _ = fs::remove_dir_all(&root);

  if problems.is_empty() {
    Ok(CASES.len() + TABLE_CASES)
  } else {
    Err(problems)
  }
}

/// How many checks [`table_cases`] makes, for the count the selftest prints.
const TABLE_CASES: usize = 3;

/// The table's own guards, exercised against deliberately broken records.
fn table_cases() -> Vec<String> {
  let mut problems = Vec::new();

  if !exempt::validate().is_empty() {
    problems.push(format!(
      "the shipped exemption table does not pass its own validation: {:?}",
      exempt::validate()
    ));
  }

  let short = exempt::Exemption {
    module: "m",
    entry: "e",
    param: "p",
    kind: exempt::Kind::Structural,
    issue: None,
    reason: "rowan",
  };
  if exempt::check_one(&short, 0).is_empty() {
    problems.push(
      "a one-word reason passed validation — an exemption could then be recorded without an \
       argument, which is the omission this table exists to prevent"
        .to_string(),
    );
  }

  let untracked = exempt::Exemption {
    module: "m",
    entry: "e",
    param: "p",
    kind: exempt::Kind::Tracked,
    issue: None,
    reason: "A narrowing that is expected to go away once the door is widened, some day, \
             by somebody.",
  };
  if exempt::check_one(&untracked, 0).is_empty() {
    problems.push(
      "a tracked narrowing with no issue passed validation — that is debt with nowhere to be paid"
        .to_string(),
    );
  }

  problems
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
    "source-census-selftest-{stamp}-{}",
    std::process::id()
  ));
  let _ = fs::create_dir_all(&dir);
  dir
}
