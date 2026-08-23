//! Who calls `drain_unless_stopped` and `root_turn`, derived from the tokens rather than believed.
//!
//! # Why this claim and not another
//!
//! `smear-parser/src/lossless/depth.rs` documents the two functions by naming their callers, and
//! that sentence has been **wrong three times running** on one branch: five roots, then six roots,
//! then six roots plus the driver macro — each revision written by a sweep that reported itself
//! complete, and each missing the in-crate cells that drive the pair directly. A caller set is
//! exactly the kind of claim prose cannot hold: it is a statement about the whole tree, it changes
//! whenever anyone adds a test, and nothing in a build says a word when it goes stale.
//!
//! So the claim is stated once, here, as data, and the prose in `depth.rs` points at it.
//!
//! # Why the tokens and not a grep
//!
//! Six files hold a call, and in five of them **every call is inside a macro body**: all twelve
//! shipped calls are written inside a `lossless_production!` invocation, and the driver macro's
//! two are inside `lossless_drivers!`'s own transcriber. `syn` parses a macro invocation's body as
//! an opaque `TokenStream`, so an AST walk over `Expr` sees none of the fourteen — a visitor-based
//! census would find eleven calls in one test file, report five files as caller-free, and look
//! completely healthy doing it.
//!
//! A substring scan has the opposite failure and this workspace has been bitten by it: `///` prose
//! naming a function reads as a call, and a local bound to the same name reads as a call too.
//!
//! [`scan`] is neither. It parses each file with `syn::parse_file` — a file it cannot parse is a
//! finding, not a silent zero — and then walks the file's **token tree**, descending into macro
//! bodies like any other group. A comment is not a token, so prose cannot be mistaken for a call;
//! a doc comment is a `#[doc = "…"]` string literal, likewise. An occurrence is a call only when
//! the identifier is followed by a parenthesised group or a turbofish, and every occurrence that
//! is neither a call, the declaration, nor an import is reported as **unclassified** rather than
//! ignored — the census refuses what it cannot read.
//!
//! # What a change to [`DECLARED`] means
//!
//! Editing that table is the decision being recorded, and the diff to those lines is the thing a
//! reviewer is meant to see. A row added means *a new frame now mints or spends a root verdict* —
//! the invariant `depth.rs` spends its length on — and it belongs in the same commit as the
//! caller, beside whatever note says why that frame gets one. A row deleted means a caller went
//! away, and the census reddens either way round: an undeclared caller and a declared caller that
//! is no longer there are both findings, so the table cannot drift in either direction.
//!
//! It is keyed by `(callee, file, owner)` and carries a count, because a total cannot see a swap.
//! The three sweeps that got this wrong all got the *number* nearly right.

use std::{
  collections::BTreeMap,
  fs,
  path::{Path, PathBuf},
};

use proc_macro2::{Delimiter, Spacing, TokenStream, TokenTree};

/// The directory the census reads.
///
/// One crate, and that is not an assumption: both functions are `pub(crate)`, and the declaration
/// each scan finds carries the spelling in [`Hit::visibility`], which is checked. So no file
/// outside this crate can hold a call at all; if either ever becomes `pub`, that check fails
/// beside the caller table — the boundary this root rests on is itself a thing that reddens.
const ROOT: &str = "smear-parser/src";

/// The two functions whose caller set is pinned.
const WATCHED: &[&str] = &["drain_unless_stopped", "root_turn"];

/// The file both are declared in, which is also the only file allowed to declare them.
const DECLARED_IN: &str = "smear-parser/src/lossless/depth.rs";

/// Fewer `.rs` files than this under [`ROOT`] and the census is looking at the wrong tree.
///
/// A floor rather than an equality: files are added often and the point of the number is that a
/// scan which suddenly reads three files cannot report an empty caller set as agreement.
const FILE_FLOOR: usize = 60;

/// One declared caller: `(callee, file, owner, calls)`.
///
/// `owner` is the enclosing named item as [`scan`] names it — the `fn`/`mod` path, `::`-joined,
/// or the `macro_rules!` name when the call is written inside a macro definition, where the
/// function it will land in has no name yet.
type Declared = (&'static str, &'static str, &'static str, usize);

/// Every caller of [`WATCHED`], in four families.
///
/// - **The six `*_entry` productions** call `drain_unless_stopped` and nothing else. One per
///   document root per dialect; each is the production a dialect's `parse_*` entry point runs, and
///   each is one line long. These are the callers `depth.rs` has always listed.
/// - **The six document roots** call `root_turn`, once per root, inside the loop that walks
///   entries. `document` and `type_system_document` in each dialect, plus each dialect's
///   `executable_document`.
/// - **The driver macro** calls both, once each, inside `lossless_drivers!`'s transcriber: a
///   driver is a root of one entry and is written as one. The owner is the macro rather than a
///   function because the function it expands to is `fn $name`, which has no name here.
/// - **The in-crate cells** in the GraphQL assembly's `tests.rs`, which drive the pair directly.
///   They are the family every previous revision of `depth.rs`'s sentence left out, and they are
///   the reason this table exists rather than another sentence. They live in the dialect assembly
///   and not beside the code they drive because gate 6 forbids a dialect-typed test under the
///   substrate — `depth.rs`'s own closing comment carries that reasoning.
///
/// Note the four distinct `drive` functions: same name, four different tests, three different
/// counts. That is why the key is a path and not a bare function name.
const DECLARED: &[Declared] = &[
  // the six `*_entry` productions
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/document.rs",
    "document_entry",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/document.rs",
    "type_system_document_entry",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/executable.rs",
    "executable_document_entry",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphqlx/lossless/document.rs",
    "document_entry",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphqlx/lossless/document.rs",
    "type_system_document_entry",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphqlx/lossless/executable.rs",
    "executable_document_entry",
    1,
  ),
  // the six document roots
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/document.rs",
    "document",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/document.rs",
    "type_system_document",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/executable.rs",
    "executable_document",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphqlx/lossless/document.rs",
    "document",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphqlx/lossless/document.rs",
    "type_system_document",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphqlx/lossless/executable.rs",
    "executable_document",
    1,
  ),
  // the driver macro's own driver
  (
    "drain_unless_stopped",
    "smear-parser/src/lossless/macros.rs",
    "lossless_drivers",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/lossless/macros.rs",
    "lossless_drivers",
    1,
  ),
  // the in-crate cells that drive the pair directly
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_refusal_is_the_error_returned_even_under_a_rejecting_emitter::run",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "each_term_of_a_roots_stop_is_alone_on_a_population::drive",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/tests.rs",
    "each_term_of_a_roots_stop_is_alone_on_a_population::drive",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_caught_trip_does_not_silence_a_later_failures_drain::root",
    3,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_caught_trip_does_not_silence_a_later_failures_drain::drive",
    1,
  ),
  (
    "root_turn",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it::inner",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it::outer",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_nested_drains_stop_is_not_reclassified_by_the_drain_above_it::drive",
    1,
  ),
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_terminal_failure_no_turn_classified_stops_the_drain_on_the_trait_alone::drive",
    1,
  ),
];

/// What one occurrence of a watched identifier turned out to be.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Occurrence {
  /// Followed by a parenthesised group, or by a turbofish and then one.
  Call,
  /// `fn drain_unless_stopped`, in the file that declares it.
  Declaration,
  /// Inside a `use` tree.
  Import,
  /// None of the above. The census does not know what this is, so it is a finding.
  Unclassified,
}

/// One occurrence, with where it is and what encloses it.
pub struct Hit {
  pub callee: String,
  pub file: String,
  pub owner: String,
  pub line: usize,
  pub what: Occurrence,
  /// The declaration's visibility, verbatim, for [`Occurrence::Declaration`].
  pub visibility: String,
}

pub struct Report {
  pub files_read: usize,
  pub hits: Vec<Hit>,
  /// Files the census could not read as Rust, with the reason.
  pub unreadable: Vec<String>,
  /// Every way the tree and [`DECLARED`] disagree, and every occurrence that is neither a call,
  /// a declaration nor an import.
  pub findings: Vec<String>,
}

/// Where the repository root is, from wherever this was run.
///
/// The same two candidates `main` tries for its own crate roots: the working directory first, so
/// a run from the repository root works, then this crate's own location, which is fixed relative
/// to the repository, so `cargo test -p source-census` works too.
pub fn repository_root() -> Result<PathBuf, String> {
  let here = PathBuf::from(".");
  if here.join(ROOT).is_dir() {
    return Ok(here);
  }
  let beside = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("..")
    .join("..");
  if beside.join(ROOT).is_dir() {
    return Ok(beside);
  }
  Err(format!(
    "no {ROOT} in the working directory or beside {} — run this from the repository root",
    env!("CARGO_MANIFEST_DIR")
  ))
}

/// Reads the tree and reconciles it against [`DECLARED`].
pub fn detect(repository: &Path) -> Report {
  let mut report = Report {
    files_read: 0,
    hits: Vec::new(),
    unreadable: Vec::new(),
    findings: Vec::new(),
  };
  let mut files = match rust_files(&repository.join(ROOT)) {
    Ok(files) => files,
    Err(message) => {
      report.findings.push(message);
      return report;
    }
  };
  files.sort();
  report.files_read = files.len();

  for path in &files {
    let shown = relative(repository, path);
    let text = match fs::read_to_string(path) {
      Ok(text) => text,
      Err(e) => {
        report.unreadable.push(format!("{shown}: {e}"));
        continue;
      }
    };
    match scan(&shown, &text) {
      Ok(hits) => report.hits.extend(hits),
      Err(message) => report.unreadable.push(message),
    }
  }

  reconcile(&mut report);
  report
}

/// Every finding, derived from what [`scan`] read.
fn reconcile(report: &mut Report) {
  for message in &report.unreadable {
    report.findings.push(format!(
      "unreadable, so nothing below is a statement about it — {message}"
    ));
  }

  // NON-VACUITY FIRST. Everything after this compares two sets, and two empty sets agree.
  if report.files_read < FILE_FLOOR {
    report.findings.push(format!(
      "read {} .rs files under {ROOT}, fewer than the {FILE_FLOOR} this census was written \
       against — either the crate moved or the walk is looking in the wrong place",
      report.files_read
    ));
  }

  for name in WATCHED {
    let declarations: Vec<&Hit> = report
      .hits
      .iter()
      .filter(|hit| hit.callee == *name && hit.what == Occurrence::Declaration)
      .collect();
    match declarations.as_slice() {
      [] => report.findings.push(format!(
        "no declaration of `{name}` anywhere under {ROOT}: it was renamed or removed, and every \
         zero below is about the name rather than about the callers"
      )),
      [one] => {
        if one.file != DECLARED_IN {
          report.findings.push(format!(
            "`{name}` is declared in {} rather than {DECLARED_IN}",
            one.file
          ));
        }
        // The census reads one crate because nothing outside it can call a `pub(crate)` item.
        // A widened door is what makes that boundary wrong, so it is checked and not assumed.
        if one.visibility != "pub(crate)" {
          report.findings.push(format!(
            "`{name}` is `{}` rather than `pub(crate)` at {}:{} — the caller set below is taken \
             over one crate, and only crate privacy makes that the whole population",
            one.visibility, one.file, one.line
          ));
        }
      }
      many => report.findings.push(format!(
        "{} declarations of `{name}`, so the census cannot say which one the calls reach",
        many.len()
      )),
    }
  }

  for hit in &report.hits {
    if hit.what == Occurrence::Unclassified {
      report.findings.push(format!(
        "{}:{}: `{}` occurs inside `{}` and is neither a call, the declaration, nor an import — \
         the census will not guess",
        hit.file, hit.line, hit.callee, hit.owner
      ));
    }
  }

  let mut observed: BTreeMap<(&str, &str, &str), usize> = BTreeMap::new();
  for hit in &report.hits {
    if hit.what == Occurrence::Call {
      *observed
        .entry((&hit.callee, &hit.file, &hit.owner))
        .or_insert(0) += 1;
    }
  }
  let declared: BTreeMap<(&str, &str, &str), usize> = DECLARED
    .iter()
    .map(|(callee, file, owner, calls)| ((*callee, *file, *owner), *calls))
    .collect();
  if declared.len() != DECLARED.len() {
    report
      .findings
      .push("DECLARED holds two rows with the same key; merge their counts".to_string());
  }

  for (key, calls) in &observed {
    match declared.get(key) {
      None => report.findings.push(format!(
        "{}: `{}` is called {calls}x by `{}` and that caller is not in DECLARED",
        key.1, key.0, key.2
      )),
      Some(want) if want != calls => report.findings.push(format!(
        "{}: `{}` is called {calls}x by `{}`, and DECLARED says {want}",
        key.1, key.0, key.2
      )),
      Some(_) => {}
    }
  }
  for key in declared.keys() {
    if !observed.contains_key(key) {
      report.findings.push(format!(
        "{}: DECLARED says `{}` is called by `{}`, and no such call is there",
        key.1, key.0, key.2
      ));
    }
  }
}

/// Every occurrence of a watched identifier in one file.
///
/// `syn::parse_file` runs first and its failure is returned: a file this tool cannot read as Rust
/// must not contribute a confident zero. The walk below is over `proc_macro2`'s token tree, which
/// descends into macro bodies — where four of the five calling files keep their calls.
pub fn scan(file: &str, text: &str) -> Result<Vec<Hit>, String> {
  syn::parse_file(text).map_err(|e| format!("{file}: syn cannot parse this file: {e}"))?;
  let stream: TokenStream = text
    .parse()
    .map_err(|e| format!("{file}: this file does not tokenize: {e}"))?;
  let mut hits = Vec::new();
  walk(&stream, file, &mut Vec::new(), false, &mut hits);
  Ok(hits)
}

/// Whether a token is a `::`, taking the two-punct spelling `proc_macro2` gives it.
fn is_path_sep(tokens: &[TokenTree], at: usize) -> bool {
  matches!(tokens.get(at), Some(TokenTree::Punct(p)) if p.as_char() == ':' && p.spacing() == Spacing::Joint)
    && matches!(tokens.get(at + 1), Some(TokenTree::Punct(p)) if p.as_char() == ':')
}

/// Walks one token stream, naming what encloses each watched identifier.
///
/// `owner` is the stack of enclosing named items. A brace group is attributed to the `fn`, `mod`
/// or `macro_rules!` name that immediately precedes it; every other group inherits the stack it
/// is written in, which is what puts a call in a closure under the function that wrote the
/// closure. `fn $name` in a macro's transcriber sets no name — the function does not have one yet
/// — so those calls land on the macro.
fn walk(
  stream: &TokenStream,
  file: &str,
  owner: &mut Vec<String>,
  in_use: bool,
  out: &mut Vec<Hit>,
) {
  let tokens: Vec<TokenTree> = stream.clone().into_iter().collect();
  // The name a following brace group belongs to, and the visibility written before it.
  let mut pending: Option<String> = None;
  let mut visibility = String::new();
  let mut in_use = in_use;

  let mut i = 0;
  while i < tokens.len() {
    match &tokens[i] {
      TokenTree::Ident(ident) => {
        let name = ident.to_string();
        match name.as_str() {
          "use" => in_use = true,
          "pub" => {
            visibility = match tokens.get(i + 1) {
              Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                format!("pub({})", g.stream())
              }
              _ => "pub".to_string(),
            };
          }
          "fn" | "mod" => {
            if let Some(TokenTree::Ident(next)) = tokens.get(i + 1) {
              let declared = next.to_string();
              if name == "fn" && WATCHED.contains(&declared.as_str()) {
                out.push(Hit {
                  callee: declared.clone(),
                  file: file.to_string(),
                  owner: owner.join("::"),
                  line: next.span().start().line,
                  what: Occurrence::Declaration,
                  visibility: if visibility.is_empty() {
                    "private".to_string()
                  } else {
                    visibility.clone()
                  },
                });
              }
              pending = Some(declared);
              i += 2;
              continue;
            }
          }
          "macro_rules" => {
            if let (true, Some(TokenTree::Ident(next))) =
              (is_bang(&tokens, i + 1), tokens.get(i + 2))
            {
              pending = Some(next.to_string());
              i += 3;
              continue;
            }
          }
          _ if WATCHED.contains(&name.as_str()) => {
            let what = classify(&tokens, i, in_use);
            out.push(Hit {
              callee: name,
              file: file.to_string(),
              owner: owner.join("::"),
              line: ident.span().start().line,
              what,
              visibility: String::new(),
            });
          }
          _ => {}
        }
      }
      TokenTree::Punct(p) => {
        if p.as_char() == ';' {
          in_use = false;
          pending = None;
          visibility.clear();
        }
      }
      TokenTree::Group(group) => {
        let named = group.delimiter() == Delimiter::Brace && pending.is_some();
        if named {
          owner.push(pending.take().expect("a brace group with a pending name"));
          visibility.clear();
        }
        walk(&group.stream(), file, owner, in_use, out);
        if named {
          owner.pop();
        }
      }
      TokenTree::Literal(_) => {}
    }
    i += 1;
  }
}

fn is_bang(tokens: &[TokenTree], at: usize) -> bool {
  matches!(tokens.get(at), Some(TokenTree::Punct(p)) if p.as_char() == '!')
}

/// What the identifier at `at` is doing.
fn classify(tokens: &[TokenTree], at: usize, in_use: bool) -> Occurrence {
  if in_use {
    return Occurrence::Import;
  }
  match tokens.get(at + 1) {
    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => Occurrence::Call,
    // `name::<T>(…)`. The turbofish is what tells this from `name::Something`, which is a path
    // through the item and not a call of it.
    _ if is_path_sep(tokens, at + 1)
      && matches!(tokens.get(at + 3), Some(TokenTree::Punct(p)) if p.as_char() == '<') =>
    {
      Occurrence::Call
    }
    _ => Occurrence::Unclassified,
  }
}

/// Every `.rs` file under `dir`, recursively.
fn rust_files(dir: &Path) -> Result<Vec<PathBuf>, String> {
  if !dir.is_dir() {
    return Err(format!(
      "{} is not a directory, so every count taken over it is zero for the wrong reason",
      dir.display()
    ));
  }
  let mut out = Vec::new();
  let mut stack = vec![dir.to_path_buf()];
  while let Some(at) = stack.pop() {
    let entries = fs::read_dir(&at).map_err(|e| format!("{} is unreadable: {e}", at.display()))?;
    for entry in entries {
      let path = entry
        .map_err(|e| format!("{} holds an unreadable entry: {e}", at.display()))?
        .path();
      if path.is_dir() {
        stack.push(path);
      } else if path.extension().is_some_and(|ext| ext == "rs") {
        out.push(path);
      }
    }
  }
  Ok(out)
}

/// `<repo>/smear-parser/src/lossless/depth.rs` -> `smear-parser/src/lossless/depth.rs`.
fn relative(repository: &Path, path: &Path) -> String {
  path
    .strip_prefix(repository)
    .unwrap_or(path)
    .display()
    .to_string()
    .replace('\\', "/")
}

pub fn render(report: &Report, verbose: bool) {
  println!();
  println!("── Root-verdict caller census ─{}", "─".repeat(62));
  println!(
    "read: {} .rs files under {ROOT}, watching {}",
    report.files_read,
    WATCHED.join(" and ")
  );
  let calls = report
    .hits
    .iter()
    .filter(|hit| hit.what == Occurrence::Call)
    .count();
  println!(
    "callers declared: {}, calls declared: {}, calls found: {calls}",
    DECLARED.len(),
    DECLARED.iter().map(|row| row.3).sum::<usize>(),
  );
  if verbose {
    for hit in &report.hits {
      if hit.what == Occurrence::Call {
        println!(
          "  . {}:{}  {} <- {}",
          hit.file, hit.line, hit.callee, hit.owner
        );
      }
    }
  } else {
    println!("  (listed with --verbose)");
  }
  if report.findings.is_empty() {
    println!("the tree and DECLARED agree, by name and by count");
  } else {
    println!("FINDINGS: {}", report.findings.len());
    for finding in &report.findings {
      println!("  x {finding}");
    }
  }
  println!("{}", "─".repeat(92));
  println!();
}

pub fn verdict(report: &Report) -> bool {
  report.findings.is_empty()
}

/// Proves each branch of [`classify`] and of the walk can fire, against synthetic sources.
///
/// The cases run through the same [`scan`] a real run uses — the `syn` gate, the token walk, the
/// owner stack and the classification, end to end. A census that has only ever agreed with itself
/// is the defect class this file is the cure for, so the shapes that matter each get a case: a
/// call inside a `macro_rules!` transcriber, a call inside a macro *invocation*'s body, prose that
/// names the function, a `use` that imports it, and a mention that is none of those.
pub fn selftest() -> Result<usize, Vec<String>> {
  struct Case {
    name: &'static str,
    source: &'static str,
    /// `(callee, owner, occurrence)`, every hit the scan must produce, in order.
    want: &'static [(&'static str, &'static str, Occurrence)],
  }

  let cases: &[Case] = &[
    Case {
      name: "a call inside a macro invocation's body is seen, and named for its own fn",
      source: r#"
        lossless_production! {
          dialect = graphql::lossless;
          fn document_entry<'inp, Src, Ctx>(inp) {
            depth::drain_unless_stopped(inp, document::<Src, Ctx>)
          }
        }
      "#,
      want: &[("drain_unless_stopped", "document_entry", Occurrence::Call)],
    },
    Case {
      name: "a call inside a macro_rules transcriber lands on the macro, not on `$modname`",
      source: r#"
        macro_rules! lossless_drivers {
          () => {
            pub mod $modname {
              pub fn $name<'inp>(src: &'inp str) {
                $crate::lossless::depth::drain_unless_stopped(inp, |inp, stop| {
                  root_turn(inp, stop, |inp| production(inp))
                })
              }
            }
          };
        }
      "#,
      want: &[
        ("drain_unless_stopped", "lossless_drivers", Occurrence::Call),
        ("root_turn", "lossless_drivers", Occurrence::Call),
      ],
    },
    Case {
      name: "prose naming the function is not a token and cannot be a call",
      source: r#"
        /// Runs `drain_unless_stopped` over the root, the way root_turn(inp) would.
        // and root_turn(inp, stop, entry) here too
        fn nothing_happens() {
          let _ = "drain_unless_stopped(inp, root)";
        }
      "#,
      want: &[],
    },
    Case {
      name: "an import is an import, at any nesting",
      source: r#"
        use crate::lossless::depth::{RootStop, drain_unless_stopped, root_turn};
        fn body() {
          use crate::lossless::depth::root_turn;
          let _ = 1;
        }
      "#,
      want: &[
        ("drain_unless_stopped", "", Occurrence::Import),
        ("root_turn", "", Occurrence::Import),
        ("root_turn", "body", Occurrence::Import),
      ],
    },
    Case {
      name: "a mention that is neither call, declaration nor import is refused",
      source: r#"
        fn hands_it_on() {
          let f = root_turn;
          take(drain_unless_stopped);
        }
      "#,
      want: &[
        ("root_turn", "hands_it_on", Occurrence::Unclassified),
        (
          "drain_unless_stopped",
          "hands_it_on",
          Occurrence::Unclassified,
        ),
      ],
    },
    Case {
      name: "the declaration is a declaration, and its visibility is read",
      source: r#"
        pub(crate) fn drain_unless_stopped<'inp>(inp: &mut u8) -> u8 {
          root_turn(inp)
        }
      "#,
      want: &[
        ("drain_unless_stopped", "", Occurrence::Declaration),
        ("root_turn", "drain_unless_stopped", Occurrence::Call),
      ],
    },
    Case {
      name: "the owner is a path, so four `drive`s are four callers",
      source: r#"
        #[test]
        fn first() {
          fn drive() { root_turn(inp, stop, entry) }
        }
        #[test]
        fn second() {
          fn drive() { root_turn(inp, stop, entry) }
        }
      "#,
      want: &[
        ("root_turn", "first::drive", Occurrence::Call),
        ("root_turn", "second::drive", Occurrence::Call),
      ],
    },
    Case {
      name: "a turbofished call is still a call",
      source: r#"
        fn t() {
          drain_unless_stopped::<Src, Ctx>(inp, root)
        }
      "#,
      want: &[("drain_unless_stopped", "t", Occurrence::Call)],
    },
  ];

  let mut problems = Vec::new();
  for case in cases {
    match scan("selftest.rs", case.source) {
      Err(message) => problems.push(format!("{}: {message}", case.name)),
      Ok(hits) => {
        let got: Vec<(&str, &str, Occurrence)> = hits
          .iter()
          .map(|hit| (hit.callee.as_str(), hit.owner.as_str(), hit.what))
          .collect();
        let want: Vec<(&str, &str, Occurrence)> = case.want.to_vec();
        if got != want {
          problems.push(format!(
            "{}:\n     want {want:?}\n     got  {got:?}",
            case.name
          ));
        }
      }
    }
  }

  // The scan must reject what it cannot read, rather than reporting no callers in it.
  if scan("selftest.rs", "fn broken( {").is_ok() {
    problems.push(
      "a file that is not Rust was scanned without complaint, so an unparseable file would \
       report zero callers"
        .to_string(),
    );
  }

  if problems.is_empty() {
    Ok(cases.len() + 1)
  } else {
    Err(problems)
  }
}
