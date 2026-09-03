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
//! # Every spelling of a call, or a refusal
//!
//! Watching two names is not the same as watching two functions. `use …::root_turn as turn;`
//! binds a second spelling; a scan that knows only the original reads that line as an import,
//! ignores it, and then cannot see `turn(inp, stop, entry)` at all. The call needs no `DECLARED`
//! row and the gate stays green — the caller-set claim false again, with the census agreeing with
//! itself about it, which is the very failure this file exists to end.
//!
//! So [`bindings`] runs first, over the same token tree, and reads every `X as Y` written inside
//! a `use`. A rename of a watched callee gives the file a second spelling, watched exactly like
//! the first. Every spelling one file cannot follow is **refused**, and a refusal is a finding:
//!
//! - a rename that is exported (`pub(crate) use …::root_turn as turn;`), or an exported re-import
//!   of a local alias, because the new spelling leaves this file and a per-file scan cannot meet
//!   it where it is called;
//! - a rename of a rename (`use self::turn as spin;`), because the census follows one hop rather
//!   than a graph;
//! - a `use` that binds a watched *name* to something else (`use elsewhere::other as root_turn;`),
//!   because every bare `root_turn(…)` under it is then a call of something else;
//! - a rename whose **destination is not an identifier** — `use …::root_turn as $alias;` inside a
//!   macro's transcriber, or `as [< … >]` inside a `paste!` — because the spelling it will bind
//!   is not in these tokens, so nothing here says what a call through it looks like;
//! - a `paste!` `[< … >]` or a `concat_idents!` that spells a watched name, or one whose pieces
//!   the census cannot evaluate — an identifier assembled out of fragments is one no walk over
//!   identifiers can recognise — and a `use` that renames `concat_idents` itself, which is the
//!   one of the two the census recognises by its name.
//!
//! With those refused the coverage argument closes. A call has to *name* its function. The name is
//! either the callee's own — which the ident walk sees bare, `r#`-escaped, path-qualified,
//! turbofished, glob-imported or written inside a macro body, because it is a walk over tokens —
//! or a spelling some `use` bound to it. A `use` in this file is in [`bindings`]; a `use` anywhere
//! else has to cross a file boundary through an exported one, which is refused on the line that
//! writes it. Naming the function without calling it — `let f = root_turn;`, which is how a root
//! is handed to the drain today — is neither a call nor an import, so it is unclassified, which is
//! a finding too.
//!
//! The other half of a rename is quiet, and on purpose. `use …::$from as turn;` renames whatever
//! `$from` is handed, and the census does not refuse it — the tree writes exactly that, twice, for
//! keywords. It does not have to: for the watched name to reach `$from`, some invocation has to
//! *write* `root_turn` as a token, and an occurrence that is neither a call, a declaration nor an
//! import is a finding. So the source side reddens at the invocation, while the destination side
//! has nowhere else to redden — the new name is one nothing else in the crate ever writes — which
//! is why one of the two is refused and the other is not.
//!
//! What stays outside is a call whose tokens this walk never reads. One kind is a proc macro that
//! synthesises `root_turn(…)` into its own output; the other is a file the population does not
//! contain — an `include!` of a path outside [`ROOT`] or of a generated one, or a `#[path]` module
//! spelled with any other extension. Neither is refused: `scan` is handed a file's text and its
//! display name, not its place on disk, so it cannot tell an `include!` the walk also reads as a
//! file of its own from one nobody reads, and a refusal that cannot tell them apart would redden
//! the ordinary `OUT_DIR` spelling. The tree holds none of the three today — no `include!`, no
//! `#[path]`, and no file under [`ROOT`] that is not `.rs` — and [`selftest`] claims none of them.
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
pub(crate) const ROOT: &str = "smear-parser/src";

/// The two functions whose caller set is pinned.
const WATCHED: &[&str] = &["drain_unless_stopped", "root_turn"];

/// An identifier without its `r#` escape, which is not part of its name.
///
/// `Ident`'s own `Display` keeps the `r#`, and `r#root_turn(inp, stop, entry)` is a call of
/// `root_turn`. Normalising here rather than at each comparison is what keeps that from being a
/// spelling the walk does not recognise.
fn bare(spelling: &str) -> &str {
  spelling.strip_prefix("r#").unwrap_or(spelling)
}

/// The [`WATCHED`] name this identifier spells, if it spells one.
fn watched(spelling: &str) -> Option<&'static str> {
  let bare = bare(spelling);
  WATCHED.iter().copied().find(|name| *name == bare)
}

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
/// - **The dialect door**, and it is no longer watched by name. It used to be: six `*_entry`
///   productions calling `drain_unless_stopped`, then a `document_root` frame taking a token
///   (round 4), then one generic `parse_lossless_document` (round 5). Round 7 generates a door per
///   dialect from `lossless_door!`, and a macro-generated `fn` has no source declaration for this
///   census to find — [`DECLARED_IN`] pins ONE file for the declaration of every watched name, and
///   there would now be two doors declared in neither. So the door leaves [`WATCHED`], and what
///   pins its callers instead is rustc (the six shipped doors and the driver macro stop compiling
///   if its signature moves), `smear/tests/durable_token_budget.rs`'s six-door population gate, and
///   `smear/tests/lossless_isolation.rs`'s count of `lossless_door!` invocations.
/// - **The door's own call** to `drain_unless_stopped` is the one non-test call there is, and it is
///   in the macro's transcriber rather than in `DECLARED_IN`. That is not a contradiction: what
///   that constant pins is the file allowed to hold a watched name's one *declaration*, not where
///   calls may be written.
/// - **The six document roots** call `root_turn`, once per root, inside the loop that walks
///   entries. `document` and `type_system_document` in each dialect, plus each dialect's
///   `executable_document`.
/// - **The driver macro** calls both, once each, inside `lossless_drivers!`'s transcriber: a
///   driver is a root of one entry and is written as one. The owner is the macro rather than a
///   function because the function it expands to is `fn $name`, which has no name here.
/// - **The in-crate cells** in the GraphQL assembly's `tests.rs`, which drive the pair directly.
///   They are the family every previous revision of `depth.rs`'s sentence left out, and they are
///   the reason this table exists rather than another sentence. Six of them now: smear issue
///   #193's rounds 2, 3 and 4 added the pairs that pin one refusal to one diagnostic through every
///   composition. They live in the dialect assembly and not beside the code they drive because
///   gate 6 forbids a dialect-typed test under the substrate — `depth.rs`'s own closing comment
///   carries that reasoning.
///
/// Note the four distinct `drive` functions: same name, four different tests, three different
/// counts. That is why the key is a path and not a bare function name.
const DECLARED: &[Declared] = &[
  // the six `*_entry` productions
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
    "root_turn",
    "smear-parser/src/lossless/macros.rs",
    "lossless_drivers",
    1,
  ),
  // THE ONLY NON-TEST CALLER OF THE DRAIN, and round 7 moved it out of `depth.rs`: the dialect
  // doors are generated by `lossless_door!`, so the call the six shipped doors reach the drain
  // through is written once, in the macro's transcriber. The owner is the macro rather than a
  // function because the function it expands to has no name here — the same reason
  // `lossless_drivers` is an owner two rows down.
  (
    "drain_unless_stopped",
    "smear-parser/src/lossless/macros.rs",
    "lossless_door::parse_lossless_document",
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
  // smear issue #193 round 2: two nested drains over one durable token-budget refusal. The pair
  // is the point — `drive` mints the outer one and `outer` the inner — because the defect is
  // exactly that both frames read the same input-absolute `false -> true` and both reported.
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_budget_refusal_is_reported_once_however_many_drains_are_stacked::outer",
    1,
  ),
  // smear issue #193 round 3: the refusal placed in the INNER frame's drain rather than in its
  // root. Round 3 added this caller and did not add this row — `cargo run -p source-census` was
  // in that round's NOT-RUN list because D2's probe crate no longer resolves, and the drift rode
  // through. It is the finding this table exists for, arriving late.
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "a_budget_refusal_is_reported_once_however_many_drains_are_stacked::outer_over_a_draining_inner",
    1,
  ),
  // smear issue #193 round 4: the four compositions of one refusal. `drive` is the door and
  // `inner_frame` is the frame under it, which is the whole shape of the finding.
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "the_report_has_an_owner_and_terminality_is_not_it::inner_frame",
    1,
  ),
  // smear issue #193 round 8, case (e): the root that catches the nested stop and EMITS it. It is
  // in the table because it is a real caller of the drain, and it is the adversary the round is
  // about — a root does not need to forge anything, only to report what the frame below handed it.
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "the_report_has_an_owner_and_terminality_is_not_it::catches_and_emits",
    1,
  ),
  // smear issue #193 round 5 (Codex round 5): the door builds its own emitter, so a cell that
  // needs a REJECTING one — or an error type whose `MaybeTerminal` arms it chooses — cannot go
  // through the door and runs the frame over a `ParserContext` of its own. It is in the table
  // rather than exempted from it: what makes it safe is that the frame it reaches stops and never
  // emits, and that is a claim about which function it calls.
  (
    "drain_unless_stopped",
    "smear-parser/src/graphql/lossless/tests.rs",
    "the_value_a_frame_hands_up_after_a_drain_refusal_is_terminal::frame",
    1,
  ),
  // Codex round 5's own regression: a door nested inside a door. TWO rows for one cell, and the
  // pair is the point — the outer parse and the composed root each run one, and the cell asserts
  // that their logs are two logs.
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
  /// The identifier as it is written, which is [`Hit::callee`] unless this file renamed it. A
  /// finding names this and not only the callee: `turn` is what a reader has to go and look at.
  pub spelling: String,
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
  /// Spellings of a watched callee that one file's tokens cannot follow, with the reason. Each is
  /// a finding: a rename the census cannot resolve makes it red rather than making it blinder.
  pub refusals: Vec<String>,
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
    refusals: Vec::new(),
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
      Ok(scan) => {
        report.hits.extend(scan.hits);
        report.refusals.extend(scan.refusals);
      }
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

  // A spelling the census will not follow is red here rather than absent from the counts below.
  // Reducing what a gate can see is the one way it fails that nothing else announces.
  for refusal in &report.refusals {
    report.findings.push(format!(
      "a second spelling this census will not follow — {refusal}"
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
      let named = if hit.spelling == hit.callee {
        format!("`{}`", hit.callee)
      } else {
        format!(
          "`{}`, this file\'s name for `{}`,",
          hit.spelling, hit.callee
        )
      };
      report.findings.push(format!(
        "{}:{}: {named} occurs inside `{}` and is neither a call, the declaration, nor an import \
         — the census will not guess",
        hit.file, hit.line, hit.owner
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
pub fn scan(file: &str, text: &str) -> Result<Scan, String> {
  syn::parse_file(text).map_err(|e| format!("{file}: syn cannot parse this file: {e}"))?;
  let stream: TokenStream = text
    .parse()
    .map_err(|e| format!("{file}: this file does not tokenize: {e}"))?;
  // What this file calls the two functions is read before anything is counted, because a rename
  // decides which identifiers the walk below is even looking at.
  let bound = bindings(file, &stream);
  let mut hits = Vec::new();
  walk(
    &stream,
    file,
    &mut Vec::new(),
    false,
    &bound.alias,
    &mut hits,
  );
  Ok(Scan {
    hits,
    refusals: bound.refusals,
  })
}

/// What one file's tokens said: every occurrence, and every spelling the census will not follow.
pub struct Scan {
  pub hits: Vec<Hit>,
  /// See [`Report::refusals`].
  pub refusals: Vec<String>,
}

/// The second spellings one file binds, and the ones it refuses to follow.
struct Bindings {
  /// The identifier written at a call site, `r#` stripped, to the watched callee it stands for.
  alias: BTreeMap<String, &'static str>,
  refusals: Vec<String>,
}

/// One identifier a `use` names: what it says, what it binds it to, and whether it is exported.
struct Named {
  spelling: String,
  bound: Bound,
  exported: bool,
  line: usize,
}

/// What a `use` binds one of its names to — every shape the tokens after that name can take.
///
/// Four answers and no fifth, and the fourth is the point. This was an `Option<String>` read off a
/// match whose last arm was `_ => None`, and that arm answered two different questions with the
/// same word: *there is no `as` here* and *there is an `as` and its destination is not a token I
/// can read*. `use …::root_turn as $alias;` took the second answer and got the first, so the
/// census recorded a plain import, bound no second spelling, and could not see `turn(…)` at all —
/// the very invisible caller [`bindings`] exists to end, through the one branch that had no
/// verdict of its own. [`Bound::Unreadable`] is that verdict.
enum Bound {
  /// No `as`: the name is bound under its own spelling.
  Itself,
  /// `as NEW`, `NEW` an identifier. The one hop this census follows.
  Renamed(String),
  /// `as _`, which binds the item under no name at all. Nothing can call what has no name, so
  /// there is no second spelling here to follow and nothing to refuse.
  Discarded,
  /// `as` and then something that is not an identifier: a `$` metavariable, a `paste!`'s
  /// `[< … >]`, a literal, or the end of the tokens. The name it will bind is not written here,
  /// so this file cannot say what a call through it looks like. Carries what *is* written, so the
  /// refusal can name it.
  Unreadable(String),
}

/// What the tokens at `at` bind the name there to.
///
/// The `as` is read here and nowhere else, so every shape its destination can take is decided in
/// one place and each has a name. `_` is recognised by its spelling rather than by its token kind,
/// because which kind it is belongs to the lexer rather than to the language.
fn bound(tokens: &[TokenTree], at: usize) -> Bound {
  match tokens.get(at + 1) {
    Some(TokenTree::Ident(keyword)) if keyword == "as" => {}
    _ => return Bound::Itself,
  }
  match tokens.get(at + 2) {
    Some(TokenTree::Ident(new)) if new != "_" => Bound::Renamed(new.to_string()),
    Some(token) if token.to_string() == "_" => Bound::Discarded,
    Some(token) => Bound::Unreadable(written(token, tokens.get(at + 3))),
    None => Bound::Unreadable("nothing at all".to_string()),
  }
}

/// A destination as it is written, so a refusal sends a reader to the right thing on the line.
///
/// A `$` metavariable is two tokens, and `$` on its own would name nothing. Quoted here rather
/// than by the refusal, because the one destination that is not a token — the tokens simply
/// ending — is a phrase and not a spelling.
fn written(token: &TokenTree, next: Option<&TokenTree>) -> String {
  match (token, next) {
    (TokenTree::Punct(p), Some(TokenTree::Ident(name))) if p.as_char() == '$' => {
      format!("`${name}`")
    }
    _ => format!("`{token}`"),
  }
}

/// Every second spelling of a watched callee this file binds, and every one it will not follow.
///
/// One hop is followed, because `use …::root_turn as turn;` is a rename a single file can read off
/// its own tokens. Everything past that hop leaves this file or leaves the census guessing, and is
/// refused instead — a shape it cannot read has to make it red, not quietly make it cover less.
fn bindings(file: &str, stream: &TokenStream) -> Bindings {
  let mut named = Vec::new();
  let mut pasted = Vec::new();
  collect(stream, false, false, &mut named, &mut pasted);
  // Over the whole file rather than inside a recognised `paste!`: `[< … >]` is a syntax of its
  // own, so it is read wherever it is written and renaming the macro that consumes it — `use
  // paste::paste as glue;` — moves nothing out of view.
  concatenations(stream, &mut pasted);

  let mut alias: BTreeMap<String, &'static str> = BTreeMap::new();
  let mut refusals = Vec::new();

  // The hop the census follows.
  for entry in &named {
    let (Bound::Renamed(new), Some(callee)) = (&entry.bound, watched(&entry.spelling)) else {
      continue;
    };
    // Renaming one watched name onto the other is a rebinding, refused in the loop below.
    if watched(new).is_some() {
      continue;
    }
    match alias.insert(bare(new).to_string(), callee) {
      Some(other) if other != callee => refusals.push(format!(
        "{file}:{}: `{new}` is this file's name for both `{other}` and `{callee}`",
        entry.line
      )),
      _ => {}
    }
    if entry.exported {
      refusals.push(format!(
        "{file}:{}: `{callee}` is re-exported as `{new}`, so a call written `{new}(…)` in another \
         file is one this per-file scan cannot see",
        entry.line
      ));
    }
  }

  // Everything the hop does not reach. Every arm is written out, because a `use` the census reads
  // and says nothing about is exactly how the shape above got past it.
  for entry in &named {
    let spelled = bare(&entry.spelling);
    // A macro the census knows by its name is one a rename hides, whatever it is renamed to.
    if spelled == READ_BY_NAME
      && let Bound::Renamed(_) | Bound::Unreadable(_) = &entry.bound
    {
      refusals.push(format!(
        "{file}:{}: `{READ_BY_NAME}` is renamed here, and the census recognises that macro by its \
         name — an identifier it assembles under any other one is not a spelling this walk reads",
        entry.line
      ));
    }
    match &entry.bound {
      Bound::Renamed(new) => {
        if let Some(callee) = watched(new) {
          refusals.push(format!(
            "{file}:{}: `{spelled} as {new}` binds the watched name `{callee}` to something else, \
             so a `{callee}(…)` written below it is not a call of the function this census watches",
            entry.line
          ));
        } else if let Some(callee) = alias.get(spelled) {
          refusals.push(format!(
            "{file}:{}: `{spelled}` is already this file's name for `{callee}`, and renaming it \
             again to `{new}` is a second hop — the census follows one",
            entry.line
          ));
        }
      }
      // The destination is not an identifier, so the spelling it binds is not in this file. It is
      // refused where the name being renamed is one the census watches — its own, or one this
      // file has already made into a second name for it — because that is where a call the census
      // would otherwise never see is being given a name it cannot read.
      Bound::Unreadable(destination) => {
        if let Some(callee) = watched(spelled).or_else(|| alias.get(spelled).copied()) {
          refusals.push(format!(
            "{file}:{}: the `as` after `{spelled}` is followed by {destination} rather than by an \
             identifier, so the second spelling it gives `{callee}` is not written in this file \
             and a call through it is one this scan cannot recognise",
            entry.line
          ));
        }
      }
      // `as _` binds the item under no name, so no call can spell it and there is nothing here to
      // follow or to refuse — including when it is exported, which exports no name either.
      Bound::Discarded => {}
      Bound::Itself => {
        if entry.exported
          && let Some(callee) = alias.get(spelled)
        {
          refusals.push(format!(
            "{file}:{}: `{spelled}` is this file's name for `{callee}` and is re-exported under \
             it, so a call written `{spelled}(…)` in another file is one this per-file scan \
             cannot see",
            entry.line
          ));
        }
      }
    }
  }

  for (spelled, line) in pasted {
    match spelled {
      Some(name) if watched(&name).is_some() => refusals.push(format!(
        "{file}:{line}: a `[< … >]` concatenation spells `{name}`, and an identifier assembled \
         from pieces is not one an identifier walk can recognise"
      )),
      None => refusals.push(format!(
        "{file}:{line}: a `[< … >]` concatenation the census cannot evaluate, so it cannot say \
         the identifier it builds is not {}",
        WATCHED.join(" or ")
      )),
      Some(_) => {}
    }
  }

  Bindings { alias, refusals }
}

/// The one identifier-assembling macro the census recognises by its name rather than by a syntax.
///
/// `paste!` marks what it builds — `[< … >]` is not Rust anywhere else — so [`concatenations`]
/// reads it wherever it appears and never has to know which macro is going to consume it.
/// `concat_idents!` has no such marker: its whole body is the identifier, and telling it from
/// `matches!(a, b)` is the name and nothing else. So a `use` that renames it is refused, because
/// that rename is itself a token this file reads.
const READ_BY_NAME: &str = "concat_idents";

/// Reads what every `use` names and what `concat_idents!` assembles.
///
/// `in_use` and the export flag descend into groups the way [`walk`]'s own state does, so a rename
/// written inside a `use` group, inside a nested module, or inside a macro body is read the same.
/// A rename consumes its `as NEW`, so the new name is not also recorded as a plain import — which
/// would refuse every exported rename twice.
fn collect(
  stream: &TokenStream,
  in_use: bool,
  exported: bool,
  named: &mut Vec<Named>,
  pasted: &mut Vec<(Option<String>, usize)>,
) {
  let tokens: Vec<TokenTree> = stream.clone().into_iter().collect();
  let mut in_use = in_use;
  let mut exported = exported;
  // A `pub` seen at this level and not yet spent, which is what makes the next `use` an export.
  let mut saw_pub = false;
  let mut i = 0;
  while i < tokens.len() {
    match &tokens[i] {
      TokenTree::Ident(ident) => {
        let name = ident.to_string();
        match name.as_str() {
          "pub" => saw_pub = true,
          "use" => {
            in_use = true;
            exported = saw_pub;
            saw_pub = false;
          }
          // The other way a file spells an identifier it does not write. `paste!`'s `[< … >]` is
          // read by [`concatenations`] over the whole file, because it is a syntax; this one has
          // no syntax of its own — its whole body is the one identifier it builds — so it is read
          // here, by its name, and a `use` that renames that name is refused.
          READ_BY_NAME => {
            if let (true, Some(TokenTree::Group(body))) =
              (is_bang(&tokens, i + 1), tokens.get(i + 2))
            {
              let pieces: Vec<TokenTree> = body.stream().into_iter().collect();
              pasted.push((spells(&pieces), body.span().start().line));
            }
          }
          _ => {}
        }
        if in_use && name != "use" {
          let bound = bound(&tokens, i);
          let renamed = matches!(bound, Bound::Renamed(_));
          named.push(Named {
            spelling: name,
            bound,
            exported,
            line: ident.span().start().line,
          });
          if renamed {
            i += 3;
            continue;
          }
        }
      }
      TokenTree::Punct(p) => {
        if p.as_char() == ';' {
          in_use = false;
          saw_pub = false;
        }
      }
      TokenTree::Group(group) => {
        collect(&group.stream(), in_use, exported, named, pasted);
        if group.delimiter() == Delimiter::Brace {
          saw_pub = false;
        }
      }
      TokenTree::Literal(_) => {}
    }
    i += 1;
  }
}

/// Every `[< … >]` inside a pasting macro's body, as the identifier it spells — or `None` where a
/// piece is not literal text, which is where the census stops being able to say what it spells.
fn concatenations(stream: &TokenStream, out: &mut Vec<(Option<String>, usize)>) {
  for token in stream.clone() {
    let TokenTree::Group(group) = token else {
      continue;
    };
    let inner: Vec<TokenTree> = group.stream().into_iter().collect();
    let opens = matches!(inner.first(), Some(TokenTree::Punct(p)) if p.as_char() == '<');
    let closes = matches!(inner.last(), Some(TokenTree::Punct(p)) if p.as_char() == '>');
    if group.delimiter() == Delimiter::Bracket && opens && closes && inner.len() > 2 {
      out.push((
        spells(&inner[1..inner.len() - 1]),
        group.span().start().line,
      ));
    }
    concatenations(&group.stream(), out);
  }
}

/// The identifier a run of tokens spells, or `None` where a piece is not literal text.
///
/// A `,` contributes nothing because it is `concat_idents!`'s separator. Anything else — a `$`
/// metavariable above all — is where the census stops being able to say what is built, and `None`
/// is what makes that a refusal rather than a silent pass.
fn spells(pieces: &[TokenTree]) -> Option<String> {
  let mut spelled = String::new();
  for piece in pieces {
    match piece {
      TokenTree::Ident(ident) => spelled.push_str(&ident.to_string()),
      TokenTree::Literal(literal) => spelled.push_str(&literal.to_string()),
      TokenTree::Punct(punct) if punct.as_char() == ',' => {}
      _ => return None,
    }
  }
  Some(spelled)
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
///
/// `alias` is what [`bindings`] read: the identifiers this file has made into second names for a
/// watched callee. They are watched exactly as the callee's own name is, and the [`Hit`] they
/// produce carries the callee, so two spellings from one owner are one caller.
fn walk(
  stream: &TokenStream,
  file: &str,
  owner: &mut Vec<String>,
  in_use: bool,
  alias: &BTreeMap<String, &'static str>,
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
              if name == "fn"
                && let Some(callee) = watched(&declared)
              {
                out.push(Hit {
                  callee: callee.to_string(),
                  spelling: declared.clone(),
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
          _ => {
            let own = watched(&name);
            if let Some(callee) = own.or_else(|| alias.get(bare(&name)).copied()) {
              // Where a `use` binds an alias, the import is already recorded against the callee's
              // own name in the same tree; recording the new name there too would double it.
              if own.is_some() || !in_use {
                out.push(Hit {
                  callee: callee.to_string(),
                  spelling: name,
                  file: file.to_string(),
                  owner: owner.join("::"),
                  line: ident.span().start().line,
                  what: classify(&tokens, i, in_use),
                  visibility: String::new(),
                });
              }
            }
          }
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
        walk(&group.stream(), file, owner, in_use, alias, out);
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
/// Two halves, and the second is the one that matters.
///
/// The first is a list of pinned shapes, each with the exact hits it must produce. That is what
/// fixes owners, counts and classifications, and it is what a reader consults to see what the scan
/// does with a shape.
///
/// But a list of shapes is written out of the shapes its author thought of, and a claim derived
/// from what someone could imagine is the defect this file exists to end — one level up. Nine
/// cases stood here while `use …::root_turn as turn;` made a caller invisible, because none of the
/// nine was aliased. The census had the same blindness as the tree it was auditing.
///
/// So the second half lists no shapes. It crosses the axes a call varies along — how the name gets
/// into scope, how the call site spells it, and where the call sits — and asserts one property
/// over the whole product: **every spelling is either counted or refused, and never silently
/// absent.** A new way to write a call is a new row in one axis, and its combination with every
/// other axis comes for free.
pub fn selftest() -> Result<usize, Vec<String>> {
  struct Case {
    name: &'static str,
    source: &'static str,
    /// `(callee, owner, occurrence)`, every hit the scan must produce, in order.
    want: &'static [(&'static str, &'static str, Occurrence)],
    /// A fragment of each refusal the scan must make, in order. Empty means it must make none.
    refuses: &'static [&'static str],
  }

  let pinned: &[Case] = &[
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
      refuses: &[],
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
      refuses: &[],
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
      refuses: &[],
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
      refuses: &[],
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
      refuses: &[],
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
      refuses: &[],
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
      refuses: &[],
    },
    Case {
      name: "a turbofished call is still a call",
      source: r#"
        fn t() {
          drain_unless_stopped::<Src, Ctx>(inp, root)
        }
      "#,
      want: &[("drain_unless_stopped", "t", Occurrence::Call)],
      refuses: &[],
    },
    Case {
      name: "a rename gives the file a second spelling, and a call through it is a call",
      source: r#"
        use crate::lossless::depth::root_turn as turn;
        fn drive() { turn(inp, stop, entry) }
      "#,
      want: &[
        ("root_turn", "", Occurrence::Import),
        ("root_turn", "drive", Occurrence::Call),
      ],
      refuses: &[],
    },
    Case {
      name: "a raw identifier is the same identifier",
      source: r#"
        fn drive() { r#root_turn(inp, stop, entry) }
      "#,
      want: &[("root_turn", "drive", Occurrence::Call)],
      refuses: &[],
    },
    Case {
      name: "a glob brings the name in unchanged, so nothing about the call moves",
      source: r#"
        use crate::lossless::depth::*;
        fn drive() { root_turn(inp, stop, entry) }
      "#,
      want: &[("root_turn", "drive", Occurrence::Call)],
      refuses: &[],
    },
    Case {
      name: "an exported rename is refused, because the spelling leaves this file",
      source: r#"
        pub(crate) use crate::lossless::depth::drain_unless_stopped as drain;
        fn drive() { drain(inp, root) }
      "#,
      want: &[
        ("drain_unless_stopped", "", Occurrence::Import),
        ("drain_unless_stopped", "drive", Occurrence::Call),
      ],
      refuses: &["is re-exported as `drain`"],
    },
    Case {
      name: "a local rename that is then re-exported is refused for the same reason",
      source: r#"
        use crate::lossless::depth::root_turn as turn;
        pub(crate) use self::turn;
        fn drive() { turn(inp, stop, entry) }
      "#,
      want: &[
        ("root_turn", "", Occurrence::Import),
        ("root_turn", "drive", Occurrence::Call),
      ],
      refuses: &["is re-exported under it"],
    },
    Case {
      name: "a second hop is refused, and the name it binds is watched by nothing",
      source: r#"
        use crate::lossless::depth::root_turn as turn;
        use self::turn as spin;
        fn drive() { spin(inp, stop, entry) }
      "#,
      want: &[("root_turn", "", Occurrence::Import)],
      refuses: &["is a second hop"],
    },
    Case {
      name: "a `use` that binds a watched name to something else is refused",
      source: r#"
        use crate::elsewhere::other as root_turn;
        fn drive() { root_turn(inp, stop, entry) }
      "#,
      want: &[
        ("root_turn", "", Occurrence::Import),
        ("root_turn", "drive", Occurrence::Call),
      ],
      refuses: &["binds the watched name `root_turn` to something else"],
    },
    Case {
      name: "`as _` binds no spelling, because nothing can call what it binds",
      source: r#"
        pub(crate) use crate::lossless::depth::root_turn as _;
        fn drive() { let _ = 1; }
      "#,
      want: &[("root_turn", "", Occurrence::Import)],
      refuses: &[],
    },
    Case {
      name: "a rename to a `$` metavariable is refused: the name it binds is not in this file",
      source: r#"
        macro_rules! bind {
          ($alias:ident) => { use crate::lossless::depth::root_turn as $alias; };
        }
        bind!(turn);
        fn drive() { turn(inp, stop, entry) }
      "#,
      want: &[("root_turn", "bind", Occurrence::Import)],
      refuses: &["followed by `$alias`"],
    },
    Case {
      name: "a rename to a name a paste assembles is the same refusal, at the same branch",
      source: r#"
        paste::paste! { use crate::lossless::depth::root_turn as [<tu rn>]; }
        fn drive() { turn(inp, stop, entry) }
      "#,
      want: &[("root_turn", "", Occurrence::Import)],
      refuses: &["followed by `[< tu rn >]`"],
    },
    Case {
      name: "an `as` with nothing after it binds a spelling that is not there either",
      source: r#"
        macro_rules! bind {
          () => { use crate::lossless::depth::root_turn as };
        }
      "#,
      want: &[("root_turn", "bind", Occurrence::Import)],
      refuses: &["followed by nothing at all"],
    },
    Case {
      name: "a rename whose OLD name is a `$` metavariable is not refused, and reddens where the \
             watched name is written instead",
      source: r#"
        macro_rules! bind {
          ($from:ident) => { use crate::lossless::depth::$from as turn; };
        }
        bind!(root_turn);
        fn drive() { turn(inp, stop, entry) }
      "#,
      want: &[("root_turn", "", Occurrence::Unclassified)],
      refuses: &[],
    },
    Case {
      name: "renaming `paste` moves nothing out of view, because `[< … >]` is read wherever it is \
             written",
      source: r#"
        use paste::paste as glue;
        fn drive() { glue! { [<root_ turn>](inp, stop, entry) } }
      "#,
      want: &[],
      refuses: &["concatenation spells `root_turn`"],
    },
    Case {
      name: "renaming the macro the census reads by its name is refused, whatever it assembles",
      source: r#"
        use core::concat_idents as glue;
        fn drive() { glue!(root_, turn)(inp, stop, entry) }
      "#,
      want: &[],
      refuses: &["recognises that macro by its name"],
    },
    Case {
      name: "a paste that spells a watched name is refused; one that spells something else is not",
      source: r#"
        fn drive() {
          paste::paste! { [<root_ turn>](inp, stop, entry) }
        }
        fn elsewhere() {
          paste::paste! { [<Named Type>]::new() }
        }
      "#,
      want: &[],
      refuses: &["concatenation spells `root_turn`"],
    },
    Case {
      name: "a `concat_idents!` is read by its own syntax, not by `paste!`'s",
      source: r#"
        fn drive() {
          concat_idents!(drain_unless_, stopped)(inp, root)
        }
      "#,
      want: &[],
      refuses: &["concatenation spells `drain_unless_stopped`"],
    },
    Case {
      name: "a paste the census cannot evaluate is refused rather than assumed harmless",
      source: r#"
        macro_rules! driver {
          ($name:ident) => {
            paste::paste! { [<$name _turn>](inp, stop, entry) }
          };
        }
      "#,
      want: &[],
      refuses: &["cannot evaluate"],
    },
  ];

  let mut problems = Vec::new();
  for case in pinned {
    match scan("selftest.rs", case.source) {
      Err(message) => problems.push(format!("{}: {message}", case.name)),
      Ok(scan) => {
        let got: Vec<(&str, &str, Occurrence)> = scan
          .hits
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
        if scan.refusals.len() != case.refuses.len()
          || scan
            .refusals
            .iter()
            .zip(case.refuses)
            .any(|(refusal, fragment)| !refusal.contains(fragment))
        {
          problems.push(format!(
            "{}:\n     want refusals {:?}\n     got  {:?}",
            case.name, case.refuses, scan.refusals
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

  problems.extend(every_spelling());

  if problems.is_empty() {
    Ok(pinned.len() + 1 + SPELLINGS.len() * PLACEMENTS.len() * WATCHED.len())
  } else {
    Err(problems)
  }
}

/// One way the name of a watched callee reaches a call site, and how the call site then writes it.
struct Spelling {
  what: &'static str,
  /// The items the file carries to make the call mean the watched function. `{callee}` is its
  /// name.
  binding: &'static str,
  /// The call expression. `{callee}` is the callee's name, `{head}`/`{tail}` its two halves.
  call: &'static str,
  /// Whether one file's tokens can follow this spelling, or the census must refuse it.
  followed: bool,
}

/// The ways a name reaches a call site, which is the axis the nine pinned cases had one value of.
///
/// A rename is followed because a single file can read it off its own tokens. Three kinds are
/// refused: the ones that leave the file, the ones that assemble an identifier out of pieces, and
/// the ones whose new name is not written in the file at all — a `$` metavariable or a `[< … >]`
/// after the `as`. See [`bindings`] for why that direction and not more resolution.
///
/// A row here is the shape of the axis: `use …::{callee} as $alias;` was invisible to the census
/// and is a row, not a case, so it is crossed with every placement and both callees for the price
/// of the four lines that write it.
const SPELLINGS: &[Spelling] = &[
  Spelling {
    what: "a path, with nothing imported",
    binding: "",
    call: "crate::lossless::depth::{callee}(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a path with a turbofish",
    binding: "",
    call: "crate::lossless::depth::{callee}::<Src, Ctx>(inp, stop)",
    followed: true,
  },
  Spelling {
    what: "the imported name",
    binding: "use crate::lossless::depth::{callee};",
    call: "{callee}(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "the imported name, turbofished",
    binding: "use crate::lossless::depth::{callee};",
    call: "{callee}::<Src, Ctx>(inp, stop)",
    followed: true,
  },
  Spelling {
    what: "a name a glob brought in",
    binding: "use crate::lossless::depth::*;",
    call: "{callee}(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a raw identifier",
    binding: "use crate::lossless::depth::r#{callee};",
    call: "r#{callee}(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a rename",
    binding: "use crate::lossless::depth::{callee} as turn;",
    call: "turn(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a rename, turbofished",
    binding: "use crate::lossless::depth::{callee} as turn;",
    call: "turn::<Src, Ctx>(inp, stop)",
    followed: true,
  },
  Spelling {
    what: "a rename inside a group",
    binding: "use crate::lossless::depth::{RootStop, {callee} as turn};",
    call: "turn(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a rename to a raw identifier",
    binding: "use crate::lossless::depth::{callee} as r#turn;",
    call: "turn(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a rename inside a nested module",
    binding: "mod inner { use crate::lossless::depth::{callee} as turn; }",
    call: "turn(inp, stop, entry)",
    followed: true,
  },
  Spelling {
    what: "a rename that is exported",
    binding: "pub(crate) use crate::lossless::depth::{callee} as turn;",
    call: "turn(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "a local rename that is re-exported",
    binding: "use crate::lossless::depth::{callee} as turn;\npub(crate) use self::turn;",
    call: "turn(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "a rename of a rename",
    binding: "use crate::lossless::depth::{callee} as turn;\nuse self::turn as spin;",
    call: "spin(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "the watched name bound to something else",
    binding: "use crate::elsewhere::other as {callee};",
    call: "{callee}(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "a rename to a `$` metavariable",
    binding: "macro_rules! bind {\n  ($alias:ident) => { use crate::lossless::depth::{callee} as \
              $alias; };\n}\nbind!(turn);",
    call: "turn(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "a rename to a name a paste assembles",
    binding: "paste::paste! { use crate::lossless::depth::{callee} as [<tu rn>]; }",
    call: "turn(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "a rename whose old name is a `$` metavariable",
    binding: "macro_rules! bind {\n  ($from:ident) => { use crate::lossless::depth::$from as turn; \
              };\n}\nbind!({callee});",
    call: "turn(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "an identifier a renamed `paste` assembles",
    binding: "use paste::paste as glue;",
    call: "glue! { [<{head} {tail}>](inp, stop, entry) }",
    followed: false,
  },
  Spelling {
    what: "an identifier a renamed `concat_idents` assembles",
    binding: "use core::concat_idents as glue;",
    call: "glue!({head}, {tail})(inp, stop, entry)",
    followed: false,
  },
  Spelling {
    what: "an identifier assembled by a paste",
    binding: "",
    call: "paste::paste! { [<{head} {tail}>](inp, stop, entry) }",
    followed: false,
  },
  Spelling {
    what: "an identifier assembled by `concat_idents!`",
    binding: "",
    call: "concat_idents!({head}, {tail})(inp, stop, entry)",
    followed: false,
  },
];

/// Where the call sits: `{items}` is the [`Spelling::binding`], `{call}` the call expression.
///
/// The macro placements are the ones that matter most and the ones a visitor-based census cannot
/// reach at all — twelve of the fourteen shipped calls are written inside a macro invocation, and
/// two inside a `macro_rules!` transcriber.
const PLACEMENTS: &[(&str, &str)] = &[
  ("a function body", "{items}\nfn holder() { {call}; }"),
  (
    "a nested module",
    "{items}\nmod outer { use super::*; pub fn holder() { {call}; } }",
  ),
  (
    "a closure",
    "{items}\nfn holder() { let f = |inp| { {call} }; }",
  ),
  (
    "a `macro_rules!` transcriber",
    "{items}\nmacro_rules! driver { () => { fn made() { {call}; } }; }",
  ),
  (
    "a macro invocation's body",
    "{items}\nlossless_production! { fn holder(inp) { {call} } }",
  ),
];

/// Crosses [`SPELLINGS`] with [`PLACEMENTS`] and [`WATCHED`], and holds one property over all of
/// it: the call is counted, or the spelling is refused, and never neither.
///
/// This is the half that is not a list of remembered shapes. It cannot be made to pass by adding
/// the case someone last thought of, and a spelling nobody has thought of yet fails it the moment
/// it is written as a row rather than as a case.
fn every_spelling() -> Vec<String> {
  let mut problems = Vec::new();
  for spelling in SPELLINGS {
    for placement in PLACEMENTS {
      for &callee in WATCHED {
        let cut = callee.rfind('_').map_or(0, |at| at + 1);
        let (head, tail) = callee.split_at(cut);
        let items = spelling.binding.replace("{callee}", callee);
        let call = spelling
          .call
          .replace("{callee}", callee)
          .replace("{head}", head)
          .replace("{tail}", tail);
        let source = placement
          .1
          .replace("{items}", &items)
          .replace("{call}", &call);
        let name = format!("{} of `{callee}`, in {}", spelling.what, placement.0);

        let scan = match scan("selftest.rs", &source) {
          Ok(scan) => scan,
          Err(message) => {
            problems.push(format!("{name}: {message}\n{source}"));
            continue;
          }
        };
        let counted = scan
          .hits
          .iter()
          .any(|hit| hit.what == Occurrence::Call && hit.callee == callee);
        let refused = !scan.refusals.is_empty()
          || scan
            .hits
            .iter()
            .any(|hit| hit.what == Occurrence::Unclassified);

        if !counted && !refused {
          problems.push(format!(
            "{name}: invisible — nothing counted the call and nothing refused the spelling\n{source}"
          ));
        } else if spelling.followed && !counted {
          problems.push(format!(
            "{name}: a spelling the census is written to follow was not counted\n{source}"
          ));
        } else if spelling.followed && !scan.refusals.is_empty() {
          problems.push(format!(
            "{name}: a spelling the census follows is refused anyway: {:?}",
            scan.refusals
          ));
        } else if !spelling.followed && !refused {
          problems.push(format!(
            "{name}: counted, without refusing a spelling one file cannot follow\n{source}"
          ));
        }
      }
    }
  }
  problems
}
