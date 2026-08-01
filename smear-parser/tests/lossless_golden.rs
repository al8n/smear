#![cfg(feature = "rowan")]

//! Gate 5: golden trees — the **shape** gate, and the only one that can see a mis-shaped tree.
//!
//! Gates 1, 2 and 3 are each green, each correct to be green, and **each provably blind to
//! structural loss.** That is not a suspicion; it is measured, and every one of the three pins its
//! own blindness as a test:
//!
//! - gate 1 compares one bit — the verdict — so a tree whose nodes moved is invisible to it, and
//!   `type T { """b""" "a" }` loses its whole `ObjectTypeDefinition` while both suites reject;
//! - gate 2 compares the padded tree's `descendants()` against its own compact tree's, which never
//!   sees a token's placement at all;
//! - gate 3 compares `text()`, which is the tree with its shape projected away — eight mutations
//!   were run against it and **not one made `text()` differ from `src`.**
//!
//! The same bytes can come back through genuinely different trees: `query Q { f }` is an
//! `OperationDefinition` through the mixed root and five `Error` nodes through the SDL-only root,
//! and the two are byte-identical. Everything in that gap is this file's to catch.
//!
//! # The format, and why it is this one
//!
//! rust-analyzer's `.rast` style: one line per node and per token, two spaces of indent per level
//! of depth, each line carrying the kind and the absolute byte range, and each **token** line
//! additionally carrying its text, escaped. Nothing else — no source echo, no summary.
//!
//! Since text is identical across this gate's entire target class, the render has to foreground
//! what text throws away, and each of the four columns is one axis of structural change:
//!
//! - **indent** — a re-parented subtree shifts a whole block, so re-parenting is the loudest diff
//!   the format can produce rather than the quietest;
//! - **kind** — a production that opens the wrong node changes one word per site, which is what a
//!   `NamedType` silently becoming a `ListType` looks like;
//! - **range** — a node boundary that moved changes numbers even when no line appears or vanishes,
//!   which is the "`Arguments` opened after the `(` instead of before" shape;
//! - **token text** — tokens are in the tree, so a token attached to the wrong parent is a moved
//!   line rather than an invisible one. Trivia is included for exactly this reason: which node a
//!   comment commits into is a real decision this suite makes, and no other gate records it.
//!
//! Token text is escaped by [`escape`] rather than by `char::escape_debug`, on purpose. A token
//! carrying a newline would otherwise break the one-line-per-element property outright, and every
//! diff after it would mis-align; and std's notion of "printable" is a Unicode table that can move
//! between toolchains, which is not something a byte-compared golden should depend on.
//!
//! Kinds print in their Rust spelling (`ObjectTypeDefinition`, not `OBJECT_TYPE_DEFINITION`) so a
//! line lifted straight out of a diff greps into the productions that emit it.
//!
//! # Re-blessing is deliberate, in two steps, and cannot be made automatic
//!
//! The failure mode of a golden gate is not that it reds — it is that it gets re-blessed on sight
//! until it records whatever the code does. Three things are arranged against that:
//!
//! - **Nothing is ever written without `UPDATE_GOLDEN=1`.** A missing golden is a failure, not an
//!   invitation; the gate never creates a file it is then compared against in the same run.
//! - **An update run still fails.** With `UPDATE_GOLDEN=1` set, a changed golden is written *and*
//!   the test panics, naming every file it touched and telling the reader to look at the diff and
//!   re-run without the variable. So blessing takes two deliberate commands, and — the part that
//!   matters for CI — a build that somehow had `UPDATE_GOLDEN=1` set in its environment still reds
//!   on a real difference instead of silently laundering it into a pass.
//! - **The failure prints the diff**, as unified hunks against the committed file, so the reader
//!   deciding whether to bless does not have to run anything to see what changed.
//!
//! # What reading the goldens found
//!
//! Every one of the 91 trees was read against its source before being committed, because a golden
//! blessed unread records whatever the parser did, bug included, and turns it into an expectation.
//! The sweep found one thing, and it is the class this gate was built for:
//! **`root_operation_type_definition` opens its `NamedType` before the trivia in front of it**, so
//! in `schema { query: Q }` that node spans `" Q"` and not `"Q"`. Six goldens carry it. See
//! [`OPENS_ON_LEADING_TRIVIA`] for the mechanism, why it is left unfixed here, and
//! [`only_the_recorded_nodes_open_on_their_leading_trivia`], which is that eye-reading turned into
//! a standing check so it cannot grow a seventh site unnoticed.
//!
//! Two further shapes were read, judged deliberate, and left unpinned: a `FieldDefinition` whose
//! type is missing recovers by consuming the enclosing `}` into an `Error`, so the surrounding
//! `FieldsDefinition` closes without a right brace; and trailing trivia attaches to whichever node
//! was still open at the peek that crossed it, which is the preceding sibling inside a list and
//! the enclosing list after its last element. Both are recovery and attachment policy rather than
//! loss, and both are now on the page where a change to either shows up as a diff.
//!
//! # What is deliberately not frozen here
//!
//! A trailing region that no token covers currently lands **outside** `Document`, as a `Gap` token
//! child of `Root`, while the same garbage lands *inside* `Document` when anything lexable follows
//! it. That asymmetry is a known tokora issue with a chosen fix, and one corpus entry is sensitive
//! to it: see [`PENDING_OPTION_B`] and
//! [`only_one_golden_is_sensitive_to_trailing_gap_placement`], which is the census that keeps the
//! blast radius of that fix to the single file named there.

use std::{collections::BTreeSet, fmt::Write as _, path::PathBuf};

use rowan::{NodeOrToken, WalkEvent};
use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{SyntaxNode, document::test_support::parse_type_system_document, parse_str},
};

/// The environment variable that turns a comparison run into a blessing run.
const UPDATE_VAR: &str = "UPDATE_GOLDEN";

/// The one golden whose expected content will change when tokora's trailing-gap placement is
/// fixed, and the only one that may be re-blessed as part of that fix.
///
/// `invalid_unterminated_string.graphql` ends in a lexer error, so its last 17 bytes are carried
/// by the sink's gap tiling rather than by any committed token — and because nothing lexable
/// follows, that `Gap` attaches to `Root` instead of to `Document`. The same bytes in the middle
/// of a document attach inside it. The chosen fix (option B) makes a trailing gap join the last
/// child when the stream is balanced, which makes `Document.text() == source` unconditional and
/// moves exactly this one line of exactly this one golden one level deeper.
///
/// It is recorded here rather than omitted from the golden set because omitting it would drop the
/// corpus's only gap-tiled tree — the single entry whose shape is not the grammar's — from the
/// one gate that looks at shape. Naming it costs one expected re-bless; dropping it costs the
/// coverage permanently.
const PENDING_OPTION_B: &[&str] = &["invalid_unterminated_string.graphql"];

/// The token images the lossless lexer surfaces as trivia, as they enter the tree.
///
/// `Gap` is deliberately not among them: it is the sink's fill for a region no token covers, not
/// something the lexer produced.
const TRIVIA_IMAGES: &[K] = &[K::Space, K::Tab, K::Newline, K::Comma, K::Comment, K::Bom];

/// Every `parent > node` pairing where a node currently opens *before* the trivia in front of it,
/// so that the trivia lands inside the node's range instead of beside it.
///
/// **Found by reading the goldens, and the second entry is a defect.** Reviewing all 91 trees by
/// eye surfaced one shape that disagrees with the other 75 of its kind:
/// `root_operation_type_definition` (`definition.rs`) calls `named_type` directly, and
/// `named_type` opens `K::NamedType` and only then calls `expect`, whose trivia skip therefore
/// runs *inside* the node. Every other call site reaches `named_type` through `type_ref`, which
/// documents doing its head peek first precisely "so the leading trivia it crosses is committed
/// BEFORE the mark". So in `schema { query: Q }` the `NamedType` spans `" Q"` rather than `"Q"`,
/// and a consumer that highlights or renames through that range takes the space with it.
///
/// It is a boundary on the wrong side of trivia — the exact class Step 3's mutation imitates —
/// and it survives gates 1 through 4 untouched: the verdict is unchanged, the descendant kinds
/// are unchanged, the bytes are unchanged, and `NamedType::name()` still finds its token.
///
/// **Left unfixed here on purpose.** The fix is a production change in `definition.rs` and belongs
/// to a change that is reviewed as one, not to the commit that adds the gate that found it. What
/// this pin buys in the meantime is that the six affected goldens are a *stated* expectation
/// rather than a silent one, and that no seventh site can join them unnoticed.
///
/// `Root > Document` is the other entry and is correct: a file's leading comment has to live
/// somewhere, and `Document` spans the whole source.
const OPENS_ON_LEADING_TRIVIA: &[&str] =
  &["Root > Document", "RootOperationTypeDefinition > NamedType"];

/// Fixtures the corpus does not carry, each one a tree whose *shape* is its whole content.
///
/// The corpus is a grammar-coverage set: it is organised around which productions run, and it is
/// shared with gates 1, 2 and 3. These are organised around which structural claims can silently
/// stop being true. Each is a source whose bytes say almost nothing and whose tree says the thing
/// under test.
const SHAPE_CASES: &[(&str, &str)] = &[
  // Task 10's finding, on the page. The body fails by unwinding, so `node_at`'s mark is never
  // spent, the `ObjectTypeDefinition` is never opened, and its tokens end up bare children of
  // `Document`. Gate 1 passes (both suites reject) and gate 3 passes (every byte is there). The
  // day this loss is fixed, this golden reds and someone reads the diff — which is the entire
  // reason it is committed with the bug in it.
  (
    "lost_object_type_definition",
    "type T { \"\"\"b\"\"\" \"a\" }",
  ),
  // The control for the line above: this body fails too, but it recovers *in place*, so nothing
  // unwinds past the mark and the definition node survives. Without it, "no `ObjectTypeDefinition`
  // in the tree" would be satisfied by any input that simply is not an object type.
  ("kept_object_type_definition", "type T { x: }"),
  // Every type-reference shape in one source, plus the argument, directive and default-value
  // nesting they sit inside. This is the concentrated witness for a production that opens the
  // wrong node kind, and for one that opens its node on the wrong side of a delimiter.
  (
    "type_reference_nesting",
    "query Q($v: [Int!]! = 3) { f(a: $v) @d(x: 1) }",
  ),
  // Type references nested in themselves, where an off-by-one-level parent is otherwise
  // undetectable: `[[A!]!]!` and `[A!]!` carry the same kinds in the same order.
  ("nested_list_types", "type T { f: [[A!]!]! }"),
  // Where trivia commits. Comments, commas and newlines are tokens in this suite, so which node
  // each one lands in is a decision the atoms make on every peek — and gate 2, which is the trivia
  // gate, compares only `descendants()` and cannot see a single one of them.
  ("trivia_attachment", "{\n  # c\n  a ,\n  b\n}\n"),
  // Recovery geometry: how wide the `Error` node is, where it starts, and whether the definitions
  // on either side of it survive intact.
  (
    "top_level_recovery",
    "type A { f: Int } ??? type B { g: Int }",
  ),
  // Trivia in front of a *retro-wrap* probe, in both of the positions that have one: the `!` that
  // turns a type into a `NonNullType`, and the `:` that turns a name into an `Alias`. Added
  // because a mutation found the gap rather than because the shape looked interesting — deleting
  // `try_eat`'s leading-trivia skip left this whole gate green, and the reason was that no corpus
  // entry writes `Int !` or `alias : f`. Its two `try_eat` call sites (`ty.rs`, `selection.rs`)
  // are the only ones in the suite, and without this fixture neither is reachable from here.
  (
    "retro_wrap_across_trivia",
    "{ alias : f }\ntype T { g: Int ! }\n",
  ),
];

/// The directory the `.rast` files live in.
fn golden_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("golden")
}

/// Every `.graphql` file in the shared corpus, in a deterministic order.
fn corpus() -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .collect();
  files.sort();
  files
    .into_iter()
    .map(|p| {
      let name = p.file_name().unwrap().to_string_lossy().to_string();
      let src = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", p.display()));
      (name, src)
    })
    .collect()
}

/// The golden stem a corpus entry is filed under: the file name without its extension.
fn corpus_stem(name: &str) -> String {
  name
    .strip_suffix(".graphql")
    .unwrap_or(name)
    .replace('.', "_")
}

/// The golden stem a [`SHAPE_CASES`] entry is filed under.
///
/// The `shape_` prefix keeps the two sets from ever colliding — every corpus entry is named
/// `valid_…` or `invalid_…`, which
/// [`the_golden_directory_holds_exactly_the_expected_files`] re-checks rather than assumes.
fn shape_stem(name: &str) -> String {
  format!("shape_{name}")
}

/// Escape a token's text so one token is always one line, forever.
///
/// Backslash, quote and the three whitespace forms that would break the layout are spelled out;
/// every other C0/C1 control, plus the byte-order mark, becomes `\u{…}`. Anything else — including
/// ordinary multi-byte text like `café` or an emoji — is written through verbatim, so a golden
/// stays readable.
///
/// `char::escape_debug` was the obvious alternative and is the wrong tool: its notion of
/// "printable" is a Unicode table that moves between toolchain releases, and a byte-compared
/// golden that changes when the compiler does is a golden that teaches people to re-bless on
/// sight. `char::is_control` is a fixed two-range test and cannot drift.
fn escape(text: &str) -> String {
  let mut out = String::with_capacity(text.len());
  for ch in text.chars() {
    match ch {
      '\\' => out.push_str("\\\\"),
      '"' => out.push_str("\\\""),
      '\n' => out.push_str("\\n"),
      '\r' => out.push_str("\\r"),
      '\t' => out.push_str("\\t"),
      c if c.is_control() || c == '\u{feff}' => {
        let _ = write!(out, "\\u{{{:04x}}}", c as u32);
      }
      c => out.push(c),
    }
  }
  out
}

/// Serialize a tree in `.rast` style: one line per element, indented by depth.
///
/// Nodes render as `Kind@start..end`; tokens render as `Kind@start..end "text"`. A token always
/// carries a quoted text and a node never does, which is what tells the two apart on the page.
fn render(root: &SyntaxNode) -> String {
  let mut out = String::new();
  let mut depth = 0usize;
  for event in root.preorder_with_tokens() {
    match event {
      WalkEvent::Enter(element) => {
        for _ in 0..depth {
          out.push_str("  ");
        }
        let range = element.text_range();
        let (start, end) = (u32::from(range.start()), u32::from(range.end()));
        match &element {
          NodeOrToken::Node(node) => {
            let _ = writeln!(out, "{:?}@{start}..{end}", node.kind());
          }
          NodeOrToken::Token(token) => {
            let _ = writeln!(
              out,
              "{:?}@{start}..{end} \"{}\"",
              token.kind(),
              escape(token.text())
            );
          }
        }
        depth += 1;
      }
      WalkEvent::Leave(_) => depth -= 1,
    }
  }
  out
}

/// The rendered tree for a source, through the ordinary mixed root.
fn render_str(src: &str) -> String {
  render(&parse_str(src).syntax())
}

/// A unified diff of two rendered trees, as hunks with two lines of context.
///
/// Written out rather than pulled in because the whole value of this gate is that its failure is
/// readable by someone who did not write the change, and a positional line-by-line comparison is
/// not: a lost node deletes one line *and* dedents everything under it, so a naive diff reports
/// every remaining line as changed and buries the one that matters.
fn unified_diff(expected: &str, actual: &str) -> String {
  const CONTEXT: usize = 2;
  /// Past this many lines the quadratic table stops being free; no golden in this suite is
  /// anywhere near it, and the fallback below is still readable.
  const LCS_LIMIT: usize = 4000;
  /// A panic message longer than this stops being read.
  const MAX_LINES: usize = 120;

  let old: Vec<&str> = expected.lines().collect();
  let new: Vec<&str> = actual.lines().collect();

  if old.len() > LCS_LIMIT || new.len() > LCS_LIMIT {
    return format!(
      "(trees too large to diff: {} committed lines vs {} produced lines)",
      old.len(),
      new.len()
    );
  }

  // Longest common subsequence, filled from the back so the walk below runs forwards.
  let (n, m) = (old.len(), new.len());
  let width = m + 1;
  let mut lcs = vec![0u32; (n + 1) * width];
  for i in (0..n).rev() {
    for j in (0..m).rev() {
      lcs[i * width + j] = if old[i] == new[j] {
        lcs[(i + 1) * width + j + 1] + 1
      } else {
        lcs[(i + 1) * width + j].max(lcs[i * width + j + 1])
      };
    }
  }

  // The edit script, as (marker, expected-line-number, text).
  let mut ops: Vec<(char, Option<usize>, &str)> = Vec::new();
  let (mut i, mut j) = (0usize, 0usize);
  while i < n && j < m {
    if old[i] == new[j] {
      ops.push((' ', Some(i + 1), old[i]));
      i += 1;
      j += 1;
    } else if lcs[(i + 1) * width + j] >= lcs[i * width + j + 1] {
      ops.push(('-', Some(i + 1), old[i]));
      i += 1;
    } else {
      ops.push(('+', None, new[j]));
      j += 1;
    }
  }
  while i < n {
    ops.push(('-', Some(i + 1), old[i]));
    i += 1;
  }
  while j < m {
    ops.push(('+', None, new[j]));
    j += 1;
  }

  // Group the changed ops into hunks, each padded with `CONTEXT` unchanged lines.
  let changed: Vec<usize> = ops
    .iter()
    .enumerate()
    .filter(|(_, (marker, _, _))| *marker != ' ')
    .map(|(index, _)| index)
    .collect();
  if changed.is_empty() {
    return "(no line differs — the two renders differ only in trailing bytes)".to_string();
  }

  let mut out = String::new();
  let mut emitted = 0usize;
  let mut cursor: Option<usize> = None;
  let mut truncated = false;
  for &index in &changed {
    let from = index.saturating_sub(CONTEXT);
    let to = (index + CONTEXT + 1).min(ops.len());
    let from = match cursor {
      Some(previous) if from <= previous => previous,
      Some(_) => {
        out.push_str("       ...\n");
        from
      }
      None => from,
    };
    for op in &ops[from..to] {
      if emitted >= MAX_LINES {
        truncated = true;
        break;
      }
      let (marker, line, text) = op;
      match line {
        Some(number) => {
          let _ = writeln!(out, "{marker} {number:>5} | {text}");
        }
        None => {
          let _ = writeln!(out, "{marker}       | {text}");
        }
      }
      emitted += 1;
    }
    if truncated {
      break;
    }
    cursor = Some(to);
  }
  if truncated {
    let _ = writeln!(
      out,
      "       ... (diff truncated at {MAX_LINES} lines; {} lines differ in all)",
      changed.len()
    );
  }
  out
}

/// One case's verdict against its committed golden.
enum Verdict {
  /// The golden exists and matches.
  Match,
  /// The golden does not exist at all.
  Missing,
  /// The golden exists and differs; the diff is against the committed file.
  Differs(String),
}

/// Compare one case against its golden, and — only under `UPDATE_GOLDEN=1` — write it.
fn check(stem: &str, produced: &str, blessing: bool) -> Verdict {
  let path = golden_dir().join(format!("{stem}.rast"));
  let committed = std::fs::read_to_string(&path).ok();

  let verdict = match &committed {
    Some(text) if text == produced => Verdict::Match,
    Some(text) => Verdict::Differs(unified_diff(text, produced)),
    None => Verdict::Missing,
  };

  if blessing && !matches!(verdict, Verdict::Match) {
    std::fs::create_dir_all(golden_dir()).expect("the golden directory could not be created");
    std::fs::write(&path, produced)
      .unwrap_or_else(|e| panic!("{} could not be written: {e}", path.display()));
  }

  verdict
}

/// Run a whole set of cases against their goldens and turn the result into a verdict.
///
/// Every case is checked before anything is reported, so one run tells a reader the full extent of
/// a change rather than the first file that happened to sort early.
fn gate(cases: &[(String, String)], set: &str) {
  let blessing = std::env::var(UPDATE_VAR).as_deref() == Ok("1");

  let mut missing: Vec<&str> = Vec::new();
  let mut differing: Vec<(&str, String)> = Vec::new();
  for (stem, produced) in cases {
    match check(stem, produced, blessing) {
      Verdict::Match => {}
      Verdict::Missing => missing.push(stem),
      Verdict::Differs(diff) => differing.push((stem, diff)),
    }
  }
  assert!(
    !cases.is_empty(),
    "the {set} golden set is empty, so this gate measured nothing"
  );
  if missing.is_empty() && differing.is_empty() {
    return;
  }

  let mut report = String::new();
  if blessing {
    // An update run writes and *then* fails. Two deliberate commands to bless, and a stray
    // `UPDATE_GOLDEN=1` in an environment can never turn a real difference into a pass.
    let _ = writeln!(
      report,
      "{UPDATE_VAR}=1: rewrote {} golden(s) in the {set} set ({} new, {} changed).\n\
       Nothing is blessed yet. Read `git diff -- smear-parser/tests/golden`, satisfy yourself that \
       every changed line is a tree you meant to produce, then re-run WITHOUT {UPDATE_VAR} set.",
      missing.len() + differing.len(),
      missing.len(),
      differing.len()
    );
  } else {
    let _ = writeln!(
      report,
      "{} golden tree(s) in the {set} set do not match the committed shape ({} missing, {} \
       changed).",
      missing.len() + differing.len(),
      missing.len(),
      differing.len()
    );
    if !differing.is_empty() {
      let _ = writeln!(
        report,
        "A changed tree carries the same bytes as before — gates 1, 2 and 3 cannot see this — so \
         read each diff as a claim about structure: an indent shift is a re-parented subtree, a \
         changed kind is a production opening the wrong node, and a changed range is a node \
         boundary that moved."
      );
    }
    let _ = writeln!(
      report,
      "If every change is intended, bless with `{UPDATE_VAR}=1 cargo test -p smear-parser \
       --features rowan --test lossless_golden`, which rewrites the files and then fails once more \
       so the diff gets read."
    );
  }
  for stem in &missing {
    let _ = writeln!(report, "\n  missing: tests/golden/{stem}.rast");
  }
  // Enough detail to decide on, capped so the message stays a message.
  const DETAILED: usize = 4;
  for (stem, diff) in differing.iter().take(DETAILED) {
    let _ = writeln!(report, "\n--- tests/golden/{stem}.rast\n{diff}");
  }
  if differing.len() > DETAILED {
    let _ = writeln!(report, "\nalso changed, diffs not shown:");
    for (stem, _) in differing.iter().skip(DETAILED) {
      let _ = writeln!(report, "  tests/golden/{stem}.rast");
    }
  }
  panic!("{report}");
}

/// Gate 5 proper: every corpus entry's tree has the committed shape.
#[test]
fn every_corpus_tree_matches_its_golden() {
  let cases: Vec<(String, String)> = corpus()
    .into_iter()
    .map(|(name, src)| (corpus_stem(&name), render_str(&src)))
    .collect();
  assert!(
    cases.len() >= 80,
    "only {} corpus entries reached the golden gate",
    cases.len()
  );
  gate(&cases, "corpus");
}

/// The structural fixtures the corpus does not carry.
///
/// See [`SHAPE_CASES`] for what each one is for. They are checked separately from the corpus so
/// that a corpus edit and a structural-claim edit are never the same diff.
#[test]
fn every_shape_fixture_matches_its_golden() {
  let cases: Vec<(String, String)> = SHAPE_CASES
    .iter()
    .map(|(name, src)| (shape_stem(name), render_str(src)))
    .collect();
  gate(&cases, "shape");
}

/// The golden directory holds exactly the files the two sets expect — no more, no fewer.
///
/// Without this, deleting a corpus entry leaves its `.rast` behind as a tree nothing produces any
/// more, and the gate above stays green while quietly shrinking. It is also what makes the
/// `shape_` prefix a guarantee rather than a convention.
#[test]
fn the_golden_directory_holds_exactly_the_expected_files() {
  let mut expected: BTreeSet<String> = corpus()
    .iter()
    .map(|(name, _)| format!("{}.rast", corpus_stem(name)))
    .collect();
  for (name, _) in SHAPE_CASES {
    let inserted = expected.insert(format!("{}.rast", shape_stem(name)));
    assert!(
      inserted,
      "the shape fixture {name:?} collides with another golden's file name"
    );
  }
  assert_eq!(
    expected.len(),
    corpus().len() + SHAPE_CASES.len(),
    "two cases share a golden file name, so one of them is not being checked"
  );

  let dir = golden_dir();
  let found: BTreeSet<String> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the golden directory at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a golden directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "rast"))
    .map(|path| path.file_name().unwrap().to_string_lossy().to_string())
    .collect();

  let stale: Vec<&String> = found.difference(&expected).collect();
  let absent: Vec<&String> = expected.difference(&found).collect();
  assert!(
    stale.is_empty(),
    "tests/golden holds {stale:?}, which no case produces any more — delete them, or the gate is \
     recording a tree nothing builds"
  );
  assert!(absent.is_empty(), "no golden is committed for {absent:?}");
}

/// The render answers differently for two trees that `text()` cannot tell apart.
///
/// This is the whole premise, as live values rather than as prose. `query Q { f }` is an
/// `OperationDefinition` through the mixed root and a run of `Error` nodes through the SDL-only
/// root; gate 3 measured that both round-trip byte for byte, so the *only* thing that can separate
/// them is a shape projection. If this ever passes because both renders are empty, or fails
/// because the two agree, the format has stopped doing its one job.
#[test]
fn the_render_answers_differently_for_trees_that_text_cannot_separate() {
  const SRC: &str = "query Q { f }";

  let mixed = parse_str(SRC);
  let sdl = parse_type_system_document(SRC);

  assert_eq!(mixed.syntax().text().to_string(), SRC);
  assert_eq!(
    sdl.syntax().text().to_string(),
    SRC,
    "the two trees must be byte-identical, or this test is not exhibiting the blind spot"
  );

  let mixed = render(&mixed.syntax());
  let sdl = render(&sdl.syntax());
  assert_ne!(
    mixed, sdl,
    "the render gave the same answer for an operation and for a pile of error nodes"
  );
  assert!(
    mixed.contains("OperationDefinition@"),
    "the mixed root's render lost the operation:\n{mixed}"
  );
  assert!(
    !sdl.contains("OperationDefinition@"),
    "the SDL-only root has no executable production, so its render must not name one:\n{sdl}"
  );
  assert!(
    sdl.contains("Error@"),
    "the SDL-only root recovers this source into error nodes:\n{sdl}"
  );
}

/// The render records a lost definition node, which is the defect class this gate exists for.
///
/// The golden for `shape_lost_object_type_definition` is committed **with the loss in it**. That is
/// deliberate and it is not the same thing as blessing a bug: the loss is Task 10's finding, it is
/// pinned from gate 1's side and from gate 3's side as a known blindness, and the value of holding
/// it here is that the day it is fixed the golden reds and the diff says exactly what came back.
///
/// What this test adds over the committed file is the *reading* of it — that the difference between
/// the two fixtures is the presence of the node and not something incidental — so that the pair
/// cannot rot into two files nobody remembers the point of.
#[test]
fn the_render_shows_a_lost_definition_node() {
  const LOST: &str = "type T { \"\"\"b\"\"\" \"a\" }";
  const KEPT: &str = "type T { x: }";

  let lost = render_str(LOST);
  let kept = render_str(KEPT);

  assert!(
    parse_str(LOST).has_errors() && parse_str(KEPT).has_errors(),
    "both fixtures must be failing parses, or gate 1 would separate them and this gate would not \
     be the only witness"
  );
  assert_eq!(parse_str(LOST).syntax().text().to_string(), LOST);
  assert_eq!(parse_str(KEPT).syntax().text().to_string(), KEPT);

  assert!(
    !lost.contains("ObjectTypeDefinition@"),
    "the unwinding body kept its definition node — the blind spot has closed, so this test should \
     become the assertion that it stays closed:\n{lost}"
  );
  assert!(
    kept.contains("ObjectTypeDefinition@"),
    "the control recovers in place and must keep its definition node, or the assertion above is \
     satisfied by any input that is not an object type:\n{kept}"
  );
}

/// Exactly one golden is sensitive to where a trailing gap attaches, and it is the one named.
///
/// A region no token covers attaches *inside* `Document` when anything lexable follows it and
/// *outside* — as a `Gap` child of `Root` — when nothing does. tokora's fix makes the trailing case
/// join the last child too. This census is what bounds the blast radius of that fix: it fails if a
/// second case becomes sensitive, and it fails if the fix lands, at which point exactly the
/// goldens listed in [`PENDING_OPTION_B`] may be re-blessed and this test updated to say the
/// asymmetry is gone.
///
/// The test is over both golden sets, not over the one entry, because "only this file is affected"
/// is a claim about all 91 of them.
#[test]
fn only_one_golden_is_sensitive_to_trailing_gap_placement() {
  fn trailing_gap(src: &str) -> bool {
    parse_str(src)
      .syntax()
      .children_with_tokens()
      .any(|element| element.as_token().is_some())
  }

  let sensitive: Vec<String> = corpus()
    .into_iter()
    .filter(|(_, src)| trailing_gap(src))
    .map(|(name, _)| name)
    .chain(
      SHAPE_CASES
        .iter()
        .filter(|(_, src)| trailing_gap(src))
        .map(|(name, _)| (*name).to_string()),
    )
    .collect();

  assert_eq!(
    sensitive, PENDING_OPTION_B,
    "the set of cases whose trailing region hangs off `Root` instead of off `Document` has \
     changed. If tokora's trailing-gap fix has landed, this set should now be empty and only the \
     goldens named in PENDING_OPTION_B may be re-blessed; if it has grown, a new case just became \
     sensitive and its golden should not be committed against the shape that is about to change."
  );

  // The positive control: the asymmetry itself, so this census cannot pass because nothing
  // anywhere produces a gap. One garbage byte, twice, differing only in what follows it.
  assert!(
    !trailing_gap("% {"),
    "a gap with something lexable after it belongs inside `Document`"
  );
  assert!(
    trailing_gap("{ %"),
    "a gap with nothing after it currently hangs off `Root`; if this now fails, option B has \
     landed and PENDING_OPTION_B is discharged"
  );

  // And the shape of the sensitive entry, stated where the reader is, so the expected re-bless is
  // one identified line rather than a file to squint at.
  let gap_owner = parse_str(
    &std::fs::read_to_string(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("corpus")
        .join(PENDING_OPTION_B[0]),
    )
    .expect("the pending corpus entry is unreadable"),
  )
  .syntax()
  .children_with_tokens()
  .filter_map(|element| element.into_token())
  .map(|token| token.kind())
  .collect::<Vec<K>>();
  assert_eq!(
    gap_owner,
    vec![K::Gap],
    "the pending entry's `Root` should carry exactly one direct token child, the trailing gap"
  );
}

/// Which nodes open on the wrong side of their leading trivia — a standing check, not a comment.
///
/// Reading 91 goldens by eye is what Step 2 asks for and it is not repeatable; this is that
/// reading turned into an assertion. It sweeps every tree in both golden sets for a node whose
/// first child is a trivia token and compares the `parent > node` pairings against
/// [`OPENS_ON_LEADING_TRIVIA`], which records the two that exist today and says which of them is a
/// defect.
///
/// It fails in both useful directions. A new production that opens its node before skipping
/// trivia adds a pairing; fixing `root_operation_type_definition` removes one, and should remove
/// the six affected goldens' `Space` lines in the same diff.
#[test]
fn only_the_recorded_nodes_open_on_their_leading_trivia() {
  fn pairings(src: &str) -> Vec<String> {
    parse_str(src)
      .syntax()
      .descendants()
      .filter_map(|node| {
        let first = node.children_with_tokens().next()?.into_token()?;
        if !TRIVIA_IMAGES.contains(&first.kind()) {
          return None;
        }
        Some(format!("{:?} > {:?}", node.parent()?.kind(), node.kind()))
      })
      .collect()
  }

  let mut found: Vec<String> = corpus()
    .iter()
    .flat_map(|(_, src)| pairings(src))
    .chain(SHAPE_CASES.iter().flat_map(|(_, src)| pairings(src)))
    .collect();
  found.sort();
  found.dedup();

  assert_eq!(
    found, OPENS_ON_LEADING_TRIVIA,
    "the set of nodes that swallow their own leading trivia has changed. A pairing that appeared \
     is a production opening its node before the trivia in front of it, which puts the node's \
     start on the wrong side of a space; a pairing that vanished is that being fixed, in which \
     case the goldens it affected change in the same diff and both should be blessed together"
  );

  // The positive control. Without it this can pass because no tree anywhere has leading trivia to
  // be wrong about — and the corpus really does contain shapes with none.
  assert!(
    pairings("schema { query: Q }")
      .contains(&"RootOperationTypeDefinition > NamedType".to_string()),
    "the recorded defect no longer reproduces on the smallest source that shows it"
  );
  assert!(
    pairings("type T { f: Int }").is_empty(),
    "an ordinary definition must open none of its nodes on trivia, or the sweep above cannot tell \
     the defect from the normal case"
  );
}
