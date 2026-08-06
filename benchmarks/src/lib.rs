//! Like-for-like benchmark support: smear's GraphQL lossless CST parser vs `apollo-parser`.
//!
//! Both sides build a **rowan 0.16 CST** over the same source. `apollo-parser` depends on
//! `rowan = "0.16.0"` directly; smear reaches the same crate through tokora, and cargo unifies
//! the two into one build of one `rowan`. So the artifact produced is the same kind of thing,
//! not merely analogous.
//!
//! This module holds everything the timing harness needs to be *honest*:
//!
//! * the corpus, taken byte-for-byte from `apollo-parser`'s own benches and test data, so the
//!   inputs cannot be accused of having been chosen to flatter smear;
//! * the three tiers' work functions, written once and called from both the correctness gate
//!   and the criterion bench, so what is measured is what was checked;
//! * the correctness gate itself — error-free parse, byte-exact round-trip, non-trivial node
//!   counts — which decides which entries are eligible to be timed at all.
//!
//! Nothing here is timed. `benches/apollo_comparison.rs` does the timing and calls into this.

use core::hint::black_box;

use apollo_parser::cst::{self, CstNode as _};
use smear::parser::graphql::lossless::{
  self as smear_lossless, GraphqlLosslessLexer, Parse as SmearParse,
  ast::{self as smear_ast, cast as smear_cast},
};
use tokora::{Lexer as _, emitter::Severity};

pub mod dump;

/// One corpus entry: a name, its source, and which of `apollo-parser`'s two published
/// traversals matches its content.
#[derive(Debug, Clone, Copy)]
pub struct Entry {
  /// Short id used in criterion benchmark names and in the report's tables.
  pub name: &'static str,
  /// The source text, embedded at compile time so no I/O happens inside a timed region.
  pub source: &'static str,
  /// Where the bytes came from, reproduced in the report.
  pub provenance: &'static str,
  /// Which tier-2 traversal shape applies.
  pub shape: Shape,
}

impl Entry {
  /// The entry's size in bytes — criterion's throughput denominator.
  #[inline]
  pub fn len(&self) -> usize {
    self.source.len()
  }

  /// Whether the entry is empty. Present only because clippy pairs it with [`Entry::len`].
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.source.is_empty()
  }
}

/// Which of `apollo-parser`'s two published bench traversals a corpus entry calls for.
///
/// `apollo-parser` ships two, and applying the executable one to a schema document would walk
/// nothing at all — a vacuous tier 2. So each entry gets the one that matches its content, and
/// both sides run the *same* one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Shape {
  /// `benches/query.rs`: walk operation definitions, walk their selection sets, black-box each
  /// field's own selection set.
  Executable,
  /// `benches/supergraph.rs`: walk object type definitions, walk their field definitions,
  /// black-box each field's type.
  Sdl,
}

/// The corpus. Every byte is `apollo-parser`'s own.
///
/// `alias.graphql` and `0032_supergraph.graphql` are verbatim copies (verified by sha256 against
/// the upstream checkout); `example_query.graphql` is the inline query string from
/// `apollo-parser`'s `benches/query.rs`, written out with its escapes resolved and no trailing
/// newline.
pub const CORPUS: &[Entry] = &[
  Entry {
    name: "example_query",
    source: include_str!("../corpus/example_query.graphql"),
    provenance: "apollo-parser benches/query.rs, inline `query ExampleQuery` string",
    shape: Shape::Executable,
  },
  Entry {
    name: "supergraph",
    source: include_str!("../corpus/0032_supergraph.graphql"),
    provenance: "apollo-parser test_data/parser/ok/0032_supergraph.graphql",
    shape: Shape::Sdl,
  },
  Entry {
    name: "alias",
    source: include_str!("../corpus/alias.graphql"),
    provenance: "apollo-parser benches/testdata/alias.graphql",
    shape: Shape::Executable,
  },
];

// ---------------------------------------------------------------------------
// Tier 0 — lex only
// ---------------------------------------------------------------------------

/// Drive smear's GraphQL **lossless** lexer to exhaustion, black-boxing every token.
///
/// Returns the token count. Panics on a lexer error: the gate has already established that the
/// corpus lexes clean, so an error here means the harness is measuring something other than what
/// it checked.
#[inline]
pub fn smear_lex(src: &str) -> usize {
  let mut lexer = GraphqlLosslessLexer::<'_, str>::new(src);
  let mut count = 0usize;
  while let Some(res) = lexer.lex() {
    black_box(res.expect("smear lexer error on a gate-passing entry"));
    count += 1;
  }
  count
}

/// Drive `apollo_parser::Lexer` to exhaustion, black-boxing every token.
///
/// The same shape as `apollo-parser`'s own `bench_query_lexer`. Note that apollo emits a
/// terminal `Eof` token and smear does not, so apollo's count is one higher for the same input.
#[inline]
pub fn apollo_lex(src: &str) -> usize {
  let lexer = apollo_parser::Lexer::new(src);
  let mut count = 0usize;
  for res in lexer {
    black_box(res.expect("apollo lexer error on a gate-passing entry"));
    count += 1;
  }
  count
}

// ---------------------------------------------------------------------------
// Tier 1 — parse to CST
// ---------------------------------------------------------------------------

/// Parse to smear's lossless rowan CST. No traversal.
#[inline]
pub fn smear_parse(src: &str) -> SmearParse {
  smear_lossless::parse_document(src)
}

/// Parse to apollo's rowan CST. No traversal.
#[inline]
pub fn apollo_parse(src: &str) -> apollo_parser::SyntaxTree {
  apollo_parser::Parser::new(src).parse()
}

// ---------------------------------------------------------------------------
// Tier 2 — typed traversal over an already-built tree
// ---------------------------------------------------------------------------

/// smear's stand-in for `apollo_parser::cst::Type`, which smear's typed layer does not have.
///
/// apollo generates a `Type` union enum and reaches it in **one** child scan that tests three
/// kinds. smear's `FieldDefinition` exposes the three arms as three separate getters, so the
/// equivalent call costs up to **three** child scans. Materialising the result into this enum
/// makes the two sides produce the same shape of value; it does not erase the extra scans, which
/// are a genuine API difference and are called out in the report.
#[derive(Debug)]
pub enum SmearFieldTy {
  /// `T`
  Named(smear_ast::NamedType),
  /// `[T]`
  List(smear_ast::ListType),
  /// `T!`
  NonNull(smear_ast::NonNullType),
}

/// The one [`smear_ast::Document`] a `parse_document` tree carries.
///
/// A structural difference from apollo worth naming: apollo's tree root **is** the `Document`,
/// while smear's root is a `Root` node that wraps exactly one `Document`. So smear's typed
/// traversal starts one `cast::child` deeper, and smear's CST carries one node apollo's does not.
#[inline]
pub fn smear_document(parse: &SmearParse) -> smear_ast::Document {
  smear_cast::child(&parse.syntax()).expect("Root wraps exactly one Document")
}

/// Walk operation definitions and black-box each field's selection set.
///
/// `apollo-parser`'s `benches/query.rs` traversal, on smear's typed layer. apollo iterates the
/// document's `definitions()` and discriminates on a `Definition` union; smear has no union and
/// exposes one iterator per kind, so `operation_definitions()` is the counterpart. Both are a
/// single ordered scan of the document's children with a kind test, and both visit exactly the
/// operation definitions, in document order.
///
/// Returns the number of fields visited — the gate uses it to prove the traversal is not vacuous.
#[inline]
pub fn smear_traverse_executable(parse: &SmearParse) -> usize {
  let doc = smear_document(parse);
  let mut visited = 0usize;
  for operation in doc.operation_definitions() {
    let selection_set = operation
      .selection_set()
      .expect("the node SelectionSet is not optional in the spec; qed");
    for field in selection_set.fields() {
      black_box(field.selection_set());
      visited += 1;
    }
  }
  visited
}

/// `benches/query.rs`'s traversal, verbatim apart from returning a visit count.
#[inline]
pub fn apollo_traverse_executable(tree: &apollo_parser::SyntaxTree) -> usize {
  let document = tree.document();
  let mut visited = 0usize;
  for definition in document.definitions() {
    if let cst::Definition::OperationDefinition(operation) = definition {
      let selection_set = operation
        .selection_set()
        .expect("the node SelectionSet is not optional in the spec; qed");
      for selection in selection_set.selections() {
        if let cst::Selection::Field(field) = selection {
          black_box(field.selection_set());
          visited += 1;
        }
      }
    }
  }
  visited
}

/// Walk object type definitions and black-box each field definition's type.
///
/// `apollo-parser`'s `benches/supergraph.rs` traversal, on smear's typed layer.
///
/// One deliberate deviation from upstream, applied identically to **both** sides: upstream
/// `.expect()`s `fields_definition()`, which would panic on a legal `type T @directive` with no
/// field block. Both sides here skip such a definition instead, so neither can panic and both do
/// the same work.
#[inline]
pub fn smear_traverse_sdl(parse: &SmearParse) -> usize {
  let doc = smear_document(parse);
  let mut visited = 0usize;
  for object in doc.object_type_definitions() {
    if let Some(fields) = object.fields_definition() {
      for field in fields.field_definitions() {
        let ty = field
          .named_type()
          .map(SmearFieldTy::Named)
          .or_else(|| field.list_type().map(SmearFieldTy::List))
          .or_else(|| field.non_null_type().map(SmearFieldTy::NonNull));
        black_box(ty);
        visited += 1;
      }
    }
  }
  visited
}

/// `benches/supergraph.rs`'s traversal, with the `fields_definition` deviation described on
/// [`smear_traverse_sdl`].
#[inline]
pub fn apollo_traverse_sdl(tree: &apollo_parser::SyntaxTree) -> usize {
  let document = tree.document();
  let mut visited = 0usize;
  for definition in document.definitions() {
    if let cst::Definition::ObjectTypeDefinition(object) = definition
      && let Some(fields) = object.fields_definition()
    {
      for field in fields.field_definitions() {
        black_box(field.ty());
        visited += 1;
      }
    }
  }
  visited
}

/// Parse and traverse in one call — tier 2's smear side.
#[inline]
pub fn smear_parse_and_traverse(src: &str, shape: Shape) -> usize {
  let parse = smear_parse(src);
  match shape {
    Shape::Executable => smear_traverse_executable(&parse),
    Shape::Sdl => smear_traverse_sdl(&parse),
  }
}

/// Parse and traverse in one call — tier 2's apollo side.
#[inline]
pub fn apollo_parse_and_traverse(src: &str, shape: Shape) -> usize {
  let tree = apollo_parse(src);
  match shape {
    Shape::Executable => apollo_traverse_executable(&tree),
    Shape::Sdl => apollo_traverse_sdl(&tree),
  }
}

// ---------------------------------------------------------------------------
// Correctness gate
// ---------------------------------------------------------------------------

/// What one parser did with one corpus entry.
#[derive(Debug, Clone)]
pub struct SideGate {
  /// Number of hard parse errors. Must be zero to pass.
  pub errors: usize,
  /// Whether `tree.text().to_string() == source`. Must hold to pass.
  pub round_trips: bool,
  /// Length of the round-tripped text, so a failure can be described rather than just flagged.
  pub round_trip_len: usize,
  /// Interior nodes in the CST, including the root.
  pub nodes: usize,
  /// Leaf tokens in the CST.
  pub tokens: usize,
  /// Tokens the standalone lexer produced for the same source.
  pub lex_tokens: usize,
  /// Nodes the tier-2 traversal visited.
  pub traversal_visits: usize,
}

impl SideGate {
  /// Whether this side is fit to be timed.
  ///
  /// Non-zero node and token counts are the "non-trivial tree" half of gate 3: a parser that
  /// returned a bare root would round-trip only on empty input, but the check is cheap and
  /// states the requirement explicitly rather than leaving it implied.
  #[inline]
  pub fn passed(&self) -> bool {
    self.errors == 0 && self.round_trips && self.nodes > 0 && self.tokens > 0
  }
}

/// Both sides' verdict on one corpus entry.
#[derive(Debug, Clone)]
pub struct GateRow {
  /// The entry's name.
  pub name: &'static str,
  /// The entry's size in bytes.
  pub bytes: usize,
  /// Which traversal shape tier 2 used.
  pub shape: Shape,
  /// smear's verdict.
  pub smear: SideGate,
  /// apollo's verdict.
  pub apollo: SideGate,
}

impl GateRow {
  /// Whether the entry is eligible for timing. Both sides must pass; an entry that fails is
  /// excluded from the timing run **and named** in the report.
  #[inline]
  pub fn eligible(&self) -> bool {
    self.smear.passed() && self.apollo.passed()
  }
}

fn gate_smear(entry: &Entry) -> SideGate {
  let parse = smear_parse(entry.source);
  let root = parse.syntax();
  let text = root.text().to_string();
  let nodes = root.descendants().count();
  let tokens = root
    .descendants_with_tokens()
    .filter(|e| e.as_token().is_some())
    .count();

  SideGate {
    errors: parse
      .diagnostics()
      .iter()
      .filter(|d| d.severity() == Severity::Error)
      .count(),
    round_trips: text == entry.source,
    round_trip_len: text.len(),
    nodes,
    tokens,
    lex_tokens: smear_lex(entry.source),
    traversal_visits: match entry.shape {
      Shape::Executable => smear_traverse_executable(&parse),
      Shape::Sdl => smear_traverse_sdl(&parse),
    },
  }
}

fn gate_apollo(entry: &Entry) -> SideGate {
  let tree = apollo_parse(entry.source);
  let document = tree.document();
  let root = document.syntax();
  let text = root.text().to_string();
  let nodes = root.descendants().count();
  let tokens = root
    .descendants_with_tokens()
    .filter(|e| e.as_token().is_some())
    .count();

  SideGate {
    errors: tree.errors().len(),
    round_trips: text == entry.source,
    round_trip_len: text.len(),
    nodes,
    tokens,
    lex_tokens: apollo_lex(entry.source),
    traversal_visits: match entry.shape {
      Shape::Executable => apollo_traverse_executable(&tree),
      Shape::Sdl => apollo_traverse_sdl(&tree),
    },
  }
}

/// Run the correctness gate over the whole corpus.
///
/// Nothing is timed here. Every entry is parsed by both sides, round-tripped, counted and
/// traversed; the caller decides what to do with the verdicts.
pub fn run_gate() -> Vec<GateRow> {
  CORPUS
    .iter()
    .map(|entry| GateRow {
      name: entry.name,
      bytes: entry.len(),
      shape: entry.shape,
      smear: gate_smear(entry),
      apollo: gate_apollo(entry),
    })
    .collect()
}

/// One side's node-kind histogram: `(kind name, occurrence count)` pairs, sorted by descending
/// count.
///
/// Named so the pair `kind_histograms` returns spells as two named types rather than the
/// `clippy::type_complexity`-tripping tuple of nested generics written out in full.
pub type KindHistogram = Vec<(String, usize)>;

/// Node-kind histograms for one entry, both sides, sorted by descending count.
///
/// Gate 3 requires that a node-count difference be *explained* rather than published bare. The
/// two kind spaces have no common vocabulary — they are different `rowan::Language`s — so the
/// histograms cannot be diffed automatically; what they do is show, per side, which kinds carry
/// the difference, which is enough for a reader to name the cause.
pub fn kind_histograms(entry: &Entry) -> (KindHistogram, KindHistogram) {
  fn sorted(mut counts: std::collections::HashMap<String, usize>) -> KindHistogram {
    let mut out: KindHistogram = counts.drain().collect();
    out.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    out
  }

  let parse = smear_parse(entry.source);
  let mut smear_counts: std::collections::HashMap<String, usize> = Default::default();
  for node in parse.syntax().descendants() {
    *smear_counts
      .entry(format!("{:?}", node.kind()))
      .or_default() += 1;
  }

  let tree = apollo_parse(entry.source);
  let mut apollo_counts: std::collections::HashMap<String, usize> = Default::default();
  for node in tree.document().syntax().descendants() {
    *apollo_counts
      .entry(format!("{:?}", node.kind()))
      .or_default() += 1;
  }

  (sorted(smear_counts), sorted(apollo_counts))
}

/// Print the gate's findings as a markdown table on stdout, then a line per exclusion.
///
/// Called from the bench before any timing so the run's own log carries the evidence, and from
/// the `gate` binary so the numbers can be captured on their own.
pub fn print_gate(rows: &[GateRow]) {
  println!("## Correctness gate");
  println!();
  println!(
    "| entry | bytes | shape | side | errors | round-trip | CST nodes | CST tokens | lexer tokens | tier-2 visits | verdict |"
  );
  println!("|---|---:|---|---|---:|---|---:|---:|---:|---:|---|");
  for row in rows {
    for (side, gate) in [("smear", &row.smear), ("apollo", &row.apollo)] {
      println!(
        "| {} | {} | {:?} | {} | {} | {} | {} | {} | {} | {} | {} |",
        row.name,
        row.bytes,
        row.shape,
        side,
        gate.errors,
        if gate.round_trips {
          "byte-exact".to_string()
        } else {
          format!("MISMATCH ({} bytes out)", gate.round_trip_len)
        },
        gate.nodes,
        gate.tokens,
        gate.lex_tokens,
        gate.traversal_visits,
        if gate.passed() { "pass" } else { "FAIL" },
      );
    }
  }
  println!();

  let excluded: Vec<&GateRow> = rows.iter().filter(|r| !r.eligible()).collect();
  if excluded.is_empty() {
    println!("All {} corpus entries eligible for timing.", rows.len());
  } else {
    println!("EXCLUDED FROM TIMING ({}):", excluded.len());
    for row in excluded {
      println!(
        "  - {}: smear {}, apollo {}",
        row.name,
        if row.smear.passed() {
          "pass".to_string()
        } else {
          format!(
            "FAIL (errors={}, round_trips={})",
            row.smear.errors, row.smear.round_trips
          )
        },
        if row.apollo.passed() {
          "pass".to_string()
        } else {
          format!(
            "FAIL (errors={}, round_trips={})",
            row.apollo.errors, row.apollo.round_trips
          )
        },
      );
    }
  }
  println!();
}
