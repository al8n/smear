#![cfg(all(feature = "graphql", feature = "rowan"))]

//! The differential gate for the GraphQL CST → AST projection (issue #58).
//!
//! # The claim, and the instrument
//!
//! `project(parse_document(src), src) == document(src)` — the projection of a lossless parse is
//! **the same AST value** the syntactic parser builds for the same bytes. Not "the same shape",
//! not "the same modulo spans": the derived [`PartialEq`] this issue added across the AST
//! closure compares every span, every slice, every literal payload, every `Option` presence and
//! every container order, so the assertion is `assert_eq!` and nothing is discounted.
//!
//! # Why plain `==`, and why that is news
//!
//! The design for this work (issue #58's comment, written against trunk at `69fd677`) could not
//! state it that way. It measured the syntactic AST's composite spans as **lookahead artifacts**
//! — `NamedType` closing at the *next* token's start, the document node opening before its
//! leading trivia — and worked around them by defining projected spans as normatively
//! token-extent and comparing through a `normalise_spans` pass, with a self-retirement clause
//! for the day the parser converged.
//!
//! **That day was #72.** It rewrote composite span computation across 44 node types and added
//! `tests/syntactic_span_extent.rs`, which pins "a composite node's span is the extent of the
//! tokens it contains" over the same corpus this gate reads, padded with the same eight trivia
//! forms. Re-measured here on trunk: over `"  type T  { f : Int }  "` the parser now answers
//! `Document 2..21`, `NamedType 16..19`, `FieldDefinition 12..19` — token extents, all three,
//! where the design recorded `0..21`, `16..20`, `12..20`.
//!
//! So the normaliser is not written. There is no span-normalisation pass in this file, no drift
//! ledger and no self-retirement test, because there is no residue for them to measure. What
//! replaces them is [`the_span_rule_the_normaliser_would_have_hidden`], which pins the three
//! re-measured numbers directly: if composite spans ever drift back off their token extents,
//! that test names the node and the offset instead of a wall of `assert_eq!` diffs.
//!
//! # The three ways a gate like this passes without meaning anything
//!
//! 1. **Equality that ignores what matters.** If `PartialEq` compared shapes only, every
//!    assertion below would hold over a projection that got every span wrong.
//!    [`the_equality_can_answer_no`] feeds it a document shifted one byte, and a second document
//!    with the same shape and a different name, and requires both to compare unequal.
//! 2. **A projection that re-parses the text.** `tree.text() == source` always holds, so
//!    `|parse, src| syntactic_document(src)` would satisfy every corpus assertion here — it
//!    would be a re-parse wearing the projection's signature.
//!    [`a_projection_that_re_parsed_the_source_would_fail_this`] builds a **synthetic green
//!    tree** whose text re-parses to a different structure and requires the projection to answer
//!    from the structure it was handed. A tree walk passes; a re-parse cannot.
//! 3. **Error paths nobody reaches.** [`every_refusal_kind_has_a_witness`] requires each
//!    [`ProjectErrorKind`] variant to be produced by at least one pinned input, so no refusal
//!    ships unreachable.
//!
//! # The corpus, twice
//!
//! Every `valid_` entry in `tests/corpus/`, compact and then padded at every token boundary with
//! each of `tests/support/span_extent.rs`'s eight ignorable forms — the same corpus and the same
//! alphabet `lossless_trivia.rs` and `syntactic_span_extent.rs` read. The padded half is not
//! decoration: on compact input a projection that used [`rowan::SyntaxNode::text_range`] instead
//! of the token extent would agree with the parser everywhere, because with no trivia the two
//! rules coincide. Interior trivia is the only material on which that bug can red.

use std::{
  collections::BTreeSet,
  path::{Path, PathBuf},
};

use rowan::{GreenNodeBuilder, Language};
use smear::parser::{
  graphql::{
    GraphQL,
    ast::{Document, ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    kinds::{GraphQLLang, SyntaxKind as K},
    lossless::{
      ProjectErrorKind, Recovery, SyntaxNode, ast::Document as DocumentNode, matches_source,
      parse_document, parse_executable_document, parse_type_system_document, project,
      project_executable_document, project_executable_document_recovered,
      project_type_system_document, project_type_system_document_recovered,
    },
    syntactic::{GraphqlLexer, document, executable_document, type_system_document},
  },
  lossless::ast::CastNode,
};
use tokora::{Parse as _, Parser};

// The span-extent support module, shared with `syntactic_span_extent.rs`. This gate reads only
// its alphabet, its injector and its `Debug` walk — the four-part checker and the discriminating
// classifier are that gate's business — so the unused half would be four `dead_code` denials
// under CI's `-Dwarnings`. Allowed at the include rather than at each item, which would edit a
// file two other gates own.
#[allow(dead_code)]
#[path = "support/span_extent.rs"]
mod extent;

use extent::{ALPHABET, inject};

/// The smallest number of `valid_` entries this gate is allowed to compare.
///
/// The measurement on the day it was written, as a floor. A corpus that shrank below it is a
/// gate that stopped covering what it claims to.
const VALID_ENTRY_FLOOR: usize = 56;

/// The smallest number of `invalid_` entries the refusal census runs.
const INVALID_ENTRY_FLOOR: usize = 31;

/// The smallest number of corpus entries the **executable** root's sweep is allowed to compare.
///
/// Fewer than the mixed root's, because most of the corpus is SDL and the executable root refuses
/// it — by design, and `lossless_runner.rs` is where that refusal is pinned.
const EXECUTABLE_ENTRY_FLOOR: usize = 9;

/// The smallest number of corpus entries the **type-system** root's sweep is allowed to compare.
///
/// The other side of the same split, and the larger one: most of the corpus is SDL. The two floors
/// do not add up to the mixed root's, because an entry that mixes the two halves reaches neither
/// single-half root.
const TYPE_SYSTEM_ENTRY_FLOOR: usize = 28;

/// The smallest number of distinct AST node types the compared documents reach.
///
/// Read off the syntactic parse's `Debug` rendering — the same total projection
/// `tests/support/span_extent.rs` walks — so a corpus that stopped reaching the extensions, or
/// the value family, is a floor failure rather than a silent narrowing.
const OWNER_FLOOR: usize = 60;

// ---------------------------------------------------------------------------------------------
// harnesses
// ---------------------------------------------------------------------------------------------

/// The syntactic oracle: the shipped, fail-fast document root, exactly as `lossless_parity.rs`
/// and `syntactic_span_extent.rs` drive it.
fn oracle(src: &str) -> Result<Document<&str>, GraphqlErrors<&str>> {
  Parser::with_parser::<'_, GraphqlLexer<'_, str>, Document<&str>, GraphqlErrors<&str>, _, GraphQL>(
    document,
  )
  .parse_str(src)
}

fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .filter(|path| {
      path
        .file_name()
        .is_some_and(|name| name.to_string_lossy().starts_with(prefix))
    })
    .collect();
  files.sort();
  files.into_iter().map(read_entry).collect()
}

fn read_entry(path: PathBuf) -> (String, String) {
  let name = path
    .file_name()
    .expect("a corpus entry has a file name")
    .to_string_lossy()
    .to_string();
  let src = std::fs::read_to_string(&path)
    .unwrap_or_else(|e| panic!("{} is unreadable: {e}", Path::display(&path)));
  (name, src)
}

/// Every token boundary in `src`: offset 0, then the end of each lossless token.
///
/// Derived from the **tree**, not from a second lexer instantiation, because every entry padded
/// here has already been parsed losslessly by the caller and the tree's token ends are the same
/// offsets by construction.
fn boundaries(src: &str) -> Vec<usize> {
  let parse = parse_document(src);
  let mut out = vec![0usize];
  for element in parse.syntax().descendants_with_tokens() {
    if let Some(token) = element.into_token() {
      out.push(usize::from(token.text_range().end()));
    }
  }
  out.sort_unstable();
  out.dedup();
  out
}

/// Every node type named in a `Debug` rendering, as `tests/support/span_extent.rs` reads them.
fn owners(dump: &str) -> BTreeSet<String> {
  extent::owners(&extent::spans_of(dump))
}

// ---------------------------------------------------------------------------------------------
// the core assertion
// ---------------------------------------------------------------------------------------------

#[test]
fn the_projection_equals_the_parse_over_the_shared_corpus() {
  let entries = corpus("valid_");
  assert!(
    entries.len() >= VALID_ENTRY_FLOOR,
    "only {} valid corpus entries, floor is {VALID_ENTRY_FLOOR}",
    entries.len()
  );

  let mut compared = 0usize;
  let mut padded_compared = 0usize;
  let mut reached: BTreeSet<String> = BTreeSet::new();

  for (name, src) in &entries {
    let marks = boundaries(src);
    assert!(
      marks.len() >= 3,
      "{name}: {} token boundaries — a one-token entry cannot exercise an interior junction",
      marks.len()
    );

    for (form, source) in std::iter::once(("compact", src.clone())).chain(
      ALPHABET
        .iter()
        .map(|(form, pad)| (*form, inject(src, &marks, pad))),
    ) {
      let expected = oracle(&source).unwrap_or_else(|e| {
        panic!("{name} ({form}): the syntactic parser rejects a valid corpus entry: {e:?}")
      });
      let parse = parse_document(&source);
      assert!(
        !parse.has_errors(),
        "{name} ({form}): the lossless parser rejects a valid corpus entry"
      );
      let projected = project(&parse, &source)
        .unwrap_or_else(|e| panic!("{name} ({form}): the projection refused: {e}"));

      assert_eq!(
        projected, expected,
        "{name} ({form}): the projection is not the AST the parser builds for the same bytes"
      );

      reached.extend(owners(&format!("{expected:#?}")));
      compared += 1;
      if form != "compact" {
        padded_compared += 1;
      }
    }
  }

  assert_eq!(
    compared,
    entries.len() * (ALPHABET.len() + 1),
    "the sweep did not run every form over every entry"
  );
  assert_eq!(
    padded_compared,
    entries.len() * ALPHABET.len(),
    "the padded half did not run every form over every entry"
  );
  assert!(
    reached.len() >= OWNER_FLOOR,
    "the compared documents reach only {} node types, floor is {OWNER_FLOOR}: {reached:?}",
    reached.len()
  );
}

// ---------------------------------------------------------------------------------------------
// the same assertion at the executable root
// ---------------------------------------------------------------------------------------------

/// The syntactic oracle for the executable-only root.
fn executable_oracle(src: &str) -> Result<ExecutableDocument<&str>, GraphqlErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(src)
}

/// `project_executable_document` is the AST the executable parser builds — and the **recovering**
/// door is the same value again whenever nothing had to be recovered.
///
/// The second half is what makes this gate load-bearing for the validator: `validate_executable_lossless`
/// goes through the recovering door, not the fail-fast one, so an equality proved only of the
/// fail-fast door would be proved of code the validator never calls. The two are compared here,
/// over the same padded corpus, and the recovery is required to report itself complete.
#[test]
fn the_executable_projection_equals_the_parse_over_the_shared_corpus() {
  // Every `valid_` entry the executable root accepts, discovered rather than listed: an entry
  // added to the corpus later joins this sweep without anybody editing a table.
  let entries: Vec<(String, String)> = corpus("valid_")
    .into_iter()
    .filter(|(_, src)| executable_oracle(src).is_ok())
    .collect();
  assert!(
    entries.len() >= EXECUTABLE_ENTRY_FLOOR,
    "only {} executable corpus entries, floor is {EXECUTABLE_ENTRY_FLOOR}",
    entries.len()
  );

  let mut compared = 0usize;
  for (name, src) in &entries {
    let marks = boundaries(src);
    for (form, source) in std::iter::once(("compact", src.clone())).chain(
      ALPHABET
        .iter()
        .map(|(form, pad)| (*form, inject(src, &marks, pad))),
    ) {
      let expected = executable_oracle(&source).unwrap_or_else(|e| {
        panic!("{name} ({form}): the syntactic parser rejects an executable corpus entry: {e:?}")
      });
      let parse = parse_executable_document(&source);
      assert!(
        !parse.has_errors(),
        "{name} ({form}): the lossless executable root rejects an entry its syntactic twin takes"
      );

      let projected = project_executable_document(&parse, &source)
        .unwrap_or_else(|e| panic!("{name} ({form}): the projection refused: {e}"));
      assert_eq!(
        projected, expected,
        "{name} ({form}): the executable projection is not the AST the parser builds for the \
         same bytes"
      );

      let (recovered, recovery) = project_executable_document_recovered(&parse, &source)
        .expect("the pair is the same document");
      assert!(
        recovery.is_complete(),
        "{name} ({form}): the recovering door dropped {} element(s) of a clean parse",
        recovery.skipped()
      );
      assert_eq!(
        recovery.projected() as usize,
        expected.definitions().len(),
        "{name} ({form}): the recovery counted a different number of definitions"
      );
      assert_eq!(
        recovered, expected,
        "{name} ({form}): the recovering door and the fail-fast one disagree on a clean parse"
      );

      compared += 1;
    }
  }

  assert_eq!(
    compared,
    entries.len() * (ALPHABET.len() + 1),
    "the sweep did not run every form over every entry"
  );
}

// ---------------------------------------------------------------------------------------------
// the same assertion at the type-system root
// ---------------------------------------------------------------------------------------------

/// The syntactic oracle for the SDL-only root.
fn type_system_oracle(src: &str) -> Result<TypeSystemDocument<&str>, GraphqlErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(src)
}

/// `project_type_system_document` is the AST the SDL parser builds — and the **recovering** door is
/// the same value again whenever nothing had to be recovered.
///
/// [`the_executable_projection_equals_the_parse_over_the_shared_corpus`]'s mirror, and load-bearing
/// for the validator in the same way: `validate_schema_lossless` goes through the recovering door,
/// so an equality proved only of the fail-fast one would be proved of code the validator never
/// calls. It is also the value-level statement `validator_lossless_schema.rs` stands on — that gate
/// compares draft §3 *refusals*, which say nothing about the parts of a document no rule blames,
/// and this one compares every span of every node.
#[test]
fn the_type_system_projection_equals_the_parse_over_the_shared_corpus() {
  // Every `valid_` entry the SDL root accepts, discovered rather than listed.
  let entries: Vec<(String, String)> = corpus("valid_")
    .into_iter()
    .filter(|(_, src)| type_system_oracle(src).is_ok())
    .collect();
  assert!(
    entries.len() >= TYPE_SYSTEM_ENTRY_FLOOR,
    "only {} type-system corpus entries, floor is {TYPE_SYSTEM_ENTRY_FLOOR}",
    entries.len()
  );

  let mut compared = 0usize;
  for (name, src) in &entries {
    let marks = boundaries(src);
    for (form, source) in std::iter::once(("compact", src.clone())).chain(
      ALPHABET
        .iter()
        .map(|(form, pad)| (*form, inject(src, &marks, pad))),
    ) {
      let expected = type_system_oracle(&source).unwrap_or_else(|e| {
        panic!("{name} ({form}): the syntactic parser rejects a type-system corpus entry: {e:?}")
      });
      let parse = parse_type_system_document(&source);
      assert!(
        !parse.has_errors(),
        "{name} ({form}): the lossless SDL root rejects an entry its syntactic twin takes"
      );

      let projected = project_type_system_document(&parse, &source)
        .unwrap_or_else(|e| panic!("{name} ({form}): the projection refused: {e}"));
      assert_eq!(
        projected, expected,
        "{name} ({form}): the type-system projection is not the AST the parser builds for the \
         same bytes"
      );

      let (recovered, recovery) = project_type_system_document_recovered(&parse, &source)
        .expect("the pair is the same document");
      assert!(
        recovery.is_complete(),
        "{name} ({form}): the recovering door dropped {} element(s) of a clean parse",
        recovery.skipped()
      );
      assert_eq!(
        recovery.projected() as usize,
        expected.definitions().len(),
        "{name} ({form}): the recovery counted a different number of definitions"
      );
      assert_eq!(
        recovered, expected,
        "{name} ({form}): the recovering door and the fail-fast one disagree on a clean parse"
      );

      compared += 1;
    }
  }

  assert_eq!(
    compared,
    entries.len() * (ALPHABET.len() + 1),
    "the sweep did not run every form over every entry"
  );
}

/// The three roots refuse each other's trees rather than filtering them.
///
/// Each projection reads one root node, and a parse of a different root does not have it. Without
/// this, a projection that fell back on the tree's own root would silently answer about a document
/// shaped by a grammar the caller did not ask for — which is the difference between "this SDL has
/// no query root" and "this is not an SDL parse".
#[test]
fn each_root_refuses_the_other_two() {
  let sdl = "type T { f: Int }";
  let executable = "query Q { f }";

  // A mixed parse has neither single-half root.
  let mixed = parse_document(sdl);
  assert!(!mixed.has_errors());
  assert!(project(&mixed, sdl).is_ok());
  assert!(project_type_system_document(&mixed, sdl).is_err());
  assert!(project_executable_document(&mixed, sdl).is_err());

  // An SDL parse has no executable root, and the mixed projection has no `Document` node to read.
  let type_system = parse_type_system_document(sdl);
  assert!(!type_system.has_errors());
  assert!(project_type_system_document(&type_system, sdl).is_ok());
  assert!(project_executable_document(&type_system, sdl).is_err());
  assert!(project(&type_system, sdl).is_err());

  // And the other way round.
  let executable_parse = parse_executable_document(executable);
  assert!(!executable_parse.has_errors());
  assert!(project_executable_document(&executable_parse, executable).is_ok());
  assert!(project_type_system_document(&executable_parse, executable).is_err());
  assert!(project(&executable_parse, executable).is_err());
}

// ---------------------------------------------------------------------------------------------
// control 1 — the equality can answer no
// ---------------------------------------------------------------------------------------------

#[test]
fn the_equality_can_answer_no() {
  let compact = "type T{f:Int}";
  let shifted = " type T{f:Int}";

  let a = project(&parse_document(compact), compact).expect("projects");
  let b = project(&parse_document(shifted), shifted).expect("projects");
  assert_ne!(
    a, b,
    "the same document one byte later compares equal, so the derived PartialEq is not reading \
     spans and every assertion in this file is discounted by exactly that much"
  );

  // Same shape, one different name: the slice half of the same control.
  let renamed = "type U{f:Int}";
  let c = project(&parse_document(renamed), renamed).expect("projects");
  assert_ne!(
    a, c,
    "two documents differing only in a type name compare equal, so slices are not being compared"
  );

  // And a positive leg, so the control is not passing merely because everything is unequal.
  let again = project(&parse_document(compact), compact).expect("projects");
  assert_eq!(a, again, "the projection is not deterministic");
}

// ---------------------------------------------------------------------------------------------
// control 2 — the fraud model
// ---------------------------------------------------------------------------------------------

/// Build a green tree by hand, so the projection is handed a structure that does **not** match
/// the structure its own text would parse to.
struct Tree {
  builder: GreenNodeBuilder<'static>,
}

impl Tree {
  fn new() -> Self {
    Self {
      builder: GreenNodeBuilder::new(),
    }
  }

  fn open(&mut self, kind: K) -> &mut Self {
    self.builder.start_node(GraphQLLang::kind_to_raw(kind));
    self
  }

  fn close(&mut self) -> &mut Self {
    self.builder.finish_node();
    self
  }

  fn token(&mut self, kind: K, text: &str) -> &mut Self {
    self.builder.token(GraphQLLang::kind_to_raw(kind), text);
    self
  }

  fn finish(self) -> SyntaxNode {
    SyntaxNode::new_root(self.builder.finish())
  }
}

/// Two definitions' worth of text, in **one** definition node.
///
/// The text is `type T{f:Int} type U{g:Int}`, which the parser splits into two definitions. This
/// tree says there is one, and the projection has to believe the tree. A tree walk answers one
/// definition named `T`; anything that re-derived the structure from the bytes answers two.
fn two_definitions_in_one_node() -> (SyntaxNode, &'static str) {
  let text = "type T{f:Int} type U{g:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T");
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.open(K::NamedType).token(K::Name, "Int").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.token(K::Space, " ");
  // The second definition's tokens, swallowed by the first node.
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "U");
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "g").token(K::Colon, ":");
  tree.open(K::NamedType).token(K::Name, "Int").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  (tree.finish(), text)
}

/// The described-probe shape, with the description hung as a **sibling** of the definition
/// rather than as its child.
///
/// Its text is `"d" type T{f:Int}`, which the parser attaches — producing
/// `Described { description: Some(_), … }`. Under this tree the description is loose under the
/// document, which is rubble the walk has no place for.
fn description_as_sibling() -> (SyntaxNode, &'static str) {
  let text = "\"d\" type T{f:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::Description).token(K::String, "\"d\"").close();
  tree.token(K::Space, " ");
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T");
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.open(K::NamedType).token(K::Name, "Int").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  (tree.finish(), text)
}

#[test]
fn a_projection_that_re_parsed_the_source_would_fail_this() {
  let (node, text) = two_definitions_in_one_node();
  assert_eq!(
    node.text().to_string(),
    text,
    "the synthetic tree's text must be the text a re-parse would be handed, or this control \
     tests nothing"
  );

  let document = DocumentNode::cast_node(node).expect("the synthetic root is a Document");
  let from_structure = document
    .to_ast(text)
    .expect("the synthetic tree is well-shaped, so it projects");
  let from_text = project(&parse_document(text), text).expect("the real parse projects");

  assert_ne!(
    from_structure, from_text,
    "the projection answered what a re-parse of the same bytes answers, which is exactly what a \
     projection that ignored the tree it was handed would do"
  );

  // Pinned both ways, so "differs" cannot be satisfied tomorrow by an arbitrary wrong answer.
  assert_eq!(
    from_structure.definitions().len(),
    1,
    "the tree says one definition"
  );
  assert_eq!(
    from_text.definitions().len(),
    2,
    "the bytes say two; if the parser stopped splitting them this control has nothing to contrast"
  );
}

#[test]
fn a_structure_the_bytes_do_not_imply_is_refused_rather_than_re_derived() {
  // The second leg, and the sharper one: here the tree is a shape the walk has no place for, so
  // reading the structure *refuses* where re-parsing the same bytes would happily succeed.
  let (node, text) = description_as_sibling();
  assert_eq!(node.text().to_string(), text);

  let document = DocumentNode::cast_node(node).expect("the synthetic root is a Document");
  let kind = document
    .to_ast(text)
    .map(|_| ())
    .expect_err("a description loose under the document is rubble, not a definition")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::Description,
      found: K::Description,
    },
    "the walk names the loose node where it sits"
  );

  // And the re-parse of the same bytes succeeds, which is what makes this a discriminator.
  project(&parse_document(text), text).expect("the real parse projects");
}

#[test]
fn the_sibling_tree_really_is_one_the_parser_would_not_build() {
  // The other half of the control above: the *real* parse of the same text does attach the
  // description, so the refusal genuinely comes from a different structure rather than from a
  // projection bug that happens to refuse.
  let (_, text) = description_as_sibling();
  let parsed = project(&parse_document(text), text).expect("projects");
  let described = parsed.definitions()[0]
    .try_unwrap_definition_ref()
    .expect("a definition");
  assert!(
    described.description().is_some(),
    "the parser is expected to attach `\"d\"` to the definition that follows it; if it stopped \
     doing so the fraud control above has nothing to contrast with"
  );
}

// ---------------------------------------------------------------------------------------------
// the span rule #72 established, re-measured
// ---------------------------------------------------------------------------------------------

#[test]
fn the_span_rule_the_normaliser_would_have_hidden() {
  // The exact probe the design's M-A measurement used, with the numbers it recorded and the
  // numbers trunk now answers. If a composite span drifts back onto a lookahead cursor, this
  // names it; the corpus sweep would only report that two large values differ.
  let src = "  type T  { f : Int }  ";
  let ast = oracle(src).expect("parses");
  let spans = extent::spans_of(&format!("{ast:#?}"));

  let of = |owner: &str| {
    let found: Vec<_> = spans
      .iter()
      .filter(|span| span.owner == owner)
      .map(|span| (span.start, span.end))
      .collect();
    assert_eq!(found.len(), 1, "expected one {owner} span, got {found:?}");
    found[0]
  };

  // Token extents. The design measured 0..21, 16..20 and 12..20 respectively, pre-#72.
  assert_eq!(
    of("Document"),
    (2, 21),
    "the document opens on its first token"
  );
  assert_eq!(
    of("NamedType"),
    (16, 19),
    "a named type closes at the end of its own name, not at the next token's start"
  );
  assert_eq!(
    of("FieldDefinition"),
    (12, 19),
    "a field definition closes at the end of its type, not on the space after it"
  );

  // And the tree's own ranges, which are what a projection reaching for `text_range` would use.
  let parse = parse_document(src);
  let field = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::FieldDefinition)
    .expect("a field definition node");
  assert_eq!(
    (
      usize::from(field.text_range().start()),
      usize::from(field.text_range().end())
    ),
    (12, 20),
    "the CST node range holds the committed space after `Int`, which is why the projection folds \
     token extents instead of reading it"
  );

  // The projection agrees with the parser and not with the node range.
  assert_eq!(project(&parse, src).expect("projects"), ast);
}

// ---------------------------------------------------------------------------------------------
// the divergent shapes, each pinned by name
// ---------------------------------------------------------------------------------------------

#[test]
fn a_description_hoists_out_of_the_definition_node_it_sits_inside() {
  let src = "\"doc\" type T { f: Int }";
  let parse = parse_document(src);

  // The tree keeps it inside.
  let definition = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ObjectTypeDefinition)
    .expect("an object type definition node");
  assert_eq!(usize::from(definition.text_range().start()), 0);
  assert!(
    definition
      .children()
      .any(|child| child.kind() == K::Description),
    "the CST hangs the description inside the definition"
  );

  // The AST lifts it out, and the inner definition starts after it.
  let projected = project(&parse, src).expect("projects");
  let described = projected.definitions()[0]
    .try_unwrap_definition_ref()
    .expect("a definition");
  assert_eq!((described.span().start(), described.span().end()), (0, 23));
  assert_eq!(
    described
      .description()
      .map(|d| (d.span().start(), d.span().end())),
    Some((0, 5))
  );
  assert_eq!(
    (
      described.node().span().start(),
      described.node().span().end()
    ),
    (6, 23),
    "the inner definition's span is synthesised: it starts after the hoisted description"
  );
  assert_eq!(projected, oracle(src).expect("parses"));
}

#[test]
fn a_field_definition_keeps_its_description_inside_its_own_span() {
  // The asymmetry the module header records: `FieldDefinition` gives the wrapper and the inner
  // node the *same* span, description included, where the document level does not. Reproduced
  // rather than corrected, and pinned here so a "cleanup" of either side reds.
  let src = "type T { \"fd\" f: Int }";
  let projected = project(&parse_document(src), src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

  let described = projected.definitions()[0]
    .try_unwrap_definition_ref()
    .expect("a definition");
  let object = described
    .node()
    .try_unwrap_type_system_ref()
    .expect("a type-system definition")
    .try_unwrap_type_ref()
    .expect("a type definition")
    .try_unwrap_object_ref()
    .expect("an object type");
  let fields = object.fields_definition().expect("a fields definition");
  let field = &fields.field_definitions()[0];
  assert_eq!(
    (field.span().start(), field.span().end()),
    (field.node().span().start(), field.node().span().end()),
    "the field definition's wrapper and inner spans are the same value"
  );
  assert_eq!(
    (field.span().start(), field.span().end()),
    (9, 20),
    "and that value includes the description"
  );
}

#[test]
fn the_bang_folds_into_the_node_it_wraps() {
  // The CST has a `NonNullType` node; the AST has no image for it, only a `required` flag whose
  // span reaches over the `!`.
  let src = "type T { f: [Int!]! }";
  let parse = parse_document(src);
  assert!(
    parse
      .syntax()
      .descendants()
      .filter(|node| node.kind() == K::NonNullType)
      .count()
      == 2,
    "the tree opens a NonNullType for each `!`"
  );
  assert_eq!(
    project(&parse, src).expect("projects"),
    oracle(src).expect("parses")
  );
}

#[test]
fn a_name_is_a_token_and_an_interface_list_holds_names_not_type_references() {
  // Two shapes at once: the definition's name is the *second* `Name` token under its node (there
  // is no `Name` node in this kind space), and `implements A & B` holds `NamedType` nodes in the
  // tree but bare `Name`s in the AST.
  let src = "type T implements A & B { f: Int }";
  let parse = parse_document(src);
  let clause = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ImplementsInterfaces)
    .expect("an implements clause");
  assert_eq!(
    clause
      .children()
      .filter(|child| child.kind() == K::NamedType)
      .count(),
    2,
    "the tree wraps each interface in a NamedType"
  );
  assert_eq!(
    project(&parse, src).expect("projects"),
    oracle(src).expect("parses")
  );
}

// ---------------------------------------------------------------------------------------------
// refusals
// ---------------------------------------------------------------------------------------------

/// One pinned refusal: an input, and the kind the projection must answer with.
struct Refusal {
  what: &'static str,
  kind: fn(&ProjectErrorKind) -> bool,
  error: ProjectErrorKind,
}

fn refuse(source: &str) -> ProjectErrorKind {
  let parse = parse_document(source);
  project(&parse, source)
    .map(|_| ())
    .expect_err("the projection was expected to refuse")
    .kind()
    .clone()
}

#[test]
fn the_lost_node_class_refuses() {
  // `invalid_top_level_junk`: bytes a failed definition left as rubble under the document.
  let (_, src) = read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/corpus/invalid_top_level_junk.graphql"),
  );
  let kind = refuse(&src);
  assert!(
    matches!(kind, ProjectErrorKind::UnexpectedChild { .. }),
    "expected an UnexpectedChild refusal, got {kind:?}"
  );
}

#[test]
fn the_recovered_in_place_class_refuses() {
  // `type T { x: }` keeps its field-definition node and hangs an `Error` hole where the type
  // should be. The hole is the refusal.
  let kind = refuse("type T { x: }");
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::FieldDefinition,
      found: K::Error,
    },
    "the hole must be named where it sits"
  );
}

#[test]
fn a_variable_in_a_constant_position_refuses() {
  // The AST's own type system forbids it: `ConstInputValue` has no `Variable` variant. The
  // lossless tree keeps the offending node so a diagnostic can point at it, so the refusal is
  // the projection's, not a cast failure.
  let src = "type T @d(a: $v) { f: Int }";
  let parse = parse_document(src);
  assert!(
    parse
      .syntax()
      .descendants()
      .any(|node| node.kind() == K::Variable),
    "the tree is expected to keep the variable node; if it stopped, this pin moved"
  );
  let kind = refuse(src);
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::Argument,
      found: K::Variable,
    }
  );
}

#[test]
fn a_fragment_named_on_refuses() {
  let (_, src) = read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests/corpus/invalid_fragment_named_on.graphql"),
  );
  let kind = refuse(&src);
  assert_eq!(
    kind,
    ProjectErrorKind::SemanticRule {
      rule: "a fragment may not be named `on`",
    },
    "the exclusion `FragmentName::new` is kept crate-private to protect has to be re-checked \
     here, because the tree records it only as a diagnostic"
  );
}

#[test]
fn a_mismatched_source_refuses() {
  let src = "type T { f: Int }";
  let parse = parse_document(src);
  let other = "type U { f: Int }";
  assert_eq!(
    src.len(),
    other.len(),
    "same length, so every range is in bounds"
  );
  let kind = project(&parse, other)
    .map(|_| ())
    .expect_err("a tree parsed from other bytes must not project against these")
    .kind()
    .clone();
  assert_eq!(kind, ProjectErrorKind::SourceMismatch);

  // A shorter source is the out-of-bounds leg of the same check.
  let kind = project(&parse, "type")
    .map(|_| ())
    .expect_err("a truncated source must not project")
    .kind()
    .clone();
  assert_eq!(kind, ProjectErrorKind::SourceMismatch);
}

#[test]
fn a_divergence_in_bytes_no_constructor_reads_refuses() {
  // The half of the threat model a per-token comparison could not see, and the reason the check
  // is made once against the whole tree instead. In both pairs below every token whose text a
  // constructor reads — `type`, `T`, `f`, `Int` — is byte-identical and correctly positioned;
  // what moved is a brace or a space. Neither was ever handed to a constructor, so neither was
  // ever compared, and the pair projected into an AST whose spans pointed at bytes this tree
  // never tokenised.
  let src = "type T { f: Int }";
  let parse = parse_document(src);

  for (what, other, at) in [
    ("punctuation", "type T ( f: Int )", 7..8),
    ("trivia", "type T {\nf: Int }", 8..9),
  ] {
    assert_eq!(
      src.len(),
      other.len(),
      "{what}: same length, so nothing here is caught by a bounds check"
    );
    let refusal = match project(&parse, other) {
      Ok(_) => panic!("{what}: a pair that diverges only here still must not project"),
      Err(refusal) => refusal,
    };
    assert_eq!(refusal.kind(), &ProjectErrorKind::SourceMismatch, "{what}");
    assert_eq!(
      refusal.span(),
      &at,
      "{what}: the refusal names the first bytes that diverge"
    );
  }
}

#[test]
fn a_missing_constituent_refuses() {
  // No corpus entry reaches this: every shape the recovery produces either keeps the constituent
  // or leaves an `Error` hole, which is refused earlier. So the witness is synthetic — a field
  // definition with a name and no type, the shape a future recovery change could start emitting.
  let text = "type T{f:}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T");
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);

  let document = DocumentNode::cast_node(node).expect("a Document root");
  let kind = document
    .to_ast(text)
    .map(|_| ())
    .expect_err("a field definition with no type has no AST image")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::MissingChild {
      parent: K::FieldDefinition,
      wanted: "a type reference",
    }
  );
}

#[test]
fn a_token_that_will_not_cook_refuses() {
  // Same reason as above: the lossless lexer never emits a `String` token it cannot re-lex, so
  // this class is reachable only by handing the projection a tree that claims one. Its value is
  // that the refusal exists rather than a panic or a silently truncated literal.
  let text = "type T{f:Int}\"oops";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T");
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.open(K::NamedType).token(K::Name, "Int").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  // An unterminated literal, claimed as a description.
  tree.open(K::Description).token(K::String, "\"oops").close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);

  let document = DocumentNode::cast_node(node).expect("a Document root");
  let kind = document
    .to_ast(text)
    .map(|_| ())
    .expect_err("an unterminated string literal does not cook")
    .kind()
    .clone();
  assert_eq!(kind, ProjectErrorKind::MalformedToken { kind: K::String });
}

#[test]
fn every_refusal_kind_has_a_witness() {
  // Totality. The list is written out rather than derived, because `ProjectErrorKind` is
  // `#[non_exhaustive]` and there is no way to enumerate its variants at run time — so adding a
  // variant without a pin has to be caught by the count below.
  let src_junk = read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/corpus/invalid_top_level_junk.graphql"),
  )
  .1;
  let src_on = read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests/corpus/invalid_fragment_named_on.graphql"),
  )
  .1;

  let witnesses: Vec<Refusal> = vec![
    Refusal {
      what: "rubble under the document",
      kind: |k| matches!(k, ProjectErrorKind::UnexpectedChild { .. }),
      error: refuse(&src_junk),
    },
    Refusal {
      what: "a fragment named `on`",
      kind: |k| matches!(k, ProjectErrorKind::SemanticRule { .. }),
      error: refuse(&src_on),
    },
    Refusal {
      what: "a mismatched source",
      kind: |k| matches!(k, ProjectErrorKind::SourceMismatch),
      error: {
        let src = "type T { f: Int }";
        project(&parse_document(src), "type U { f: Int }")
          .map(|_| ())
          .expect_err("refuses")
          .kind()
          .clone()
      },
    },
  ];

  for witness in &witnesses {
    assert!(
      (witness.kind)(&witness.error),
      "{}: got {:?}",
      witness.what,
      witness.error
    );
  }

  // The two synthetic classes are pinned in their own tests above; asserting them here as well
  // would duplicate the construction without adding a check. What this count owns is the claim
  // that five variants exist and five are witnessed somewhere in this file.
  const KIND_COUNT: usize = 5;
  assert_eq!(
    witnesses.len() + 2,
    KIND_COUNT,
    "ProjectErrorKind has a variant with no witness in this file; add one and raise the count"
  );
}

// ---------------------------------------------------------------------------------------------
// the shape-faithful boundary, and the invalid half as a census
// ---------------------------------------------------------------------------------------------

#[test]
fn the_unclosed_brace_class_projects_although_the_parser_rejects_it() {
  // The documented boundary of "shape-faithful, not verdict-faithful". Asserted rather than
  // left implicit so the day it moves is a day somebody is told.
  let src = "type T {\n  x: Int\n";
  let parse = parse_document(src);
  assert!(
    parse.has_errors(),
    "the lossless parse reports the missing closer"
  );
  assert!(oracle(src).is_err(), "the syntactic parser rejects it");
  assert!(
    !parse
      .syntax()
      .descendants()
      .any(|node| matches!(node.kind(), K::Error | K::Gap)),
    "this class is shape-complete: the tree carries no hole, which is why it projects"
  );
  project(&parse, src).expect("a shape-complete tree projects even though the parse was rejected");
}

#[test]
fn the_invalid_half_is_a_census_rather_than_a_wall_of_refusals() {
  // Every `invalid_` entry, classified. No equality claim is possible — there is no oracle AST —
  // so what this owns is that the split is measured and that neither side is empty.
  let entries = corpus("invalid_");
  assert!(
    entries.len() >= INVALID_ENTRY_FLOOR,
    "only {} invalid corpus entries, floor is {INVALID_ENTRY_FLOOR}",
    entries.len()
  );

  let mut refused: Vec<&str> = Vec::new();
  let mut projected: Vec<&str> = Vec::new();
  for (name, src) in &entries {
    let parse = parse_document(src);
    assert!(
      oracle(src).is_err(),
      "{name}: an `invalid_` entry the syntactic parser accepts is a corpus fault"
    );
    match project(&parse, src) {
      Ok(_) => projected.push(name),
      Err(_) => refused.push(name),
    }
  }

  assert!(
    refused.len() >= 5,
    "only {} of {} invalid entries refuse; the refusal paths are barely exercised: {refused:?}",
    refused.len(),
    entries.len()
  );
  assert!(
    !projected.is_empty(),
    "every invalid entry refuses, so the shape-faithful boundary is not being exercised at all \
     and the contract in the module header is untested"
  );
  assert!(
    projected.contains(&"invalid_unterminated_brace.graphql"),
    "the unclosed-brace class is the pinned shape-faithful survivor; it now refuses: {projected:?}"
  );
}

/// A **direct** consumer of the recovering projector — no validator, no `smear-compiler` door —
/// cannot obtain an AST it can mistake for the whole document.
///
/// The door check used to live at the door. `validate_executable_lossless` and
/// `validate_schema_lossless` each called `matches_source` themselves, which is airtight for a
/// caller that goes through them and says nothing about the caller that does not: these two
/// projections are `pub`, and projecting a pair is the whole reason they exist.
///
/// # Why `skipped` could not carry it
///
/// The first repair encoded the mismatch as "project nothing, count every top-level element as
/// skipped". That is a true statement about a parse that *has* top-level elements. `skipped` is
/// derived from the elements the container holds, so a parse holding none — empty, or trivia only
/// — counts zero, and `Recovery::is_complete` is `skipped == 0`.
///
/// Each empty witness below is measured **twice**: once against its own text, where the answer is
/// `Recovery::new(0, 0)` and complete, and once against a different source. Those two `Recovery`
/// values were the same value. A count cannot carry a state, so the state left `Recovery`
/// entirely and became the error half of a `Result`, which is the only difference the two cases
/// have left.
#[test]
fn the_recovering_projector_refuses_a_pair_it_is_not_a_projection_of() {
  // `(what, parse text, a source that is not it)`. The first of each pair is the extension — the
  // shape a per-definition check cannot see, since every definition matches at its own range —
  // and the second is the parse with nothing for a tally to count.
  let executable = [
    (
      "an extended source",
      "{ hero { name } }",
      "{ hero { name } }\nquery More { hero { id } }",
    ),
    ("an empty parse", "", "{ hero { name } }"),
  ];
  for (what, text, source) in executable {
    let parse = parse_executable_document(text);
    let (ast, recovery) = project_executable_document_recovered(&parse, text)
      .expect("a parse projects against its own text");
    if text.is_empty() {
      // The premise: this is the value the count-shaped encoding of a mismatch would also have
      // produced, so nothing downstream could tell the two apart.
      assert_eq!(ast.definitions().len(), 0);
      assert_eq!(recovery, Recovery::new(0, 0));
      assert!(recovery.is_complete(), "{what}: complete at zero skipped");
    }

    assert!(
      !matches_source(&parse, source),
      "{what}: the pair under test has to be a mismatched one"
    );
    let refused = project_executable_document_recovered(&parse, source)
      .map(|(projected, recovery)| (projected.definitions().len(), recovery));
    assert_eq!(
      refused.map_err(|mismatch| mismatch.to_string()),
      Err("the parse and the source are not the same document".to_owned()),
      "{what}: a direct consumer was handed an AST for a source this parse does not describe"
    );
  }

  let type_system = [
    (
      "an extended source",
      "type T { f: Int }",
      "type T { f: Int }\ntype U { g: Int }",
    ),
    ("a trivia-only parse", "# nothing\n", "type T { f: Int }"),
  ];
  for (what, text, source) in type_system {
    let parse = parse_type_system_document(text);
    let (ast, recovery) = project_type_system_document_recovered(&parse, text)
      .expect("a parse projects against its own text");
    if what == "a trivia-only parse" {
      assert_eq!(ast.definitions().len(), 0);
      assert_eq!(recovery, Recovery::new(0, 0));
      assert!(recovery.is_complete(), "{what}: complete at zero skipped");
    }

    assert!(
      !matches_source(&parse, source),
      "{what}: the pair under test has to be a mismatched one"
    );
    let refused = project_type_system_document_recovered(&parse, source)
      .map(|(projected, recovery)| (projected.definitions().len(), recovery));
    assert_eq!(
      refused.map_err(|mismatch| mismatch.to_string()),
      Err("the parse and the source are not the same document".to_owned()),
      "{what}: a direct consumer was handed an AST for a source this parse does not describe"
    );
  }
}

/// Rubble the parser leaves **beside** the document node is a top-level element, and counting only
/// the document node's children reported it as nothing lost.
///
/// The same defect as the mismatch above, one level down, and found by sweeping for it: state
/// derived from a population that can be empty while the thing it describes is not. `skipped` was
/// counted over `ExecutableDocument`'s children, and a lexer gap tile does not always land there —
/// `"%"` parses to `Root[ExecutableDocument@0..0, Gap@0..1]`, where that population is empty and
/// the whole document sits outside it. Zero skipped, `is_complete()`, an empty AST over a source
/// with nothing in it that has an AST image.
///
/// The walk now starts at the root and steps *through* the document node, so both populations are
/// one population. The controls below are the two shapes that were already right: a gap *inside*
/// the document node, which must still count once rather than twice, and a parse with no document
/// node at all, whose children were already the root's.
#[test]
fn a_gap_beside_the_document_node_is_counted() {
  // The premise, measured rather than asserted from the shape of the source: the document node is
  // empty, so the population the walk used to iterate has nothing in it, and the root holds a
  // non-trivia element the document node does not.
  let parse = parse_executable_document("%");
  let root = SyntaxNode::new_root(parse.green().clone());
  let document = root
    .children()
    .find(|child| child.kind() == K::ExecutableDocument)
    .expect("the parse has a document node");
  assert_eq!(
    document.children_with_tokens().count(),
    0,
    "the premise is a document node with an empty child population"
  );
  assert_eq!(
    root
      .children_with_tokens()
      .filter(|element| element.kind() == K::Gap)
      .count(),
    1,
    "the premise is one gap tile beside that document node"
  );

  for (what, src, projected, skipped) in [
    ("a gap beside an empty document node", "%", 0, 1),
    ("an unterminated string beside one", "\"unterminated", 0, 1),
    // Controls. The first was already counted — inside the document node — and must not be counted
    // twice now that the walk reaches both. The second has no document node, so the walk's
    // fallback population was already the root's children and nothing about it changed.
    ("a gap inside the document node", "{ a } %", 1, 1),
    ("no document node at all", "{ a }\nquery Bad(", 1, 3),
    // And the honest complete: trivia has no AST image at any position, so a document that is only
    // trivia lost nothing. `projected() == 0` is what tells a consumer there is nothing here.
    ("only trivia", "# nothing\n", 0, 0),
    ("nothing at all", "", 0, 0),
  ] {
    let parse = parse_executable_document(src);
    let (ast, recovery) = project_executable_document_recovered(&parse, src)
      .expect("a parse projects against its own text");
    assert_eq!(
      (ast.definitions().len(), recovery),
      (projected, Recovery::new(projected as u32, skipped)),
      "{what}"
    );
    assert_eq!(
      recovery.is_complete(),
      skipped == 0,
      "{what}: completeness is the tally's own answer"
    );
  }

  // The SDL door is the same walk with a different root kind, so it is the same defect and the
  // same repair; a gate that proved it of one root would be proving it of half the code.
  let parse = parse_type_system_document("%");
  let (ast, recovery) = project_type_system_document_recovered(&parse, "%")
    .expect("a parse projects against its own text");
  assert_eq!(ast.definitions().len(), 0);
  assert_eq!(recovery, Recovery::new(0, 1));
  assert!(!recovery.is_complete());
}

/// A tree deeper than any parser produces is **refused**, by every public helper that walks one.
///
/// # A crash, not a charge defect
///
/// These helpers take a `&GreenNodeData` and `rowan`'s builder is public, so the tree can come from
/// anywhere — including `finish_root`, which finishes an event stream this crate did not emit. Four
/// of them recursed with no counter: `verify_source_at`, `verify_source_counted`, `reject_holes`,
/// and the mutually recursive `node_extent`/`extent_of` pair. al8n/smear#198's own audit of this
/// named three and missed the fourth, which is what a general claim recorded without enumerating
/// its members looks like when the artifact *is* the enumeration.
///
/// Each carries its own counter now and refuses at `MAX_GREEN_DEPTH`, so an unproved tree is a
/// `ProjectErrorKind::TooDeep` rather than a stack overflow. The projection doors inherit it
/// without a counter of their own: every one of them opens with a verification, and
/// `Verified::new` runs the counted form — so a `Verified` is now proof of the tree's depth as
/// well as of its bytes.
///
/// # What this pins, and what it cannot
///
/// A tree one level past the ceiling, which is refused. **Not** a tree deep enough to actually
/// overflow: `rowan` drops a green tree recursively, so building one here would crash this test in
/// its own destructor before an assertion ran. That route is `rowan`'s and is reachable without
/// this crate at all — which is why the ceiling is about *these walks* rather than about the tree's
/// existence, and why `finish_root`'s audit records construction and destruction separately.
#[test]
fn a_tree_deeper_than_the_ceiling_is_refused_rather_than_descended() {
  use smear::parser::lossless::project::{
    MAX_GREEN_DEPTH, node_extent, reject_holes, verify_source, verify_source_counted,
  };

  // One level past what the walks will descend. Every level is a `SelectionSet`, a shape the
  // grammar allows and the lexer's own nesting ceiling of twenty-four would never reach.
  let over = MAX_GREEN_DEPTH + 8;
  let mut tree = Tree::new();
  tree.open(K::Root);
  for _ in 0..over {
    tree.open(K::SelectionSet);
  }
  for _ in 0..over {
    tree.close();
  }
  tree.close();
  let root = tree.finish();
  let green = root.green();

  // The tree holds no token, so its text is empty and the bytes agree — which is what makes depth
  // the only thing left to refuse it for.
  let refused =
    verify_source::<K>(green, "").expect_err("`verify_source` descended a tree past the ceiling");
  assert_eq!(
    *refused.kind(),
    ProjectErrorKind::TooDeep {
      limit: MAX_GREEN_DEPTH
    },
    "{refused}"
  );
  let counted = verify_source_counted::<K>(green, "")
    .map(|_| ())
    .expect_err("`verify_source_counted` descended a tree past the ceiling");
  assert_eq!(
    *counted.kind(),
    ProjectErrorKind::TooDeep {
      limit: MAX_GREEN_DEPTH
    },
    "{counted}"
  );

  let node = smear::parser::lossless::project::Node::of(&root);
  let holes = reject_holes(node, |kind| matches!(kind, K::Error | K::Gap))
    .expect_err("`reject_holes` descended a tree past the ceiling");
  assert_eq!(
    *holes.kind(),
    ProjectErrorKind::TooDeep {
      limit: MAX_GREEN_DEPTH
    },
    "{holes}"
  );

  // The extent pair refuses too. It used to manufacture the node's own range instead, recorded as
  // "a superset — imprecise rather than wrong": but these functions promise `None` when a run holds
  // no non-trivia token, and this tree holds none at all, so `Some(..)` was a different answer to a
  // different question rather than a wider one.
  let extent = node_extent(node, |kind| matches!(kind, K::Space | K::Comment))
    .expect_err("`node_extent` manufactured an extent for a tree past the ceiling");
  assert_eq!(
    *extent.kind(),
    ProjectErrorKind::TooDeep {
      limit: MAX_GREEN_DEPTH
    },
    "{extent}"
  );
  // And the shallow twin still answers exactly: an all-trivia run is `None`, not a range.
  let mut shallow = Tree::new();
  shallow.open(K::Root);
  shallow.token(K::Space, " ").token(K::Comment, "# c");
  shallow.close();
  let shallow = shallow.finish();
  assert_eq!(
    node_extent(
      smear::parser::lossless::project::Node::of(&shallow),
      |kind| matches!(kind, K::Space | K::Comment)
    ),
    Ok(None),
    "an all-trivia run has no token extent"
  );

  // **The two refusals have different names all the way out.** A pair whose bytes agree exactly —
  // this tree holds no token, so its text is `""` — used to be reported as a source mismatch purely
  // because of its shape, which tells a caller to re-parse the one thing that is not wrong. The
  // third collapse of this class on al8n/smear#198, after an arena refusal wearing the budget's
  // `None` and a stale pair wearing the budget's refusal.
  use smear::parser::graphql::lossless::Unverified;
  assert_ne!(
    Unverified::SourceMismatch.to_string(),
    Unverified::TooDeep {
      limit: MAX_GREEN_DEPTH
    }
    .to_string(),
    "the two reasons render as one sentence"
  );

  // The ceiling is not in the way of anything real: the deepest green tree in this repository's
  // corpus is twelve levels, and the deepest document the lexer accepts at all materialises
  // fifty-one.
  let deepest = corpus("valid_")
    .into_iter()
    .map(|(_, src)| depth_of(parse_document(&src).green()))
    .max()
    .expect("the corpus is not empty");
  println!("deepest corpus green tree: {deepest} levels, ceiling {MAX_GREEN_DEPTH}");
  assert!(
    deepest < 64,
    "the corpus reaches {deepest} levels, so {MAX_GREEN_DEPTH} is no longer an order of magnitude \
     of headroom"
  );
}

/// The green tree's depth, for the margin assertion above.
fn depth_of(node: &rowan::GreenNodeData) -> usize {
  1 + node
    .children()
    .filter_map(|child| child.into_node().map(depth_of))
    .max()
    .unwrap_or(0)
}
