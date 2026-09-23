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
  collections::{BTreeMap, BTreeSet},
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
      ProjectErrorKind, Recovery, SyntaxNode, Unverified, Verified, ast::Document as DocumentNode,
      ast::ExecutableDocument as ExecutableDocumentNode,
      ast::TypeSystemDocument as TypeSystemDocumentNode, parse_document, parse_executable_document,
      parse_type_system_document, project, project_executable_document,
      project_executable_document_recovered, project_executable_document_verified,
      project_type_system_document, project_type_system_document_recovered,
      project_type_system_document_verified, verify_parse,
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

  /// `NamedType > Name`.
  fn named_type(&mut self, name: &str) -> &mut Self {
    self.open(K::NamedType).token(K::Name, name).close()
  }

  /// `FieldDefinition` for `name: Type`.
  fn field(&mut self, name: &str, ty: &str) -> &mut Self {
    self.open(K::FieldDefinition);
    self.token(K::Name, name).token(K::Colon, ":");
    self.named_type(ty);
    self.close()
  }

  /// `FieldsDefinition` holding one field.
  fn fields(&mut self, name: &str, ty: &str) -> &mut Self {
    self.open(K::FieldsDefinition);
    self.token(K::LBrace, "{");
    self.field(name, ty);
    self.token(K::RBrace, "}");
    self.close()
  }

  /// `Directives > Directive > [@ d Arguments > Argument > <value>]`, over one argument whose value
  /// node the caller opens. The smallest shape that puts a value in a document.
  fn one_argument(&mut self, name: &str, value: impl FnOnce(&mut Self)) -> &mut Self {
    self.open(K::Directives);
    self.open(K::Directive);
    self.token(K::At, "@").token(K::Name, "d");
    self.open(K::Arguments);
    self.token(K::LParen, "(");
    self.open(K::Argument);
    self.token(K::Name, name).token(K::Colon, ":");
    value(self);
    self.close();
    self.token(K::RParen, ")");
    self.close();
    self.close();
    self.close()
  }

  fn finish(self) -> SyntaxNode {
    SyntaxNode::new_root(self.builder.finish())
  }
}

/// One selection moved **out** of the set that holds it in the text.
///
/// The text is `{a{b c}`, which the parser reads as one field `a` selecting `b` and `c`, with the
/// outer set's `}` missing. This tree groups it differently: `a`'s set holds only `b` — its `}` is
/// the lenient one now — and `c` is a second top-level selection, whose set the one `}` closes.
/// Every token is consumed by the production of the node that holds it, so the tree is
/// well-shaped, and the projection has to believe it: a walk answers two top-level selections, and
/// anything that re-derived the structure from the bytes answers one.
///
/// **Why this shape and not the one it replaces.** Until al8n/smear#218 this was one
/// `ObjectTypeDefinition` holding a second definition's `type U{g:Int}`, which projected because
/// the slot walk folded any token and dropped a second `FieldsDefinition` behind its `is_none()`
/// guard — the witness was the hatch the issue closed. A stray `type` is refused where it stands
/// now, so the control diverges from the text through a grouping the productions *do* spell, and
/// the lenient closer is one. GraphQLx's control made the same move in al8n/smear#58's sixth round.
fn two_selections_for_one() -> (SyntaxNode, &'static str) {
  let text = "{a{b c}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field);
  tree.token(K::Name, "a");
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field).token(K::Name, "b").close();
  tree.token(K::Space, " ");
  tree.close();
  tree.close();
  tree.open(K::Field).token(K::Name, "c").close();
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
  let (node, text) = two_selections_for_one();
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
    top_level_selections(&from_structure),
    2,
    "the tree says two top-level selections"
  );
  assert_eq!(
    top_level_selections(&from_text),
    1,
    "the bytes say one; if the parser stopped nesting `c` under `a` this control has nothing to \
     contrast"
  );
}

/// How many selections the document's one shorthand operation holds at its top level, read off the
/// value itself.
fn top_level_selections(document: &Document<&str>) -> usize {
  let definition = document.definitions()[0]
    .try_unwrap_definition_ref()
    .expect("a definition");
  let operation = definition
    .node()
    .try_unwrap_executable_ref()
    .expect("an executable definition")
    .try_unwrap_operation_ref()
    .expect("an operation");
  operation
    .try_unwrap_shorthand_ref()
    .expect("the shorthand")
    .selections()
    .len()
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
  // The parent is the document, because that is where the node sits. The slot walk this file had
  // until al8n/smear#218 handed every child node of the document to the definition dispatch, whose
  // unknown-kind arm named the *child* as its own parent — `{ Description, Description }` — which
  // pointed at the right bytes and said nothing true about the shape. The transcribed document
  // walk reads `Definition+` and `end` refuses the stranger under the node that holds it.
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::Document,
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

/// A gap tile is a **token**, and the substrate's hole scan tested node kinds only.
///
/// Added by al8n/smear#58's GraphQLx round, and it is this dialect's witness for a substrate fix
/// rather than a change to anything in `graphql/lossless/project.rs`. `reject_holes` discarded
/// every token, so `scan_holes` declared a tree free of holes while it held one — both dialects
/// pass `Error | Gap` to that walker and both spell `Gap` as a token image, and this file's
/// `scan_holes` carried a comment saying the arm was dead as written. It was, and what it was dead
/// about was not a shape the parser has yet to produce but the one it produces today: the walk then
/// folded the gap's bytes into the enclosing node's extent as an ordinary non-trivia token and the
/// door answered `Ok`.
///
/// The walker is asked of every element now, so the refusal names the gap's parent and its exact
/// range. `smear/tests/lossless_x_project.rs` carries the same witness over the other dialect.
#[test]
fn a_gap_token_is_a_hole_the_scan_sees() {
  for (what, src, parent, at) in [
    (
      "beside a complete definition",
      "type T { f: Int } %",
      K::Document,
      18..19,
    ),
    (
      "inside a fields block",
      "type T { % f: Int }",
      K::FieldsDefinition,
      9..10,
    ),
  ] {
    let parse = parse_document(src);
    assert_eq!(
      parse
        .syntax()
        .descendants_with_tokens()
        .filter(|element| element.kind() == K::Gap)
        .count(),
      1,
      "{what}: the premise is one gap tile; if the lexer stopped tiling here this pin moved"
    );
    assert!(
      !parse.syntax().descendants().any(|n| n.kind() == K::Error),
      "{what}: an Error node would refuse this tree for the other reason and the gap would go \
       unmeasured"
    );

    let refusal = project(&parse, src)
      .map(|_| ())
      .expect_err("a gap is a region with no AST image");
    assert_eq!(
      refusal.kind(),
      &ProjectErrorKind::UnexpectedChild {
        parent,
        found: K::Gap,
      },
      "{what}"
    );
    assert_eq!(refusal.span(), &at, "{what}: the gap's own range");
  }
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
  //
  // The description is where a definition's production puts one — **first**. Until
  // al8n/smear#218 this tree hung it after the fields block, and the slot walk read a
  // `Description` wherever it sat; the transcription refuses a description anywhere but in front
  // (`UnexpectedChild`), which would be a different refusal wearing this cell's name.
  let text = "\"oops type T{f:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  // An unterminated literal, claimed as a description.
  tree.open(K::Description).token(K::String, "\"oops").close();
  tree.token(K::Space, " ");
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T");
  tree.fields("f", "Int");
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

  // Five classes are pinned in their own cells rather than here: `MissingChild`, `MalformedToken`,
  // `TooDeep` (`a_tree_deeper_than_the_ceiling_is_refused_rather_than_descended`),
  // `InvalidRawKind` (`a_raw_kind_outside_the_space_refuses_rather_than_panicking`), and
  // `WrongRoot` (`a_parse_minted_over_a_wrong_root_is_refused_not_reported_complete`). This count
  // used to read five and omit `TooDeep`, which had a cell all along; it owns the claim that eight
  // variants exist and eight are witnessed somewhere in this file.
  const KIND_COUNT: usize = 8;
  assert_eq!(
    witnesses.len() + 5,
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
/// `validate_schema_lossless` each called `verify_parse` themselves, which is airtight for a
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

    assert_eq!(
      verify_parse(&parse, source),
      Err(Unverified::SourceMismatch),
      "{what}: the pair under test has to be a mismatched one, and mismatched for its BYTES — a \
       shape refusal here would be a different test wearing this one's name"
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

    assert_eq!(
      verify_parse(&parse, source),
      Err(Unverified::SourceMismatch),
      "{what}: the pair under test has to be a mismatched one, and mismatched for its BYTES — a \
       shape refusal here would be a different test wearing this one's name"
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
/// Each then carried its own counter and refused at `MAX_GREEN_DEPTH` — which is what this cell
/// pins — and **the counter was not what made them safe**. A counter bounds a depth; the frames
/// belonged to the host and the stack to whichever thread the caller walked on, so a walk on a
/// thread too small to hold the ceiling aborted before reaching the refusal. Measured out of suite,
/// one child process per depth, the tree built on one thread and the walk run on another: 726
/// levels of `node_extent` and 927 of `reject_holes` on 512 KiB, 566 and 530 of the two
/// verifications on 256 KiB. None of the four spends a native frame per level now, so the refusal
/// below is reached on any stack.
///
/// The projection doors inherit the ceiling without a counter of their own: every one of them opens
/// with a verification, and `Verified::new` runs the counted form — so a `Verified` is proof of the
/// tree's depth as well as of its bytes. What that ceiling stands in front of is the projection's
/// own node dispatch, and the four cycles it used to recurse through are worklists too
/// (al8n/smear#201) — so what the inheritance now bounds is how many entries they hold, not
/// whether a document the doors produce can be projected at all. `smear-parser`'s
/// `deep_projection.rs` reads that flatness off a real projection.
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
  assert_ne!(
    Unverified::SourceMismatch.to_string(),
    Unverified::TooDeep {
      limit: MAX_GREEN_DEPTH
    }
    .to_string(),
    "the two reasons render as one sentence"
  );

  // The ceiling is not in the way of anything real: the deepest green tree in this repository's
  // corpus is twelve levels. What the *doors* can produce is a different and much larger
  // population — `HARD_MAX` brackets, not `MAX_NESTING_DEPTH`, reaching 516 levels — and reading
  // the second off the first is what opened the window `MAX_GREEN_DEPTH`'s header now records.
  // That obligation is a `const` assertion beside the constant; this cell is about the corpus.
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

/// The refusal is reached on a stack that cannot hold the ceiling in native frames.
///
/// # There is no red side in this file either
///
/// A stack overflow is `SIGABRT`, which takes the harness with it and which no `#[should_panic]`
/// sees, so what this can pin is the green side: the four walks answer `TooDeep` on a thread far
/// too small to have held 1 024 frames of any of them. A regression takes the whole file down
/// loudly rather than passing quietly, which is the arrangement `ast_release.rs` uses for the same
/// reason.
///
/// # Why the fixture picks its own stack, and which one
///
/// Because the boundary is a property of the stack and libtest's is not this file's to know. At
/// `8b73965`, measured out of suite one child process per depth with the tree built on another
/// thread, these walks reached **2 MiB** without dying — the ceiling refused first — so a fixture
/// run on libtest's own thread would have passed before the repair and proved nothing. On 512 KiB
/// `node_extent` aborted at 726 levels and `reject_holes` at 927; on the **128 KiB** this test
/// spawns, all four die an order of magnitude below the ceiling. Sizing the stack is what makes the
/// fixture decisive instead of a bet on the runner, and it costs nothing, since a walk that does
/// not recurse needs no more of it at 1 032 levels than at one.
#[test]
fn the_ceiling_is_reached_on_a_stack_too_small_to_hold_it() {
  use smear::parser::lossless::project::{
    MAX_GREEN_DEPTH, Node, node_extent, reject_holes, verify_source, verify_source_counted,
  };

  /// An order of magnitude under every boundary the four walks were measured at on this stack.
  const STACK: usize = 128 * 1024;

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

  // The tree is built here and walked there, which is the shape the defect needs: nothing ties a
  // `Parse` to the thread that produced it, and only the walking thread's stack is at stake.
  let green: rowan::GreenNode = root.green().to_owned();
  std::thread::Builder::new()
    .stack_size(STACK)
    .spawn(move || {
      let too_deep = ProjectErrorKind::TooDeep {
        limit: MAX_GREEN_DEPTH,
      };
      let node = Node::<GraphQLLang>::new(&green, rowan::TextSize::new(0));
      assert_eq!(
        *verify_source::<K>(&green, "")
          .expect_err("`verify_source` descended a tree past the ceiling")
          .kind(),
        too_deep
      );
      assert_eq!(
        *verify_source_counted::<K>(&green, "")
          .map(|_| ())
          .expect_err("`verify_source_counted` descended a tree past the ceiling")
          .kind(),
        too_deep
      );
      assert_eq!(
        *reject_holes(node, |kind| matches!(kind, K::Error | K::Gap))
          .expect_err("`reject_holes` descended a tree past the ceiling")
          .kind(),
        too_deep
      );
      assert_eq!(
        *node_extent(node, |kind| matches!(kind, K::Space | K::Comment))
          .expect_err("`node_extent` manufactured an extent for a tree past the ceiling")
          .kind(),
        too_deep
      );
    })
    .expect("a fixture thread")
    .join()
    .expect("the fixture thread returned");
}

/// The green tree's depth, for the margin assertion above.
fn depth_of(node: &rowan::GreenNodeData) -> usize {
  1 + node
    .children()
    .filter_map(|child| child.into_node().map(depth_of))
    .max()
    .unwrap_or(0)
}

// ---------------------------------------------------------------------------------------------
// al8n/smear#217 and #218: the GraphQLx projection's form, site for site
// ---------------------------------------------------------------------------------------------

/// The refusal a caller-minted tree draws, by kind.
///
/// The pairs below are built rather than parsed, which is the only way to put a shape in front of
/// the projection that no production makes — and [`Verified`] exists precisely so one can be.
fn refuse_tree(node: SyntaxNode, text: &str) -> ProjectErrorKind {
  DocumentNode::cast_node(node)
    .expect("the synthetic root is a Document")
    .to_ast(text)
    .map(|_| ())
    .expect_err("the projection was expected to refuse this tree")
    .kind()
    .clone()
}

/// The fail-fast answer for `src` under the mixed root, spelled for a table.
fn refusal_of(src: &str) -> String {
  match project(&parse_document(src), src) {
    Ok(_) => "Ok".to_string(),
    Err(error) => answer(error.kind()),
  }
}

/// `scalar S@d(n:<literal>)`, with the literal claimed as `kind` under a `node` value node.
fn scalar_with_literal(node: K, token: K, literal: &str) -> (SyntaxNode, String) {
  let text = std::format!("scalar S@d(n:{literal})");
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "S");
  tree.one_argument("n", |tree| {
    tree.open(node).token(token, literal).close();
  });
  tree.close();
  tree.close();
  (tree.finish(), text)
}

/// **al8n/smear#217.** A written-down empty argument list is `None`, with its span still covered.
///
/// # Before
///
/// `optional_arguments` answered `Some(Arguments { arguments: [] })` whenever the node existed, and
/// the lossless production opens one for a written-down `()` — so `project(&parse, src) !=
/// document(src)` over `query Q { f() }` and `type T @d() { f: Int }`, both of which both parsers
/// accept. The corpus held no `()`, which is why the differential sweep never saw it.
///
/// # After
///
/// Row two of the container table: `None`, and the field's or directive's span still reaches over
/// the parentheses — the value the syntactic parser builds, measured here against it rather than
/// asserted.
#[test]
fn a_written_down_empty_argument_list_is_none_with_a_span() {
  let src = "query Q { f() }";
  let parse = parse_document(src);
  assert!(!parse.has_errors());
  assert!(
    parse
      .syntax()
      .descendants()
      .any(|node| node.kind() == K::Arguments),
    "the tree opens an Arguments node for the written-down `()`"
  );
  let expected = oracle(src).expect("parses");
  let projected = project(&parse, src).expect("projects");
  assert_eq!(projected, expected);

  let described = projected.definitions()[0]
    .try_unwrap_definition_ref()
    .expect("a definition");
  let operation = described
    .node()
    .try_unwrap_executable_ref()
    .expect("an executable definition")
    .try_unwrap_operation_ref()
    .expect("an operation")
    .try_unwrap_named_ref()
    .expect("a named operation");
  let field = operation.selection_set().selections()[0]
    .try_unwrap_field_ref()
    .expect("a field");
  assert!(
    field.arguments().is_none(),
    "the syntactic parser answers `None` for `()`, and so does the projection"
  );
  assert_eq!(
    (field.span().start(), field.span().end()),
    (10, 13),
    "and the field's span still covers the parentheses"
  );

  // Both flavours, all three positions, and the three empty containers that are values rather than
  // optional constituents — none of them is in the corpus.
  for what in [
    "type T @d() { f: Int }",
    "query Q { f @d() }",
    "query Q @d() { f }",
    "type T { f(a: Int = []): Int }",
    "type T @d(a: {}) { f: Int }",
    "{ f(a: [], b: {}) }",
  ] {
    let parse = parse_document(what);
    assert!(!parse.has_errors(), "{what}: the lossless parse rejects it");
    let expected = oracle(what).unwrap_or_else(|e| panic!("{what}: the parser rejects it: {e:?}"));
    assert_eq!(
      project(&parse, what).unwrap_or_else(|e| panic!("{what}: the projection refused: {e}")),
      expected,
      "{what}: a written-down empty list projects to a different value from the parser's"
    );
  }
}

/// **al8n/smear#218, finding 1.** A present-but-empty `+` container refuses.
///
/// # Before
///
/// Seven shapes — each parsing losslessly with a diagnostic, the syntactic parser rejecting, the
/// tree carrying no `Error` child — projected `Ok` to an AST value the syntactic parser can never
/// produce: a `SelectionSet` with no selections, a `FieldsDefinition` with no fields.
///
/// # After
///
/// Row one of the container table: `MissingChild { parent, wanted }`.
#[test]
fn a_present_but_empty_required_container_refuses() {
  for (src, parent, wanted) in [
    ("query Q { }", K::SelectionSet, "a selection"),
    ("type T { }", K::FieldsDefinition, "a field definition"),
    (
      "type T { f(): Int }",
      K::ArgumentsDefinition,
      "an argument definition",
    ),
    (
      "input I { }",
      K::InputFieldsDefinition,
      "an input field definition",
    ),
    (
      "enum E { }",
      K::EnumValuesDefinition,
      "an enum value definition",
    ),
    (
      "schema { }",
      K::RootOperationTypeDefinitions,
      "a root operation type",
    ),
    (
      "query Q() { f }",
      K::VariablesDefinition,
      "a variable definition",
    ),
    (
      "extend type T { }",
      K::FieldsDefinition,
      "a field definition",
    ),
  ] {
    let parse = parse_document(src);
    // The premise, measured rather than asserted from the shape of the source: gate 1 is intact —
    // the lossless parser reports and the syntactic parser rejects — and the tree is
    // shape-complete, which is exactly why the hole scan cannot see this class.
    assert!(parse.has_errors(), "{src}: the lossless parser accepts it");
    assert!(
      oracle(src).is_err(),
      "{src}: the syntactic parser accepts it"
    );
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: the tree carries a hole, so this proves nothing about the cardinality rule"
    );
    assert!(
      parse
        .syntax()
        .descendants()
        .any(|node| node.kind() == parent),
      "{src}: the tree does not even hold a {parent:?}"
    );
    assert_eq!(
      refuse(src),
      ProjectErrorKind::MissingChild { parent, wanted },
      "{src}"
    );
  }
}

/// `query Q{f}` with the field carrying a `Directives` node and whatever `inside` writes into it.
fn field_with_directives(
  inside: impl FnOnce(&mut Tree),
  text: &'static str,
) -> (SyntaxNode, &'static str) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::OperationType).token(K::Name, "query").close();
  tree.token(K::Space, " ").token(K::Name, "Q");
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field);
  tree.token(K::Name, "f");
  tree.open(K::Directives);
  inside(&mut tree);
  tree.close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  (tree.finish(), text)
}

/// **al8n/smear#218's worse form of finding 1.** A directive run with no directive refuses.
///
/// `optional_directives` ended `Ok(Some(Directives::new(..)))` unconditionally, so a run holding a
/// stray `@` and no `Directive` projected to `Some(Directives { directives: [] })` — an empty
/// carrier the parser produces for no input. `Directive+` is row one of the container table.
#[test]
fn a_present_directive_run_with_no_directive_refuses() {
  let (node, text) = field_with_directives(|_| {}, "query Q{f}");
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::MissingChild {
      parent: K::Directives,
      wanted: "a directive",
    },
    "a present run with no directive is the refusal, not an empty carrier"
  );

  // The const twin, through an SDL definition's own run.
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "S");
  tree.open(K::Directives).close();
  tree.close();
  tree.close();
  assert_eq!(
    refuse_tree(tree.finish(), "scalar S"),
    ProjectErrorKind::MissingChild {
      parent: K::Directives,
      wanted: "a directive",
    },
    "both flavours, or the fix is half a fix"
  );

  // The witness as it was reported: a run holding a stray `@` and no directive. The token refusal
  // fires first — a `Directives` node spells no tokens of its own.
  let (node, text) = field_with_directives(
    |tree| {
      tree.token(K::At, "@");
    },
    "query Q{f@}",
  );
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::Directives,
      found: K::At,
    },
    "the stray `@` is refused where it stands"
  );
}

/// **al8n/smear#218, finding 2.** The three typed `to_ast` doors scan their own subtree for holes.
///
/// # Before
///
/// They ran `open_node` and no hole scan, on the claim that the walk would reach any hole inside
/// the subtree — and the walk's permissive arms routed an unknown child into the unread extent, so
/// a recovered subtree with an `Error` child after its valid halves projected `Ok`.
///
/// # After
///
/// `open_node` then a subtree-scoped, token-aware `scan_holes` in all three. The hole below sits
/// after a list type's element, where the old `open_list_element` had a wildcard arm.
#[test]
fn a_typed_door_scans_its_own_subtree_for_holes() {
  let text = "type T{f:[Int junk]}";
  let build = |tree: &mut Tree| {
    tree.open(K::ObjectTypeDefinition);
    tree.token(K::Name, "type").token(K::Space, " ");
    tree.token(K::Name, "T");
    tree.open(K::FieldsDefinition);
    tree.token(K::LBrace, "{");
    tree.open(K::FieldDefinition);
    tree.token(K::Name, "f").token(K::Colon, ":");
    tree.open(K::ListType);
    tree.token(K::LBracket, "[");
    tree.named_type("Int");
    tree.token(K::Space, " ");
    tree.open(K::Error).token(K::Name, "junk").close();
    tree.token(K::RBracket, "]");
    tree.close();
    tree.close();
    tree.token(K::RBrace, "}");
    tree.close();
    tree.close();
  };
  let hole = ProjectErrorKind::UnexpectedChild {
    parent: K::ListType,
    found: K::Error,
  };

  let mut tree = Tree::new();
  tree.open(K::Document);
  build(&mut tree);
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(refuse_tree(node, text), hole, "the mixed door");

  let mut tree = Tree::new();
  tree.open(K::TypeSystemDocument);
  build(&mut tree);
  tree.close();
  let node = TypeSystemDocumentNode::cast_node(tree.finish()).expect("a TypeSystemDocument root");
  assert_eq!(
    node.to_ast(text).map(|_| ()).expect_err("refuses").kind(),
    &hole,
    "the type-system door"
  );

  let text = "{f(a:[1 junk])}";
  let mut tree = Tree::new();
  tree.open(K::ExecutableDocument);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field);
  tree.token(K::Name, "f");
  tree.open(K::Arguments);
  tree.token(K::LParen, "(");
  tree.open(K::Argument);
  tree.token(K::Name, "a").token(K::Colon, ":");
  tree.open(K::ListValue);
  tree.token(K::LBracket, "[");
  tree.open(K::IntValue).token(K::Int, "1").close();
  tree.token(K::Space, " ");
  tree.open(K::Error).token(K::Name, "junk").close();
  tree.token(K::RBracket, "]");
  tree.close();
  tree.close();
  tree.token(K::RParen, ")");
  tree.close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = ExecutableDocumentNode::cast_node(tree.finish()).expect("an ExecutableDocument root");
  assert_eq!(
    node.to_ast(text).map(|_| ()).expect_err("refuses").kind(),
    &ProjectErrorKind::UnexpectedChild {
      parent: K::ListValue,
      found: K::Error,
    },
    "the executable door"
  );
}

/// **al8n/smear#218, finding 3.** The root's shape is asserted rather than searched.
///
/// # Before
///
/// Each fail-fast door selected the first child of the wanted kind, and answered
/// `MissingChild { Root }` when there was none. A root holding a valid document **and** a sibling
/// would verify byte for byte and project to an AST omitting the sibling.
///
/// # After
///
/// `sole_document` requires one container of the wanted kind plus trivia, and refuses anything
/// else with `UnexpectedChild { parent: Root, found }`.
///
/// # What this cell can reach, measured rather than assumed
///
/// #218 recorded that over the whole corpus the parser's root holds exactly one container and no
/// tokens, so that the check is preventive. **At this dialect's executable root that is not so**,
/// and the cell measures both halves: the executable root's production abandons the document node
/// when a definition fails — `executable_document` returns the turn's error rather than resyncing —
/// so the lost-node class leaves rubble *beside* no container at all. Over the corpus every such
/// root also carries a hole, which the preflight refuses first; but a hole-free one exists, and the
/// door's answer for it moved from `MissingChild { Root }` to `UnexpectedChild { Root, found }` at
/// the first stranger. Both refuse. A root holding a container **and** a sibling, the shape the
/// check exists for, is reached by no parse this cell has found: it stays preventive against a
/// caller of `finish_root`, and removing it reds nothing here.
#[test]
fn the_root_shape_is_asserted_rather_than_searched() {
  let mut one_container = 0usize;
  let mut rubble_with_a_hole = 0usize;
  for (name, src) in corpus("valid_") {
    for (what, root, kind) in [
      ("mixed", parse_document(&src).syntax(), K::Document),
      (
        "executable",
        parse_executable_document(&src).syntax(),
        K::ExecutableDocument,
      ),
      (
        "type system",
        parse_type_system_document(&src).syntax(),
        K::TypeSystemDocument,
      ),
    ] {
      let children: Vec<K> = root.children_with_tokens().map(|e| e.kind()).collect();
      if children == vec![kind] {
        one_container += 1;
        continue;
      }
      assert!(
        root
          .descendants_with_tokens()
          .any(|element| matches!(element.kind(), K::Error | K::Gap)),
        "{name} ({what}): a hole-free root holding {children:?}"
      );
      assert!(
        !children.contains(&kind),
        "{name} ({what}): a container **and** a sibling, the shape `sole_document` was written for"
      );
      rubble_with_a_hole += 1;
    }
  }
  println!("ROOTS one_container={one_container} rubble_with_a_hole={rubble_with_a_hole}");
  assert_eq!(
    (one_container, rubble_with_a_hole),
    (ROOTS_WITH_ONE_CONTAINER, ROOTS_OF_RUBBLE),
    "the corpus's root shapes moved"
  );

  // The hole-free root with no container, and the answer it now gets.
  let src = "query Q($a: Int";
  let parse = parse_executable_document(src);
  let root = parse.syntax();
  assert!(
    !root
      .descendants_with_tokens()
      .any(|element| matches!(element.kind(), K::Error | K::Gap)),
    "the premise is a hole-free root"
  );
  assert!(
    !root
      .children()
      .any(|child| child.kind() == K::ExecutableDocument),
    "and no container under it"
  );
  assert_eq!(
    project_executable_document(&parse, src)
      .map(|_| ())
      .expect_err("rubble is not a document")
      .kind(),
    &ProjectErrorKind::UnexpectedChild {
      parent: K::Root,
      found: K::OperationType,
    }
  );

  // The positive half of the check itself, at all three doors.
  let sdl = "  # a comment\n  type T { f: Int }  \n";
  assert_eq!(
    project(&parse_document(sdl), sdl).expect("projects"),
    oracle(sdl).expect("parses")
  );
  let executable = "  query Q { f }  ";
  assert!(project_executable_document(&parse_executable_document(executable), executable).is_ok());
  assert!(project_type_system_document(&parse_type_system_document(sdl), sdl).is_ok());
}

/// `(entry, root)` pairs over the valid corpus whose root is exactly one container.
const ROOTS_WITH_ONE_CONTAINER: usize = 165;

/// The rest: every one of them carries a hole and holds no container.
const ROOTS_OF_RUBBLE: usize = 3;

/// **al8n/smear#218, finding 4.** A type condition whose type precedes its `on` refuses rather
/// than panicking.
///
/// # Before
///
/// `TypeCondition::new(SimpleSpan::new(on.start, name.end), …)`, with the `on` found by counting
/// `Name` tokens and the type found by kind — so a caller-built tree with the type first made the
/// span's constructor panic, in `fragment_definition` and in `open_inline_fragment` both. A safe
/// public door that answers `ProjectError` for every other malformed tree aborted for this one.
///
/// # After
///
/// Both walks consume the `on` before the type in their own sequence, so the order is the cursor's
/// and the misplaced `on` is refused where it stands.
#[test]
fn a_type_condition_whose_type_precedes_its_on_refuses_rather_than_panicking() {
  let text = "fragment F T on{f}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::FragmentDefinition);
  tree.token(K::Name, "fragment").token(K::Space, " ");
  tree.token(K::Name, "F").token(K::Space, " ");
  tree.named_type("T");
  tree.token(K::Space, " ").token(K::Name, "on");
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field).token(K::Name, "f").close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  // `to_ast` is a safe public entry point: the answer has to be a value, and an unwinding panic
  // here would take the harness with it rather than being caught by this assertion.
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::FragmentDefinition,
      found: K::Name,
    },
    "the fragment definition's site"
  );

  let text = "{... T on{f}}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::InlineFragment);
  tree.token(K::Spread, "...").token(K::Space, " ");
  tree.named_type("T");
  tree.token(K::Space, " ").token(K::Name, "on");
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field).token(K::Name, "f").close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::InlineFragment,
      found: K::NamedType,
    },
    "the inline fragment's site: with no `on` in front of it the type is not in the sequence"
  );
}

/// **al8n/smear#218, finding 5.** The recovering doors verify the pair once, at the door.
///
/// # Before
///
/// Both recovering paths establish the pair over the whole root, and each entry callback then
/// called `open_node` and re-compared the entry's own bytes — `O(source)` per entry over bytes a
/// single pass had already compared.
///
/// # After
///
/// The entry callbacks do the subtree hole scan and the projection. Nothing observable changed,
/// which is the claim: a mismatched pair is refused **before** any entry is projected, and the
/// verified door and the fallible one still agree entry for entry.
#[test]
fn the_recovering_door_verifies_the_pair_once_at_the_door() {
  let executable = "{ hero { name } }\nquery Q { hero { id } }";
  let parse = parse_executable_document(executable);
  assert!(!parse.has_errors());
  assert_eq!(
    project_executable_document_recovered(&parse, "{ hero { name } }")
      .map(|(ast, _)| ast.definitions().len())
      .map_err(|e| e.to_string()),
    Err("the parse and the source are not the same document".to_owned()),
    "a mismatched pair has to be refused at the door, not counted as skipped entries"
  );
  let (fallible, fallible_recovery) =
    project_executable_document_recovered(&parse, executable).expect("the pair matches");
  let pair = Verified::new(&parse, executable).expect("the pair matches");
  let (verified, verified_recovery) = project_executable_document_verified(pair);
  assert_eq!(fallible, verified);
  assert_eq!(fallible_recovery, verified_recovery);
  assert_eq!(fallible_recovery.projected(), 2);
  assert!(fallible_recovery.is_complete());

  let sdl = "type T { f: Int }\ntype U { g: Int }";
  let parse = parse_type_system_document(sdl);
  let (fallible, _) = project_type_system_document_recovered(&parse, sdl).expect("matches");
  let pair = Verified::new(&parse, sdl).expect("matches");
  let (verified, recovery) = project_type_system_document_verified(pair);
  assert_eq!(fallible, verified);
  assert_eq!(recovery.projected(), 2);
}

/// **al8n/smear#218's round-four addendum.** A described shorthand refuses, and its recovery is not
/// complete.
///
/// `"d" { f }` is reported by the lossless `definition` and the operation is built *around* the
/// description. The walk this replaces returned `OperationDefinition::Shorthand` with the
/// description preserved — a value the syntactic parser refuses — and the recovering executable
/// door counted the entry projected with `skipped == 0`.
#[test]
fn a_described_shorthand_refuses_and_its_recovery_is_not_complete() {
  let src = "\"d\" { f }";
  let parse = parse_document(src);
  assert!(parse.has_errors(), "the parser reports the description");
  assert!(oracle(src).is_err(), "and the syntactic parser refuses it");
  assert_eq!(
    refuse(src),
    ProjectErrorKind::UnexpectedChild {
      parent: K::OperationDefinition,
      found: K::Description,
    }
  );
  let parse = parse_executable_document(src);
  let (document, recovery) =
    project_executable_document_recovered(&parse, src).expect("the pair verifies");
  assert_eq!(document.definitions().len(), 0);
  assert_eq!(recovery, Recovery::new(0, 1));
  assert!(!recovery.is_complete());

  // The control: a described *named* operation is this dialect's, and it projects.
  let named = "\"d\" query Q { f }";
  assert_eq!(
    project(&parse_document(named), named).expect("projects"),
    oracle(named).expect("parses")
  );
}

/// A description in front of an `extend` sits **inside** the extension node in this dialect — the
/// node opens at a mark taken before the description — and every extension's walk refuses it as
/// the first element its sequence has no place for. The walk this replaces covered it with the
/// unread extent and projected the extension.
#[test]
fn a_described_extension_refuses() {
  for (src, parent) in [
    ("\"d\" extend scalar S @k", K::ScalarTypeExtension),
    ("\"d\" extend type T @k", K::ObjectTypeExtension),
    ("\"d\" extend schema @k", K::SchemaExtension),
  ] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src}");
    assert!(oracle(src).is_err(), "{src}");
    assert_eq!(
      refuse(src),
      ProjectErrorKind::UnexpectedChild {
        parent,
        found: K::Description,
      },
      "{src}"
    );
    let (_, recovery) =
      project_type_system_document_recovered(&parse_type_system_document(src), src)
        .expect("the pair verifies");
    assert!(!recovery.is_complete(), "{src}");
  }
}

// ---------------------------------------------------------------------------------------------
// the hatches, each refused where it stands
// ---------------------------------------------------------------------------------------------

#[test]
fn a_fragment_name_split_into_two_tokens_refuses() {
  // The `on` bypass, and the class behind it. The slot walk collected every direct `Name` into
  // three slots and read the fragment's name out of the second, so `Name("o")` + `Name("n")`
  // spelled `on` in the source while the rule inspected `o`. A fragment's sequence holds one name;
  // the second is refused where it sits.
  let text = "fragment on on T{f}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::FragmentDefinition);
  tree.token(K::Name, "fragment").token(K::Space, " ");
  tree.token(K::Name, "o").token(K::Name, "n");
  tree
    .token(K::Space, " ")
    .token(K::Name, "on")
    .token(K::Space, " ");
  tree.named_type("T");
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field).token(K::Name, "f").close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(
    node.text().to_string(),
    text,
    "the split has to spell `on` in the source, or it witnesses nothing"
  );
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::FragmentDefinition,
      found: K::Name,
    },
    "the second name is refused where it sits"
  );
}

#[test]
fn a_fourth_name_token_refuses() {
  // `Names` dropped a fourth `Name` rather than storing it, with its bytes covered: "no production
  // reads one". A directive definition's sequence reads `directive`, its name, `repeatable` and
  // `on`; a fifth is a tree no production builds.
  let text = "directive @d repeatable on x FIELD";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::DirectiveDefinition);
  tree.token(K::Name, "directive").token(K::Space, " ");
  tree.token(K::At, "@").token(K::Name, "d");
  tree.token(K::Space, " ").token(K::Name, "repeatable");
  tree.token(K::Space, " ").token(K::Name, "on");
  tree.token(K::Space, " ").token(K::Name, "x");
  tree.token(K::Space, " ");
  tree
    .open(K::DirectiveLocations)
    .token(K::Name, "FIELD")
    .close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::DirectiveDefinition,
      found: K::Name,
    }
  );

  // And the shape `Names` was sized for: an extension's third name is its target, so a fourth used
  // to vanish inside the extension's span.
  let text = "extend type T U @k";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeExtension);
  tree.token(K::Name, "extend").token(K::Space, " ");
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.token(K::Name, "T").token(K::Space, " ");
  tree.token(K::Name, "U").token(K::Space, " ");
  tree.open(K::Directives);
  tree
    .open(K::Directive)
    .token(K::At, "@")
    .token(K::Name, "k")
    .close();
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::ObjectTypeExtension,
      found: K::Name,
    }
  );
}

#[test]
fn a_duplicate_of_an_expected_once_child_refuses() {
  // Sixty-seven arms dispatched behind an `if x_node.is_none()` guard, and every one of them fell
  // through to a wildcard that covered the duplicate and dropped it. The second one is simply not in
  // the sequence now.
  for (what, text, build, found) in [
    (
      "a second fields block",
      "type T{f:Int}{g:Int}",
      (|tree: &mut Tree| {
        tree.open(K::ObjectTypeDefinition);
        tree.token(K::Name, "type").token(K::Space, " ");
        tree.token(K::Name, "T");
        tree.fields("f", "Int");
        tree.fields("g", "Int");
        tree.close();
      }) as fn(&mut Tree),
      K::FieldsDefinition,
    ),
    (
      "a second directive run",
      "scalar S@a@b",
      |tree: &mut Tree| {
        tree.open(K::ScalarTypeDefinition);
        tree.token(K::Name, "scalar").token(K::Space, " ");
        tree.token(K::Name, "S");
        for name in ["a", "b"] {
          tree.open(K::Directives);
          tree
            .open(K::Directive)
            .token(K::At, "@")
            .token(K::Name, name)
            .close();
          tree.close();
        }
        tree.close();
      },
      K::Directives,
    ),
    (
      "a second type reference",
      "type T{f:A B}",
      |tree: &mut Tree| {
        tree.open(K::ObjectTypeDefinition);
        tree.token(K::Name, "type").token(K::Space, " ");
        tree.token(K::Name, "T");
        tree.open(K::FieldsDefinition);
        tree.token(K::LBrace, "{");
        tree.open(K::FieldDefinition);
        tree.token(K::Name, "f").token(K::Colon, ":");
        tree.named_type("A");
        tree.token(K::Space, " ");
        tree.named_type("B");
        tree.close();
        tree.token(K::RBrace, "}");
        tree.close();
        tree.close();
      },
      K::NamedType,
    ),
  ] {
    let mut tree = Tree::new();
    tree.open(K::Document);
    build(&mut tree);
    tree.close();
    let node = tree.finish();
    assert_eq!(node.text().to_string(), text, "{what}");
    let kind = refuse_tree(node, text);
    assert!(
      matches!(kind, ProjectErrorKind::UnexpectedChild { found: f, .. } if f == found),
      "{what}: {kind:?}"
    );
  }
}

#[test]
fn a_stray_token_a_shape_does_not_spell_refuses() {
  // `extent.token(token)` folded any non-trivia token of any kind into the span. A scalar definition
  // spells `scalar`, a name and directives; a `:` under it is a byte the walk would have covered and
  // never represented.
  let text = "scalar S:";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "S").token(K::Colon, ":");
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::ScalarTypeDefinition,
      found: K::Colon,
    }
  );
}

#[test]
fn a_keyword_slot_is_read_by_spelling() {
  // The directive definition read "the `Name` at index 2 is `repeatable` or `on`" and never read
  // the `on` at all, so `directive @d foo FIELD` answered what `directive @d on FIELD` answers, with
  // `foo`'s bytes in the span and nowhere else. Every keyword is an atom at its own position now.
  for text in [
    "directive @d foo FIELD",
    "directive @d repeatable foo FIELD",
  ] {
    let parse = parse_document(text);
    assert!(parse.has_errors(), "{text}: the parser reports it");
    let mut tree = Tree::new();
    tree.open(K::Document);
    tree.open(K::DirectiveDefinition);
    tree.token(K::Name, "directive").token(K::Space, " ");
    tree.token(K::At, "@").token(K::Name, "d");
    for word in text["directive @d ".len()..].split(' ') {
      if word == "FIELD" {
        tree
          .open(K::DirectiveLocations)
          .token(K::Name, "FIELD")
          .close();
      } else {
        tree
          .token(K::Space, " ")
          .token(K::Name, word)
          .token(K::Space, " ");
      }
    }
    tree.close();
    tree.close();
    let node = tree.finish();
    let built = node.text().to_string();
    assert_eq!(
      refuse_tree(node, &built),
      ProjectErrorKind::UnexpectedChild {
        parent: K::DirectiveDefinition,
        found: K::Name,
      },
      "{text}"
    );
  }

  // A root operation type and an operation's keyword are read for their spelling too.
  assert_eq!(
    refusal_of("schema { foo: Q }"),
    "MalformedToken Name",
    "the root keyword is classified, not assumed"
  );
}

/// The smallest definition of `kind` whose sequence is complete, with `foreign` appended.
fn definition_with_foreign(kind: K, head: &[&str], foreign: K) -> (SyntaxNode, String) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(kind);
  for (i, word) in head.iter().enumerate() {
    if i > 0 {
      tree.token(K::Space, " ");
    }
    tree.token(K::Name, word);
  }
  match foreign {
    K::FieldsDefinition => {
      tree.fields("f", "Int");
    }
    K::EnumValuesDefinition => {
      tree.open(K::EnumValuesDefinition);
      tree.token(K::LBrace, "{");
      tree.open(K::EnumValueDefinition);
      tree.open(K::EnumValue).token(K::Name, "A").close();
      tree.close();
      tree.token(K::RBrace, "}");
      tree.close();
    }
    K::UnionMemberTypes => {
      tree.open(K::UnionMemberTypes);
      tree.token(K::Equal, "=");
      tree.named_type("A");
      tree.close();
    }
    _ => unreachable!("a foreign kind this helper does not build"),
  }
  tree.close();
  tree.close();
  let node = tree.finish();
  let text = node.text().to_string();
  (node, text)
}

#[test]
fn every_definition_kind_refuses_a_foreign_child() {
  // Each definition's walk used to send every child it had no slot for to the unread extent — a
  // caller-built `ScalarTypeDefinition` holding a whole `FieldsDefinition` projected `Ok` with the
  // block dropped inside its span. A foreign child is not in the sequence now, and `end` refuses it.
  for (kind, head, foreign) in [
    (
      K::ScalarTypeDefinition,
      &["scalar", "S"][..],
      K::FieldsDefinition,
    ),
    (
      K::UnionTypeDefinition,
      &["union", "U"][..],
      K::FieldsDefinition,
    ),
    (
      K::EnumTypeDefinition,
      &["enum", "E"][..],
      K::FieldsDefinition,
    ),
    (
      K::InputObjectTypeDefinition,
      &["input", "I"][..],
      K::FieldsDefinition,
    ),
    (
      K::ObjectTypeDefinition,
      &["type", "T"][..],
      K::EnumValuesDefinition,
    ),
    (
      K::InterfaceTypeDefinition,
      &["interface", "I"][..],
      K::UnionMemberTypes,
    ),
  ] {
    let (node, text) = definition_with_foreign(kind, head, foreign);
    assert_eq!(
      refuse_tree(node, &text),
      ProjectErrorKind::UnexpectedChild {
        parent: kind,
        found: foreign,
      },
      "{kind:?} holding a {foreign:?}"
    );
  }
}

#[test]
fn every_extension_kind_refuses_a_foreign_tail() {
  // `extension_parts` accepted the union of all six extension tails regardless of the node's kind,
  // and the kind-specific constructors ignored the tails they could not hold. The tail is each
  // kind's own sequence now.
  for (kind, head, foreign) in [
    (
      K::ScalarTypeExtension,
      &["extend", "scalar", "S"][..],
      K::FieldsDefinition,
    ),
    (
      K::UnionTypeExtension,
      &["extend", "union", "U"][..],
      K::FieldsDefinition,
    ),
    (
      K::EnumTypeExtension,
      &["extend", "enum", "E"][..],
      K::FieldsDefinition,
    ),
    (
      K::InputObjectTypeExtension,
      &["extend", "input", "I"][..],
      K::FieldsDefinition,
    ),
    (
      K::ObjectTypeExtension,
      &["extend", "type", "T"][..],
      K::EnumValuesDefinition,
    ),
    (
      K::InterfaceTypeExtension,
      &["extend", "interface", "I"][..],
      K::UnionMemberTypes,
    ),
  ] {
    let (node, text) = definition_with_foreign(kind, head, foreign);
    assert_eq!(
      refuse_tree(node, &text),
      ProjectErrorKind::UnexpectedChild {
        parent: kind,
        found: foreign,
      },
      "{kind:?} holding a {foreign:?}"
    );
  }
  // And each kind's own tail still projects, so the refusals above are about foreignness.
  for src in [
    "extend scalar S @k",
    "extend type T implements I @k { f: Int }",
    "extend interface I { f: Int }",
    "extend union U @k = A | B",
    "extend enum E { A }",
    "extend input I @k { f: Int }",
    "extend schema @k { query: Q }",
  ] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}");
    assert_eq!(
      project(&parse, src).expect(src),
      oracle(src).expect(src),
      "{src}"
    );
  }
}

/// A separated walk's node inside the smallest document that reaches it, with `inside` writing the
/// node's own children after its opener.
fn separated_in(parent: K, inside: impl FnOnce(&mut Tree)) -> (SyntaxNode, String) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  match parent {
    K::ImplementsInterfaces => {
      tree.open(K::ObjectTypeDefinition);
      tree.token(K::Name, "type").token(K::Space, " ");
      tree.token(K::Name, "T").token(K::Space, " ");
      tree.open(K::ImplementsInterfaces);
      tree.token(K::Name, "implements").token(K::Space, " ");
      inside(&mut tree);
      tree.close();
      tree.fields("f", "Int");
      tree.close();
    }
    K::UnionMemberTypes => {
      tree.open(K::UnionTypeDefinition);
      tree.token(K::Name, "union").token(K::Space, " ");
      tree.token(K::Name, "U").token(K::Space, " ");
      tree.open(K::UnionMemberTypes);
      tree.token(K::Equal, "=");
      inside(&mut tree);
      tree.close();
      tree.close();
    }
    K::DirectiveLocations => {
      tree.open(K::DirectiveDefinition);
      tree.token(K::Name, "directive").token(K::Space, " ");
      tree
        .token(K::At, "@")
        .token(K::Name, "d")
        .token(K::Space, " ");
      tree.token(K::Name, "on").token(K::Space, " ");
      tree.open(K::DirectiveLocations);
      inside(&mut tree);
      tree.close();
      tree.close();
    }
    _ => unreachable!("not a separated walk"),
  }
  tree.close();
  let node = tree.finish();
  let text = node.text().to_string();
  (node, text)
}

#[test]
fn the_separated_atoms_refuse_at_the_obstruction() {
  // `sep? item (sep item)*`, transcribed once in the substrate and used by all three of this
  // dialect's separated walks. A dangling separator with nothing after it is `MissingChild` over the
  // node; a doubled one, or a leading one where none is allowed, is `UnexpectedChild` at the
  // separator — the obstruction in plain view.
  let member: fn(&mut Tree, &str) = |tree, name| {
    tree.named_type(name);
  };
  let separator: [(K, &str); 3] = [
    (K::ImplementsInterfaces, "&"),
    (K::UnionMemberTypes, "|"),
    (K::DirectiveLocations, "|"),
  ];
  for (parent, sep) in separator {
    let kind = if sep == "&" { K::Ampersand } else { K::Pipe };
    let item = |tree: &mut Tree, name: &str| {
      if parent == K::DirectiveLocations {
        tree.token(K::Name, name);
      } else {
        member(tree, name);
      }
    };
    let first = if parent == K::DirectiveLocations {
      "FIELD"
    } else {
      "A"
    };
    // A trailing separator.
    let (node, text) = separated_in(parent, |tree| {
      item(tree, first);
      tree.token(kind, sep);
    });
    let refused = refuse_tree(node, &text);
    assert!(
      matches!(refused, ProjectErrorKind::MissingChild { parent: p, .. } if p == parent),
      "{parent:?} `{text}`: {refused:?}"
    );
    // A doubled separator.
    let (node, text) = separated_in(parent, |tree| {
      item(tree, first);
      tree.token(kind, sep).token(kind, sep);
      item(tree, first);
    });
    assert_eq!(
      refuse_tree(node, &text),
      ProjectErrorKind::UnexpectedChild {
        parent,
        found: kind,
      },
      "{parent:?} `{text}`"
    );
    // Nothing at all after the opener.
    let (node, text) = separated_in(parent, |_| {});
    let refused = refuse_tree(node, &text);
    assert!(
      matches!(refused, ProjectErrorKind::MissingChild { parent: p, .. } if p == parent),
      "{parent:?} `{text}`: {refused:?}"
    );
  }
}

#[test]
fn a_one_of_slot_group_refuses_its_second_member() {
  // A slot filled from a kind set — a field's type, an object field's value — is one atom, and the
  // next atom in the sequence refuses a second member rather than a guard dropping it.
  let text = "{f(a:{b:1 2})}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field);
  tree.token(K::Name, "f");
  tree.open(K::Arguments);
  tree.token(K::LParen, "(");
  tree.open(K::Argument);
  tree.token(K::Name, "a").token(K::Colon, ":");
  tree.open(K::ObjectValue);
  tree.token(K::LBrace, "{");
  tree.open(K::ObjectField);
  tree.token(K::Name, "b").token(K::Colon, ":");
  tree.open(K::IntValue).token(K::Int, "1").close();
  tree.token(K::Space, " ");
  tree.open(K::IntValue).token(K::Int, "2").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.token(K::RParen, ")");
  tree.close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::ObjectField,
      found: K::IntValue,
    }
  );
}

// ---------------------------------------------------------------------------------------------
// leaves: the lexer's own doors
// ---------------------------------------------------------------------------------------------

#[test]
fn a_name_token_spelled_as_a_number_refuses() {
  // `Name` → `slice`, no door: a caller-built `Name` token spelled `1` projected to `Name("1")`, a
  // value no source produces. `smear_lexer::graphql::identifier` — the shipped scanner,
  // whole-slice — is what says so now.
  let text = "scalar 1";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "1");
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::MalformedToken { kind: K::Name }
  );

  // A keyword passes: this dialect's keywords are contextual, so the door reads `query` as an
  // identifier and the classifier tells them apart afterwards.
  let src = "scalar query";
  assert_eq!(
    project(&parse_document(src), src).expect("a keyword-spelled name projects"),
    oracle(src).expect("and the parser agrees")
  );
}

#[test]
fn a_numeric_spelling_the_scanner_will_not_read_back_refuses() {
  // `Int`/`Float` → the raw slice, no door at all. This dialect's AST stores the text rather than a
  // classified literal, so there was no radix to get wrong — but `IntValue("abc")` was producible.
  // `int_literal` and `float_literal` are the scanner over the whole slice.
  for (what, node, token, literal) in [
    ("letters claimed as an integer", K::IntValue, K::Int, "abc"),
    ("a float claimed as an integer", K::IntValue, K::Int, "1.5"),
    (
      "an integer claimed as a float",
      K::FloatValue,
      K::Float,
      "1",
    ),
    ("a leading zero", K::IntValue, K::Int, "01"),
    ("two literals in one token", K::IntValue, K::Int, "1 2"),
  ] {
    let (tree, text) = scalar_with_literal(node, token, literal);
    assert_eq!(tree.text().to_string(), text, "{what}");
    assert_eq!(
      refuse_tree(tree, &text),
      ProjectErrorKind::MalformedToken { kind: token },
      "{what}"
    );
  }
  // And what the scanner does read passes, the slice unchanged.
  for literal in ["-0", "12", "1.5e3", "-2E-1"] {
    let src = std::format!("scalar S @d(n: {literal})");
    assert_eq!(
      project(&parse_document(&src), &src).expect(&src),
      oracle(&src).expect(&src),
      "{src}"
    );
  }
}

#[test]
fn a_null_value_over_another_identifier_refuses() {
  // `NullValue` → no spelling check: whatever identifier the tree held became `null`. Its sibling
  // `BooleanValue` always compared.
  let (node, text) = scalar_with_literal(K::NullValue, K::Name, "X");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::MalformedToken { kind: K::Name }
  );
  let (node, text) = scalar_with_literal(K::BooleanValue, K::Name, "X");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::MalformedToken { kind: K::Name }
  );
}

#[test]
fn a_string_leaf_without_its_quotes_refuses() {
  let (node, text) = scalar_with_literal(K::StringValue, K::String, "abc");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::MalformedToken { kind: K::String }
  );
}

#[test]
fn a_leaf_with_two_literal_tokens_refuses() {
  // A leaf that read the first token of its kind and folded the rest answered `1` for `12` with a
  // span across both bytes.
  let text = "scalar S@d(n:12)";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "S");
  tree.one_argument("n", |tree| {
    tree.open(K::IntValue);
    tree.token(K::Int, "1").token(K::Int, "2");
    tree.close();
  });
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::IntValue,
      found: K::Int,
    }
  );
  assert_eq!(
    project(&parse_document(text), text).expect("projects"),
    oracle(text).expect("parses"),
    "the control: one `Int` token is the shape the parser gives the same bytes"
  );
}

#[test]
fn a_tree_that_splits_a_token_projects_the_tree_it_was_handed() {
  // **A decision, not a defect** — see the module header's *what a tree the parser did not build
  // is promised*. Over `[-12]` a caller can build two adjacent `IntValue` tokens; each slice is one
  // whole integer to the lexer's door, the byte verification passes because the concatenation is
  // the source, and the projection answers the AST of the sentence the *tree* spells.
  let text = "scalar S@d(n:[-12])";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.token(K::Name, "S");
  tree.one_argument("n", |tree| {
    tree.open(K::ListValue);
    tree.token(K::LBracket, "[");
    tree.open(K::IntValue).token(K::Int, "-1").close();
    tree.open(K::IntValue).token(K::Int, "2").close();
    tree.token(K::RBracket, "]");
    tree.close();
  });
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  let projected = DocumentNode::cast_node(node)
    .expect("a Document root")
    .to_ast(text)
    .expect("a tree the parser would not build is still a tree, and this one is well-shaped");
  let debug = std::format!("{projected:?}");
  assert_eq!(debug.matches("IntValue").count(), 2, "the tree says two");
  let parsed = project(&parse_document(text), text).expect("the real parse projects");
  assert_eq!(
    std::format!("{parsed:?}").matches("IntValue").count(),
    1,
    "the shipped lexer reads one integer `-12`"
  );
  assert!(
    debug.contains("start: 14, end: 16") && debug.contains("start: 16, end: 17"),
    "the two literals carry the tree's own ranges: {debug}"
  );
}

// ---------------------------------------------------------------------------------------------
// the rule positions, derived from the syntactic parser's refusals
// ---------------------------------------------------------------------------------------------

#[test]
fn an_enum_value_named_true_refuses() {
  let src = read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests/corpus/invalid_enum_reserved_spelling.graphql"),
  )
  .1;
  assert_eq!(
    refuse(&src),
    ProjectErrorKind::SemanticRule {
      rule: "an enum value may not be `true`, `false` or `null`",
    }
  );
}

/// `{f(a:<value>)}` with the value node `value` writes — the executable value grammar.
fn executable_value(value: impl FnOnce(&mut Tree), text: &str) -> (SyntaxNode, String) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field);
  tree.token(K::Name, "f");
  tree.open(K::Arguments);
  tree.token(K::LParen, "(");
  tree.open(K::Argument);
  tree.token(K::Name, "a").token(K::Colon, ":");
  value(&mut tree);
  tree.close();
  tree.token(K::RParen, ")");
  tree.close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  (node, text.to_string())
}

#[test]
fn every_derived_rule_position_refuses_its_spelling_in_a_hand_built_tree() {
  // The module header derives four positions from the syntactic parser's refusals. Two of them the
  // lossless parser reports and still builds, so a parse reaches them — `a_fragment_named_on_refuses`
  // and `an_enum_value_named_true_refuses` — and two it never builds at all: the spread dispatch
  // reads `... on` as an inline fragment's head, and the value dispatch reads `true`, `false` and
  // `null` as a boolean and a null. Those two are invisible to the mutation law, so they are pinned
  // here, over trees built by hand.
  let rule_enum = ProjectErrorKind::SemanticRule {
    rule: "an enum value may not be `true`, `false` or `null`",
  };
  for spelling in ["true", "false", "null"] {
    let (node, text) = executable_value(
      |tree| {
        tree.open(K::EnumValue).token(K::Name, spelling).close();
      },
      &std::format!("{{f(a:{spelling})}}"),
    );
    assert_eq!(
      refuse_tree(node, &text),
      rule_enum,
      "a value-position `{spelling}`"
    );
  }

  let text = "{...on}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::FragmentSpread);
  tree.token(K::Spread, "...").token(K::Name, "on");
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::SemanticRule {
      rule: "a fragment spread may not target `on`",
    }
  );

  // The two a parse reaches, from a hand-built tree too, so all four read the same way here.
  let text = "enum E{null}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::EnumTypeDefinition);
  tree
    .token(K::Name, "enum")
    .token(K::Space, " ")
    .token(K::Name, "E");
  tree.open(K::EnumValuesDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::EnumValueDefinition);
  tree.open(K::EnumValue).token(K::Name, "null").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(refuse_tree(node, text), rule_enum);
  assert_eq!(
    refuse("fragment on on T { f }"),
    ProjectErrorKind::SemanticRule {
      rule: "a fragment may not be named `on`",
    }
  );
}

#[test]
fn a_contextual_keyword_is_a_name_at_every_name_position() {
  // This dialect's keywords are contextual: the lexer reads `on`, `query` and `type` as identifiers,
  // and the syntactic parser accepts each of them wherever the grammar says *name* — except at the
  // four positions the header derives. Each of these is accepted and projects to the parse.
  for src in [
    "type on { on: on }",
    "type query implements on & type @on(on: on) { on(on: on = on): on }",
    "interface on { on: [on!]! }",
    "union on = on | query",
    "enum on { on query type }",
    "input on { on: on = { on: on } }",
    "scalar on @on",
    "directive @on(on: on) repeatable on FIELD",
    "schema { query: on }",
    "extend type on @on",
    "query query($on: on = on) @on(on: $on) { on: on(on: on) ...query ... on on { on } }",
    "fragment query on on { on }",
  ] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}");
    assert_eq!(
      project(&parse, src).expect(src),
      oracle(src).expect(src),
      "{src}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// what the parser reports and still builds
// ---------------------------------------------------------------------------------------------

/// Which root a probe goes through.
#[derive(Clone, Copy, Debug)]
enum Root {
  Mixed,
  Executable,
  TypeSystem,
}

/// What a report-and-build site leaves out of the node it builds.
#[derive(Clone, Copy, Debug)]
enum Absent {
  /// One token, with **no AST image**, is missing from a node of this kind — the leniency
  /// criterion's candidate. The probe's tree is the witness if it holds such a node.
  Token(K, &'static str),
  /// What is missing is a constituent the AST holds — a member, a name, a tail. Never lenient.
  Imaged,
  /// Nothing is missing: the site reports something *present* — a description, a spelling a rule
  /// or a classifier refuses, a variable in a constant position.
  Not,
}

/// One non-hole recovery site of the GraphQL lossless productions: the file, the family (`report`
/// for a `recover::report_unexpected::<…>` call, `unclosed` for a `recover::unclosed_*::<…>` one), a
/// probe that reaches it with a hole-free tree, the root, what the projection answers, and what
/// the site leaves out.
type Site = (
  &'static str,
  &'static str,
  &'static str,
  Root,
  &'static str,
  Absent,
);

/// Every report-and-build site, one probe each — the module header's table, executable.
const SITES: &[Site] = &[
  (
    "document.rs",
    "report",
    "extend scalar S",
    Root::Mixed,
    "MissingChild ScalarTypeExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend type T",
    Root::Mixed,
    "MissingChild ObjectTypeExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend union U",
    Root::Mixed,
    "MissingChild UnionTypeExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend enum E",
    Root::Mixed,
    "MissingChild EnumTypeExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend input I",
    Root::Mixed,
    "MissingChild InputObjectTypeExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend schema",
    Root::Mixed,
    "MissingChild SchemaExtension",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "extend",
    Root::Mixed,
    "UnexpectedChild Document Name",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "\"d\" { f }",
    Root::Mixed,
    "UnexpectedChild OperationDefinition Description",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" extend scalar S @k",
    Root::Mixed,
    "UnexpectedChild ScalarTypeExtension Description",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" extend scalar S @k",
    Root::TypeSystem,
    "UnexpectedChild ScalarTypeExtension Description",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "",
    Root::Mixed,
    "MissingChild Document",
    Absent::Imaged,
  ),
  (
    "document.rs",
    "report",
    "",
    Root::TypeSystem,
    "MissingChild TypeSystemDocument",
    Absent::Imaged,
  ),
  (
    "executable.rs",
    "report",
    "query Q() { f }",
    Root::Executable,
    "MissingChild VariablesDefinition",
    Absent::Imaged,
  ),
  (
    "executable.rs",
    "unclosed",
    "query Q($a: Int",
    Root::Executable,
    "UnexpectedChild Root OperationType",
    Absent::Token(K::VariablesDefinition, ")"),
  ),
  (
    "executable.rs",
    "report",
    "fragment on on T { f }",
    Root::Executable,
    "SemanticRule",
    Absent::Not,
  ),
  (
    "executable.rs",
    "report",
    "\"d\" { f }",
    Root::Executable,
    "UnexpectedChild OperationDefinition Description",
    Absent::Not,
  ),
  (
    "executable.rs",
    "report",
    "",
    Root::Executable,
    "MissingChild ExecutableDocument",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "report",
    "fragment F T { f }",
    Root::Mixed,
    "Ok",
    Absent::Token(K::FragmentDefinition, "on"),
  ),
  (
    "selection.rs",
    "report",
    "fragment F on { f }",
    Root::Mixed,
    "UnexpectedChild FragmentDefinition SelectionSet",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "report",
    "{ ... }",
    Root::Mixed,
    "UnexpectedChild SelectionSet Spread",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "report",
    "{ }",
    Root::Mixed,
    "MissingChild SelectionSet",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "unclosed",
    "{ f",
    Root::Mixed,
    "Ok",
    Absent::Token(K::SelectionSet, "}"),
  ),
  (
    "definition.rs",
    "report",
    "type T { f(): Int }",
    Root::Mixed,
    "MissingChild ArgumentsDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "type T { f(a: Int",
    Root::Mixed,
    "UnexpectedChild Document Name",
    Absent::Token(K::ArgumentsDefinition, ")"),
  ),
  (
    "definition.rs",
    "report",
    "type T { }",
    Root::Mixed,
    "MissingChild FieldsDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "type T { f: Int",
    Root::Mixed,
    "Ok",
    Absent::Token(K::FieldsDefinition, "}"),
  ),
  (
    "definition.rs",
    "report",
    "input I { }",
    Root::Mixed,
    "MissingChild InputFieldsDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "input I { f: Int",
    Root::Mixed,
    "Ok",
    Absent::Token(K::InputFieldsDefinition, "}"),
  ),
  (
    "definition.rs",
    "report",
    "type T implements { f: Int }",
    Root::Mixed,
    "MissingChild ImplementsInterfaces",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "type T implements A & { f: Int }",
    Root::Mixed,
    "MissingChild ImplementsInterfaces",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "union U =",
    Root::Mixed,
    "MissingChild UnionMemberTypes",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "union U = A |",
    Root::Mixed,
    "MissingChild UnionMemberTypes",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "directive @d on FOO",
    Root::Mixed,
    "MalformedToken Name",
    Absent::Not,
  ),
  (
    "definition.rs",
    "report",
    "directive @d on |",
    Root::Mixed,
    "MissingChild DirectiveLocations",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "directive @d on FIELD |",
    Root::Mixed,
    "MissingChild DirectiveLocations",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "enum E { true }",
    Root::Mixed,
    "SemanticRule",
    Absent::Not,
  ),
  (
    "definition.rs",
    "report",
    "enum E { }",
    Root::Mixed,
    "MissingChild EnumValuesDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "enum E { A",
    Root::Mixed,
    "Ok",
    Absent::Token(K::EnumValuesDefinition, "}"),
  ),
  (
    "definition.rs",
    "report",
    "schema { foo: Q }",
    Root::Mixed,
    "MalformedToken Name",
    Absent::Not,
  ),
  (
    "definition.rs",
    "report",
    "schema { }",
    Root::Mixed,
    "MissingChild RootOperationTypeDefinitions",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "schema { query: Q",
    Root::Mixed,
    "Ok",
    Absent::Token(K::RootOperationTypeDefinitions, "}"),
  ),
  (
    "definition.rs",
    "report",
    "directive @d FIELD",
    Root::Mixed,
    "Ok",
    Absent::Token(K::DirectiveDefinition, "on"),
  ),
  (
    "definition.rs",
    "report",
    "directive @d on",
    Root::Mixed,
    "MissingChild DirectiveDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "schema @k",
    Root::Mixed,
    "MissingChild SchemaDefinition",
    Absent::Imaged,
  ),
  (
    "directive.rs",
    "unclosed",
    "{ f(a: 1",
    Root::Mixed,
    "Ok",
    Absent::Token(K::Arguments, ")"),
  ),
  (
    "ty.rs",
    "unclosed",
    "type T { f: [Int",
    Root::Mixed,
    "Ok",
    Absent::Token(K::ListType, "]"),
  ),
  (
    "value.rs",
    "unclosed",
    "{ f(a: [1",
    Root::Mixed,
    "Ok",
    Absent::Token(K::ListValue, "]"),
  ),
  (
    "value.rs",
    "unclosed",
    "{ f(a: {b: 1",
    Root::Mixed,
    "Ok",
    Absent::Token(K::ObjectValue, "}"),
  ),
  (
    "value.rs",
    "report",
    "type T { f(a: Int = $v): Int }",
    Root::Mixed,
    "UnexpectedChild DefaultValue Variable",
    Absent::Not,
  ),
];

/// Whether the parse of `text` under `root` holds a node of `kind` without the token `token` —
/// the hole-free witness the leniency criterion asks for. A keyword row is read by **position**,
/// as the mutation law reads it: both keyword rows' nodes open with the definition's keyword and
/// its name, and the `on` is a third `Name`.
fn witnessed(text: &str, root: Root, kind: K, token: &str) -> bool {
  let parse = match root {
    Root::Mixed => parse_document(text),
    Root::Executable => parse_executable_document(text),
    Root::TypeSystem => parse_type_system_document(text),
  };
  let tree = parse.syntax();
  !tree
    .descendants_with_tokens()
    .any(|element| matches!(element.kind(), K::Error | K::Gap))
    && tree.descendants().any(|node| {
      node.kind() == kind && {
        let tokens: Vec<_> = node
          .children_with_tokens()
          .filter_map(|element| element.into_token())
          .collect();
        if token == "on" {
          !tokens
            .iter()
            .filter(|child| child.kind() == K::Name)
            .skip(2)
            .any(|child| child.text() == token)
        } else {
          !tokens.iter().any(|child| child.text() == token)
        }
      }
    })
}

/// The module header's missing-token table, as `(parent kind, token)` rows read off the source.
fn header_lenient_rows() -> Vec<(String, String)> {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphql/lossless/project.rs");
  let source = std::fs::read_to_string(path).expect("the projection's source");
  let mut rows = Vec::new();
  let mut inside = false;
  for line in source.lines() {
    if line.starts_with("//! | parent | absent token |") {
      inside = true;
      continue;
    }
    if !inside || line.starts_with("//! |---") {
      continue;
    }
    if !line.starts_with("//! | ") {
      break;
    }
    let cells: Vec<&str> = line.split(" | ").collect();
    let kind = cells[1]
      .split("SyntaxKind::")
      .nth(1)
      .and_then(|rest| rest.split(')').next())
      .expect("a kind link");
    let token = cells[2].split('`').nth(1).expect("a spelled token");
    rows.push((kind.to_string(), token.to_string()));
  }
  rows.sort();
  rows
}

/// A refusal's kind, spelled compactly enough to sit in a table cell.
fn answer(kind: &ProjectErrorKind) -> String {
  match kind {
    ProjectErrorKind::MissingChild { parent, .. } => std::format!("MissingChild {parent:?}"),
    ProjectErrorKind::UnexpectedChild { parent, found } => {
      std::format!("UnexpectedChild {parent:?} {found:?}")
    }
    ProjectErrorKind::MalformedToken { kind } => std::format!("MalformedToken {kind:?}"),
    ProjectErrorKind::SemanticRule { .. } => "SemanticRule".to_string(),
    other => std::format!("{other:?}"),
  }
}

/// What the fail-fast door of `root` answers for `text` — `hole` when the tree carries one — and
/// whether the parse reported anything.
fn site_answer(text: &str, root: Root) -> (bool, String) {
  let parse = match root {
    Root::Mixed => parse_document(text),
    Root::Executable => parse_executable_document(text),
    Root::TypeSystem => parse_type_system_document(text),
  };
  let hole = parse
    .syntax()
    .descendants_with_tokens()
    .any(|element| matches!(element.kind(), K::Error | K::Gap));
  if hole {
    return (parse.has_errors(), "hole".to_string());
  }
  let projected = match root {
    Root::Mixed => project(&parse, text).map(|_| ()),
    Root::Executable => project_executable_document(&parse, text).map(|_| ()),
    Root::TypeSystem => project_type_system_document(&parse, text).map(|_| ()),
  };
  (
    parse.has_errors(),
    match projected {
      Ok(()) => "Ok".to_string(),
      Err(error) => answer(error.kind()),
    },
  )
}

/// The two call families [`SITES`] maps, as the census reads them out of the source.
const FAMILIES: [(&str, &[&str]); 2] = [
  ("report", &["recover::report_unexpected::<"]),
  (
    "unclosed",
    &[
      "recover::unclosed_list::<",
      "recover::unclosed_object::<",
      "recover::unclosed_parens::<",
    ],
  ),
];

/// How many of [`SITES`]' probes project.
const PROJECTING_SITES: usize = 11;

/// Image-less tokens a site can leave out whose probe is **not** a witness.
const UNWITNESSED: &[(&str, &str)] = &[];

#[test]
fn every_report_and_build_site_has_a_measured_answer() {
  // The census, per file and family: `grep -c 'recover::report_unexpected::<'` and the three
  // `recover::unclosed_*::<` spellings over the code lines of `graphql/lossless/*.rs`.
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../smear-parser/src/graphql/lossless");
  let mut in_source: BTreeMap<(String, &str), usize> = BTreeMap::new();
  for entry in std::fs::read_dir(&dir).expect("the lossless sources are readable") {
    let path = entry.expect("an entry").path();
    if path.extension().is_none_or(|ext| ext != "rs") {
      continue;
    }
    let file = path
      .file_name()
      .expect("a name")
      .to_string_lossy()
      .to_string();
    let text = std::fs::read_to_string(&path).expect("a readable source");
    let code: Vec<&str> = text
      .lines()
      .filter(|line| !line.trim_start().starts_with("//"))
      .collect();
    for (family, spellings) in FAMILIES {
      let count: usize = spellings
        .iter()
        .map(|spelling| {
          code
            .iter()
            .map(|line| line.matches(spelling).count())
            .sum::<usize>()
        })
        .sum();
      if count > 0 {
        in_source.insert((file.clone(), family), count);
      }
    }
  }
  let mut in_table: BTreeMap<(String, &str), usize> = BTreeMap::new();
  for (file, family, ..) in SITES {
    *in_table.entry(((*file).to_string(), *family)).or_default() += 1;
  }
  assert_eq!(
    in_table, in_source,
    "a recovery site was added or removed without its row here, or a row names no site"
  );
  let total = |family: &str| -> usize {
    in_source
      .iter()
      .filter(|((_, f), _)| *f == family)
      .map(|(_, n)| n)
      .sum()
  };
  assert_eq!(
    (total("report"), total("unclosed")),
    (38, 11),
    "the two families' populations"
  );

  // Each probe: the parser reported, the tree is hole-free, and the projection answers what the
  // table says. Every mismatch is collected before the assertion, so a moved answer names all of
  // its siblings at once.
  let mut lenient = 0;
  let mut moved = Vec::new();
  for (file, family, text, root, expected, _) in SITES {
    let (errors, got) = site_answer(text, *root);
    if !errors {
      moved.push(std::format!(
        "{file} {family} {text:?}: the probe reported nothing"
      ));
    }
    if got != *expected {
      moved.push(std::format!(
        "{file} {family} {text:?} under {root:?}: expected {expected}, got {got}"
      ));
    }
    lenient += usize::from(got == "Ok");
  }
  assert!(moved.is_empty(), "{moved:#?}");
  assert_eq!(
    lenient, PROJECTING_SITES,
    "the sites whose probe projects moved; each is a lenient row a site can leave well-formed"
  );

  // The derivation. A position is lenient iff the token the site leaves out has no AST image and
  // the site's probe is a hole-free witness of a node without it; the set this enumerates is the
  // header's missing-token table, row for row.
  let mut derived: Vec<(String, String)> = Vec::new();
  let mut unwitnessed: Vec<(String, String)> = Vec::new();
  for (_, _, text, root, _, absent) in SITES {
    if let Absent::Token(kind, token) = absent {
      let row = (std::format!("{kind:?}"), token.to_string());
      if witnessed(text, *root, *kind, token) {
        derived.push(row);
      } else {
        unwitnessed.push(row);
      }
    }
  }
  derived.sort();
  unwitnessed.sort();
  println!("DERIVED {derived:?}\nUNWITNESSED {unwitnessed:?}");
  assert_eq!(
    derived,
    header_lenient_rows(),
    "the header's lenient table is the report-and-build census's derivation"
  );
  let unwitnessed: Vec<(&str, &str)> = unwitnessed
    .iter()
    .map(|(kind, token)| (kind.as_str(), token.as_str()))
    .collect();
  assert_eq!(unwitnessed, UNWITNESSED);
}

/// `image` with every span end past `end` pulled back to `end` — the one place a closed text's
/// spans differ from the unclosed tree's, when the closers are a suffix written with no trivia.
fn clamp_spans(image: &str, end: usize) -> String {
  const OPEN: &str = "SimpleSpan { start: ";
  const MID: &str = ", end: ";
  let mut out = String::with_capacity(image.len());
  let mut rest = image;
  while let Some(at) = rest.find(OPEN) {
    out.push_str(&rest[..at + OPEN.len()]);
    rest = &rest[at + OPEN.len()..];
    for (i, sep) in [MID, ""].into_iter().enumerate() {
      let digits = rest.find(|c: char| !c.is_ascii_digit()).expect("a number");
      let n: usize = rest[..digits].parse().expect("a number");
      out.push_str(&n.min(end).to_string());
      rest = &rest[digits..];
      if i == 0 {
        assert!(rest.starts_with(sep));
        out.push_str(sep);
        rest = &rest[sep.len()..];
      }
    }
  }
  out.push_str(rest);
  out
}

#[test]
fn every_unclosed_closer_projects_what_the_closed_text_parses_to() {
  // Each `unclosed_*` site builds its node hole-free without the closer, and the closer has no AST
  // image. The projection of the unclosed text is the parse of the text closed, every span that
  // ended on a restored closer ending on the last token instead.
  for (open, closers) in [
    ("{ f(a: 1", ")}"),
    ("{ f(a: [1", "])}"),
    ("{ f(a: {b: 1", "})}"),
    ("{ f(a: {b: [1", "]})}"),
    ("scalar S @k(a: [1", "])"),
    ("type T { f: [Int", "]}"),
    ("type T { f: [[Int!]", "]}"),
    ("type T { f: Int", "}"),
    ("input I { f: Int", "}"),
    ("enum E { A", "}"),
    ("schema { query: Q", "}"),
    ("{ f", "}"),
  ] {
    let parse = parse_document(open);
    assert!(parse.has_errors(), "{open}");
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{open}: hole-free"
    );
    let closed = std::format!("{open}{closers}");
    let projected = project(&parse, open).expect(open);
    let expected = oracle(&closed).expect(&closed);
    assert_eq!(
      std::format!("{projected:?}"),
      clamp_spans(&std::format!("{expected:?}"), open.len()),
      "{open}"
    );
  }

  // The two whose closed text the parser still refuses: a list whose definition is lost at end of
  // input, left as an orphan beside its rubble. Lenient by the criterion, and it changes no
  // answer.
  for src in ["query Q($a: Int", "type T { f(a: Int"] {
    assert!(oracle(src).is_err(), "{src}");
    assert!(
      !parse_document(src)
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: hole-free"
    );
    assert!(
      project(&parse_document(src), src).is_err(),
      "{src}: the refusal is the rubble's, not the list's"
    );
  }
}

#[test]
fn a_type_condition_without_its_on_projects_what_the_text_with_it_parses_to() {
  // `(FragmentDefinition, on)`: the keyword has no AST image — the condition stores only its type's
  // name — and the lossless production reports a missing one and still builds the definition,
  // hole-free, around the type. The two texts are padded so every token but the keyword sits at the
  // same offset: the one span that differs is the condition's own, which starts at its first token
  // — the type in the tree, the `on` in the text.
  let src = "fragment F    T { f }";
  let restored = "fragment F on T { f }";
  let parse = parse_document(src);
  assert!(
    parse.has_errors(),
    "the production reports the missing `on`"
  );
  assert!(
    !parse
      .syntax()
      .descendants_with_tokens()
      .any(|element| matches!(element.kind(), K::Error | K::Gap)),
    "and builds the definition whole"
  );
  assert!(oracle(src).is_err(), "the syntactic parser rejects it");

  let projected = project(&parse, src).expect("a missing `on` is lenient");
  let expected = oracle(restored).expect("the text with the keyword parses");
  const IN_TEXT: &str = "SimpleSpan { start: 11, end: 15 }";
  const IN_TREE: &str = "SimpleSpan { start: 14, end: 15 }";
  let image = std::format!("{expected:?}");
  assert_eq!(
    image.matches(IN_TEXT).count(),
    1,
    "the condition's span is the one span `on` opens"
  );
  assert_eq!(
    std::format!("{projected:?}"),
    image.replacen(IN_TEXT, IN_TREE, 1),
    "the projection answers the restored text's value, the condition starting at its type"
  );
  let (document, recovery) =
    project_executable_document_recovered(&parse_executable_document(src), src)
      .expect("the pair verifies");
  assert_eq!(document.definitions().len(), 1);
  assert!(recovery.is_complete());
}

#[test]
fn a_directive_definition_without_its_on_projects_what_the_text_with_it_parses_to() {
  // `(DirectiveDefinition, on)`: no image, and the production reports a missing one and still
  // builds the definition, hole-free, around its locations. No span starts or ends on the `on`, so
  // the two values are equal outright.
  let src = "directive @d    FIELD | QUERY";
  let restored = "directive @d on FIELD | QUERY";
  let parse = parse_document(src);
  assert!(
    parse.has_errors(),
    "the production reports the missing `on`"
  );
  assert!(
    !parse
      .syntax()
      .descendants_with_tokens()
      .any(|element| matches!(element.kind(), K::Error | K::Gap)),
    "and builds the definition whole"
  );
  assert!(oracle(src).is_err(), "the syntactic parser rejects it");
  assert_eq!(
    project(&parse, src).expect("a missing `on` is lenient"),
    oracle(restored).expect("the text with the keyword parses"),
  );
}

#[test]
fn every_walk_is_a_transcription() {
  // al8n/smear#218's addenda counted the hatches in this file at `c885c07`: 48 bare
  // `extent.token(` folds, 34 `extent.unread(` wildcards behind 67 `is_none() =>` guards, and a
  // `Names` collector read at 22 sites. The census is read off the projection's own source so it
  // cannot drift from what the module header says. Comment lines are skipped — the header's table
  // of retired hatches names every one.
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphql/lossless/project.rs");
  let source = std::fs::read_to_string(path).expect("the projection's source is readable");
  let code: Vec<&str> = source
    .lines()
    .filter(|line| !line.trim_start().starts_with("//"))
    .collect();
  let count =
    |needle: &str| -> usize { code.iter().map(|line| line.matches(needle).count()).sum() };
  let word = |word: &str| -> usize {
    code
      .iter()
      .map(|line| {
        line
          .split(|c: char| !c.is_alphanumeric() && c != '_')
          .filter(|piece| *piece == word)
          .count()
      })
      .sum()
  };
  assert_eq!(count("extent.token("), 0, "a token folded by a vocabulary");
  assert_eq!(count("unread("), 0, "a child covered and not read");
  assert_eq!(word("Names"), 0, "a fixed-width name collector");
  assert_eq!(count("is_none() =>"), 0, "a slot guard");
  assert_eq!(
    code
      .iter()
      .filter(|line| line.contains("_ =>") && (line.contains("extent") || line.contains("cover")))
      .count(),
    0,
    "a wildcard arm that covers what it drops"
  );
  // The one loop over a child iterator left is the recovering door's pass over the root, which
  // counts what it skips rather than projecting a production; the cursor's trivia skip is the
  // substrate's.
  assert_eq!(
    count("for element in"),
    1,
    "a child loop outside the cursor"
  );
  assert_eq!(count("Cursor::new("), CURSORS, "the walks, one cursor each");
}

/// The walks, one cursor each — every production this file transcribes.
const CURSORS: usize = 56;

/// **Codex round 1 on al8n/smear#217/#218.** A raw kind outside this dialect's space refuses at
/// every typed door rather than panicking.
///
/// A caller mints a tree with rowan's public builder: a legal root kind, so the cast to the typed
/// wrapper succeeds, and one element whose raw kind no `SyntaxKind` names. `kind_from_raw` has no
/// fallible form and panics on it, and the hole scan the typed doors gained was the first code to
/// ask. The substrate's `reject_foreign_kinds_and_holes` now reads every kind raw first.
///
/// **No `catch_unwind` here**, deliberately: the cell calls the door in-process, so a panic is the
/// test failing, which is the regression it exists to catch. The fail-fast and recovering doors
/// take a `Parse`, which has no public constructor that skips the kind validator; they run the
/// same scan, and the cell cannot mint their input.
#[test]
fn a_raw_kind_outside_the_space_refuses_rather_than_panicking() {
  const FOREIGN: u16 = 60_000;
  let foreign = rowan::SyntaxKind(FOREIGN);
  let refusal = ProjectErrorKind::InvalidRawKind { raw: FOREIGN };
  for root in [K::Document, K::ExecutableDocument, K::TypeSystemDocument] {
    // A foreign token, and a foreign node holding a legal one.
    for as_node in [false, true] {
      let mut builder = GreenNodeBuilder::new();
      builder.start_node(GraphQLLang::kind_to_raw(root));
      if as_node {
        builder.start_node(foreign);
        builder.token(GraphQLLang::kind_to_raw(K::Name), "x");
        builder.finish_node();
      } else {
        builder.token(foreign, "x");
      }
      builder.finish_node();
      let node = SyntaxNode::new_root(builder.finish());
      let error = match root {
        K::Document => DocumentNode::cast_node(node)
          .expect("a legal root")
          .to_ast("x")
          .map(|_| ())
          .expect_err("refuses"),
        K::ExecutableDocument => ExecutableDocumentNode::cast_node(node)
          .expect("a legal root")
          .to_ast("x")
          .map(|_| ())
          .expect_err("refuses"),
        _ => TypeSystemDocumentNode::cast_node(node)
          .expect("a legal root")
          .to_ast("x")
          .map(|_| ())
          .expect_err("refuses"),
      };
      assert_eq!(error.kind(), &refusal, "{root:?}, as a node: {as_node}");
      assert_eq!(error.span(), &(0..1), "{root:?}, as a node: {as_node}");
    }
  }
}

/// A `Parse` minted through the public, generic `finish_root` from a `Cst` whose profile admits every
/// raw kind, over the root kind `root` — the one public route to this dialect's `Parse` that skips
/// its door. The
/// closure consumes nothing, so a non-empty source is tiled as one gap token under the root.
fn foreign_root_parse<'a>(src: &'a str, root: u16) -> smear::parser::graphql::lossless::Parse {
  use smear::parser::{
    graphql::lossless::{Brand, GraphqlLosslessErrors, Lexer, LexerState},
    lossless::runner::finish_root,
  };
  use tokora::{
    InputRef, SimpleSpan,
    cache::DefaultCache,
    cst::{CstProfile, KindValidator, Sink, parse_lossless},
    emitter::Verbose,
  };

  type Lx<'a> = Lexer<'a, str>;
  type Em<'a> = Verbose<GraphqlLosslessErrors<&'a str>, SimpleSpan, Brand>;
  type Ctx<'a> = (Sink<'a, Lx<'a>, Em<'a>>, DefaultCache<'a, Lx<'a>>);

  fn unmapped<T>(_: &T) -> u16 {
    0
  }

  let profile = CstProfile::new(
    unmapped as fn(&_) -> u16,
    KindValidator::accept_all(),
    GraphQLLang::kind_to_raw(K::Error).0,
    GraphQLLang::kind_to_raw(K::Gap).0,
  );
  let (cst, _) = parse_lossless::<Lx<'a>, Brand, Em<'a>, DefaultCache<'a, Lx<'a>>, (), _>(
    src,
    LexerState::default(),
    Em::new(),
    profile,
    DefaultCache::<'a, Lx<'a>>::default(),
    |_: &mut InputRef<'a, '_, Lx<'a>, Ctx<'a>, Brand>| Ok(()),
  );
  finish_root::<GraphQLLang, Lx<'a>, Em<'a>>(cst, root, "a permissive profile")
    .expect("the permissive profile admits the root")
}

/// **Codex rounds 2 and 4 on al8n/smear#217/#218.** A `Parse` whose root is not this dialect's
/// document root is refused by every projection door — never projected, never reported complete.
///
/// The public generic `finish_root` takes the root kind as an argument and checks it only against
/// the caller's profile, so it can mint this dialect's `Parse` rooted at raw 60000 (outside the
/// space) or at `Name` (inside it, and not a document). The byte comparison reads green data only,
/// and over an empty source the recovering doors answered `Recovery::new(0, 0)` — complete, over a
/// tree no door finished. Round 2 closed the out-of-space root and passed `Name`; the check is the
/// root's identity now, and both answer `WrongRoot`.
///
/// **The typed door is reached through a cast, and the cast refuses first**: `cast_node` compares
/// the raw kind with the wrapper's own, so neither root casts to a `Document`, an
/// `ExecutableDocument` or a `TypeSystemDocument`, and `to_ast` cannot be called on it. The cast
/// used to ask `SyntaxNode::kind`, which panicked on the out-of-space root. In-process, no
/// `catch_unwind`.
#[test]
fn a_parse_minted_over_a_wrong_root_is_refused_not_reported_complete() {
  use smear::parser::graphql::lossless::{
    Verified, ast::ExecutableDocument as ExecutableDocumentNode,
    ast::TypeSystemDocument as TypeSystemDocumentNode,
  };

  for root in [GraphQLLang::kind_to_raw(K::Name).0, 60_000] {
    let unverified = Unverified::WrongRoot { raw: root };
    let refusal = ProjectErrorKind::WrongRoot { raw: root };
    for src in ["", "{ f }"] {
      let parse = foreign_root_parse(src, root);
      assert_eq!(parse.green().kind().0, root, "{root} {src:?}: the premise");
      assert_eq!(
        project_executable_document_recovered(&parse, src).map(|(_, recovery)| recovery),
        Err(unverified),
        "{root} {src:?}: the executable recovering door"
      );
      assert_eq!(
        project_type_system_document_recovered(&parse, src).map(|(_, recovery)| recovery),
        Err(unverified),
        "{root} {src:?}: the type-system recovering door"
      );
      assert_eq!(
        Verified::new(&parse, src).map(|_| ()),
        Err(unverified),
        "{root} {src:?}: `Verified::new`"
      );
      assert_eq!(
        verify_parse(&parse, src),
        Err(unverified),
        "{root} {src:?}: `verify_parse`"
      );
      for (what, refused) in [
        ("project", project(&parse, src).map(|_| ()).err()),
        (
          "project_executable_document",
          project_executable_document(&parse, src).map(|_| ()).err(),
        ),
        (
          "project_type_system_document",
          project_type_system_document(&parse, src).map(|_| ()).err(),
        ),
      ] {
        assert_eq!(
          refused.map(|error| error.kind().clone()),
          Some(refusal.clone()),
          "{root} {src:?}: {what}"
        );
      }
      // The typed door: the cast answers `None` for all three wrappers, without panicking.
      let node = parse.syntax();
      assert!(
        DocumentNode::cast_node(node.clone()).is_none(),
        "{root} {src:?}"
      );
      assert!(
        ExecutableDocumentNode::cast_node(node.clone()).is_none(),
        "{root} {src:?}"
      );
      assert!(
        TypeSystemDocumentNode::cast_node(node).is_none(),
        "{root} {src:?}"
      );
    }
  }
}

/// **Codex round 6 on al8n/smear#217/#218.** `MalformedToken` is reachable from a parse, at a
/// position that reads a spelling.
///
/// `FOO` and `foo` lex as identifiers; what they are not is a directive location and a root
/// operation type. The lossless productions report both and still build the node — the
/// report-and-build table's two `MalformedToken { Name }` rows — so the variant's doc and Display
/// describe a token that cannot be read in the role its position gives it, and make no claim that
/// only a caller-minted tree reaches it.
#[test]
fn a_spelling_a_position_does_not_classify_is_malformed_in_a_parse() {
  for (src, at) in [
    ("directive @d on FOO", 16..19),
    ("schema { foo: Q }", 9..12),
  ] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src}: the parser reports it");
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: and builds the node whole"
    );
    let refusal = project(&parse, src)
      .map(|_| ())
      .expect_err("the word is not one the position classifies");
    assert_eq!(
      refusal.kind(),
      &ProjectErrorKind::MalformedToken { kind: K::Name },
      "{src}"
    );
    assert_eq!(refusal.span(), &at, "{src}: the token's own range");
    assert_eq!(
      refusal.to_string(),
      std::format!(
        "{}..{}: the Name token's text cannot be read in the role this position gives it",
        at.start,
        at.end
      ),
      "{src}"
    );
  }
}

/// One step of a tree's shape, replayed through the sink by [`twin_container_parse`].
enum Replay {
  Open(u16),
  Token,
  Close(u16),
}

/// A `Parse` minted through `finish_root` whose root holds **two** document containers of `kind`,
/// each the tree this dialect's own door builds for `one`, over the source `one` written twice.
///
/// The shape is read off a real parse of `one` and replayed through the dialect's own profile, so
/// every node and token kind is one the door itself emits; only the root's arity is the caller's.
/// `one` ends in punctuation, so its second copy lexes to the same tokens as its first and every
/// token the replay takes — trivia included — is the one the shape names.
fn twin_container_parse(one: &str, kind: K) -> (String, smear::parser::graphql::lossless::Parse) {
  use smear::parser::{
    graphql::lossless::{Brand, GraphqlLosslessErrors, Lexer, LexerState, profile},
    lossless::runner::finish_root,
  };
  use tokora::{InputRef, SimpleSpan, cache::DefaultCache, cst::Sink, emitter::Verbose};

  type Lx<'a> = Lexer<'a, str>;
  type Em<'a> = Verbose<GraphqlLosslessErrors<&'a str>, SimpleSpan, Brand>;
  type Ctx<'a> = (Sink<'a, Lx<'a>, Em<'a>>, DefaultCache<'a, Lx<'a>>);

  let raw = |kind: K| GraphQLLang::kind_to_raw(kind).0;
  let single = match kind {
    K::ExecutableDocument => parse_executable_document(one),
    _ => parse_type_system_document(one),
  };
  assert!(!single.has_errors(), "{one:?} parses clean");
  let root = single.syntax();
  let container = root
    .children()
    .find(|child| child.kind() == kind)
    .expect("the door builds its container");
  assert_eq!(
    root.children_with_tokens().count(),
    1,
    "{one:?}: nothing beside the container"
  );
  let mut shape = Vec::new();
  for event in container.preorder_with_tokens() {
    match event {
      rowan::WalkEvent::Enter(rowan::NodeOrToken::Node(node)) => {
        shape.push(Replay::Open(raw(node.kind())))
      }
      rowan::WalkEvent::Leave(rowan::NodeOrToken::Node(node)) => {
        shape.push(Replay::Close(raw(node.kind())))
      }
      rowan::WalkEvent::Enter(rowan::NodeOrToken::Token(_)) => shape.push(Replay::Token),
      rowan::WalkEvent::Leave(rowan::NodeOrToken::Token(_)) => {}
    }
  }

  let src = format!("{one}{one}");
  let parse = mint(&src, &shape);
  return (src, parse);

  fn mint<'a>(src: &'a str, shape: &[Replay]) -> smear::parser::graphql::lossless::Parse {
    let (cst, _) =
      tokora::cst::parse_lossless::<Lx<'a>, Brand, Em<'a>, DefaultCache<'a, Lx<'a>>, (), _>(
        src,
        LexerState::default(),
        Em::new(),
        profile::<str>(),
        DefaultCache::<'a, Lx<'a>>::default(),
        |inp: &mut InputRef<'a, '_, Lx<'a>, Ctx<'a>, Brand>| {
          for _ in 0..2 {
            for step in shape {
              match step {
                Replay::Open(kind) => {
                  let _ = inp.cst_start(*kind);
                }
                Replay::Token => {
                  let _ = inp.next();
                }
                Replay::Close(kind) => inp.cst_finish(*kind),
              }
            }
          }
          Ok(())
        },
      );
    finish_root::<GraphQLLang, Lx<'a>, Em<'a>>(
      cst,
      GraphQLLang::kind_to_raw(K::Root).0,
      "the dialect's profile",
    )
    .expect("the dialect's profile admits its own root")
  }
}

/// A root holding two valid document containers: the recovering doors project both, the fail-fast
/// doors refuse the second.
///
/// The two answers are different contracts, and this cell pins both. A recovering door steps
/// through **every** container of its kind under the root, and each container is a legitimate
/// document image: its definitions are projected one by one, so the [`Recovery`] is complete with
/// both halves' definitions counted. A fail-fast door asserts the root holds **exactly one**
/// container, so the second is `UnexpectedChild { parent: Root, .. }` at the second container's
/// range. A complete [`Recovery`] therefore says nothing was lost, not that the fail-fast door
/// would have answered.
#[test]
fn a_root_with_two_containers_is_recovered_whole_and_refused_fail_fast() {
  for (one, kind) in [
    ("{a}", K::ExecutableDocument),
    ("enum E{A}", K::TypeSystemDocument),
  ] {
    let (src, parse) = twin_container_parse(one, kind);
    let root = parse.syntax();
    assert_eq!(
      root.text().to_string(),
      src,
      "{kind:?}: the minted tree spells its source"
    );
    assert_eq!(
      root
        .children_with_tokens()
        .map(|e| e.kind())
        .collect::<Vec<_>>(),
      vec![kind, kind],
      "{kind:?}: the root holds two containers and nothing else"
    );
    let second = one.len()..src.len();
    let wanted = Recovery::new(2, 0);

    let (fail_fast, recovered, verified) = match kind {
      K::ExecutableDocument => (
        project_executable_document(&parse, &src).map(|_| ()),
        project_executable_document_recovered(&parse, &src)
          .map(|(ast, r)| (ast.definitions().len(), r)),
        {
          let (ast, r) = project_executable_document_verified(
            Verified::new(&parse, &src).expect("the pair verifies"),
          );
          (ast.definitions().len(), r)
        },
      ),
      _ => (
        project_type_system_document(&parse, &src).map(|_| ()),
        project_type_system_document_recovered(&parse, &src)
          .map(|(ast, r)| (ast.definitions().len(), r)),
        {
          let (ast, r) = project_type_system_document_verified(
            Verified::new(&parse, &src).expect("the pair verifies"),
          );
          (ast.definitions().len(), r)
        },
      ),
    };
    assert_eq!(recovered, Ok((2, wanted)), "{kind:?}: recovered");
    assert!(wanted.is_complete());
    assert_eq!(verified, (2, wanted), "{kind:?}: verified");
    let refusal = fail_fast.expect_err("the fail-fast door asserts one container");
    assert_eq!(
      refusal.kind(),
      &ProjectErrorKind::UnexpectedChild {
        parent: K::Root,
        found: kind,
      },
      "{kind:?}: fail-fast"
    );
    assert_eq!(refusal.span(), &second, "{kind:?}: at the second container");
  }
}
