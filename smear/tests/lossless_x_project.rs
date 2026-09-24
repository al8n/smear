#![cfg(all(feature = "graphqlx", feature = "rowan"))]

//! The differential gate for the GraphQLx CST → AST projection (issue #58).
//!
//! # The claim, and the instrument
//!
//! `project(parse_document(src), src) == document(src)` — the projection of a lossless parse is
//! **the same AST value** the syntactic parser builds for the same bytes. Not "the same shape",
//! not "the same modulo spans": the [`PartialEq`] this issue derived across the GraphQLx AST
//! closure compares every span, every slice, every literal payload, every `Option` presence and
//! every container order, so the assertion is `assert_eq!` and nothing is discounted.
//!
//! # Why plain `==`, and why the design said otherwise
//!
//! The design for this work (issue #58's comment, written against trunk at `69fd677`) measured the
//! syntactic AST's composite spans as **lookahead artifacts** and worked around them with a
//! `normalise_spans` pass, a drift ledger and a self-retirement clause. **Issue #68 closed that**,
//! and closed it three times harder here than in the vanilla dialect: eleven GraphQL node types
//! closed a span on a lookahead position and **thirty-three** GraphQLx ones did, because every
//! construct this dialect adds is a production that peeks past its own tail.
//! `tests/syntactic_x_span_extent.rs` pins the converged rule over this very corpus, padded with
//! this very alphabet.
//!
//! So there is no normalisation pass in this file, no drift ledger and no self-retirement test,
//! because there is no residue for them to measure. What replaces them is
//! [`the_span_rule_the_normaliser_would_have_hidden`], which measures the residue directly on the
//! probe the design used: if a composite span ever drifts back off its token extent, that test
//! names the node and the offset instead of a wall of `assert_eq!` diffs.
//!
//! # The three ways a gate like this passes without meaning anything
//!
//! 1. **Equality that ignores what matters.** If `PartialEq` compared shapes only, every assertion
//!    below would hold over a projection that got every span wrong.
//!    [`the_equality_can_answer_no`] feeds it a document shifted one byte, and a second document
//!    with the same shape and a different name, and requires both to compare unequal.
//! 2. **A projection that re-parses the text.** `tree.text() == source` always holds, so
//!    `|parse, src| syntactic_document(src)` would satisfy every corpus assertion here.
//!    [`a_projection_that_re_parsed_the_source_would_fail_this`] builds a **synthetic green tree**
//!    whose text re-parses to a different structure and requires the projection to answer from the
//!    structure it was handed.
//! 3. **Error paths nobody reaches.** [`every_refusal_kind_has_a_witness`] requires each
//!    [`ProjectErrorKind`] variant to be produced by at least one pinned input.
//!
//! # The corpus, twice
//!
//! Every `valid_` entry in `tests/corpusx/`, compact and then padded at every token boundary with
//! each of `tests/support/span_extent.rs`'s eight ignorable forms. The padded half is not
//! decoration: on compact input a projection that used [`rowan::SyntaxNode::text_range`] instead of
//! the token extent would agree with the parser everywhere, because with no trivia the two rules
//! coincide.
//!
//! # The shapes this dialect has and the other does not
//!
//! Five families get a witness of their own below, because each is a *mapping* divergence rather
//! than a renaming and an aggregate sweep would report only that two large values differ:
//! [`a_definitions_name_is_a_node_and_not_a_token_at_an_index`],
//! [`a_description_is_a_token_of_the_definition_it_precedes`],
//! [`a_map_entrys_two_halves_are_told_apart_by_their_order`],
//! [`a_map_types_two_halves_are_told_apart_by_their_order`] and
//! [`a_where_predicate_is_a_constrained_type_and_then_its_bounds`].

use std::{
  collections::BTreeSet,
  path::{Path, PathBuf},
};

use rowan::{GreenNodeBuilder, Language};
use smear::parser::{
  graphqlx::{
    GraphQLx,
    ast::{Document, ExecutableDocument, TypeSystemDocument},
    error::GraphqlxErrors,
    kinds::{GraphQLxLang, SyntaxKind as K},
    lossless::{
      ProjectErrorKind, Recovery, SyntaxNode, Unverified, ast::Document as DocumentNode,
      ast::ExecutableDocument as ExecutableDocumentNode,
      ast::TypeSystemDocument as TypeSystemDocumentNode, parse_document, parse_executable_document,
      parse_type_system_document, project, project_executable_document,
      project_executable_document_recovered, project_type_system_document,
      project_type_system_document_recovered, verify_parse,
    },
    syntactic::{GraphqlxLexer, document, executable_document, type_system_document},
  },
  lossless::ast::CastNode,
};
use tokora::{Parse as _, Parser};

// The span-extent support module, shared with `syntactic_x_span_extent.rs`. This gate reads only
// its alphabet, its injector and its `Debug` walk — the four-part checker and the discriminating
// classifier are that gate's business — so the unused half would be `dead_code` denials under CI's
// `-Dwarnings`. Allowed at the include rather than at each item, which would edit a file two other
// gates own.
#[allow(dead_code)]
#[path = "support/span_extent.rs"]
mod extent;

use extent::{ALPHABET, inject};

/// The smallest number of `valid_` entries this gate is allowed to compare.
///
/// The measurement on the day it was written, as a floor. A corpus that shrank below it is a gate
/// that stopped covering what it claims to. Eighty-nine against the vanilla dialect's fifty-six,
/// which is why the padded sweep below runs 801 comparisons at the mixed root alone — 712 padded
/// and 89 compact.
///
/// **It was ninety, and the design asked for ≥ 720 padded variants.** One entry left the valid half
/// rather than the corpus: `valid_fragment_named_on.graphqlx` is
/// `invalid_fragment_named_on.graphqlx` now, because GraphQLx accepting a fragment named `on` was a
/// missing rule rather than a dialect difference (al8n/smear#58). So the padded sweep is 712, eight
/// short of the design's number, and the eight it is short of are eight paddings of a document no
/// dialect accepts. The arithmetic itself is not restated as a constant: the sweep asserts
/// `compared == entries.len() * (ALPHABET.len() + 1)` exactly, which is the live statement a
/// fitted constant would only shadow.
const VALID_ENTRY_FLOOR: usize = 89;

/// The smallest number of `invalid_` entries the refusal census runs.
///
/// Forty-five when this gate was written, and fifty-one now: one entry moved over from the valid
/// half with the fragment-name rule, and five were added for the empty required containers Codex's
/// first round found projecting `Ok` — the corpus already held four of the nine shapes and the
/// census could not see them because the invalid half makes no equality claim.
const INVALID_ENTRY_FLOOR: usize = 51;

/// The smallest number of corpus entries the **executable** root's sweep is allowed to compare.
///
/// Fewer than the mixed root's, because most of the corpus is SDL and the executable root refuses
/// it — by design, and `lossless_x_runner.rs` is where that refusal is pinned. Forty-two until the
/// fragment-name rule moved one entry to the invalid half.
const EXECUTABLE_ENTRY_FLOOR: usize = 41;

/// The smallest number of corpus entries the **type-system** root's sweep is allowed to compare.
///
/// The other side of the same split, and the larger one. The two floors do not add up to the mixed
/// root's, because an entry that mixes the two halves reaches neither single-half root.
const TYPE_SYSTEM_ENTRY_FLOOR: usize = 47;

/// The smallest number of distinct AST node types the compared documents reach.
///
/// Read off the syntactic parse's `Debug` rendering — the same total projection
/// `tests/support/span_extent.rs` walks — so a corpus that stopped reaching the imports, the
/// generics or the set/map families is a floor failure rather than a silent narrowing.
const OWNER_FLOOR: usize = 93;

// ---------------------------------------------------------------------------------------------
// harnesses
// ---------------------------------------------------------------------------------------------

/// The syntactic oracle: the shipped, fail-fast document root, exactly as `lossless_x_parity.rs`
/// and `syntactic_x_span_extent.rs` drive it.
fn oracle(src: &str) -> Result<Document<&str>, GraphqlxErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    Document<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(document)
  .parse_str(src)
}

/// The syntactic oracle for the executable-only root.
fn executable_oracle(src: &str) -> Result<ExecutableDocument<&str>, GraphqlxErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(executable_document)
  .parse_str(src)
}

/// The syntactic oracle for the SDL-only root.
fn type_system_oracle(src: &str) -> Result<TypeSystemDocument<&str>, GraphqlxErrors<&str>> {
  Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(type_system_document)
  .parse_str(src)
}

fn corpus(prefix: &str) -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the GraphQLx corpus at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
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

fn entry(name: &str) -> String {
  read_entry(
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests/corpusx")
      .join(name),
  )
  .1
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

/// The refusal kind `project` answers for `source`, at the mixed root.
fn refuse(source: &str) -> ProjectErrorKind {
  let parse = parse_document(source);
  project(&parse, source)
    .map(|_| ())
    .expect_err("the projection was expected to refuse")
    .kind()
    .clone()
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

/// `project_executable_document` is the AST the executable parser builds — and the **recovering**
/// door is the same value again whenever nothing had to be recovered.
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
        "{name} ({form}): the recovery counted a different number of entries"
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

/// `project_type_system_document` is the AST the SDL parser builds — and the **recovering** door is
/// the same value again whenever nothing had to be recovered.
#[test]
fn the_type_system_projection_equals_the_parse_over_the_shared_corpus() {
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
        "{name} ({form}): the recovery counted a different number of entries"
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

  // The payload half, over a literal whose *classification* is the only difference: `16` and `0x10`
  // are the same number in different radices, and `LitInt` keeps which. A `PartialEq` that
  // compared only the slice would still separate these two, so the third leg is the radix pair
  // below, whose slices differ by two bytes and whose values do not.
  let decimal = "type T@d(n:16){f:Int}";
  let hexadecimal = "type T@d(n:0x10){f:Int}";
  let d = project(&parse_document(decimal), decimal).expect("projects");
  let e = project(&parse_document(hexadecimal), hexadecimal).expect("projects");
  assert_ne!(d, e, "two radices of one number compare equal");

  // And a positive leg, so the control is not passing merely because everything is unequal.
  let again = project(&parse_document(compact), compact).expect("projects");
  assert_eq!(a, again, "the projection is not deterministic");
}

// ---------------------------------------------------------------------------------------------
// control 2 — the fraud model
// ---------------------------------------------------------------------------------------------

/// Build a green tree by hand, so the projection is handed a structure that does **not** match the
/// structure its own text would parse to.
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
    self.builder.start_node(GraphQLxLang::kind_to_raw(kind));
    self
  }

  fn close(&mut self) -> &mut Self {
    self.builder.finish_node();
    self
  }

  fn token(&mut self, kind: K, text: &str) -> &mut Self {
    self.builder.token(GraphQLxLang::kind_to_raw(kind), text);
    self
  }

  /// `Path > Name`, the shape every name-shaped position in this dialect goes through.
  fn path(&mut self, name: &str) -> &mut Self {
    self.open(K::Path).token(K::Name, name).close()
  }

  /// `DefinitionTypePath > Path > Name` — a type reference to a bare name.
  fn named_type(&mut self, name: &str) -> &mut Self {
    self.open(K::DefinitionTypePath).path(name).close()
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

  /// `Directives > Directive > [TypePath, Arguments > Argument > <value>]`, over one argument whose
  /// value node the caller opens. The smallest shape that puts a value in a document.
  fn one_argument(&mut self, name: &str, value: impl FnOnce(&mut Self)) -> &mut Self {
    self.open(K::Directives);
    self.open(K::Directive);
    self.token(K::At, "@");
    self.open(K::TypePath).path("d").close();
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

/// `scalar S@d(n:<literal>)`, with the literal claimed as `kind`.
///
/// The smallest document that carries one numeric literal, so a caller-minted pair can put a
/// spelling in front of the projection that the scanner will not read back.
fn scalar_with_literal(node: K, token: K, literal: &str) -> (SyntaxNode, String) {
  let text = std::format!("scalar S@d(n:{literal})");
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "S").close();
  tree.one_argument("n", |tree| {
    tree.open(node).token(token, literal).close();
  });
  tree.close();
  tree.close();
  (tree.finish(), text)
}

/// One selection moved **out** of the set that holds it in the text.
///
/// The text is `{a{b c}`, which the parser reads as one field `a` selecting `b` and `c`, with the
/// outer set's `}` missing. This tree closes nothing where the text closes nothing either, but it
/// groups differently: `a`'s set holds only `b` — its `}` is the lenient one now — and `c` is a
/// second top-level selection, whose set the one `}` closes. Every token is consumed by the
/// production of the node that holds it, so the tree is well-shaped, and the projection has to
/// believe it: a walk answers two top-level selections, and anything that re-derived the
/// structure from the bytes answers one.
///
/// **Why this shape and not the one it replaces.** Through al8n/smear#58's fifth round this was one
/// [`ListType`](SyntaxKind::ListType) holding both brackets of `[[Int]]`, which projected because a
/// type wrapper's walk folded any number of its own brackets. Round six transcribed that walk to
/// `[ Type ] !?`, and the second `[` is now refused where it stands — the control has to diverge
/// from the text through a grouping the productions *do* spell, and the lenient closer is one.
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

/// The described-probe shape, with the description hung as a **sibling** of the definition rather
/// than as its own token.
///
/// Its text is `"d" type T{f:Int}`, which the parser attaches — producing
/// `Described { description: Some(_), … }`. Under this tree the string is loose under the document,
/// which is rubble the walk has no place for. It is also this dialect's description shape stated
/// twice over: the string is a **token** either way, and what the hoist turns on is whose token it
/// is.
fn description_as_sibling() -> (SyntaxNode, &'static str) {
  let text = "\"d\" type T{f:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.token(K::InlineString, "\"d\"").token(K::Space, " ");
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
  tree.fields("f", "Int");
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
    .expect_err("a string loose under the document is rubble, not a definition")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::Document,
      found: K::InlineString,
    },
    "the walk names the loose token where it sits"
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
// the span rule #68 established, re-measured for this dialect
// ---------------------------------------------------------------------------------------------

#[test]
fn the_span_rule_the_normaliser_would_have_hidden() {
  // The design's M-A probe, in this dialect's spelling. Under the rule it measured, `Document`
  // would open before its leading trivia and a type reference would close at the *next* token's
  // start; under the rule #68 converged on, both are token extents. If a composite span ever
  // drifts back, this names the node and the offset instead of two large values differing.
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

  assert_eq!(
    of("Document"),
    (2, 21),
    "the document opens on its first token"
  );
  assert_eq!(
    of("DefinitionTypePath"),
    (16, 19),
    "a path type closes at the end of its own name, not at the next token's start"
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
fn a_definitions_name_is_a_node_and_not_a_token_at_an_index() {
  // GraphQL puts the keyword and the name under one node as two `Name` tokens and reaches the
  // second by index. Here the keyword is the definition's only direct `Name` token and the name is
  // a `DefinitionName` child that also holds whatever generics follow it.
  let src = "type T<A = Int> implements I { f: A }";
  let parse = parse_document(src);
  let definition = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ObjectTypeDefinition)
    .expect("an object type definition node");
  assert_eq!(
    definition
      .children_with_tokens()
      .filter(|element| element.kind() == K::Name)
      .count(),
    1,
    "the definition's only direct `Name` token is its keyword; a positional getter would be \
     reading the wrong thing"
  );
  let name = definition
    .children()
    .find(|child| child.kind() == K::DefinitionName)
    .expect("the name is a node");
  assert_eq!(
    name.text().to_string(),
    "T<A = Int>",
    "the generics are inside the name node, not beside it"
  );

  let projected = project(&parse, src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

  // And the AST's name carries the same region, which is what a bare-token projection would lose.
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
  assert_eq!(
    (object.name().span().start(), object.name().span().end()),
    (5, 15),
    "the projected name spans `T<A = Int>`"
  );
  assert!(
    object.name().generics().is_some(),
    "the declared parameter is inside the name"
  );
}

#[test]
fn a_description_is_a_token_of_the_definition_it_precedes() {
  let src = "\"doc\" type T { f: Int }";
  let parse = parse_document(src);

  // This kind space has no `Description` node at all, and the tree keeps the string inside the
  // definition it precedes.
  assert!(
    !parse
      .syntax()
      .descendants()
      .any(|node| format!("{:?}", node.kind()) == "Description"),
    "a Description node appeared in a kind space that has none"
  );
  let definition = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ObjectTypeDefinition)
    .expect("an object type definition node");
  assert_eq!(usize::from(definition.text_range().start()), 0);
  assert!(
    definition
      .children_with_tokens()
      .any(|element| element.kind() == K::InlineString),
    "the CST hangs the description inside the definition, as a token"
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
  let field = &fields.target().field_definitions()[0];
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
fn a_variable_definition_hoists_where_a_field_definition_does_not() {
  // The fourth described node below document level, and the one that follows the *document*
  // rule — so the four do not agree with each other and this pins which side each is on.
  let src = "query Q(\"vd\" $v: Int) { f }";
  let projected = project(&parse_document(src), src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

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
  let variables = operation
    .variable_definitions()
    .expect("a variables definition");
  let variable = &variables.variable_definitions()[0];
  assert_eq!(
    (variable.span().start(), variable.span().end()),
    (8, 20),
    "the wrapper covers the description"
  );
  assert_eq!(
    (variable.node().span().start(), variable.node().span().end()),
    (13, 20),
    "and the inner definition starts after it, unlike a field definition's"
  );
}

#[test]
fn a_map_entrys_two_halves_are_told_apart_by_their_order() {
  // One child stream, two subtrees, and nothing but the `=>` between them: the typed wrapper layer
  // has to expose plural getters here and the projection has to read positionally.
  let src = "type T @d(m: map { 1 => 2 }) { f: Int }";
  let parse = parse_document(src);
  let entry = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::MapEntry)
    .expect("a map entry node");
  assert_eq!(
    entry
      .children()
      .filter(|child| child.kind() == K::IntValue)
      .count(),
    2,
    "both halves are the same kind, which is exactly why an `opt` getter cannot tell them apart"
  );

  let projected = project(&parse, src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

  let value = const_argument_value(&projected);
  let map = value.try_unwrap_map_ref().expect("a map value");
  let entry = &map.entries()[0];
  assert_eq!(
    (
      entry
        .key()
        .try_unwrap_int_ref()
        .expect("an int")
        .span()
        .start(),
      entry
        .value()
        .try_unwrap_int_ref()
        .expect("an int")
        .span()
        .start()
    ),
    (19, 24),
    "the key is the first half and the value the second; swapping them would still be two ints"
  );
}

#[test]
fn a_map_types_two_halves_are_told_apart_by_their_order() {
  let src = "type T { f: <Int => String> }";
  let parse = parse_document(src);
  let map = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::MapType)
    .expect("a map type node");
  assert_eq!(
    map
      .children()
      .filter(|child| child.kind() == K::DefinitionTypePath)
      .count(),
    2,
    "both halves are the same kind"
  );

  let projected = project(&parse, src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

  let ty = first_field_type(&projected);
  let map = ty.try_unwrap_map_ref().expect("a map type");
  assert_eq!(
    (map.key().span().start(), map.value().span().start()),
    (13, 20),
    "the key is the first half and the value the second"
  );
}

#[test]
fn a_where_predicate_is_a_constrained_type_and_then_its_bounds() {
  // The third two-subtree carrier, and the widest: every half is a `TypePath`, so the constrained
  // type is the first and everything after it is a bound.
  let src = "type T<A> where A: Node & Other { f: A }";
  let parse = parse_document(src);
  let predicate = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::WherePredicate)
    .expect("a where predicate node");
  assert_eq!(
    predicate
      .children()
      .filter(|child| child.kind() == K::TypePath)
      .count(),
    3,
    "three type paths under one node, and only their order says which is which"
  );

  let projected = project(&parse, src).expect("projects");
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
  let fields = object
    .fields_definition()
    .expect("a constrained fields block");
  let clause = fields.where_clause().expect("a where clause");
  let predicate = &clause.predicates()[0];
  assert_eq!(
    (
      predicate.bounded_type().span().start(),
      predicate.bounds().len()
    ),
    (16, 2),
    "the constrained type is the first path and the two after it are its bounds"
  );
  assert_eq!(
    (fields.span().start(), fields.target().span().start()),
    (10, 32),
    "the constrained wrapper opens on the `where` and the block it constrains starts later"
  );
}

#[test]
fn the_bang_is_a_token_of_the_type_it_modifies_and_not_a_wrapper() {
  // GraphQL's kind space has a `NonNullType` node; this one has none, so `[Int!]!` puts one `!`
  // under the `ListType` and one under the `DefinitionTypePath` inside it.
  let src = "type T { f: [Int!]! }";
  let parse = parse_document(src);
  let list = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ListType)
    .expect("a list type node");
  assert_eq!(
    list
      .children_with_tokens()
      .filter(|element| element.kind() == K::Bang)
      .count(),
    1,
    "the list's own `!` is its direct token; the element's belongs to the element"
  );
  assert_eq!(
    project(&parse, src).expect("projects"),
    oracle(src).expect("parses")
  );
}

#[test]
fn an_interface_list_holds_type_paths_and_a_directive_name_is_one() {
  // Two widenings at once, and both would compile as a port of GraphQL's shape while answering
  // `None` or the wrong node forever.
  let src = "type T implements ns::I<Int> & J @ns::d(a: 1) { f: Int }";
  let parse = parse_document(src);
  let clause = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::ImplementInterfaces)
    .expect("an implements clause");
  assert_eq!(
    clause
      .children()
      .filter(|child| child.kind() == K::TypePath)
      .count(),
    2,
    "the tree wraps each interface in a TypePath"
  );
  let directive = parse
    .syntax()
    .descendants()
    .find(|node| node.kind() == K::Directive)
    .expect("a directive");
  assert!(
    directive
      .children()
      .any(|child| child.kind() == K::TypePath),
    "even a one-segment directive name nests Directive > TypePath > Path"
  );
  assert_eq!(
    project(&parse, src).expect("projects"),
    oracle(src).expect("parses")
  );
}

#[test]
fn a_written_down_empty_argument_list_is_none_with_a_span() {
  // `()` is a real node in the tree and `None` in the AST, and the field's span still covers it.
  // A projection that answered `Some(<empty>)` would compare unequal; one that dropped the extent
  // would answer a shorter field.
  let src = "query Q { f() }";
  let parse = parse_document(src);
  assert!(
    parse
      .syntax()
      .descendants()
      .any(|node| node.kind() == K::Arguments),
    "the tree opens an Arguments node for the written-down `()`"
  );
  let projected = project(&parse, src).expect("projects");
  assert_eq!(projected, oracle(src).expect("parses"));

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
  let set = operation.selection_set().target();
  let field = set.selections()[0].try_unwrap_field_ref().expect("a field");
  assert!(field.arguments().is_none(), "an empty `()` is `None`");
  assert_eq!(
    (field.span().start(), field.span().end()),
    (10, 13),
    "and the field's span still covers the parentheses"
  );

  // The **const** argument list is a separate reader with the same rule, and the four containers
  // that are written down empty are four more. None of them is in the corpus, and each is a place
  // an `Optional` that dropped its extent — or kept its emptiness — would answer a different
  // value from the parser's.
  for what in [
    "type T @d() { f: Int }",
    "query Q { f @d() }",
    "type T @d(a: []) { f: Int }",
    "type T @d(a: {}) { f: Int }",
    "type T @d(a: set { }) { f: Int }",
    "type T @d(a: map { }) { f: Int }",
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

#[test]
fn every_integer_radix_and_both_float_radices_survive_the_projection() {
  // The payloads that had no re-cooking door until al8n/smear#58 gave `smear-lexer` one. Each radix
  // still gets a direct witness rather than resting on whichever ones the corpus happens to hold —
  // what changed is that the classification is the scanner's, so these assert the door reads back
  // what the parser was handed rather than that a prefix test agrees with itself.
  for (what, literal) in [
    ("decimal", "16"),
    ("hexadecimal", "0x10"),
    ("octal", "0o20"),
    ("binary", "0b10000"),
    ("negative decimal", "-16"),
    ("negative hexadecimal", "-0x10"),
    ("decimal float", "1.5e3"),
    ("hexadecimal float", "0x1.8p3"),
    ("negative hexadecimal float", "-0x1.8p3"),
  ] {
    let src = format!("type T @d(n: {literal}) {{ f: Int }}");
    let parse = parse_document(&src);
    assert!(!parse.has_errors(), "{what}: the lossless parse rejects it");
    let expected = oracle(&src).unwrap_or_else(|e| panic!("{what}: the parser rejects it: {e:?}"));
    assert_eq!(
      project(&parse, &src).unwrap_or_else(|e| panic!("{what}: the projection refused: {e}")),
      expected,
      "{what}: the projected literal payload is not the one the lexer handed the parser"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// the seven findings of Codex round 1, one cell each, before and after
// ---------------------------------------------------------------------------------------------

/// **Finding 1.** A present-but-empty container the grammar writes `+` has no AST value, so it is
/// a refusal and not an empty carrier.
///
/// # Before
///
/// All nine projected `Ok`. Each of these documents parses losslessly **with a diagnostic** and is
/// rejected by the syntactic parser, and the tree the lossless parser leaves carries **no `Error`
/// child** — the production reports and still builds the node — so the hole scan had nothing to
/// refuse and the walk read a container with no members as a container with no members. The value
/// it built is one the syntactic parser cannot produce for any input, which is the case the
/// design's contract forbids in so many words.
///
/// # After
///
/// [`MissingChild`](ProjectErrorKind::MissingChild) naming the container and what it wanted. The
/// three rows of the rule are in the projection's module header; the other two are pinned by
/// [`a_written_down_empty_argument_list_is_none_with_a_span`] and by every absent optional in the
/// corpus sweep.
#[test]
fn a_present_but_empty_required_container_refuses() {
  for (src, parent, wanted) in [
    ("import {} from \"m\"", K::ImportList, "an import member"),
    (
      "type T<> { f: Int }",
      K::DefinitionTypeGenerics,
      "a generic parameter",
    ),
    ("query Q { }", K::SelectionSet, "a selection"),
    (
      "query Q() { f }",
      K::VariablesDefinition,
      "a variable definition",
    ),
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
      K::RootOperationTypesDefinition,
      "a root operation type",
    ),
  ] {
    let parse = parse_document(src);

    // The premise, measured rather than asserted from the shape of the source. Gate 1 is intact —
    // the lossless parser reports and the syntactic parser rejects — and the tree is
    // shape-complete, which is exactly why the hole scan cannot see this class.
    assert!(
      parse.has_errors(),
      "{src}: the lossless parser accepts it, which would be a parity defect in the parser rather \
       than a projection one — report it, do not paper over it here"
    );
    assert!(
      oracle(src).is_err(),
      "{src}: the syntactic parser accepts it, so there is a value to project to after all"
    );
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: the tree carries a hole, so this entry is refused by the preflight and proves \
       nothing about the cardinality rule"
    );
    assert!(
      parse
        .syntax()
        .descendants()
        .any(|node| node.kind() == parent),
      "{src}: the tree does not even hold a {parent:?}, so the pin below is aimed at nothing"
    );

    assert_eq!(
      refuse(src),
      ProjectErrorKind::MissingChild { parent, wanted },
      "{src}"
    );
  }
}

/// **Finding 2.** A gap tile is a **token**, and the substrate's hole scan tested node kinds only.
///
/// # Before
///
/// `reject_holes` discarded every token, so `scan_holes` declared a tree free of holes while it
/// held one; the projection's own walk then folded the gap's bytes into the enclosing node's extent
/// as an ordinary non-trivia token and answered `Ok`. Both dialects passed `Error | Gap` to that
/// walker and both spell `Gap` as a token, so both carried a comment saying the arm was dead — it
/// was, and what it was dead about was the shape the parser produces today.
///
/// # After
///
/// [`UnexpectedChild`](ProjectErrorKind::UnexpectedChild) naming the gap's parent and its exact
/// range. The fix is in `crate::lossless::project::reject_holes`, so the vanilla dialect gets it
/// too — `lossless_project.rs` carries the same witness.
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
    (
      "inside an argument list",
      "type T @d(a: 1 %) { f: Int }",
      K::Arguments,
      15..16,
    ),
    (
      "inside a list value",
      "type T @d(a: [1 %]) { f: Int }",
      K::ListValue,
      16..17,
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

  // A gap the parser leaves beside the *document node* is a child of `Root`, which is the shape
  // smear #57 produces and the reason the scan starts at the root rather than at the document.
  let parse = parse_executable_document("%");
  let refusal = project_executable_document(&parse, "%")
    .map(|_| ())
    .expect_err("a gap under the root is still a hole");
  assert_eq!(
    refusal.kind(),
    &ProjectErrorKind::UnexpectedChild {
      parent: K::Root,
      found: K::Gap,
    }
  );
}

/// **Finding 3.** The typed doors scan their own subtree.
///
/// # Before
///
/// They ran the source verification and no hole scan, on the claim that the walk would reach any
/// hole inside the subtree. The claim is false: the walk's permissive arms route a child they have
/// no slot for through `Extent::unread`, which folds its bytes into the parent's span and never
/// looks at its kind. A `MapType` with an `Error` child after its two halves is the shortest
/// witness, and it projected `Ok`.
///
/// # After
///
/// A subtree-scoped, token-aware `scan_holes` after `open_node` in all three typed doors.
#[test]
fn a_typed_door_scans_its_own_subtree_for_holes() {
  // `type T{f:<Int=>Str junk>}` — both halves of the map type are there and well-shaped, and the
  // `Error` node after them is what the walk has no slot for.
  let text = "type T{f:<Int=>Str junk>}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.open(K::MapType);
  tree.token(K::LAngle, "<");
  tree.named_type("Int");
  tree.token(K::FatArrow, "=>");
  tree.named_type("Str");
  tree.token(K::Space, " ");
  tree.open(K::Error).token(K::Name, "junk").close();
  tree.token(K::RAngle, ">");
  tree.close();
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
    .expect_err("an Error node inside the subtree is a region with no AST image")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::MapType,
      found: K::Error,
    },
    "the scan refuses the hole before any walk — and since al8n/smear#58's sixth round the \
     transcribed `MapType` walk would refuse it too, at the same element"
  );
}

/// **Finding 4.** The root's shape is asserted rather than searched.
///
/// # Before
///
/// Each fail-fast door selected the first child of the wanted kind. A root holding a valid document
/// **and** a second container, or another node, or a bare token would verify byte for byte — the
/// verification compares bytes and every one of those bytes is in the tree — and project to an AST
/// omitting the sibling.
///
/// # After
///
/// `sole_document` requires one container of the wanted kind plus trivia and refuses anything else
/// with `UnexpectedChild { parent: Root, found }`, duplicates included.
///
/// # What this cell can reach, measured rather than assumed
///
/// **Nothing adversarial.** Over the whole corpus, at all three roots, the root holds exactly one
/// container and *no tokens at all* — the container's own extent covers the file, leading and
/// trailing trivia included. So the parser reaches neither the refusal arms nor even the trivia
/// arm, and the only adversarial root a public API produces — a gap tile beside an empty document
/// node — is refused by the hole scan before this check runs and is pinned by
/// [`a_gap_token_is_a_hole_the_scan_sees`] instead.
///
/// A `Parse` has **no public constructor** other than `finish_root`, which takes a tokora `Cst` a
/// parse produces, so this suite cannot mint the offending tree. The check is therefore preventive
/// against a caller of `finish_root`, removing it reds nothing here, and what this cell owns is the
/// measurement that says so — a claim about reachability, held every run, rather than a sentence in
/// a comment.
#[test]
fn the_root_shape_is_asserted_rather_than_searched() {
  let mut roots = 0usize;
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
      assert_eq!(
        children,
        vec![kind],
        "{name} ({what}): the root holds something other than one container, which is the shape \
         `sole_document` was written for — if the parser has started producing it, this cell is \
         the wrong instrument and the refusal arms need a direct witness"
      );
      roots += 1;
    }
  }
  assert!(
    roots >= 3 * VALID_ENTRY_FLOOR,
    "only {roots} roots measured"
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

/// **Finding 5.** A fragment whose name precedes its keyword refuses rather than panicking.
///
/// # Before
///
/// The keyword came from the token stream and the name from the child stream, and the header's span
/// was `SimpleSpan::new(keyword.start, name.end)` — which panics when the end precedes the start. A
/// safe public door that answers `ProjectError` for every other malformed tree aborted for this
/// one.
///
/// # After
///
/// The order is checked and the answer is the refusal every other misplaced child gets. Checked
/// rather than clamped: a clamp would hand back a span for a tree the AST has no image for.
#[test]
fn a_fragment_whose_name_precedes_its_keyword_refuses_rather_than_panicking() {
  let text = "F fragment on T{f}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::FragmentDefinition);
  tree
    .open(K::ExecutableDefinitionName)
    .token(K::Name, "F")
    .close();
  tree.token(K::Space, " ");
  tree.token(K::Name, "fragment").token(K::Space, " ");
  tree.open(K::TypeCondition);
  tree.token(K::Name, "on").token(K::Space, " ");
  tree.open(K::TypePath).path("T").close();
  tree.close();
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::Field).token(K::Name, "f").close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);

  let document = DocumentNode::cast_node(node).expect("a Document root");
  // `to_ast` is a safe public entry point: the answer has to be a value, and an unwinding panic
  // here would take the harness with it rather than being caught by this assertion.
  let kind = document
    .to_ast(text)
    .map(|_| ())
    .expect_err("a name in front of the keyword is not a shape the AST holds")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::FragmentDefinition,
      found: K::ExecutableDefinitionName,
    }
  );
}

/// **Finding 6.** A numeric spelling the scanner will not read back does not project.
///
/// # Before
///
/// `int_lit` and `float_lit` inspected a prefix — `0x`, `0o`, `0b`, or a `p` for a hex float — and
/// always returned a variant. A caller-minted pair whose tree claims `0b2` as an `Int`, or `0x1` as
/// a `Float`, projected to an invalid or wrongly classified AST literal.
///
/// # After
///
/// `smear-lexer` exposes the whole-slice doors `LitInt::try_from` and `LitFloat::try_from`, which
/// **are** the scanner, and the projection answers
/// [`MalformedToken`](ProjectErrorKind::MalformedToken) when the slice does not read back. The
/// door's own cells are in `smear-lexer/src/graphqlx/number/tests.rs`.
#[test]
fn a_numeric_spelling_the_scanner_will_not_read_back_refuses() {
  for (what, node, token, literal) in [
    ("a binary digit that is not one", K::IntValue, K::Int, "0b2"),
    ("a radix prefix with no digits", K::IntValue, K::Int, "0x"),
    (
      "an exponent with no mantissa radix",
      K::FloatValue,
      K::Float,
      "1p2",
    ),
    (
      "an integer claimed as a float",
      K::FloatValue,
      K::Float,
      "0x1",
    ),
    ("a float claimed as an integer", K::IntValue, K::Int, "1.5"),
  ] {
    let (tree, text) = scalar_with_literal(node, token, literal);
    assert_eq!(tree.text().to_string(), text, "{what}");
    let document = DocumentNode::cast_node(tree).expect("a Document root");
    let kind = match document.to_ast(&text) {
      Ok(_) => panic!("{what}: `{literal}` projected to a literal"),
      Err(refusal) => refusal.kind().clone(),
    };
    assert_eq!(
      kind,
      ProjectErrorKind::MalformedToken { kind: token },
      "{what}"
    );
  }

  // `007` is on the reported list and is **not** refused, because the grammar admits it: the
  // decimal production is `-?(?&digit)[0-9_]*`, so a leading zero is a perfectly good GraphQLx
  // decimal. The door is the scanner, so it agrees with the parser — which is the whole point of
  // routing through it rather than through a hand-written table of what looks wrong.
  let leading_zeros = "type T @d(n: 007) { f: Int }";
  let parse = parse_document(leading_zeros);
  assert!(!parse.has_errors(), "the lossless parser accepts `007`");
  assert_eq!(
    project(&parse, leading_zeros).expect("projects"),
    oracle(leading_zeros).expect("parses"),
    "`007` is a decimal integer in this grammar and the projection has to say what the parser says"
  );
}

/// **Finding 7.** The recovering doors verify the pair once, at the door.
///
/// # Before
///
/// Both recovering paths establish the pair over the whole root — `recovered_top_level` with
/// `verify_source`, and the `_verified` doors through the [`Verified`] they are handed — and each
/// entry callback then called `open_node` and re-compared the entry's own bytes. `O(source)` per
/// entry over bytes a single pass had already compared.
///
/// # After
///
/// The entry callbacks do the subtree hole scan and the projection. Nothing observable changed,
/// which is the claim: the property the removed check was thought to carry is the door's, and this
/// cell is what says so — a mismatched pair is refused **before** any entry is projected, and the
/// verified door and the fallible one still agree entry for entry.
#[test]
fn the_recovering_door_verifies_the_pair_once_at_the_door() {
  use smear::parser::graphqlx::lossless::{
    Verified, project_executable_document_verified, project_type_system_document_verified,
  };

  let executable = "{ hero { name } }
query Q { hero { id } }";
  let parse = parse_executable_document(executable);
  assert!(!parse.has_errors());

  // The door's own check is the only one, and it is the one that refuses.
  assert_eq!(
    project_executable_document_recovered(&parse, "{ hero { name } }")
      .map(|(ast, _)| ast.definitions().len())
      .map_err(|e| e.to_string()),
    Err("the parse and the source are not the same document".to_owned()),
    "a mismatched pair has to be refused at the door, not counted as skipped entries"
  );

  // And the two doors agree, which is what the per-entry re-verification could only have confirmed.
  let (fallible, fallible_recovery) =
    project_executable_document_recovered(&parse, executable).expect("the pair matches");
  let pair = Verified::new(&parse, executable).expect("the pair matches");
  let (verified, verified_recovery) = project_executable_document_verified(pair);
  assert_eq!(fallible, verified);
  assert_eq!(fallible_recovery, verified_recovery);
  assert_eq!(fallible_recovery.projected(), 2);
  assert!(fallible_recovery.is_complete());

  // The SDL root is the same walk with a different root kind.
  let sdl = "type T { f: Int }
type U { g: Int }";
  let parse = parse_type_system_document(sdl);
  let (fallible, _) = project_type_system_document_recovered(&parse, sdl).expect("matches");
  let pair = Verified::new(&parse, sdl).expect("matches");
  let (verified, recovery) = project_type_system_document_verified(pair);
  assert_eq!(fallible, verified);
  assert_eq!(recovery.projected(), 2);
}

// ---------------------------------------------------------------------------------------------
// refusals
// ---------------------------------------------------------------------------------------------

#[test]
fn the_lost_node_class_refuses() {
  // `invalid_top_level_junk`: bytes a failed definition left as rubble under the document.
  let kind = refuse(&entry("invalid_top_level_junk.graphqlx"));
  assert!(
    matches!(
      kind,
      ProjectErrorKind::UnexpectedChild {
        parent: K::Document,
        ..
      }
    ),
    "expected an UnexpectedChild at Document, got {kind:?}"
  );
}

#[test]
fn the_recovered_in_place_class_refuses() {
  // `type T { f: }` keeps its field-definition node and hangs an `Error` hole where the type
  // should be. The hole is the refusal.
  let kind = refuse(&entry("invalid_missing_field_type.graphqlx"));
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
fn a_variable_in_a_constant_position_refuses_through_both_new_containers() {
  // The AST's own type system forbids it: `ConstInputValue` has no `Variable` variant. This
  // dialect reaches the refusal through two containers GraphQL does not have.
  for (name, parent) in [
    (
      "invalid_x_variable_in_set_const_position.graphqlx",
      K::SetValue,
    ),
    (
      "invalid_x_variable_in_map_const_position.graphqlx",
      K::MapEntry,
    ),
  ] {
    let src = entry(name);
    let parse = parse_document(&src);
    assert!(
      parse
        .syntax()
        .descendants()
        .any(|node| node.kind() == K::VariableValue),
      "{name}: the tree is expected to keep the variable node; if it stopped, this pin moved"
    );
    assert_eq!(
      refuse(&src),
      ProjectErrorKind::UnexpectedChild {
        parent,
        found: K::VariableValue,
      },
      "{name}"
    );
  }

  // And the plain argument position, which is the shape GraphQL has too.
  let src = "type T @d(a: $v) { f: Int }";
  assert_eq!(
    refuse(src),
    ProjectErrorKind::UnexpectedChild {
      parent: K::Argument,
      found: K::VariableValue,
    }
  );
}

#[test]
fn an_enum_value_named_true_refuses() {
  // This dialect's one semantic rule, and the place it differs from the vanilla dialect's: GraphQLx
  // has no fragment-name exclusion at all, and this is the rule it has instead.
  let kind = refuse(&entry("invalid_enum_reserved_spelling.graphqlx"));
  assert_eq!(
    kind,
    ProjectErrorKind::SemanticRule {
      rule: "an enum value may not be named `true`, `false` or `null`",
    },
    "the lossless production records the violation as a diagnostic and still builds the node, so \
     the shape alone cannot tell a legal declaring name from an illegal one"
  );
}

#[test]
fn a_fragment_named_on_refuses() {
  // **Before al8n/smear#58 this cell asserted the opposite**, and named it a dialect difference:
  // both GraphQLx suites accepted `fragment on on T { f }` and the projection answered the AST the
  // parser built for it. It is not a difference — GraphQLx forbids the spelling exactly as GraphQL
  // does — so the rule was given a home at the same three custody points the vanilla dialect uses,
  // and this is the third of them.
  let src = &entry("invalid_fragment_named_on.graphqlx");
  let parse = parse_document(src);
  assert!(
    parse.has_errors(),
    "the lossless production reports the excluded spelling"
  );
  assert!(oracle(src).is_err(), "and the syntactic parser rejects it");
  assert_eq!(
    refuse(src),
    ProjectErrorKind::SemanticRule {
      rule: "a fragment may not be named `on`",
    },
    "the lossless production reports the violation and still builds the node, so the shape alone \
     cannot tell a legal fragment name from an illegal one"
  );

  // The exclusion is exactly `on`, and exactly the fragment's own name: a *type condition* named
  // `on` carries none, and neither does a fragment named `true`.
  for legal in ["fragment F on on { f }", "fragment true on T { f }"] {
    let parse = parse_document(legal);
    assert!(!parse.has_errors(), "{legal}: rejected, and it is legal");
    assert_eq!(
      project(&parse, legal).expect("projects"),
      oracle(legal).expect("parses"),
      "{legal}"
    );
  }
}

#[test]
fn an_unknown_directive_location_refuses() {
  let kind = refuse(&entry("invalid_directive_location_unknown.graphqlx"));
  assert_eq!(kind, ProjectErrorKind::MalformedToken { kind: K::Name });
}

#[test]
fn a_described_import_refuses_as_rubble() {
  // A description may precede only a definition, so the string in front of an `import` stays
  // outside the node it precedes and lands as a bare token under the document.
  let kind = refuse(&entry("invalid_x_described_import.graphqlx"));
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::Document,
      found: K::InlineString,
    }
  );
}

#[test]
fn an_import_source_that_is_a_block_string_refuses() {
  // `ImportDefinition` holds an `InlineStringValue`, so a block string here is a shape the AST
  // cannot store — the lossless production reports it and builds the node anyway.
  let kind = refuse(&entry("invalid_x_import_block_string_source.graphqlx"));
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::StringValue,
      found: K::BlockString,
    }
  );
}

#[test]
fn a_where_clause_with_nothing_to_constrain_refuses() {
  // The AST can only hold a `where` inside a `Constrained`, so a clause whose target the tree does
  // not have is a region with no AST image.
  let kind = refuse(&entry("invalid_x_where_without_block.graphqlx"));
  assert_eq!(
    kind,
    ProjectErrorKind::MissingChild {
      parent: K::ObjectTypeDefinition,
      wanted: "the fields a where clause constrains",
    }
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
  // The half of the threat model a per-token comparison could not see, and the reason the check is
  // made once against the whole tree instead. In both pairs below every token whose text a
  // constructor reads is byte-identical and correctly positioned; what moved is a brace or a
  // space.
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
fn a_tree_deeper_than_the_ceiling_is_refused_rather_than_descended() {
  use smear::parser::lossless::project::MAX_GREEN_DEPTH;

  // One level past what the door's verification will descend. The tree holds no token, so its text
  // is empty and the bytes agree — which is what makes depth the only thing left to refuse it for.
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

  // The verification is what every door opens with, and it is the reachable half for a hand-built
  // tree: the free `project*` functions take a `Parse` and nothing here can mint one. What the
  // ceiling stands in front of is the dispatch, which is a worklist and would descend this tree on
  // any stack — so the refusal is a policy about which trees a door admits, not a rescue.
  let kind = smear::parser::lossless::project::verify_source::<K>(root.green(), "")
    .expect_err("`verify_source` descended a tree past the ceiling")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::TooDeep {
      limit: MAX_GREEN_DEPTH
    }
  );
}

#[test]
fn a_missing_constituent_refuses() {
  // No corpus entry reaches this at a *type* position: every shape the recovery produces either
  // keeps the constituent or leaves an `Error` hole, which is refused earlier. So the witness is
  // synthetic — a field definition with a name and no type, the shape a future recovery change
  // could start emitting.
  let text = "type T{f:}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
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
  // The lossless lexer never emits a string token it cannot re-lex, so this class is reachable
  // only by handing the projection a tree that claims one. Its value is that the refusal exists
  // rather than a panic or a silently truncated literal.
  let text = "\"oops type T{f:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  // An unterminated literal, claimed as this definition's description.
  tree.token(K::InlineString, "\"oops").token(K::Space, " ");
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
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
  assert_eq!(
    kind,
    ProjectErrorKind::MalformedToken {
      kind: K::InlineString
    }
  );
}

#[test]
fn every_refusal_kind_has_a_witness() {
  // Totality. The list is written out rather than derived, because `ProjectErrorKind` is
  // `#[non_exhaustive]` and there is no way to enumerate its variants at run time — so adding a
  // variant without a pin has to be caught by the count below.
  let witnesses: Vec<(&str, ProjectErrorKind)> = vec![
    (
      "rubble under the document",
      refuse(&entry("invalid_top_level_junk.graphqlx")),
    ),
    (
      "an enum value named `true`",
      refuse(&entry("invalid_enum_reserved_spelling.graphqlx")),
    ),
    (
      "an unknown directive location",
      refuse(&entry("invalid_directive_location_unknown.graphqlx")),
    ),
    (
      "a where clause with nothing to constrain",
      refuse(&entry("invalid_x_where_without_block.graphqlx")),
    ),
    ("a mismatched source", {
      let src = "type T { f: Int }";
      project(&parse_document(src), "type U { f: Int }")
        .map(|_| ())
        .expect_err("refuses")
        .kind()
        .clone()
    }),
  ];

  let seen: Vec<&'static str> = witnesses
    .iter()
    .map(|(what, kind)| match kind {
      ProjectErrorKind::MissingChild { .. } => "MissingChild",
      ProjectErrorKind::UnexpectedChild { .. } => "UnexpectedChild",
      ProjectErrorKind::MalformedToken { .. } => "MalformedToken",
      ProjectErrorKind::SourceMismatch => "SourceMismatch",
      ProjectErrorKind::SemanticRule { .. } => "SemanticRule",
      ProjectErrorKind::TooDeep { .. } => "TooDeep",
      other => panic!("{what}: a kind with no name here: {other:?}"),
    })
    .collect();
  let distinct: BTreeSet<&'static str> = seen.iter().copied().collect();
  assert_eq!(
    distinct.len(),
    5,
    "the five corpus-reachable kinds are not all witnessed: {seen:?}"
  );

  // `TooDeep` is the sixth and is pinned by
  // `a_tree_deeper_than_the_ceiling_is_refused_rather_than_descended`, `InvalidRawKind` the
  // seventh, pinned by `a_raw_kind_outside_the_space_refuses_rather_than_panicking`, and
  // `WrongRoot` the eighth, pinned by `a_parse_minted_over_a_wrong_root_is_refused_not_reported_complete`;
  // all three need a synthetic tree. What this count owns is the claim that eight variants exist
  // and eight are witnessed somewhere in this file.
  const KIND_COUNT: usize = 8;
  assert_eq!(
    distinct.len() + 3,
    KIND_COUNT,
    "ProjectErrorKind has a variant with no witness in this file; add one and raise the count"
  );
}

// ---------------------------------------------------------------------------------------------
// the shape-faithful boundary, and the invalid half as a census
// ---------------------------------------------------------------------------------------------

#[test]
fn the_unclosed_brace_class_projects_although_the_parser_rejects_it() {
  // The documented boundary of "shape-faithful, not verdict-faithful". Asserted rather than left
  // implicit so the day it moves is a day somebody is told.
  let src = entry("invalid_unterminated_brace.graphqlx");
  let parse = parse_document(&src);
  assert!(
    parse.has_errors(),
    "the lossless parse reports the missing closer"
  );
  assert!(oracle(&src).is_err(), "the syntactic parser rejects it");
  assert!(
    !parse
      .syntax()
      .descendants()
      .any(|node| matches!(node.kind(), K::Error | K::Gap)),
    "this class is shape-complete: the tree carries no hole, which is why it projects"
  );
  project(&parse, &src).expect("a shape-complete tree projects even though the parse was rejected");
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
    projected.contains(&"invalid_unterminated_brace.graphqlx"),
    "the unclosed-brace class is the pinned shape-faithful survivor; it now refuses: {projected:?}"
  );
}

/// A **direct** consumer of the recovering projector — no validator, no `smear-compiler` door —
/// cannot obtain an AST it can mistake for the whole document.
#[test]
fn the_recovering_projector_refuses_a_pair_it_is_not_a_projection_of() {
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
      "{what}: the pair under test has to be a mismatched one, and mismatched for its BYTES"
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
      "{what}: the pair under test has to be a mismatched one, and mismatched for its BYTES"
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
/// the document node's children would report it as nothing lost.
#[test]
fn a_gap_beside_the_document_node_is_counted() {
  for (what, src, projected, skipped) in [
    ("a gap beside an empty document node", "%", 0, 1),
    ("a gap inside the document node", "{ a } %", 1, 1),
    ("no document node at all", "{ a }\nquery Bad(", 1, 3),
    ("only trivia", "# nothing\n", 0, 0),
    ("nothing at all", "", 0, 0),
  ] {
    let parse = parse_executable_document(src);
    let (ast, recovery) = project_executable_document_recovered(&parse, src)
      .expect("a parse projects against its own text");
    assert_eq!(
      (ast.definitions().len(), recovery.projected() as usize),
      (projected, projected),
      "{what}: the projected tally and the AST disagree"
    );
    assert_eq!(recovery, Recovery::new(projected as u32, skipped), "{what}");
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

// ---------------------------------------------------------------------------------------------
// navigation helpers for the shaped half
// ---------------------------------------------------------------------------------------------

/// The value of the first constant argument of the first definition's first directive.
fn const_argument_value<'a>(
  document: &'a Document<&'a str>,
) -> &'a smear::parser::graphqlx::ast::ConstInputValue<&'a str> {
  let described = document.definitions()[0]
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
  let directives = object.directives().expect("directives");
  let arguments = directives.directives()[0]
    .arguments()
    .expect("an argument list");
  arguments.arguments()[0].value()
}

/// The type of the first definition's first field.
fn first_field_type<'a>(
  document: &'a Document<&'a str>,
) -> &'a smear::parser::graphqlx::ast::Type<&'a str> {
  let described = document.definitions()[0]
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
  fields.target().field_definitions()[0].node().ty()
}

// ---------------------------------------------------------------------------------------------
// al8n/smear#58 round 3: a walk represents every non-trivia byte under its node, or it refuses
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

/// `query Q{f}` with the field carrying a `Directives` node and whatever `inside` writes into it.
fn field_with_directives(
  inside: impl FnOnce(&mut Tree),
  text: &'static str,
) -> (SyntaxNode, &'static str) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.token(K::Name, "query").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "Q").close();
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

#[test]
fn a_present_directive_run_with_no_directive_refuses() {
  // Row one of the container table, at the site the production-side derivation missed. `Directive+`
  // has no empty value the syntactic parser produces — accepting an `@` commits it to a whole
  // directive — so a present run with nothing in it is `MissingChild` and only an *absent* run is
  // `None`. Before this round both flavours collapsed a present-empty run to `None`, which is the
  // one answer the AST must never carry: the bytes under the node reached nothing.
  let (node, text) = field_with_directives(|_| {}, "query Q{f}");
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::MissingChild {
      parent: K::Directives,
      wanted: "a directive",
    },
    "a present run with no directive is the refusal, not `None`"
  );

  // The const twin, through an SDL definition's own run.
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "S").close();
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

  // And the witness as it was reported: a run holding a stray `@` and no directive. **The token
  // refusal fires first**, not `MissingChild` — a `Directives` node spells no tokens of its own
  // (its `@`s belong to its `Directive` children), so the walk refuses the `@` where it stands and
  // never reaches the emptiness check at the end of the loop.
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
    "the vocabulary refuses the token before the cardinality refuses the run"
  );
}

#[test]
fn a_fragment_name_split_into_two_tokens_refuses() {
  // The `on` bypass, and the class behind it. `ExecutableDefinitionName` holds exactly one name, so
  // a walk that collected three and read the first let `Name("o")` + `Name("n")` spell `on` in the
  // source while the rule inspected `o`. The surplus token is now the refusal, and the rule then
  // runs over the only name there is.
  let text = "fragment on on T{f}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::FragmentDefinition);
  tree.token(K::Name, "fragment").token(K::Space, " ");
  tree.open(K::ExecutableDefinitionName);
  tree.token(K::Name, "o").token(K::Name, "n");
  tree.close();
  tree.token(K::Space, " ");
  tree.open(K::TypeCondition);
  tree.token(K::Name, "on").token(K::Space, " ");
  tree.open(K::TypePath).path("T").close();
  tree.close();
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
      parent: K::ExecutableDefinitionName,
      found: K::Name,
    },
    "the second name is refused where it sits"
  );
}

#[test]
fn a_fourth_name_token_refuses() {
  // The other end of the same parameter: a directive definition is the one shape that reads three
  // direct names (`directive`, `repeatable`, `on`), and a fourth is a tree no production builds.
  let text = "directive @d repeatable on x FIELD";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::DirectiveDefinition);
  tree.token(K::Name, "directive").token(K::Space, " ");
  tree.token(K::At, "@");
  tree.open(K::DefinitionName).token(K::Name, "d").close();
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
    },
    "the count is the shape's, not a constant"
  );
}

#[test]
fn a_duplicate_of_an_expected_once_child_refuses() {
  // Roughly forty arms in this file used to dispatch behind an `if x_node.is_none()` guard, and
  // every one of them fell through to a wildcard that covered the duplicate and dropped it: a tree
  // with two names projected to the first name and a span over both. The walks are transcriptions
  // now and the second name is simply not in the sequence; this is that case, reached.
  let text = "type TU{f:Int}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
  tree.open(K::DefinitionName).token(K::Name, "U").close();
  tree.fields("f", "Int");
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::ObjectTypeDefinition,
      found: K::DefinitionName,
    },
    "the second one is refused rather than covered and dropped"
  );
}

#[test]
fn a_stray_token_a_shape_does_not_spell_refuses() {
  // The token half of the same rule. A `DefinitionName` spells one `Name` and nothing else, so a
  // `:` under it is a byte the walk would fold into the definition's span and never represent.
  let text = "scalar S:";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName);
  tree.token(K::Name, "S").token(K::Colon, ":");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::DefinitionName,
      found: K::Colon,
    },
    "the vocabulary is the shape's own keywords and punctuation, and nothing else"
  );
}

#[test]
fn a_name_token_spelled_as_a_number_refuses() {
  // The leaf table's fourth row. The range comes from a token the *tree* labelled `Name`, so the
  // bytes under it are whatever the caller wrote; `Name("1")` is a value no source produces, and
  // `smear_lexer::graphqlx::identifier` — the shipped scanner, whole-slice — is what says so.
  let text = "scalar 1";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "1").close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::MalformedToken { kind: K::Name },
    "a name is re-cooked through the lexer's own identifier door"
  );

  // A keyword passes: this dialect's keywords are contextual, so the door reads `query` as an
  // identifier and it is the classifier afterwards that tells them apart. Without this half the
  // door could be a spelling table and the cell would not notice.
  let src = "scalar query";
  assert_eq!(
    project(&parse_document(src), src).expect("a keyword-spelled name projects"),
    oracle(src).expect("and the parser agrees")
  );
}

#[test]
fn a_null_value_over_another_identifier_refuses() {
  // The leaf table's last row, and the hole it closed. `BooleanValue` always compared its text;
  // `NullValue` carried whatever identifier the tree put under it straight into the AST, so this
  // pair used to project to a `null` spelled `X`.
  let (node, text) = scalar_with_literal(K::NullValue, K::Name, "X");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::MalformedToken { kind: K::Name },
    "the spelling is classified through the lexer's keyword table, not assumed from the node kind"
  );
}

#[test]
fn a_string_leaf_without_its_quotes_refuses() {
  // The leaf table's third row, measured rather than assumed: `LitStr::try_from` is the string
  // sub-lexer, and an image with no quotes is not a string literal to it.
  let (node, text) = scalar_with_literal(K::StringValue, K::InlineString, "abc");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::MalformedToken {
      kind: K::InlineString
    },
    "the decoder refuses a malformed image rather than trusting the node's label"
  );
}

#[test]
fn a_tree_that_splits_a_token_projects_the_tree_it_was_handed() {
  // **A decision, not a defect** — see the module header's *what a tree the parser did not build is
  // promised*. Over `[-12]` a caller can build two adjacent `IntValue` tokens; each slice is one
  // whole integer to the lexer's door, the byte verification passes because the concatenation is
  // the source, and the projection answers the AST of the sentence the *tree* spells. Making it
  // answer otherwise means re-lexing the source around every token, which is the re-parse
  // `a_projection_that_re_parsed_the_source_would_fail_this` exists to forbid.
  let text = "scalar S@d(n:[-12])";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "S").close();
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
    .expect("the synthetic root is a Document")
    .to_ast(text)
    .expect("a tree the parser would not build is still a tree, and this one is well-shaped");

  // Two literals with the tree's own boundaries, and the real parse of the same bytes has one.
  let debug = std::format!("{projected:?}");
  assert_eq!(
    debug.matches("IntValue").count(),
    2,
    "the tree says two integers"
  );
  let parsed = project(&parse_document(text), text).expect("the real parse projects");
  assert_eq!(
    std::format!("{parsed:?}").matches("IntValue").count(),
    1,
    "the shipped lexer reads one integer `-12`; if it stopped, this contrast is gone"
  );
  assert_ne!(projected, parsed);

  // The spans are the tree's boundaries: `-1` at 14..16 and `2` at 16..17 inside `[` at 13.
  assert!(
    debug.contains("start: 14, end: 16") && debug.contains("start: 16, end: 17"),
    "the two literals carry the tree's own ranges, not the lexer's: {debug}"
  );
}

#[test]
fn a_leaf_with_two_literal_tokens_refuses() {
  // The byte rule one level down, found by carrying it across the file rather than by the review.
  // A leaf's vocabulary *is* its literal's kind, so every non-trivia token under it is a candidate
  // value — and a walk that read the first and folded the second answered `1` for `12` with a span
  // across both bytes.
  let text = "scalar S@d(n:12)";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "S").close();
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
    },
    "a leaf carries one token, and the second is not a token to skip past"
  );

  // The single-token form of the same bytes is what the parser builds, and it projects.
  let src = "scalar S@d(n:12)";
  assert_eq!(
    project(&parse_document(src), src).expect("projects"),
    oracle(src).expect("parses"),
    "the control: this is the same text, and one `Int` token is the shape the parser gives it"
  );
}

#[test]
fn a_keyword_slot_a_walk_collects_is_read_by_spelling() {
  // A directive definition is the one shape with three keyword slots, and only index 1 was ever
  // read — so `foo` in the `repeatable` position answered `repeatable: false`, the same value
  // `on` alone answers, with `foo`'s bytes in the span and nowhere else. Every slot is checked now.
  let text = "directive @d foo FIELD";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::DirectiveDefinition);
  tree.token(K::Name, "directive").token(K::Space, " ");
  tree.token(K::At, "@");
  tree.open(K::DefinitionName).token(K::Name, "d").close();
  tree.token(K::Space, " ").token(K::Name, "foo");
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
    },
    "the word in the slot is read, not counted"
  );

  // The import specifier's second slot, the other shape that collects more than one name.
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ImportDefinition);
  tree.token(K::Name, "import").token(K::Space, " ");
  tree.open(K::ImportList);
  tree.token(K::LBrace, "{").token(K::Space, " ");
  tree.open(K::NamedSpecifier);
  tree.token(K::Name, "a").token(K::Space, " ");
  tree.token(K::Name, "b").token(K::Space, " ");
  tree.open(K::Path).token(K::Name, "ns").close();
  tree.close();
  tree.token(K::Space, " ").token(K::RBrace, "}");
  tree.close();
  tree.token(K::Space, " ").token(K::Name, "from");
  tree.token(K::Space, " ");
  tree
    .open(K::StringValue)
    .token(K::InlineString, "\"m\"")
    .close();
  tree.close();
  tree.close();
  let node = tree.finish();
  let text = "import { a b ns } from \"m\"";
  assert_eq!(
    node.text().to_string(),
    text,
    "the probe's own text, so the refusal is about the tree and not about a typo here"
  );
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::NamedSpecifier,
      found: K::Name,
    },
    "`b` is not `as`, and the slot it sits in reaches no AST field"
  );

  // Both legal spellings still project, so the check is a spelling test rather than a count.
  for src in [
    "directive @d repeatable on FIELD",
    "directive @d on FIELD",
    "import { a as b } from \"m\"",
  ] {
    assert_eq!(
      project(&parse_document(src), src).expect("projects"),
      oracle(src).expect("parses"),
      "{src}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// al8n/smear#58 round 4: a shared walker's vocabulary is its concrete kind's, and a choice is one
// ---------------------------------------------------------------------------------------------

/// The child kinds each SDL definition's production spells, **read off the productions here** so
/// the cross-product below is not derived from the table it is checking.
///
/// `graphqlx/lossless/definition.rs`: scalar is `DefinitionName Directives?`; type and interface
/// share `DefinitionName ImplementInterfaces? Directives? WhereClause? FieldsDefinition?`; union is
/// `DefinitionName Directives? UnionMemberTypes? WhereClause?`; enum is
/// `DefinitionName Directives? EnumValuesDefinition?` and is the one with no clause at all; input
/// is `DefinitionName Directives? WhereClause? InputFieldsDefinition?`.
const DEFINITION_ROWS: &[(K, &str, &[K])] = &[
  (K::ScalarTypeDefinition, "scalar", &[K::Directives]),
  (
    K::ObjectTypeDefinition,
    "type",
    &[
      K::ImplementInterfaces,
      K::Directives,
      K::WhereClause,
      K::FieldsDefinition,
    ],
  ),
  (
    K::InterfaceTypeDefinition,
    "interface",
    &[
      K::ImplementInterfaces,
      K::Directives,
      K::WhereClause,
      K::FieldsDefinition,
    ],
  ),
  (
    K::UnionTypeDefinition,
    "union",
    &[K::Directives, K::UnionMemberTypes, K::WhereClause],
  ),
  (
    K::EnumTypeDefinition,
    "enum",
    &[K::Directives, K::EnumValuesDefinition],
  ),
  (
    K::InputObjectTypeDefinition,
    "input",
    &[K::Directives, K::WhereClause, K::InputFieldsDefinition],
  ),
];

/// [`DEFINITION_ROWS`]' extension twin, from `graphqlx/lossless/extension.rs`.
const EXTENSION_ROWS: &[(K, &str, &[K])] = &[
  (K::ScalarTypeExtension, "scalar", &[K::Directives]),
  (
    K::ObjectTypeExtension,
    "type",
    &[
      K::ImplementInterfaces,
      K::Directives,
      K::WhereClause,
      K::FieldsDefinition,
    ],
  ),
  (
    K::InterfaceTypeExtension,
    "interface",
    &[
      K::ImplementInterfaces,
      K::Directives,
      K::WhereClause,
      K::FieldsDefinition,
    ],
  ),
  (
    K::UnionTypeExtension,
    "union",
    &[K::Directives, K::UnionMemberTypes, K::WhereClause],
  ),
  (
    K::EnumTypeExtension,
    "enum",
    &[K::Directives, K::EnumValuesDefinition],
  ),
  (
    K::InputObjectTypeExtension,
    "input",
    &[K::Directives, K::WhereClause, K::InputFieldsDefinition],
  ),
];

/// The union of every tail the two shared walkers used to accept, minus the name each row always
/// has — which is exactly the set the union let through to the wrong constructor.
const SHARED_TAILS: &[K] = &[
  K::ImplementInterfaces,
  K::Directives,
  K::WhereClause,
  K::FieldsDefinition,
  K::InputFieldsDefinition,
  K::UnionMemberTypes,
  K::EnumValuesDefinition,
];

/// `<keyword> T` under `parent`, with an empty `foreign` node hung off it.
///
/// The foreign node is empty on purpose: the refusal is about `child.kind()` and fires before the
/// walk ever descends, so the subtree's content cannot be what makes the cell pass — and the
/// assertion names the exact refusal, so a walk that descended first would answer `MissingChild`
/// and fail here rather than pass for the wrong reason.
fn definition_with_foreign(parent: K, keyword: &str, foreign: K) -> (SyntaxNode, String) {
  let text = std::format!("{keyword} T");
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(parent);
  tree.token(K::Name, keyword).token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
  tree.open(foreign).close();
  tree.close();
  tree.close();
  (tree.finish(), text)
}

/// [`definition_with_foreign`]'s extension twin: `extend <keyword> T`.
fn extension_with_foreign(parent: K, keyword: &str, foreign: K) -> (SyntaxNode, String) {
  let text = std::format!("extend {keyword} T");
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(parent);
  tree.token(K::Name, "extend").token(K::Space, " ");
  tree.token(K::Name, keyword).token(K::Space, " ");
  tree.open(K::ExtensionName).path("T").close();
  tree.open(foreign).close();
  tree.close();
  tree.close();
  (tree.finish(), text)
}

#[test]
fn every_definition_kind_refuses_a_foreign_child() {
  // `definition_head` is one walk reached by six node kinds, and each constructor reads only the
  // slots its own production has. With the union as its vocabulary a `ScalarTypeDefinition`
  // holding a `FieldsDefinition` reached `scalar_type_definition`, which reads the name and the
  // directives and answered `Ok` — the field block covered by the definition's span and present
  // in no AST field. The row is the parameter now, and this is the whole cross-product.
  let mut pairs = 0usize;
  for (parent, keyword, row) in DEFINITION_ROWS {
    for foreign in SHARED_TAILS.iter().filter(|k| !row.contains(k)) {
      let (node, text) = definition_with_foreign(*parent, keyword, *foreign);
      assert_eq!(
        node.text().to_string(),
        text,
        "{parent:?}/{foreign:?}: the probe's own text"
      );
      assert_eq!(
        refuse_tree(node, &text),
        ProjectErrorKind::UnexpectedChild {
          parent: *parent,
          found: *foreign,
        },
        "{parent:?} has no place for a {foreign:?}, so the walk refuses it rather than filling a \
         slot its constructor never reads"
      );
      pairs += 1;
    }
  }
  assert_eq!(
    pairs, 25,
    "six definition kinds against the seven shared tails, minus each row's own: the cross-product \
     is the claim, so its size is asserted rather than trusted"
  );
}

#[test]
fn every_extension_kind_refuses_a_foreign_tail() {
  // `extension_parts`, the same defect one section down: every tail was projected and the
  // type-specific constructors ignored the ones they had no field for.
  let mut pairs = 0usize;
  for (parent, keyword, row) in EXTENSION_ROWS {
    for foreign in SHARED_TAILS.iter().filter(|k| !row.contains(k)) {
      let (node, text) = extension_with_foreign(*parent, keyword, *foreign);
      assert_eq!(node.text().to_string(), text);
      assert_eq!(
        refuse_tree(node, &text),
        ProjectErrorKind::UnexpectedChild {
          parent: *parent,
          found: *foreign,
        },
        "{parent:?} has no place for a {foreign:?}"
      );
      pairs += 1;
    }
  }
  assert_eq!(pairs, 25);
}

#[test]
fn each_kinds_own_children_still_project() {
  // The control the cross-product needs: the rows are not simply refusing everything. One legal
  // document per definition and extension kind, through the real parser, compared with the oracle.
  for src in [
    "scalar S @d",
    "type T implements I @d where A: B { f: Int }",
    "interface I implements J @d where A: B { f: Int }",
    "union U @d = A | B where A: C",
    "enum E @d { A }",
    "input I @d where A: B { f: Int }",
    "extend scalar S @d",
    "extend type T implements I @d where A: B { f: Int }",
    "extend interface I implements J @d where A: B { f: Int }",
    "extend union U @d = A | B where A: C",
    "extend enum E @d { A }",
    "extend input I @d where A: B { f: Int }",
  ] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}: the probe must be legal input");
    assert_eq!(
      project(&parse, src).expect("projects"),
      oracle(src).expect("parses"),
      "{src}"
    );
  }
}

#[test]
fn a_described_shorthand_refuses_and_its_recovery_is_not_complete() {
  // **The parser builds this one.** `document.rs`'s `definition_after_description` reports the
  // pair and still parses the operation, and because the description was committed before the mark
  // the node opens at, the string ends up *inside* the `OperationDefinition` — unlike a described
  // import or extension, where it stays outside and the document level refuses it as rubble.
  let src = "\"docs\" { id }";
  let parse = parse_executable_document(src);
  assert!(
    parse.has_errors(),
    "the lossless production reports the pair"
  );
  assert!(
    !parse
      .syntax()
      .descendants_with_tokens()
      .any(|element| matches!(element.kind(), K::Error | K::Gap)),
    "and builds it whole, so the hole scan cannot see this class"
  );
  assert!(
    parse
      .syntax()
      .descendants()
      .find(|node| node.kind() == K::OperationDefinition)
      .expect("an operation definition")
      .children_with_tokens()
      .any(|element| element.kind() == K::InlineString),
    "the description is a direct token of the operation, which is what made it projectable"
  );
  assert!(
    executable_oracle(src).is_err(),
    "and the syntactic parser rejects it — `refuse_described_shorthand`"
  );

  // Before: `Ok(Described { description: Some(_), node: Shorthand(_) })`, a value the syntactic
  // parser produces for no input at all. `UnexpectedChild` and not `SemanticRule`: the syntactic
  // side refuses with an `Expectation` at the `{` rather than by naming a rule.
  let kind = project_executable_document(&parse, src)
    .map(|_| ())
    .expect_err("a described shorthand is outside the image")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::OperationDefinition,
      found: K::InlineString,
    }
  );

  // And the recovering door must stop calling it complete: before, `entries = 1, skipped = 0`.
  let (document, recovery) =
    project_executable_document_recovered(&parse, src).expect("the pair verifies");
  assert_eq!(document.definitions().len(), 0, "nothing was projected");
  assert_eq!(recovery.skipped(), 1, "and the entry is counted as lost");
  assert!(
    !recovery.is_complete(),
    "a recovery that reports complete over a dropped description is the same defect wearing a \
     success type"
  );

  // The same branch also dropped a `DefinitionName`: a shorthand has no name.
  let text = "Q{ id }";
  let mut tree = Tree::new();
  tree.open(K::ExecutableDocument);
  tree.open(K::OperationDefinition);
  tree.open(K::DefinitionName).token(K::Name, "Q").close();
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{").token(K::Space, " ");
  tree.open(K::Field).token(K::Name, "id").close();
  tree.token(K::Space, " ").token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  let kind = ExecutableDocumentNode::cast_node(node)
    .expect("the synthetic root is an ExecutableDocument")
    .to_ast(text)
    .map(|_| ())
    .expect_err("a shorthand with a name is not a tree the parser builds")
    .kind()
    .clone();
  assert_eq!(
    kind,
    ProjectErrorKind::UnexpectedChild {
      parent: K::OperationDefinition,
      found: K::DefinitionName,
    }
  );
}

#[test]
fn an_import_with_both_clause_alternatives_refuses_in_either_order() {
  // `ImportList | WildcardSpecifier` is a choice, and reading it as `(Some(list), _)` selected the
  // list while the wildcard's subtree vanished with its bytes still inside the import's extent.
  // Exclusivity is at the walk now, so the *second* alternative is what refuses — in either order.
  for (first, second) in [
    (K::ImportList, K::WildcardSpecifier),
    (K::WildcardSpecifier, K::ImportList),
  ] {
    let mut tree = Tree::new();
    tree.open(K::Document);
    tree.open(K::ImportDefinition);
    tree.token(K::Name, "import").token(K::Space, " ");
    for kind in [first, second] {
      match kind {
        K::ImportList => {
          tree.open(K::ImportList);
          tree.token(K::LBrace, "{");
          tree.open(K::NamedSpecifier).token(K::Name, "A").close();
          tree.token(K::RBrace, "}");
          tree.close();
        }
        _ => {
          tree.open(K::WildcardSpecifier);
          tree.token(K::Asterisk, "*");
          tree.close();
        }
      }
      tree.token(K::Space, " ");
    }
    tree.token(K::Name, "from").token(K::Space, " ");
    tree
      .open(K::StringValue)
      .token(K::InlineString, "\"m\"")
      .close();
    tree.close();
    tree.close();
    let node = tree.finish();
    let text = node.text().to_string();
    assert_eq!(
      refuse_tree(node, &text),
      ProjectErrorKind::UnexpectedChild {
        parent: K::ImportDefinition,
        found: second,
      },
      "{first:?} then {second:?}: whichever arrives second is the one with no place"
    );
  }

  // Both single-alternative spellings still project, so this is exclusivity and not a ban.
  for src in ["import { A } from \"m\"", "import * as N from \"m\""] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}");
    assert_eq!(
      project(&parse, src).expect("projects"),
      oracle(src).expect("parses"),
      "{src}"
    );
  }
}

#[test]
fn a_one_of_slot_group_refuses_its_second_member() {
  // The fourth row of the one-of table: a slot filled from a *kind set* rather than from one kind.
  // It is one `one_of` atom in the transcription, so a second member of the set is not in the
  // sequence and the next atom refuses it where it stands.
  let text = "type T{f:IntInt}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ObjectTypeDefinition);
  tree.token(K::Name, "type").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "T").close();
  tree.open(K::FieldsDefinition);
  tree.token(K::LBrace, "{");
  tree.open(K::FieldDefinition);
  tree.token(K::Name, "f").token(K::Colon, ":");
  tree.named_type("Int");
  tree.named_type("Int");
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
      parent: K::FieldDefinition,
      found: K::DefinitionTypePath,
    },
    "one type reference per field, and the second is not a node to fold past"
  );

  // The same shape in a value position, through a different kind set.
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ScalarTypeDefinition);
  tree.token(K::Name, "scalar").token(K::Space, " ");
  tree.open(K::DefinitionName).token(K::Name, "S").close();
  tree.one_argument("n", |tree| {
    tree.open(K::ObjectValue);
    tree.token(K::LBrace, "{");
    tree.open(K::ObjectField);
    tree.token(K::Name, "a").token(K::Colon, ":");
    tree.open(K::IntValue).token(K::Int, "1").close();
    tree.open(K::IntValue).token(K::Int, "2").close();
    tree.close();
    tree.token(K::RBrace, "}");
    tree.close();
  });
  tree.close();
  tree.close();
  let node = tree.finish();
  let text = node.text().to_string();
  assert!(text.starts_with("scalar S@d(n:{a:12})"), "{text}");
  assert_eq!(
    refuse_tree(node, &text),
    ProjectErrorKind::UnexpectedChild {
      parent: K::ObjectField,
      found: K::IntValue,
    },
    "one value per object field"
  );
}

// ---------------------------------------------------------------------------------------------
// al8n/smear#58 round 5: a committing prefix commits, and a separator introduces a member
// ---------------------------------------------------------------------------------------------

#[test]
fn an_as_without_its_path_refuses_and_its_recovery_is_not_complete() {
  // **The parser builds this one.** `graphqlx/lossless/import.rs`'s `optional_alias` consumes the
  // `as`, reports when no path follows, and closes the specifier with no hole — so the walk saw a
  // `NamedSpecifier` whose `Path` slot was simply empty and answered an *unaliased* import with
  // `as` inside its extent. A committing prefix commits: `if opt(as) then require(Path)`.
  let src = "import { A as } from \"m\"";
  let parse = parse_executable_document(src);
  assert!(
    parse.has_errors(),
    "the production reports the missing target"
  );
  assert!(
    !parse
      .syntax()
      .descendants_with_tokens()
      .any(|element| matches!(element.kind(), K::Error | K::Gap)),
    "and builds it whole, which is why the hole scan cannot see this class"
  );
  assert!(
    executable_oracle(src).is_err(),
    "the syntactic parser rejects a committed alias with no target"
  );
  assert!(
    project_executable_document(&parse, src).is_err(),
    "an `as` inside the extent and in no AST field is the class this round closed"
  );

  let (document, recovery) =
    project_executable_document_recovered(&parse, src).expect("the pair verifies");
  assert_eq!(document.definitions().len(), 0, "nothing projected");
  assert_eq!(recovery.skipped(), 1, "the entry is counted as lost");
  assert!(!recovery.is_complete(), "and the recovery is not complete");

  // The wildcard twin, caller-built. It has to be: `import * as from "m"` is **not** this shape —
  // `optional_alias` peeks an identifier head, so `from` becomes the alias path and the import
  // then has no `from` keyword at all, which is the lenient missing-token class and projects.
  let text = "import * as from \"m\"";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::ImportDefinition);
  tree.token(K::Name, "import").token(K::Space, " ");
  tree.open(K::WildcardSpecifier);
  tree.token(K::Asterisk, "*").token(K::Space, " ");
  tree.token(K::Name, "as");
  tree.close();
  tree.token(K::Space, " ").token(K::Name, "from");
  tree.token(K::Space, " ");
  tree
    .open(K::StringValue)
    .token(K::InlineString, "\"m\"")
    .close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert_eq!(
    refuse_tree(node, text),
    ProjectErrorKind::MissingChild {
      parent: K::WildcardSpecifier,
      wanted: "the path an `as` renames to",
    },
    "the same coupling on the other specifier"
  );

  // Both legal spellings still project, so this is a coupling and not a ban on `as`.
  for src in ["import { A as B } from \"m\"", "import * as N from \"m\""] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}");
    assert_eq!(
      project(&parse, src).expect("projects"),
      oracle(src).expect("parses"),
      "{src}"
    );
  }
}

#[test]
fn a_dangling_separator_refuses_in_every_separated_walk() {
  // The fifth shape of the cover-without-represent class, at multiplicity. The lossless parser
  // consumes a separator, reports that nothing follows it, and closes the node with no hole; a walk
  // that counted its members and never checked that every separator introduced one projected the
  // *weaker* value with the separator's bytes inside its span.
  for (src, parent) in [
    ("type T where A: B & { f: Int }", K::WherePredicate),
    ("type T implements A & { f: Int }", K::ImplementInterfaces),
    ("union U = A |", K::UnionMemberTypes),
    ("directive @d on FIELD |", K::DirectiveLocations),
  ] {
    let parse = parse_document(src);
    assert!(
      parse.has_errors(),
      "{src}: the production reports the dangle"
    );
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: and builds it whole"
    );
    assert!(
      parse
        .syntax()
        .descendants()
        .any(|node| node.kind() == parent),
      "{src}: the tree does not hold a {parent:?}, so this probe is aimed at nothing"
    );
    assert!(
      oracle(src).is_err(),
      "{src}: the syntactic parser rejects it"
    );
    assert!(
      project(&parse, src).is_err(),
      "{src}: a separator that introduces nothing is bytes in the span and in no AST field"
    );

    let (document, recovery) =
      project_type_system_document_recovered(&parse_type_system_document(src), src)
        .expect("the pair verifies");
    assert_eq!(document.definitions().len(), 0, "{src}: nothing projected");
    assert_eq!(recovery.skipped(), 1, "{src}");
    assert!(!recovery.is_complete(), "{src}");
  }

  // And two separators in a row, which is the same rule read from the other side.
  for src in [
    "type T where A: B & & C { f: Int }",
    "type T implements A & & B { f: Int }",
  ] {
    assert!(
      project(&parse_document(src), src).is_err(),
      "{src}: exactly one separator between adjacent members"
    );
  }

  // The legal spellings, including the leading separators two of the four allow.
  for src in [
    "type T where A: B & C { f: Int }",
    "type T implements & A & B { f: Int }",
    "union U = | A | B",
    "directive @d on | FIELD | QUERY",
  ] {
    let parse = parse_document(src);
    assert!(!parse.has_errors(), "{src}");
    assert_eq!(
      project(&parse, src).expect("projects"),
      oracle(src).expect("parses"),
      "{src}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// the report-and-build table, 65/65
// ---------------------------------------------------------------------------------------------

/// Which door a probe goes through.
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
  /// What is missing is a constituent the AST holds — a member, a path, a tail. Never lenient.
  Imaged,
  /// Nothing is missing: the site reports something *present* — an extra description, a spelling
  /// a rule or a classifier refuses, a variable in a constant position.
  Not,
}

/// One non-hole recovery site of the GraphQLx lossless productions: the file it is in, which
/// family it is (`report` for a `recover::report_unexpected::<…>` call, `unclosed` for a
/// `recover::unclosed_*::<…>` one), a probe that reaches it with a hole-free tree, the root, what
/// the projection answers, and what the site leaves out.
type Site = (
  &'static str,
  &'static str,
  &'static str,
  Root,
  &'static str,
  Absent,
);

/// Every report-and-build site, one probe each — the module header's table, executable.
///
/// The count per file and family is asserted against the source's own `grep`, so a site added to
/// a production without a row here fails the cell rather than going unmapped.
const SITES: &[Site] = &[
  (
    "document.rs",
    "report",
    "\"d\" import { A } from \"m\"",
    Root::Mixed,
    "UnexpectedChild Document InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" extend scalar S @k",
    Root::Mixed,
    "UnexpectedChild Document InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" { f }",
    Root::Mixed,
    "UnexpectedChild OperationDefinition InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" import { A } from \"m\"",
    Root::Executable,
    "UnexpectedChild ExecutableDocument InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" { f }",
    Root::Executable,
    "UnexpectedChild OperationDefinition InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" import { A } from \"m\"",
    Root::TypeSystem,
    "UnexpectedChild TypeSystemDocument InlineString",
    Absent::Not,
  ),
  (
    "document.rs",
    "report",
    "\"d\" extend scalar S @k",
    Root::TypeSystem,
    "UnexpectedChild TypeSystemDocument InlineString",
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
    "report",
    "",
    Root::Executable,
    "MissingChild ExecutableDocument",
    Absent::Imaged,
  ),
  (
    "executable.rs",
    "unclosed",
    "query Q($a: Int",
    Root::Executable,
    "UnexpectedChild ExecutableDocument Name",
    Absent::Token(K::VariablesDefinition, ")"),
  ),
  (
    "import.rs",
    "report",
    "import { A as } from \"m\"",
    Root::Mixed,
    "MissingChild NamedSpecifier",
    Absent::Imaged,
  ),
  (
    "import.rs",
    "report",
    "import { } from \"m\"",
    Root::Mixed,
    "MissingChild ImportList",
    Absent::Imaged,
  ),
  (
    "import.rs",
    "report",
    "import { A } \"m\"",
    Root::Mixed,
    "Ok",
    Absent::Token(K::ImportDefinition, "from"),
  ),
  (
    "import.rs",
    "report",
    "import { A } from \"\"\"m\"\"\"",
    Root::Mixed,
    "UnexpectedChild StringValue BlockString",
    Absent::Not,
  ),
  (
    "import.rs",
    "report",
    "import { A } from",
    Root::Mixed,
    "MissingChild ImportDefinition",
    Absent::Imaged,
  ),
  (
    "import.rs",
    "unclosed",
    "import { A",
    Root::Mixed,
    "MissingChild ImportDefinition",
    Absent::Token(K::ImportList, "}"),
  ),
  (
    "generic.rs",
    "report",
    "type T<> { f: Int }",
    Root::Mixed,
    "MissingChild DefinitionTypeGenerics",
    Absent::Imaged,
  ),
  (
    "generic.rs",
    "unclosed",
    "type T<A",
    Root::Mixed,
    "Ok",
    Absent::Token(K::DefinitionTypeGenerics, ">"),
  ),
  (
    "generic.rs",
    "report",
    "fragment <> F on T { f }",
    Root::Mixed,
    "MissingChild ExecutableDefinitionTypeGenerics",
    Absent::Imaged,
  ),
  (
    "generic.rs",
    "unclosed",
    "extend type T<A",
    Root::Mixed,
    "MissingChild ObjectTypeExtension",
    Absent::Token(K::ExtensionTypeGenerics, ">"),
  ),
  (
    "generic.rs",
    "report",
    "fragment on on T { f }",
    Root::Mixed,
    "SemanticRule",
    Absent::Not,
  ),
  (
    "generic.rs",
    "report",
    "type T where A: { f: Int }",
    Root::Mixed,
    "MissingChild WherePredicate",
    Absent::Imaged,
  ),
  (
    "generic.rs",
    "report",
    "type T where { f: Int }",
    Root::Mixed,
    "MissingChild WhereClause",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "report",
    "fragment F { f }",
    Root::Mixed,
    "UnexpectedChild FragmentDefinition SelectionSet",
    Absent::Imaged,
  ),
  (
    "selection.rs",
    "report",
    "fragment F T { f }",
    Root::Mixed,
    "Ok",
    Absent::Token(K::TypeCondition, "on"),
  ),
  (
    "selection.rs",
    "report",
    "fragment F on { f }",
    Root::Mixed,
    "MissingChild TypeCondition",
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
    "type T implements A & { f: Int }",
    Root::Mixed,
    "MissingChild ImplementInterfaces",
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
    "MissingChild RootOperationTypesDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "unclosed",
    "schema { query: Q",
    Root::Mixed,
    "Ok",
    Absent::Token(K::RootOperationTypesDefinition, "}"),
  ),
  (
    "definition.rs",
    "report",
    "type T where A: B",
    Root::Mixed,
    "MissingChild ObjectTypeDefinition",
    Absent::Imaged,
  ),
  (
    "definition.rs",
    "report",
    "union U where A: B",
    Root::Mixed,
    "MissingChild UnionTypeDefinition",
    Absent::Imaged,
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
    "extension.rs",
    "report",
    "extend scalar S",
    Root::Mixed,
    "MissingChild ScalarTypeExtension",
    Absent::Imaged,
  ),
  (
    "extension.rs",
    "report",
    "extend type T",
    Root::Mixed,
    "MissingChild ObjectTypeExtension",
    Absent::Imaged,
  ),
  (
    "extension.rs",
    "report",
    "extend union U",
    Root::Mixed,
    "MissingChild UnionTypeExtension",
    Absent::Imaged,
  ),
  (
    "extension.rs",
    "report",
    "extend enum E",
    Root::Mixed,
    "MissingChild EnumTypeExtension",
    Absent::Imaged,
  ),
  (
    "extension.rs",
    "report",
    "extend schema",
    Root::Mixed,
    "MissingChild SchemaExtension",
    Absent::Imaged,
  ),
  (
    "value.rs",
    "report",
    "type T { f(a: Int = $v): Int }",
    Root::Mixed,
    "UnexpectedChild DefaultValue VariableValue",
    Absent::Not,
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
    "unclosed",
    "{ f(a: set {1",
    Root::Mixed,
    "Ok",
    Absent::Token(K::SetValue, "}"),
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
    "type T { f: A<B",
    Root::Mixed,
    "Ok",
    Absent::Token(K::TypeGenerics, ">"),
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
    "ty.rs",
    "unclosed",
    "type T { f: <Int",
    Root::Mixed,
    "Ok",
    Absent::Token(K::SetType, ">"),
  ),
];

/// The second node kind three shared sites build: `angle_name_list` serves both bare-name generic
/// lists, `set_or_map_type` both angle types, and `collection_body` both keyword containers. One
/// probe per kind, so the derivation below sees every `(node, token)` a site can leave.
const SHARED: &[(&str, &str, Absent)] = &[
  (
    "generic.rs",
    "fragment <T",
    Absent::Token(K::ExecutableDefinitionTypeGenerics, ">"),
  ),
  (
    "ty.rs",
    "type T { f: <Int => Str",
    Absent::Token(K::MapType, ">"),
  ),
  (
    "value.rs",
    "{ f(a: map {1 => 2",
    Absent::Token(K::MapValue, "}"),
  ),
];

/// Whether the parse of `text` under `root` holds a node of `kind` without a direct token spelled
/// `token` — the hole-free witness the leniency criterion asks for.
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
      node.kind() == kind
        && !node
          .children_with_tokens()
          .filter_map(|element| element.into_token())
          .any(|child| child.text() == token)
    })
}

/// The module header's missing-token table, as `(parent kind, token)` rows read off the source.
fn header_lenient_rows() -> Vec<(String, String)> {
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphqlx/lossless/project.rs");
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

/// The two call families [`SITES`] maps, as the census reads them out of the source: the text a
/// `grep -c` for each would count.
const FAMILIES: [(&str, &[&str]); 2] = [
  ("report", &["recover::report_unexpected::<"]),
  (
    "unclosed",
    &[
      "recover::unclosed_list::<",
      "recover::unclosed_object::<",
      "recover::unclosed_parens::<",
      "recover::unclosed_angle::<",
    ],
  ),
];

#[test]
fn every_report_and_build_site_has_a_measured_answer() {
  // The census, per file and family: `grep -c 'recover::report_unexpected::<'` and the four
  // `recover::unclosed_*::<` spellings over the code lines of `graphqlx/lossless/*.rs`.
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../smear-parser/src/graphqlx/lossless");
  let mut in_source: std::collections::BTreeMap<(String, &str), usize> = Default::default();
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
    // Code lines only: the projection's own header spells both calls while describing them.
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
  let mut in_table: std::collections::BTreeMap<(String, &str), usize> = Default::default();
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
    (48, 17),
    "the two families' populations"
  );

  // Each probe: the parser reported, the tree is hole-free — the site is a report-and-build one
  // and not a hole — and the projection answers what the table says.
  let mut lenient = 0;
  for (file, family, text, root, expected, _) in SITES {
    let (errors, got) = site_answer(text, *root);
    assert!(
      errors,
      "{file} {family} {text:?}: the probe reported nothing"
    );
    assert_eq!(
      got, *expected,
      "{file} {family} {text:?} under {root:?}: the projection's answer moved"
    );
    lenient += usize::from(got == "Ok");
  }
  assert_eq!(
    lenient, PROJECTING_SITES,
    "the sites whose probe projects moved; each is a lenient row a site can leave well-formed"
  );

  // The derivation. A position is lenient iff the token the site leaves out has no AST image and
  // the site's probe is a hole-free witness of a node without it; the set this enumerates is the
  // header's missing-token table, row for row.
  let mut derived: Vec<(String, String)> = Vec::new();
  let mut unwitnessed: Vec<(String, String)> = Vec::new();
  let candidates = SITES
    .iter()
    .map(|(file, _, text, root, _, absent)| (*file, *text, *root, *absent))
    .chain(
      SHARED
        .iter()
        .map(|(file, text, absent)| (*file, *text, Root::Mixed, *absent)),
    );
  for (_, text, root, absent) in candidates {
    if let Absent::Token(kind, token) = absent {
      let row = (std::format!("{kind:?}"), token.to_string());
      if witnessed(text, root, kind, token) {
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
  assert_eq!(
    unwitnessed, UNWITNESSED,
    "the image-less tokens no probe witnesses moved"
  );
}

/// How many of [`SITES`]' probes project.
const PROJECTING_SITES: usize = 16;

/// Image-less tokens a site can leave out whose probe is **not** a witness — none: even the three
/// closers whose definition is lost at end of input leave their list in the tree, an orphan child
/// of the document, which is a hole-free witness by the criterion's letter.
const UNWITNESSED: &[(&str, &str)] = &[];

// ---------------------------------------------------------------------------------------------
// the separated atoms, and where they refuse
// ---------------------------------------------------------------------------------------------

/// A separated walk's node inside the smallest document that reaches it, with `inside` writing the
/// node's own children. Answers the tree and its text.
fn separated_in(parent: K, inside: impl FnOnce(&mut Tree)) -> (SyntaxNode, String) {
  let mut tree = Tree::new();
  tree.open(K::Document);
  match parent {
    K::WherePredicate => {
      tree.open(K::ObjectTypeDefinition);
      tree.token(K::Name, "type").token(K::Space, " ");
      tree.open(K::DefinitionName).token(K::Name, "T").close();
      tree.token(K::Space, " ");
      tree.open(K::WhereClause);
      tree.token(K::Name, "where").token(K::Space, " ");
      tree.open(K::WherePredicate);
      inside(&mut tree);
      tree.close();
      tree.close();
      tree.fields("f", "Int");
      tree.close();
    }
    K::ImplementInterfaces => {
      tree.open(K::ObjectTypeDefinition);
      tree.token(K::Name, "type").token(K::Space, " ");
      tree.open(K::DefinitionName).token(K::Name, "T").close();
      tree.token(K::Space, " ");
      tree.open(K::ImplementInterfaces);
      tree.token(K::Name, "implements").token(K::Space, " ");
      inside(&mut tree);
      tree.close();
      tree.fields("f", "Int");
      tree.close();
    }
    K::UnionMemberTypes => {
      tree.open(K::UnionTypeDefinition);
      tree.token(K::Name, "union").token(K::Space, " ");
      tree.open(K::DefinitionName).token(K::Name, "U").close();
      tree.open(K::UnionMemberTypes);
      tree.token(K::Equal, "=");
      inside(&mut tree);
      tree.close();
      tree.close();
    }
    K::DirectiveLocations => {
      tree.open(K::DirectiveDefinition);
      tree.token(K::Name, "directive").token(K::Space, " ");
      tree.token(K::At, "@");
      tree.open(K::DefinitionName).token(K::Name, "d").close();
      tree
        .token(K::Space, " ")
        .token(K::Name, "on")
        .token(K::Space, " ");
      tree.open(K::DirectiveLocations);
      inside(&mut tree);
      tree.close();
      tree.close();
    }
    _ => {
      // `K::Path`, as a field's type.
      tree.open(K::ObjectTypeDefinition);
      tree.token(K::Name, "type").token(K::Space, " ");
      tree.open(K::DefinitionName).token(K::Name, "T").close();
      tree.open(K::FieldsDefinition);
      tree.token(K::LBrace, "{");
      tree.open(K::FieldDefinition);
      tree.token(K::Name, "f").token(K::Colon, ":");
      tree.open(K::DefinitionTypePath);
      tree.open(K::Path);
      inside(&mut tree);
      tree.close();
      tree.close();
      tree.close();
      tree.token(K::RBrace, "}");
      tree.close();
      tree.close();
    }
  }
  tree.close();
  let node = tree.finish();
  let text = node.text().to_string();
  (node, text)
}

/// `TypePath > Path > Name`.
fn type_path_named(tree: &mut Tree, name: &str) {
  tree.open(K::TypePath).path(name).close();
}

#[test]
fn the_separated_atoms_refuse_at_the_obstruction() {
  // After the opening position and after every separator an item is required, and the refusal is
  // decided by what is in hand: `MissingChild` over the parent when the children ran out, and
  // `UnexpectedChild` **at the element** when something else is there. Codex round five found the
  // earlier form answering `MissingChild` for both, which names the parent while the obstruction
  // sits in plain view. Kinds and ranges pinned exactly.

  // The parser-built half: every one of these is a tree the lossless parser builds hole-free.
  for (src, want, range) in [
    // leading, where the production forbids one
    (
      "type T where A: & B { f: Int }",
      ProjectErrorKind::UnexpectedChild {
        parent: K::WherePredicate,
        found: K::Ampersand,
      },
      16..17,
    ),
    // doubled
    (
      "type T where A: B & & C { f: Int }",
      ProjectErrorKind::UnexpectedChild {
        parent: K::WherePredicate,
        found: K::Ampersand,
      },
      20..21,
    ),
    // trailing, in each of the four walks the parser can dangle
    (
      "type T where A: B & { f: Int }",
      ProjectErrorKind::MissingChild {
        parent: K::WherePredicate,
        wanted: "a bound",
      },
      13..20,
    ),
    (
      "type T implements A & { f: Int }",
      ProjectErrorKind::MissingChild {
        parent: K::ImplementInterfaces,
        wanted: "an interface",
      },
      7..22,
    ),
    (
      "union U = A |",
      ProjectErrorKind::MissingChild {
        parent: K::UnionMemberTypes,
        wanted: "a member type",
      },
      8..13,
    ),
    (
      "directive @d on FIELD |",
      ProjectErrorKind::MissingChild {
        parent: K::DirectiveLocations,
        wanted: "a directive location",
      },
      16..23,
    ),
  ] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src}: the production reports it");
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: and builds it whole"
    );
    let error = project(&parse, src).expect_err(src);
    assert_eq!((error.kind(), error.span()), (&want, &range), "{src}");
  }

  // The caller-built half: the doubled and wrong-kind shapes the parser recovers with a hole, in
  // every separated walk, and the path's own three.
  let cases: [(K, fn(&mut Tree), ProjectErrorKind, &str); 9] = [
    (
      K::WherePredicate,
      |tree| {
        type_path_named(tree, "A");
        tree.token(K::Colon, ":");
        type_path_named(tree, "B");
        tree.token(K::Ampersand, "&");
        tree.named_type("C");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::WherePredicate,
        found: K::DefinitionTypePath,
      },
      "C",
    ),
    (
      K::ImplementInterfaces,
      |tree| {
        type_path_named(tree, "A");
        tree.token(K::Ampersand, "&").token(K::Ampersand, "&");
        type_path_named(tree, "B");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::ImplementInterfaces,
        found: K::Ampersand,
      },
      "&B",
    ),
    (
      K::ImplementInterfaces,
      |tree| {
        type_path_named(tree, "A");
        tree.token(K::Ampersand, "&");
        tree.named_type("B");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::ImplementInterfaces,
        found: K::DefinitionTypePath,
      },
      "B",
    ),
    (
      K::UnionMemberTypes,
      |tree| {
        type_path_named(tree, "A");
        tree.token(K::Pipe, "|").token(K::Pipe, "|");
        type_path_named(tree, "B");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::UnionMemberTypes,
        found: K::Pipe,
      },
      "|B",
    ),
    (
      K::DirectiveLocations,
      |tree| {
        tree.token(K::Name, "FIELD");
        tree.token(K::Pipe, "|").token(K::Pipe, "|");
        tree.token(K::Name, "QUERY");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::DirectiveLocations,
        found: K::Pipe,
      },
      "|QUERY",
    ),
    (
      K::DirectiveLocations,
      |tree| {
        tree.token(K::Name, "FIELD").token(K::Pipe, "|");
        type_path_named(tree, "QUERY");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::DirectiveLocations,
        found: K::TypePath,
      },
      "QUERY",
    ),
    (
      K::Path,
      |tree| {
        tree.token(K::Name, "a");
        tree
          .token(K::PathSeparator, "::")
          .token(K::PathSeparator, "::");
        tree.token(K::Name, "b");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::Path,
        found: K::PathSeparator,
      },
      "::b",
    ),
    (
      K::Path,
      |tree| {
        tree.token(K::Name, "a").token(K::PathSeparator, "::");
      },
      ProjectErrorKind::MissingChild {
        parent: K::Path,
        wanted: "a path segment",
      },
      "a::",
    ),
    (
      K::Path,
      |tree| {
        tree
          .token(K::PathSeparator, "::")
          .token(K::PathSeparator, "::");
        tree.token(K::Name, "a");
      },
      ProjectErrorKind::UnexpectedChild {
        parent: K::Path,
        found: K::PathSeparator,
      },
      "::a",
    ),
  ];
  for (parent, inside, want, at) in cases {
    let (node, text) = separated_in(parent, inside);
    let start = text.rfind(at).expect("the obstruction is in the text");
    let range = match want {
      // The parent's own range: the `Path` node, which here is everything its tokens cover.
      ProjectErrorKind::MissingChild { .. } => start..start + at.len(),
      // The element in hand: its first token.
      _ => {
        let len = if at.starts_with("::") {
          2
        } else if at.starts_with(['&', '|']) {
          1
        } else {
          at.len()
        };
        start..start + len
      }
    };
    let error = DocumentNode::cast_node(node)
      .expect("a Document")
      .to_ast(&text)
      .map(|_| ())
      .expect_err(&text);
    assert_eq!(
      (error.kind(), error.span()),
      (&want, &range),
      "{parent:?} in `{text}`"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// the eighth lenient row, reserved spellings, and the transcription census
// ---------------------------------------------------------------------------------------------

#[test]
fn a_type_condition_without_its_on_projects_what_the_text_with_it_parses_to() {
  // `(TypeCondition, on)` meets the leniency criterion exactly as `(ImportDefinition, from)` does:
  // the keyword has no AST image — the condition stores only its path — and the lossless
  // production reports a missing one and still builds the node, hole-free, around the path. Codex
  // round five's first finding: a required `on` atom refused the present path and cost the whole
  // fragment. The comparison is against the text **with** the keyword, and the two texts are
  // padded so every token but the keyword sits at the same offset: the one span that differs is
  // the condition's own, which starts at its first token — the path in the tree, the `on` in the
  // text.
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
    "and builds the condition whole"
  );
  assert!(
    parse
      .syntax()
      .descendants()
      .any(|node| node.kind() == K::TypeCondition),
    "the tree holds a `TypeCondition`, so this probe is aimed at the lenient row"
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
    "the projection answers the restored text's value, the condition starting at its path"
  );

  // And the recovering door counts it complete: nothing was dropped.
  let (document, recovery) =
    project_executable_document_recovered(&parse_executable_document(src), src)
      .expect("the pair verifies");
  assert_eq!(document.definitions().len(), 1);
  assert!(recovery.is_complete());
}

#[test]
fn a_contextual_keyword_is_a_name_at_every_name_position() {
  // This dialect's keywords are contextual: the lexer reads `on`, `true`, `null` and `type` as
  // identifiers, and the syntactic parser accepts each of them wherever the grammar says *name* —
  // except at the two positions that make a rule of it, a fragment's name and an enum value's
  // declaring name, which `a_fragment_named_on_refuses` and `an_enum_value_named_true_refuses`
  // pin. Round five read 45 mutation-law violations as a reserved-spelling rule missing at four
  // name positions; every one was a mutation of a corpus tree already missing its `}` or its
  // `from`, and each of these texts is accepted and projects to the parse.
  for src in [
    "type on { x: Int }",
    "type true { x: Int }",
    "type null { x: Int }",
    "type T { on: Int }",
    "type T { false: Int }",
    "type T { type: Int }",
    "type T { x: on }",
    "type T { x: null }",
    "type T { x: ns::true }",
    "import { on } from \"m\"",
    "import { true as null } from \"m\"",
    "extend type on @k",
    "query on($on: Int) { on: true(on: $on) }",
    "fragment F on on { f }",
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

#[test]
fn every_walk_is_a_transcription() {
  // Round six's census, read off the projection's own source so it cannot drift from what the
  // module header says: no token folded by a vocabulary, no fixed-width name collector, no slot
  // guard, no wildcard arm that covers what it drops. Comment lines are skipped — the header's
  // table of retired hatches names all four.
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphqlx/lossless/project.rs");
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
  // The two loops over a child iterator that remain are not walks: the cursor's own trivia skip,
  // and the recovering door's pass over the root, which counts what it skips rather than
  // projecting a production. The cursor is the substrate's since al8n/smear#217/#218 hoisted it,
  // so its loop is counted where it now lives — the substrate's `walk` module — and the two still
  // sum to the two this census has always found.
  assert_eq!(
    count("for element in"),
    1,
    "a child loop outside the cursor"
  );
  let substrate = std::fs::read_to_string(
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../smear-parser/src/lossless/project.rs"),
  )
  .expect("the substrate's source is readable");
  let walk = &substrate[substrate
    .find("pub(crate) mod walk {")
    .expect("the substrate's walk module")..];
  assert_eq!(
    walk
      .lines()
      .filter(|line| !line.trim_start().starts_with("//"))
      .map(|line| line.matches("for element in").count())
      .sum::<usize>(),
    1,
    "the cursor's trivia skip, and nothing else"
  );
  assert_eq!(count("Cursor::new("), 74, "the walks, one cursor each");
}

#[test]
fn a_directive_definition_without_its_on_projects_what_the_text_with_it_parses_to() {
  // `(DirectiveDefinition, on)` — the leniency criterion applied uniformly (al8n/smear#58): the
  // keyword has no AST image and the production reports a missing one and still builds the
  // definition, hole-free, around its locations. Padded so every other token keeps its offset; no
  // span starts or ends on the `on`, so the two values are equal outright.
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
  let (document, recovery) =
    project_type_system_document_recovered(&parse_type_system_document(src), src)
      .expect("the pair verifies");
  assert_eq!(document.definitions().len(), 1);
  assert!(recovery.is_complete());
}

#[test]
fn an_extension_argument_list_without_its_closer_answers_what_the_text_with_it_does() {
  // `(ExtensionTypeGenerics, >)` meets the criterion — no image, and the list is built hole-free
  // without it — but only at end of input: any other token after the names is recovered as an
  // `Error` node. So the extension is always tail-less, and the leniency changes no answer: the
  // projection refuses at the extension, exactly as it does for the text with the `>` restored,
  // and the recovering door counts it skipped either way.
  for src in ["extend type T<A", "extend type T<A>"] {
    let parse = parse_document(src);
    assert!(parse.has_errors(), "{src}");
    assert!(
      !parse
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: hole-free"
    );
    assert_eq!(
      project(&parse, src).expect_err(src).kind(),
      &ProjectErrorKind::MissingChild {
        parent: K::ObjectTypeExtension,
        wanted: "interfaces, directives or fields for the extension to add",
      },
      "{src}: the refusal is the missing tail's, not the list's"
    );
    let (_, recovery) =
      project_type_system_document_recovered(&parse_type_system_document(src), src)
        .expect("the pair verifies");
    assert_eq!(recovery.skipped(), 1, "{src}");
  }
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
  // The closers the report-and-build census derives lenient beyond the corpus's own (Codex round
  // six's first finding): each `unclosed_*` site builds its node hole-free without the closer, and
  // the closer has no AST image. The projection of the unclosed text is the parse of the text
  // closed, every span that ended on a restored closer ending on the last token instead.
  for (open, closers) in [
    ("{ f(a: [1", "])}"),
    ("{ f(a: {b: 1", "})}"),
    ("{ f(a: set {1", "})}"),
    ("{ f(a: map {1 => 2", "})}"),
    ("scalar S @k(a: [1", "])"),
    ("scalar S @k(a: {b: set {1", "}})"),
    ("type T { f: A<B", ">}"),
    ("type T { f: [Int", "]}"),
    ("type T { f: <Int", ">}"),
    ("type T { f: <Int => Str", ">}"),
    ("type T<A", ">"),
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
    let (_, recovery) =
      project_executable_document_recovered(&parse_executable_document(open), open)
        .unwrap_or_else(|_| panic!("{open}"));
    if open.starts_with('{') {
      assert!(recovery.is_complete(), "{open}");
    }
  }

  // The four whose closed text the parser still refuses. An import list without its `}` is
  // reachable only at end of input, where the import has no source either; the other three are
  // lists whose definition is lost at end of input, left as orphans beside its rubble.
  for (src, found) in [
    ("query Q($a: Int", K::Name),
    ("type T { f(a: Int", K::Name),
    ("fragment <T", K::Name),
  ] {
    assert!(oracle(src).is_err(), "{src}");
    assert!(
      !parse_document(src)
        .syntax()
        .descendants_with_tokens()
        .any(|element| matches!(element.kind(), K::Error | K::Gap)),
      "{src}: hole-free"
    );
    assert_eq!(
      project(&parse_document(src), src).expect_err(src).kind(),
      &ProjectErrorKind::UnexpectedChild {
        parent: K::Document,
        found,
      },
      "{src}: the refusal is the rubble's, not the list's"
    );
  }
  for src in ["import { A", "import { A }"] {
    assert!(oracle(src).is_err(), "{src}");
    assert_eq!(
      project(&parse_document(src), src).expect_err(src).kind(),
      &ProjectErrorKind::MissingChild {
        parent: K::ImportDefinition,
        wanted: "a module to import from",
      },
      "{src}: the refusal is the source's, not the list's"
    );
  }
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
  // The four positions the syntactic crate refuses a spelling at, each reached by a tree the
  // lossless parser never builds — the mutation law cannot see them — and each still accepting the
  // qualified spellings the parser accepts.
  let rule = |kind: ProjectErrorKind| matches!(kind, ProjectErrorKind::SemanticRule { .. });

  // An enum value spelled `true`, `false` or `null`, unqualified: executable grammar …
  for spelling in ["true", "false", "null"] {
    let text = std::format!("{{f(a:{spelling})}}");
    let (node, text) = executable_value(
      |tree| {
        tree.open(K::EnumValue).path(spelling).close();
      },
      &text,
    );
    assert!(rule(refuse_tree(node, &text)), "{text}");
  }
  // … and a first segment of a longer path.
  let (node, text) = executable_value(
    |tree| {
      tree.open(K::EnumValue).open(K::Path);
      tree
        .token(K::Name, "true")
        .token(K::PathSeparator, "::")
        .token(K::Name, "x");
      tree.close().close();
    },
    "{f(a:true::x)}",
  );
  assert!(rule(refuse_tree(node, &text)), "{text}");
  // … and the const grammar, through a const directive's argument.
  let (node, text) = scalar_with_literal(K::EnumValue, K::Name, "null");
  let text_owned = text.clone();
  let rebuilt = {
    let mut tree = Tree::new();
    tree.open(K::Document);
    tree.open(K::ScalarTypeDefinition);
    tree.token(K::Name, "scalar").token(K::Space, " ");
    tree.open(K::DefinitionName).token(K::Name, "S").close();
    tree.one_argument("n", |tree| {
      tree.open(K::EnumValue).path("null").close();
    });
    tree.close();
    tree.close();
    tree.finish()
  };
  drop(node);
  assert_eq!(rebuilt.text().to_string(), text_owned);
  assert!(rule(refuse_tree(rebuilt, &text_owned)), "{text_owned}");

  // A fragment spread whose target is an unqualified `on`.
  let text = "{...on}";
  let mut tree = Tree::new();
  tree.open(K::Document);
  tree.open(K::OperationDefinition);
  tree.open(K::SelectionSet);
  tree.token(K::LBrace, "{");
  tree.open(K::FragmentSpread);
  tree.token(K::Spread, "...");
  tree.open(K::TypePath).path("on").close();
  tree.close();
  tree.token(K::RBrace, "}");
  tree.close();
  tree.close();
  tree.close();
  let node = tree.finish();
  assert_eq!(node.text().to_string(), text);
  assert!(rule(refuse_tree(node, text)), "{text}");

  // The two name rules, for completeness of the four — their parser-built witnesses are
  // `a_fragment_named_on_refuses` and `an_enum_value_named_true_refuses`.
  for src in ["fragment on on T { f }", "enum E { null }"] {
    assert!(
      rule(
        project(&parse_document(src), src)
          .expect_err(src)
          .kind()
          .clone()
      ),
      "{src}"
    );
  }

  // The qualified spellings stay the parser's, in both value grammars and in a spread.
  for src in [
    "{ f(a: ::true, b: x::null, c: ns::on) }",
    "scalar S @k(a: ::false, b: y::true)",
    "{ ...ns::on }",
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
      builder.start_node(GraphQLxLang::kind_to_raw(root));
      if as_node {
        builder.start_node(foreign);
        builder.token(GraphQLxLang::kind_to_raw(K::Name), "x");
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
fn foreign_root_parse<'a>(src: &'a str, root: u16) -> smear::parser::graphqlx::lossless::Parse {
  use smear::parser::{
    graphqlx::lossless::{Brand, GraphqlxLosslessErrors, Lexer, LexerState},
    lossless::runner::finish_root,
  };
  use tokora::{
    InputRef, SimpleSpan,
    cache::DefaultCache,
    cst::{CstProfile, KindValidator, Sink, parse_lossless},
    emitter::Verbose,
  };

  type Lx<'a> = Lexer<'a, str>;
  type Em<'a> = Verbose<GraphqlxLosslessErrors<&'a str>, SimpleSpan, Brand>;
  type Ctx<'a> = (Sink<'a, Lx<'a>, Em<'a>>, DefaultCache<'a, Lx<'a>>);

  fn unmapped<T>(_: &T) -> u16 {
    0
  }

  let profile = CstProfile::new(
    unmapped as fn(&_) -> u16,
    KindValidator::accept_all(),
    GraphQLxLang::kind_to_raw(K::Error).0,
    GraphQLxLang::kind_to_raw(K::Gap).0,
  );
  let (cst, _) = parse_lossless::<Lx<'a>, Brand, Em<'a>, DefaultCache<'a, Lx<'a>>, (), _>(
    src,
    LexerState::default(),
    Em::new(),
    profile,
    DefaultCache::<'a, Lx<'a>>::default(),
    |_: &mut InputRef<'a, '_, Lx<'a>, Ctx<'a>, Brand>| Ok(()),
  );
  finish_root::<GraphQLxLang, Lx<'a>, Em<'a>>(cst, root, "a permissive profile")
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
  use smear::parser::graphqlx::lossless::{
    Verified, ast::ExecutableDocument as ExecutableDocumentNode,
    ast::TypeSystemDocument as TypeSystemDocumentNode,
  };

  for root in [GraphQLxLang::kind_to_raw(K::Name).0, 60_000] {
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
fn twin_container_parse(one: &str, kind: K) -> (String, smear::parser::graphqlx::lossless::Parse) {
  use smear::parser::{
    graphqlx::lossless::{Brand, GraphqlxLosslessErrors, Lexer, LexerState, profile},
    lossless::runner::finish_root,
  };
  use tokora::{InputRef, SimpleSpan, cache::DefaultCache, cst::Sink, emitter::Verbose};

  type Lx<'a> = Lexer<'a, str>;
  type Em<'a> = Verbose<GraphqlxLosslessErrors<&'a str>, SimpleSpan, Brand>;
  type Ctx<'a> = (Sink<'a, Lx<'a>, Em<'a>>, DefaultCache<'a, Lx<'a>>);

  let raw = |kind: K| GraphQLxLang::kind_to_raw(kind).0;
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

  fn mint<'a>(src: &'a str, shape: &[Replay]) -> smear::parser::graphqlx::lossless::Parse {
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
    finish_root::<GraphQLxLang, Lx<'a>, Em<'a>>(
      cst,
      GraphQLxLang::kind_to_raw(K::Root).0,
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
          let (ast, r) = smear::parser::graphqlx::lossless::project_executable_document_verified(
            smear::parser::graphqlx::lossless::Verified::new(&parse, &src)
              .expect("the pair verifies"),
          );
          (ast.definitions().len(), r)
        },
      ),
      _ => (
        project_type_system_document(&parse, &src).map(|_| ()),
        project_type_system_document_recovered(&parse, &src)
          .map(|(ast, r)| (ast.definitions().len(), r)),
        {
          let (ast, r) = smear::parser::graphqlx::lossless::project_type_system_document_verified(
            smear::parser::graphqlx::lossless::Verified::new(&parse, &src)
              .expect("the pair verifies"),
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
