#![cfg(all(feature = "graphqlx", feature = "rowan"))]

//! The mutation law: a falsifier for "a walk represents every non-trivia byte, or it refuses".
//!
//! Rounds 3, 4 and 5 of al8n/smear#58 each found the next place where a slot-and-guard walk's
//! *approximation* of a production and the production itself differ — a token folded by kind, a
//! child dropped by a wildcard, a shared walker taking the union of six vocabularies, a committing
//! prefix with no target, a dangling separator. A cell per finding pins the finding. This pins the
//! **class**: it perturbs real trees one child at a time and asks whether the projection still
//! agrees with the syntactic parser, or refuses.
//!
//! # The law
//!
//! For every hole-free corpus tree, every node, every non-trivia child, and five one-child
//! mutations, rebuild the tree and take its own text as the source. When the mutated tree's
//! trivia-stripped skeleton differs from the lossless re-parse's, no oracle applies — the
//! projection is a function of the *tree*, and the syntactic parse of the text is only an oracle
//! for trees that are parses: deleting `as` from `A as B` leaves a tree whose text `{ A B }`
//! re-parses to two members, and the projection's answer for the tree it was handed is right while
//! the parser's answer for the text is a different sentence. Those carry the no-panic obligation
//! only. Every **skeleton-faithful** one is held to the parser:
//!
//! 1. **Nothing lenient is missing** — the projection must equal the syntactic parse of the text
//!    exactly, spans included, or both must refuse.
//! 2. **A token in a [lenient position](LENIENT) is missing** — deleted by this mutation, or
//!    already absent from the corpus tree it was applied to. The oracle is then the syntactic parse
//!    of the text **with every such token restored**: each is one splice at a known offset, so the
//!    parser's spans map back through one delta per splice, and the projection must equal that
//!    remapped value — or both must refuse. This is the design's shape-faithful clause stated as an
//!    equation: the lenient projection answers what the parser answers for the text with the token
//!    put back, and nothing else. A projector that dropped a member whenever a closer was gone
//!    passes a lenient *bucket*; it cannot pass this.
//!
//! Every population and bucket below is asserted as a measured constant, and so is the exact
//! class → instance map of what the law still finds — which is empty — so the mutation space
//! cannot silently shrink into a gate that passes because it stopped looking.

use std::{collections::BTreeMap, path::PathBuf};

use rowan::{GreenNodeBuilder, Language, NodeOrToken};
use smear::parser::graphqlx::{
  GraphQLx,
  ast::{Document, ExecutableDocument, TypeSystemDocument},
  error::GraphqlxErrors,
  kinds::{GraphQLxLang, SyntaxKind as K},
  lossless::{
    SyntaxNode, ast::Document as DocumentNode, ast::ExecutableDocument as ExecutableDocumentNode,
    ast::TypeSystemDocument as TypeSystemDocumentNode, parse_document, parse_executable_document,
    parse_type_system_document,
  },
  syntactic::{GraphqlxLexer, document, executable_document, type_system_document},
};
use smear::parser::lossless::ast::CastNode;
use tokora::{Parse as _, Parser};

type SyntaxElement = rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<GraphQLxLang>>;

/// The six ignorable token images, as `graphqlx/lossless/project.rs` spells them.
const fn is_trivia(kind: K) -> bool {
  matches!(
    kind,
    K::Space | K::Tab | K::Newline | K::Comma | K::Comment | K::Bom
  )
}

/// Positions where an **absent** token still projects, and why.
///
/// The criterion al8n/smear#58 ruled: a position is lenient iff the token has **no AST image of
/// its own** *and* there is a witness that the lossless parser builds the tree **hole-free**
/// without it. The design's shape-faithful clause is what makes those project: only a token is
/// gone, every constituent the AST holds is still there, and the value is the one the parser
/// builds for the text with the token restored — which is exactly what the law checks.
///
/// Each row carries the token's spelling, because the restoration re-inserts it. **The rows are
/// derived elsewhere** — from the report-and-build census in `lossless_x_project.rs`, whose every
/// image-less row is witnessed by its own probe — and the projection's header states them once;
/// `the_lenient_rows_are_the_headers_table` asserts this table is that one. This law is the second
/// instrument: its [`STRICT_DELETIONS`] census would name any image-less position the derivation
/// missed. A token with an image is never lenient: `(NamedSpecifier, Path)` and
/// `(WherePredicate, TypePath)` are al8n/smear#58 round five's two findings, not rows.
const LENIENT: [(K, K, &str); 23] = [
  // The unclosed-delimiter family — `recover.rs`'s `unclosed_{object,parens}`, which report and
  // build no hole. `the_unclosed_brace_class_projects_although_the_parser_rejects_it` pins the
  // value.
  (K::SelectionSet, K::RBrace, "}"),
  (K::FieldsDefinition, K::RBrace, "}"),
  (K::InputFieldsDefinition, K::RBrace, "}"),
  (K::EnumValuesDefinition, K::RBrace, "}"),
  (K::RootOperationTypesDefinition, K::RBrace, "}"),
  (K::Arguments, K::RParen, ")"),
  // The `from` keyword — corpus `invalid_x_import_without_from`.
  (K::ImportDefinition, K::Name, "from"),
  // The `on` keyword — `selection.rs`'s `type_condition` reports a missing one and still builds
  // the node around the path it found. Codex round five's first finding; the round-five ruling
  // that it "stays strict" is withdrawn on the issue.
  (K::TypeCondition, K::Name, "on"),
  // The `on` of a directive definition and the `>` of an extension's generic arguments — found
  // by this law's strict-deletion census below and ruled on al8n/smear#58: the criterion is the
  // rule, applied uniformly.
  (K::DirectiveDefinition, K::Name, "on"),
  (K::ExtensionTypeGenerics, K::RAngle, ">"),
  // The rest of the `unclosed_*` family, derived from the report-and-build census rather than
  // from corpus deletions (Codex round six's first finding): each closer has no AST image and its
  // site builds the container hole-free without it.
  (K::ImportList, K::RBrace, "}"),
  (K::DefinitionTypeGenerics, K::RAngle, ">"),
  (K::TypeGenerics, K::RAngle, ">"),
  (K::ListType, K::RBracket, "]"),
  (K::SetType, K::RAngle, ">"),
  (K::MapType, K::RAngle, ">"),
  (K::ListValue, K::RBracket, "]"),
  (K::ObjectValue, K::RBrace, "}"),
  (K::SetValue, K::RBrace, "}"),
  (K::MapValue, K::RBrace, "}"),
  (K::VariablesDefinition, K::RParen, ")"),
  (K::ArgumentsDefinition, K::RParen, ")"),
  (K::ExecutableDefinitionTypeGenerics, K::RAngle, ">"),
];

fn raw(kind: K) -> rowan::SyntaxKind {
  GraphQLxLang::kind_to_raw(kind)
}

/// The trivia-stripped skeleton: kinds, nesting, and every non-trivia token's kind and text.
fn skeleton(node: &SyntaxNode, out: &mut String) {
  out.push('(');
  out.push_str(&std::format!("{:?}", node.kind()));
  for element in node.children_with_tokens() {
    match element {
      NodeOrToken::Node(child) => skeleton(&child, out),
      NodeOrToken::Token(token) if !is_trivia(token.kind()) => {
        out.push_str(&std::format!(" {:?}={:?}", token.kind(), token.text()));
      }
      NodeOrToken::Token(_) => {}
    }
  }
  out.push(')');
}

fn skeleton_of(node: &SyntaxNode) -> String {
  let mut out = String::new();
  skeleton(node, &mut out);
  out
}

/// Emit `element` into `builder` verbatim.
fn emit(builder: &mut GreenNodeBuilder<'_>, element: &SyntaxElement) {
  match element {
    NodeOrToken::Node(node) => {
      builder.start_node(raw(node.kind()));
      for child in node.children_with_tokens() {
        emit(builder, &child);
      }
      builder.finish_node();
    }
    NodeOrToken::Token(token) => builder.token(raw(token.kind()), token.text()),
  }
}

/// What one mutation does to one child of one node.
#[derive(Debug, Clone)]
enum Op {
  /// Drop the child at `index`.
  Delete { index: usize },
  /// Emit the child at `index` twice.
  Duplicate { index: usize },
  /// Emit the child at `index` after the next non-trivia element at `with`.
  Swap { index: usize, with: usize },
  /// Insert a copy of `source` in front of the child at `index`.
  Transplant { index: usize, source: SyntaxElement },
  /// Re-spell the `Name` token at `index`.
  Retext { index: usize, text: &'static str },
}

impl Op {
  const fn index(&self) -> usize {
    match self {
      Self::Delete { index }
      | Self::Duplicate { index }
      | Self::Swap { index, .. }
      | Self::Transplant { index, .. }
      | Self::Retext { index, .. } => *index,
    }
  }

  const fn label(&self) -> &'static str {
    match self {
      Self::Delete { .. } => "delete",
      Self::Duplicate { .. } => "duplicate",
      Self::Swap { .. } => "swap",
      Self::Transplant { .. } => "transplant",
      Self::Retext { .. } => "retext",
    }
  }
}

/// Rebuild `node` with `op` applied to the children of the node `path` leads to.
fn rebuild(builder: &mut GreenNodeBuilder<'_>, node: &SyntaxNode, path: &[usize], op: &Op) {
  builder.start_node(raw(node.kind()));
  let children: Vec<SyntaxElement> = node.children_with_tokens().collect();
  match path.split_first() {
    Some((&step, rest)) => {
      for (position, element) in children.iter().enumerate() {
        match element {
          NodeOrToken::Node(child) if position == step => rebuild(builder, child, rest, op),
          other => emit(builder, other),
        }
      }
    }
    None => apply(builder, &children, op),
  }
  builder.finish_node();
}

fn apply(builder: &mut GreenNodeBuilder<'_>, children: &[SyntaxElement], op: &Op) {
  for (position, element) in children.iter().enumerate() {
    if position != op.index() {
      // The swap's partner is emitted by the arm below, in the mutated order.
      if let Op::Swap { with, .. } = op
        && position == *with
      {
        continue;
      }
      emit(builder, element);
      continue;
    }
    match op {
      Op::Delete { .. } => {}
      Op::Duplicate { .. } => {
        emit(builder, element);
        emit(builder, element);
      }
      Op::Swap { with, .. } => {
        emit(builder, &children[*with]);
        emit(builder, element);
      }
      Op::Transplant { source, .. } => {
        emit(builder, source);
        emit(builder, element);
      }
      Op::Retext { text, .. } => {
        let NodeOrToken::Token(token) = element else {
          panic!("retext targets a token")
        };
        builder.token(raw(token.kind()), text);
      }
    }
  }
}

/// Which door a container node goes back through.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Door {
  Mixed,
  Executable,
  TypeSystem,
}

impl Door {
  fn container(self) -> K {
    match self {
      Self::Mixed => K::Document,
      Self::Executable => K::ExecutableDocument,
      Self::TypeSystem => K::TypeSystemDocument,
    }
  }

  fn parse(self, source: &str) -> SyntaxNode {
    match self {
      Self::Mixed => parse_document(source).syntax(),
      Self::Executable => parse_executable_document(source).syntax(),
      Self::TypeSystem => parse_type_system_document(source).syntax(),
    }
  }

  /// The syntactic parser's verdict and value for `source`, as the oracle.
  fn oracle(self, source: &str) -> Result<Answer<'_>, ()> {
    match self {
      Self::Mixed => oracle_mixed(source).map(Answer::Mixed).map_err(|_| ()),
      Self::Executable => oracle_executable(source)
        .map(Answer::Executable)
        .map_err(|_| ()),
      Self::TypeSystem => oracle_type_system(source)
        .map(Answer::TypeSystem)
        .map_err(|_| ()),
    }
  }

  /// The projection of `node`, which must be a container of this door's kind.
  fn project<'src>(self, node: SyntaxNode, source: &'src str) -> Result<Answer<'src>, String> {
    match self {
      Self::Mixed => DocumentNode::cast_node(node)
        .ok_or_else(|| "not a Document".to_string())?
        .to_ast(source)
        .map(Answer::Mixed)
        .map_err(|error| std::format!("{:?}", error.kind())),
      Self::Executable => ExecutableDocumentNode::cast_node(node)
        .ok_or_else(|| "not an ExecutableDocument".to_string())?
        .to_ast(source)
        .map(Answer::Executable)
        .map_err(|error| std::format!("{:?}", error.kind())),
      Self::TypeSystem => TypeSystemDocumentNode::cast_node(node)
        .ok_or_else(|| "not a TypeSystemDocument".to_string())?
        .to_ast(source)
        .map(Answer::TypeSystem)
        .map_err(|error| std::format!("{:?}", error.kind())),
    }
  }
}

#[derive(Debug, PartialEq)]
enum Answer<'src> {
  Mixed(Document<&'src str>),
  Executable(ExecutableDocument<&'src str>),
  TypeSystem(TypeSystemDocument<&'src str>),
}

fn oracle_mixed(src: &str) -> Result<Document<&str>, GraphqlxErrors<&str>> {
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

fn oracle_executable(src: &str) -> Result<ExecutableDocument<&str>, GraphqlxErrors<&str>> {
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

fn oracle_type_system(src: &str) -> Result<TypeSystemDocument<&str>, GraphqlxErrors<&str>> {
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

fn corpus() -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .expect("the GraphQLx corpus is readable")
    .map(|entry| entry.expect("a corpus entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
    .collect();
  files.sort();
  files
    .into_iter()
    .map(|path| {
      let name = path
        .file_name()
        .expect("a file name")
        .to_string_lossy()
        .to_string();
      let source = std::fs::read_to_string(&path).expect("a readable entry");
      (name, source)
    })
    .collect()
}

fn has_hole(node: &SyntaxNode) -> bool {
  node
    .descendants_with_tokens()
    .any(|element| matches!(element.kind(), K::Error | K::Gap))
}

/// Every `(node path, node)` in document order.
fn walk(node: &SyntaxNode, path: &mut Vec<usize>, out: &mut Vec<(Vec<usize>, SyntaxNode)>) {
  out.push((path.clone(), node.clone()));
  for (position, element) in node.children_with_tokens().enumerate() {
    if let NodeOrToken::Node(child) = element {
      path.push(position);
      walk(&child, path, out);
      path.pop();
    }
  }
}

/// One representative node per kind, first in document order — amendment 6's transplant source.
fn representatives(root: &SyntaxNode) -> BTreeMap<K, SyntaxNode> {
  let mut map = BTreeMap::new();
  for node in root.descendants() {
    map.entry(node.kind()).or_insert(node);
  }
  map
}

/// Every lenient token a tree lacks, as `(offset, row)` splices into its text.
///
/// A closer is missing when its container holds no token of that kind, and it is restored
/// directly after the container's last non-trivia token; a keyword is missing when its node holds
/// no `Name` of that spelling, and it is restored directly after the last non-trivia token in
/// front of the constituent it introduces — the source string for `from`, the type path for `on`.
/// A node without the constituent a restoration would be anchored on is left alone: its
/// projection refuses on its own.
fn restorations(root: &SyntaxNode) -> Vec<(usize, usize)> {
  let tokens: Vec<(usize, usize)> = root
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| !is_trivia(token.kind()))
    .map(|token| {
      let range = token.text_range();
      (usize::from(range.start()), usize::from(range.end()))
    })
    .collect();
  // The end of the last non-trivia token that ends at or before `offset`.
  let after = |offset: usize| {
    tokens
      .iter()
      .rev()
      .find(|(_, end)| *end <= offset)
      .map(|(_, end)| *end)
  };
  let mut splices = Vec::new();
  for node in root.descendants() {
    for (row, (parent, kind, text)) in LENIENT.iter().enumerate() {
      if node.kind() != *parent {
        continue;
      }
      let present = node.children_with_tokens().any(|element| match element {
        NodeOrToken::Token(token) => {
          token.kind() == *kind && (*kind != K::Name || token.text() == *text)
        }
        NodeOrToken::Node(_) => false,
      });
      if present {
        continue;
      }
      let anchor = match *kind {
        K::Name => {
          let before = match *parent {
            K::ImportDefinition => K::StringValue,
            K::DirectiveDefinition => K::DirectiveLocations,
            _ => K::TypePath,
          };
          node
            .children()
            .find(|child| child.kind() == before)
            .and_then(|child| after(usize::from(child.text_range().start())))
        }
        _ => after(usize::from(node.text_range().end())),
      };
      if let Some(offset) = anchor {
        splices.push((offset, core::cmp::Reverse(node.ancestors().count()), row));
      }
    }
  }
  // Equal offsets restore **innermost first**: `{ f(a: 1` needs `) }`, and ordering by table row
  // would write `} )` — an unrelated rejection that would let a wrong projection into the
  // both-refuse bucket. Codex round six's fourth finding.
  splices.sort_unstable();
  splices
    .into_iter()
    .map(|(offset, _, row)| (offset, row))
    .collect()
}

/// The text with every splice applied — a space and the token, so a keyword never fuses with the
/// name in front of it — and each splice's `(offset, width)` in the **restored** text.
fn restore(text: &str, splices: &[(usize, usize)]) -> (String, Vec<(usize, usize)>) {
  let mut restored = String::with_capacity(text.len() + 8 * splices.len());
  let mut placed = Vec::with_capacity(splices.len());
  let mut from = 0;
  for &(offset, row) in splices {
    restored.push_str(&text[from..offset]);
    let insert = std::format!(" {}", LENIENT[row].2);
    placed.push((restored.len(), insert.len()));
    restored.push_str(&insert);
    from = offset;
  }
  restored.push_str(&text[from..]);
  (restored, placed)
}

/// A position in the restored text, mapped back into the mutated one: one delta per splice before
/// it. A position **inside** a splice belongs to a composite span the restored token opened or
/// closed, and it lands where the tree's token extent puts that span's edge — an end on the splice's
/// own offset, the end of the last token before it; a start on the first token after it. That is
/// the composite-span convention the module header of the projection states, applied at the one
/// place a restored token moves an edge; nothing else is normalised.
fn unsplice(position: usize, is_start: bool, placed: &[(usize, usize)], starts: &[usize]) -> usize {
  let mut delta = 0;
  for &(offset, width) in placed {
    if position >= offset + width {
      delta += width;
    } else if position > offset {
      let anchor = offset - delta;
      return if is_start {
        starts
          .iter()
          .copied()
          .find(|&start| start >= anchor)
          .unwrap_or(anchor)
      } else {
        anchor
      };
    } else {
      break;
    }
  }
  position - delta
}

/// Every `SimpleSpan { start: …, end: … }` in a `Debug` image, mapped through [`unsplice`].
///
/// The AST has no span visitor, and its `Debug` is derived field by field, so the image is the
/// value with every span spelled one way; rewriting the numbers there and comparing images is the
/// same comparison `==` makes, with the parser's spans moved back to the text the tree has.
/// `starts` is the mutated text's non-trivia token starts, in order.
fn remap(image: &str, placed: &[(usize, usize)], starts: &[usize]) -> String {
  const OPEN: &str = "SimpleSpan { start: ";
  const MID: &str = ", end: ";
  let mut out = String::with_capacity(image.len());
  let mut rest = image;
  while let Some(at) = rest.find(OPEN) {
    out.push_str(&rest[..at + OPEN.len()]);
    rest = &rest[at + OPEN.len()..];
    let digits = rest
      .find(|c: char| !c.is_ascii_digit())
      .expect("a span's start");
    let start: usize = rest[..digits].parse().expect("a start");
    out.push_str(&unsplice(start, true, placed, starts).to_string());
    rest = &rest[digits..];
    assert!(rest.starts_with(MID), "a span's end follows its start");
    out.push_str(MID);
    rest = &rest[MID.len()..];
    let digits = rest
      .find(|c: char| !c.is_ascii_digit())
      .expect("a span's end");
    let end: usize = rest[..digits].parse().expect("an end");
    out.push_str(&unsplice(end, false, placed, starts).to_string());
    rest = &rest[digits..];
  }
  out.push_str(rest);
  out
}

/// The non-trivia token starts of a tree, in document order.
fn token_starts(root: &SyntaxNode) -> Vec<usize> {
  root
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| !is_trivia(token.kind()))
    .map(|token| usize::from(token.text_range().start()))
    .collect()
}

/// The classes the law still finds, as `(class, instances)` — **empty**, and asserted equal.
///
/// Round five closed at 64 instances in 14 classes and read them as three families of open work:
/// a reserved-spelling rule missing at four name positions (45), imports after definitions (6)
/// and duplicated members (13). Round six measured every one of them and found a single cause
/// instead: all 64 were mutations of the two corpus trees that are **themselves** lenient —
/// `invalid_unterminated_brace` (no `}`) and `invalid_x_import_without_from` (no `from`) — whose
/// text the parser rejects for the missing token whatever the mutation did. `type on { x: Int }`,
/// `import { A A } from "m"` and an import after an import are all sentences the parser accepts.
/// The oracle had no restored text to consult; now it does, and the class map is empty. A class
/// appearing here is a finding: name it rather than ledgering it.
const OPEN: &[(&str, usize)] = &[];

/// The `(entry, door)` pairs the law runs over: every hole-free parse of every corpus entry under
/// each of the three doors that builds that door's container.
const PAIRS: usize = 251;

/// Of [`PAIRS`], the ones whose unmutated tree already lacks a lenient token — every mutation of
/// them is compared against a restored text.
const LENIENT_BASES: usize = 5;

/// Mutations tried, per door, per operation — the population, cell by cell.
const TRIED: &[((&str, &str), usize)] = &[
  (("executable", "delete"), 1273),
  (("executable", "duplicate"), 1273),
  (("executable", "retext"), 1434),
  (("executable", "swap"), 669),
  (("executable", "transplant"), 13_750),
  (("mixed", "delete"), 3184),
  (("mixed", "duplicate"), 3184),
  (("mixed", "retext"), 4163),
  (("mixed", "swap"), 1640),
  (("mixed", "transplant"), 35_899),
  (("type-system", "delete"), 1881),
  (("type-system", "duplicate"), 1881),
  (("type-system", "retext"), 2679),
  (("type-system", "swap"), 955),
  (("type-system", "transplant"), 20_767),
];

/// Mutations tried in total.
const TRIED_TOTAL: usize = 94_632;

/// Skeleton-unfaithful: the no-panic obligation only.
const UNFAITHFUL: usize = 83_979;

/// Skeleton-faithful, nothing lenient missing, and the projection equal to the parse.
const EQUAL: usize = 8036;

/// Skeleton-faithful, nothing lenient missing, and both refusing.
const BOTH_REFUSE: usize = 2302;

/// Skeleton-faithful with a lenient token missing, and the projection equal to the restored parse.
const LENIENT_EQUAL: usize = 247;

/// Skeleton-faithful with a lenient token missing, and both refusing the restored text.
const LENIENT_BOTH_REFUSE: usize = 68;

/// How many restorations each [`LENIENT`] row contributed to [`LENIENT_EQUAL`], in table order —
/// the value witnesses this law finds.
///
/// The tenth row (an extension's `>`) has none and cannot: the list is hole-free without it only
/// at end of input, where the extension has no tail. The last thirteen — the closers the
/// report-and-build census added in round seven — have none from this corpus: deleting a closer
/// mid-document re-parses to a different skeleton, and only end of input keeps it faithful. Their
/// values are pinned by `every_unclosed_closer_projects_what_the_closed_text_parses_to` and
/// `nested_missing_closers_restore_innermost_first` instead.
const LENIENT_HITS: [usize; 23] = [
  82, 86, 6, 4, 6, 2, 42, 15, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

/// How many restorations each row contributed to [`LENIENT_BOTH_REFUSE`] — restored texts the
/// parser still rejects for another reason, and the projection refuses too. **Every row is
/// exercised** in one of the two: a row with neither would be an exemption no mutation reaches.
const LENIENT_REFUSED_HITS: [usize; 23] = [
  8, 22, 4, 6, 4, 4, 12, 2, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

/// Every faithful **deletion of a token** the parser and the projection both refuse, as
/// `(parent token-kind text, count)` — the census that enumerates the leniency criterion.
///
/// A position is lenient iff the token has no AST image and the lossless parser builds the tree
/// hole-free without it; a faithful deletion is exactly the second half, so any token here with
/// no image would be a lenient position still strict. Every entry left has an image: a directive
/// location, a generic parameter or argument name, a path segment. Round six's first run of this
/// census found two image-less ones — `(DirectiveDefinition, on)` and `(ExtensionTypeGenerics,
/// >)` — which are the last two [`LENIENT`] rows now.
const STRICT_DELETIONS: &[(&str, usize)] = &[
  ("DirectiveLocations Name ARGUMENT_DEFINITION", 2),
  ("DirectiveLocations Name INPUT_FIELD_DEFINITION", 2),
  ("DirectiveLocations Name OBJECT", 2),
  ("ExecutableDefinitionTypeGenerics Name T", 2),
  ("ExtensionTypeGenerics Name A", 6),
  ("ExtensionTypeGenerics Name B", 2),
  ("Path Name ns", 2),
];

#[derive(Default)]
struct Tally {
  tried: BTreeMap<(&'static str, &'static str), usize>,
  unfaithful: usize,
  equal: usize,
  both_refuse: usize,
  lenient_equal: usize,
  lenient_both_refuse: usize,
  lenient_hits: [usize; 23],
  lenient_refused_hits: [usize; 23],
  strict_deletions: BTreeMap<String, usize>,
  violations: Vec<String>,
}

fn door_label(door: Door) -> &'static str {
  match door {
    Door::Mixed => "mixed",
    Door::Executable => "executable",
    Door::TypeSystem => "type-system",
  }
}

#[test]
fn the_projection_agrees_with_the_parser_under_every_one_child_mutation() {
  let started = std::time::Instant::now();
  let mut tally = Tally::default();
  let mut pairs = 0usize;
  let mut lenient_bases = 0usize;

  for (name, source) in corpus() {
    for door in [Door::Mixed, Door::Executable, Door::TypeSystem] {
      let root = door.parse(&source);
      if has_hole(&root) {
        continue;
      }
      let Some(container) = root
        .children()
        .find(|child| child.kind() == door.container())
      else {
        continue;
      };
      pairs += 1;
      if !restorations(&container).is_empty() {
        lenient_bases += 1;
      }
      let reps = representatives(&container);
      let mut nodes = Vec::new();
      walk(&container, &mut Vec::new(), &mut nodes);

      for (path, node) in &nodes {
        let children: Vec<SyntaxElement> = node.children_with_tokens().collect();
        let live: Vec<usize> = children
          .iter()
          .enumerate()
          .filter(|(_, element)| !is_trivia(element.kind()))
          .map(|(position, _)| position)
          .collect();

        for (slot, &index) in live.iter().enumerate() {
          let mut ops = vec![Op::Delete { index }, Op::Duplicate { index }];
          if let Some(&next) = live.get(slot + 1) {
            ops.push(Op::Swap { index, with: next });
          }
          for source_node in reps.values() {
            ops.push(Op::Transplant {
              index,
              source: NodeOrToken::Node(source_node.clone()),
            });
          }
          if children[index].kind() == K::Name {
            let mine = children[index]
              .as_token()
              .expect("a Name is a token")
              .text();
            let other = container
              .descendants_with_tokens()
              .filter_map(|element| element.into_token())
              .find(|token| token.kind() == K::Name && token.text() != mine)
              .map(|token| token.text().to_string());
            for text in ["on", "true", "false", "null"] {
              ops.push(Op::Retext { index, text });
            }
            if let Some(other) = other {
              ops.push(Op::Retext {
                index,
                text: Box::leak(other.into_boxed_str()),
              });
            }
          }

          for op in &ops {
            *tally
              .tried
              .entry((door_label(door), op.label()))
              .or_default() += 1;
            let mut builder = GreenNodeBuilder::new();
            rebuild(&mut builder, &container, path, op);
            let mutated = SyntaxNode::new_root(builder.finish());
            let text = mutated.text().to_string();

            let reparsed = door.parse(&text);
            let faithful = reparsed
              .children()
              .find(|child| child.kind() == door.container())
              .is_some_and(|child| skeleton_of(&child) == skeleton_of(&mutated));
            let splices = restorations(&mutated);
            let starts = token_starts(&mutated);

            let projected = door.project(mutated, &text);
            if !faithful {
              tally.unfaithful += 1;
              continue;
            }
            let at = || {
              std::format!(
                "{name} [{door:?}] {:?} {} #{index} ({:?})",
                node.kind(),
                op.label(),
                children[index].kind()
              )
            };
            if splices.is_empty() {
              match (door.oracle(&text), projected) {
                (Ok(expected), Ok(actual)) if actual == expected => tally.equal += 1,
                (Ok(_), Ok(_)) => tally.violations.push(std::format!(
                  "{}: projected a value the parser does not produce for `{text}`",
                  at()
                )),
                (Ok(_), Err(refusal)) => tally.violations.push(std::format!(
                  "{}: the parser accepts `{text}` and the projection refused with {refusal}",
                  at()
                )),
                (Err(()), Err(_)) => {
                  tally.both_refuse += 1;
                  if let (Op::Delete { .. }, NodeOrToken::Token(token)) = (op, &children[index]) {
                    *tally
                      .strict_deletions
                      .entry(std::format!(
                        "{:?} {:?} {}",
                        node.kind(),
                        token.kind(),
                        token.text()
                      ))
                      .or_default() += 1;
                  }
                }
                (Err(()), Ok(_)) => tally.violations.push(std::format!(
                  "{}: the parser rejects `{text}` and the projection answered Ok",
                  at()
                )),
              }
              continue;
            }
            let (restored, placed) = restore(&text, &splices);
            match (door.oracle(&restored), projected) {
              (Ok(expected), Ok(actual)) => {
                if std::format!("{actual:?}")
                  == remap(&std::format!("{expected:?}"), &placed, &starts)
                {
                  tally.lenient_equal += 1;
                  for &(_, row) in &splices {
                    tally.lenient_hits[row] += 1;
                  }
                } else {
                  tally.violations.push(std::format!(
                    "{}: projected a value the parser does not produce for the restored \
                     `{restored}`",
                    at()
                  ));
                }
              }
              (Ok(_), Err(refusal)) => tally.violations.push(std::format!(
                "{}: the parser accepts the restored `{restored}` and the projection refused \
                 with {refusal}",
                at()
              )),
              (Err(()), Err(_)) => {
                tally.lenient_both_refuse += 1;
                for &(_, row) in &splices {
                  tally.lenient_refused_hits[row] += 1;
                }
              }
              (Err(()), Ok(_)) => tally.violations.push(std::format!(
                "{}: the parser rejects even the restored `{restored}` and the projection \
                 answered Ok",
                at()
              )),
            }
          }
        }
      }
    }
  }

  let elapsed = started.elapsed();
  let tried_total: usize = tally.tried.values().sum();
  println!(
    "MUTATION pairs={pairs} lenient_bases={lenient_bases} tried={tried_total} \
     unfaithful={} equal={} both_refuse={} lenient_equal={} lenient_both_refuse={} \
     lenient_hits={:?} refused_hits={:?} violations={} elapsed={elapsed:?}",
    tally.unfaithful,
    tally.equal,
    tally.both_refuse,
    tally.lenient_equal,
    tally.lenient_both_refuse,
    tally.lenient_hits,
    tally.lenient_refused_hits,
    tally.violations.len(),
  );
  for ((door, op), count) in &tally.tried {
    println!("TRIED {door} {op} {count}");
  }

  let mut classes: BTreeMap<String, usize> = BTreeMap::new();
  for violation in &tally.violations {
    *classes.entry(class_of(violation)).or_default() += 1;
  }
  for violation in tally.violations.iter().take(60) {
    println!("  {violation}");
  }

  // The class map first: if the law found something, that is the news, not a shifted bucket.
  let found: Vec<(&str, usize)> = classes
    .iter()
    .map(|(class, n)| (class.as_str(), *n))
    .collect();
  assert_eq!(
    found, OPEN,
    "the law's class -> instance map moved; a new class is a finding — name it"
  );
  assert_eq!(pairs, PAIRS, "the (entry, door) population moved");
  assert_eq!(
    lenient_bases, LENIENT_BASES,
    "the lenient-base population moved"
  );
  let tried: Vec<((&str, &str), usize)> = tally.tried.iter().map(|(k, v)| (*k, *v)).collect();
  assert_eq!(
    tried, TRIED,
    "a door or a mutation family tried a different population"
  );
  assert_eq!(tried_total, TRIED_TOTAL, "the mutation population moved");
  assert_eq!(tally.unfaithful, UNFAITHFUL, "the unfaithful bucket moved");
  assert_eq!(tally.equal, EQUAL, "the equal bucket moved");
  assert_eq!(
    tally.both_refuse, BOTH_REFUSE,
    "the both-refuse bucket moved"
  );
  assert_eq!(
    tally.lenient_equal, LENIENT_EQUAL,
    "the lenient-equal bucket moved"
  );
  assert_eq!(
    tally.lenient_both_refuse, LENIENT_BOTH_REFUSE,
    "the lenient both-refuse bucket moved"
  );
  assert_eq!(
    tally.lenient_hits, LENIENT_HITS,
    "a lenient row's hit count moved"
  );
  assert_eq!(
    tally.lenient_refused_hits, LENIENT_REFUSED_HITS,
    "a lenient row's refused-restoration count moved"
  );
  // A row's witness is its report-and-build probe (`lossless_x_project.rs`'s derivation), not
  // this law; a row with no restoration here is a position no corpus tree's one-child mutation
  // reaches, and the constants above say which.
  let strict: Vec<(&str, usize)> = tally
    .strict_deletions
    .iter()
    .map(|(deleted, n)| (deleted.as_str(), *n))
    .collect();
  assert_eq!(
    strict, STRICT_DELETIONS,
    "the strict-deletion census moved; an image-less token here is a lenient position left strict"
  );
  assert_eq!(
    tried_total,
    tally.unfaithful
      + tally.equal
      + tally.both_refuse
      + tally.lenient_equal
      + tally.lenient_both_refuse
      + tally.violations.len(),
    "every mutation lands in exactly one bucket"
  );
}

/// `<node kind> <mutation>` — position-independent, so the map survives a corpus edit.
fn class_of(violation: &str) -> String {
  let tail = violation
    .split_once("] ")
    .map_or(violation, |(_, rest)| rest);
  tail
    .split_whitespace()
    .take(2)
    .collect::<Vec<_>>()
    .join(" ")
}

/// Whether the projection of `tree` equals the parse of its text with every lenient token
/// restored, spans mapped back — the law's lenient clause, for one tree. Answers the restored text
/// beside the verdict.
fn agrees_restored(door: Door, tree: SyntaxNode) -> (String, bool) {
  let text = tree.text().to_string();
  let splices = restorations(&tree);
  let starts = token_starts(&tree);
  let (restored, placed) = restore(&text, &splices);
  let agree = match (door.oracle(&restored), door.project(tree, &text)) {
    (Ok(expected), Ok(actual)) => {
      std::format!("{actual:?}") == remap(&std::format!("{expected:?}"), &placed, &starts)
    }
    _ => false,
  };
  (restored, agree)
}

#[test]
fn nested_missing_closers_restore_innermost_first() {
  // Several lenient tokens missing at one offset: the restoration writes them innermost first, so
  // the restored text is the sentence the tree spells rather than an unrelated rejection.
  for (text, restored) in [
    ("{ f(a: 1", "{ f(a: 1 ) }"),
    ("{ f(a: [1", "{ f(a: [1 ] ) }"),
    ("{ f(a: {b: 1", "{ f(a: {b: 1 } ) }"),
    ("{ f(a: set {[1", "{ f(a: set {[1 ] } ) }"),
    ("type T { f: [A<B", "type T { f: [A<B > ] }"),
  ] {
    let tree = Door::Mixed.parse(text);
    assert!(!has_hole(&tree), "{text}: hole-free");
    let container = tree
      .children()
      .find(|child| child.kind() == K::Document)
      .expect("a document");
    let (got, agree) = agrees_restored(Door::Mixed, container);
    assert_eq!(got, restored, "{text}");
    assert!(agree, "{text}: the projection is the restored text's value");
  }
}

#[test]
fn a_lenient_token_deleted_from_an_already_lenient_base_restores_both() {
  // `{ f(a: 1)` already lacks its `}`; deleting the `)` too leaves two missing tokens at one
  // offset, restored `) }` — a base the law's corpus pairs reach only by chance.
  let base = Door::Mixed.parse("{ f(a: 1)");
  assert!(!has_hole(&base));
  let container = base
    .children()
    .find(|child| child.kind() == K::Document)
    .expect("a document");
  let mut nodes = Vec::new();
  walk(&container, &mut Vec::new(), &mut nodes);
  let (path, arguments) = nodes
    .iter()
    .find(|(_, node)| node.kind() == K::Arguments)
    .expect("an argument list");
  let index = arguments
    .children_with_tokens()
    .position(|element| element.kind() == K::RParen)
    .expect("its `)`");
  let mut builder = GreenNodeBuilder::new();
  rebuild(&mut builder, &container, path, &Op::Delete { index });
  let mutated = SyntaxNode::new_root(builder.finish());
  assert_eq!(mutated.text().to_string(), "{ f(a: 1");
  let (restored, agree) = agrees_restored(Door::Mixed, mutated);
  assert_eq!(restored, "{ f(a: 1 ) }");
  assert!(agree);
}

#[test]
fn the_lenient_rows_are_the_headers_table() {
  // The projection's header states the lenient set once — derived there from the
  // report-and-build census — and this table must be that set, row for row.
  let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../smear-parser/src/graphqlx/lossless/project.rs");
  let source = std::fs::read_to_string(path).expect("the projection's source");
  let mut header: Vec<(String, String)> = Vec::new();
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
    header.push((kind.to_string(), token.to_string()));
  }
  header.sort();
  let mut table: Vec<(String, String)> = LENIENT
    .iter()
    .map(|(parent, _, text)| (std::format!("{parent:?}"), (*text).to_string()))
    .collect();
  table.sort();
  assert_eq!(table, header);
}
