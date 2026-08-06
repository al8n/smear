//! The GraphQLx kind space, checked against the GraphQLx sources it was derived from.
//!
//! # What this file is for
//!
//! A kind space for a second dialect can be produced two ways: derived from that dialect's own
//! grammar, or copied from the first dialect's and adjusted. **The two are indistinguishable by
//! inspection and by every behavioural gate** — a copied space compiles, parses, round-trips and
//! prints identical golden trees for every construct the two dialects share. The four tests here
//! are the difference:
//!
//! - [`the_graphqlx_kind_space_is_well_formed`] inherits the substrate's three structural
//!   invariants by declaring the impl.
//! - [`every_kind_is_justified_by_a_graphqlx_source`] re-extracts the names GraphQLx's own
//!   `ast/**` and `syntactic/**` use and censuses the space against *that*, in both directions.
//! - [`the_two_spaces_diverge_inside_the_token_block`] is the coarse mechanical control: a space
//!   derived from GraphQLx's lexer cannot agree with GraphQL's through its image block.
//! - [`the_image_block_matches_the_graphqlx_lexer`] is the sharp one: the image block must *be*
//!   the GraphQLx lexer's variant list, in the lexer's declaration order, under the lexer's own
//!   names, with exactly one documented many-to-one fold.
//!
//! The last one is sharp because of two things measured rather than assumed. Copying GraphQL's
//! twenty-five-entry image block and appending GraphQLx's seven extras — with the enum reordered
//! to match, so the space stays well-formed — passes the other three tests; only the **order**
//! assertion catches it. And renaming one image to another dialect's word for it, applied
//! consistently to the space, to [`TOKEN_IMAGES`] and to [`image_of`], passes even that; only the
//! **name** assertion against [`RENAMED_IMAGES`] catches that one.
#![cfg(all(feature = "rowan", feature = "graphqlx"))]

use std::{
  collections::{BTreeMap, BTreeSet},
  fs,
  path::{Path, PathBuf},
};

use smear::parser::{
  graphqlx::kinds::SyntaxKind as X,
  lexer::graphqlx::lossless::{LosslessLexer, LosslessToken, LosslessTokenKind},
  lossless::{KindSpace, test_support::assert_kind_space_is_well_formed},
};

// ---------------------------------------------------------------------------------------------
// The four pinned lists.
// ---------------------------------------------------------------------------------------------

/// The image block, in `SyntaxKind::ALL` order.
///
/// Pinned rather than sliced out of `ALL` so that moving a node kind into the block, or an image
/// out of it, is a diff in this file too. [`the_image_block_matches_the_graphqlx_lexer`] proves
/// the list *is* `ALL`'s first `IMAGE_BLOCK` entries and that they are the lexer's.
const TOKEN_IMAGES: &[&str] = &[
  "Asterisk",
  "At",
  "Dollar",
  "FatArrow",
  "LAngle",
  "RAngle",
  "LParen",
  "RParen",
  "Spread",
  "Colon",
  "Equal",
  "LBracket",
  "RBracket",
  "LBrace",
  "RBrace",
  "Pipe",
  "Bang",
  "Ampersand",
  "Plus",
  "Minus",
  "PathSeparator",
  "Name",
  "Float",
  "Int",
  "InlineString",
  "BlockString",
  "Bom",
  "Comma",
  "Space",
  "Tab",
  "Newline",
  "Comment",
];

/// The bookkeeping triple, whose names and positions the substrate's `KindSpace` fixes.
const BOOKKEEPING: &[&str] = &["Error", "Gap", "Root"];

/// Every image the tree spells differently from the lexer, and why.
///
/// The list is short on purpose: an image's name is the lexer's unless GraphQLx's *own* grammar
/// has a better word for it, and "GraphQL calls it that" is not a reason. Checked exhaustively by
/// [`the_image_block_matches_the_graphqlx_lexer`], so a rename that is merely consistent with
/// itself still has to be justified here.
const RENAMED_IMAGES: &[(&str, &str, &str)] = &[
  (
    "Identifier",
    "Name",
    "graphqlx::syntactic::name builds graphqlx::ast::Name from it, and so does every one of the \
     forty-four contextual keywords",
  ),
  (
    "CarriageReturn",
    "Newline",
    "one of three spellings of a line terminator; the tree keeps the bytes verbatim",
  ),
  (
    "CarriageReturnAndNewline",
    "Newline",
    "the third spelling of the same",
  ),
];

/// Node kinds whose name is not, literally, a name GraphQLx's own sources use.
///
/// **The derivation produced none, and that is the finding, not an accident.** Every node kind in
/// the space is spelled exactly as some `graphqlx/ast/**` declaration, some shared carrier that
/// module binds, or some `graphqlx/syntactic/**` public production. The list stays here — rather
/// than the filter being deleted as dead code — because it is the door a future kind would have to
/// come through, and having to write the reason down is the point.
const KINDS_WITH_NO_NAMED_SOURCE: &[(&str, &str)] = &[];

/// GraphQLx sources that deliberately have no kind of their own, each with the reason.
///
/// Three rules cover almost all of it — a choice is not a region, one token is not a region, and
/// constness is a parse-time flavour rather than a shape — but each application is written out,
/// because "it follows from a rule" is exactly the claim a census exists to check.
const SOURCES_WITH_NO_KIND: &[(&str, &str)] = &[
  // ---- a choice is not a region: every alternative is already its own node ----
  (
    "Type",
    "a choice of DefinitionTypePath | ListType | SetType | MapType; the tree records which won",
  ),
  (
    "Ty",
    "the `ty` production's PascalCase; the same choice as `Type`",
  ),
  (
    "Value",
    "the `value` production; a choice over the eleven InputValue alternatives",
  ),
  (
    "InputValue",
    "the AST enum behind `value`; the same choice, and each arm is a *Value node",
  ),
  (
    "Selection",
    "a choice of Field | FragmentSpread | InlineFragment",
  ),
  (
    "TypeDefinition",
    "a choice over the six *TypeDefinition nodes",
  ),
  (
    "TypeExtension",
    "a choice over the six *TypeExtension nodes",
  ),
  (
    "TypeSystemDefinition",
    "a choice of TypeDefinition | DirectiveDefinition | SchemaDefinition",
  ),
  (
    "TypeSystemExtension",
    "a choice of TypeExtension | SchemaExtension",
  ),
  (
    "TypeSystemDefinitionOrExtension",
    "a choice of the two above",
  ),
  (
    "Definition",
    "a choice of TypeSystemDefinition | ExecutableDefinition",
  ),
  (
    "DefinitionOrExtension",
    "a choice of Definition | TypeSystemExtension",
  ),
  (
    "ExecutableDefinition",
    "a choice of OperationDefinition | FragmentDefinition",
  ),
  ("ImportClause", "a choice of ImportList | WildcardSpecifier"),
  (
    "ImportMember",
    "a choice of NamedSpecifier | WildcardSpecifier",
  ),
  (
    "ImportOrDefinitionOrExtension",
    "the `document` entry dispatcher; a choice, and the winner is the child of Document",
  ),
  (
    "ImportOrExecutableDefinition",
    "the same dispatcher for ExecutableDocument",
  ),
  (
    "ImportOrTypeSystemDefinitionOrExtension",
    "the same dispatcher for TypeSystemDocument",
  ),
  // ---- one token is not a region: the parent already says what it is ----
  (
    "Name",
    "one identifier token; `Name` is the image kind in the token block, not a node",
  ),
  (
    "OperationType",
    "one `query`/`mutation`/`subscription` identifier token inside OperationDefinition",
  ),
  (
    "Location",
    "one identifier token inside DirectiveLocations, which exposes them as a token run",
  ),
  (
    "ExtensionTypeParam",
    "one identifier token inside ExtensionTypeGenerics",
  ),
  (
    "Description",
    "one string token; it is a bare token child of the definition's own retro-wrapped node, \
     while every string *value* is a StringValue node, so the two can never be confused",
  ),
  // ---- a wrapper whose parts are already nodes ----
  (
    "Constrained",
    "`WhereClause? Target`; the clause and the target are siblings, both already nodes",
  ),
  (
    "ConstrainedFieldsDefinition",
    "the `Constrained` alias over FieldsDefinition",
  ),
  (
    "ConstrainedInputFieldsDefinition",
    "the `Constrained` alias over InputFieldsDefinition",
  ),
  (
    "Described",
    "`Description? Target`; the description is a token child of the target's own node",
  ),
  (
    "ExecutableDefinitionHeader",
    "`ExecutableDefinitionTypeGenerics? ExecutableDefinitionName`; both halves are nodes",
  ),
  (
    "NamedOperationDefinition",
    "the named form of OperationDefinition; one production, one node, and the operation \
     keyword is what tells the two forms apart",
  ),
  // ---- an alias of a node that already exists ----
  (
    "FragmentTypePath",
    "`= TypePath` (graphqlx/ast/generic.rs:93): the same production and the same node",
  ),
  (
    "ArgumentList",
    "the shared carrier behind Arguments; the kind takes the GraphQLx alias's name",
  ),
  (
    "DefaultInputValue",
    "the AST alias for the `default_value` production; the kind takes the production's name",
  ),
  (
    "List",
    "the AST alias for `list_value`; the kind is ListValue, which cannot be misread as ListType",
  ),
  (
    "Set",
    "the AST alias for `set_value`; the kind is SetValue, against SetType",
  ),
  (
    "Map",
    "the AST alias for `map_value`; the kind is MapValue, against MapType",
  ),
  (
    "Object",
    "the AST alias for `object_value`; the kind is ObjectValue, against ObjectTypeDefinition",
  ),
  (
    "Implements",
    "the production's name; the node takes its AST carrier's, ImplementInterfaces",
  ),
  (
    "UnionMembers",
    "the production's name; the node takes its AST carrier's, UnionMemberTypes",
  ),
  (
    "InlineStringValue",
    "a StringValue node whose token child is InlineString; the import's `from` clause is the \
     only position that narrows to it, and it narrows on the token, not on the node",
  ),
  (
    "BlockStringValue",
    "a StringValue node whose token child is BlockString, for the same reason",
  ),
  // ---- not a grammar production at all ----
  ("DefaultVec", "the AST's default container type parameter"),
];

// ---------------------------------------------------------------------------------------------
// The tests.
// ---------------------------------------------------------------------------------------------

/// The GraphQLx space satisfies the same contract GraphQL's does.
///
/// Inherited by declaring the impl — the substrate's whole point. If this needed its own three
/// hand-written properties, the contract would not have been worth lifting.
#[test]
fn the_graphqlx_kind_space_is_well_formed() {
  assert_kind_space_is_well_formed::<X>();
  assert_eq!(X::NAME, "graphqlx");
  assert_eq!(<X as KindSpace>::ERROR, X::Error);
  assert_eq!(<X as KindSpace>::GAP, X::Gap);
  assert_eq!(<X as KindSpace>::ROOT, X::Root);

  // The inherent `raw`/`from_raw` and the trait's are the same function, not two that agree by
  // luck: the impl forwards, and a forward that resolved to itself would recurse forever here.
  for kind in X::ALL {
    assert_eq!(<X as KindSpace>::raw(*kind), kind.raw());
    assert_eq!(<X as KindSpace>::from_raw(kind.raw()), Some(*kind));
  }
}

/// Every kind the space declares has a GraphQLx source anchor, and every anchor has a kind.
///
/// **This is the "derived, not diffed" check.** [`source_node_names`] reads
/// `smear/src/parser/graphqlx/ast/**` and `smear/src/parser/graphqlx/syntactic/**` and extracts the
/// names GraphQLx's own grammar uses; the kind space is compared against *that*, never against
/// GraphQL's. A kind copied from GraphQL for a production GraphQLx does not have lands in
/// `kinds_without_a_source`; a GraphQLx production with no kind lands in `sources_without_a_kind`
/// and is a hole the productions would later paper over.
#[test]
fn every_kind_is_justified_by_a_graphqlx_source() {
  let kinds: BTreeSet<String> = X::ALL
    .iter()
    .map(|k| format!("{k:?}"))
    .filter(|n| !TOKEN_IMAGES.contains(&n.as_str()) && !BOOKKEEPING.contains(&n.as_str()))
    .collect();
  let sources = source_node_names();

  let kinds_without_a_source: Vec<_> = kinds
    .difference(&sources)
    .filter(|n| !KINDS_WITH_NO_NAMED_SOURCE.iter().any(|(k, _)| k == *n))
    .collect();
  assert!(
    kinds_without_a_source.is_empty(),
    "these kinds name nothing in graphqlx's own grammar — a copied kind looks exactly like \
     this: {kinds_without_a_source:?}"
  );

  let sources_without_a_kind: Vec<_> = sources
    .difference(&kinds)
    .filter(|n| !SOURCES_WITH_NO_KIND.iter().any(|(s, _)| s == *n))
    .collect();
  assert!(
    sources_without_a_kind.is_empty(),
    "graphqlx declares these and the kind space cannot represent them: {sources_without_a_kind:?}"
  );

  // Both exception lists are a census, not a filter: an entry that stopped applying would sit
  // here forever and quietly widen the hole the next one is allowed through.
  for (kind, _) in KINDS_WITH_NO_NAMED_SOURCE {
    assert!(
      kinds.contains(*kind),
      "{kind} is excused from needing a source and is not a kind"
    );
  }
  for (source, _) in SOURCES_WITH_NO_KIND {
    assert!(
      sources.contains(*source),
      "{source} is excused from needing a kind and is not a graphqlx source name"
    );
    assert!(
      !kinds.contains(*source),
      "{source} is excused from needing a kind and has one"
    );
  }

  // The positive control. Both differences being empty is only evidence if the two sets are
  // non-trivial and actually overlap.
  assert!(
    kinds.len() >= 60,
    "only {} node kinds; the extraction is broken",
    kinds.len()
  );
  assert!(
    sources.len() >= 60,
    "only {} source names; the extraction is broken",
    sources.len()
  );
  assert!(kinds.intersection(&sources).count() >= 50);
}

/// The two dialects' spaces diverge inside the token-image block, not after it.
///
/// **The mechanical anti-diff control, and it is derivation-agnostic.** GraphQLx's lexer has seven
/// token images GraphQL's does not — `<`, `>`, `::`, `=>`, `*`, `+`, `-` — and the bookkeeping
/// triple owns the tail, so those seven had nowhere to go but *inside* the image block. A space
/// that still agrees with GraphQL's once GraphQLx's images have run out either appended the seven
/// after `Root`, which breaks the substrate's invariant, or left them out, in which case the token
/// mapper cannot be written.
///
/// This test does not say the spaces *should* diverge early. It says that a space derived from the
/// GraphQLx lexer *does*, and that a space that does not was not derived.
#[cfg(feature = "graphql")]
#[test]
fn the_two_spaces_diverge_inside_the_token_block() {
  use smear::parser::graphql::kinds::SyntaxKind as G;

  let g: Vec<String> = G::ALL.iter().map(|k| format!("{k:?}")).collect();
  let x: Vec<String> = X::ALL.iter().map(|k| format!("{k:?}")).collect();

  let first_difference = g
    .iter()
    .zip(&x)
    .position(|(a, b)| a != b)
    .unwrap_or(g.len().min(x.len()));

  assert!(
    first_difference < X::IMAGE_BLOCK,
    "the two kind spaces agree through index {first_difference}; graphqlx's image block is \
     {} wide and holds seven images graphql has no counterpart for, so agreeing that far means \
     the space was appended to graphql's rather than derived from graphqlx's lexer",
    X::IMAGE_BLOCK
  );
}

/// Every image the GraphQLx lexer can emit has exactly one kind, no kind claims to be an image the
/// lexer cannot emit, and the block is in the lexer's declaration order.
///
/// **The sharp anti-diff test.** The two weaker forms — "every image is covered" and "the spaces
/// differ somewhere" — both pass for a space built by copying another dialect's image block and
/// appending GraphQLx's seven extras to it. Requiring the block to *be* the lexer's list in the
/// lexer's order does not.
///
/// The trivia partition is the lexer's answer too: it comes from calling
/// `LosslessToken::is_trivia` on a sample token per kind, not from a second list here.
///
/// **The fold is stated, not absorbed.** Exactly one tree kind may have more than one lexer
/// preimage, it must be `Newline`, and it must have exactly the three line terminators.
///
/// The bound worth knowing: a variant appended to `LosslessTokenKind` *after* `BlockString` is not
/// caught here, because a `#[non_exhaustive]` enum cannot be matched exhaustively from another
/// crate. The lexer's own `lossless_token_kind_census` guards its list; this guards the mapping.
#[test]
fn the_image_block_matches_the_graphqlx_lexer() {
  // Declaration order, proven below rather than asserted in prose.
  const LEXER_KINDS: &[LosslessTokenKind] = &[
    LosslessTokenKind::Asterisk,
    LosslessTokenKind::At,
    LosslessTokenKind::Bom,
    LosslessTokenKind::Dollar,
    LosslessTokenKind::FatArrow,
    LosslessTokenKind::LAngle,
    LosslessTokenKind::RAngle,
    LosslessTokenKind::LParen,
    LosslessTokenKind::RParen,
    LosslessTokenKind::Spread,
    LosslessTokenKind::Colon,
    LosslessTokenKind::Equal,
    LosslessTokenKind::LBracket,
    LosslessTokenKind::RBracket,
    LosslessTokenKind::LBrace,
    LosslessTokenKind::RBrace,
    LosslessTokenKind::Pipe,
    LosslessTokenKind::Bang,
    LosslessTokenKind::Ampersand,
    LosslessTokenKind::Plus,
    LosslessTokenKind::Minus,
    LosslessTokenKind::PathSeparator,
    LosslessTokenKind::Comma,
    LosslessTokenKind::Space,
    LosslessTokenKind::Tab,
    LosslessTokenKind::Newline,
    LosslessTokenKind::CarriageReturn,
    LosslessTokenKind::CarriageReturnAndNewline,
    LosslessTokenKind::Comment,
    LosslessTokenKind::Identifier,
    LosslessTokenKind::Float,
    LosslessTokenKind::Int,
    LosslessTokenKind::InlineString,
    LosslessTokenKind::BlockString,
  ];

  for (index, kind) in LEXER_KINDS.iter().enumerate() {
    assert_eq!(
      *kind as u16, index as u16,
      "LEXER_KINDS is not the lexer's declaration order at {kind:?}"
    );
  }

  // Walk the lexer's order, split by the lexer's own trivia predicate, emitting each tree kind
  // the first time it is reached. Non-trivia first, then trivia: that is the block's layout, and
  // within each half the lexer's relative order is preserved.
  let samples = lexed_samples();
  let mut non_trivia: Vec<X> = Vec::new();
  let mut trivia: Vec<X> = Vec::new();
  let mut preimages: BTreeMap<String, Vec<LosslessTokenKind>> = BTreeMap::new();

  for kind in LEXER_KINDS {
    let token = samples
      .get(kind)
      .unwrap_or_else(|| panic!("no snippet in this test makes the lexer emit {kind:?}"));

    let tree = image_of(*kind);
    preimages
      .entry(format!("{tree:?}"))
      .or_default()
      .push(*kind);

    let half = if token.is_trivia() {
      &mut trivia
    } else {
      &mut non_trivia
    };
    if !half.contains(&tree) {
      half.push(tree);
    }
  }

  let derived: Vec<X> = non_trivia.into_iter().chain(trivia).collect();
  assert_eq!(
    derived.as_slice(),
    &X::ALL[..X::IMAGE_BLOCK],
    "the image block is not the graphqlx lexer's variant list in the lexer's order"
  );

  // The pinned list is the block, by name.
  let block: Vec<String> = X::ALL[..X::IMAGE_BLOCK]
    .iter()
    .map(|k| format!("{k:?}"))
    .collect();
  let pinned: Vec<String> = TOKEN_IMAGES.iter().map(|n| (*n).to_string()).collect();
  assert_eq!(block, pinned, "TOKEN_IMAGES drifted from the block");

  // Nothing after the block claims to be an image.
  for kind in &X::ALL[X::IMAGE_BLOCK..] {
    assert!(
      !derived.contains(kind),
      "{kind:?} sits outside the image block and is an image"
    );
  }

  // The tree's word for each image is the lexer's word, and every departure is in RENAMED_IMAGES
  // with a reason. Without this, adopting another dialect's vocabulary for a GraphQLx image —
  // calling `InlineString` `String`, say — is internally consistent and reds nothing: the block
  // still covers every image, in the lexer's order, with one fold.
  for kind in LEXER_KINDS {
    let lexer_name = format!("{kind:?}");
    let tree_name = format!("{:?}", image_of(*kind));
    let expected = match RENAMED_IMAGES
      .iter()
      .find(|(from, _, _)| *from == lexer_name)
    {
      Some((_, to, _)) => (*to).to_string(),
      None => lexer_name.clone(),
    };
    assert_eq!(
      tree_name, expected,
      "the tree calls the lexer's {lexer_name} image {tree_name}; an unlisted rename is another \
       dialect's vocabulary leaking in"
    );
  }
  for (from, _, _) in RENAMED_IMAGES {
    assert!(
      LEXER_KINDS.iter().any(|k| format!("{k:?}") == *from),
      "{from} is excused from matching its lexer name and is not a lexer image"
    );
  }

  // The fold, stated.
  let folded: Vec<_> = preimages.iter().filter(|(_, ks)| ks.len() > 1).collect();
  assert_eq!(
    folded.len(),
    1,
    "exactly one image mapping may be many-to-one; found {folded:?}"
  );
  let (name, sources) = folded[0];
  assert_eq!(name, "Newline");
  assert_eq!(
    sources.as_slice(),
    &[
      LosslessTokenKind::Newline,
      LosslessTokenKind::CarriageReturn,
      LosslessTokenKind::CarriageReturnAndNewline,
    ]
  );
}

// ---------------------------------------------------------------------------------------------
// Support.
// ---------------------------------------------------------------------------------------------

/// The tree kind for one lexer image.
///
/// The image half of what the dialect's `kind_map` will be. `Identifier` becomes `Name` because
/// that is what GraphQLx's grammar calls it; the three line terminators become `Newline` because
/// they are three spellings of one lexical concept and the tree keeps the text verbatim.
fn image_of(kind: LosslessTokenKind) -> X {
  match kind {
    LosslessTokenKind::Asterisk => X::Asterisk,
    LosslessTokenKind::At => X::At,
    LosslessTokenKind::Bom => X::Bom,
    LosslessTokenKind::Dollar => X::Dollar,
    LosslessTokenKind::FatArrow => X::FatArrow,
    LosslessTokenKind::LAngle => X::LAngle,
    LosslessTokenKind::RAngle => X::RAngle,
    LosslessTokenKind::LParen => X::LParen,
    LosslessTokenKind::RParen => X::RParen,
    LosslessTokenKind::Spread => X::Spread,
    LosslessTokenKind::Colon => X::Colon,
    LosslessTokenKind::Equal => X::Equal,
    LosslessTokenKind::LBracket => X::LBracket,
    LosslessTokenKind::RBracket => X::RBracket,
    LosslessTokenKind::LBrace => X::LBrace,
    LosslessTokenKind::RBrace => X::RBrace,
    LosslessTokenKind::Pipe => X::Pipe,
    LosslessTokenKind::Bang => X::Bang,
    LosslessTokenKind::Ampersand => X::Ampersand,
    LosslessTokenKind::Plus => X::Plus,
    LosslessTokenKind::Minus => X::Minus,
    LosslessTokenKind::PathSeparator => X::PathSeparator,
    LosslessTokenKind::Comma => X::Comma,
    LosslessTokenKind::Space => X::Space,
    LosslessTokenKind::Tab => X::Tab,
    LosslessTokenKind::Newline
    | LosslessTokenKind::CarriageReturn
    | LosslessTokenKind::CarriageReturnAndNewline => X::Newline,
    LosslessTokenKind::Comment => X::Comment,
    LosslessTokenKind::Identifier => X::Name,
    LosslessTokenKind::Float => X::Float,
    LosslessTokenKind::Int => X::Int,
    LosslessTokenKind::InlineString => X::InlineString,
    LosslessTokenKind::BlockString => X::BlockString,
    // `LosslessTokenKind` is `#[non_exhaustive]`, so this arm is forced. It is a panic rather
    // than a fallback kind: an image with no tree kind must stop the suite, not land in the tree
    // as something else.
    other => panic!("the graphqlx lexer emits {other:?} and the kind space does not name it"),
  }
}

/// One token per lexer kind, **lexed** rather than constructed.
///
/// Two reasons this runs the lexer instead of building tokens by hand. The narrow one is that the
/// string literals' constructors are `pub(crate)`, so they cannot be built from here at all. The
/// broad one is better: a hand-built token proves only that a variant exists, while a lexed one
/// proves the GraphQLx lexer *emits* it — which is what the image block claims about all
/// thirty-four of them. One snippet per image, so a snippet that lexes into something unexpected
/// shows up as a missing kind rather than being absorbed by a neighbour.
fn lexed_samples() -> BTreeMap<LosslessTokenKind, LosslessToken<&'static str>> {
  use tokora::lexer::Lexer as _;

  const SNIPPETS: &[&str] = &[
    "*",
    "@",
    "\u{FEFF}",
    "$",
    "=>",
    "<",
    ">",
    "(",
    ")",
    "...",
    ":",
    "=",
    "[",
    "]",
    "{",
    "}",
    "|",
    "!",
    "&",
    "+",
    "-",
    "::",
    ",",
    " ",
    "\t",
    "\n",
    "\r",
    "\r\n",
    "# c",
    "x",
    "1.0",
    "1",
    "\"s\"",
    "\"\"\"b\"\"\"",
  ];

  let mut samples = BTreeMap::new();
  for snippet in SNIPPETS {
    let mut lexer = LosslessLexer::<&str>::new(snippet);
    while let Some(item) = lexer.lex() {
      let token =
        item.unwrap_or_else(|e| panic!("the sample snippet {snippet:?} does not lex: {e:?}"));
      samples.entry(token.kind()).or_insert(token);
    }
  }
  samples
}

/// The names GraphQLx's own grammar uses, extracted from `graphqlx/ast/**` and
/// `graphqlx/syntactic/**`.
///
/// # The four extraction rules, and why each one
///
/// 1. **`pub struct` / `pub enum` / `pub type` in `ast/**`** — what GraphQLx declares.
/// 2. **`crate::<module>::Name` in `ast/**`, excluding `crate::graphqlx::…`** — the shared carriers
///    GraphQLx *binds*. Without this, `ListType`, `SetType`, `MapType`, `TypeGenerics` and
///    `DefinitionTypePath` would look unanchored: GraphQLx names them in an enum payload and a type
///    alias rather than declaring them. The `graphqlx::` exclusion keeps the dialect marker and the
///    module's own re-references out.
/// 3. **`pub fn` at column zero in `syntactic/**`** — a top-level public production. Column zero
///    rather than any indentation, because `pub fn graphqlx` inside a `macro_rules!` body is an
///    entry point on an AST type, not a production of its own.
/// 4. **A `*_parser!` invocation at column zero whose production is `pub`** — the macro-declared
///    productions, which are most of them. `pub` rather than any visibility, because a
///    `pub(crate)` or private production is a fused helper (`committed_selection_set`,
///    `*_after_keyword`) that a public production wraps.
///
/// # And three normalisations, each undoing a naming convention rather than a distinction
///
/// - a leading `Try` — the declining twin of the same production;
/// - a leading `Const` — constness is a parse-time flavour, and `[1, 2]` is one syntax;
/// - a leading `Described` — a description is a token child of the definition's own node.
///
/// Applied repeatedly, so `TryConstX` reaches `X`.
fn source_node_names() -> BTreeSet<String> {
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/parser/graphqlx");
  let ast = rust_files(&root.join("ast"));
  let syntactic = rust_files(&root.join("syntactic"));
  assert!(
    ast.len() >= 8 && syntactic.len() >= 20,
    "the graphqlx sources moved: {} ast files, {} syntactic files",
    ast.len(),
    syntactic.len()
  );

  let mut names = BTreeSet::new();

  for path in &ast {
    let text = fs::read_to_string(path).expect("a graphqlx ast file");
    for line in text.lines() {
      let trimmed = line.trim_start();
      for keyword in ["pub struct ", "pub enum ", "pub type "] {
        if let Some(rest) = trimmed.strip_prefix(keyword) {
          insert(&mut names, leading_ident(rest));
        }
      }
      for name in bound_carriers(line) {
        insert(&mut names, Some(name));
      }
    }
  }

  for path in &syntactic {
    let text = fs::read_to_string(path).expect("a graphqlx syntactic file");
    let lines: Vec<&str> = text.lines().collect();
    for (index, line) in lines.iter().enumerate() {
      if let Some(rest) = line.strip_prefix("pub fn ") {
        insert(&mut names, leading_ident(rest).map(|n| pascal(&n)));
      }
      if let Some(production) = parser_macro_production(&lines, index) {
        insert(&mut names, Some(pascal(&production)));
      }
    }
  }

  names
}

/// Every `.rs` file under `dir`, recursively, except the per-module test files.
fn rust_files(dir: &Path) -> Vec<PathBuf> {
  let mut out = Vec::new();
  let entries = fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()));
  for entry in entries {
    let path = entry.expect("a directory entry").path();
    if path.is_dir() {
      out.extend(rust_files(&path));
    } else if path.extension().is_some_and(|e| e == "rs")
      && path.file_name().is_some_and(|n| n != "tests.rs")
    {
      out.push(path);
    }
  }
  out.sort();
  out
}

/// The shared carriers a line binds, through `crate::<module>::…::Name`.
///
/// At least one lowercase module segment must precede the type, and the first of them may not be
/// `graphqlx` — `crate::graphqlx::…` is GraphQLx naming itself rather than binding anything.
fn bound_carriers(line: &str) -> Vec<String> {
  let mut out = Vec::new();
  let mut search = 0;
  // `crate::parser::` and not `crate::`: the parser moved one module below the crate root when
  // the crates merged, so every in-tree path gained that hop. Matching the bare `crate::` here
  // would read `parser` as the first module segment and defeat both rules below — the
  // "at least one lowercase module" test and the `graphqlx`-names-itself exclusion.
  while let Some(hit) = line[search..].find("crate::parser::") {
    let mut cursor = search + hit + "crate::parser::".len();
    search = cursor;
    let mut modules = 0usize;
    while let Some(segment) = leading_ident(&line[cursor..]) {
      cursor += segment.len();
      if segment.starts_with(|c: char| c.is_ascii_uppercase()) {
        if modules > 0 {
          out.push(segment);
        }
        break;
      }
      if modules == 0 && segment == "graphqlx" {
        break;
      }
      modules += 1;
      if line[cursor..].starts_with("::") {
        cursor += 2;
      } else {
        break;
      }
    }
  }
  out
}

/// The `pub` production a `*_parser!` invocation beginning at `index` declares, if any.
///
/// The name is on the invocation line for the one-line form and on the first line after the doc
/// comment otherwise. Only column-zero invocations count: an indented one is inside a
/// `macro_rules!` body.
fn parser_macro_production(lines: &[&str], index: usize) -> Option<String> {
  let line = lines[index];
  if line.starts_with(char::is_whitespace) {
    return None;
  }
  let open = line.find("_parser!(")?;
  if !line[..open]
    .chars()
    .all(|c| c.is_ascii_alphanumeric() || c == '_')
  {
    return None;
  }

  let rest = &line[open + "_parser!(".len()..];
  let candidate = if rest.trim().is_empty() {
    let mut found = None;
    for following in &lines[index + 1..] {
      let trimmed = following.trim();
      if trimmed.is_empty() || trimmed.starts_with("//") {
        continue;
      }
      found = Some(*following);
      break;
    }
    found?
  } else {
    rest
  };

  let production = candidate.trim().strip_prefix("pub ")?;
  let ident = leading_ident(production)?;
  production[ident.len()..]
    .trim_start()
    .starts_with(',')
    .then_some(ident)
}

/// The leading `[A-Za-z0-9_]` run of `s`, if it starts with one.
fn leading_ident(s: &str) -> Option<String> {
  let end = s
    .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
    .unwrap_or(s.len());
  (end > 0).then(|| s[..end].to_string())
}

/// `snake_case` to `PascalCase`.
fn pascal(snake: &str) -> String {
  snake
    .split('_')
    .map(|word| {
      let mut chars = word.chars();
      match chars.next() {
        Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
        None => String::new(),
      }
    })
    .collect()
}

/// Insert `name` with the three naming conventions stripped.
fn insert(names: &mut BTreeSet<String>, name: Option<String>) {
  let Some(mut name) = name else { return };
  loop {
    let next = ["Try", "Const", "Described"]
      .iter()
      .find_map(|prefix| {
        name
          .strip_prefix(prefix)
          .filter(|rest| rest.starts_with(|c: char| c.is_ascii_uppercase()))
      })
      .map(str::to_string);
    match next {
      Some(stripped) => name = stripped,
      None => break,
    }
  }
  if !name.is_empty() {
    names.insert(name);
  }
}
