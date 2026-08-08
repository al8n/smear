#![cfg(all(feature = "rowan", feature = "graphqlx"))]

//! Gate 5: GraphQLx golden trees — the **shape** gate, and the only one that can see a mis-shaped
//! tree.
//!
//! Gates 1 through 4 are each green, each correct to be green, and **each provably blind to
//! structural loss**. Every one of the four pins its own blindness, in this suite, as a test:
//!
//! - gate 1 compares one bit, the verdict, so a tree whose nodes moved is invisible to it;
//! - gate 2 compares the padded tree's node pre-order against its own compact tree's, which never
//!   sees a token's placement at all — and it compares a tree only with *itself*, so a defect that
//!   mis-shapes compact and padded identically passes;
//! - gate 3 compares `text()`, which is the tree with its shape projected away. Phase A ran eight
//!   mutations at its GraphQL twin and **not one made `text()` differ from `src`**;
//! - gate 4 requires "cast once, getter called once", never that a getter returns the *right*
//!   child.
//!
//! The same bytes come back through genuinely different trees: `query Q { f }` is an
//! `OperationDefinition` through the mixed root and a run of `Error` nodes through the SDL-only
//! root, and the two are byte-identical. Everything in that gap is this file's to catch.
//!
//! # Why GraphQLx needs this more than GraphQL does
//!
//! Three of GraphQLx's productions decide a **node kind** rather than a verdict, so their failure
//! mode is a tree that is wrong and valid:
//!
//! - `set`/`map` before a `{` is a [`SetValue`](K::SetValue)/[`MapValue`](K::MapValue) and the same
//!   spelling anywhere else is an [`EnumValue`](K::EnumValue) — `{ a: set }` and `{ a: set { 1 } }`
//!   differ *only* in the tree;
//! - `<T>` is a [`SetType`](K::SetType) and `<K => V>` a [`MapType`](K::MapType);
//! - a directive's name is a [`TypePath`](K::TypePath) with its own generics, so `@ns::d<T>(a: 1)`
//!   has four levels of structure where GraphQL has one token.
//!
//! And GraphQLx has two node kinds whose whole content is *where they start*:
//! [`DefinitionName`](K::DefinitionName) and [`ExtensionName`](K::ExtensionName) wrap a path plus an
//! optional generic list, so a boundary on the wrong side of a space is a range change and nothing
//! else. Only this format records it.
//!
//! # The format, and why it is this one
//!
//! rust-analyzer's `.rast` style: one line per node and per token, two spaces of indent per level of
//! depth, each line carrying the kind and the absolute byte range, and each **token** line
//! additionally carrying its text, escaped. Nothing else — no source echo, no summary.
//!
//! Since text is identical across this gate's entire target class, the render has to foreground what
//! text throws away, and each of the four columns is one axis of structural change:
//!
//! - **indent** — a re-parented subtree shifts a whole block, so re-parenting is the loudest diff
//!   the format can produce rather than the quietest;
//! - **kind** — a production that opens the wrong node changes one word per site, which is what a
//!   `SetType` silently becoming a `MapType` looks like;
//! - **range** — a node boundary that moved changes numbers even when no line appears or vanishes,
//!   which is the "`Arguments` opened after the `(` instead of before" shape;
//! - **token text** — tokens are in the tree, so a token attached to the wrong parent is a moved
//!   line rather than an invisible one. Trivia is included for exactly this reason: which node a
//!   comment commits into is a real decision this suite makes, and no other gate records it.
//!
//! Token text is escaped by [`escape`] rather than by `char::escape_debug`, on purpose. A token
//! carrying a newline would otherwise break the one-line-per-element property outright and
//! mis-align every diff after it; and std's notion of "printable" is a Unicode table that can move
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
//! - **An update run still fails.** With the variable set, a changed golden is written *and* the
//!   test panics, naming every file it touched. So blessing takes two deliberate commands, and — the
//!   part that matters for CI — a build that somehow had `UPDATE_GOLDEN=1` in its environment still
//!   reds on a real difference instead of laundering it into a pass.
//! - **The failure prints the diff**, as unified hunks against the committed file, so the reader
//!   deciding whether to bless does not have to run anything to see what changed.
//!
//! # What reading the goldens found
//!
//! Every tree in both sets was read against its source before being committed, because a golden
//! blessed unread records whatever the parser did, bug included, and turns it into an expectation.
//! The sweep's two findings are on the page as constants rather than as prose:
//! [`OPENS_ON_LEADING_TRIVIA`] and [`GAP_TILES_AT_ROOT`], each with a standing test that re-derives
//! it every run. Phase A's `NamedType` defect — a node opened before the trivia in front of it — is
//! **not** present here: GraphQLx inherited the fixed `named_type` discipline, and
//! [`only_the_recorded_nodes_open_on_their_leading_trivia`] is what says so on every run rather than
//! once.
//!
//! Two shapes were read and judged deliberate, both inherited from Phase A's reading: a member
//! whose type is missing recovers by consuming the enclosing `}` into an `Error`, so the
//! surrounding block closes without a right brace; and trailing trivia attaches to whichever node
//! was still open at the peek that crossed it, which is the preceding sibling inside a list and the
//! enclosing list after its last element. Both are attachment policy rather than loss.
//!
//! **The second of those was left as prose, and prose is what a re-bless walks through.** Reading a
//! policy once and judging it deliberate is not the same as pinning it: the goldens were the only
//! thing holding the placement, and an `UPDATE_GOLDEN=1` run rewrites goldens. smear#131 measured
//! what that costs — an `expect` that skips trivia on its accepting branch as well as before it
//! moves trailing placement across the whole corpus while round-trip, the gap census, the parity
//! gate, the injection gate and the projection differential all stay green. So the sentence is now
//! [`CLOSES_ON_TRAILING_TRIVIA`], forty-eight pairings re-derived on every run by
//! [`only_the_recorded_nodes_close_on_their_trailing_trivia`]. It is nearly twice its GraphQL
//! twin's length for a reason the constant states: GraphQLx spells a name as a path, and a path
//! ends by probing for a `::` that is usually not there.

use std::{collections::BTreeSet, fmt::Write as _, path::PathBuf};

use rowan::{NodeOrToken, WalkEvent};
use smear::parser::graphqlx::{
  kinds::SyntaxKind as K,
  lossless::{SyntaxNode, parse_document, parse_type_system_document},
};

/// The environment variable that turns a comparison run into a blessing run.
const UPDATE_VAR: &str = "UPDATE_GOLDEN";

/// The token images the lossless lexer surfaces as trivia, as they enter the tree.
///
/// The kind space's trivia block verbatim — `kinds.rs` puts the six at `26..32`, immediately before
/// [`K::IMAGE_BLOCK`]. `Gap` is deliberately not among them: it is the sink's fill for a region no
/// token covers, not something the lexer produced.
const TRIVIA_IMAGES: &[K] = &[K::Bom, K::Comma, K::Space, K::Tab, K::Newline, K::Comment];

/// The corpus entries whose `Gap` hangs off `Root` rather than off a node inside the tree.
///
/// **Measured, not copied.** The GraphQLx lexer accepts seven images GraphQL rejects — `*`, `+`,
/// `-`, `<`, `>`, `::`, `=>` — so "the lexer refuses this byte for byte" is a different set in the
/// two dialects, and an entry inherited from `tests/corpus/` is not automatically tokenless here.
///
/// The rule these satisfy is stated at
/// [`only_a_source_with_no_committed_token_tiles_its_gap_at_the_root`], and this list is its
/// consequence rather than a carve-out: a run is tiled where it opens, in the node open at the token
/// it trails, so a run reaches `Root` only when the parse committed no token at all. Each of these
/// is refused by the lexer byte for byte, so there is no such moment and the fallback tiles the run
/// where the walk ends — `Root@0..n` over an empty `Document@0..0` and one `Gap@0..n`, three lines
/// each.
///
/// The consequence worth stating for a consumer: `Document.text() == source` is unconditional for
/// every source with a committed token in it, and false only for these, where `Document` is empty
/// because the grammar never got a token to put in it.
const GAP_TILES_AT_ROOT: &[&str] = &[
  "invalid_lex_illegal_character.graphqlx",
  "invalid_lex_unterminated_block_string.graphqlx",
  "invalid_lex_unterminated_string.graphqlx",
];

/// Every `parent > node` pairing where a node opens *before* the trivia in front of it, so that the
/// trivia lands inside the node's range instead of beside it.
///
/// **One entry, and it is the correct one.** `Root > Document` is not a defect: a file's leading
/// comment has to live somewhere, and `Document` spans the whole source.
///
/// That there is exactly one is the headline finding of reading these trees. Phase A's sweep of the
/// GraphQL goldens turned up a second — `RootOperationTypeDefinition > NamedType`, where
/// `named_type` opened its node and only then called `expect`, so the trivia skip ran inside a node
/// that was already open and `schema { query: Q }` gave a `NamedType` spanning `" Q"`. GraphQLx's
/// productions were written against the fixed discipline, so the defect never arrived here; that is
/// a claim this file re-measures every run rather than a sentence.
///
/// The three GraphQLx name wrappers are where a repeat would be most expensive.
/// [`DefinitionName`](K::DefinitionName), [`ExtensionName`](K::ExtensionName) and
/// [`ExecutableDefinitionName`](K::ExecutableDefinitionName) exist to carry a path plus an optional
/// generic list and nothing else, so their entire content *is* a range — a boundary on the wrong
/// side of a space is the whole defect, with no kind change and no line moved to show for it.
const OPENS_ON_LEADING_TRIVIA: &[&str] = &["Root > Document"];

/// Every `parent > node` pairing where a node closes *after* the trivia behind it, so that the
/// trivia lands inside the node's range instead of beside it.
///
/// **Forty-eight entries where [`OPENS_ON_LEADING_TRIVIA`] has one.** The mechanism is the shared
/// one and its full account is on the GraphQL twin's constant: an atom commits the trivia it
/// crosses to whichever node is open at that moment, a production about to open a node crosses its
/// leading trivia at a dispatch peek made *before* the mark, and trailing trivia has no such
/// moment — the peek that crosses it is the peek that discovers the node is over, made from inside
/// the node it is ending. What is worth reading here is why GraphQLx's list is nearly twice its
/// twin's.
///
/// # The families GraphQL also has
///
/// - **The preceding sibling inside a list**, when a production ends in an optional component it
///   has to probe for: `SelectionSet > Field`, `SelectionSet > FragmentSpread`,
///   `FieldsDefinition > FieldDefinition`, `EnumValuesDefinition > EnumValueDefinition`,
///   `ArgumentsDefinition > InputValueDefinition`, `InputFieldsDefinition > InputValueDefinition`,
///   `VariablesDefinition > VariableDefinition`, and at document level
///   `Document > ObjectTypeExtension`, the one shape here whose own last probe, rather than a list
///   inside it, is what crossed the trivia.
///   `DefinitionTypeGenerics > DefinitionTypeParam`, `WhereClause > WherePredicate` and
///   `ImportList > NamedSpecifier` are GraphQLx lists of the same shape.
/// - **A list after its last element**, the same probe one level up: every `> Directives` pairing,
///   plus `DirectiveDefinition > DirectiveLocations`.
/// - **Recovery geometry**, where the node open at the peek is open because a recovery put it
///   there. `Document > Error` is a resynchronised run; `Argument > ListValue`
///   (`invalid_unterminated_bracket`) is a list that never gets its `]`, so the enclosing `}` is
///   consumed into an `Error` inside it and the trailing newline follows it in; `TypeGenerics >
///   Error` (`invalid_x_name_adjacent_to_angle_in_generics`) is that shape inside an angle list;
///   and `Document > DefinitionName` is Task 10's unwound mark — the definition node is never
///   opened, so the name the production had already built is left as a bare child of `Document`
///   with the trivia after it. That last one is recorded **with the loss in it**, exactly as
///   `shape_lost_object_type_definition` is, and it moves the day the loss is fixed.
///
/// # GraphQLx's own, and it is most of the difference
///
/// GraphQLx spells a name as a **path** — `Path` is segments joined by `::`, and
/// [`DefinitionName`](K::DefinitionName), [`ExtensionName`](K::ExtensionName) and
/// [`ExecutableDefinitionName`](K::ExecutableDefinitionName) wrap a path plus an optional generic
/// list. Every one of those ends by probing for a continuation that usually is not there: `Path`
/// peeks for another `::`, the wrappers peek for a `<`. The peek crosses the trivia, declines, and
/// the trivia stays inside. So in `type T { f: Int }` the `DefinitionName` spans `"T "` and the
/// `Path` under `f`'s type spans `"Int "` — which is `TypePath > Path` (77 witnesses),
/// `DefinitionTypePath > Path` (68) and `ObjectTypeDefinition > DefinitionName` (43), the three
/// commonest pairings in this list after `Root > Document`. `EnumValue > Path`,
/// `NamedSpecifier > Path`, `WildcardSpecifier > Path`, `ExtensionName > Path`,
/// `FragmentDefinition > ExecutableDefinitionName` and the eight remaining `> DefinitionName`
/// pairings are the same production in other positions.
///
/// **This is the one family worth arguing about, and here is the argument.** It is the trailing
/// mirror of the leading shape Phase A treated as a defect — `RootOperationTypeDefinition >
/// NamedType`, where a `NamedType` spanned `" Q"` and "a consumer that highlighted or renamed
/// through that range took the space with it". Three things separate them. The leading one was an
/// **inconsistency**: six of `named_type`'s seven call sites crossed the trivia first and one did
/// not, where this is uniform, every path in the dialect doing it because every path has the same
/// optional continuation. GraphQL has no counterpart at all, its `NamedType` wrapping a bare `Name`
/// token with nothing optional after it — so the difference is one of grammar rather than of
/// discipline. And the range a consumer reads is not this one: `extent_of` computes an AST node's
/// span from its first non-trivia token to its last, so the projected span of that `DefinitionName`
/// is `"T"` either way. What is recorded here is the CST range, and recording it is what makes a
/// *change* to it visible.
///
/// # Why this is an array and not a sentence
///
/// It was a sentence — this file's header read the policy, judged it deliberate, and left it
/// unpinned, with the `.rast` goldens as the only thing holding the placement and a bless able to
/// launder them. smear#131 measured the cost: an `expect` that skips trivia on its accepting branch
/// as well as before it moves this set in both directions at once, and round-trip, the gap census,
/// the parity gate, the injection gate and the projection differential all stay green through it.
const CLOSES_ON_TRAILING_TRIVIA: &[&str] = &[
  "Argument > ListValue",
  "ArgumentsDefinition > InputValueDefinition",
  "DefinitionTypeGenerics > DefinitionTypeParam",
  "DefinitionTypePath > Path",
  "DefinitionTypePath > TypeGenerics",
  "DirectiveDefinition > DefinitionName",
  "DirectiveDefinition > DirectiveLocations",
  "Document > DefinitionName",
  "Document > Error",
  "Document > ObjectTypeExtension",
  "EnumTypeDefinition > DefinitionName",
  "EnumValue > Path",
  "EnumValueDefinition > Directives",
  "EnumValuesDefinition > EnumValueDefinition",
  "ExtensionName > Path",
  "Field > Directives",
  "FieldDefinition > DefinitionTypePath",
  "FieldDefinition > Directives",
  "FieldDefinition > ListType",
  "FieldDefinition > SetType",
  "FieldsDefinition > FieldDefinition",
  "FragmentDefinition > Directives",
  "FragmentDefinition > ExecutableDefinitionName",
  "FragmentSpread > Directives",
  "ImportDefinition > WildcardSpecifier",
  "ImportList > NamedSpecifier",
  "InlineFragment > Directives",
  "InputFieldsDefinition > InputValueDefinition",
  "InputObjectTypeDefinition > DefinitionName",
  "InterfaceTypeDefinition > DefinitionName",
  "NamedSpecifier > Path",
  "ObjectTypeDefinition > DefinitionName",
  "ObjectTypeDefinition > Directives",
  "ObjectTypeExtension > Directives",
  "OperationDefinition > DefinitionName",
  "Root > Document",
  "ScalarTypeDefinition > DefinitionName",
  "ScalarTypeDefinition > Directives",
  "SchemaDefinition > Directives",
  "SelectionSet > Field",
  "SelectionSet > FragmentSpread",
  "TypeGenerics > Error",
  "TypeGenerics > ListType",
  "TypePath > Path",
  "UnionTypeDefinition > DefinitionName",
  "VariablesDefinition > VariableDefinition",
  "WhereClause > WherePredicate",
  "WildcardSpecifier > Path",
];

/// Fixtures the corpus does not carry, each one a tree whose *shape* is its whole content.
///
/// The corpus is a grammar-coverage set: it is organised around which productions run, and it is
/// shared with gates 1, 2 and 3. These are organised around which structural claims can silently
/// stop being true. Each is a source whose bytes say almost nothing and whose tree says the thing
/// under test.
const SHAPE_CASES: &[(&str, &str)] = &[
  // ---- Carried over from Phase A, where the claim still applies ----
  //
  // Task 10's finding, on the page. The body fails by unwinding, so `node_at`'s mark is never spent,
  // the `ObjectTypeDefinition` is never opened, and its tokens end up bare children of `Document`.
  // Gate 1 passes (both suites reject) and gate 3 passes (every byte is there). The day this loss is
  // fixed, this golden reds and someone reads the diff — which is the entire reason it is committed
  // with the bug in it.
  (
    "lost_object_type_definition",
    "type T { \"\"\"b\"\"\" \"a\" }",
  ),
  // The control for the line above: this body fails too, but it recovers *in place*, so nothing
  // unwinds past the mark and the definition node survives. Without it, "no `ObjectTypeDefinition`
  // in the tree" would be satisfied by any input that simply is not an object type.
  ("kept_object_type_definition", "type T { x: }"),
  // Every GraphQL-shaped type reference in one source, plus the argument, directive and
  // default-value nesting they sit inside.
  (
    "type_reference_nesting",
    "query Q($v: [Int!]! = 3) { f(a: $v) @d(x: 1) }",
  ),
  // Type references nested in themselves, where an off-by-one-level parent is otherwise
  // undetectable: `[[A!]!]!` and `[A!]!` carry the same kinds in the same order.
  ("nested_list_types", "type T { f: [[A!]!]! }"),
  // Where trivia commits. Comments, commas and newlines are tokens in this suite, so which node each
  // one lands in is a decision the atoms make on every peek — and gate 2, which is the trivia gate,
  // compares only the node pre-order and cannot see a single one of them.
  ("trivia_attachment", "{\n  # c\n  a ,\n  b\n}\n"),
  // Recovery geometry: how wide the `Error` node is, where it starts, and whether the definitions on
  // either side of it survive intact.
  (
    "top_level_recovery",
    "type A { f: Int } ??? type B { g: Int }",
  ),
  // The line above, truncated at the run — and the pair is the point. Two streams that share a
  // prefix through the token a run trails must place that run in the **same** node, including when
  // one of them simply stops there. Gates 1 and 3 see a verdict and a string, so before tokora
  // `60f27a3` these two disagreed and every gate stayed green.
  ("top_level_recovery_at_eof", "type A { f: Int } ???"),
  // Trivia in front of a *retro-wrap* probe, in the two positions GraphQL also has: the `!` that
  // folds into the type it follows, and the `:` that turns a name into an `Alias`. Carried over
  // because a mutation forced it into existence rather than because the shape looked interesting —
  // gate 2 measured that `tests/corpusx/` writes neither `Int !` nor `alias : f`.
  (
    "retro_wrap_across_trivia",
    "{ alias : f }\ntype T { g: Int ! }\n",
  ),
  // ---- GraphQLx's own ----
  //
  // Divergences 3, 4 and 5 in one tree: a map type, inside a list type, under a non-null fold. Three
  // node kinds that only exist here, nested in the one order that makes an off-by-one-level parent
  // invisible to every other gate.
  ("map_type_inside_a_list", "type T { f: [<K => V>]! }"),
  // Divergence 13: a fragment definition carries **two** generic lists, and — read off the golden
  // rather than assumed — they are the *same node kind* at two different depths. The one before the
  // name is a sibling of `ExecutableDefinitionName`; the one after it is that node's child. So the
  // pair is invisible to any comparison over kinds alone, gate 2's included, and shows up here as an
  // indent. The type condition then carries a third list under `TypeGenerics`, which is the applying
  // kind a type *reference* uses: three generic lists, two kinds, one source.
  (
    "fragment_generics",
    "fragment <T, U> F<A, B> on N::M<W> { f }",
  ),
  // Divergence 10: a directive's name is a type path with its own generics, so `@ns::d<A>` is four
  // levels of structure where GraphQL has one token. The second directive is the leading-`::`
  // spelling, whose `Path` starts one token earlier.
  (
    "directive_name_is_a_type_path",
    "type T @ns::d<A>(a: 1) @::x::y { f: Int }",
  ),
  // Divergences 14, 15 and 16 together: a generic parameter with a **type** default (not a
  // `DefaultValue` — the `=` here introduces a type reference), and a `where` clause on the
  // definition that names it.
  (
    "generic_default_and_where",
    "type T<A = Int> where A: B { f: A }",
  ),
  // Divergence 21: an import list mixing an alias, a qualified alias and a wildcard, which is three
  // specifier shapes under one `ImportList`.
  (
    "import_alias_and_wildcard",
    "import { a as b, c as ns::d, * as w } from \"m\"\n",
  ),
  // Divergence 7, the contextual `set`, and this gate's sharpest instance: both documents are valid,
  // both round-trip, and the two readings differ **only** in the tree. See
  // [`the_render_separates_the_two_readings_of_a_contextual_keyword`], which is this fixture read
  // rather than merely stored.
  (
    "contextual_set_both_readings",
    "query A { f(a: set) }\nquery B { f(a: set { 1 }) }\n",
  ),
  // The GraphQLx half of the retro-wrap claim, and the one with a kind at stake rather than a
  // wrapper. `set` commits as a keyword and the production retro-wraps only if a `{` follows, which
  // may be any amount of trivia away; lose that skip and this is an `EnumValue` followed by a syntax
  // error. A comment is used rather than a space so the crossed trivia is impossible to mistake for
  // formatting.
  (
    "set_retro_wrap_across_trivia",
    "{ f(a: set\n  # c\n  { 1 }, b: map\n  , { 1 => 2 }) }\n",
  ),
  // The `Path` production on its own: a leading `::`, three segments and generics at the end, inside
  // a list and under a non-null fold. `Path` is GraphQLx's most-opened node kind (3474 hits in gate
  // 2's sweep) and the one whose boundaries nothing else pins.
  (
    "qualified_path_nesting",
    "type T { f: [::ns::Inner<Int>!]! }",
  ),
];

/// The directory the `.rast` files live in.
fn golden_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("goldenx")
}

/// Every `.graphqlx` file in the GraphQLx corpus, in a deterministic order.
fn corpus() -> Vec<(String, String)> {
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
    .strip_suffix(".graphqlx")
    .unwrap_or(name)
    .replace('.', "_")
}

/// The golden stem a [`SHAPE_CASES`] entry is filed under.
///
/// The `shape_` prefix keeps the two sets from ever colliding — every corpus entry is named
/// `valid_…` or `invalid_…`, which [`the_golden_directory_holds_exactly_the_expected_files`]
/// re-checks rather than assumes.
fn shape_stem(name: &str) -> String {
  format!("shape_{name}")
}

/// Escape a token's text so one token is always one line, forever.
///
/// Backslash, quote and the three whitespace forms that would break the layout are spelled out;
/// every other C0/C1 control, plus the byte-order mark, becomes `\u{…}`. Anything else — including
/// ordinary multi-byte text like `café` or an emoji — is written through verbatim, so a golden stays
/// readable.
///
/// `char::escape_debug` was the obvious alternative and is the wrong tool: its notion of "printable"
/// is a Unicode table that moves between toolchain releases, and a byte-compared golden that changes
/// when the compiler does is a golden that teaches people to re-bless on sight. `char::is_control`
/// is a fixed two-range test and cannot drift.
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
  render(&parse_document(src).syntax())
}

/// A unified diff of two rendered trees, as hunks with two lines of context.
///
/// Written out rather than pulled in because the whole value of this gate is that its failure is
/// readable by someone who did not write the change, and a positional line-by-line comparison is
/// not: a lost node deletes one line *and* dedents everything under it, so a naive diff reports
/// every remaining line as changed and buries the one that matters.
fn unified_diff(expected: &str, actual: &str) -> String {
  const CONTEXT: usize = 2;
  /// Past this many lines the quadratic table stops being free; no golden in this suite is anywhere
  /// near it, and the fallback below is still readable.
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
/// Every case is checked before anything is reported, so one run tells a reader the full extent of a
/// change rather than the first file that happened to sort early.
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
       Nothing is blessed yet. Read `git diff -- smear/tests/goldenx`, satisfy yourself that \
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
        "A changed tree carries the same bytes as before — gates 1, 2, 3 and 4 cannot see this — so \
         read each diff as a claim about structure: an indent shift is a re-parented subtree, a \
         changed kind is a production opening the wrong node, and a changed range is a node \
         boundary that moved."
      );
    }
    let _ = writeln!(
      report,
      "If every change is intended, bless with `{UPDATE_VAR}=1 cargo test -p smear \
       --features rowan,graphqlx --test lossless_x_golden`, which rewrites the files and then fails \
       once more so the diff gets read."
    );
  }
  for stem in &missing {
    let _ = writeln!(report, "\n  missing: tests/goldenx/{stem}.rast");
  }
  // Enough detail to decide on, capped so the message stays a message.
  const DETAILED: usize = 4;
  for (stem, diff) in differing.iter().take(DETAILED) {
    let _ = writeln!(report, "\n--- tests/goldenx/{stem}.rast\n{diff}");
  }
  if differing.len() > DETAILED {
    let _ = writeln!(report, "\nalso changed, diffs not shown:");
    for (stem, _) in differing.iter().skip(DETAILED) {
      let _ = writeln!(report, "  tests/goldenx/{stem}.rast");
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
/// See [`SHAPE_CASES`] for what each one is for. They are checked separately from the corpus so that
/// a corpus edit and a structural-claim edit are never the same diff.
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
/// more, and the gate above stays green while quietly shrinking. It is also what makes the `shape_`
/// prefix a guarantee rather than a convention.
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
    "tests/goldenx holds {stale:?}, which no case produces any more — delete them, or the gate is \
     recording a tree nothing builds"
  );
  assert!(absent.is_empty(), "no golden is committed for {absent:?}");
}

/// The render answers differently for two trees that `text()` cannot tell apart.
///
/// This is the whole premise, as live values rather than as prose. `query Q { f }` is an
/// `OperationDefinition` through the mixed root and a run of `Error` nodes through the SDL-only
/// root; gate 3 measured that both round-trip byte for byte, so the *only* thing that can separate
/// them is a shape projection. If this ever passes because both renders are empty, or fails because
/// the two agree, the format has stopped doing its one job.
#[test]
fn the_render_answers_differently_for_trees_that_text_cannot_separate() {
  const SRC: &str = "query Q { f }";

  let mixed = parse_document(SRC);
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

/// The contextual `set`, whose two readings differ **only** in the tree.
///
/// GraphQLx's sharpest instance of this gate's premise, and the reason divergence 7 needed a golden
/// rather than a corpus entry. `set` is an ordinary identifier to the lexer; whether it names an
/// enum value or opens a set literal is decided on the next significant token, and the two answers
/// are both valid parses of valid documents. No verdict separates them, no `text()` comparison
/// separates them, and gate 2 compares each tree only with itself.
///
/// Asserted as a *reading* of the two committed goldens rather than left to the files, so the pair
/// cannot rot into two `.rast` blobs nobody remembers the point of.
#[test]
fn the_render_separates_the_two_readings_of_a_contextual_keyword() {
  const ENUM_VALUE: &str = "{ f(a: set) }";
  const SET_LITERAL: &str = "{ f(a: set { 1 }) }";

  assert!(
    !parse_document(ENUM_VALUE).has_errors() && !parse_document(SET_LITERAL).has_errors(),
    "both readings must be clean parses, or gate 1 separates them and this gate is not the only \
     witness"
  );

  let as_enum = render_str(ENUM_VALUE);
  let as_set = render_str(SET_LITERAL);
  assert!(
    as_enum.contains("EnumValue@") && !as_enum.contains("SetValue@"),
    "`set` in value position with no `{{` after it is an enum value:\n{as_enum}"
  );
  assert!(
    as_set.contains("SetValue@") && !as_set.contains("EnumValue@"),
    "`set {{ 1 }}` is a set literal, and the keyword must not survive as an enum value \
     beside it:\n{as_set}"
  );
}

/// `<T>` and `<K => V>` are two node kinds, and the render is what says which one was built.
///
/// The second of GraphQLx's kind-deciding productions. Both spellings are valid types in the same
/// positions, so a discriminator that answered the wrong one would build a well-formed tree of the
/// wrong shape — a defect with no verdict, no byte difference, and no padded-versus-compact
/// disagreement.
#[test]
fn the_render_separates_a_set_type_from_a_map_type() {
  let set = render_str("type T { f: <Int> }");
  let map = render_str("type T { f: <Int => String> }");

  assert!(
    set.contains("SetType@") && !set.contains("MapType@"),
    "`<Int>` is a set type:\n{set}"
  );
  assert!(
    map.contains("MapType@") && !map.contains("SetType@"),
    "`<Int => String>` is a map type, and no set type hides inside it:\n{map}"
  );
}

/// A gap hangs off `Root` exactly when the parse committed no token — a law, over both golden sets.
///
/// A region no committed token covers is tiled by the sink as a `Gap` token, and it is tiled **where
/// it opens**: immediately after the token it trails, in whatever node was open at that moment
/// (tokora `60f27a3`). So a run reaches `Root` only when there is no such token anywhere, which is to
/// say only when the parse committed nothing at all.
///
/// [`GAP_TILES_AT_ROOT`] names the sources that satisfy that today, and the biconditional below is
/// what makes the list a consequence instead of an assertion: a new entry can only join it by being
/// tokenless, and one of the present ones can only leave it by gaining a token.
///
/// The test is over both golden sets, not over the named entries, because "only these" is a claim
/// about all of them.
#[test]
fn only_a_source_with_no_committed_token_tiles_its_gap_at_the_root() {
  /// Does `src` put a token directly under `Root`? Only the gap can be there — every grammar token
  /// is committed inside `Document` — so this is "the gap escaped the tree".
  fn gap_at_root(src: &str) -> bool {
    parse_document(src)
      .syntax()
      .children_with_tokens()
      .any(|element| element.as_token().is_some())
  }

  /// The kind of the node each `Gap` in `src` is a child of, in source order.
  fn gap_parents(src: &str) -> Vec<K> {
    parse_document(src)
      .syntax()
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
      .filter(|token| token.kind() == K::Gap)
      .filter_map(|token| Some(token.parent()?.kind()))
      .collect()
  }

  /// How many tokens the grammar committed, as opposed to the sink tiling them.
  fn committed_tokens(src: &str) -> usize {
    parse_document(src)
      .syntax()
      .descendants_with_tokens()
      .filter_map(|element| element.into_token())
      .filter(|token| token.kind() != K::Gap)
      .count()
  }

  let cases: Vec<(String, String)> = corpus()
    .into_iter()
    .chain(
      SHAPE_CASES
        .iter()
        .map(|(name, src)| ((*name).to_string(), (*src).to_string())),
    )
    .collect();

  let at_root: Vec<&str> = cases
    .iter()
    .filter(|(_, src)| gap_at_root(src))
    .map(|(name, _)| name.as_str())
    .collect();

  assert_eq!(
    at_root, GAP_TILES_AT_ROOT,
    "the set of cases whose gap hangs off `Root` instead of off a node inside the tree has \
     changed. A case that appeared committed no token where it used to commit one, or the sink \
     stopped tiling a run at the token it trails; a case that vanished gained a token, and its \
     golden moves that `Gap` line one or more levels deeper in the same diff."
  );

  // The law the list is a consequence of. Scoped to the cases that actually carry a gap, because a
  // source with neither a gap nor a token — the empty document — satisfies neither side and says
  // nothing either way.
  for (name, src) in &cases {
    let is_at_root = gap_at_root(src);
    if gap_parents(src).is_empty() && !is_at_root {
      continue;
    }
    let committed = committed_tokens(src);
    let where_it_is = if is_at_root { "under" } else { "not under" };
    assert_eq!(
      is_at_root,
      committed == 0,
      "{name}: a gap reaches `Root` only when no committed token precedes it anywhere. This case \
       has {committed} committed token(s) and its gap is {where_it_is} `Root`."
    );
  }

  // The positive controls, so this census cannot pass because nothing anywhere produces a gap.
  //
  // GraphQLx lexes seven images GraphQL does not, so its "illegal character" is a different byte:
  // `%` is still one, and it is the smallest source that reaches the fallback clause.
  assert_eq!(
    gap_parents("% {"),
    vec![K::Document],
    "a leading run tiles at the node its first committed token lands in"
  );
  assert!(
    !gap_at_root("{ %"),
    "a run with nothing lexable after it must not escape to `Root`; if this fails, the \
     trailing-gap fix has been reverted upstream"
  );
  assert!(
    gap_at_root("%"),
    "a source with no committed token has no moment at which its run opens, so the run tiles \
     where the walk ends, which is `Root`"
  );

  // The property the trailing-gap fix exists to establish, as the pair the shape set carries: two
  // streams sharing a prefix through the token a run trails place that run in the same node, and
  // one of them simply stops there.
  const MID: &str = "type A { f: Int } ??? type B { g: Int }";
  const EOF: &str = "type A { f: Int } ???";
  assert_eq!(
    gap_parents(MID),
    gap_parents(EOF),
    "the same run, once with a definition after it and once at end of input, landed in two \
     different nodes — which is the asymmetry tokora `60f27a3` removed"
  );

  // And the shape of each root-level entry, stated where the reader is. One direct token child of
  // `Root`, and it is the gap.
  for name in GAP_TILES_AT_ROOT {
    let src = std::fs::read_to_string(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("corpusx")
        .join(name),
    )
    .unwrap_or_else(|e| panic!("the corpus entry {name} is unreadable: {e}"));
    let gap_owner = parse_document(&src)
      .syntax()
      .children_with_tokens()
      .filter_map(|element| element.into_token())
      .map(|token| token.kind())
      .collect::<Vec<K>>();
    assert_eq!(
      gap_owner,
      vec![K::Gap],
      "{name}: a root-level entry's `Root` should carry exactly one direct token child, the gap \
       that is its whole source"
    );
  }
}

/// Which nodes open on the wrong side of their leading trivia — a standing check, not a comment.
///
/// Reading every golden by eye is what Step 2 asks for and it is not repeatable; this is that
/// reading turned into an assertion. It sweeps every tree in both golden sets for a node whose first
/// child is a trivia token and compares the `parent > node` pairings against
/// [`OPENS_ON_LEADING_TRIVIA`], which records the ones that exist today and why each is not a defect.
///
/// It fails in both useful directions. A new production that opens its node before skipping trivia
/// adds a pairing; fixing one removes a pairing, in which case the goldens it affected change in the
/// same diff and both should be blessed together — which is exactly how Phase A's `NamedType` defect
/// left its GraphQL twin.
#[test]
fn only_the_recorded_nodes_open_on_their_leading_trivia() {
  fn pairings(src: &str) -> Vec<String> {
    parse_document(src)
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

  // The positive control, and it is not optional: an empty expectation is also what a `pairings`
  // that had stopped detecting anything would produce, so one source must still answer with a
  // pairing. A document that opens on a comment is the recorded, correct one.
  assert!(
    pairings("# lead\nscalar S").contains(&"Root > Document".to_string()),
    "the sweep no longer detects a node opening on its leading trivia at all, so every empty \
     answer below is worthless"
  );
  // The regression control for the defect Phase A found and fixed in the shared discipline
  // GraphQLx inherited. The corpus could lose its schema entries without anyone noticing this went
  // with them.
  assert!(
    pairings("schema { query: Q }").is_empty(),
    "the `RootOperationTypeDefinition > NamedType` defect has arrived in GraphQLx: the type path \
     after `query:` opened on the space in front of it"
  );
  assert!(
    pairings("type T { f: Int }").is_empty(),
    "an ordinary definition must open none of its nodes on trivia, or the sweep above cannot tell \
     a defect from the normal case"
  );
  // GraphQLx's own: the two name wrappers whose entire content is where they start, and the three
  // kind-deciding productions. A space before any of these is the shape this census exists for.
  for src in [
    "type T <A> where A : Node { f: A }",
    "extend type T <A> { f: A }",
    "{ f(a: set { 1 }, b: map { 1 => 2 }) }",
    "type T { f: <Int => String> }",
    "@ns::d<T>(a: 1) on FIELD",
  ] {
    assert!(
      pairings(src).is_empty(),
      "{src:?} opens a node on its leading trivia: {:?}",
      pairings(src)
    );
  }
}

/// Which nodes close on the wrong side of their trailing trivia — the twin of the check above, and
/// the reason [`CLOSES_ON_TRAILING_TRIVIA`] is an array rather than a sentence in the header.
///
/// Same sweep over the same two golden sets, reading `.last()` where the leading census reads
/// `.next()`. It is the only gate in this suite that sees trailing placement without going through
/// a `.rast` file, which is what makes it the one that survives a bless: an `UPDATE_GOLDEN=1` run
/// rewrites every expectation a placement change touches and leaves this assertion exactly where it
/// was. The atoms it is measuring are `smear/src/parser/lossless/trivia.rs`'s — shared with
/// GraphQL — so this and its GraphQL twin are one gate over two grammars, and a change to the
/// substrate reds both.
///
/// It fails in both directions and a real change moves it in both at once. smear#131 measured that
/// with an `expect` that skipped trivia on its *accepting* branch as well as before it: pairings
/// appeared where a production began holding its node open across a peek, and list pairings
/// vanished, because trivia eaten by the `expect` that ends an element never reaches the loop peek
/// that used to leave it inside the preceding sibling. So the failure is reported as the two
/// differences rather than as two forty-eight-element vectors: which half moved is what tells a
/// reader whether a production gained a probe or lost one.
#[test]
fn only_the_recorded_nodes_close_on_their_trailing_trivia() {
  fn pairings(src: &str) -> Vec<String> {
    parse_document(src)
      .syntax()
      .descendants()
      .filter_map(|node| {
        let last = node.children_with_tokens().last()?.into_token()?;
        if !TRIVIA_IMAGES.contains(&last.kind()) {
          return None;
        }
        Some(format!("{:?} > {:?}", node.parent()?.kind(), node.kind()))
      })
      .collect()
  }

  // The recorded set is written in the order the sweep produces it, so a hand edit that duplicates
  // an entry or files one out of place is a failure here rather than a confusing diff below.
  assert!(
    CLOSES_ON_TRAILING_TRIVIA.windows(2).all(|w| w[0] < w[1]),
    "CLOSES_ON_TRAILING_TRIVIA must be sorted and free of duplicates"
  );

  let mut found: Vec<String> = corpus()
    .iter()
    .flat_map(|(_, src)| pairings(src))
    .chain(SHAPE_CASES.iter().flat_map(|(_, src)| pairings(src)))
    .collect();
  found.sort();
  found.dedup();

  let appeared: Vec<&str> = found
    .iter()
    .map(String::as_str)
    .filter(|pairing| !CLOSES_ON_TRAILING_TRIVIA.contains(pairing))
    .collect();
  let vanished: Vec<&str> = CLOSES_ON_TRAILING_TRIVIA
    .iter()
    .copied()
    .filter(|pairing| !found.iter().any(|f| f == pairing))
    .collect();
  assert!(
    appeared.is_empty() && vanished.is_empty(),
    "the set of nodes that swallow their own trailing trivia has changed.\n  appeared: \
     {appeared:?}\n  vanished: {vanished:?}\nA pairing that appeared is a production holding its \
     node open across a peek it used to make from outside, which puts the node's end on the far \
     side of a space; a pairing that vanished is trivia consumed before the peek that used to \
     cross it, which is what an `expect` that skips on its accepting branch does. Both move the \
     `.rast` goldens in the same diff, and blessing those does not answer this."
  );

  // The positive control, and it is not optional: an empty `found` is also what a `pairings` that
  // had stopped detecting anything would produce, so one source must still answer with a pairing.
  // A field that ends the line it is on is the commonest instance of the commonest shared family.
  assert_eq!(
    pairings("{ a\n  b }"),
    vec!["SelectionSet > Field", "SelectionSet > Field"],
    "the sweep no longer detects a node closing on its trailing trivia at all, so every empty \
     answer below is worthless"
  );
  // And the negative one: a source carrying trivia that answers with nothing, or the assertion
  // above is satisfied by a `pairings` that reports every node it sees. Every node here ends on a
  // real token, and the one space in it is not the last child of anything.
  assert!(
    pairings("query Q{f}").is_empty(),
    "a source whose every node ends on a real token must answer with no pairing at all, or this \
     census cannot tell an attachment from a node boundary"
  );
  // The GraphQLx family, on the smallest source that carries it, so the reason this list is twice
  // its twin's length is a live value rather than a paragraph. Both halves matter: the name wrapper
  // and the path inside the type reference.
  assert_eq!(
    pairings("type T { f: Int }"),
    vec![
      "ObjectTypeDefinition > DefinitionName",
      "DefinitionTypePath > Path"
    ],
    "a path and a definition name no longer hold the trivia their continuation probe crossed. If \
     that is deliberate it is a good change, and the array above shrinks by most of its entries in \
     the same diff"
  );
}
