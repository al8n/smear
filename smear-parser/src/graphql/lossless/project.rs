//! The GraphQL CST → AST projection.
//!
//! [`project`] turns a lossless parse plus the text it was parsed from into **the AST the
//! syntactic parser produces for that text**, without re-parsing. It is the door an editor
//! goes through: parse losslessly once, format and highlight off the tree, and hand the same
//! parse to a consumer that wants typed nodes — a validator (#85), a name resolver, a codegen
//! back end.
//!
//! ```
//! # #[cfg(all(feature = "graphql", feature = "rowan"))] {
//! use smear_parser::graphql::lossless::{parse_document, project};
//!
//! let source = "type Query { field: Int }";
//! let parse = parse_document(source);
//! assert!(!parse.has_errors());
//!
//! let ast = project(&parse, source).expect("a well-shaped tree projects");
//! assert_eq!(ast.definitions().len(), 1);
//! # }
//! ```
//!
//! # The second dialect's form, transcribed from this dialect's productions
//!
//! The contract, the error vocabulary, the span rule, the `(tree, source)` verification, the green
//! traversal and — since al8n/smear#217 and #218 — the transcription atoms are
//! [the substrate](crate::lossless::project)'s. The **form** is the one the GraphQLx projection
//! arrived at over al8n/smear#58's seven rounds, and #218 measured that this file had every defect
//! class that form closes. It is not a diff of that file: every walk below is GraphQL's own
//! production under `graphql/lossless/*.rs`, transcribed, and four differences in this kind space
//! change what a walk does rather than what it calls something:
//!
//! - **A definition's name is a bare `Name` token.** GraphQLx wraps it in a `DefinitionName` node;
//!   here it is the token after the keyword, which the slot walk this replaces reached *by index*
//!   into a three-slot collector. A transcription reads it at its position in the sequence, so a
//!   name that spells a keyword (`directive @on on FIELD`) is a name because of where it is.
//! - **A description is a node.** [`Description`](SyntaxKind::Description) sits inside the node it
//!   precedes — and, unlike GraphQLx, inside an extension or a query shorthand too, because the
//!   production opens those nodes at a mark taken before the description. The syntactic parser
//!   refuses both, so both walks refuse the node as the first element their sequence has no place
//!   for.
//! - **A `!` is a wrapper node.** [`NonNullType`](SyntaxKind::NonNullType) retro-wraps the type it
//!   modifies, so the type cycle has a wrapper level GraphQLx's does not, and `[T]!` builds one AST
//!   list out of two tree nodes.
//! - **A type condition is not a node.** Its `on` is a token of the fragment that carries it and its
//!   type that fragment's [`NamedType`](SyntaxKind::NamedType) child, so the condition's span is
//!   assembled from two elements of one sequence — which is what made finding 4 a panic.
//!
//! # Why `source` is a parameter, and why the pair is checked
//!
//! The AST is keyed by `S = &'src str` — the syntactic parser has the same property — and the
//! bytes it borrows are the **caller's**, not the tree's. A green token does carry its own text
//! and could lend it, but that text belongs to the parse: an AST borrowing it could not outlive
//! the [`Parse`] it came from, and `Parse` is deliberately lifetime-free precisely so an editor
//! can cache one per file and drop it on the next keystroke. The caller who wants "the AST
//! without re-parsing" is holding the source by construction, so it is an argument.
//!
//! It is **verified, not trusted**: each door compares the whole of the tree's text against
//! `source` before it walks anything, and a mismatch is [`ProjectErrorKind::SourceMismatch`]
//! rather than a silently wrong AST pointing into unrelated bytes. The comparison is one green
//! walk — see [`verify_source`] — so it covers punctuation and trivia too, which a per-token
//! check never reached, and every slice taken afterwards is a plain index.
//!
//! # Shape-faithful, not verdict-faithful
//!
//! The projection succeeds iff the tree's **shape** determines a well-formed AST. It does not
//! re-derive the acceptance verdict, which stays [`Parse::has_errors`].
//!
//! The two come apart in one measured direction. `type T { x: Int` — no closing brace — leaves a
//! shape-complete tree with no recovery hole in it, so it projects, while the syntactic parser
//! rejects it. Making the projection refuse it would mean re-implementing delimiter accounting,
//! which is the grammar, which is the drift this door exists to avoid. **Check
//! [`has_errors`](Parse::has_errors) first**; a projection of an errorful tree is best-effort.
//!
//! In the other direction the projection is stricter than shape alone: a tree carrying an
//! [`Error`](SyntaxKind::Error) hole or a [`Gap`](SyntaxKind::Gap) tile is refused outright,
//! before any walk, because a hole is a region with no AST image and skipping it would be data
//! loss wearing a success type. The typed `to_ast` doors make the same scan over their own subtree.
//!
//! # What a tree the parser did not build is promised
//!
//! The projection is a **function of the tree**, and its guarantees are about its *output*:
//!
//! 1. **No panic** on any `(tree, source)` pair that passes the byte verification the doors run.
//! 2. **Image membership** — every AST value it returns is one the syntactic parser produces for
//!    *some* source, read **modulo the composite-span convention**. A composite node's span is its
//!    token extent, so the lenient shapes below carry a span no source gives the parser: a
//!    `FieldsDefinition` whose `}` is gone runs from its first field's name to its last field's
//!    end. It is the value's *shape* that must be in the image; the span is the tree's, by the
//!    rule the whole file is built on.
//! 3. **Totality** — every non-trivia byte under a projected node is represented in the AST, or
//!    the projection refuses.
//!
//! It does **not** promise that the tree is the one the lossless parser would build over those
//! bytes. A caller can mint a green tree, and [`Verified`] exists so one can be projected; nothing
//! in the three guarantees says the tree is a parse.
//!
//! **The worked case.** Over the source `[-12]`, a caller-built list can hold two adjacent
//! [`IntValue`](SyntaxKind::IntValue) nodes, `-1` at `1..3` and `2` at `3..4`. Each slice is one
//! whole integer to [`int_literal`], the byte verification passes because the concatenation is the
//! source, and the projection answers `[-1, 2]` with exactly those two spans. The shipped lexer
//! would read one integer `-12`, so this is an AST no *parse* of `[-12]` yields — and it is inside
//! all three guarantees, because it is the AST of the sentence the **tree** spells.
//! `a_tree_that_splits_a_token_projects_the_tree_it_was_handed` pins it as a decision: restoring
//! lexical-boundary fidelity would mean re-lexing the source around every token, the re-parse
//! `a_projection_that_re_parsed_the_source_would_fail_this` exists to forbid, and a second
//! custodian of boundaries the lexer already owns.
//!
//! # What the parser reports and still builds
//!
//! GraphQL's lossless productions **report and continue**: a violation goes on the diagnostic
//! channel and the node is built anyway, so `Parse::has_errors()` sees it and the *shape* does not.
//! The population is every call that reports without building a hole — **38**
//! `recover::report_unexpected::<…>` sites and **11** `recover::unclosed_{list,object,parens}::<…>`
//! sites across `graphql/lossless/*.rs`, the counts `grep -c` gives for those spellings — and
//! **all 49 are mapped** below, one probe each: a text that reaches the site with a hole-free tree,
//! and what this projection answers for it. `every_report_and_build_site_has_a_measured_answer`
//! holds the same rows, asserts their count per file and family against the source's own spelling
//! of each call, and measures every answer.
//!
//! **The "what the site leaves out" column is where the lenient set comes from.** A site either
//! leaves out one token with **no AST image** (a closer, an `on`), leaves out a constituent the AST
//! holds (a member, a name, a tail — never lenient), or leaves nothing out and reports something
//! present (a description, a spelling a rule refuses). **Eleven project; thirty-eight refuse.**
//!
//! | file | site | probe | what the site leaves out | the projection |
//! |---|---|---|---|---|
//! | `document.rs` | report | `extend scalar S` | a constituent with an image | `MissingChild { ScalarTypeExtension }` |
//! | `document.rs` | report | `extend type T` (and `extend interface I`, the same site) | a constituent with an image | `MissingChild { ObjectTypeExtension }` |
//! | `document.rs` | report | `extend union U` | a constituent with an image | `MissingChild { UnionTypeExtension }` |
//! | `document.rs` | report | `extend enum E` | a constituent with an image | `MissingChild { EnumTypeExtension }` |
//! | `document.rs` | report | `extend input I` | a constituent with an image | `MissingChild { InputObjectTypeExtension }` |
//! | `document.rs` | report | `extend schema` | a constituent with an image | `MissingChild { SchemaExtension }` |
//! | `document.rs` | report | `extend` | a constituent with an image — the `extend` is rubble | `UnexpectedChild { Document, Name }` |
//! | `document.rs` | report | `"d" { f }` | nothing — a present node is refused | `UnexpectedChild { OperationDefinition, Description }` |
//! | `document.rs` | report | `"d" extend scalar S @k` | nothing — a present node is refused | `UnexpectedChild { ScalarTypeExtension, Description }` |
//! | `document.rs` | report | `"d" extend scalar S @k` (SDL root) | nothing — a present node is refused | `UnexpectedChild { ScalarTypeExtension, Description }` |
//! | `document.rs` | report | the empty document | a constituent with an image | `MissingChild { Document }` |
//! | `document.rs` | report | the empty document (SDL root) | a constituent with an image | `MissingChild { TypeSystemDocument }` |
//! | `executable.rs` | report | `query Q() { f }` (executable root) | a constituent with an image | `MissingChild { VariablesDefinition }` |
//! | `executable.rs` | unclosed | `query Q($a: Int` (executable root) | `)` of `VariablesDefinition` — **no image** | `UnexpectedChild { Root, OperationType }` — the executable root abandons its document node, so the list is rubble beside no container |
//! | `executable.rs` | report | `fragment on on T { f }` (executable root) | nothing — a present token is refused | `SemanticRule` |
//! | `executable.rs` | report | `"d" { f }` (executable root) | nothing — a present node is refused | `UnexpectedChild { OperationDefinition, Description }` |
//! | `executable.rs` | report | the empty document (executable root) | a constituent with an image | `MissingChild { ExecutableDocument }` |
//! | `selection.rs` | report | `fragment F T { f }` | `on` of `FragmentDefinition` — **no image** | **projects** |
//! | `selection.rs` | report | `fragment F on { f }` | a constituent with an image | `UnexpectedChild { FragmentDefinition, SelectionSet }` |
//! | `selection.rs` | report | `{ ... }` | a constituent with an image — the `...` is rubble | `UnexpectedChild { SelectionSet, Spread }` |
//! | `selection.rs` | report | `{ }` | a constituent with an image | `MissingChild { SelectionSet }` |
//! | `selection.rs` | unclosed | `{ f` | `}` of `SelectionSet` — **no image** | **projects** |
//! | `definition.rs` | report | `type T { f(): Int }` | a constituent with an image | `MissingChild { ArgumentsDefinition }` |
//! | `definition.rs` | unclosed | `type T { f(a: Int` | `)` of `ArgumentsDefinition` — **no image** | `UnexpectedChild { Document, Name }` |
//! | `definition.rs` | report | `type T { }` | a constituent with an image | `MissingChild { FieldsDefinition }` |
//! | `definition.rs` | unclosed | `type T { f: Int` | `}` of `FieldsDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `input I { }` | a constituent with an image | `MissingChild { InputFieldsDefinition }` |
//! | `definition.rs` | unclosed | `input I { f: Int` | `}` of `InputFieldsDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `type T implements { f: Int }` | a constituent with an image | `MissingChild { ImplementsInterfaces }` |
//! | `definition.rs` | report | `type T implements A & { f: Int }` | a constituent with an image | `MissingChild { ImplementsInterfaces }` |
//! | `definition.rs` | report | `union U =` | a constituent with an image | `MissingChild { UnionMemberTypes }` |
//! | `definition.rs` | report | `union U = A \|` | a constituent with an image | `MissingChild { UnionMemberTypes }` |
//! | `definition.rs` | report | `directive @d on FOO` | nothing — a present token is refused | `MalformedToken { Name }` |
//! | `definition.rs` | report | `directive @d on \|` | a constituent with an image | `MissingChild { DirectiveLocations }` |
//! | `definition.rs` | report | `directive @d on FIELD \|` | a constituent with an image | `MissingChild { DirectiveLocations }` |
//! | `definition.rs` | report | `enum E { true }` | nothing — a present token is refused | `SemanticRule` |
//! | `definition.rs` | report | `enum E { }` | a constituent with an image | `MissingChild { EnumValuesDefinition }` |
//! | `definition.rs` | unclosed | `enum E { A` | `}` of `EnumValuesDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `schema { foo: Q }` | nothing — a present token is refused | `MalformedToken { Name }` |
//! | `definition.rs` | report | `schema { }` | a constituent with an image | `MissingChild { RootOperationTypeDefinitions }` |
//! | `definition.rs` | unclosed | `schema { query: Q` | `}` of `RootOperationTypeDefinitions` — **no image** | **projects** |
//! | `definition.rs` | report | `directive @d FIELD` | `on` of `DirectiveDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `directive @d on` | a constituent with an image | `MissingChild { DirectiveDefinition }` |
//! | `definition.rs` | report | `schema @k` | a constituent with an image | `MissingChild { SchemaDefinition }` |
//! | `directive.rs` | unclosed | `{ f(a: 1` | `)` of `Arguments` — **no image** | **projects** |
//! | `ty.rs` | unclosed | `type T { f: [Int` | `]` of `ListType` — **no image** | **projects** |
//! | `value.rs` | unclosed | `{ f(a: [1` | `]` of `ListValue` — **no image** | **projects** |
//! | `value.rs` | unclosed | `{ f(a: {b: 1` | `}` of `ObjectValue` — **no image** | **projects** |
//! | `value.rs` | report | `type T { f(a: Int = $v): Int }` | nothing — a present node is refused | `UnexpectedChild { DefaultValue, Variable }` |
//!
//! ## The missing-token class: where an absent token still projects
//!
//! **The criterion is the parser-witnessed floor.** A position is lenient iff the lossless parser
//! itself builds a **hole-free** tree for the text with that token missing, *and* the token has no
//! AST image of its own. Both halves matter: the first is why such a shape reaches a projection at
//! all, the second is why nothing is lost by projecting it — only a token is gone, every
//! constituent the AST holds is still there, and the value's shape is one the parser builds for the
//! text with the token restored.
//!
//! | parent | absent token | witness |
//! |---|---|---|
//! | [`SelectionSet`](SyntaxKind::SelectionSet) | `}` | `selection.rs` unclosed — `{ f` |
//! | [`FieldsDefinition`](SyntaxKind::FieldsDefinition) | `}` | `definition.rs` unclosed — `type T { f: Int` |
//! | [`InputFieldsDefinition`](SyntaxKind::InputFieldsDefinition) | `}` | `definition.rs` unclosed — `input I { f: Int` |
//! | [`EnumValuesDefinition`](SyntaxKind::EnumValuesDefinition) | `}` | `definition.rs` unclosed — `enum E { A` |
//! | [`RootOperationTypeDefinitions`](SyntaxKind::RootOperationTypeDefinitions) | `}` | `definition.rs` unclosed — `schema { query: Q` |
//! | [`Arguments`](SyntaxKind::Arguments) | `)` | `directive.rs` unclosed — `{ f(a: 1` |
//! | [`FragmentDefinition`](SyntaxKind::FragmentDefinition) | `on` | `selection.rs` report — `fragment F T { f }` |
//! | [`DirectiveDefinition`](SyntaxKind::DirectiveDefinition) | `on` | `definition.rs` report — `directive @d FIELD` |
//! | [`ListType`](SyntaxKind::ListType) | `]` | `ty.rs` unclosed — `type T { f: [Int` |
//! | [`ListValue`](SyntaxKind::ListValue) | `]` | `value.rs` unclosed — `{ f(a: [1` |
//! | [`ObjectValue`](SyntaxKind::ObjectValue) | `}` | `value.rs` unclosed — `{ f(a: {b: 1` |
//! | [`VariablesDefinition`](SyntaxKind::VariablesDefinition) | `)` | `executable.rs` unclosed — `query Q($a: Int`; refuses either way, the operation being lost |
//! | [`ArgumentsDefinition`](SyntaxKind::ArgumentsDefinition) | `)` | `definition.rs` unclosed — `type T { f(a: Int`; refuses either way, the definition being lost |
//!
//! **This table is the criterion's whole extension, derived rather than listed.** The
//! report-and-build census above enumerates every place the parser leaves a token out without a
//! hole, and its thirteen image-less rows — each witnessed by its own probe's tree — are these
//! thirteen, which the cell asserts. Two are witnessed only as **orphans**: at end of input the
//! operation or the field around the list cannot be finished, so the root keeps the list as a stray
//! child beside the lost definition's tokens, and every projection of such a tree refuses at the
//! rubble. They are lenient by the criterion and change no answer.
//!
//! The `on` of an **inline** fragment is not a row, and not by omission: the same `type_condition`
//! production serves both fragments, but the spread dispatch opens an inline fragment *because* it
//! read `on`, so no parse holds an inline fragment's type without one. Its walk requires the `on`
//! in front of the type.
//!
//! Every row is also **measured as a value** where a value exists: `tests/lossless_mutation.rs`
//! deletes the token from a real tree — or finds a corpus tree already without it — and requires
//! the projection to equal the syntactic parse of the text **with every missing lenient token
//! restored**, innermost first at a shared offset, spans mapped back through the splices. Its
//! per-row counts are asserted, and its own table is asserted equal to this one.
//!
//! **A described shorthand or extension is not in this class**: the description is *present*, and
//! the syntactic parser rejects the combination categorically rather than doing without a token.
//!
//! **Which of these are *rules* rather than shapes.** The spelling rules are **derived from the
//! syntactic crate**, not remembered: every production under `graphql/syntactic/**` whose match on
//! a keyword *refuses* a `Name` rather than dispatching on it. The grep is `downcast_ref()` over
//! that tree — this dialect's productions classify a token through the lexer's `DowncastRef`
//! rather than through a `keyword_of` helper — filtered to the sites whose match refuses: four
//! positions.
//!
//! | position | the syntactic refusal | the projection |
//! |---|---|---|
//! | a fragment's name is not `on` | `mod.rs:132`, `fragment_name` (`Expectation::FragmentName`), reached from `executable/mod.rs:647` | `name_except` in `fragment_definition` |
//! | a fragment spread's target is not `on` | `selection/mod.rs:338` (the spread dispatch reads `on` as an inline fragment's head) and `fragment_name` again at `selection/mod.rs:443` | `name_except` in `fragment_spread` |
//! | an enum value is not `true`, `false` or `null` | `value/mod.rs:393` and `:537` (the enum productions), and the value dispatch that reads those spellings as a boolean or a null in both grammars — `value/mod.rs:969-975` non-const, `:1081-1087` const | `name_except` in `enum_value_name`, shared by both value grammars |
//! | an enum value definition is not named `true`, `false` or `null` | `definition/enum_type.rs:20` (`Expectation::EnumValue`) | `name_except` in `enum_value_name` — the declaring name sits inside the same `EnumValue` node kind |
//!
//! Each answers [`SemanticRule`](ProjectErrorKind::SemanticRule) at the offending name. The spread
//! and value-position rules are invisible to the mutation law — a tree the parser builds never
//! holds them, and a retexted one re-parses to a different skeleton — so hand-built cells pin all
//! four. Every other `downcast_ref()` match dispatches (a definition's or an extension's keyword,
//! an operation type, a directive location) and its refusal is a classification rather than a
//! rule: a location or an operation keyword the lexer's table does not classify is
//! [`MalformedToken`](ProjectErrorKind::MalformedToken).
//!
//! **No other position reserves a spelling.** This dialect's keywords are contextual: the lexer
//! reads `on`, `query` and `type` as identifiers, and the syntactic parser accepts
//! `type on { on: on }` — so every other name position takes any identifier, through the one door
//! `name_token`. `a_contextual_keyword_is_a_name_at_every_name_position` pins it.
//!
//! # What is walked, and how a span is folded
//!
//! Every function below takes a [`Node`](crate::lossless::project::Node) — a green node
//! plus where it starts — and never a rowan cursor. See
//! [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
//! for why: a cursor's parent pointer is what the dispatch already carries — a call frame in the
//! bounded part of the grammar, a worklist entry in the four cycles below — and its offset
//! is what [`Node::children`](crate::lossless::project::Node::children) accumulates, so
//! the allocation per element it costs buys nothing here. The two whole-tree checks a door makes
//! were already green; now the walk between them is too, and a fail-fast projection allocates for
//! the AST it builds, for the worklist entry each branching ancestor past the sixteenth costs the
//! two checks, and for nothing else. [`verify_parse`] carries that second clause in full, with the
//! 95-byte document that reaches it.
//!
//! Every span is the **token extent** of the constituents it covers — never the node's own
//! range, which includes committed trivia. See [`crate::lossless::project`] for that
//! rule and the measurement behind it. It is folded **bottom-up, once**: each node function reads
//! its own children a single time, through the substrate's cursor in its production's order,
//! covers the ranges of the tokens its atoms consume, and covers the extent each projected child
//! hands back beside its AST value.
//!
//! Two places the tree's geometry and the AST's differ, and both are span-relevant:
//!
//! - **Descriptions hoist.** The CST hangs a [`Description`](SyntaxKind::Description) node
//!   *inside* the definition it precedes; the AST lifts it into the [`Described`] wrapper. At
//!   document level the wrapper spans description-through-definition while the inner definition
//!   starts *after* the description, so the inner span is synthesised by folding the node's
//!   constituents with the description excluded.
//! - **…except in three node types**, where the syntactic parser gives the wrapper and the inner
//!   node the *same* span, description included: `FieldDefinition`, `InputValueDefinition` and
//!   `EnumValueDefinition`. `VariableDefinition` — the fourth described node below document
//!   level — follows the document-level rule instead, so the four do not agree with each other.
//!   That asymmetry is trunk's, not this module's, and it is reproduced rather than corrected:
//!   `tests/lossless_project.rs` compares against the parser, so a "fix" here would be a
//!   divergence.
//!
//! # Every non-trivia byte is represented, or the walk refuses
//!
//! A byte that is merely *covered* — folded into a span while reaching no AST field — is the shape
//! of guarantee 3's failure. al8n/smear#218's addenda counted this file's ways to produce one at
//! trunk `c885c07`, and every count held unchanged until this rewrite:
//!
//! | the hatch | what it dropped | the obligation now |
//! |---|---|---|
//! | `extent.token(token)` folded any non-trivia token — **48** sites | a stray `@`, a duplicated delimiter, a second name | **gone** — a token is consumed only by an atom that names its kind, at its place in the sequence |
//! | `extent.unread(child)` covered a node with no arm — **34** sites, **28** of them `_ =>` wildcards, and **67** `is_none()` dispatch guards fell through to one | a second `Directives`, a second `SelectionSet`, a whole `FieldsDefinition` under a scalar | **gone**, and so are the guards — a child is consumed by an atom or refused by `end` |
//! | `Names`, `[Option<Token>; 3]`, read by index at **22** sites | a fourth name; a fragment name split into `Name("o")` `Name("n")` passing the `on` rule | **gone** — a name is one atom per position the production spells |
//! | a leaf that took the first token of its kind and folded the rest | `IntValue` over `Int("1")` and `Int("2")` answering `1` with a span across both | **gone** — a leaf is one token atom and then `end` |
//! | a keyword slot read by index and never read | `directive @d foo FIELD` answering what `directive @d on FIELD` answers | each keyword is an atom at its own position |
//! | a **shared** extension walker taking the union of six tails | a `ScalarTypeExtension` holding a `FieldsDefinition`, projected `Ok` with the block dropped | one transcription per kind |
//! | the query shorthand's branch keeping a description and covering what it forbade | `"d" { f }` projected described; a recovering door counting it complete | the shorthand's sequence has no description |
//!
//! `every_walk_is_a_transcription` reads this file's own code and asserts the census: no
//! `extent.token(`, no `unread(`, no `Names`, no `is_none() =>` guard, no wildcard arm that covers,
//! and one child loop left — the recovering door's pass over the root. The file had **52** child
//! loops; each became its production's cursor walk.
//!
//! **What "represented" means.** A token is represented when its *kind* is one the shape's own
//! production spells, and, if its *text* would reach an AST field, when it actually reaches one. So
//! a `{`, an `&` between two interfaces and a definition's `type` keyword are represented by being
//! consumed: their text carries nothing the node kind does not already say. A `Name`, a literal
//! image and the `repeatable` in a directive definition are not: their text selects a value.
//!
//! ## A walk is its production transcribed
//!
//! A walk that dispatches children into slots by kind can express a shape's **set** of children and
//! cannot express their **sequence** or **multiplicity**, and wherever a production has a committing
//! prefix (`on`, `implements`, `=`, `:`) or a separator (`&`, `|`) the same kinds in a different
//! order or count are a different sentence. So each walk is a sequence of atoms over the
//! substrate's cursor in the grammar's own order, ending in `end`: *represented* stops being a
//! property checked against a table and becomes **consumed by an atom**, and sequence,
//! multiplicity, vocabulary, one-of exclusivity, committing prefixes and separators become
//! consequences of the transcription. There is no `token(K::Name)`: a `Name` is consumed by
//! `keyword`, `name_token` or `spelling`.
//!
//! **What checks the transcription.** A node handed back by an atom and then neither projected nor
//! descended into is the one cover-and-drop this form cannot make impossible.
//! `tests/lossless_mutation.rs` is what finds it: it perturbs every hole-free corpus tree one child
//! at a time and requires the projection to agree with the syntactic parser — with every lenient
//! token restored, where one is missing — or refuse. Run against the slot walks this replaces it
//! found **386** violations in 30 classes, 22 of them panics; against this file it finds none, and
//! every population and bucket it counts is an asserted constant.
//!
//! ## The same rule one level down: leaf text
//!
//! A token whose text reaches the AST **as a value** is re-cooked through the lexer's own
//! whole-slice door, because on a caller-minted tree the bytes under a token are whatever the
//! caller wrote and the kind label is not evidence. A token whose text only contributes a *range*
//! — every keyword and every piece of punctuation — is **not** re-cooked.
//!
//! | leaf | becomes | door |
//! |---|---|---|
//! | [`Int`](SyntaxKind::Int) | the raw slice, in [`IntValue`] | [`int_literal`], the shipped scanner, whole-slice — it validates the spelling and hands the slice back |
//! | [`Float`](SyntaxKind::Float) | the raw slice, in [`FloatValue`] | [`float_literal`], likewise; the two doors do not coerce into each other |
//! | [`String`](SyntaxKind::String), [`BlockString`](SyntaxKind::BlockString) | [`LitStr`] | `LitStr::try_from`, the string sub-lexer, as before |
//! | [`Name`](SyntaxKind::Name) whose text becomes a name | [`Name`] | [`identifier`], added beside GraphQLx's for this — every name position reaches it through `name_token` |
//! | a `Name` read for its **spelling** | an operation type, a directive location, `true`/`false`/`null` | `contextual_keyword` — the lexer's own table — and every reader of it refuses a spelling it does not classify |
//!
//! The last row is where [`NullValue`](SyntaxKind::NullValue) was: `BooleanValue` always compared
//! its text and `NullValue` did not, so a `NullValue` node over any identifier projected to a
//! `null` carrying that identifier's bytes. It classifies now.
//!
//! # Empty containers: the three-way rule
//!
//! A container the tree opens can be empty, and what the AST does about that is decided by the
//! **syntactic production**, not by the node's presence. There are exactly three answers:
//!
//! | the grammar | the tree | the AST | this projection |
//! |---|---|---|---|
//! | `X+` | node present, no members | *no value* — the parser rejects the document | [`MissingChild`](ProjectErrorKind::MissingChild) |
//! | `X*` inside delimiters, mapped to `None` when empty | node present, no members | `None` | `None`, with the node's extent still covered |
//! | optional, undelimited | no node | `None` | `None`, nothing covered |
//!
//! **Why the first row is a refusal and not an empty carrier.** The projection never produces a
//! value outside the syntactic parser's image, and a `SelectionSet` with no selections or a
//! `FieldsDefinition` with no fields is a value the parser produces for no input. It is **not** the
//! unclosed-brace case, which projects precisely because its image *is* a value the parser
//! produces, for the closed text.
//!
//! ## The list, derived twice
//!
//! Once from the **productions** — a `report_unexpected` on an empty body, a mandatory first
//! element followed by a `while` — and once from the **AST**: every container this file constructs
//! from a `Vec`. What follows is the **union**; the two derivations differ by the two directive
//! runs, which the production side misses because `directives` opens its node only at an `@` and
//! never reports an empty one. Every row is measured.
//!
//! | AST container | grammar | present-empty | pinned by |
//! |---|---|---|---|
//! | [`Document`](SyntaxKind::Document), [`TypeSystemDocument`](SyntaxKind::TypeSystemDocument), [`ExecutableDocument`](SyntaxKind::ExecutableDocument) | `Definition+` | `MissingChild` | `every_report_and_build_site_has_a_measured_answer` |
//! | [`SelectionSet`](SyntaxKind::SelectionSet) | `Selection+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`VariablesDefinition`](SyntaxKind::VariablesDefinition) | `VariableDefinition+` in `( )` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`FieldsDefinition`](SyntaxKind::FieldsDefinition) | `FieldDefinition+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`ArgumentsDefinition`](SyntaxKind::ArgumentsDefinition) | `InputValueDefinition+` in `( )` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`InputFieldsDefinition`](SyntaxKind::InputFieldsDefinition) | `InputValueDefinition+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`EnumValuesDefinition`](SyntaxKind::EnumValuesDefinition) | `EnumValueDefinition+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`RootOperationTypeDefinitions`](SyntaxKind::RootOperationTypeDefinitions) | `RootOperationTypeDefinition+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`ImplementsInterfaces`](SyntaxKind::ImplementsInterfaces) | `NamedType+` after `implements`, separated by `&` | `MissingChild` | `the_separated_atoms_refuse_at_the_obstruction` |
//! | [`UnionMemberTypes`](SyntaxKind::UnionMemberTypes) | `NamedType+` after `=`, separated by `\|` | `MissingChild` | `the_separated_atoms_refuse_at_the_obstruction` |
//! | [`DirectiveLocations`](SyntaxKind::DirectiveLocations) | `Name+` separated by `\|` | `MissingChild` | `the_separated_atoms_refuse_at_the_obstruction` |
//! | [`Directives`](SyntaxKind::Directives), both flavours | `Directive+` | `MissingChild` | `a_present_directive_run_with_no_directive_refuses` |
//! | [`Arguments`](SyntaxKind::Arguments), both flavours | `Argument*` in `( )` | `None`, extent covered | `a_written_down_empty_argument_list_is_none_with_a_span` |
//! | a list or object **value** | `Value*` in its delimiters | an empty container value | `a_written_down_empty_argument_list_is_none_with_a_span` |
//!
//! The last row is not a fourth answer to the same question: those are not optional constituents at
//! all, they are values, and `[]` is as much a value as `[1]`.
//!
//! **Row two, measured rather than asserted.** `Arguments` is in it only because the syntactic
//! parser really does answer `None` for a written-down empty list: over `query Q { f() }` the
//! parse's field answers `arguments().is_none() == true`, and so do `type T @d() { f: Int }` and
//! `query Q { f @d() }` — compared with plain `==` against `graphql::syntactic::document` in
//! `a_written_down_empty_argument_list_is_none_with_a_span`. al8n/smear#217.
//!
//! **The one container that may be empty and is not in the table** is the recovering doors'
//! accumulator: [`project_executable_document_recovered`] and its twin answer a document with no
//! definitions when every entry was skipped. That is the recovery contract — [`Recovery::skipped`]
//! is the bound on what was lost — rather than a cardinality claim.
//!
//! # No node dispatch below spends a native frame per level
//!
//! The grammar is bounded above a value: a document holds definitions, a definition holds fields,
//! a field holds arguments, and none of those can contain another of itself. **Four cycles are
//! not** — `value` ↔ `object_field`, `const_value` ↔ `const_object_field`, `selection_set` ↔
//! `field`/`inline_fragment`, and `ty` ↔ `list_element`/`non_null_type` — and each is a worklist
//! rather than a recursion. al8n/smear#201.
//!
//! Each cycle is a **worklist**, and they share one shape:
//!
//! - A frame is a container the walk has entered and not finished: its **cursor** — the node, its
//!   fold so far and the children it has not read — and the accumulator it is filling. A container
//!   costs one frame however **wide** it is, because the cursor adopts the tree's own child
//!   iterator rather than copying the children out.
//! - **The fold stays bottom-up**: a parent's span is the cover of its children's, so a parent
//!   cannot be finished before them. A frame is therefore only ever completed by the value the
//!   level below hands back, and the constituent that value belongs to — an `ObjectField` waiting on
//!   its value, a `Field` waiting on its selection set — travels **on the frame** as an open slot.
//! - A frame is pushed only with a live descent already chosen below it, so the open slot is never
//!   empty while the frame is on the stack, and it travels by value: an open selection holds a
//!   finished `Alias`, `Arguments` and `Directives` that have to leave the frame when it closes.
//!
//! What that costs is one heap entry per nesting level in place of a native frame, bounded by
//! `MAX_GREEN_DEPTH` — inherited rather than re-counted, because every door into this module opens
//! with a verification that refuses past it. `smear-parser/tests/deep_projection.rs` reads the
//! flatness off a real projection, one fixture per cycle and one per carrier a cycle passes
//! through.

use std::vec::Vec;

use rowan::{NodeOrToken, TextRange, TextSize};
use tokora::SimpleSpan;

use smear_lexer::{
  LitStr,
  graphql::{
    ContextualKeyword, float_literal, identifier, int_literal, keyword::contextual_keyword,
  },
  keywords::{Mutation, Query, Subscription},
};

use crate::{
  graphql::{
    ast::{
      Alias, Argument, Arguments, BooleanValue, ConstArgument, ConstArguments, ConstDirective,
      ConstDirectives, ConstInputValue, ConstList, ConstObject, ConstObjectField,
      DefaultInputValue, DefinitionOrExtension, Described, DescribedExecutableDefinition,
      DescribedVariableDefinition, Directive, Directives, Document, EnumTypeDefinition,
      EnumTypeExtension, EnumValue, EnumValuesDefinition, ExecutableDefinition, ExecutableDocument,
      Field, FieldsDefinition, FloatValue, FragmentName, FragmentSpread, ImplementInterfaces,
      InlineFragment, InputFieldsDefinition, InputObjectTypeDefinition, InputObjectTypeExtension,
      InputValue, IntValue, InterfaceTypeDefinition, InterfaceTypeExtension, List, ListType,
      Location, Name, NamedOperationDefinition, NamedType, NullValue, Object, ObjectField,
      ObjectTypeDefinition, ObjectTypeExtension, OperationDefinition, OperationType,
      RootOperationTypeDefinition, RootOperationTypesDefinition, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, Selection, SelectionSet, StringValue,
      Type, TypeCondition, TypeDefinition, TypeExtension, TypeSystemDefinition,
      TypeSystemDefinitionOrExtension, TypeSystemDocument, TypeSystemExtension, UnionMemberTypes,
      UnionTypeDefinition, UnionTypeExtension, VariableDefinition, VariableValue,
      VariablesDefinition,
    },
    kinds::{GraphQLLang, SyntaxKind},
    lossless::Parse,
    syntactic::definition::classify_location,
  },
  lossless::project::{
    Recovery, Unverified, reject_foreign_kinds_and_holes, to_range, to_span, verify_root_kind,
    verify_source, verify_source_at, verify_source_counted,
    walk::{
      Extent, Leading, Optional, Trivia, described_extents, missing, unexpected, unexpected_node,
    },
  },
};

// Spelled out rather than folded into the group above, so `tests/lossless_isolation.rs`'s source
// census — which reads `crate::…` prefixes out of the text — can see the edge. These are the
// **shared, dialect-free** AST carriers: the undescribed cores three `Described<…>` aliases wrap,
// and the six `…Data` enums an extension's alternatives are encoded in. Neither has a spelling
// under `graphql::ast`, and a projection has to construct both.
use crate::type_system::{
  ArgumentsDefinition, DirectiveDefinition, DirectiveLocations, EnumTypeExtensionData,
  EnumValueDefinition, FieldDefinition, InputObjectTypeExtensionData, InputValueDefinition,
  InterfaceTypeExtensionData, ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

use SyntaxKind as K;

/// A refusal from the GraphQL projection, keyed by this dialect's [`SyntaxKind`].
pub type ProjectError = crate::lossless::project::ProjectError<SyntaxKind>;

/// Why the GraphQL projection refused, keyed by this dialect's [`SyntaxKind`].
pub type ProjectErrorKind = crate::lossless::project::ProjectErrorKind<SyntaxKind>;

/// A green node and where it starts, in this dialect's kind space.
///
/// The unit every function below walks. See
/// [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
/// for why the traversal is green and what a cursor would have cost.
type Node<'g> = crate::lossless::project::Node<'g, GraphQLLang>;

/// [`Node`]'s other half.
type Token<'g> = crate::lossless::project::Token<'g, GraphQLLang>;

/// A node's child sequence, read in its production's order — the substrate's cursor over this
/// dialect's kind space. See the substrate's `walk` module for the atoms and why they are shared.
type Cursor<'g> = crate::lossless::project::walk::Cursor<'g, GraphQLLang>;

type Out<T> = Result<T, ProjectError>;

impl Unverified {
  /// The reason a [`ProjectError`] from a whole-root verification names.
  ///
  /// The two are established by one walk and were collapsed into one name at this boundary — the
  /// third time on al8n/smear#198 that two abandonments with different remedies met a channel that
  /// could carry one. The others were an arena refusal wearing the budget's `None` and a stale pair
  /// wearing the budget's refusal; this one is a shape wearing a mismatch.
  ///
  /// # Why the constructor is here and the type is in the substrate
  ///
  /// [`Unverified`] is what a dialect-free verification answers, so it belongs to the substrate.
  /// *Building* one out of a [`ProjectError`] is something only a projection door does, and the
  /// only doors that exist are this module's two — [`Verified::new`] and
  /// [`recovered_top_level`]. A `pub(crate)` item in the substrate with no in-crate caller is
  /// `dead_code` under `-Dwarnings` whenever the crate is built with no dialect at all, which is
  /// what `lossless-coverage` and `cargo hack --each-feature` do.
  ///
  /// The first repair for that was a `#[cfg(feature = "graphql")]` on the item where it stood,
  /// and it was the wrong one: `tests/lossless_isolation.rs` pins every dialect-facing gate in the
  /// substrate at `any(feature = "graphql", feature = "graphqlx")` precisely so that a generic
  /// layer cannot acquire a favourite dialect, and the narrower gate is that drift by definition.
  /// The `dead_code` denial was not noise to silence — it was the substrate reporting that the
  /// item had no business living there. Moving it to the dialect that reads it discharges both:
  /// the gate comes from `pub mod graphql`, which already carries one, and the substrate goes back
  /// to naming no dialect at all.
  ///
  /// A GraphQLx projection door, when there is one, writes its own — over its own `SyntaxKind`,
  /// with its own `ProjectErrorKind`. That is a second three-line `match` rather than a shared
  /// generic one, and it is the right trade: the alternative puts a `pub(crate)` item back in the
  /// substrate whose liveness depends on which dialects are compiled.
  pub(crate) fn of(error: &ProjectError) -> Self {
    match error.kind() {
      ProjectErrorKind::TooDeep { limit } => Self::TooDeep { limit: *limit },
      ProjectErrorKind::WrongRoot { raw } => Self::WrongRoot { raw: *raw },
      _ => Self::SourceMismatch,
    }
  }
}

/// Project a lossless parse to the AST the syntactic parser produces for `source`.
///
/// The root this reads is the mixed [`Document`](SyntaxKind::Document) that
/// [`parse_document`](super::parse_document) builds.
///
/// See the module header for the contract; the short version is: check
/// [`has_errors`](Parse::has_errors) first, pass the same text the tree was parsed from, and
/// expect a refusal rather than a guess when the tree carries a hole.
pub fn project<'src>(parse: &Parse, source: &'src str) -> Out<Document<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  document(sole_document(root, K::Document, "a document")?, source)
}

impl super::ast::Document {
  /// Project this document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project`], for a caller that already holds the typed wrapper.
  ///
  /// The scan is **scoped rather than skipped**: a hole anywhere in this node's subtree refuses,
  /// and a hole elsewhere in the parse is not this node's business. It used to be skipped, on the
  /// claim that a hole inside the subtree would still refuse when the walk reached it — and that
  /// claim was false while the walk's permissive arms routed a child they had no slot for into the
  /// parent's *extent*, folding its bytes without ever looking at its kind. al8n/smear#218,
  /// finding 2.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<Document<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    scan_holes(node)?;
    document(node, source)
  }
}

/// Project a lossless **executable** parse to the AST the syntactic parser produces for `source`.
///
/// [`project`]'s root swapped: this reads the
/// [`ExecutableDocument`](SyntaxKind::ExecutableDocument) that
/// [`parse_executable_document`](super::parse_executable_document) builds, and answers the
/// `ExecutableDocument<&str>` that `syntactic::executable_document` answers for the same bytes.
/// Everything else — the hole scan, the verified `(tree, source)` pair, the token-extent span
/// rule — is [`project`]'s, unchanged.
///
/// The root matters. A mixed parse holds a [`Document`](SyntaxKind::Document) node, so it is
/// refused here rather than filtered: dropping the type-system half of a mixed document would
/// answer a different question from the one the executable root asks, and the executable root is
/// the one that reports an SDL definition *at the parser's own position*.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{parse_executable_document, project_executable_document};
///
/// let source = "query Q { hero { name } }";
/// let parse = parse_executable_document(source);
/// assert!(!parse.has_errors());
///
/// let ast = project_executable_document(&parse, source).expect("a well-shaped tree projects");
/// assert_eq!(ast.definitions().len(), 1);
/// # }
/// ```
pub fn project_executable_document<'src>(
  parse: &Parse,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  executable_document(
    sole_document(root, K::ExecutableDocument, "an executable document")?,
    source,
  )
}

/// Project every definition of a lossless **executable** parse that has an AST image, and count
/// the ones that do not.
///
/// [`project_executable_document`] is fail-fast: one hole anywhere and the whole document is
/// refused.
///
/// This door walks the top level instead, projects each definition **independently**, and keeps
/// the ones that succeeded. What it could see is the [`Recovery`].
///
/// # What counts as a top-level element
///
/// Every child of the tree's [`Root`](SyntaxKind::Root), with the
/// [`ExecutableDocument`](SyntaxKind::ExecutableDocument) node stepped *through* rather than
/// descended into as the whole population. Both halves of that are load-bearing. The lost-node
/// recovery class drops a failed document production's children straight under the root, so
/// `"{ a }\nquery Bad("` has no document node at all and its one good operation is reachable only
/// from the root; and a gap tile can land beside an *empty* document node, so `"%"` has one
/// top-level element and it is not the document node's child.
///
/// Every container of the root's kind is stepped through, not only the first: a caller-minted root
/// holding two valid containers projects the definitions of both and reports complete, where this
/// door's fail-fast twin, which asserts exactly one container, refuses the second. Each container
/// is a legitimate document image and this door's contract is per entry — see [`Recovery`].
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_executable_document, project_executable_document_recovered,
/// };
///
/// // The second operation is half-typed; the first one is not.
/// let source = "{ hero { name } }\nquery Bad(";
/// let parse = parse_executable_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_executable_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
///
/// // A `source` the parse does not describe is refused rather than projected — including one that
/// // merely *extends* the parse's text: the whole-root verification compares lengths, so the
/// // appended bytes are refused rather than silently absent from a complete-looking recovery.
/// let longer = format!("{source} query More {{ hero {{ name }} }}");
/// assert!(project_executable_document_recovered(&parse, &longer).is_err());
/// # }
/// ```
pub fn project_executable_document_recovered<'src>(
  parse: &Parse,
  source: &'src str,
) -> Result<(ExecutableDocument<&'src str>, Recovery), Unverified> {
  let (span, definitions, recovery) =
    recovered_top_level(parse, K::ExecutableDocument, recoverable_entry, source)?;
  Ok((ExecutableDocument::new(span, definitions), recovery))
}

/// [`project_executable_document_recovered`] for a pair that already carries its verification.
///
/// Infallible: a [`Verified`] is the proof the fallible form's error half exists to report, so
/// there is no error half left. See [`Verified`] for why the type exists rather than the check
/// simply being moved.
pub fn project_executable_document_verified<'src>(
  pair: Verified<'_, 'src>,
) -> (ExecutableDocument<&'src str>, Recovery) {
  let (span, definitions, recovery) = recovered_top_level_verified(
    pair.parse(),
    K::ExecutableDocument,
    recoverable_entry,
    pair.source(),
  );
  (ExecutableDocument::new(span, definitions), recovery)
}

impl super::ast::ExecutableDocument {
  /// Project this executable-document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project_executable_document`], and
  /// [`Document::to_ast`](super::ast::Document::to_ast)'s twin: like it, the hole scan is scoped to
  /// this node's own subtree rather than run over the whole parse, and see it for why scoping it is
  /// not the same as skipping it.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<ExecutableDocument<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    scan_holes(node)?;
    executable_document(node, source)
  }
}

/// Project a lossless **type-system** parse to the AST the syntactic parser produces for `source`.
///
/// [`project`]'s root swapped the other way: this reads the
/// [`TypeSystemDocument`](SyntaxKind::TypeSystemDocument) that
/// [`parse_type_system_document`](super::parse_type_system_document) builds, and answers the
/// `TypeSystemDocument<&str>` that `syntactic::type_system_document` answers for the same bytes —
/// which is the document [`Schema::build`] consumes. Everything else — the hole scan, the verified
/// `(tree, source)` pair, the token-extent span rule — is [`project`]'s, unchanged.
///
/// The root matters, for [`project_executable_document`]'s reason mirrored: a mixed parse holds a
/// [`Document`](SyntaxKind::Document) node and is refused here rather than filtered, because
/// dropping the executable half of a mixed document would answer a different question from the one
/// the SDL root asks, and the SDL root is the one that reports an operation *at the parser's own
/// position*.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_type_system_document, project_type_system_document,
/// };
///
/// let source = "type Query { hero: String }";
/// let parse = parse_type_system_document(source);
/// assert!(!parse.has_errors());
///
/// let ast = project_type_system_document(&parse, source).expect("a well-shaped tree projects");
/// assert_eq!(ast.definitions().len(), 1);
/// # }
/// ```
///
/// [`Schema::build`]: https://docs.rs/smear/latest/smear/validator/struct.Schema.html
pub fn project_type_system_document<'src>(
  parse: &Parse,
  source: &'src str,
) -> Out<TypeSystemDocument<&'src str>> {
  let root = parse_root(parse);
  open(root, source)?;
  type_system_document(
    sole_document(root, K::TypeSystemDocument, "a type system document")?,
    source,
  )
}

/// Project every definition of a lossless **type-system** parse that has an AST image, and count
/// the ones that do not.
///
/// [`project_executable_document_recovered`]'s mirror at the SDL root, walking the same top level
/// with the same accounting.
///
/// ```
/// # #[cfg(all(feature = "graphql", feature = "rowan"))] {
/// use smear_parser::graphql::lossless::{
///   parse_type_system_document, project_type_system_document_recovered,
/// };
///
/// // The second type's field has no type yet; the first one is finished.
/// let source = "type Query { hero: String }\ntype Half { f: }";
/// let parse = parse_type_system_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_type_system_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
///
/// // And a `source` the parse does not describe is refused rather than projected.
/// let longer = format!("{source} type Extra {{ n: Int }}");
/// assert!(project_type_system_document_recovered(&parse, &longer).is_err());
/// # }
/// ```
pub fn project_type_system_document_recovered<'src>(
  parse: &Parse,
  source: &'src str,
) -> Result<(TypeSystemDocument<&'src str>, Recovery), Unverified> {
  let (span, definitions, recovery) = recovered_top_level(
    parse,
    K::TypeSystemDocument,
    recoverable_type_system_entry,
    source,
  )?;
  Ok((TypeSystemDocument::new(span, definitions), recovery))
}

/// [`project_type_system_document_recovered`] for a pair that already carries its verification.
///
/// [`project_executable_document_verified`]'s mirror at the SDL root.
pub fn project_type_system_document_verified<'src>(
  pair: Verified<'_, 'src>,
) -> (TypeSystemDocument<&'src str>, Recovery) {
  let (span, definitions, recovery) = recovered_top_level_verified(
    pair.parse(),
    K::TypeSystemDocument,
    recoverable_type_system_entry,
    pair.source(),
  );
  (TypeSystemDocument::new(span, definitions), recovery)
}

impl super::ast::TypeSystemDocument {
  /// Project this type-system-document node to the AST the syntactic parser produces for `source`.
  ///
  /// The compositional form of [`project_type_system_document`], and
  /// [`ExecutableDocument::to_ast`](super::ast::ExecutableDocument::to_ast)'s twin, scoping its
  /// hole scan for the same reason.
  pub fn to_ast<'src>(&self, source: &'src str) -> Out<TypeSystemDocument<&'src str>> {
    let node = Node::of(self.syntax());
    open_node(node, source)?;
    scan_holes(node)?;
    type_system_document(node, source)
  }
}

/// A parse and the source it was produced from, **verified once**.
///
/// [`Verified::new`] is the only constructor: it runs the whole-root byte comparison and the root
/// check once. [`project_executable_document_verified`] and its twin take one and run no
/// verification, so they have no error half.
/// al8n/smear#198.
#[derive(Clone, Copy)]
pub struct Verified<'p, 'src> {
  parse: &'p Parse,
  source: &'src str,
  /// What projecting this pair costs, in elements — see [`Verified::projection_cost`].
  elements: u32,
}

impl core::fmt::Debug for Verified<'_, '_> {
  /// The source, and that the pair is verified. `Parse` is not `Debug` — a green tree has no useful
  /// rendering — so the half that can be shown is.
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Verified")
      .field("source", &self.source)
      .finish_non_exhaustive()
  }
}

impl<'p, 'src> Verified<'p, 'src> {
  /// Verifies that `source` is the whole text `parse` was produced from, and that the parse's root
  /// is this dialect's document root.
  ///
  /// The second half exists because `finish_root` is public and generic, so a `Parse` can be
  /// minted over a root that is not this dialect's `Root` — outside the kind space, or an in-space
  /// kind such as `Name` — and its bytes verify. The refusal is [`Unverified::WrongRoot`]. Kinds
  /// below the root are checked by each entry's own scan, so a recovering door counts an entry
  /// holding one as skipped. al8n/smear#218.
  ///
  /// `O(green elements + source bytes)` over the borrowed green root: [`verify_source_counted`]
  /// visits every node and token and compares every token's bytes. See [`verify_parse`], which is
  /// the same comparison and answers the same [`Unverified`].
  ///
  /// # Allocation
  ///
  /// **It allocates nothing through sixteen branching ancestors.** The comparison keeps one entry
  /// per ancestor of the node in hand that still has an unvisited child, the first sixteen of them
  /// in a fixed array in its own frame; a seventeenth spills to the heap through an infallible
  /// `push`, bounded by [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH).
  pub fn new(parse: &'p Parse, source: &'src str) -> Result<Self, Unverified> {
    // Counted by the same walk that verifies, so the proof and the price are established together
    // and cost one pass between them. See [`Verified::projection_cost`].
    //
    // And the root's raw kind, which the byte walk never reads: a `Verified` proves the recovering
    // walk's first question — whether a root child is the container — can be asked of this root
    // without answering for a tree outside this dialect's space.
    match verify_source_counted::<SyntaxKind>(parse.green(), source)
      .and_then(|elements| verify_root_kind::<SyntaxKind>(parse.green()).map(|()| elements))
    {
      Ok(elements) => Ok(Self {
        parse,
        source,
        elements,
      }),
      // The counted walk distinguishes a byte divergence from a shape refusal, and `Unverified::of`
      // keeps them apart: a pair whose bytes agree exactly is never reported as stale.
      Err(refusal) => Err(Unverified::of(&refusal)),
    }
  }

  /// What projecting this pair costs, in **elements** — one per green node and one per token.
  ///
  /// `Verified` proves the *bytes* agree, and bytes do not bound structure:
  /// [`finish_root`](crate::lossless::runner::finish_root) is public, so a caller can mint a
  /// `Parse` from its own CST event stream, and a balanced pair of **zero-width** GraphQL nodes adds
  /// structure without adding a byte. This count is taken by the same walk that verified the pair.
  /// It saturates at [`u32::MAX`]: no finite validation budget covers that cost, and a disabled
  /// ledger or a projection that takes no budget proceeds.
  /// al8n/smear#198.
  #[inline]
  pub const fn projection_cost(&self) -> u32 {
    self.elements
  }

  /// The parse half of the pair.
  #[inline]
  pub const fn parse(&self) -> &'p Parse {
    self.parse
  }

  /// The source half of the pair.
  #[inline]
  pub const fn source(&self) -> &'src str {
    self.source
  }
}

/// That `parse` and `source` describe the same bytes, over the **whole root**, and that the root is
/// this dialect's document root — or the reason the pair is refused.
///
/// The recovering doors' precondition: their entries compare no bytes, so this whole-root
/// comparison is the only one they make. It compares lengths first, so a `source` that *begins*
/// with the parse's text and then adds to it is refused before a byte is walked.
///
/// [`verify_source`] over the parse's **green** root — the comparison the fail-fast doors open
/// with — then [`verify_root_kind`]. It is `O(green elements + source bytes)`: every node and token
/// is visited and every token's bytes are compared. It reads no `Parse` state beyond a borrow.
///
/// The recovering projections make the same check themselves and answer [`Unverified`].
///
/// # Allocation
///
/// **It allocates nothing through sixteen branching ancestors.** The comparison keeps one entry per
/// ancestor of the node in hand that still has an unvisited child, the first sixteen of them in a
/// fixed array in its own frame; a seventeenth spills to the heap through an infallible `push`,
/// bounded by [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH). A chain of
/// single-child nodes holds one entry however long it is, and a node is handed over whole, so a
/// wide one is one entry too. `tests/validator_allocation.rs`'s
/// `the_whole_root_check_allocates_nothing` measures the zero over a fixture whose branching
/// nesting is three.
///
/// # Why this is not a `bool`
///
/// It refuses for three reasons, and [`Unverified`] names which: the bytes differ
/// ([`Unverified::SourceMismatch`]), the tree is deeper than
/// [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH) whatever its bytes say
/// ([`Unverified::TooDeep`]), or the root is not this dialect's ([`Unverified::WrongRoot`]).
/// [`Verified::new`] refuses at the same ceiling. al8n/smear#198.
pub fn verify_parse(parse: &Parse, source: &str) -> Result<(), Unverified> {
  verify_source::<SyntaxKind>(parse.green(), source)
    .and_then(|()| verify_root_kind::<SyntaxKind>(parse.green()))
    .map_err(|refusal| Unverified::of(&refusal))
}

/// The recovering top-level walk, shared by both single-half roots.
///
/// One implementation rather than one per root, because what it computes is [`Recovery`] and both
/// doors report it. The root's kind and the entry projection are the arguments; `entry_of` is a
/// `fn` pointer.
///
/// # Every element of the root, not every element of the document node
///
/// The walk starts at the **root** and steps *through* the document node rather than starting
/// inside it. The two are not the same population: the parser can leave a gap tile beside the
/// document node instead of within it, and `"%"` parses to exactly that — `ExecutableDocument@0..0`
/// with `Gap@0..1` as its sibling.
///
/// **Every** container of `root_kind` under the root is stepped through, not only the first. The
/// dialect's own doors build one; a caller-minted root can hold two, and this walk projects the
/// definitions of both and reports complete. That is deliberate: each container is a legitimate
/// document image, and this walk's contract is per entry — [`Recovery`] counts what had an AST
/// image and what did not, and a second container's definitions have one. Whether the root is
/// exactly one document is the fail-fast doors' question, and [`sole_document`] answers it there.
///
/// Answers the surviving definitions, their span, and the tally — or [`Unverified`],
/// which is none of those three and so is not spelled as a value of any of them.
fn recovered_top_level<'src, T>(
  parse: &Parse,
  root_kind: SyntaxKind,
  entry_of: fn(Node<'_>, &'src str) -> Out<(T, TextRange)>,
  source: &'src str,
) -> Result<(SimpleSpan, Vec<T>, Recovery), Unverified> {
  // Established once, over the whole root, before a single element is projected, and **returned**
  // rather than folded into the tally. The entries below compare no bytes, so this is the only
  // byte comparison this walk makes.
  if let Err(refusal) = verify_source::<SyntaxKind>(parse.green(), source)
    .and_then(|()| verify_root_kind::<SyntaxKind>(parse.green()))
  {
    return Err(Unverified::of(&refusal));
  }
  Ok(recovered_top_level_verified(
    parse, root_kind, entry_of, source,
  ))
}

/// [`recovered_top_level`] for a pair whose verification is already established.
///
/// Infallible: it runs no verification. Its callers are [`recovered_top_level`], after its own, and
/// the `_verified` doors, whose [`Verified`] carries one. al8n/smear#198.
fn recovered_top_level_verified<'src, T>(
  parse: &Parse,
  root_kind: SyntaxKind,
  entry_of: fn(Node<'_>, &'src str) -> Out<(T, TextRange)>,
  source: &'src str,
) -> (SimpleSpan, Vec<T>, Recovery) {
  let root = parse_root(parse);
  let container = child_node(root, root_kind).unwrap_or(root);

  let mut definitions = Vec::new();
  let mut skipped = 0u32;
  // The document's own span is the extent of the tokens under the definitions that **survived**,
  // not of the bytes that were dropped: an AST span is an extent of the tokens its node covers,
  // and a skipped region is not one of them.
  let mut extent = Extent::default();
  let mut take = |element: NodeOrToken<Node<'_>, Token<'_>>| match element {
    // Rubble the parser could not attach to a definition. Counted per token rather than per run:
    // a bound on what was lost, which is what `Recovery::skipped` promises.
    //
    // Read raw: this pass runs before any scan, so a token outside the kind space is counted as
    // rubble rather than asked a kind `kind_from_raw` would panic on.
    NodeOrToken::Token(token)
      if !SyntaxKind::from_raw(token.green().kind().0).is_some_and(is_trivia) =>
    {
      skipped = skipped.saturating_add(1)
    }
    NodeOrToken::Token(_) => {}
    NodeOrToken::Node(child) => match entry_of(child, source) {
      Ok((entry, piece)) => {
        extent.cover(piece);
        definitions.push(entry);
      }
      Err(_) => skipped = skipped.saturating_add(1),
    },
  };
  for element in root.children() {
    match element {
      // The document node is stepped *through*: its children are the definitions. Everything else
      // under the root is a top-level element in its own right — the lost-node class puts a failed
      // production's children there, and the lexer's gap tiles land there when the document node
      // came out empty.
      NodeOrToken::Node(child) if child.green().kind() == raw_of(root_kind) => {
        child.children().for_each(&mut take)
      }
      other => take(other),
    }
  }

  // With nothing projected there is no extent, and the zero-width span at the container's start
  // is the only position that is not a claim about text no node holds.
  let span = match extent.get() {
    Some(range) => to_span(range),
    None => {
      let start = usize::from(container.text_range().start());
      SimpleSpan::new(start, start)
    }
  };
  let recovery = Recovery::new(definitions.len() as u32, skipped);
  (span, definitions, recovery)
}

/// The one document container a fail-fast door reads, with the rest of the root's shape refused.
///
/// **Finding the container is not enough.** `finish_root` is public, so a caller can mint a `Parse`
/// whose root holds a valid document **and** a second container, or another node, or a bare token,
/// and the door's whole-source verification passes over all of it: that check compares bytes, not
/// shape.
///
/// So the shape is asserted rather than searched: one container of `kind`, trivia, and nothing
/// else, and a sibling or a duplicate is `UnexpectedChild { parent: Root, found }`. The recovering
/// doors do not share this assertion — see [`recovered_top_level`] for why. al8n/smear#218.
fn sole_document<'g>(root: Node<'g>, kind: SyntaxKind, wanted: &'static str) -> Out<Node<'g>> {
  // `Trivia* Container Trivia*`, transcribed like every walk below.
  let mut cursor = Cursor::new(root);
  let container = cursor.node(kind, wanted)?;
  cursor.end()?;
  Ok(container)
}

/// One top-level definition, with the holes in **its own** subtree refused.
///
/// The scan is scoped to the definition, so a hole is charged to the definition that holds it and
/// to no other.
///
/// # It compares no bytes
///
/// **Both** recovering paths establish the pair over the whole root before the first definition
/// is reached — [`recovered_top_level`] with [`verify_source`], and
/// [`project_executable_document_verified`] through the [`Verified`] it is handed.
fn recoverable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedExecutableDefinition<&'src str>, TextRange)> {
  scan_holes(node)?;
  executable_entry(node, source)
}

/// [`recoverable_entry`]'s twin at the SDL root, scoped for the same reason and comparing no bytes
/// for the same one.
fn recoverable_type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeSystemDefinitionOrExtension<&'src str>, TextRange)> {
  scan_holes(node)?;
  type_system_entry(node, source)
}

// ---------------------------------------------------------------------------------------------
// the doors' two whole-tree checks
// ---------------------------------------------------------------------------------------------

/// Open a fail-fast door: the `(tree, source)` pair verified whole, then every recovery hole
/// refused.
///
/// Both walks read the **green** tree: the byte comparison visits every node and token and compares
/// every token's bytes, and the hole scan visits every node and token. The pair is checked first,
/// so every range the door reports afterwards is a range of `source`.
fn open(root: Node<'_>, source: &str) -> Out<()> {
  verify_source(root.green(), source)?;
  // The root's identity before anything reads its children: a parse rooted anywhere but this
  // dialect's `Root` was not finished by a dialect door. al8n/smear#218.
  verify_root_kind::<SyntaxKind>(root.green())?;
  scan_holes(root)
}

/// [`open`]'s subtree form: `node`'s own text is checked where `node` sits.
///
/// The compositional doors' check. The hole scan beside it is theirs to make, scoped to the same
/// node — see [`Document::to_ast`](super::ast::Document::to_ast).
fn open_node(node: Node<'_>, source: &str) -> Out<()> {
  verify_source_at(node.green(), source, usize::from(node.start()))
}

/// Refuse a tree that carries any recovery hole or gap tile.
///
/// Scanned over the whole subtree rather than per walked node, so the answer does not depend on
/// which nodes a particular walk happens to descend into: a hole anywhere is a region of the
/// document with no AST image, and a projection that silently omitted it would be losing data
/// under a success type. At a fail-fast door the subtree is the **root's**, which is what reaches
/// a hole the parser left beside the document node rather than inside it — see
/// [`reject_holes`](crate::lossless::project::reject_holes).
fn scan_holes(node: Node<'_>) -> Out<()> {
  // `Gap` is a **token** kind, and this arm used to be dead for exactly that reason: `reject_holes`
  // tested node kinds only, so every gap tile in a scanned subtree was walked past and the
  // projection folded it into an enclosing extent as an ordinary non-trivia token. The arm was kept
  // as a statement of the refusal's scope; al8n/smear#58 made the substrate's scan token-aware, so
  // the statement is now a live branch and this door refuses a gap where it sits.
  // The kind check rides on the same pass: this scan is the first code in every door to ask an
  // element its kind, so it is where a raw kind outside this space is refused rather than handed
  // to `kind_from_raw`, which can only panic. al8n/smear#218.
  reject_foreign_kinds_and_holes(node, |kind| matches!(kind, K::Error | K::Gap))
}

/// A parse's green root, as the walk's first node.
fn parse_root(parse: &Parse) -> Node<'_> {
  Node::new(parse.green(), TextSize::new(0))
}

/// The first direct child of `parent` whose kind is `kind`.
///
/// Compared **raw**, because its one caller runs before the per-entry scan that checks kinds: a
/// sibling outside the kind space is simply not the one wanted.
fn child_node(parent: Node<'_>, kind: SyntaxKind) -> Option<Node<'_>> {
  parent.children().find_map(|child| match child {
    NodeOrToken::Node(child) if child.green().kind() == raw_of(kind) => Some(child),
    _ => None,
  })
}

/// `kind` as the green tree stores it.
fn raw_of(kind: SyntaxKind) -> rowan::SyntaxKind {
  <GraphQLLang as rowan::Language>::kind_to_raw(kind)
}

// ---------------------------------------------------------------------------------------------
// the substrate this dialect binds
// ---------------------------------------------------------------------------------------------

/// The six ignorable token images. The tree keeps them; no AST span may contain one.
const fn is_trivia(kind: SyntaxKind) -> bool {
  matches!(
    kind,
    K::Space | K::Tab | K::Newline | K::Comma | K::Comment | K::Bom
  )
}

impl Trivia for GraphQLLang {
  #[inline]
  fn is_trivia(kind: SyntaxKind) -> bool {
    is_trivia(kind)
  }
}

/// The two token images a string — a description's or a value's — can be written as.
const STRING_KINDS: [SyntaxKind; 2] = [K::String, K::BlockString];

/// This dialect's half of the cursor: the atoms that need its lexer or its keyword table.
///
/// The substrate's cursor owns every atom that takes a kind as a parameter; what is left here is
/// what reading a `Name` means in this dialect — a keyword by its spelling, a name through
/// [`identifier`], a spelling a caller classifies. A trait rather than free functions so a walk
/// reads as one sequence of calls on one cursor, and a trait rather than inherent methods because
/// the cursor is the substrate's type.
///
/// **There is no `token(K::Name)`**, and that is the point. The slot walks this file had until
/// al8n/smear#218 collected every direct `Name` into a three-slot `Names` and read positions out of
/// it — so a fourth name was dropped with its bytes covered, a fragment name split into `Name("o")`
/// and `Name("n")` passed the `on` rule, and `directive @d foo FIELD` answered what
/// `directive @d on FIELD` answers. A `Name` is consumed here by what it is at its position.
trait Atoms<'g> {
  fn keyword(&mut self, keyword: ContextualKeyword, wanted: &'static str) -> Out<Token<'g>>;
  fn opt_keyword(&mut self, keyword: ContextualKeyword) -> Option<Token<'g>>;
  fn name_token<'src>(&mut self, source: &'src str, wanted: &'static str) -> Out<Name<&'src str>>;
  fn name_except<'src>(
    &mut self,
    source: &'src str,
    reserved: &[ContextualKeyword],
    rule: &'static str,
    wanted: &'static str,
  ) -> Out<Name<&'src str>>;
  fn opt_name<'src>(&mut self, source: &'src str) -> Out<Option<Name<&'src str>>>;
  fn spelling(&mut self, wanted: &'static str) -> Out<Token<'g>>;
  fn opt_spelling(&mut self) -> Option<Token<'g>>;
}

impl<'g> Atoms<'g> for Cursor<'g> {
  /// A `Name` token spelling `keyword`, through the lexer's own table.
  fn keyword(&mut self, keyword: ContextualKeyword, wanted: &'static str) -> Out<Token<'g>> {
    match self.peek() {
      Some(NodeOrToken::Token(token))
        if token.kind() == K::Name && keyword_of(token) == Some(keyword) =>
      {
        self.bump();
        self.extent.cover(token.text_range());
        Ok(token)
      }
      Some(element) => Err(self.unexpected(element)),
      None => Err(self.missing(wanted)),
    }
  }

  /// [`keyword`](Self::keyword) where the production makes it optional.
  fn opt_keyword(&mut self, keyword: ContextualKeyword) -> Option<Token<'g>> {
    match self.peek() {
      Some(NodeOrToken::Token(token))
        if token.kind() == K::Name && keyword_of(token) == Some(keyword) =>
      {
        self.bump();
        self.extent.cover(token.text_range());
        Some(token)
      }
      _ => None,
    }
  }

  /// A `Name` token in a position the grammar spells as a **name**, re-cooked into one.
  ///
  /// The one door every name position goes through: the slice is handed to [`identifier`] — the
  /// shipped scanner, which must read the whole slice as exactly one identifier — so a `Name`
  /// label over bytes the lexer would not produce a name from is
  /// [`MalformedToken`](ProjectErrorKind::MalformedToken) here, at a name position. The other way a
  /// `Name` reaches that refusal is a spelling position whose classifier does not know the word —
  /// an operation keyword, a directive location — which a parse can reach. See [`name`].
  fn name_token<'src>(&mut self, source: &'src str, wanted: &'static str) -> Out<Name<&'src str>> {
    self.name_except(source, &[], "", wanted)
  }

  /// [`name_token`](Self::name_token) at a position the grammar makes a **rule** of: a spelling in
  /// `reserved` is [`SemanticRule`](ProjectErrorKind::SemanticRule) naming `rule`.
  ///
  /// The positions are derived from the syntactic parser's own refusals — see the module header's
  /// table. Everywhere else this dialect's keywords are contextual and every one of them is a
  /// name: the lexer reads `on`, `true` and `type` as identifiers, and the syntactic parser accepts
  /// `type on { on: Int }`.
  fn name_except<'src>(
    &mut self,
    source: &'src str,
    reserved: &[ContextualKeyword],
    rule: &'static str,
    wanted: &'static str,
  ) -> Out<Name<&'src str>> {
    match self.peek() {
      Some(NodeOrToken::Token(token)) if token.kind() == K::Name => {
        self.bump();
        self.extent.cover(token.text_range());
        if keyword_of(token).is_some_and(|keyword| reserved.contains(&keyword)) {
          return Err(ProjectError::new(
            ProjectErrorKind::SemanticRule { rule },
            to_range(token.text_range()),
          ));
        }
        name(source, token)
      }
      Some(element) => Err(self.unexpected(element)),
      None => Err(self.missing(wanted)),
    }
  }

  /// [`name_token`](Self::name_token) where the production makes the name optional.
  fn opt_name<'src>(&mut self, source: &'src str) -> Out<Option<Name<&'src str>>> {
    match self.peek() {
      Some(NodeOrToken::Token(token)) if token.kind() == K::Name => {
        self.name_token(source, "a name").map(Some)
      }
      _ => Ok(None),
    }
  }

  /// A `Name` token read for its **spelling** — an operation keyword, a directive location,
  /// `true`, `false`, `null`. The caller classifies it and refuses what it does not classify; the
  /// atom only guarantees that it is one `Name` and not a node or a punctuation token.
  fn spelling(&mut self, wanted: &'static str) -> Out<Token<'g>> {
    match self.peek() {
      Some(NodeOrToken::Token(token)) if token.kind() == K::Name => {
        self.bump();
        self.extent.cover(token.text_range());
        Ok(token)
      }
      Some(element) => Err(self.unexpected(element)),
      None => Err(self.missing(wanted)),
    }
  }

  /// [`spelling`](Self::spelling) where one may be absent.
  fn opt_spelling(&mut self) -> Option<Token<'g>> {
    match self.peek() {
      Some(NodeOrToken::Token(token)) if token.kind() == K::Name => {
        self.bump();
        self.extent.cover(token.text_range());
        Some(token)
      }
      _ => None,
    }
  }
}

// ---------------------------------------------------------------------------------------------
// leaves: the lexer's own doors
// ---------------------------------------------------------------------------------------------

/// The source text under `token`.
///
/// Bounds-checked, not compared: the door already verified every byte of the tree against
/// `source`, so a token's range is in bounds and on a character boundary by construction. The
/// refusal below is that invariant's receipt rather than a second check — see
/// [`verify_source`].
#[inline]
fn slice<'src>(source: &'src str, token: Token<'_>) -> Out<&'src str> {
  let range = token.text_range();
  source
    .get(usize::from(range.start())..usize::from(range.end()))
    .ok_or_else(|| ProjectError::new(ProjectErrorKind::SourceMismatch, to_range(range)))
}

/// The refusal a leaf door answers: the tree labelled bytes the scanner will not read back.
fn malformed(token: Token<'_>) -> ProjectError {
  ProjectError::new(
    ProjectErrorKind::MalformedToken { kind: token.kind() },
    to_range(token.text_range()),
  )
}

/// Re-cook a name through the **same** door the lexer's identifiers come from.
///
/// [`identifier`] is the shipped scanner, and it must answer the whole slice. The range comes from
/// a token the *tree* labelled `Name`, so on a caller-minted tree the bytes are whatever the caller
/// wrote, and `Name("1")` is a value the syntactic parser has no way to produce. al8n/smear#218's
/// leaf-trust addendum.
fn name<'src>(source: &'src str, token: Token<'_>) -> Out<Name<&'src str>> {
  let text = slice(source, token)?;
  identifier(text).map_err(|_| malformed(token))?;
  Ok(Name::new(to_span(token.text_range()), text))
}

/// The keyword a `Name` token spells, classified through the lexer's own table.
fn keyword_of(token: Token<'_>) -> Option<ContextualKeyword> {
  contextual_keyword(token.text().as_bytes())
}

/// Re-cook a string literal through the **same** door the lexer's payload comes from.
///
/// [`LitStr`]'s `TryFrom<&str>` is the string lexer, so the `Plain`/`Complex` discriminant and
/// the `required_capacity` a consumer allocates against are the lexer's answers rather than a
/// second implementation of the escape rules. A refusal here means the token's text is not a
/// string literal to that lexer — a caller-minted label, since a parse's string tokens come from
/// it.
fn string_value<'src>(token: Token<'_>, source: &'src str) -> Out<StringValue<&'src str>> {
  let slice = slice(source, token)?;
  let lit = LitStr::try_from(slice).map_err(|_| malformed(token))?;
  Ok(StringValue::new(to_span(token.text_range()), lit))
}

/// An integer literal's text, checked by the scanner that produced it.
///
/// This dialect's AST stores the **raw slice** rather than a classified literal, so there is no
/// radix to get wrong — but `IntValue("abc")` was producible all the same: the old walk sliced
/// whatever a token labelled `Int` covered. [`int_literal`] is the shipped scanner over the whole
/// slice, and it hands the slice back. al8n/smear#218's leaf-trust addendum.
fn int_text<'src>(source: &'src str, token: Token<'_>) -> Out<&'src str> {
  int_literal(slice(source, token)?).map_err(|_| malformed(token))
}

/// [`int_text`]'s twin for a float literal. The two doors do not coerce into each other, so an
/// `Int`-labelled `1.5` and a `Float`-labelled `1` both refuse.
fn float_text<'src>(source: &'src str, token: Token<'_>) -> Out<&'src str> {
  float_literal(slice(source, token)?).map_err(|_| malformed(token))
}

// ---------------------------------------------------------------------------------------------
// documents
// ---------------------------------------------------------------------------------------------

/// The seven extension kinds, each its own transcription.
const EXTENSION_KINDS: [SyntaxKind; 7] = [
  K::ScalarTypeExtension,
  K::ObjectTypeExtension,
  K::InterfaceTypeExtension,
  K::UnionTypeExtension,
  K::EnumTypeExtension,
  K::InputObjectTypeExtension,
  K::SchemaExtension,
];

/// The eight type-system definition kinds.
const TYPE_SYSTEM_DEFINITION_KINDS: [SyntaxKind; 8] = [
  K::ScalarTypeDefinition,
  K::ObjectTypeDefinition,
  K::InterfaceTypeDefinition,
  K::UnionTypeDefinition,
  K::EnumTypeDefinition,
  K::InputObjectTypeDefinition,
  K::DirectiveDefinition,
  K::SchemaDefinition,
];

/// What the mixed root's definition run holds: either executable definition, a type-system
/// definition or an extension.
const MIXED_ENTRY_KINDS: [SyntaxKind; 17] = entry_kinds::<17>(true, true);

/// What the SDL root's definition run holds — the mixed run without the two executable kinds.
const TYPE_SYSTEM_ENTRY_KINDS: [SyntaxKind; 15] = entry_kinds::<15>(false, true);

/// What the executable root's definition run holds.
const EXECUTABLE_ENTRY_KINDS: [SyntaxKind; 2] = entry_kinds::<2>(true, false);

/// One entry-kind row, assembled from the lists above so the three roots cannot disagree about a
/// kind they share. `N` is checked by the assembly itself: a count that does not match the chosen
/// halves fails const evaluation.
const fn entry_kinds<const N: usize>(executable: bool, type_system: bool) -> [SyntaxKind; N] {
  let mut kinds = [K::OperationDefinition; N];
  let mut at = 0;
  if executable {
    kinds[at] = K::OperationDefinition;
    kinds[at + 1] = K::FragmentDefinition;
    at += 2;
  }
  if type_system {
    let mut i = 0;
    while i < TYPE_SYSTEM_DEFINITION_KINDS.len() {
      kinds[at] = TYPE_SYSTEM_DEFINITION_KINDS[i];
      at += 1;
      i += 1;
    }
    let mut i = 0;
    while i < EXTENSION_KINDS.len() {
      kinds[at] = EXTENSION_KINDS[i];
      at += 1;
      i += 1;
    }
  }
  assert!(
    at == N,
    "an entry-kind row's length is the sum of its halves"
  );
  kinds
}

/// The three selection kinds a selection set holds.
const SELECTION_KINDS: [SyntaxKind; 3] = [K::Field, K::FragmentSpread, K::InlineFragment];

fn document<'src>(node: Node<'_>, source: &'src str) -> Out<Document<&'src str>> {
  // `Definition+`. A token here is rubble — the lost-node recovery class drops a failed
  // definition's bytes straight under the document — so the run stops at it and `end` refuses it
  // where it stands.
  let mut cursor = Cursor::new(node);
  let entries = cursor.many1(&MIXED_ENTRY_KINDS, None, "a definition")?;
  cursor.end()?;
  let mut definitions = Vec::with_capacity(entries.len());
  for child in entries {
    definitions.push(cursor.keep(document_entry(child, source)?));
  }
  Ok(Document::new(
    to_span(cursor.range("a token")?),
    definitions,
  ))
}

fn document_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DefinitionOrExtension<&'src str>, TextRange)> {
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((DefinitionOrExtension::Extension(extension), extent));
  }
  let (description, definition, outer) = definition(node, source)?;
  Ok((
    DefinitionOrExtension::Definition(Described::new(to_span(outer), description, definition)),
    outer,
  ))
}

/// [`document`]'s SDL-only twin, over the `TypeSystemDefinitionOrExtension+` root.
fn type_system_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<TypeSystemDocument<&'src str>> {
  // `TypeSystemDefinitionOrExtension+` — rubble exactly as at the mixed root, and an executable
  // definition is not in this root's run at all, so `end` refuses it at its own range.
  let mut cursor = Cursor::new(node);
  let entries = cursor.many1(&TYPE_SYSTEM_ENTRY_KINDS, None, "a definition")?;
  cursor.end()?;
  let mut definitions = Vec::with_capacity(entries.len());
  for child in entries {
    definitions.push(cursor.keep(type_system_entry(child, source)?));
  }
  Ok(TypeSystemDocument::new(
    to_span(cursor.range("a token")?),
    definitions,
  ))
}

/// [`document_entry`]'s SDL-only twin.
///
/// The extension arm stays — `extend` is type-system syntax and this root is the one that builds
/// it — and what goes is the executable half: an `OperationDefinition` or a `FragmentDefinition`
/// under this root has no image in a `TypeSystemDocument`, and the SDL root reports one at the
/// parser's own position rather than shaping it, so reaching that arm means the tree is not the
/// one this door was handed.
fn type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeSystemDefinitionOrExtension<&'src str>, TextRange)> {
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((
      TypeSystemDefinitionOrExtension::Extension(extension),
      extent,
    ));
  }
  let (description, definition, outer) = type_system_definition(node, source)?;
  Ok((
    TypeSystemDefinitionOrExtension::Definition(Described::new(
      to_span(outer),
      description,
      definition,
    )),
    outer,
  ))
}

/// [`document`]'s executable-only twin, over the `ExecutableDefinition+` root.
fn executable_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  // `ExecutableDefinition+` — the SDL root's mirror.
  let mut cursor = Cursor::new(node);
  let entries = cursor.many1(&EXECUTABLE_ENTRY_KINDS, None, "a definition")?;
  cursor.end()?;
  let mut definitions = Vec::with_capacity(entries.len());
  for child in entries {
    definitions.push(cursor.keep(executable_entry(child, source)?));
  }
  Ok(ExecutableDocument::new(
    to_span(cursor.range("a token")?),
    definitions,
  ))
}

/// [`document_entry`]'s executable-only twin.
///
/// No extension arm — `extend` is not executable syntax and the root that produced this node
/// reports one rather than building it — and the description hoist is the document-level one: the
/// wrapper spans description-through-definition and the inner node starts after the description.
fn executable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedExecutableDefinition<&'src str>, TextRange)> {
  let (description, definition, outer) = executable_definition(node, source)?;
  Ok((
    Described::new(to_span(outer), description, definition),
    outer,
  ))
}

/// A described definition's three answers, folded from the node's single walk.
///
/// The hoisted description, the definition itself — whose own span is the inner extent, already
/// built into it — and the node's full token extent, which is both the `Described` wrapper's span
/// and what the enclosing document covers.
type Definition<'src, T> = (Option<StringValue<&'src str>>, T, TextRange);

fn executable_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ExecutableDefinition<&'src str>>> {
  Ok(match node.kind() {
    K::OperationDefinition => {
      let (description, operation, outer) = operation_definition(node, source)?;
      (
        description,
        ExecutableDefinition::Operation(operation),
        outer,
      )
    }
    K::FragmentDefinition => {
      let (description, fragment, outer) = fragment_definition(node, source)?;
      (description, ExecutableDefinition::Fragment(fragment), outer)
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

fn definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::Definition<&'src str>>> {
  use crate::graphql::ast::Definition as D;

  // The two executable kinds, then the eight type-system ones through the shared arm. One list of
  // the eight, not two: `type_system_definition` is what the SDL root reaches them by, and a
  // second copy here would be eight chances for the mixed root and the SDL root to build different
  // ASTs out of the same node.
  Ok(match node.kind() {
    K::OperationDefinition => {
      let (description, operation, outer) = operation_definition(node, source)?;
      (
        description,
        D::Executable(ExecutableDefinition::Operation(operation)),
        outer,
      )
    }
    K::FragmentDefinition => {
      let (description, fragment, outer) = fragment_definition(node, source)?;
      (
        description,
        D::Executable(ExecutableDefinition::Fragment(fragment)),
        outer,
      )
    }
    _ => {
      let (description, definition, outer) = type_system_definition(node, source)?;
      (description, D::TypeSystem(definition), outer)
    }
  })
}

/// The eight type-system definition kinds, shared by the mixed root and the SDL-only one.
fn type_system_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, TypeSystemDefinition<&'src str>>> {
  Ok(match node.kind() {
    K::ScalarTypeDefinition => {
      let (description, definition, outer) = scalar_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Scalar(definition)),
        outer,
      )
    }
    K::ObjectTypeDefinition => {
      let (description, definition, outer) = object_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Object(definition)),
        outer,
      )
    }
    K::InterfaceTypeDefinition => {
      let (description, definition, outer) = interface_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Interface(definition)),
        outer,
      )
    }
    K::UnionTypeDefinition => {
      let (description, definition, outer) = union_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Union(definition)),
        outer,
      )
    }
    K::EnumTypeDefinition => {
      let (description, definition, outer) = enum_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::Enum(definition)),
        outer,
      )
    }
    K::InputObjectTypeDefinition => {
      let (description, definition, outer) = input_object_type_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Type(TypeDefinition::InputObject(definition)),
        outer,
      )
    }
    K::DirectiveDefinition => {
      let (description, definition, outer) = directive_definition(node, source)?;
      (
        description,
        TypeSystemDefinition::Directive(definition),
        outer,
      )
    }
    K::SchemaDefinition => {
      let (description, definition, outer) = schema_definition(node, source)?;
      (description, TypeSystemDefinition::Schema(definition), outer)
    }
    found => return Err(unexpected(node, found, node.text_range())),
  })
}

/// `Some` when `node` is one of the seven extension kinds, `None` when it is anything else.
///
/// An extension carries no description. The lossless production reports a string written in front
/// of an `extend` and — unlike GraphQLx, where the string stays outside — builds the extension
/// node **around** it, because the mark it opens at was taken before the description. So the
/// description is inside the node here and each extension's walk refuses it as the first element
/// its sequence has no place for.
fn type_system_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Option<(TypeSystemExtension<&'src str>, TextRange)>> {
  Ok(Some(match node.kind() {
    K::ScalarTypeExtension => {
      let (extension, extent) = scalar_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Scalar(extension)),
        extent,
      )
    }
    K::ObjectTypeExtension => {
      let (extension, extent) = object_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Object(extension)),
        extent,
      )
    }
    K::InterfaceTypeExtension => {
      let (extension, extent) = interface_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Interface(extension)),
        extent,
      )
    }
    K::UnionTypeExtension => {
      let (extension, extent) = union_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Union(extension)),
        extent,
      )
    }
    K::EnumTypeExtension => {
      let (extension, extent) = enum_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::Enum(extension)),
        extent,
      )
    }
    K::InputObjectTypeExtension => {
      let (extension, extent) = input_object_type_extension(node, source)?;
      (
        TypeSystemExtension::Type(TypeExtension::InputObject(extension)),
        extent,
      )
    }
    K::SchemaExtension => {
      let (extension, extent) = schema_extension(node, source)?;
      (TypeSystemExtension::Schema(extension), extent)
    }
    _ => return Ok(None),
  }))
}

// ---------------------------------------------------------------------------------------------
// descriptions
// ---------------------------------------------------------------------------------------------

/// The string a `Description` node holds, and the node's extent.
///
/// The extent is the parent's business rather than this node's: a description has no AST node of
/// its own — it is hoisted into a [`Described`] wrapper — so what the parent needs back is where
/// the description sat, which is the difference between the wrapper's span and the definition's.
fn description<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(StringValue<&'src str>, TextRange)> {
  // `String | BlockString` — one token, nothing else.
  let mut cursor = Cursor::new(node);
  let token = cursor.token_of(&STRING_KINDS, "a string token")?;
  let extent = cursor.finish("a token")?;
  Ok((string_value(token, source)?, extent))
}

/// Project the `Description` a node's walk collected, if it collected one.
///
/// Answers the hoisted string and its extent side by side, because [`described_extents`] needs the
/// second to tell the wrapper's span from the definition's.
fn hoisted_description<'src>(
  node: Option<Node<'_>>,
  source: &'src str,
) -> Out<(Option<StringValue<&'src str>>, Option<TextRange>)> {
  match node {
    Some(node) => {
      let (value, extent) = description(node, source)?;
      Ok((Some(value), Some(extent)))
    }
    None => Ok((None, None)),
  }
}

// ---------------------------------------------------------------------------------------------
// executable definitions
// ---------------------------------------------------------------------------------------------

fn operation_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, OperationDefinition<&'src str>>> {
  // `Description? OperationType Name? VariablesDefinition? Directives? SelectionSet`
  //  | `Description? SelectionSet`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  let Some(type_node) = cursor.opt_node(K::OperationType) else {
    // Query shorthand: the definition *is* its selection set, and the AST's span for it is the
    // selection set's own. The grammar gives a shorthand no other constituent, so anything else
    // here is not in the sequence and `end` refuses it.
    //
    // **The description is one the parser itself produces.** `"docs" { id }` is reported by
    // `document.rs`'s `definition` and the operation is still built *around* the description,
    // because the mark the node opens at was taken before it — so a parser-built pair reaches here
    // with one, and the walk this replaces kept it: a `Described` shorthand, a value the syntactic
    // parser produces for no input, and a recovering projection that counted the entry complete.
    // `UnexpectedChild` and not `SemanticRule`: the syntactic side refuses it with an expectation
    // at the `{` rather than by naming a rule. al8n/smear#218's round-four addendum.
    if let Some(description) = description_node {
      return Err(unexpected_node(node, description));
    }
    let set = cursor.node(K::SelectionSet, "a selection set")?;
    cursor.end()?;
    let selections = cursor.keep(selection_set(set, source)?);
    let (outer, _) = described_extents(node, cursor.extent, None)?;
    return Ok((None, OperationDefinition::Shorthand(selections), outer));
  };
  let name = cursor.opt_name(source)?;
  let variables_node = cursor.opt_node(K::VariablesDefinition);
  let directives_node = cursor.opt_node(K::Directives);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let operation_type = cursor.keep(operation_type(type_node)?);
  let variables = cursor.keep_opt(variables_definition(variables_node, source)?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let selections = cursor.keep(selection_set(set, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    OperationDefinition::Named(NamedOperationDefinition::new(
      to_span(inner),
      operation_type,
      name,
      variables,
      directives,
      selections,
    )),
    outer,
  ))
}

/// An `OperationType` node: one `Name` read for its spelling, which must be one of the three
/// operation keywords.
fn operation_type(node: Node<'_>) -> Out<(OperationType, TextRange)> {
  // `query | mutation | subscription`
  let mut cursor = Cursor::new(node);
  let token = cursor.spelling("an operation keyword")?;
  let extent = cursor.finish("a token")?;
  let span = to_span(token.text_range());
  let operation_type = match keyword_of(token) {
    Some(ContextualKeyword::Query) => OperationType::Query(Query::new(span)),
    Some(ContextualKeyword::Mutation) => OperationType::Mutation(Mutation::new(span)),
    Some(ContextualKeyword::Subscription) => OperationType::Subscription(Subscription::new(span)),
    _ => return Err(malformed(token)),
  };
  Ok((operation_type, extent))
}

/// An operation's variable definitions, `( VariableDefinition+ )`.
///
/// **A `+` container, not an `()` one**, and the pair is worth stating side by side: an argument
/// list has no `at_least(1)`, so `f()` is a written-down empty list the syntactic parser maps to
/// `None`; a variables definition has one, so `query Q() { f }` is a document the syntactic parser
/// **rejects**. See the module header's three-way rule.
fn variables_definition<'src>(
  list: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(VariablesDefinition<&'src str>, TextRange)>> {
  let Some(list) = list else {
    return Ok(None);
  };
  // `( VariableDefinition+ )`
  let mut cursor = Cursor::new(list);
  cursor.token(K::LParen, "the `(` a variables list opens with")?;
  let listed = cursor.many1(
    &[K::VariableDefinition],
    Some(K::RParen),
    "a variable definition",
  )?;
  // Lenient: no AST image, and `unclosed_parens` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RParen);
  cursor.end()?;
  let mut definitions = Vec::with_capacity(listed.len());
  for child in listed {
    definitions.push(cursor.keep(variable_definition(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok(Some((
    VariablesDefinition::new(to_span(extent), definitions),
    extent,
  )))
}

fn variable_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DescribedVariableDefinition<&'src str>, TextRange)> {
  // `Description? Variable : Type DefaultValue? Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  let variable_node = cursor.node(K::Variable, "a variable")?;
  cursor.token(K::Colon, "the `:` before a variable's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let default_node = cursor.opt_node(K::DefaultValue);
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let variable = cursor.keep(variable_value(variable_node, source)?);
  let ty = cursor.keep(ty(type_node, source)?);
  let default_value = cursor.keep_opt(optional_default_value(default_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  // The one described node below document level whose inner span excludes the description — see
  // the module header for the three that do not.
  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    Described::new(
      to_span(outer),
      description,
      VariableDefinition::new(to_span(inner), variable, ty, default_value, directives),
    ),
    outer,
  ))
}

fn fragment_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::FragmentDefinition<&'src str>>> {
  // `Description? fragment FragmentName on? NamedType Directives? SelectionSet`, and
  // `FragmentName : Name but not on`. The lossless production reports the violation on the
  // diagnostic channel and still builds the node, so the shape alone cannot tell a legal fragment
  // name from an illegal one and this is the rule's custodian beside the two parsers'.
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Fragment, "the `fragment` keyword")?;
  let name = cursor.name_except(
    source,
    &[ContextualKeyword::On],
    "a fragment may not be named `on`",
    "a fragment name",
  )?;
  // Lenient: no AST image of its own — the condition stores only the type's name — and the
  // production reports a missing `on` and still builds the definition, hole-free, around the type
  // it did find. The type is required either way.
  let on = cursor.opt_keyword(ContextualKeyword::On);
  let condition_node = cursor.node(K::NamedType, "a type condition")?;
  let directives_node = cursor.opt_node(K::Directives);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let type_condition = cursor.keep(type_condition(on, condition_node, source)?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let selections = cursor.keep(selection_set(set, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    crate::graphql::ast::FragmentDefinition::new(
      to_span(inner),
      FragmentName::from_name(name),
      type_condition,
      directives,
      selections,
    ),
    outer,
  ))
}

/// A type condition, `on NamedType` — **not a node in this kind space**: the keyword is a token of
/// the fragment that carries it and the type is that fragment's `NamedType` child, so the two
/// arrive here separately.
///
/// The span runs from the `on` to the end of the type. It used to be built with
/// `SimpleSpan::new(on.start, name.end)` from an `on` found by counting `Name` tokens and a type
/// found by kind, and a caller-built tree with the type before the keyword made that constructor
/// panic. Both callers now consume the `on` before the type in their own sequence, so the order is
/// the cursor's rather than a claim, and `cover` could not invert even if it were not.
/// al8n/smear#218, finding 4.
///
/// Without an `on` — the lenient fragment-definition case — the span is the type's own, which is
/// the composite-span convention the module header states.
fn type_condition<'src>(
  on: Option<Token<'_>>,
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeCondition<&'src str>, TextRange)> {
  let (name, name_range) = named_type_name(node, source)?;
  let range = match on {
    Some(on) => on.text_range().cover(name_range),
    None => name_range,
  };
  Ok((TypeCondition::new(to_span(range), name), range))
}

// ---------------------------------------------------------------------------------------------
// selections
// ---------------------------------------------------------------------------------------------

/// A selection whose own selection set is being built below.
///
/// Everything a `Field` or an `InlineFragment` needs *except* its selection set is bounded work —
/// its alias, its name, its arguments, its directives, its type condition — so it is all folded
/// before the descent and travels here. A `FragmentSpread` never appears: it holds no selection set
/// and is finished where it is read.
enum OpenSelection<'g, 'src> {
  Field {
    /// The `Field` node, and the owner of the finished selection's span.
    node: Node<'g>,
    /// The field's fold, everything but its selection set.
    extent: Extent,
    alias: Option<Alias<&'src str>>,
    name: Name<&'src str>,
    arguments: Option<Arguments<&'src str>>,
    directives: Option<Directives<&'src str>>,
    /// The `SelectionSet` node this field is waiting on.
    pending: Node<'g>,
  },
  InlineFragment {
    /// The `InlineFragment` node, and the owner of the finished selection's span.
    node: Node<'g>,
    /// The fragment's fold, everything but its selection set.
    extent: Extent,
    type_condition: Option<TypeCondition<&'src str>>,
    directives: Option<Directives<&'src str>>,
    /// The `SelectionSet` node this fragment is waiting on.
    pending: Node<'g>,
  },
}

impl<'g> OpenSelection<'g, '_> {
  /// The `SelectionSet` node this selection is waiting on.
  const fn set(&self) -> Node<'g> {
    match self {
      Self::Field { pending, .. } | Self::InlineFragment { pending, .. } => *pending,
    }
  }
}

/// A `SelectionSet` suspended while the set of one of its selections is built below.
///
/// `{ a { a … } }` nests without bound at the lexer's own ceiling, so the walk that reads it is a
/// loop over these rather than a native frame per level — see the module header's *No node
/// dispatch below spends a native frame per level*.
struct SelectionFrame<'g, 'src> {
  /// The `SelectionSet`'s cursor: its node, the fold over its own tokens and the selections
  /// already finished, and the selections not yet read.
  cursor: Cursor<'g>,
  /// The selections already finished, in document order.
  selections: Vec<Selection<&'src str>>,
  /// The selection whose own set is being built below.
  open: OpenSelection<'g, 'src>,
}

/// What a [`SelectionFrame`] did when the set below it finished.
///
/// The frame travels by value: an open selection holds a finished `Alias`, `Arguments` and
/// `Directives` that have to leave the frame when it closes, and a frame that descends again is
/// pushed back by the resume itself.
enum ResumedSet<'g, 'src> {
  /// The next nested selection set that has to be built; the frame is back on the worklist.
  Descend(Node<'g>),
  /// This frame is finished, and what it finished to.
  Done(SelectionSet<&'src str>, TextRange),
}

/// Read selections until one is waiting on a set of its own.
///
/// Everything that finishes where it is read — a fragment spread, a field with no set — is folded
/// into `selections` here, so only a selection that actually nests ever occupies the worklist.
/// `None` means the set's selections have run out.
fn next_selection<'g, 'src>(
  cursor: &mut Cursor<'g>,
  selections: &mut Vec<Selection<&'src str>>,
  source: &'src str,
) -> Out<Option<OpenSelection<'g, 'src>>> {
  while let Some(child) = cursor.opt_one_of(&SELECTION_KINDS) {
    match child.kind() {
      K::Field => match open_field(child, source)? {
        Ok(open) => return Ok(Some(open)),
        Err(finished) => selections.push(Selection::Field(cursor.keep(finished))),
      },
      K::FragmentSpread => {
        selections.push(Selection::FragmentSpread(
          cursor.keep(fragment_spread(child, source)?),
        ));
      }
      _ => return Ok(Some(open_inline_fragment(child, source)?)),
    }
  }
  Ok(None)
}

/// Open selection sets from `node` down to the first one that nests no further, and answer that
/// one.
///
/// Every set on the way is suspended on `frames` with the selection it must build first already
/// chosen, so a frame is never on the stack without a live descent below it.
fn open_selection_chain<'g, 'src>(
  frames: &mut Vec<SelectionFrame<'g, 'src>>,
  node: Node<'g>,
  source: &'src str,
) -> Out<(SelectionSet<&'src str>, TextRange)> {
  let mut node = node;
  loop {
    // `{ Selection+ }`, with the `}` lenient.
    let mut cursor = Cursor::new(node);
    cursor.token(K::LBrace, "the `{` a selection set opens with")?;
    let mut selections = Vec::new();
    match next_selection(&mut cursor, &mut selections, source)? {
      Some(open) => {
        let nested = open.set();
        frames.push(SelectionFrame {
          cursor,
          selections,
          open,
        });
        node = nested;
      }
      None => return close_selection_set(cursor, selections),
    }
  }
}

/// Close a selection set whose selections have run out: `Selection+`, so an empty one is refused
/// through the cursor's `absent`, and the `}` is **lenient** — see the module header's
/// missing-token table.
fn close_selection_set<'src>(
  mut cursor: Cursor<'_>,
  selections: Vec<Selection<&'src str>>,
) -> Out<(SelectionSet<&'src str>, TextRange)> {
  if selections.is_empty() {
    return Err(cursor.absent(Some(K::RBrace), "a selection"));
  }
  cursor.opt_token(K::RBrace);
  let extent = cursor.finish("a token")?;
  Ok((
    SelectionSet::new(to_span(extent), selections.into()),
    extent,
  ))
}

/// Fold the set the level below finished into the selection that was waiting on it, and read on to
/// the next selection that nests.
fn resume_selection<'g, 'src>(
  frames: &mut Vec<SelectionFrame<'g, 'src>>,
  frame: SelectionFrame<'g, 'src>,
  set: SelectionSet<&'src str>,
  piece: TextRange,
  source: &'src str,
) -> Out<ResumedSet<'g, 'src>> {
  let SelectionFrame {
    mut cursor,
    mut selections,
    open,
  } = frame;
  let (selection, range) = close_selection(open, set, piece)?;
  cursor.extent.cover(range);
  selections.push(selection);
  Ok(
    match next_selection(&mut cursor, &mut selections, source)? {
      Some(next) => {
        let nested = next.set();
        frames.push(SelectionFrame {
          cursor,
          selections,
          open: next,
        });
        ResumedSet::Descend(nested)
      }
      None => {
        let (set, range) = close_selection_set(cursor, selections)?;
        ResumedSet::Done(set, range)
      }
    },
  )
}

/// A selection set, with the nesting inside it read on a worklist rather than a stack.
fn selection_set<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(SelectionSet<&'src str>, TextRange)> {
  let mut frames: Vec<SelectionFrame<'_, 'src>> = Vec::new();
  let mut built = open_selection_chain(&mut frames, node, source)?;
  loop {
    let Some(frame) = frames.pop() else {
      return Ok(built);
    };
    let (set, piece) = built;
    built = match resume_selection(&mut frames, frame, set, piece, source)? {
      ResumedSet::Descend(nested) => open_selection_chain(&mut frames, nested, source)?,
      ResumedSet::Done(set, range) => (set, range),
    };
  }
}

/// A field read as far as its selection set.
///
/// `Ok` is a field that nests and is now on the worklist; `Err` is one that does not and is
/// finished — the `Result` is a two-way answer here rather than a refusal, and the refusals this
/// makes are the `?`s above it.
#[allow(clippy::type_complexity)]
fn open_field<'g, 'src>(
  node: Node<'g>,
  source: &'src str,
) -> Out<Result<OpenSelection<'g, 'src>, (Field<&'src str>, TextRange)>> {
  // `Alias? Name Arguments? Directives? SelectionSet?` — the alias is a node holding its own `Name`
  // and `:`, so the field's name is the one `Name` this node holds directly.
  let mut cursor = Cursor::new(node);
  let alias_node = cursor.opt_node(K::Alias);
  let name = cursor.name_token(source, "a field name")?;
  let arguments_node = cursor.opt_node(K::Arguments);
  let directives_node = cursor.opt_node(K::Directives);
  let selection_set_node = cursor.opt_node(K::SelectionSet);
  cursor.end()?;

  let alias = cursor.keep_opt(alias_node.map(|child| alias(child, source)).transpose()?);
  let arguments = cursor.keep_optional(optional_arguments(arguments_node, source)?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  match selection_set_node {
    Some(set) => Ok(Ok(OpenSelection::Field {
      node,
      extent: cursor.extent,
      alias,
      name,
      arguments,
      directives,
      pending: set,
    })),
    None => {
      let extent = cursor.range("a token")?;
      Ok(Err((
        Field::new(to_span(extent), alias, name, arguments, directives, None),
        extent,
      )))
    }
  }
}

/// A field alias, `name :` — a node here, and its span holds the colon.
fn alias<'src>(node: Node<'_>, source: &'src str) -> Out<(Alias<&'src str>, TextRange)> {
  // `Name :`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "an alias")?;
  cursor.token(K::Colon, "the `:` after an alias")?;
  let extent = cursor.finish("a token")?;
  Ok((Alias::new(to_span(extent), name), extent))
}

fn fragment_spread<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(FragmentSpread<&'src str>, TextRange)> {
  // `... FragmentName Directives?`. `... on` is an inline fragment's head in both parsers — the
  // lossless spread dispatch, the syntactic spread arm — and the committed spread reads its target
  // through the same `FragmentName` door a definition does, so an `on` here is no spread any
  // source produces.
  let mut cursor = Cursor::new(node);
  cursor.token(K::Spread, "the `...` a spread opens with")?;
  let name = cursor.name_except(
    source,
    &[ContextualKeyword::On],
    "a fragment spread may not target `on`",
    "a fragment name",
  )?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((
    FragmentSpread::new(to_span(extent), FragmentName::from_name(name), directives),
    extent,
  ))
}

/// An inline fragment read as far as its selection set, which the grammar makes mandatory — so
/// unlike a field this always nests.
fn open_inline_fragment<'g, 'src>(
  node: Node<'g>,
  source: &'src str,
) -> Out<OpenSelection<'g, 'src>> {
  // `... (on NamedType)? Directives? SelectionSet` — **the `on` commits its type**, and here it is
  // not lenient: the spread dispatch opens an inline fragment *because* it read `on`, so no parse
  // holds this node's type without it, and a type with no `on` in front of it is refused where it
  // stands.
  let mut cursor = Cursor::new(node);
  cursor.token(K::Spread, "the `...` an inline fragment opens with")?;
  let condition = match cursor.opt_keyword(ContextualKeyword::On) {
    Some(on) => Some((on, cursor.node(K::NamedType, "the type an `on` names")?)),
    None => None,
  };
  let directives_node = cursor.opt_node(K::Directives);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let type_condition = cursor.keep_opt(
    condition
      .map(|(on, child)| type_condition(Some(on), child, source))
      .transpose()?,
  );
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  Ok(OpenSelection::InlineFragment {
    node,
    extent: cursor.extent,
    type_condition,
    directives,
    pending: set,
  })
}

/// Build the selection that was waiting on `set`, and answer its own extent.
fn close_selection<'src>(
  open: OpenSelection<'_, 'src>,
  set: SelectionSet<&'src str>,
  piece: TextRange,
) -> Out<(Selection<&'src str>, TextRange)> {
  Ok(match open {
    OpenSelection::Field {
      node,
      mut extent,
      alias,
      name,
      arguments,
      directives,
      ..
    } => {
      extent.cover(piece);
      let range = extent.range(node, "a token")?;
      (
        Selection::Field(Field::new(
          to_span(range),
          alias,
          name,
          arguments,
          directives,
          Some(set),
        )),
        range,
      )
    }
    OpenSelection::InlineFragment {
      node,
      mut extent,
      type_condition,
      directives,
      ..
    } => {
      extent.cover(piece);
      let range = extent.range(node, "a token")?;
      (
        Selection::InlineFragment(InlineFragment::new(
          to_span(range),
          type_condition,
          directives,
          set,
        )),
        range,
      )
    }
  })
}

// ---------------------------------------------------------------------------------------------
// type references
// ---------------------------------------------------------------------------------------------

/// The three type-reference node kinds.
const TYPE_KINDS: [SyntaxKind; 3] = [K::NamedType, K::ListType, K::NonNullType];

/// The two kinds a `NonNullType` wraps: `T!!` has no production, so a `NonNullType` is not one.
const NULLABLE_KINDS: [SyntaxKind; 2] = [K::NamedType, K::ListType];

/// A `NamedType` node's name and extent: `Name`, nothing else.
///
/// The AST holds a bare [`Name`] wherever the grammar can hold no `!` and no brackets — an
/// implemented interface, a union member, a type condition, a root operation type — so those
/// positions read the name out of the node rather than building a type reference over it.
fn named_type_name<'src>(node: Node<'_>, source: &'src str) -> Out<(Name<&'src str>, TextRange)> {
  // `Name`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "a type name")?;
  let extent = cursor.finish("a token")?;
  Ok((name, extent))
}

/// A `ListType` whose element is being built below, and the `!` that folds into it.
///
/// The single-child half of this file's four worklists. `[[[Int]]]` nests without bound, so the
/// walk that reads it is a loop over these rather than a native frame per bracket — see the
/// module header's *No node dispatch below spends a native frame per level*. A list holds exactly
/// one element, so its whole sequence is read before the descent and only the folds travel.
struct TypeFrame<'g> {
  /// The `ListType` node, whose own tokens are the brackets [`Self::extent`] folded.
  list: Node<'g>,
  /// The fold over `list`'s tokens.
  extent: Extent,
  /// The `NonNullType` that wraps `list`, when the AST list being built is `[T]!`.
  ///
  /// `[T]!` builds **one** AST list from two tree nodes and the span it carries is the outer
  /// one's, so the wrapper's own fold has to survive the descent alongside the inner one's.
  required: Option<Required<'g>>,
}

/// The `NonNullType` half of a `[T]!`: the node whose span the finished list carries, and the
/// fold over its own tokens — the `!` among them.
struct Required<'g> {
  node: Node<'g>,
  extent: Extent,
}

/// A `ListType`'s sequence, `[ Type ]`, read up to its element: the list's fold and the element's
/// node.
fn open_list_element<'g>(node: Node<'g>) -> Out<(Extent, Node<'g>)> {
  // `[ Type ]`, with the `]` lenient — `unclosed_list` builds the node hole-free without it.
  let mut cursor = Cursor::new(node);
  cursor.token(K::LBracket, "the `[` a list type opens with")?;
  let element = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  cursor.opt_token(K::RBracket);
  cursor.end()?;
  Ok((cursor.extent, element))
}

/// Open list types from `node` down to the first type reference that needs no frame, and answer
/// that one.
///
/// The `!` folds into the node it wraps, exactly as the syntactic parser folds it: a `NonNullType`
/// has no AST image of its own, `T!` is a `NamedType` with `required` set, and its span is the
/// extent that includes the `!`.
fn open_type_chain<'g, 'src>(
  frames: &mut Vec<TypeFrame<'g>>,
  node: Node<'g>,
  source: &'src str,
) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let mut node = node;
  loop {
    match node.kind() {
      K::NamedType => {
        let (name, extent) = named_type_name(node, source)?;
        return Ok((
          Type::Name(NamedType::new(to_span(extent), name, false)),
          extent,
        ));
      }
      K::ListType => {
        let (extent, element) = open_list_element(node)?;
        frames.push(TypeFrame {
          list: node,
          extent,
          required: None,
        });
        node = element;
      }
      K::NonNullType => {
        // `(NamedType | ListType) !` — the `!` is a token of this node, and the span it produces
        // is this node's.
        let mut cursor = Cursor::new(node);
        let inner = cursor.one_of(&NULLABLE_KINDS, "a wrapped type")?;
        cursor.token(K::Bang, "the `!` a non-null type ends with")?;
        cursor.end()?;
        match inner.kind() {
          K::NamedType => {
            let name = cursor.keep(named_type_name(inner, source)?);
            let extent = cursor.range("a token")?;
            return Ok((
              Type::Name(NamedType::new(to_span(extent), name, true)),
              extent,
            ));
          }
          _ => {
            let (inner_extent, element) = open_list_element(inner)?;
            frames.push(TypeFrame {
              list: inner,
              extent: inner_extent,
              required: Some(Required {
                node,
                extent: cursor.extent,
              }),
            });
            node = element;
          }
        }
      }
      found => return Err(unexpected(node, found, node.text_range())),
    }
  }
}

/// Fold the element the level below finished into the list that wraps it.
///
/// A list type holds exactly one element, so a frame closed here is finished: unlike the value
/// and selection worklists there is no next child to scan for.
fn close_type<'src>(
  frame: TypeFrame<'_>,
  element: Type<Name<&'src str>>,
  piece: TextRange,
) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let TypeFrame {
    list,
    mut extent,
    required,
  } = frame;
  extent.cover(piece);
  let extent = extent.range(list, "a token")?;
  match required {
    None => Ok((
      ListType::new(to_span(extent), element, false).into(),
      extent,
    )),
    Some(Required {
      node,
      extent: mut outer,
    }) => {
      outer.cover(extent);
      let outer = outer.range(node, "a token")?;
      Ok((ListType::new(to_span(outer), element, true).into(), outer))
    }
  }
}

/// A type reference, with the list nesting inside it read on a worklist rather than a stack.
fn ty<'src>(node: Node<'_>, source: &'src str) -> Out<(Type<Name<&'src str>>, TextRange)> {
  let mut frames: Vec<TypeFrame<'_>> = Vec::new();
  let mut built = open_type_chain(&mut frames, node, source)?;
  while let Some(frame) = frames.pop() {
    let (element, piece) = built;
    built = close_type(frame, element, piece)?;
  }
  Ok(built)
}

// ---------------------------------------------------------------------------------------------
// directives and arguments
// ---------------------------------------------------------------------------------------------

/// A directive run, `Directive+` — **row one** of the container table: undelimited and at least
/// one, so a present node with no [`Directive`](SyntaxKind::Directive) child stands for no value
/// the parser produces and is refused as [`MissingChild`](ProjectErrorKind::MissingChild). The
/// walk this replaces answered `Some(Directives { directives: [] })` for a run holding a stray `@`
/// and no directive — al8n/smear#218's worse form of finding 1. Only an *absent* run is `None`.
fn optional_directives<'src>(
  run: Option<Node<'_>>,
  source: &'src str,
) -> Out<Optional<Directives<&'src str>>> {
  let Some(run) = run else {
    return Ok((None, None));
  };
  // `Directive+`
  let mut cursor = Cursor::new(run);
  let listed = cursor.many1(&[K::Directive], None, "a directive")?;
  cursor.end()?;
  let mut directives = Vec::with_capacity(listed.len());
  for child in listed {
    directives.push(cursor.keep(directive(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    Some(Directives::new(to_span(extent), directives)),
    Some(extent),
  ))
}

fn directive<'src>(node: Node<'_>, source: &'src str) -> Out<(Directive<&'src str>, TextRange)> {
  // `@ Name Arguments?`
  let mut cursor = Cursor::new(node);
  cursor.token(K::At, "the `@` a directive opens with")?;
  let name = cursor.name_token(source, "a directive name")?;
  let arguments_node = cursor.opt_node(K::Arguments);
  cursor.end()?;
  let arguments = cursor.keep_optional(optional_arguments(arguments_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((Directive::new(to_span(extent), name, arguments), extent))
}

/// An argument list, `( Argument* )` — **row two** of the container table.
///
/// Delimited, so `()` is a real, written-down empty list and gets its node — and the syntactic
/// parser answers `None` for it while still covering the parentheses. The walk this replaces
/// answered `Some(Arguments { arguments: [] })`, so `project(&parse, src) != document(src)` over
/// `query Q { f() }` and `type T @d() { f: Int }`. al8n/smear#217.
fn optional_arguments<'src>(
  list: Option<Node<'_>>,
  source: &'src str,
) -> Out<Optional<Arguments<&'src str>>> {
  let Some(list) = list else {
    return Ok((None, None));
  };
  // `( Argument* )`, with the `)` **lenient** — see the module header's missing-token table.
  let mut cursor = Cursor::new(list);
  cursor.token(K::LParen, "the `(` an argument list opens with")?;
  let listed = cursor.many(&[K::Argument]);
  cursor.opt_token(K::RParen);
  cursor.end()?;
  let mut arguments = Vec::with_capacity(listed.len());
  for child in listed {
    arguments.push(cursor.keep(argument(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    (!arguments.is_empty()).then(|| Arguments::new(to_span(extent), arguments)),
    Some(extent),
  ))
}

fn argument<'src>(node: Node<'_>, source: &'src str) -> Out<(Argument<&'src str>, TextRange)> {
  // `Name : Value`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "an argument name")?;
  cursor.token(K::Colon, "the `:` before an argument's value")?;
  let value_node = cursor.one_of(&VALUE_KINDS, "a value")?;
  cursor.end()?;
  let value = cursor.keep(value(value_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((Argument::new(to_span(extent), name, value), extent))
}

fn optional_const_directives<'src>(
  run: Option<Node<'_>>,
  source: &'src str,
) -> Out<Optional<ConstDirectives<&'src str>>> {
  let Some(run) = run else {
    return Ok((None, None));
  };
  // `Directive[Const]+`
  let mut cursor = Cursor::new(run);
  let listed = cursor.many1(&[K::Directive], None, "a directive")?;
  cursor.end()?;
  let mut directives = Vec::with_capacity(listed.len());
  for child in listed {
    directives.push(cursor.keep(const_directive(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    Some(ConstDirectives::new(to_span(extent), directives)),
    Some(extent),
  ))
}

fn const_directive<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstDirective<&'src str>, TextRange)> {
  // `@ Name Arguments[Const]?`
  let mut cursor = Cursor::new(node);
  cursor.token(K::At, "the `@` a directive opens with")?;
  let name = cursor.name_token(source, "a directive name")?;
  let arguments_node = cursor.opt_node(K::Arguments);
  cursor.end()?;
  let arguments = cursor.keep_optional(optional_const_arguments(arguments_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((
    ConstDirective::new(to_span(extent), name, arguments),
    extent,
  ))
}

fn optional_const_arguments<'src>(
  list: Option<Node<'_>>,
  source: &'src str,
) -> Out<Optional<ConstArguments<&'src str>>> {
  let Some(list) = list else {
    return Ok((None, None));
  };
  // `( Argument[Const]* )`, the `)` lenient as in the non-const list.
  let mut cursor = Cursor::new(list);
  cursor.token(K::LParen, "the `(` an argument list opens with")?;
  let listed = cursor.many(&[K::Argument]);
  cursor.opt_token(K::RParen);
  cursor.end()?;
  let mut arguments = Vec::with_capacity(listed.len());
  for child in listed {
    arguments.push(cursor.keep(const_argument(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    (!arguments.is_empty()).then(|| ConstArguments::new(to_span(extent), arguments)),
    Some(extent),
  ))
}

fn const_argument<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstArgument<&'src str>, TextRange)> {
  // `Name : Value[Const]`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "an argument name")?;
  cursor.token(K::Colon, "the `:` before an argument's value")?;
  let value_node = cursor.one_of(&VALUE_KINDS, "a value")?;
  cursor.end()?;
  let value = cursor.keep(const_value(node, value_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((ConstArgument::new(to_span(extent), name, value), extent))
}

// ---------------------------------------------------------------------------------------------
// values
// ---------------------------------------------------------------------------------------------

/// The nine value node kinds.
const VALUE_KINDS: [SyntaxKind; 9] = [
  K::Variable,
  K::IntValue,
  K::FloatValue,
  K::StringValue,
  K::BooleanValue,
  K::NullValue,
  K::EnumValue,
  K::ListValue,
  K::ObjectValue,
];

/// A leaf value node's one token and the node's extent.
///
/// One literal token and nothing else. The walk this replaces read the first token of the leaf's
/// kind and folded the rest, so `IntValue` over `Int("1")` and `Int("2")` answered `1` with a span
/// across both.
fn leaf<'g>(node: Node<'g>, kind: SyntaxKind, wanted: &'static str) -> Out<(Token<'g>, TextRange)> {
  let mut cursor = Cursor::new(node);
  let token = cursor.token(kind, wanted)?;
  let extent = cursor.finish("a token")?;
  Ok((token, extent))
}

/// A leaf holding one `Name` read for its **spelling** — `true`, `false`, `null` — and the
/// spelling's classification through the lexer's own table. The caller refuses what it does not
/// classify.
fn spelled_leaf<'g>(
  node: Node<'g>,
  wanted: &'static str,
) -> Out<(Option<ContextualKeyword>, Token<'g>, TextRange)> {
  let mut cursor = Cursor::new(node);
  let token = cursor.spelling(wanted)?;
  let extent = cursor.finish("a token")?;
  Ok((keyword_of(token), token, extent))
}

fn string_literal<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(StringValue<&'src str>, TextRange)> {
  // One string token, either spelling. The [`StringValue`]'s span is its **token's**, which is
  // what the syntactic parser produces.
  let mut cursor = Cursor::new(node);
  let token = cursor.token_of(&STRING_KINDS, "a string literal")?;
  let extent = cursor.finish("a token")?;
  Ok((string_value(token, source)?, extent))
}

fn boolean_literal<'src>(node: Node<'_>) -> Out<(BooleanValue<&'src str>, TextRange)> {
  // One `Name`, read for its spelling.
  let (keyword, token, extent) = spelled_leaf(node, "a `true` or `false` keyword")?;
  let span = to_span(token.text_range());
  match keyword {
    Some(ContextualKeyword::True) => Ok((BooleanValue::new(span, true), extent)),
    Some(ContextualKeyword::False) => Ok((BooleanValue::new(span, false), extent)),
    _ => Err(malformed(token)),
  }
}

/// An `EnumValue` node's name — in a value position and in an enum value definition alike — and the
/// one rule both positions share: it is not `true`, `false` or `null`.
///
/// Derived from the syntactic parser rather than remembered: the enum production refuses the three
/// spellings, and the value dispatch reads them as a boolean and a null before an enum is ever
/// tried. The lossless enum value definition reports the violation and still builds the node, so
/// the shape alone cannot tell a legal declaring name from an illegal one.
fn enum_value_name<'src>(node: Node<'_>, source: &'src str) -> Out<(Name<&'src str>, TextRange)> {
  // `Name`, not `true`, `false` or `null`.
  let mut cursor = Cursor::new(node);
  let name = cursor.name_except(
    source,
    &[
      ContextualKeyword::True,
      ContextualKeyword::False,
      ContextualKeyword::Null,
    ],
    "an enum value may not be `true`, `false` or `null`",
    "an enum value",
  )?;
  let extent = cursor.finish("a token")?;
  Ok((name, extent))
}

fn variable_value<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(VariableValue<&'src str>, TextRange)> {
  // `$ Name`
  let mut cursor = Cursor::new(node);
  cursor.token(K::Dollar, "the `$` a variable opens with")?;
  let name = cursor.name_token(source, "a variable name")?;
  let extent = cursor.finish("a token")?;
  Ok((VariableValue::new(to_span(extent), name), extent))
}

/// The two value grammars this dialect projects, over one walk.
///
/// `value` and `const_value` are each other with `Const` spelled in: nine arms, seven of them
/// character-for-character identical, differing in the constructors they name and in one refusal.
/// Duplicating a `match` is cheap; **duplicating a worklist is not** — the arms are the easy half
/// and the frame discipline is the half a second copy gets subtly wrong — so the machine below is
/// written once and the difference is this trait.
trait ValueGrammar<'src> {
  /// The value this grammar builds.
  type Value;
  /// One field of an object value in this grammar.
  type Field;

  /// A `$name` in this position.
  ///
  /// The one arm the two grammars genuinely disagree about: [`ConstInputValue`] has no `Variable`
  /// variant, so a constant position has nothing to construct and answers a refusal attributed to
  /// `parent` — the position, not the variable, is what is wrong.
  fn variable(parent: Node<'_>, node: Node<'_>, source: &'src str)
  -> Out<(Self::Value, TextRange)>;

  fn int(span: SimpleSpan, text: &'src str) -> Self::Value;
  fn float(span: SimpleSpan, text: &'src str) -> Self::Value;
  fn string(value: StringValue<&'src str>) -> Self::Value;
  fn boolean(value: BooleanValue<&'src str>) -> Self::Value;
  fn null(span: SimpleSpan, text: &'src str) -> Self::Value;
  fn enumeration(span: SimpleSpan, text: &'src str) -> Self::Value;
  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value;
  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value;
  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field;
}

/// A value position that admits a variable — an argument, a list element or an object field of a
/// non-constant argument.
struct Executable;

impl<'src> ValueGrammar<'src> for Executable {
  type Value = InputValue<&'src str>;
  type Field = ObjectField<&'src str>;

  fn variable(_: Node<'_>, node: Node<'_>, source: &'src str) -> Out<(Self::Value, TextRange)> {
    let (variable, extent) = variable_value(node, source)?;
    Ok((InputValue::Variable(variable), extent))
  }

  fn int(span: SimpleSpan, text: &'src str) -> Self::Value {
    InputValue::Int(IntValue::new(span, text))
  }

  fn float(span: SimpleSpan, text: &'src str) -> Self::Value {
    InputValue::Float(FloatValue::new(span, text))
  }

  fn string(value: StringValue<&'src str>) -> Self::Value {
    InputValue::String(value)
  }

  fn boolean(value: BooleanValue<&'src str>) -> Self::Value {
    InputValue::Boolean(value)
  }

  fn null(span: SimpleSpan, text: &'src str) -> Self::Value {
    InputValue::Null(NullValue::new(span, text))
  }

  fn enumeration(span: SimpleSpan, text: &'src str) -> Self::Value {
    InputValue::Enum(EnumValue::new(span, text))
  }

  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    InputValue::List(List::new(span, values.into()))
  }

  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value {
    InputValue::Object(Object::new(span, fields.into()))
  }

  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field {
    ObjectField::new(span, name, value)
  }
}

/// A constant value position, where the AST's own type system forbids a variable.
struct Constant;

impl<'src> ValueGrammar<'src> for Constant {
  type Value = ConstInputValue<&'src str>;
  type Field = ConstObjectField<&'src str>;

  fn variable(parent: Node<'_>, node: Node<'_>, _: &'src str) -> Out<(Self::Value, TextRange)> {
    // The refusal is attributed to the position, not to the variable: a `Variable` node is
    // perfectly legal, and what is wrong is the const context that is holding one.
    Err(unexpected(parent, K::Variable, node.text_range()))
  }

  fn int(span: SimpleSpan, text: &'src str) -> Self::Value {
    ConstInputValue::Int(IntValue::new(span, text))
  }

  fn float(span: SimpleSpan, text: &'src str) -> Self::Value {
    ConstInputValue::Float(FloatValue::new(span, text))
  }

  fn string(value: StringValue<&'src str>) -> Self::Value {
    ConstInputValue::String(value)
  }

  fn boolean(value: BooleanValue<&'src str>) -> Self::Value {
    ConstInputValue::Boolean(value)
  }

  fn null(span: SimpleSpan, text: &'src str) -> Self::Value {
    ConstInputValue::Null(NullValue::new(span, text))
  }

  fn enumeration(span: SimpleSpan, text: &'src str) -> Self::Value {
    ConstInputValue::Enum(EnumValue::new(span, text))
  }

  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    ConstInputValue::List(ConstList::new(span, values.into()))
  }

  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value {
    ConstInputValue::Object(ConstObject::new(span, fields.into()))
  }

  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field {
    ConstObjectField::new(span, name, value)
  }
}

/// An `ObjectField` whose name and own tokens are folded and whose value is being built below.
#[derive(Clone, Copy)]
struct OpenField<'g, 'src> {
  /// The `ObjectField` node — the owner of the field's span, and the parent a refusal inside its
  /// value names.
  node: Node<'g>,
  /// The field's fold, everything but its value.
  extent: Extent,
  name: Name<&'src str>,
  /// The value node the descent below is building.
  pending: Node<'g>,
}

/// A container value suspended while the value below it is built.
///
/// `{a: {a: … }}` and `[[…]]` nest without bound at the lexer's own ceiling, so the walk that
/// reads them is a loop over these rather than a native frame per level.
struct ValueFrame<'g, 'src, G: ValueGrammar<'src>> {
  /// The container's cursor: its node, the fold over its own tokens and the members already
  /// finished, and the members not yet read.
  cursor: Cursor<'g>,
  /// What has been folded so far, and which container this is.
  built: Built<'g, 'src, G>,
}

/// A container's accumulator.
enum Built<'g, 'src, G: ValueGrammar<'src>> {
  /// A `ListValue`'s elements, in document order.
  List(Vec<G::Value>),
  /// An `ObjectValue`'s finished fields, and the field whose value is being built below.
  Object {
    fields: Vec<G::Field>,
    open: OpenField<'g, 'src>,
  },
}

/// What a [`ValueFrame`] did with the value the level below finished.
///
/// A frame that descends again is pushed back onto the worklist by the resume itself, so this
/// answer carries only the two nodes.
enum ResumedValue<'g, 'src, G: ValueGrammar<'src>> {
  /// The node whose dispatch reaches the next value, and that value's node; the frame is back on
  /// the worklist.
  Descend(Node<'g>, Node<'g>),
  /// This frame is finished, and what it finished to.
  Done(G::Value, TextRange),
}

/// The next field of an object value.
///
/// **It does not ask whether the field's value is a container**, and it must not: deciding that
/// means entering the value, and entering an object value is what calls this — so the peek would be
/// one native frame per level of nesting, which is the whole defect back again on the other side of
/// the worklist.
fn next_field<'g, 'src>(
  cursor: &mut Cursor<'g>,
  source: &'src str,
) -> Out<Option<OpenField<'g, 'src>>> {
  match cursor.opt_node(K::ObjectField) {
    Some(child) => open_object_field(child, source).map(Some),
    None => Ok(None),
  }
}

/// An `ObjectField` read as far as its value.
fn open_object_field<'g, 'src>(node: Node<'g>, source: &'src str) -> Out<OpenField<'g, 'src>> {
  // `Name : Value` — the value is the last constituent, so everything else is read before the
  // descent and only the fold travels.
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "a field name")?;
  cursor.token(K::Colon, "the `:` before a field's value")?;
  let pending = cursor.one_of(&VALUE_KINDS, "a value")?;
  cursor.end()?;
  Ok(OpenField {
    node,
    extent: cursor.extent,
    name,
    pending,
  })
}

/// Build the field that was waiting on `value`, and answer its own extent.
fn close_field<'src, G: ValueGrammar<'src>>(
  open: OpenField<'_, 'src>,
  value: G::Value,
  piece: TextRange,
) -> Out<(G::Field, TextRange)> {
  let OpenField {
    node,
    mut extent,
    name,
    ..
  } = open;
  extent.cover(piece);
  let extent = extent.range(node, "a token")?;
  Ok((G::field(to_span(extent), name, value), extent))
}

/// Open containers from `node` down to the first value that finishes without one, and answer that
/// one.
///
/// Every container on the way is suspended on `frames` with the child it must build first already
/// chosen, so a frame is never on the stack without a live descent below it.
///
/// `parent` is the node whose dispatch reached `node`. A green tree carries no parent pointer, and
/// this is where the one a refusal names comes from.
fn open_value_chain<'g, 'src, G: ValueGrammar<'src>>(
  frames: &mut Vec<ValueFrame<'g, 'src, G>>,
  parent: Node<'g>,
  node: Node<'g>,
  source: &'src str,
) -> Out<(G::Value, TextRange)> {
  let (mut parent, mut node) = (parent, node);
  loop {
    match node.kind() {
      K::Variable => return G::variable(parent, node, source),
      K::IntValue => {
        let (token, extent) = leaf(node, K::Int, "an integer literal")?;
        let text = int_text(source, token)?;
        return Ok((G::int(to_span(token.text_range()), text), extent));
      }
      K::FloatValue => {
        let (token, extent) = leaf(node, K::Float, "a float literal")?;
        let text = float_text(source, token)?;
        return Ok((G::float(to_span(token.text_range()), text), extent));
      }
      K::StringValue => {
        let (string, extent) = string_literal(node, source)?;
        return Ok((G::string(string), extent));
      }
      K::BooleanValue => {
        let (boolean, extent) = boolean_literal(node)?;
        return Ok((G::boolean(boolean), extent));
      }
      K::NullValue => {
        let (keyword, token, extent) = spelled_leaf(node, "a `null` keyword")?;
        // The spelling, not just the kind: `NullValue` carries its own text into the AST, so a
        // node of this kind over any other identifier would project to a `null` the parser has no
        // way to produce. Its sibling `BooleanValue` has always checked; this one did not.
        if keyword != Some(ContextualKeyword::Null) {
          return Err(malformed(token));
        }
        return Ok((
          G::null(to_span(token.text_range()), slice(source, token)?),
          extent,
        ));
      }
      K::EnumValue => {
        let (name, extent) = enum_value_name(node, source)?;
        return Ok((G::enumeration(to_span(extent), name.source()), extent));
      }
      K::ListValue | K::ObjectValue => {
        // `[ Value* ]` and `{ ObjectField* }` — the opener here, each member one descent, and the
        // closer when the members run out.
        let mut cursor = Cursor::new(node);
        let opened = match node.kind() {
          K::ListValue => {
            cursor.token(K::LBracket, "the `[` a list opens with")?;
            cursor
              .opt_one_of(&VALUE_KINDS)
              .map(|first| (node, first, Built::List(Vec::new())))
          }
          _ => {
            cursor.token(K::LBrace, "the `{` an object opens with")?;
            next_field(&mut cursor, source)?.map(|open| {
              (
                open.node,
                open.pending,
                Built::Object {
                  fields: Vec::new(),
                  open,
                },
              )
            })
          }
        };
        match opened {
          Some((owner, first, built)) => {
            frames.push(ValueFrame { cursor, built });
            parent = owner;
            node = first;
          }
          None => {
            let range = close_container(&mut cursor)?;
            let span = to_span(range);
            let empty = match node.kind() {
              K::ListValue => G::list(span, Vec::new()),
              _ => G::object(span, Vec::new()),
            };
            return Ok((empty, range));
          }
        }
      }
      found => return Err(unexpected(node, found, node.text_range())),
    }
  }
}

/// The closer of a container value whose members have run out: `]` for a list, `}` for an object
/// — **lenient**, because `unclosed_list` and `unclosed_object` build each hole-free without it and
/// the closer has no AST image — and then nothing left over.
fn close_container(cursor: &mut Cursor<'_>) -> Out<TextRange> {
  match cursor.node.kind() {
    K::ListValue => cursor.opt_token(K::RBracket),
    _ => cursor.opt_token(K::RBrace),
  };
  cursor.finish("a token")
}

/// Fold the value the level below finished into the frame that was waiting on it, and read on to
/// the next child that needs one.
fn resume_value<'g, 'src, G: ValueGrammar<'src>>(
  frames: &mut Vec<ValueFrame<'g, 'src, G>>,
  frame: ValueFrame<'g, 'src, G>,
  value: G::Value,
  piece: TextRange,
  source: &'src str,
) -> Out<ResumedValue<'g, 'src, G>> {
  let ValueFrame { mut cursor, built } = frame;
  let node = cursor.node;
  Ok(match built {
    Built::List(mut values) => {
      cursor.extent.cover(piece);
      values.push(value);
      match cursor.opt_one_of(&VALUE_KINDS) {
        Some(next) => {
          frames.push(ValueFrame {
            cursor,
            built: Built::List(values),
          });
          ResumedValue::Descend(node, next)
        }
        None => {
          let range = close_container(&mut cursor)?;
          ResumedValue::Done(G::list(to_span(range), values), range)
        }
      }
    }
    Built::Object { mut fields, open } => {
      let (field, range) = close_field::<G>(open, value, piece)?;
      cursor.extent.cover(range);
      fields.push(field);
      match next_field(&mut cursor, source)? {
        Some(next) => {
          let (parent, pending) = (next.node, next.pending);
          frames.push(ValueFrame {
            cursor,
            built: Built::Object { fields, open: next },
          });
          ResumedValue::Descend(parent, pending)
        }
        None => {
          let range = close_container(&mut cursor)?;
          ResumedValue::Done(G::object(to_span(range), fields), range)
        }
      }
    }
  })
}

/// A value, with the nesting inside it read on a worklist rather than a stack.
fn value_tree<'src, G: ValueGrammar<'src>>(
  parent: Node<'_>,
  node: Node<'_>,
  source: &'src str,
) -> Out<(G::Value, TextRange)> {
  let mut frames: Vec<ValueFrame<'_, 'src, G>> = Vec::new();
  let mut built = open_value_chain::<G>(&mut frames, parent, node, source)?;
  loop {
    let Some(frame) = frames.pop() else {
      return Ok(built);
    };
    let (value, piece) = built;
    built = match resume_value::<G>(&mut frames, frame, value, piece, source)? {
      ResumedValue::Descend(parent, node) => {
        open_value_chain::<G>(&mut frames, parent, node, source)?
      }
      ResumedValue::Done(value, range) => (value, range),
    };
  }
}

fn value<'src>(node: Node<'_>, source: &'src str) -> Out<(InputValue<&'src str>, TextRange)> {
  value_tree::<Executable>(node, node, source)
}

/// A constant value position, where the AST's own type system forbids a variable.
///
/// `parent` is the node whose dispatch reached `node`. A green tree carries no parent pointer, and
/// the caller's own frame is where the one the refusal names comes from.
fn const_value<'src>(
  parent: Node<'_>,
  node: Node<'_>,
  source: &'src str,
) -> Out<(ConstInputValue<&'src str>, TextRange)> {
  value_tree::<Constant>(parent, node, source)
}

/// `= Value[Const]`, which is const in both positions the grammar puts it in.
fn optional_default_value<'src>(
  default: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(DefaultInputValue<&'src str>, TextRange)>> {
  let Some(default) = default else {
    return Ok(None);
  };
  // `= Value[Const]` — the span covers the `=` and the value, which is the node's own extent.
  let mut cursor = Cursor::new(default);
  cursor.token(K::Equal, "the `=` a default opens with")?;
  let value_node = cursor.one_of(&VALUE_KINDS, "a value")?;
  cursor.end()?;
  let value = cursor.keep(const_value(default, value_node, source)?);
  let extent = cursor.range("a token")?;
  Ok(Some((
    DefaultInputValue::new(to_span(extent), value),
    extent,
  )))
}

// ---------------------------------------------------------------------------------------------
// SDL definitions
// ---------------------------------------------------------------------------------------------

/// `implements &? NamedType (& NamedType)*` — the AST holds `Name`s, not `NamedType`s: an
/// implemented interface can carry no `!` and no brackets, so the type-reference level would be a
/// wrapper over nothing.
fn optional_implements<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(ImplementInterfaces<Name<&'src str>>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  // `implements &? NamedType (& NamedType)*`
  let mut cursor = Cursor::new(clause);
  cursor.keyword(ContextualKeyword::Implements, "the `implements` keyword")?;
  let members = cursor.separated_nodes(
    &[K::NamedType],
    K::Ampersand,
    Leading::Allowed,
    "an interface",
  )?;
  let mut interfaces = Vec::with_capacity(members.len());
  for child in members {
    interfaces.push(cursor.keep(named_type_name(child, source)?));
  }
  let extent = cursor.finish("a token")?;
  Ok(Some((
    ImplementInterfaces::new(to_span(extent), interfaces),
    extent,
  )))
}

fn optional_union_members<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(UnionMemberTypes<Name<&'src str>>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  // `= |? NamedType (| NamedType)*`
  let mut cursor = Cursor::new(clause);
  cursor.token(K::Equal, "the `=` before a union's members")?;
  let listed =
    cursor.separated_nodes(&[K::NamedType], K::Pipe, Leading::Allowed, "a member type")?;
  let mut members = Vec::with_capacity(listed.len());
  for child in listed {
    members.push(cursor.keep(named_type_name(child, source)?));
  }
  let extent = cursor.finish("a token")?;
  Ok(Some((
    UnionMemberTypes::new(to_span(extent), members),
    extent,
  )))
}

fn optional_fields_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(FieldsDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  // `{ FieldDefinition+ }`, with the `}` lenient.
  let mut cursor = Cursor::new(block);
  cursor.token(K::LBrace, "the `{` a fields block opens with")?;
  let listed = cursor.many1(&[K::FieldDefinition], Some(K::RBrace), "a field definition")?;
  cursor.opt_token(K::RBrace);
  cursor.end()?;
  let mut fields = Vec::with_capacity(listed.len());
  for child in listed {
    fields.push(cursor.keep(field_definition(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok(Some((
    FieldsDefinition::new(to_span(extent), fields),
    extent,
  )))
}

fn field_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(crate::graphql::ast::FieldDefinition<&'src str>, TextRange)> {
  // `Description? Name ArgumentsDefinition? : Type Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  let name = cursor.name_token(source, "a field name")?;
  let arguments_node = cursor.opt_node(K::ArgumentsDefinition);
  cursor.token(K::Colon, "the `:` before a field's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  // One span for both halves, description included — trunk's rule for this node, see the header.
  // The description is therefore folded straight in rather than kept apart for the hoist.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    cursor.extent.cover(described);
  }
  let arguments_definition =
    cursor.keep_opt(optional_arguments_definition(arguments_node, source)?);
  let ty = cursor.keep(ty(type_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  let extent = cursor.range("a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      FieldDefinition::new(span, name, arguments_definition, ty, directives),
    ),
    extent,
  ))
}

fn optional_arguments_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<
  Option<(
    crate::graphql::ast::ArgumentsDefinition<&'src str>,
    TextRange,
  )>,
> {
  let Some(block) = block else {
    return Ok(None);
  };
  // `( InputValueDefinition+ )`
  let mut cursor = Cursor::new(block);
  cursor.token(K::LParen, "the `(` an arguments definition opens with")?;
  let listed = cursor.many1(
    &[K::InputValueDefinition],
    Some(K::RParen),
    "an argument definition",
  )?;
  // Lenient: no AST image, and `unclosed_parens` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RParen);
  cursor.end()?;
  let mut definitions = Vec::with_capacity(listed.len());
  for child in listed {
    definitions.push(cursor.keep(input_value_definition(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok(Some((
    ArgumentsDefinition::new(to_span(extent), definitions),
    extent,
  )))
}

fn input_value_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphql::ast::InputValueDefinition<&'src str>,
  TextRange,
)> {
  // `Description? Name : Type DefaultValue? Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  let name = cursor.name_token(source, "an input value name")?;
  cursor.token(K::Colon, "the `:` before an input value's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let default_node = cursor.opt_node(K::DefaultValue);
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  // The second of the three node types whose wrapper and inner span agree — see the header.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    cursor.extent.cover(described);
  }
  let ty = cursor.keep(ty(type_node, source)?);
  let default_value = cursor.keep_opt(optional_default_value(default_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  let extent = cursor.range("a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      InputValueDefinition::new(span, name, ty, default_value, directives),
    ),
    extent,
  ))
}

fn optional_input_fields_definition<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(InputFieldsDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  // `{ InputValueDefinition+ }`, with the `}` lenient.
  let mut cursor = Cursor::new(block);
  cursor.token(K::LBrace, "the `{` an input fields block opens with")?;
  let listed = cursor.many1(
    &[K::InputValueDefinition],
    Some(K::RBrace),
    "an input field definition",
  )?;
  cursor.opt_token(K::RBrace);
  cursor.end()?;
  let mut definitions = Vec::with_capacity(listed.len());
  for child in listed {
    definitions.push(cursor.keep(input_value_definition(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok(Some((
    InputFieldsDefinition::new(to_span(extent), definitions),
    extent,
  )))
}

fn optional_enum_values<'src>(
  block: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(EnumValuesDefinition<&'src str>, TextRange)>> {
  let Some(block) = block else {
    return Ok(None);
  };
  // `{ EnumValueDefinition+ }`, with the `}` lenient.
  let mut cursor = Cursor::new(block);
  cursor.token(K::LBrace, "the `{` an enum values block opens with")?;
  let listed = cursor.many1(
    &[K::EnumValueDefinition],
    Some(K::RBrace),
    "an enum value definition",
  )?;
  cursor.opt_token(K::RBrace);
  cursor.end()?;
  let mut values = Vec::with_capacity(listed.len());
  for child in listed {
    values.push(cursor.keep(enum_value_definition(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok(Some((
    EnumValuesDefinition::new(to_span(extent), values),
    extent,
  )))
}

fn enum_value_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphql::ast::EnumValueDefinition<&'src str>,
  TextRange,
)> {
  // `Description? EnumValue Directives[Const]?` — the declaring name sits inside an `EnumValue`
  // node, the same kind a value position uses, and carries the same rule.
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  let value_node = cursor.node(K::EnumValue, "an enum value")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  // The third of the three node types whose wrapper and inner span agree — see the header.
  let (description, described) = hoisted_description(description_node, source)?;
  if let Some(described) = described {
    cursor.extent.cover(described);
  }
  let value = cursor.keep(enum_value_name(value_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  let extent = cursor.range("a token")?;
  let span = to_span(extent);
  Ok((
    Described::new(
      span,
      description,
      EnumValueDefinition::new(span, value, directives),
    ),
    extent,
  ))
}

fn scalar_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ScalarTypeDefinition<&'src str>>> {
  // `Description? scalar Name Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Scalar, "the `scalar` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    ScalarTypeDefinition::new(to_span(inner), name, directives),
    outer,
  ))
}

fn object_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, ObjectTypeDefinition<&'src str>>> {
  // `Description? type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Type, "the `type` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let implements_node = cursor.opt_node(K::ImplementsInterfaces);
  let directives_node = cursor.opt_node(K::Directives);
  let fields_node = cursor.opt_node(K::FieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields_definition = cursor.keep_opt(optional_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    ObjectTypeDefinition::new(
      to_span(inner),
      name,
      implements,
      directives,
      fields_definition,
    ),
    outer,
  ))
}

fn interface_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, InterfaceTypeDefinition<&'src str>>> {
  // `Description? interface Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Interface, "the `interface` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let implements_node = cursor.opt_node(K::ImplementsInterfaces);
  let directives_node = cursor.opt_node(K::Directives);
  let fields_node = cursor.opt_node(K::FieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields_definition = cursor.keep_opt(optional_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    InterfaceTypeDefinition::new(
      to_span(inner),
      name,
      implements,
      directives,
      fields_definition,
    ),
    outer,
  ))
}

fn union_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, UnionTypeDefinition<&'src str>>> {
  // `Description? union Name Directives[Const]? UnionMemberTypes?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Union, "the `union` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let members_node = cursor.opt_node(K::UnionMemberTypes);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let members = cursor.keep_opt(optional_union_members(members_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    UnionTypeDefinition::new(to_span(inner), name, directives, members),
    outer,
  ))
}

fn enum_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, EnumTypeDefinition<&'src str>>> {
  // `Description? enum Name Directives[Const]? EnumValuesDefinition?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Enum, "the `enum` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let values_node = cursor.opt_node(K::EnumValuesDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let values = cursor.keep_opt(optional_enum_values(values_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    EnumTypeDefinition::new(to_span(inner), name, directives, values),
    outer,
  ))
}

fn input_object_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, InputObjectTypeDefinition<&'src str>>> {
  // `Description? input Name Directives[Const]? InputFieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Input, "the `input` keyword")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let fields_node = cursor.opt_node(K::InputFieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields = cursor.keep_opt(optional_input_fields_definition(fields_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    InputObjectTypeDefinition::new(to_span(inner), name, directives, fields),
    outer,
  ))
}

fn directive_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphql::ast::DirectiveDefinition<&'src str>>> {
  // `Description? directive @ Name ArgumentsDefinition? repeatable? on DirectiveLocations` — three
  // keyword positions, each read by its spelling in its own place. The walk this replaces took
  // "the `Name` at index 2 is `repeatable` or `on`" and never read the `on`, so
  // `directive @d foo FIELD` answered what `directive @d on FIELD` answers.
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Directive, "the `directive` keyword")?;
  cursor.token(K::At, "the `@` before a directive's name")?;
  let name = cursor.name_token(source, "a name after the keyword")?;
  let arguments_node = cursor.opt_node(K::ArgumentsDefinition);
  let repeatable = cursor.opt_keyword(ContextualKeyword::Repeatable).is_some();
  // Lenient, as a fragment definition's `on` is: no AST image, and the production reports a
  // missing one and still builds the definition, hole-free, around the locations. The locations
  // stay required.
  cursor.opt_keyword(ContextualKeyword::On);
  let locations_node = cursor.node(K::DirectiveLocations, "a location list")?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let arguments_definition =
    cursor.keep_opt(optional_arguments_definition(arguments_node, source)?);
  let locations = cursor.keep(directive_locations(locations_node)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    DirectiveDefinition::new(
      to_span(inner),
      name,
      arguments_definition,
      repeatable,
      locations,
    ),
    outer,
  ))
}

/// A directive definition's `FIELD | QUERY` location list.
///
/// The locations are bare `Name` tokens inside one node, and the `on` before them is a token of the
/// **definition** rather than of this node — so the extent opens on the optional leading `|` or on
/// the first location, which is what the syntactic parser builds.
fn directive_locations(node: Node<'_>) -> Out<(DirectiveLocations<Location>, TextRange)> {
  // `|? Name (| Name)*` — each location a `Name` read for its spelling.
  let mut cursor = Cursor::new(node);
  let (tokens, _) = cursor.separated(
    |cursor| Ok(cursor.opt_spelling()),
    K::Pipe,
    Leading::Allowed,
    "a directive location",
  )?;
  let mut locations = Vec::with_capacity(tokens.len());
  for token in tokens {
    let location = keyword_of(token)
      .and_then(|keyword| classify_location(keyword, to_span(token.text_range())))
      .ok_or_else(|| malformed(token))?;
    locations.push(location);
  }
  let extent = cursor.finish("a token")?;
  Ok((DirectiveLocations::new(to_span(extent), locations), extent))
}

fn schema_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, SchemaDefinition<&'src str>>> {
  // `Description? schema Directives[Const]? RootOperationTypeDefinitions`
  let mut cursor = Cursor::new(node);
  let description_node = cursor.opt_node(K::Description);
  cursor.keyword(ContextualKeyword::Schema, "the `schema` keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let roots_node = cursor.node(
    K::RootOperationTypeDefinitions,
    "a root operation types block",
  )?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_node, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let roots = cursor.keep(root_operation_types(roots_node, source)?);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    SchemaDefinition::new(to_span(inner), directives, roots),
    outer,
  ))
}

fn root_operation_types<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(RootOperationTypesDefinition<&'src str>, TextRange)> {
  // `{ RootOperationTypeDefinition+ }`, with the `}` lenient.
  let mut cursor = Cursor::new(node);
  cursor.token(K::LBrace, "the `{` a root operation types block opens with")?;
  let listed = cursor.many1(
    &[K::RootOperationTypeDefinition],
    Some(K::RBrace),
    "a root operation type",
  )?;
  cursor.opt_token(K::RBrace);
  cursor.end()?;
  let mut roots = Vec::with_capacity(listed.len());
  for child in listed {
    roots.push(cursor.keep(root_operation_type(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    RootOperationTypesDefinition::new(to_span(extent), roots),
    extent,
  ))
}

fn root_operation_type<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(RootOperationTypeDefinition<&'src str>, TextRange)> {
  // `OperationType : NamedType`
  let mut cursor = Cursor::new(node);
  let keyword_node = cursor.node(K::OperationType, "an operation keyword")?;
  cursor.token(K::Colon, "the `:` before a root type")?;
  let named_node = cursor.node(K::NamedType, "a root type name")?;
  cursor.end()?;
  let operation_type = cursor.keep(operation_type(keyword_node)?);
  let named = cursor.keep(named_type_name(named_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((
    RootOperationTypeDefinition::new(to_span(extent), operation_type, named),
    extent,
  ))
}

// ---------------------------------------------------------------------------------------------
// SDL extensions
// ---------------------------------------------------------------------------------------------

/// The constituents an extension's tail is assembled from, and its extent.
///
/// Each extension kind is its own transcription, and what they share is a sequence **prefix**
/// rather than a slot struct filled from the union of six vocabularies — the union is what let a
/// caller-built `ScalarTypeExtension` carry a `FieldsDefinition` into a constructor that reads the
/// directives and answered `Ok` with the block dropped inside its span. al8n/smear#218's round-four
/// addendum. A slot below that the kind's own sequence has no place for is simply never filled:
/// the foreign child is not in the sequence, and `end` refuses it.
struct ExtensionParts<'src> {
  name: Name<&'src str>,
  implements: Option<ImplementInterfaces<Name<&'src str>>>,
  directives: Option<ConstDirectives<&'src str>>,
  fields: Option<FieldsDefinition<&'src str>>,
  input_fields: Option<InputFieldsDefinition<&'src str>>,
  members: Option<UnionMemberTypes<Name<&'src str>>>,
  values: Option<EnumValuesDefinition<&'src str>>,
  extent: TextRange,
}

fn extension_parts<'src>(node: Node<'_>, source: &'src str) -> Out<ExtensionParts<'src>> {
  // `extend <keyword> Name <this kind's tail>` — the prefix, then one tail per kind. A string a
  // caller wrote in front of the `extend` is inside this node (see `type_system_extension`) and
  // the `extend` atom is what refuses it.
  let mut cursor = Cursor::new(node);
  cursor.keyword(ContextualKeyword::Extend, "the `extend` keyword")?;
  let (keyword, spelling) = match node.kind() {
    K::ScalarTypeExtension => (ContextualKeyword::Scalar, "the `scalar` keyword"),
    K::ObjectTypeExtension => (ContextualKeyword::Type, "the `type` keyword"),
    K::InterfaceTypeExtension => (ContextualKeyword::Interface, "the `interface` keyword"),
    K::UnionTypeExtension => (ContextualKeyword::Union, "the `union` keyword"),
    K::EnumTypeExtension => (ContextualKeyword::Enum, "the `enum` keyword"),
    K::InputObjectTypeExtension => (ContextualKeyword::Input, "the `input` keyword"),
    // Not an extension this walk transcribes — `SchemaExtension` has its own, and anything else
    // means a caller was wired to the wrong one. A refusal and not a panic: a caller mints trees.
    _ => {
      return Err(missing(
        node,
        "an extension whose tail this walk transcribes",
      ));
    }
  };
  cursor.keyword(keyword, spelling)?;
  let name = cursor.name_token(source, "an extended type's name")?;

  let mut implements_node = None;
  let mut fields_node = None;
  let mut input_fields_node = None;
  let mut members_node = None;
  let mut values_node = None;
  let directives_node;
  match node.kind() {
    // `extend scalar Name Directives[Const]`
    K::ScalarTypeExtension => directives_node = cursor.opt_node(K::Directives),
    // `extend (type|interface) Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?`
    K::ObjectTypeExtension | K::InterfaceTypeExtension => {
      implements_node = cursor.opt_node(K::ImplementsInterfaces);
      directives_node = cursor.opt_node(K::Directives);
      fields_node = cursor.opt_node(K::FieldsDefinition);
    }
    // `extend union Name Directives[Const]? UnionMemberTypes?`
    K::UnionTypeExtension => {
      directives_node = cursor.opt_node(K::Directives);
      members_node = cursor.opt_node(K::UnionMemberTypes);
    }
    // `extend enum Name Directives[Const]? EnumValuesDefinition?`
    K::EnumTypeExtension => {
      directives_node = cursor.opt_node(K::Directives);
      values_node = cursor.opt_node(K::EnumValuesDefinition);
    }
    // `extend input Name Directives[Const]? InputFieldsDefinition?`
    _ => {
      directives_node = cursor.opt_node(K::Directives);
      input_fields_node = cursor.opt_node(K::InputFieldsDefinition);
    }
  }
  cursor.end()?;

  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields = cursor.keep_opt(optional_fields_definition(fields_node, source)?);
  let input_fields = cursor.keep_opt(optional_input_fields_definition(input_fields_node, source)?);
  let members = cursor.keep_opt(optional_union_members(members_node, source)?);
  let values = cursor.keep_opt(optional_enum_values(values_node, source)?);

  Ok(ExtensionParts {
    name,
    implements,
    directives,
    fields,
    input_fields,
    members,
    values,
    extent: cursor.range("a token")?,
  })
}

fn scalar_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ScalarTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  // The one extension whose directives the grammar makes mandatory: it has no other tail.
  let directives = parts
    .directives
    .ok_or_else(|| missing(node, "the directives a scalar extension must add"))?;
  Ok((
    ScalarTypeExtension::new(to_span(parts.extent), parts.name, directives),
    parts.extent,
  ))
}

fn object_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ObjectTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.implements, parts.directives, parts.fields) {
    (implements, directives, Some(fields_definition)) => ObjectTypeExtensionData::Fields {
      implements,
      directives,
      fields_definition,
    },
    (implements, Some(directives), None) => ObjectTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => ObjectTypeExtensionData::Implements(implements),
    (None, None, None) => {
      return Err(missing(
        node,
        "interfaces, directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    ObjectTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn interface_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InterfaceTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.implements, parts.directives, parts.fields) {
    (implements, directives, Some(fields_definition)) => InterfaceTypeExtensionData::Fields {
      implements,
      directives,
      fields_definition,
    },
    (implements, Some(directives), None) => InterfaceTypeExtensionData::Directives {
      implements,
      directives,
    },
    (Some(implements), None, None) => InterfaceTypeExtensionData::Implements(implements),
    (None, None, None) => {
      return Err(missing(
        node,
        "interfaces, directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    InterfaceTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn union_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(UnionTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.members) {
    (directives, Some(member_types)) => UnionTypeExtensionData::Members {
      directives,
      member_types,
    },
    (Some(directives), None) => UnionTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or members for the extension to add",
      ));
    }
  };
  Ok((
    UnionTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn enum_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(EnumTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.values) {
    (directives, Some(enum_values_definition)) => EnumTypeExtensionData::Values {
      directives,
      enum_values_definition,
    },
    (Some(directives), None) => EnumTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or values for the extension to add",
      ));
    }
  };
  Ok((
    EnumTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

fn input_object_type_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InputObjectTypeExtension<&'src str>, TextRange)> {
  let parts = extension_parts(node, source)?;
  let data = match (parts.directives, parts.input_fields) {
    (directives, Some(fields_definition)) => InputObjectTypeExtensionData::Fields {
      directives,
      fields_definition,
    },
    (Some(directives), None) => InputObjectTypeExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or fields for the extension to add",
      ));
    }
  };
  Ok((
    InputObjectTypeExtension::new(to_span(parts.extent), parts.name, data),
    parts.extent,
  ))
}

/// The one extension with no name: `extend schema` names nothing, so this walks the node itself
/// rather than going through [`extension_parts`].
fn schema_extension<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(SchemaExtension<&'src str>, TextRange)> {
  // `extend schema Directives[Const]? RootOperationTypeDefinitions?`
  let mut cursor = Cursor::new(node);
  cursor.keyword(ContextualKeyword::Extend, "the `extend` keyword")?;
  cursor.keyword(ContextualKeyword::Schema, "the `schema` keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let roots_node = cursor.opt_node(K::RootOperationTypeDefinitions);
  cursor.end()?;

  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let roots = match roots_node {
    Some(block) => Some(cursor.keep(root_operation_types(block, source)?)),
    None => None,
  };
  let data = match (directives, roots) {
    (directives, Some(root_operation_types_definition)) => SchemaExtensionData::Operations {
      directives,
      root_operation_types_definition,
    },
    (Some(directives), None) => SchemaExtensionData::Directives(directives),
    (None, None) => {
      return Err(missing(
        node,
        "directives or root operation types for the extension to add",
      ));
    }
  };
  let extent = cursor.range("a token")?;
  Ok((SchemaExtension::new(to_span(extent), data), extent))
}
