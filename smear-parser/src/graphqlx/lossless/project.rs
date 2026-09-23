//! The GraphQLx CST → AST projection.
//!
//! [`project`] turns a lossless parse plus the text it was parsed from into **the AST the
//! syntactic parser produces for that text**, without re-parsing. It is the door an editor
//! goes through: parse losslessly once, format and highlight off the tree, and hand the same
//! parse to a consumer that wants typed nodes.
//!
//! ```
//! # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
//! use smear_parser::graphqlx::lossless::{parse_document, project};
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
//! # Derived from the substrate, not diffed against the other dialect
//!
//! The contract, the error vocabulary, the span rule, the `(tree, source)` verification and the
//! green traversal are [the substrate](crate::lossless::project)'s, and the entry shapes are the
//! naming symmetry its header states. **Everything below that is this dialect's**: sixteen of
//! GraphQLx's seventy-eight wrappers are structurally identical to a GraphQL one, thirty-six share
//! a name with a different shape, and twenty-six have no GraphQL image at all. Three of those
//! differences are *mapping* divergences rather than renamings, and each one changes what a walk
//! has to do rather than what it has to call something:
//!
//! - **A definition's name is a node.** Every SDL definition's name is a
//!   [`DefinitionName`](SyntaxKind::DefinitionName) and every extension's target an
//!   [`ExtensionName`](SyntaxKind::ExtensionName), where GraphQL puts a bare `Name` token after
//!   the keyword and reaches it by index. So no walk here counts `Name` tokens to find a name —
//!   the keyword is the only direct one a definition has, and an operation's and a root operation
//!   type's is read for its *spelling* rather than for its position.
//! - **A description is a token.** This kind space has no `Description` node — one token is not a
//!   region — so a described definition carries its description as a direct
//!   [`InlineString`](SyntaxKind::InlineString) or [`BlockString`](SyntaxKind::BlockString) token
//!   of the definition node, and the hoist is a token the fold holds back rather than a child it
//!   skips.
//! - **Three carriers hold two subtrees in one child stream.**
//!   [`MapEntry`](SyntaxKind::MapEntry) is `Value => Value`, [`MapType`](SyntaxKind::MapType) is
//!   `< Type => Type >`, and a [`WherePredicate`](SyntaxKind::WherePredicate) is a constrained
//!   type followed by its bounds. Nothing tells the halves apart but their order, so the walks
//!   below read them positionally out of one `children()` pass — and for the two that nest, the
//!   half in hand travels **on the worklist frame**.
//!
//! # Why `source` is a parameter, and why it stays `&str`
//!
//! The AST is keyed by `S = &'src str` — the syntactic parser has the same property — and the
//! bytes it borrows are the **caller's**, not the tree's. A green token does carry its own text
//! and could lend it, but that text belongs to the parse, and [`Parse`] is deliberately
//! lifetime-free precisely so an editor can cache one per file and drop it on the next keystroke.
//!
//! Since al8n/smear#121 every lossless parse door has a `parse_*_from` sibling over any
//! [`LosslessSource`](crate::lossless::LosslessSource) — twelve across the two dialects, six here
//! — beside the `&str` door itself; the projection doors have no such sibling and stay `&str`,
//! which is a decision rather than an oversight. **What pins this parameter is the output, not the input**: a
//! projection hands back an AST that borrows the caller's buffer, so the AST's source type *is*
//! this parameter. Widening it would not widen a door — it would change what every consumer of the
//! lossless half receives. Nothing in the walk needs UTF-8 either; the verification compares bytes
//! and every slice is taken at a token boundary the tree already holds.
//!
//! It is **verified, not trusted**: each door compares the whole of the tree's text against
//! `source` before it walks anything, and a mismatch is [`ProjectErrorKind::SourceMismatch`]
//! rather than a silently wrong AST pointing into unrelated bytes.
//!
//! # Shape-faithful, not verdict-faithful
//!
//! The projection succeeds iff the tree's **shape** determines a well-formed AST. It does not
//! re-derive the acceptance verdict, which stays [`Parse::has_errors`]. `type T { x: Int` — no
//! closing brace — leaves a shape-complete tree with no recovery hole in it, so it projects, while
//! the syntactic parser rejects it. **Check [`has_errors`](Parse::has_errors) first**; a projection
//! of an errorful tree is best-effort.
//!
//! In the other direction the projection is stricter than shape alone: a tree carrying an
//! [`Error`](SyntaxKind::Error) hole or a [`Gap`](SyntaxKind::Gap) tile is refused outright, before
//! any walk, because a hole is a region with no AST image and skipping it would be data loss
//! wearing a success type.
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
//!    rule the whole file is built on. The three-way rule and the leaf re-cooking below serve the
//!    shape half.
//! 3. **Totality** — every non-trivia byte under a projected node is represented in the AST, or
//!    the projection refuses. The walk rule below is what serves this.
//!
//! It does **not** promise that the tree is the one the lossless parser would build over those
//! bytes. A caller can mint a green tree, and [`Verified`] exists so one can be projected; nothing
//! in the three guarantees says the tree is a parse.
//!
//! **The worked case.** Over the source `[-12]`, a caller-built list can hold two adjacent
//! [`IntValue`](SyntaxKind::IntValue) tokens, `-1` at `1..3` and `2` at `3..4`. Each slice is one
//! whole integer to [`LitInt`]'s door, the byte verification passes because the concatenation is
//! the source, and the projection answers `[-1, 2]` with exactly those two spans. The shipped
//! lexer would read one integer `-12`, so this is an AST no *parse* of `[-12]` yields — and it is
//! nonetheless inside all three guarantees, because it is the AST of the sentence the **tree**
//! spells. `a_tree_that_splits_a_token_projects_the_tree_it_was_handed` pins it as a decision.
//!
//! Restoring lexical-boundary fidelity would mean re-lexing the source around every token — the
//! re-parse `a_projection_that_re_parsed_the_source_would_fail_this` exists to forbid, and a
//! second custodian of boundaries the lexer already owns. So token-boundary fidelity is out of
//! contract, deliberately, and a caller who smuggles a hand-built tree past the lexer keeps the
//! pieces.
//!
//! # What the parser reports and still builds
//!
//! GraphQLx's lossless productions **report and continue**: a violation goes on the diagnostic
//! channel and the node is built anyway, so `Parse::has_errors()` sees it and the *shape* does not.
//! Every one of those shapes reaches this projection looking well-formed, and each needs an
//! answer. The population is every call that reports without building a hole — **48**
//! `recover::report_unexpected::<…>` sites and **17** `recover::unclosed_{list,object,parens,angle}::<…>`
//! sites across `graphqlx/lossless/*.rs`, the counts `grep -c` gives for those spellings — and
//! **all 65 are mapped** in the table below, one probe each: a text that reaches the site with a
//! hole-free tree, and what this projection answers for it. The table is executable:
//! `every_report_and_build_site_has_a_measured_answer` holds the same rows, asserts their count
//! per file and family against the source's own spelling of each call, and measures every answer.
//!
//! **The "what the site leaves out" column is where the lenient set comes from.** A site either
//! leaves out one token with **no AST image** (a closer, `from`, `on`), leaves out a constituent
//! the AST holds (a member, a path, a tail — never lenient), or leaves nothing out and reports
//! something present (an extra description, a spelling a rule refuses). For every row of the first
//! kind the site's own probe is the witness the criterion asks for, when its tree holds the node
//! without the token — and the cell derives the missing-token table below from exactly that and
//! asserts the two are the same set.
//!
//! **Sixteen project; forty-nine refuse.** The sixteen are the lenient rows a site can leave with a
//! well-formed value. The others refuse either the shape the site built, or — for the unclosed
//! sites whose definition the parser cannot finish at end of input — the rubble the document is
//! left holding.
//!
//! | file | site | probe | what the site leaves out | the projection |
//! |---|---|---|---|---|
//! | `document.rs` | report | `"d" import { A } from "m"` | nothing — a present token is refused | `UnexpectedChild { Document, InlineString }` |
//! | `document.rs` | report | `"d" extend scalar S @k` | nothing — a present token is refused | `UnexpectedChild { Document, InlineString }` |
//! | `document.rs` | report | `"d" { f }` | nothing — a present token is refused | `UnexpectedChild { OperationDefinition, InlineString }` |
//! | `document.rs` | report | `"d" import { A } from "m"` (executable root) | nothing — a present token is refused | `UnexpectedChild { ExecutableDocument, InlineString }` |
//! | `document.rs` | report | `"d" { f }` (executable root) | nothing — a present token is refused | `UnexpectedChild { OperationDefinition, InlineString }` |
//! | `document.rs` | report | `"d" import { A } from "m"` (SDL root) | nothing — a present token is refused | `UnexpectedChild { TypeSystemDocument, InlineString }` |
//! | `document.rs` | report | `"d" extend scalar S @k` (SDL root) | nothing — a present token is refused | `UnexpectedChild { TypeSystemDocument, InlineString }` |
//! | `document.rs` | report | the empty document | a constituent with an image | `MissingChild { Document }` |
//! | `document.rs` | report | the empty document (SDL root) | a constituent with an image | `MissingChild { TypeSystemDocument }` |
//! | `executable.rs` | report | `query Q() { f }` (executable root) | a constituent with an image | `MissingChild { VariablesDefinition }` |
//! | `executable.rs` | report | the empty document (executable root) | a constituent with an image | `MissingChild { ExecutableDocument }` |
//! | `executable.rs` | unclosed | `query Q($a: Int` (executable root) | `)` of `VariablesDefinition` — **no image** | `UnexpectedChild { ExecutableDocument, Name }` |
//! | `import.rs` | report | `import { A as } from "m"` | a constituent with an image | `MissingChild { NamedSpecifier }` |
//! | `import.rs` | report | `import { } from "m"` | a constituent with an image | `MissingChild { ImportList }` |
//! | `import.rs` | report | `import { A } "m"` | `from` of `ImportDefinition` — **no image** | **projects** |
//! | `import.rs` | report | `import { A } from """m"""` | nothing — a present token is refused | `UnexpectedChild { StringValue, BlockString }` |
//! | `import.rs` | report | `import { A } from` | a constituent with an image | `MissingChild { ImportDefinition }` |
//! | `import.rs` | unclosed | `import { A` | `}` of `ImportList` — **no image** | `MissingChild { ImportDefinition }` |
//! | `generic.rs` | report | `type T<> { f: Int }` | a constituent with an image | `MissingChild { DefinitionTypeGenerics }` |
//! | `generic.rs` | unclosed | `type T<A` | `>` of `DefinitionTypeGenerics` — **no image** | **projects** |
//! | `generic.rs` | report | `fragment <> F on T { f }` | a constituent with an image | `MissingChild { ExecutableDefinitionTypeGenerics }` |
//! | `generic.rs` | unclosed | `extend type T<A` | `>` of `ExtensionTypeGenerics` — **no image** | `MissingChild { ObjectTypeExtension }` |
//! | `generic.rs` | report | `fragment on on T { f }` | nothing — a present token is refused | `SemanticRule` |
//! | `generic.rs` | report | `type T where A: { f: Int }` | a constituent with an image | `MissingChild { WherePredicate }` |
//! | `generic.rs` | report | `type T where { f: Int }` | a constituent with an image | `MissingChild { WhereClause }` |
//! | `selection.rs` | report | `fragment F { f }` | a constituent with an image | `UnexpectedChild { FragmentDefinition, SelectionSet }` |
//! | `selection.rs` | report | `fragment F T { f }` | `on` of `TypeCondition` — **no image** | **projects** |
//! | `selection.rs` | report | `fragment F on { f }` | a constituent with an image | `MissingChild { TypeCondition }` |
//! | `selection.rs` | report | `{ ... }` | a constituent with an image | `UnexpectedChild { SelectionSet, Spread }` |
//! | `selection.rs` | report | `{ }` | a constituent with an image | `MissingChild { SelectionSet }` |
//! | `selection.rs` | unclosed | `{ f` | `}` of `SelectionSet` — **no image** | **projects** |
//! | `definition.rs` | report | `type T { f(): Int }` | a constituent with an image | `MissingChild { ArgumentsDefinition }` |
//! | `definition.rs` | unclosed | `type T { f(a: Int` | `)` of `ArgumentsDefinition` — **no image** | `UnexpectedChild { Document, Name }` |
//! | `definition.rs` | report | `type T { }` | a constituent with an image | `MissingChild { FieldsDefinition }` |
//! | `definition.rs` | unclosed | `type T { f: Int` | `}` of `FieldsDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `input I { }` | a constituent with an image | `MissingChild { InputFieldsDefinition }` |
//! | `definition.rs` | unclosed | `input I { f: Int` | `}` of `InputFieldsDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `type T implements A & { f: Int }` | a constituent with an image | `MissingChild { ImplementInterfaces }` |
//! | `definition.rs` | report | `union U = A \|` | a constituent with an image | `MissingChild { UnionMemberTypes }` |
//! | `definition.rs` | report | `directive @d on FOO` | nothing — a present token is refused | `MalformedToken { Name }` |
//! | `definition.rs` | report | `directive @d on FIELD \|` | a constituent with an image | `MissingChild { DirectiveLocations }` |
//! | `definition.rs` | report | `enum E { true }` | nothing — a present token is refused | `SemanticRule` |
//! | `definition.rs` | report | `enum E { }` | a constituent with an image | `MissingChild { EnumValuesDefinition }` |
//! | `definition.rs` | unclosed | `enum E { A` | `}` of `EnumValuesDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `schema { foo: Q }` | nothing — a present token is refused | `MalformedToken { Name }` |
//! | `definition.rs` | report | `schema { }` | a constituent with an image | `MissingChild { RootOperationTypesDefinition }` |
//! | `definition.rs` | unclosed | `schema { query: Q` | `}` of `RootOperationTypesDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `type T where A: B` | a constituent with an image | `MissingChild { ObjectTypeDefinition }` |
//! | `definition.rs` | report | `union U where A: B` | a constituent with an image | `MissingChild { UnionTypeDefinition }` |
//! | `definition.rs` | report | `directive @d FIELD` | `on` of `DirectiveDefinition` — **no image** | **projects** |
//! | `definition.rs` | report | `directive @d on` | a constituent with an image | `MissingChild { DirectiveDefinition }` |
//! | `definition.rs` | report | `schema @k` | a constituent with an image | `MissingChild { SchemaDefinition }` |
//! | `extension.rs` | report | `extend scalar S` | a constituent with an image | `MissingChild { ScalarTypeExtension }` |
//! | `extension.rs` | report | `extend type T` | a constituent with an image | `MissingChild { ObjectTypeExtension }` |
//! | `extension.rs` | report | `extend union U` | a constituent with an image | `MissingChild { UnionTypeExtension }` |
//! | `extension.rs` | report | `extend enum E` | a constituent with an image | `MissingChild { EnumTypeExtension }` |
//! | `extension.rs` | report | `extend schema` | a constituent with an image | `MissingChild { SchemaExtension }` |
//! | `value.rs` | report | `type T { f(a: Int = $v): Int }` | nothing — a present token is refused | `UnexpectedChild { DefaultValue, VariableValue }` |
//! | `value.rs` | unclosed | `{ f(a: [1` | `]` of `ListValue` — **no image** | **projects** |
//! | `value.rs` | unclosed | `{ f(a: {b: 1` | `}` of `ObjectValue` — **no image** | **projects** |
//! | `value.rs` | unclosed | `{ f(a: set {1` | `}` of `SetValue` — **no image** | **projects** |
//! | `directive.rs` | unclosed | `{ f(a: 1` | `)` of `Arguments` — **no image** | **projects** |
//! | `ty.rs` | unclosed | `type T { f: A<B` | `>` of `TypeGenerics` — **no image** | **projects** |
//! | `ty.rs` | unclosed | `type T { f: [Int` | `]` of `ListType` — **no image** | **projects** |
//! | `ty.rs` | unclosed | `type T { f: <Int` | `>` of `SetType` — **no image** | **projects** |
//!
//! The rows are what the named cells pin by shape, where a shape has one:
//!
//! | what the parser reports and still builds | the projection | pinned by |
//! |---|---|---|
//! | a present-but-empty required container | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | a variable in a constant position | `UnexpectedChild` | `a_variable_in_a_constant_position_refuses_through_both_new_containers` |
//! | a description in front of an extension or an import | `UnexpectedChild { parent: Document, found: InlineString }` — the string stays **outside** the node, so it is rubble at document level | `a_described_import_refuses_as_rubble` |
//! | **a description in front of a shorthand operation** | `UnexpectedChild { parent: OperationDefinition, found: InlineString }` — here the string is **inside** the node | `a_described_shorthand_refuses_and_its_recovery_is_not_complete` |
//! | an enum value named `true`, `false` or `null` | `SemanticRule` | `an_enum_value_named_true_refuses` |
//! | a fragment named `on` | `SemanticRule` | `a_fragment_named_on_refuses` |
//! | an unknown directive location | `MalformedToken` | `an_unknown_directive_location_refuses` |
//! | a `where` clause with no block to constrain | `MissingChild` | `a_where_clause_with_nothing_to_constrain_refuses` |
//! | a block string as an import source | `UnexpectedChild` | `an_import_source_that_is_a_block_string_refuses` |
//! | **an `as` with no path after it** | refuses — the committing prefix commits | `an_as_without_its_path_refuses_and_its_recovery_is_not_complete` |
//! | **a dangling separator**, in each of the four separated walks | `MissingChild` over the node when nothing follows it; `UnexpectedChild` at the element when a doubled separator or a stranger does | `the_separated_atoms_refuse_at_the_obstruction` |
//! | **an unterminated closer** whose site builds its node hole-free, **an import with no `from`**, **a type condition or a directive definition with no `on`** | **project** — or refuse for another constituent, exactly as the text with the token restored does; they are the missing-token class below | `the_unclosed_brace_class_projects_although_the_parser_rejects_it`, `a_type_condition_without_its_on_projects_what_the_text_with_it_parses_to`, `a_directive_definition_without_its_on_projects_what_the_text_with_it_parses_to`, `every_unclosed_closer_projects_what_the_closed_text_parses_to` |
//! | an unterminated `)` of a variables or arguments definition, or `>` of an executable definition's generics | lenient, and refused all the same — the definition around it is lost and the list is rubble | `every_unclosed_closer_projects_what_the_closed_text_parses_to` |
//!
//! ## The missing-token class: where an absent token still projects
//!
//! **The criterion is the parser-witnessed floor.** A position is lenient iff the lossless parser
//! itself builds a **hole-free** tree for the text with that token missing, *and* the token has no
//! AST image of its own. Both halves matter: the first is why such a shape reaches a projection at
//! all, the second is why nothing is lost by projecting it — only a token is gone, every
//! constituent the AST holds is still there, and the value's shape is one the parser builds for
//! the text with the token restored.
//!
//! A token **with** an image is never lenient. `as` without its `Path` and `&` without its member
//! are al8n/smear#58 round five's two findings, not rows: those bytes would be covered and reach
//! no AST field, which is the whole class this file has been closing.
//!
//! | parent | absent token | witness |
//! |---|---|---|
//! | [`SelectionSet`](SyntaxKind::SelectionSet) | `}` | `selection.rs` unclosed — `{ f` |
//! | [`FieldsDefinition`](SyntaxKind::FieldsDefinition) | `}` | `definition.rs` unclosed — `type T { f: Int` |
//! | [`InputFieldsDefinition`](SyntaxKind::InputFieldsDefinition) | `}` | `definition.rs` unclosed — `input I { f: Int` |
//! | [`EnumValuesDefinition`](SyntaxKind::EnumValuesDefinition) | `}` | `definition.rs` unclosed — `enum E { A` |
//! | [`RootOperationTypesDefinition`](SyntaxKind::RootOperationTypesDefinition) | `}` | `definition.rs` unclosed — `schema { query: Q` |
//! | [`Arguments`](SyntaxKind::Arguments) | `)` | `directive.rs` unclosed — `{ f(a: 1` |
//! | [`ImportDefinition`](SyntaxKind::ImportDefinition) | `from` | `import.rs` report — `import { A } "m"` |
//! | [`TypeCondition`](SyntaxKind::TypeCondition) | `on` | `selection.rs` report — `fragment F T { f }`; Codex round five's first finding |
//! | [`DirectiveDefinition`](SyntaxKind::DirectiveDefinition) | `on` | `definition.rs` report — `directive @d FIELD` |
//! | [`ExtensionTypeGenerics`](SyntaxKind::ExtensionTypeGenerics) | `>` | `generic.rs` unclosed — `extend type T<A`; refuses either way, the extension having no tail |
//! | [`ImportList`](SyntaxKind::ImportList) | `}` | `import.rs` unclosed — `import { A`; refuses either way, the import having no source |
//! | [`DefinitionTypeGenerics`](SyntaxKind::DefinitionTypeGenerics) | `>` | `generic.rs` unclosed — `type T<A` |
//! | [`TypeGenerics`](SyntaxKind::TypeGenerics) | `>` | `ty.rs` unclosed — `type T { f: A<B` |
//! | [`ListType`](SyntaxKind::ListType) | `]` | `ty.rs` unclosed — `type T { f: [Int` |
//! | [`SetType`](SyntaxKind::SetType) | `>` | `ty.rs` unclosed — `type T { f: <Int` |
//! | [`MapType`](SyntaxKind::MapType) | `>` | `ty.rs` unclosed — `type T { f: <Int => Str` |
//! | [`ListValue`](SyntaxKind::ListValue) | `]` | `value.rs` unclosed — `{ f(a: [1` |
//! | [`ObjectValue`](SyntaxKind::ObjectValue) | `}` | `value.rs` unclosed — `{ f(a: {b: 1` |
//! | [`SetValue`](SyntaxKind::SetValue) | `}` | `value.rs` unclosed — `{ f(a: set {1` |
//! | [`MapValue`](SyntaxKind::MapValue) | `}` | `value.rs` unclosed — `{ f(a: map {1 => 2` |
//! | [`VariablesDefinition`](SyntaxKind::VariablesDefinition) | `)` | `executable.rs` unclosed — `query Q($a: Int`; refuses either way, the operation being lost |
//! | [`ArgumentsDefinition`](SyntaxKind::ArgumentsDefinition) | `)` | `definition.rs` unclosed — `type T { f(a: Int`; refuses either way, the definition being lost |
//! | [`ExecutableDefinitionTypeGenerics`](SyntaxKind::ExecutableDefinitionTypeGenerics) | `>` | `generic.rs` unclosed — `fragment <T`; refuses either way, the fragment being lost |
//!
//! **This table is the criterion's whole extension, derived rather than listed.** A position is
//! lenient iff its token has no AST image and the lossless parser builds a hole-free tree holding
//! the node without it; the report-and-build census above enumerates every place the parser leaves
//! a token out without a hole, and its twenty-three image-less rows — each witnessed by its own
//! probe's tree — are these twenty-three, which the cell asserts. Three of them are witnessed only
//! as **orphans**: at end of input the operation, the field or the fragment around the list cannot
//! be finished, so the root keeps the list as a stray child of the document beside the lost
//! definition's tokens, and every projection of such a tree refuses at the rubble. Those three,
//! with an extension's `>` and an import list's `}`, are lenient by the criterion and change no
//! answer. The mutation law's census of deletions both sides refuse is a second instrument, and it
//! agrees: every token it still lists has an image.
//!
//! Every row is also **measured as a value** where a value exists: `tests/lossless_x_mutation.rs`
//! deletes the token from a real tree — or finds a corpus tree already without it — and requires
//! the projection to equal the syntactic parse of the text **with every missing lenient token
//! restored**, innermost first at a shared offset, spans mapped back through the splices. Its
//! per-row counts are asserted, and its own table is asserted equal to this one.
//!
//! **A described shorthand is not in this class**, which is the distinction the round-four table
//! drew and this one keeps: `Described { description: Some(_), node: Shorthand(_) }` is a value the
//! syntactic parser produces for *no* input, because the syntactic side's
//! `refuse_described_shorthand` rejects the combination categorically rather than reporting a token
//! it can do without.
//!
//! **Which of these are *rules* rather than shapes.** The spelling rules are **derived from the
//! syntactic crate**, not remembered: every production under `graphqlx/syntactic/**` that refuses
//! a `Name` or a path by its spelling. The grep is `keyword_of(` over that tree, filtered to the
//! sites whose match *refuses* rather than dispatches — seven sites, four positions:
//!
//! | position | the syntactic refusal | the projection |
//! |---|---|---|
//! | a fragment's name is not `on` | `generic/mod.rs:392` (`Expectation::FragmentName`) | `name_except` in `executable_definition_name` |
//! | a fragment spread's target is not an unqualified `on…` | `selection/mod.rs:446` (the spread dispatch reads `on` as an inline fragment's head) and `:551` (`fragment_type_path`, `Expectation::Path`) | `path_except` through `type_path_except` in `fragment_spread` |
//! | an enum value is not an unqualified `true…`, `false…` or `null…` | `value.rs:288-293` (the enum production, `Expectation::EnumValue`), and the value dispatch that reads those spellings as a boolean or a null in both grammars — `value.rs:781-790` non-const, `:892-901` const | `path_except` in `enum_value`, shared by both value grammars |
//! | an enum value definition is not named `true`, `false` or `null` | `definition/enum_type.rs:21-22` (`Expectation::EnumValue`) | `name_except` in `enum_value_definition` |
//!
//! Each answers [`SemanticRule`](ProjectErrorKind::SemanticRule) at the offending segment, and a
//! path rule reads only the first segment of a path with no leading `::` — `::on`, `ns::on`,
//! `::true` and `x::true` are the parser's too. The two path rules are invisible to the mutation
//! law: a tree the parser builds never holds them, and a retexted one re-parses to a different
//! skeleton. Hand-built cells pin all four. Beside them, two shapes rather than rules:
//!
//! - **A variable has no place in a constant position.** The AST's own type system forbids it —
//!   [`ConstInputValue`] has no `Variable` variant — so it is
//!   [`UnexpectedChild`](ProjectErrorKind::UnexpectedChild) and not a policy this module invented.
//!   GraphQLx reaches it through two carriers GraphQL does not have, a set and a map.
//! - **An import's source must be an inline string.** [`ImportDefinition`](SyntaxKind::ImportDefinition)
//!   holds an [`InlineStringValue`], so a block string there is a shape the AST cannot store, and
//!   the refusal names the token.
//!
//! The fragment-name rule is the draft's `FragmentName : Name but not on`, the same rule the
//! vanilla dialect carries; this dialect accepted the spelling in both suites until
//! al8n/smear#58, which was a **parser defect** repaired at
//! [`executable_definition_name`](crate::graphqlx::syntactic::generic::executable_definition_name),
//! and the projection is a custodian because the lossless production reports and still builds the
//! node.
//!
//! **No other position reserves a spelling.** This dialect's keywords are contextual: the lexer
//! reads `on`, `true`, `null` and `type` as identifiers, and the syntactic parser accepts
//! `type on { x: null }`, `import { true as null } from "m"` and `query on { on: true }` — so every
//! other name position takes any identifier, through the one door `Cursor::name_token`, and the
//! rules above are that door with a reserved list (`Cursor::name_except`, `path_except`).
//! `a_contextual_keyword_is_a_name_at_every_name_position` pins it. al8n/smear#58's round five read
//! 45 mutation-law violations as a rule missing at four more positions; round six measured each
//! and found every one a mutation of a corpus tree that already lacked its `}` or its `from`.
//!
//! # What is walked, and how a span is folded
//!
//! Every function below takes a [`Node`](crate::lossless::project::Node) — a green node plus where
//! it starts — and never a rowan cursor. See
//! [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
//! for why. Every span is the **token extent** of the constituents it covers — never the node's own
//! range, which includes committed trivia — and it is folded **bottom-up, once**: each node
//! function reads its own [`children`](crate::lossless::project::Node::children) a single time,
//! through a `Cursor` in its production's order, covers the ranges of the tokens its atoms
//! consume, and covers the extent each projected child hands back beside its AST value.
//!
//! Two shapes make the fold and the AST value come apart, and both are the tree's honesty rather
//! than a wrinkle here:
//!
//! - **Descriptions hoist.** The wrapper spans description-through-definition while the inner
//!   definition starts *after* the description — **except** in
//!   [`FieldDefinition`](SyntaxKind::FieldDefinition),
//!   [`InputValueDefinition`](SyntaxKind::InputValueDefinition) and
//!   [`EnumValueDefinition`](SyntaxKind::EnumValueDefinition), where the syntactic parser gives the
//!   wrapper and the inner node the *same* span, description included.
//!   [`VariableDefinition`](SyntaxKind::VariableDefinition) — the fourth described node below
//!   document level — follows the
//!   document-level rule instead, so the four do not agree with each other. That asymmetry is
//!   trunk's and is reproduced rather than corrected.
//! - **A written-down empty list is not one thing.** `f()` and `query Q()` are the same bytes in
//!   two positions and the syntactic parser answers differently for them, which is the whole of the
//!   three-way rule below.
//!
//! # Every non-trivia byte is represented, or the walk refuses
//!
//! A byte that is merely *covered* — folded into a span while reaching no AST field — is the shape
//! of guarantee 3's failure, and this file had **seven** ways to produce one. Five were closed in
//! al8n/smear#58's third round — three the review named and two the rule found when it was carried
//! across the file — and two more in the fourth, which are the two the first census could not
//! count because no arm and no token is at fault in either: a shared walker taking the union of
//! its callers' vocabularies, and a one-of slot group read by picking a winner. Each is closed by
//! an **obligation** rather than by an observer wherever a signature could carry it.
//!
//! **What "represented" means, since the rule has to be decidable.** A token is represented when
//! its *kind* is one the shape's own production spells — that is what the vocabularies below do —
//! and, if its *text* would reach an AST field, when it actually reaches one. So a `{`, a `::`
//! between two path segments and a definition's `type` keyword are represented by being named:
//! their text carries nothing the node kind does not already say. A `Name`, a literal image and
//! the `repeatable` in a directive definition are not: their text selects a value, and a walk that
//! folds one without reading it has answered for a sentence it was not given.
//!
//! | the hatch | what it dropped | the obligation now |
//! |---|---|---|
//! | `extent.token(token)` folded any non-trivia token | a `@` in an empty run, a stray `:` | **gone** — a token is consumed only by an atom that names its kind, at its place in the sequence |
//! | `extent.unread(child)` covered a node with no arm — 45 sites, and 72 `is_none()` dispatch guards fell through to one | a second `Path`, a duplicate `Directives`, a whole swallowed definition | **gone**, and so are the guards — a child is consumed by an atom or refused by `end` |
//! | `Names::push` dropped a fourth `Name` | `query a b c d { f }` projecting as `query a b c { f }` | **gone** — a name is one `name_token` per position the production spells |
//! | `leaf_token` took the first token of the leaf's kind and folded the rest | `IntValue` over `Int("1")` and `Int("2")` answering `1` with a span across both | **gone** — a leaf is one token atom and then `end` |
//! | a keyword slot a walk collected and never read | `directive @d foo FIELD` answering what `directive @d on FIELD` answers | each keyword is a `keyword` atom at its own position |
//! | a **shared walker** taking the union of six callers' vocabularies | a `ScalarTypeDefinition` holding a whole `FieldsDefinition`, projected `Ok` with the block dropped | one transcription per kind — see below |
//! | a **one-of slot group** read by picking a winner | an import holding both a list and a wildcard, projected as the list | one `one_of` atom — see below |
//!
//! `every_walk_is_a_transcription` reads this file's own code and asserts the census: no
//! `extent.token(`, no `Names`, no `is_none() =>` guard, no wildcard arm that covers, and two child
//! loops left — the cursor's trivia skip and the recovering door's pass over the root.
//!
//! **No arm survives that covers and drops**, and there is no kind that legitimately has no image:
//! the old doc's claim that "an element with no AST image is not part of the AST, but its bytes are
//! part of the parent's" was the hatch talking. A hole ([`Error`](SyntaxKind::Error),
//! [`Gap`](SyntaxKind::Gap)) is the one element with no image and the doors refuse it before any
//! walk, so a walk never meets one.
//!
//! The fold carries no `TooDeep` of its own: nothing below covers a subtree it has not walked, so
//! no extent is unknowable. The refusal comes from the doors' own verification, and
//! `a_tree_deeper_than_the_ceiling_is_refused_rather_than_descended` pins it there.
//!
//! ## A walk is its production transcribed
//!
//! The three shapes above were each closed by narrowing an *approximation*: a token vocabulary, a
//! per-kind row, a slot guard. Round five showed the approximation cannot be narrowed far enough.
//! A walk that dispatches children into slots by kind can express a shape's **set** of children
//! and cannot express their **sequence** or **multiplicity**, and wherever a production has a
//! committing prefix (`as`, `:`, `=`, `implements`, `where`, `on`) or a separator (`&`, `|`, `=>`,
//! `::`) the same kinds in a different order or count are a different sentence:
//! `import { A as } from "m"` projected an *unaliased* import with `as` inside its extent, and
//! `type T where A:B & { f: Int }` projected the weaker `A:B` with the `&` covered. Each earlier
//! round had found the previous such gap; the next would have found the sixth.
//!
//! So a walk is its production **transcribed**: a sequence of atoms over a `Cursor` in the
//! grammar's own order, ending in `end`. *Represented* stops being a property
//! checked against a table and becomes **consumed by an atom** — and sequence, multiplicity,
//! vocabulary, per-kind rows, one-of exclusivity, committing prefixes and separators stop being
//! rules laid over the walk and become consequences of the transcription. There is deliberately
//! no `token(K::Name)`: a `Name` is consumed by `keyword`,
//! `name_token` or a spelling door, because a vocabulary that admitted
//! `Name` by kind admitted any number of any words —
//! `ScalarTypeDefinition { Name("scalar") DefinitionName(S) Name("junk") }` projected `scalar S`.
//!
//! The two shared walkers are gone with the form: each definition and extension kind is its own
//! transcription, and what they share is a sequence **prefix** rather than a slot struct filled
//! from the union of six vocabularies. `every_definition_kind_refuses_a_foreign_child` and
//! `every_extension_kind_refuses_a_foreign_tail` still hold — a foreign child is simply not in the
//! sequence, so `end` is what refuses it.
//!
//! **What checks the transcription.** A node handed back by an atom and then neither projected nor
//! descended into is the one cover-and-drop this form cannot make impossible, and no table of
//! rules would catch it either. `tests/lossless_x_mutation.rs` is what does: it perturbs every
//! hole-free corpus tree one child at a time and requires the projection to agree with the
//! syntactic parser — with every lenient token restored, where one is missing — or refuse. Every
//! population and bucket it counts is a constant, and the class map of what it still finds is
//! asserted empty.
//!
//! **What it cannot see**, measured: a walk's `end` removed. A surplus child under
//! a small node changes the text enough that the re-parse regroups it, so the case lands in the
//! skeleton-unfaithful bucket, which carries no oracle. `end` is pinned by the named
//! cross-product and hatch cells instead — the law covers parser-shaped trees, the cells cover
//! caller-built ones.
//!
//! ## One-of slot groups
//!
//! Where a production spells a choice, exactly one alternative may be there. Picking a winner from
//! two filled slots drops the loser's subtree while its bytes stay inside the parent's extent, so
//! exclusivity is enforced **while walking**: a choice is one atom, and whichever alternative
//! arrives second is not in the sequence — the next atom, or `end`, refuses it, in either order.
//!
//! | group | the production | the walk | pinned by |
//! |---|---|---|---|
//! | an import's clause | `ImportList \| WildcardSpecifier` | `import_definition` — one `one_of`, so the second alternative is left for `end` | `an_import_with_both_clause_alternatives_refuses_in_either_order` |
//! | an operation's head | `Description? OperationType … \| SelectionSet` | `operation_definition` — the shorthand branch refuses a description, a name, variables, directives and a where clause | `a_described_shorthand_refuses_and_its_recovery_is_not_complete` |
//! | a definition's or an extension's tail | one tail per keyword | structural: no row above holds two tails | the two cross-product cells |
//! | a slot filled from a **kind set** — a type reference under a field, input value or variable definition, the element of a list or set type, a value under an object field | one `Type` / one `Value` | one `one_of` atom; the next atom in the sequence refuses a second | `a_one_of_slot_group_refuses_its_second_member` |
//!
//! ## The same rule one level down: leaf text
//!
//! A token whose text reaches the AST **as a value** is re-cooked through the lexer's own
//! whole-slice door, because on a caller-minted tree the bytes under a token are whatever the
//! caller wrote and the kind label is not evidence. A token whose text only contributes a *range*
//! — every keyword and every piece of punctuation in a walk's own vocabulary — is **not** re-cooked:
//! its arm already named its kind, and its bytes reach no AST field.
//!
//! | leaf | becomes | door |
//! |---|---|---|
//! | [`Int`](SyntaxKind::Int) | [`LitInt`] | `LitInt::try_from`, the shipped scanner, whole-slice |
//! | [`Float`](SyntaxKind::Float) | [`LitFloat`] | `LitFloat::try_from`, likewise |
//! | [`InlineString`](SyntaxKind::InlineString), [`BlockString`](SyntaxKind::BlockString) | [`LitStr`] | `LitStr::try_from`, the string sub-lexer — it is what decides `Plain` vs `Complex` and the `required_capacity`, so a malformed image (a slice without its quotes, a bad escape) fails there |
//! | [`Name`](SyntaxKind::Name) whose text becomes a name | `Name<&str>` | [`identifier`], added beside [`LitInt`]'s for this — every name position reaches it through `Cursor::name_token` |
//! | a `Name` read for its **spelling** | an operation type, a directive location, `true`/`false`/`null` | `contextual_keyword` — the lexer's own table — and every reader of it refuses a spelling it does not classify |
//!
//! The last row is where the fifth hole was: [`BooleanValue`](SyntaxKind::BooleanValue) always
//! compared its text and [`NullValue`](SyntaxKind::NullValue) did not, so a `NullValue` node over
//! any identifier projected to a `null` carrying that identifier's bytes. It classifies now.
//!
//! # Empty containers: the three-way rule
//!
//! A container the tree opens can be empty, and what the AST does about that is decided by the
//! **syntactic production**, not by the node's presence. There are exactly three answers, and
//! nothing here may invent a fourth:
//!
//! | the grammar | the tree | the AST | this projection |
//! |---|---|---|---|
//! | `X+` inside delimiters | node present, no members | *no value* — the parser rejects the document | [`MissingChild`](ProjectErrorKind::MissingChild) |
//! | `X*` inside delimiters, mapped to `None` when empty | node present, no members | `None` | `None`, with the node's extent still covered |
//! | optional, undelimited | no node | `None` | `None`, nothing covered |
//!
//! **Why the first row is a refusal and not an empty carrier.** The design's contract is that the
//! projection succeeds iff the tree's shape determines a well-formed AST, and that it never
//! produces a value outside the syntactic parser's image — placeholder ASTs are rejected there in
//! so many words. A `SelectionSet` with no selections, a `FieldsDefinition` with no fields, an
//! `ImportList` with no members: the syntactic parser cannot produce any of those values for any
//! input, so projecting to one is exactly the forbidden case. It is **not** the unclosed-brace
//! case, which projects precisely because its image — a definition with the one field it does have
//! — *is* a value the parser produces, for the closed text.
//!
//! ## The list, derived twice
//!
//! Once from the **productions** — an `at_least(1)` under a delimiter, a
//! `collect_with(Vec::from([first]))` over a separated tail, a mandatory first element followed by
//! a `while` — and once from the **AST**: every container this file constructs from a `Vec`, which
//! is every `let mut … = Vec::new()` accumulator and the four a value grammar builds. What follows
//! is the **union**, and the two derivations differ by exactly two members: `Directives` and
//! `ConstDirectives` are the third spelling above and the production-side reading missed them,
//! which is the defect al8n/smear#58's third round closed. Every row is measured — the *this
//! projection* column is what the code answers, not what it intends to.
//!
//! | AST container | grammar | present-empty | pinned by |
//! |---|---|---|---|
//! | [`Document`](SyntaxKind::Document), [`TypeSystemDocument`](SyntaxKind::TypeSystemDocument), [`ExecutableDocument`](SyntaxKind::ExecutableDocument) | `Entry+` | `MissingChild` | kind-level only |
//! | [`ImportList`](SyntaxKind::ImportList) | `Member+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`DefinitionTypeGenerics`](SyntaxKind::DefinitionTypeGenerics) | `Param+` in `< >` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`ExtensionTypeGenerics`](SyntaxKind::ExtensionTypeGenerics) | `Name+` in `< >` | `MissingChild` | kind-level only |
//! | [`ExecutableDefinitionTypeGenerics`](SyntaxKind::ExecutableDefinitionTypeGenerics) | `Name+` in `< >` | `MissingChild` | kind-level only |
//! | [`TypeGenerics`](SyntaxKind::TypeGenerics), both spellings | `Type+` in `< >` | `MissingChild` | kind-level only |
//! | [`WhereClause`](SyntaxKind::WhereClause) | `Predicate+` | `MissingChild` | `a_where_clause_with_nothing_to_constrain_refuses` |
//! | a predicate's bounds | `TypePath+` after `:` | `MissingChild` | kind-level only |
//! | [`Path`](SyntaxKind::Path) | `Name+` separated by `::` | `MissingChild` | kind-level only |
//! | [`Directives`](SyntaxKind::Directives), [`ConstDirectives`](SyntaxKind::Directives) | `Directive+` | `MissingChild` | `a_present_directive_run_with_no_directive_refuses` |
//! | [`SelectionSet`](SyntaxKind::SelectionSet) | `Selection+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`VariablesDefinition`](SyntaxKind::VariablesDefinition) | `Definition+` in `( )` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`ImplementInterfaces`](SyntaxKind::ImplementInterfaces) | `TypePath+` after `implements` | `MissingChild` | kind-level only |
//! | [`UnionMemberTypes`](SyntaxKind::UnionMemberTypes) | `TypePath+` after `=` | `MissingChild` | kind-level only |
//! | [`FieldsDefinition`](SyntaxKind::FieldsDefinition) | `Field+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`ArgumentsDefinition`](SyntaxKind::ArgumentsDefinition) | `Definition+` in `( )` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`InputFieldsDefinition`](SyntaxKind::InputFieldsDefinition) | `Definition+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`EnumValuesDefinition`](SyntaxKind::EnumValuesDefinition) | `Value+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`DirectiveLocations`](SyntaxKind::DirectiveLocations) | `Name+` separated by `\|` | `MissingChild` | kind-level only |
//! | [`RootOperationTypesDefinition`](SyntaxKind::RootOperationTypesDefinition) | `Root+` in `{ }` | `MissingChild` | `a_present_but_empty_required_container_refuses` |
//! | [`Arguments`](SyntaxKind::Arguments), [`ConstArguments`](SyntaxKind::Arguments) | `Argument*` in `( )` | `None`, extent covered | `a_written_down_empty_argument_list_is_none_with_a_span` |
//! | a list, set, map or object **value** | `Value*` in its delimiters | an empty container value | `the_projection_equals_the_parse_over_the_shared_corpus` |
//!
//! *kind-level only* means the shape has no cell of its own: the emptiness path is reached by no
//! corpus entry and by no pin, and what stands behind it is
//! `every_refusal_kind_has_a_witness`, which requires the `MissingChild` **kind** to be produced
//! by some input rather than by this one. Of the twenty row-one entries, nine share one cell, two
//! have a cell of their own and nine are kind-level — stated so a later reader does not read the
//! column as uniform.
//!
//! The last row is not a fourth answer to the same question: those four are not optional
//! constituents at all, they are values, and `[]` is as much a value as `[1]`.
//!
//! **Row two, measured rather than asserted.** `Arguments` stays in it only because the syntactic
//! parser really does answer `None` for a written-down empty list. Over `query Q { f() }` the
//! parse's sole field answers `arguments().is_none() == true`, and over `type T @d() { f: Int }`
//! so does the directive — run through a scratch `--test` binary against
//! `smear::parser::graphqlx::syntactic::{document, executable_document}`, which is the same door
//! `the_projection_equals_the_parse_over_the_shared_corpus` compares against.
//!
//! **The one container that may be empty and is not in the table.** The recovering doors'
//! accumulator: [`project_executable_document_recovered`] and its twin answer a document with no
//! entries and a zero-width span at the container's start when every entry was skipped. That is
//! the recovery contract — [`Recovery::skipped`] is the bound on what was lost — rather than a
//! cardinality claim, and the two are told apart by which door the caller went through.
//!
//! **The measurement behind the first rows.** Over each of the nine delimited shapes the lossless
//! parser reports a diagnostic and the syntactic parser rejects, so `Parse::has_errors()` and the
//! syntactic verdict agree and gate 1 is intact — but the tree carries **no `Error` child**, which
//! is why a walk that only refuses holes let every one of them through.
//!
//! # No node dispatch below spends a native frame per level
//!
//! The grammar is bounded above a value, and **three cycles are not**: `value` ↔
//! `object_field`/`map_entry`, `selection_set` ↔ `field`/`inline_fragment`, and `ty` ↔ a list's,
//! a set's and a map's element and a path's generic arguments. Each is a **worklist**, as in
//! `graphql/lossless/project.rs`. al8n/smear#201.
//!
//! Three cycles rather than GraphQL's four, and it is not a smaller surface: the const and
//! non-const value walks are one machine parameterised by a trait here as they are there, and what
//! GraphQL splits off as a `NonNullType` wrapper is a `!` token of the type it modifies here, so
//! the type cycle absorbs it. What the type cycle gains instead is a *branching* level GraphQL's
//! does not have — a path's generic arguments are types, so `A<B<C<…>>>` nests without a bracket
//! — and a map's key and value are two subtrees under one node, which is the shape the frame's
//! open slot exists for.
//!
//! `smear-parser/tests/deep_projection_x.rs` reads the flatness off a real projection, one fixture
//! per cycle plus one per GraphQLx-only carrier.

use std::vec::Vec;

use rowan::{NodeOrToken, TextRange, TextSize};
use tokora::SimpleSpan;

use smear_lexer::{
  LitStr,
  graphqlx::{ContextualKeyword, LitFloat, LitInt, identifier, keyword::contextual_keyword},
};

use crate::{
  graphqlx::{
    ast::{
      Alias, Argument, Arguments, ArgumentsDefinition, BooleanValue, ConstArgument, ConstArguments,
      ConstDirective, ConstDirectives, ConstInputValue, ConstList, ConstMap, ConstMapEntry,
      ConstObject, ConstObjectField, ConstSet, DefaultInputValue, DefinitionName,
      DefinitionTypeGenerics, DefinitionTypeParam, DefinitionTypePath, Described,
      DescribedVariableDefinition, Directive, DirectiveLocations, Directives, Document,
      EnumTypeDefinition, EnumTypeExtension, EnumValue, EnumValuesDefinition, ExecutableDefinition,
      ExecutableDefinitionHeader, ExecutableDefinitionName, ExecutableDefinitionTypeGenerics,
      ExecutableDocument, ExtensionName, ExtensionTypeGenerics, ExtensionTypeParam, Field,
      FieldsDefinition, FloatValue, FragmentSpread, ImplementInterfaces,
      ImportOrDefinitionOrExtension, ImportOrExecutableDefinition,
      ImportOrTypeSystemDefinitionOrExtension, InlineFragment, InlineStringValue,
      InputFieldsDefinition, InputObjectTypeDefinition, InputObjectTypeExtension, InputValue,
      IntValue, InterfaceTypeDefinition, InterfaceTypeExtension, List, Map, MapEntry, Name,
      NamedOperationDefinition, Nest, Nested, NullValue, Object, ObjectField, ObjectTypeDefinition,
      ObjectTypeExtension, OperationDefinition, OperationType, Path, RootOperationTypeDefinition,
      RootOperationTypesDefinition, ScalarTypeDefinition, ScalarTypeExtension, SchemaDefinition,
      SchemaExtension, Selection, SelectionSet, Set, StringValue, Type, TypeCondition,
      TypeDefinition, TypeExtension, TypeGenerics, TypePath, TypeSystemDefinition,
      TypeSystemDocument, TypeSystemExtension, UnionMemberTypes, UnionTypeDefinition,
      UnionTypeExtension, VariableDefinition, VariableValue, VariablesDefinition, WhereClause,
      WherePredicate,
    },
    kinds::{GraphQLxLang, SyntaxKind},
    lossless::Parse,
    syntactic::definition::classify_location,
  },
  lossless::project::{
    Recovery, Unverified, reject_foreign_kinds_and_holes, to_range, to_span, verify_root_kind,
    verify_source, verify_source_at, verify_source_counted,
    walk::{
      Extent, Leading, Optional, Trivia, described_extents, missing, unexpected, unexpected_token,
    },
  },
};

// The three roots below are spelled out rather than folded into the group above, so
// `tests/lossless_isolation.rs`'s source census — which reads `crate::<segment>` out of the text —
// can see each edge. A grouped `use crate::{…}` spells `crate::{`, which that census cannot read,
// and `imports_granularity = "Crate"` re-folds any two statements a comment does not separate — so
// the comments between them are load-bearing rather than decorative.
//
// All three hold **shared, dialect-free** AST carriers with no spelling under `graphqlx::ast`, and
// a projection has to construct every one of them.

// The undescribed cores five `Described<…>` aliases wrap, and the six `…Data` enums an extension's
// alternatives are encoded in.
use crate::type_system::{
  DirectiveDefinition, EnumTypeExtensionData, EnumValueDefinition, FieldDefinition,
  InputObjectTypeExtensionData, InputValueDefinition, InterfaceTypeExtensionData,
  ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

// The target six definitions and extensions hold their `where` clause with.
use crate::generic::Constrained;

// The three type carriers this dialect's `Type` stands behind a `Nest`. The enum names its
// pointees inline rather than aliasing them, so the projection has to as well.
use crate::ty::{ListType, MapType, SetType};

use SyntaxKind as K;

/// A refusal from the GraphQLx projection, keyed by this dialect's [`SyntaxKind`].
pub type ProjectError = crate::lossless::project::ProjectError<SyntaxKind>;

/// Why the GraphQLx projection refused, keyed by this dialect's [`SyntaxKind`].
pub type ProjectErrorKind = crate::lossless::project::ProjectErrorKind<SyntaxKind>;

/// A green node and where it starts, in this dialect's kind space.
///
/// The unit every function below walks. See
/// [the substrate](crate::lossless::project#what-a-projection-walks-the-green-tree-not-a-cursor)
/// for why the traversal is green and what a cursor would have cost.
type Node<'g> = crate::lossless::project::Node<'g, GraphQLxLang>;

/// [`Node`]'s other half.
type Token<'g> = crate::lossless::project::Token<'g, GraphQLxLang>;

/// A node's child sequence, read in its production's order — the substrate's cursor over this
/// dialect's kind space. See the substrate's `walk` module for the atoms and why they are shared;
/// [`Atoms`] is this dialect's half.
type Cursor<'g> = crate::lossless::project::walk::Cursor<'g, GraphQLxLang>;

type Out<T> = Result<T, ProjectError>;

/// A described definition's three answers, folded from the node's single walk.
///
/// The hoisted description, the definition itself — whose own span is the inner extent, already
/// built into it — and the node's full token extent, which is both the [`Described`] wrapper's span
/// and what the enclosing document covers.
type Definition<'src, T> = (Option<StringValue<&'src str>>, T, TextRange);

/// The reason a [`ProjectError`] from a whole-root verification names.
///
/// The three-line `match` the GraphQL dialect's own header predicted this dialect would write:
/// over *this* `SyntaxKind`, with *this* `ProjectErrorKind`, rather than a shared generic one in
/// the substrate whose liveness would depend on which dialects are compiled.
///
/// A free function and not a second inherent method of that name on [`Unverified`], which is the
/// one shape the prediction could not take: the type is the substrate's, both dialects would be
/// adding an inherent `of` to it, and `rustc` refuses the pair outright (`E0592`) in any build that
/// compiles both. What the header sanctions duplicating is the `match`, not the name.
fn unverified(error: &ProjectError) -> Unverified {
  match error.kind() {
    ProjectErrorKind::TooDeep { limit } => Unverified::TooDeep { limit: *limit },
    ProjectErrorKind::WrongRoot { raw } => Unverified::WrongRoot { raw: *raw },
    _ => Unverified::SourceMismatch,
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
  /// claim that the walk would reach any hole inside the subtree anyway — and that claim is false,
  /// because the walk's permissive arms route a child they have no slot for into the parent's
  /// *extent*: its bytes are folded and its kind is never looked at.
  /// A `MapType` with an `Error` child after its two halves is the shortest witness. al8n/smear#58.
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
/// [`parse_executable_document`](super::parse_executable_document) builds. Everything else — the
/// hole scan, the verified `(tree, source)` pair, the token-extent span rule — is [`project`]'s,
/// unchanged.
///
/// The root matters. A mixed parse holds a [`Document`](SyntaxKind::Document) node, so it is
/// refused here rather than filtered: dropping the type-system half of a mixed document would
/// answer a different question from the one the executable root asks.
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::graphqlx::lossless::{parse_executable_document, project_executable_document};
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

/// Project every entry of a lossless **executable** parse that has an AST image, and count the
/// ones that do not.
///
/// [`project_executable_document`] is fail-fast: one hole anywhere and the whole document is
/// refused.
///
/// This door walks the top level instead, projects each entry **independently**, and keeps the
/// ones that succeeded. What it could see is the [`Recovery`].
///
/// Every container of the root's kind is stepped through, not only the first: a caller-minted root
/// holding two valid containers projects the entries of both and reports complete, where
/// [`project_executable_document`], which asserts exactly one container, refuses the second. Each
/// container is a legitimate document image and this door's contract is per entry — see
/// [`Recovery`].
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::graphqlx::lossless::{
///   parse_executable_document, project_executable_document_recovered,
/// };
///
/// let source = "{ hero { name } }\nquery Bad(";
/// let parse = parse_executable_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_executable_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
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
/// there is no error half left.
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
/// [`parse_type_system_document`](super::parse_type_system_document) builds. The root matters, for
/// [`project_executable_document`]'s reason mirrored.
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::graphqlx::lossless::{
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

/// Project every entry of a lossless **type-system** parse that has an AST image, and count the
/// ones that do not.
///
/// [`project_executable_document_recovered`]'s mirror at the SDL root, walking the same top level
/// with the same accounting.
///
/// ```
/// # #[cfg(all(feature = "graphqlx", feature = "rowan"))] {
/// use smear_parser::graphqlx::lossless::{
///   parse_type_system_document, project_type_system_document_recovered,
/// };
///
/// let source = "type Query { hero: String }\ntype Half { f: }";
/// let parse = parse_type_system_document(source);
/// assert!(parse.has_errors());
///
/// let (ast, recovery) =
///   project_type_system_document_recovered(&parse, source).expect("one document");
/// assert_eq!(ast.definitions().len(), 1);
/// assert_eq!(recovery.projected(), 1);
/// assert!(!recovery.is_complete());
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
/// verification, so they have no error half. al8n/smear#198.
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
  /// `push`, bounded by [`MAX_GREEN_DEPTH`](crate::lossless::project::MAX_GREEN_DEPTH). A chain of
  /// single-child nodes holds one entry however long it is.
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
      Err(refusal) => Err(unverified(&refusal)),
    }
  }

  /// What projecting this pair costs, in **elements** — one per green node and one per token.
  ///
  /// `Verified` proves the *bytes* agree, and bytes do not bound structure:
  /// [`finish_root`](crate::lossless::runner::finish_root) is public, so a caller can mint a
  /// `Parse` from its own CST event stream, and a balanced pair of **zero-width** nodes adds
  /// structure without adding a byte. This count is taken by the same walk that verified the pair.
  /// It saturates at [`u32::MAX`]: no finite validation budget covers that cost, and a disabled
  /// ledger or a projection that takes no budget proceeds. al8n/smear#198.
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
/// is visited and every token's bytes are compared. It reads no `Parse` state beyond a borrow, and
/// it allocates nothing through sixteen branching ancestors, on the terms [`Verified::new`] states.
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
    .map_err(|refusal| unverified(&refusal))
}

/// The recovering top-level walk, shared by both single-half roots.
///
/// One implementation rather than one per root, because what it computes is [`Recovery`] and both
/// doors report it. `entry_of` is a `fn` pointer.
///
/// # Every element of the root, not every element of the document node
///
/// The walk starts at the **root** and steps *through* the document node rather than starting
/// inside it. The two are not the same population: the parser can leave a gap tile beside the
/// document node instead of within it.
///
/// **Every** container of `root_kind` under the root is stepped through, not only the first. The
/// dialect's own doors build one; a caller-minted root can hold two, and this walk projects the
/// entries of both and reports complete. That is deliberate: each container is a legitimate
/// document image, and this walk's contract is per entry — [`Recovery`] counts what had an AST
/// image and what did not, and a second container's entries have one. Whether the root is exactly
/// one document is the fail-fast doors' question, and [`sole_document`] answers it there.
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
    return Err(unverified(&refusal));
  }
  Ok(recovered_top_level_verified(
    parse, root_kind, entry_of, source,
  ))
}

/// [`recovered_top_level`] for a pair whose verification is already established.
///
/// Infallible: it runs no verification. Its callers are [`recovered_top_level`], after its own, and
/// the `_verified` doors, whose [`Verified`] carries one.
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
  // The document's own span is the extent of the tokens under the entries that **survived**, not
  // of the bytes that were dropped: an AST span is an extent of the tokens its node covers, and a
  // skipped region is not one of them.
  let mut extent = Extent::default();
  let mut take = |element: NodeOrToken<Node<'_>, Token<'_>>| match element {
    // Rubble the parser could not attach to an entry. Counted per token rather than per run: a
    // bound on what was lost, which is what `Recovery::skipped` promises.
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
      // The document node is stepped *through*: its children are the entries. Everything else
      // under the root is a top-level element in its own right.
      NodeOrToken::Node(child) if child.green().kind() == raw_of(root_kind) => {
        child.children().for_each(&mut take)
      }
      other => take(other),
    }
  }

  // With nothing projected there is no extent, and the zero-width span at the container's start is
  // the only position that is not a claim about text no node holds.
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
/// else. A duplicate container is an unexpected sibling like any other — the second one is a region
/// with no image in the AST the first one produces. The recovering doors do not share this
/// assertion — see [`recovered_top_level`] for why. al8n/smear#58.
fn sole_document<'g>(root: Node<'g>, kind: SyntaxKind, wanted: &'static str) -> Out<Node<'g>> {
  // `Trivia* Container Trivia*`, transcribed like every walk below.
  let mut cursor = Cursor::new(root);
  let container = cursor.node(kind, wanted)?;
  cursor.end()?;
  Ok(container)
}

/// One top-level entry, with the holes in **its own** subtree refused.
///
/// The scan is scoped to the entry, so a hole is charged to the entry that holds it and to no
/// other.
///
/// # It compares no bytes
///
/// **Both** recovering paths establish the pair over the whole root before the first entry is
/// reached — [`recovered_top_level`] with [`verify_source`], and
/// [`project_executable_document_verified`] through the [`Verified`] it is handed. al8n/smear#58.
fn recoverable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ImportOrExecutableDefinition<&'src str>, TextRange)> {
  scan_holes(node)?;
  executable_entry(node, source)
}

/// [`recoverable_entry`]'s twin at the SDL root, scoped for the same reason and comparing no bytes
/// for the same one.
fn recoverable_type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  ImportOrTypeSystemDefinitionOrExtension<&'src str>,
  TextRange,
)> {
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
/// which nodes a particular walk happens to descend into.
fn scan_holes(node: Node<'_>) -> Out<()> {
  // Both images, and **both are live**. `Gap` is a token kind, which is why the substrate's scan
  // is asked of every element rather than of every node: a scan that tested nodes only walked past
  // every gap tile, and the projection then folded the bytes it covers into an enclosing extent as
  // an ordinary non-trivia token — a hole the preflight had declared absent.
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
  <GraphQLxLang as rowan::Language>::kind_to_raw(kind)
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

/// The two token images a description can be written as.
const fn is_string_image(kind: SyntaxKind) -> bool {
  matches!(kind, K::InlineString | K::BlockString)
}

impl Trivia for GraphQLxLang {
  #[inline]
  fn is_trivia(kind: SyntaxKind) -> bool {
    is_trivia(kind)
  }
}

/// This dialect's half of the cursor: the atoms that need its lexer or its keyword table.
///
/// The substrate's cursor owns every atom that takes a kind as a parameter; what is left here is
/// what reading a `Name` means in this dialect — a keyword by its spelling, a name through
/// [`identifier`], a spelling a caller classifies — and where a description sits. A trait rather
/// than free functions so every walk below reads as the one sequence of calls it was before the
/// hoist, and a trait rather than inherent methods because the cursor is the substrate's type.
///
/// **There is no `token(K::Name)`**, and that is the point: ten vocabularies at al8n/smear#58's
/// fourth round HEAD folded keyword names by kind alone and unbounded in count, so
/// `ScalarTypeDefinition { Name("scalar") DefinitionName(S) Name("junk") }` projected `scalar S`
/// with `junk` covered.
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
  fn opt_description(&mut self) -> Option<Token<'g>>;
}

impl<'g> Atoms<'g> for Cursor<'g> {
  /// A `Name` token spelling `keyword`, through the lexer's own table.
  ///
  /// **There is no `token(K::Name)`**, and that is the point: ten vocabularies at the fourth
  /// round's HEAD folded keyword names by kind alone and unbounded in count, so
  /// `ScalarTypeDefinition { Name("scalar") DefinitionName(S) Name("junk") }` projected `scalar S`
  /// with `junk` covered.
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
  /// Two positions have one — a fragment's name (`FragmentName : Name but not on`) and an enum
  /// value's declaring name (`true`, `false`, `null`). Everywhere else this dialect's keywords are
  /// contextual and every one of them is a name: the lexer reads `on`, `true` and `type` as
  /// identifiers, and the syntactic parser accepts `type on { x: null }`, which is why no other
  /// position refuses a spelling.
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

  /// [`name_token`](Self::name_token) where the production makes the name optional — or repeats
  /// it, as the two bare-name generic lists and a path's segments do.
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

  /// The description a definition may open with, **held back rather than folded**.
  ///
  /// This kind space has no `Description` node — one token is not a region — so the hoist is a
  /// token the walk keeps out of its own extent and hands to [`described_extents`]: the wrapper's
  /// span covers it and the definition's starts after it. The three node types whose wrapper and
  /// inner span agree fold it themselves.
  fn opt_description(&mut self) -> Option<Token<'g>> {
    match self.peek() {
      Some(NodeOrToken::Token(token)) if is_string_image(token.kind()) => {
        self.bump();
        Some(token)
      }
      _ => None,
    }
  }
}

/// The source text under `token`.
///
/// Bounds-checked, not compared: the door already verified every byte of the tree against `source`,
/// so a token's range is in bounds and on a character boundary by construction. The refusal below
/// is that invariant's receipt rather than a second check.
#[inline]
fn slice<'src>(source: &'src str, token: Token<'_>) -> Out<&'src str> {
  let range = token.text_range();
  source
    .get(usize::from(range.start())..usize::from(range.end()))
    .ok_or_else(|| ProjectError::new(ProjectErrorKind::SourceMismatch, to_range(range)))
}

/// Re-cook a name through the **same** door the lexer's identifiers come from.
///
/// [`identifier`] is the shipped scanner, and it must answer the whole slice. The leaf table in
/// the module header says why this is a door rather than a `slice`: the range comes from a token
/// the *tree* labelled `Name`, so on a caller-minted tree the bytes are whatever the caller wrote,
/// and `Name("1")` is a value the syntactic parser has no way to produce. A refusal here means the
/// tree labelled something a name scan will not read back.
fn name<'src>(source: &'src str, token: Token<'_>) -> Out<Name<&'src str>> {
  let text = slice(source, token)?;
  identifier(text).map_err(|_| {
    ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Name },
      to_range(token.text_range()),
    )
  })?;
  Ok(Name::new(to_span(token.text_range()), text))
}

/// The keyword a `Name` token spells, classified through the lexer's own table.
fn keyword_of(token: Token<'_>) -> Option<ContextualKeyword> {
  contextual_keyword(token.text().as_bytes())
}

/// Re-cook a string literal through the **same** door the lexer's payload comes from.
///
/// [`LitStr`]'s `TryFrom<&str>` is the string lexer, so the `Plain`/`Complex` discriminant and the
/// `required_capacity` a consumer allocates against are the lexer's answers rather than a second
/// implementation of the escape rules. A refusal here means the token's text is not a string
/// literal to that lexer — a caller-minted label, since a parse's string tokens come from it.
fn string_lit<'src>(token: Token<'_>, source: &'src str) -> Out<LitStr<&'src str>> {
  let slice = slice(source, token)?;
  LitStr::try_from(slice).map_err(|_| {
    ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: token.kind() },
      to_range(token.text_range()),
    )
  })
}

fn string_value<'src>(token: Token<'_>, source: &'src str) -> Out<StringValue<&'src str>> {
  Ok(StringValue::new(
    to_span(token.text_range()),
    string_lit(token, source)?,
  ))
}

/// An import's source, which the grammar narrows to an **inline** string.
///
/// [`ImportDefinition`](crate::graphqlx::ast::ImportDefinition) stores an [`InlineStringValue`], so a
/// block string here is a shape the AST cannot hold rather than a value it holds differently — the
/// lossless production reports it and builds the [`StringValue`](SyntaxKind::StringValue) node
/// anyway, which is why the narrowing has to be re-checked. The classification is the same
/// [`LitStr`] door every other literal goes through, read for which half it answered.
fn inline_string_value<'src>(
  parent: Node<'_>,
  token: Token<'_>,
  source: &'src str,
) -> Out<InlineStringValue<&'src str>> {
  match string_lit(token, source)? {
    LitStr::Inline(lit) => Ok(InlineStringValue::new(to_span(token.text_range()), lit)),
    LitStr::Block(_) => Err(unexpected_token(parent, token)),
  }
}

/// An integer literal's payload, re-scanned by the lexer that produced it.
///
/// # It used to be a second custodian, and now it is not
///
/// Every other literal this projection rebuilds goes through the lexer's own entry, so the payload
/// is the lexer's answer rather than a second implementation. [`LitInt`] and [`LitFloat`] had no
/// such entry: the tree keeps one `Int` image over all four radices and one `Float` over both, the
/// number sub-lexer's `logos` grammars are private, and this function classified by inspecting the
/// slice's prefix. Total over what the lexer accepts, and silent about everything else — `0b2`,
/// `0x` and a `Float`-labelled `0x1` all produced a literal, because a prefix test asks a narrower
/// question than the grammar does.
///
/// `smear-lexer` grew the door on al8n/smear#58 and it **is** the scanner: `LitInt::try_from`
/// requires the slice to scan to exactly one whole integer literal and answers the scanner's own
/// error otherwise. So the exception is retired and the projection re-implements no number
/// grammar. A refusal here means the tree labelled `Int` a slice the scanner will not read back as
/// one — [`MalformedToken`](ProjectErrorKind::MalformedToken), reachable from a caller-minted tree
/// and never from a parse, whose number tokens come from that scanner.
fn int_lit(text: &str, span: SimpleSpan) -> Out<LitInt<&str>> {
  LitInt::try_from(text).map_err(|_| {
    ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Int },
      span.start()..span.end(),
    )
  })
}

/// A float literal's payload, re-scanned by the lexer that produced it — [`int_lit`]'s twin, and
/// see it for why the classification is the scanner's rather than this module's.
///
/// The two doors do not coerce into each other: `LitFloat::try_from("0x1")` refuses rather than
/// widening an integer, which is what makes a `Float`-labelled integer token a refusal here
/// instead of a wrongly classified AST literal.
fn float_lit(text: &str, span: SimpleSpan) -> Out<LitFloat<&str>> {
  LitFloat::try_from(text).map_err(|_| {
    ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Float },
      span.start()..span.end(),
    )
  })
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

/// What the mixed root's entry run holds: an import, either executable definition, a type-system
/// definition or an extension.
const MIXED_ENTRY_KINDS: [SyntaxKind; 18] = entry_kinds::<18>(true, true);

/// What the SDL root's entry run holds — the mixed run without the two executable kinds.
const TYPE_SYSTEM_ENTRY_KINDS: [SyntaxKind; 16] = entry_kinds::<16>(false, true);

/// What the executable root's entry run holds — an import or one of the two executable kinds.
const EXECUTABLE_ENTRY_KINDS: [SyntaxKind; 3] = entry_kinds::<3>(true, false);

/// One entry-kind row, assembled from the lists above so the three roots cannot disagree about a
/// kind they share. `N` is checked by the assembly itself: a count that does not match the chosen
/// halves fails const evaluation.
const fn entry_kinds<const N: usize>(executable: bool, type_system: bool) -> [SyntaxKind; N] {
  let mut kinds = [K::ImportDefinition; N];
  let mut at = 1;
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
  // `ImportOrDefinitionOrExtension+`, in source order: neither this root nor the syntactic
  // `document` orders an import against a definition. A token here is rubble — the lost-node
  // recovery class drops a failed definition's bytes straight under the document, and so does
  // the description of a described import or extension, a string this grammar gives no slot —
  // so the run stops at it and `end` refuses it where it stands.
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
) -> Out<(ImportOrDefinitionOrExtension<&'src str>, TextRange)> {
  if node.kind() == K::ImportDefinition {
    let (import, extent) = import_definition(node, source)?;
    return Ok((ImportOrDefinitionOrExtension::Import(import), extent));
  }
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((ImportOrDefinitionOrExtension::Extension(extension), extent));
  }
  let (description, definition, outer) = definition(node, source)?;
  Ok((
    ImportOrDefinitionOrExtension::Definition(Described::new(
      to_span(outer),
      description,
      definition,
    )),
    outer,
  ))
}

/// [`document`]'s SDL-only twin, over the `ImportOrTypeSystemDefinitionOrExtension+` root.
fn type_system_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<TypeSystemDocument<&'src str>> {
  // `ImportOrTypeSystemDefinitionOrExtension+` — rubble exactly as at the mixed root, and an
  // executable definition is not in this root's run at all, so `end` refuses it at its own range.
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
/// The import and extension arms stay — both are type-system syntax and this root builds them —
/// and what goes is the executable half: an `OperationDefinition` or a `FragmentDefinition` under
/// this root has no image in a `TypeSystemDocument`, and the SDL root reports one at the parser's
/// own position rather than shaping it, so reaching that arm means the tree is not the one this
/// door was handed.
fn type_system_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  ImportOrTypeSystemDefinitionOrExtension<&'src str>,
  TextRange,
)> {
  if node.kind() == K::ImportDefinition {
    let (import, extent) = import_definition(node, source)?;
    return Ok((
      ImportOrTypeSystemDefinitionOrExtension::Import(import),
      extent,
    ));
  }
  if let Some((extension, extent)) = type_system_extension(node, source)? {
    return Ok((
      ImportOrTypeSystemDefinitionOrExtension::Extension(extension),
      extent,
    ));
  }
  let (description, definition, outer) = type_system_definition(node, source)?;
  Ok((
    ImportOrTypeSystemDefinitionOrExtension::Definition(Described::new(
      to_span(outer),
      description,
      definition,
    )),
    outer,
  ))
}

/// [`document`]'s executable-only twin, over the `ImportOrExecutableDefinition+` root.
fn executable_document<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<ExecutableDocument<&'src str>> {
  // `ImportOrExecutableDefinition+` — the SDL root's mirror.
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
/// The import arm stays — this dialect's executable root admits imports, which is the one entry
/// kind GraphQL's twin has no counterpart for — and the extension arm goes, `extend` not being
/// executable syntax.
fn executable_entry<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ImportOrExecutableDefinition<&'src str>, TextRange)> {
  if node.kind() == K::ImportDefinition {
    let (import, extent) = import_definition(node, source)?;
    return Ok((ImportOrExecutableDefinition::Import(import), extent));
  }
  let (description, definition, outer) = executable_definition(node, source)?;
  Ok((
    ImportOrExecutableDefinition::Definition(Described::new(
      to_span(outer),
      description,
      definition,
    )),
    outer,
  ))
}

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
) -> Out<Definition<'src, crate::graphqlx::ast::Definition<&'src str>>> {
  use crate::graphqlx::ast::Definition as D;

  // The two executable kinds, then the eight type-system ones through the shared arm. One list of
  // the eight, not two: `type_system_definition` is what the SDL root reaches them by, and a
  // second copy here would be eight chances for the mixed root and the SDL root to build different
  // ASTs out of the same node. Its refusal for an unknown kind is this one's, unchanged.
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
/// An extension carries no description — a string written in front of an `extend` is reported and
/// stays outside the extension's node — so this answers before the hoist rather than inside it, and
/// its extent is the node's own with nothing lifted out of it.
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

/// Project the description token a node's walk collected, if it collected one.
///
/// Answers the hoisted string and its extent side by side, because [`described_extents`] needs the
/// second to tell the wrapper's span from the definition's. **A token, not a node** — this kind
/// space has no `Description`, so what the walk holds back is one of the definition's own tokens.
fn hoisted_description<'src>(
  token: Option<Token<'_>>,
  source: &'src str,
) -> Out<(Option<StringValue<&'src str>>, Option<TextRange>)> {
  match token {
    Some(token) => Ok((Some(string_value(token, source)?), Some(token.text_range()))),
    None => Ok((None, None)),
  }
}

// ---------------------------------------------------------------------------------------------
// imports
// ---------------------------------------------------------------------------------------------

fn import_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(crate::graphqlx::ast::ImportDefinition<&'src str>, TextRange)> {
  use crate::graphqlx::ast::{ImportClause, ImportDefinition};

  // `import (ImportList | WildcardSpecifier) from InlineStringValue`
  let mut cursor = Cursor::new(node);
  cursor.keyword(ContextualKeyword::Import, "the `import` keyword")?;
  // A choice is one atom, so the exclusivity the fourth round enforced with two cross-guards is a
  // consequence here: whichever alternative comes second is left for `end` to refuse.
  let clause_node = cursor.one_of(&[K::ImportList, K::WildcardSpecifier], "an import clause")?;
  let clause = match clause_node.kind() {
    K::ImportList => ImportClause::List(cursor.keep(import_list(clause_node, source)?)),
    _ => ImportClause::Wildcard(cursor.keep(wildcard_specifier(clause_node, source)?)),
  };
  // Lenient: the parser reports a missing `from` and still builds a hole-free tree — the corpus
  // has `invalid_x_import_without_from` — so an absent one projects, and the clause and the source
  // are both still present. A `from` with no source is not lenient; the atom below is required.
  cursor.opt_keyword(ContextualKeyword::From);
  let source_node = cursor.node(K::StringValue, "a module to import from")?;
  let file = cursor.keep(import_source(source_node, source)?);
  let extent = cursor.finish("a token")?;
  Ok((ImportDefinition::new(to_span(extent), clause, file), extent))
}

/// The `from "…"` half of an import, narrowed to the inline spelling the AST can hold.
fn import_source<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(InlineStringValue<&'src str>, TextRange)> {
  // `StringValue` over one string token, narrowed to the inline spelling.
  let mut cursor = Cursor::new(node);
  let token = cursor.token_of(&[K::InlineString, K::BlockString], "a string literal")?;
  let extent = cursor.finish("a token")?;
  Ok((inline_string_value(node, token, source)?, extent))
}

fn import_list<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(crate::graphqlx::ast::ImportList<&'src str>, TextRange)> {
  use crate::graphqlx::ast::{ImportList, ImportMember};

  // `{ ImportMember+ }` — the `}` is required: no corpus tree witnesses a hole-free list without
  // it, which is the leniency criterion's second half.
  let mut cursor = Cursor::new(node);
  cursor.token(K::LBrace, "the `{` an import list opens with")?;
  let listed = cursor.many1(
    &[K::NamedSpecifier, K::WildcardSpecifier],
    Some(K::RBrace),
    "an import member",
  )?;
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RBrace);
  cursor.end()?;
  let mut members = Vec::with_capacity(listed.len());
  for child in listed {
    members.push(match child.kind() {
      K::NamedSpecifier => ImportMember::Named(cursor.keep(named_specifier(child, source)?)),
      _ => ImportMember::Wildcard(cursor.keep(wildcard_specifier(child, source)?)),
    });
  }
  let extent = cursor.range("a token")?;
  Ok((ImportList::new(to_span(extent), members), extent))
}

fn named_specifier<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(crate::graphqlx::ast::NamedSpecifier<&'src str>, TextRange)> {
  use crate::graphqlx::ast::NamedSpecifier;

  // `Name (as Path)?`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "an imported name")?;
  let alias = optional_alias(&mut cursor, source)?;
  let extent = cursor.finish("a token")?;
  Ok((NamedSpecifier::new(to_span(extent), name, alias), extent))
}

fn wildcard_specifier<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphqlx::ast::WildcardSpecifier<&'src str>,
  TextRange,
)> {
  use crate::graphqlx::ast::WildcardSpecifier;

  // `* (as Path)?`
  let mut cursor = Cursor::new(node);
  cursor.token(K::Asterisk, "a `*`")?;
  let alias = optional_alias(&mut cursor, source)?;
  let extent = cursor.finish("a token")?;
  Ok((WildcardSpecifier::new(to_span(extent), alias), extent))
}

/// `(as Path)?` — the alias both specifier forms carry, and **the `as` commits its `Path`**.
///
/// One of the cross-child rules the transcription keeps as a rule: `if opt(A) then require(B)`.
/// The lossless production consumes the keyword, reports when no path follows, and closes the node
/// with **no hole** (`graphqlx/lossless/import.rs`'s `optional_alias`), so `import { A as } from
/// "m"` reached a walk that accepted `alias == None` and answered an unaliased import with `as`
/// inside its extent — covered, and in no AST field. al8n/smear#58, round 5.
fn optional_alias<'src>(
  cursor: &mut Cursor<'_>,
  source: &'src str,
) -> Out<Option<Path<&'src str>>> {
  match cursor.opt_keyword(ContextualKeyword::As) {
    Some(_) => {
      let child = cursor.node(K::Path, "the path an `as` renames to")?;
      Ok(Some(cursor.keep(path(child, source)?)))
    }
    None => Ok(None),
  }
}

// ---------------------------------------------------------------------------------------------
// names, generic parameters and `where` clauses — no GraphQL counterpart at all
// ---------------------------------------------------------------------------------------------

/// A definition's name, `Name DefinitionTypeGenerics?`.
///
/// **The node that removes every positional token getter from this dialect.** GraphQL puts the
/// keyword and the name under one node as two `Name` tokens and reaches the second by index; here
/// the keyword is the definition's only direct `Name` token and this is a child.
fn definition_name<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DefinitionName<&'src str>, TextRange)> {
  // `Name DefinitionTypeGenerics?`
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "a name")?;
  let generics_node = cursor.opt_node(K::DefinitionTypeGenerics);
  cursor.end()?;
  let generics = cursor.keep_opt(
    generics_node
      .map(|child| definition_type_generics(child, source))
      .transpose()?,
  );
  let extent = cursor.range("a token")?;
  Ok((DefinitionName::new(to_span(extent), name, generics), extent))
}

/// The generic parameters a definition declares, `< DefinitionTypeParam+ >`.
///
/// The one list of the three whose members are nodes, because only its member can carry something:
/// a definition *declares* parameters and may default them, an extension *applies* arguments and
/// may not.
fn definition_type_generics<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DefinitionTypeGenerics<&'src str>, TextRange)> {
  // `< DefinitionTypeParam+ >`
  let mut cursor = Cursor::new(node);
  cursor.token(K::LAngle, "the `<` a parameter list opens with")?;
  let listed = cursor.many1(
    &[K::DefinitionTypeParam],
    Some(K::RAngle),
    "a generic parameter",
  )?;
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RAngle);
  cursor.end()?;
  let mut params = Vec::with_capacity(listed.len());
  for child in listed {
    params.push(cursor.keep(definition_type_param(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((DefinitionTypeGenerics::new(to_span(extent), params), extent))
}

/// One declared generic parameter, `Name (= Type)?`.
///
/// The `=` introduces a **type**, not a
/// [`DefaultValue`](SyntaxKind::DefaultValue): the two spell the same token and mean different
/// things, and this position's default is a type reference.
fn definition_type_param<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(DefinitionTypeParam<&'src str>, TextRange)> {
  // `Name (= Type)?` — **the `=` commits its type**, as `as` commits its path.
  let mut cursor = Cursor::new(node);
  let name = cursor.name_token(source, "a parameter name")?;
  let default_node = match cursor.opt_token(K::Equal) {
    Some(_) => Some(cursor.one_of(&TYPE_KINDS, "the type an `=` defaults to")?),
    None => None,
  };
  cursor.end()?;
  let default = cursor.keep_opt(default_node.map(|child| ty(child, source)).transpose()?);
  let extent = cursor.range("a token")?;
  Ok((
    DefinitionTypeParam::new(to_span(extent), name, default),
    extent,
  ))
}

/// A type-system extension's target, `Path ExtensionTypeGenerics?`.
///
/// **A path, where a [`DefinitionName`] takes a bare name**: `extend type ns::T` names a qualified
/// target and `type ns::T` does not.
fn extension_name<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ExtensionName<&'src str>, TextRange)> {
  // `Path ExtensionTypeGenerics?`
  let mut cursor = Cursor::new(node);
  let path_node = cursor.node(K::Path, "a path")?;
  let generics_node = cursor.opt_node(K::ExtensionTypeGenerics);
  cursor.end()?;
  let path = cursor.keep(path(path_node, source)?);
  let generics = cursor.keep_opt(
    generics_node
      .map(|child| extension_type_generics(child, source))
      .transpose()?,
  );
  let extent = cursor.range("a token")?;
  Ok((ExtensionName::new(to_span(extent), path, generics), extent))
}

/// `Name+` inside `< >` — the member run of the two generic lists whose members are bare names.
///
/// Each member goes through the name door, and an empty run is refused through
/// [`Cursor::absent`]: `MissingChild` for `< >`, `UnexpectedChild` at whatever stands where the
/// first name should.
fn angle_names<'src>(
  cursor: &mut Cursor<'_>,
  source: &'src str,
  wanted: &'static str,
) -> Out<Vec<Name<&'src str>>> {
  let mut names = Vec::new();
  while let Some(name) = cursor.opt_name(source)? {
    names.push(name);
  }
  if names.is_empty() {
    return Err(cursor.absent(Some(K::RAngle), wanted));
  }
  Ok(names)
}

/// The generic arguments a type-system extension applies, `< Name+ >`.
///
/// Bare name tokens: an extension applies arguments and may not default them, so its members carry
/// nothing a node could hold — and the AST wraps each in an [`ExtensionTypeParam`] whose span is
/// the name's own.
fn extension_type_generics<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ExtensionTypeGenerics<&'src str>, TextRange)> {
  // `< Name+ >`
  let mut cursor = Cursor::new(node);
  cursor.token(K::LAngle, "the `<` an argument list opens with")?;
  let names = angle_names(&mut cursor, source, "a generic argument")?;
  // Lenient: the `>` has no AST image and the production builds the list hole-free without it.
  cursor.opt_token(K::RAngle);
  let extent = cursor.finish("a token")?;
  let params = names
    .into_iter()
    .map(|name| ExtensionTypeParam::new(name.span(), name))
    .collect();
  Ok((ExtensionTypeGenerics::new(to_span(extent), params), extent))
}

/// An executable definition's name, `Name ExecutableDefinitionTypeGenerics?`.
///
/// The list here is the **name's own**, not the implementation list the definition declares before
/// it. The two are the same node kind and only the level tells them apart, which is why the
/// definition's own walk matches direct children and this one matches its own.
/// An executable definition's name, and the fragment-name exclusion.
///
/// See the sibling below for the walk. The rule is here because this node **is** the fragment-name
/// production — a fragment is the only executable definition whose name is one of these — so the
/// projection's custody sits exactly where the two parsers' does.
fn executable_definition_name<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ExecutableDefinitionName<&'src str>, TextRange)> {
  // `Name ExecutableDefinitionTypeGenerics?`, and `FragmentName : Name but not on`. The lossless
  // production reports the violation on the diagnostic channel and still builds the node, so the
  // shape alone cannot tell a legal fragment name from an illegal one and this is the third
  // custodian of a rule the two parsers share.
  let mut cursor = Cursor::new(node);
  let name = cursor.name_except(
    source,
    &[ContextualKeyword::On],
    "a fragment may not be named `on`",
    "a name",
  )?;
  let generics_node = cursor.opt_node(K::ExecutableDefinitionTypeGenerics);
  cursor.end()?;
  let generics = cursor.keep_opt(
    generics_node
      .map(|child| executable_definition_type_generics(child, source))
      .transpose()?,
  );
  let extent = cursor.range("a token")?;
  Ok((
    ExecutableDefinitionName::new(to_span(extent), name, generics),
    extent,
  ))
}

/// The generic parameters an executable definition declares, `< Name+ >`.
fn executable_definition_type_generics<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(ExecutableDefinitionTypeGenerics<&'src str>, TextRange)> {
  // `< Name+ >`
  let mut cursor = Cursor::new(node);
  cursor.token(K::LAngle, "the `<` a parameter list opens with")?;
  let names = angle_names(&mut cursor, source, "a generic parameter")?;
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RAngle);
  let extent = cursor.finish("a token")?;
  Ok((
    ExecutableDefinitionTypeGenerics::new(to_span(extent), names),
    extent,
  ))
}

/// A `where` clause, `where WherePredicate (, WherePredicate)*`.
///
/// Undelimited, so this node exists only where a `where` was written, and its extent opens on the
/// keyword.
fn where_clause<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(WhereClause<&'src str>, TextRange)> {
  // `where WherePredicate+`
  let mut cursor = Cursor::new(node);
  cursor.keyword(ContextualKeyword::Where, "the `where` keyword")?;
  let listed = cursor.many1(&[K::WherePredicate], None, "a where predicate")?;
  cursor.end()?;
  let mut predicates = Vec::with_capacity(listed.len());
  for child in listed {
    predicates.push(cursor.keep(where_predicate(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((WhereClause::new(to_span(extent), predicates), extent))
}

/// One `where` predicate, `TypePath : TypePath (& TypePath)*`.
///
/// **Positional, and forced to be.** The constrained type and its bounds are all
/// [`TypePath`](SyntaxKind::TypePath)s under one node — the third of this dialect's two-subtree
/// carriers — so the first is the constrained type and the rest are its bounds, and the `:` and the
/// `&`s are bare tokens between them.
fn where_predicate<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(WherePredicate<&'src str>, TextRange)> {
  // `TypePath : TypePath (& TypePath)*` — no leading `&`, unlike its two siblings.
  let mut cursor = Cursor::new(node);
  let bounded_node = cursor.node(K::TypePath, "a constrained type")?;
  cursor.token(K::Colon, "the `:` before a bound")?;
  let bound_nodes =
    cursor.separated_nodes(&[K::TypePath], K::Ampersand, Leading::Forbidden, "a bound")?;
  cursor.end()?;
  let bounded = cursor.keep(type_path(bounded_node, source)?);
  let mut bounds = Vec::with_capacity(bound_nodes.len());
  for child in bound_nodes {
    bounds.push(cursor.keep(type_path(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((
    WherePredicate::new(to_span(extent), bounded, bounds),
    extent,
  ))
}

/// A `where` clause and the target it constrains, where the clause comes **first**.
///
/// An object's, an interface's and an input object's fields block, and an operation's and a
/// fragment's selection set. The wrapper spans clause-through-target, and with no clause it is the
/// target's own span.
fn constrained_before<Target>(
  clause: Option<(WhereClause<&str>, TextRange)>,
  target: (Target, TextRange),
) -> (Constrained<Target, WhereClause<&str>>, TextRange) {
  let (target, target_range) = target;
  match clause {
    Some((clause, clause_range)) => {
      let range = clause_range.cover(target_range);
      (
        Constrained::new(to_span(range), Some(clause), target),
        range,
      )
    }
    None => (
      Constrained::new(to_span(target_range), None, target),
      target_range,
    ),
  }
}

/// A `where` clause and the target it constrains, where the clause comes **last**.
///
/// A union's members and a directive definition's locations — the two sites this grammar puts the
/// clause after the thing it constrains, which is the reverse of every other one and is visible
/// only in the span.
fn constrained_after<Target>(
  target: (Target, TextRange),
  clause: Option<(WhereClause<&str>, TextRange)>,
) -> (Constrained<Target, WhereClause<&str>>, TextRange) {
  let (target, target_range) = target;
  match clause {
    Some((clause, clause_range)) => {
      let range = target_range.cover(clause_range);
      (
        Constrained::new(to_span(range), Some(clause), target),
        range,
      )
    }
    None => (
      Constrained::new(to_span(target_range), None, target),
      target_range,
    ),
  }
}

/// The `where` clause a node's walk collected, projected.
fn optional_where_clause<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(WhereClause<&'src str>, TextRange)>> {
  match clause {
    Some(node) => where_clause(node, source).map(Some),
    None => Ok(None),
  }
}

/// [`constrained_before`] where the target itself is optional.
///
/// A clause with nothing after it is a `MissingChild`: the AST can only hold a `where` inside a
/// [`Constrained`], so a clause whose target the tree does not have is a region with no AST image.
/// The lossless production reports exactly that and still builds the clause, which is why it has
/// to be re-checked here.
fn optional_constrained_before<'src, T>(
  node: Node<'_>,
  clause: Option<(WhereClause<&'src str>, TextRange)>,
  target: Option<(T, TextRange)>,
  wanted: &'static str,
) -> Out<Option<(Constrained<T, WhereClause<&'src str>>, TextRange)>> {
  match (clause, target) {
    (clause, Some(target)) => Ok(Some(constrained_before(clause, target))),
    (None, None) => Ok(None),
    (Some(_), None) => Err(missing(node, wanted)),
  }
}

/// [`constrained_after`] where the target itself is optional — the union sites.
fn optional_constrained_after<'src, T>(
  node: Node<'_>,
  target: Option<(T, TextRange)>,
  clause: Option<(WhereClause<&'src str>, TextRange)>,
  wanted: &'static str,
) -> Out<Option<(Constrained<T, WhereClause<&'src str>>, TextRange)>> {
  match (target, clause) {
    (Some(target), clause) => Ok(Some(constrained_after(target, clause))),
    (None, None) => Ok(None),
    (None, Some(_)) => Err(missing(node, wanted)),
  }
}

// ---------------------------------------------------------------------------------------------
// paths
// ---------------------------------------------------------------------------------------------

/// A `::`-separated path, and its extent.
///
/// GraphQLx only, and the node every name-shaped position in this dialect goes through. The
/// leading `::` is **part of the path** rather than a prefix on it — `graphqlx::ast::Path` carries
/// `fully_qualified` and the tree records the same fact by keeping the token — so the flag is read
/// off whether the node's first non-trivia token is a separator, which is the same question the
/// wrapper layer answers by counting.
fn path<'src>(node: Node<'_>, source: &'src str) -> Out<(Path<&'src str>, TextRange)> {
  path_except(node, source, &[], "")
}

/// [`path`] at a position the syntactic parser makes a **rule** of: an **unqualified** path whose
/// first segment spells one of `reserved` is [`SemanticRule`](ProjectErrorKind::SemanticRule)
/// naming `rule`. `::on` and `ns::on` are untouched — the syntactic productions refuse only the
/// first segment of a path with no leading `::`.
fn path_except<'src>(
  node: Node<'_>,
  source: &'src str,
  reserved: &[ContextualKeyword],
  rule: &'static str,
) -> Out<(Path<&'src str>, TextRange)> {
  // `::? Name (:: Name)*` — the leading separator is what `fully_qualified` records, and every
  // segment goes through the name door; the first one through the rule's, when unqualified.
  let mut cursor = Cursor::new(node);
  let fully_qualified = cursor.opt_token(K::PathSeparator).is_some();
  let mut first = !fully_qualified;
  let (segments, _) = cursor.separated(
    |cursor| {
      if core::mem::take(&mut first) {
        match cursor.peek() {
          Some(NodeOrToken::Token(token)) if token.kind() == K::Name => cursor
            .name_except(source, reserved, rule, "a path segment")
            .map(Some),
          _ => Ok(None),
        }
      } else {
        cursor.opt_name(source)
      }
    },
    K::PathSeparator,
    Leading::Forbidden,
    "a path segment",
  )?;
  let extent = cursor.finish("a token")?;
  Ok((
    Path::new(to_span(extent), segments, fully_qualified),
    extent,
  ))
}

/// `Path TypeGenerics?` in a position that admits no `!`.
///
/// A directive's name, an interface, a union member, a type condition, a fragment spread's target,
/// a `where` bound, a schema root's type. **A node where GraphQL has a token or a `NamedType`**,
/// and the reason this dialect's `implements` clause holds type paths rather than bare names.
///
/// Its arguments are types, so this reaches [`ty`] — but nothing reachable *from* a type holds one
/// of these, so the call is bounded and does not need the worklist. That asymmetry is also why the
/// two argument lists have different containers: a `TypePath`'s is a plain `Vec` and a
/// `DefinitionTypePath`'s is the [`Nested`] that closes the type cycle.
fn type_path<'src>(node: Node<'_>, source: &'src str) -> Out<(TypePath<&'src str>, TextRange)> {
  type_path_except(node, source, &[], "")
}

/// [`type_path`] whose path is read through [`path_except`] — a fragment spread's target.
fn type_path_except<'src>(
  node: Node<'_>,
  source: &'src str,
  reserved: &[ContextualKeyword],
  rule: &'static str,
) -> Out<(TypePath<&'src str>, TextRange)> {
  // `Path TypeGenerics?` — no `!` in this position.
  let mut cursor = Cursor::new(node);
  let path_node = cursor.node(K::Path, "a path")?;
  let generics_node = cursor.opt_node(K::TypeGenerics);
  cursor.end()?;
  let path = cursor.keep(path_except(path_node, source, reserved, rule)?);
  let generics = cursor.keep_opt(
    generics_node
      .map(|child| flat_type_generics(child, source))
      .transpose()?,
  );
  let extent = cursor.range("a token")?;
  Ok((TypePath::new(to_span(extent), path, generics), extent))
}

/// A `< Type+ >` argument list in a [`TypePath`], whose container is a plain `Vec`.
fn flat_type_generics<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeGenerics<&'src str>, TextRange)> {
  // `< Type+ >`
  let mut cursor = Cursor::new(node);
  cursor.token(K::LAngle, "the `<` an argument list opens with")?;
  let listed = cursor.many1(&TYPE_KINDS, Some(K::RAngle), "a type argument")?;
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
  // module header's missing-token table.
  cursor.opt_token(K::RAngle);
  cursor.end()?;
  let mut params = Vec::with_capacity(listed.len());
  for child in listed {
    params.push(cursor.keep(ty(child, source)?));
  }
  let extent = cursor.range("a token")?;
  Ok((TypeGenerics::new(to_span(extent), params), extent))
}

// ---------------------------------------------------------------------------------------------
// type references
// ---------------------------------------------------------------------------------------------

/// The four type-reference node kinds. **There is no non-null wrapper**: a `!` is a
/// [`Bang`](SyntaxKind::Bang) token of the node it modifies, so each of these carries its own
/// `required` flag and the fold picks it up with the rest of the node's tokens.
const TYPE_KINDS: [SyntaxKind; 4] = [K::DefinitionTypePath, K::ListType, K::SetType, K::MapType];

/// The generic-argument list a **type** carries.
///
/// [`Nested`] rather than a `Vec`, because this is the list that closes the type cycle: a path's
/// arguments are types, so `A<B<C<…>>>` nests without passing through a bracket at all, and
/// releasing one has to be iterative for the reason `graphqlx::ast::Type`'s header records.
type NestedTypeGenerics<'src> =
  crate::ty::TypeGenerics<Type<&'src str>, SimpleSpan, Nested<Type<&'src str>>>;

/// A type container the walk has entered and not finished.
///
/// `[[[Int]]]`, `<<<Int>>>` and `A<A<A<Int>>>` all nest without bound at the lexer's own ceiling,
/// so the walk that reads them is a loop over these rather than a native frame per level — see the
/// module header's *No node dispatch below spends a native frame per level*.
enum TypeFrame<'g, 'src> {
  /// A `[ T ]` or a `< T >`: one element, and the node's own tokens — brackets and `!` — already
  /// folded.
  Wrapper {
    /// The `ListType` or `SetType` node, and the owner of the finished type's span.
    node: Node<'g>,
    extent: Extent,
    required: bool,
  },
  /// A `< K => V >`: two full types under one node, told apart only by their order.
  Map {
    /// The `MapType` node.
    node: Node<'g>,
    extent: Extent,
    required: bool,
    /// The value half, which the walk descends into once the key is built.
    value: Node<'g>,
    /// The key, once the level below has handed it back.
    key: Option<Type<&'src str>>,
  },
  /// A `DefinitionTypePath`'s `< T+ >`, and the path that will wrap the arguments.
  ///
  /// The fourth way this dialect's types nest, and the one a reading of the three bracket shapes
  /// alone would miss.
  Generics {
    /// The `TypeGenerics` node's cursor: its fold, and the arguments not yet read.
    cursor: Cursor<'g>,
    params: Vec<Type<&'src str>>,
    /// The `DefinitionTypePath` node this list belongs to, and everything it had before the list.
    outer: Node<'g>,
    outer_extent: Extent,
    outer_required: bool,
    path: Path<&'src str>,
  },
}

/// What a [`TypeFrame`] did with the type the level below finished.
///
/// # The frame travels by value, and this dialect is why
///
/// The vanilla dialect resumes its frames through `&mut self`, which it can because every frame it
/// has is finished by the value handed to it. Two of the three here are not: a map's key has to
/// **survive** while its value is built, and a path's arguments accumulate. Closing one therefore
/// means moving an owned AST value out of the frame, which a `&mut` resume can only do through a
/// slot whose empty state no input produces — an unreachable refusal, or a panic in a door whose
/// whole contract is that it has neither. So the frame is handed over and handed back, at a
/// constant cost in one driver frame rather than a growing one per level.
enum ResumedType<'g, 'src> {
  /// The frame, and the next child that has to be built.
  Descend(TypeFrame<'g, 'src>, Node<'g>),
  /// This frame is finished, and what it finished to.
  Done(Type<&'src str>, TextRange),
}

/// Open type containers from `node` down to the first type reference that needs no frame, and
/// answer that one.
///
/// Every container on the way is suspended on `frames` with its first child already chosen, so a
/// frame is never on the stack without a live descent below it. A frame is **pushed** rather than
/// returned, because returning one means moving a partly-built type through the caller's frame on
/// a walk whose whole subject is frame size.
fn open_type_chain<'g, 'src>(
  frames: &mut Vec<TypeFrame<'g, 'src>>,
  node: Node<'g>,
  source: &'src str,
) -> Out<(Type<&'src str>, TextRange)> {
  let mut node = node;
  loop {
    match node.kind() {
      K::DefinitionTypePath => {
        // `Path TypeGenerics? !?`
        let mut cursor = Cursor::new(node);
        let path_node = cursor.node(K::Path, "a path")?;
        let generics_node = cursor.opt_node(K::TypeGenerics);
        let required = cursor.opt_token(K::Bang).is_some();
        cursor.end()?;
        let path = cursor.keep(path(path_node, source)?);
        let Some(generics) = generics_node else {
          let extent = cursor.range("a token")?;
          return Ok((
            Type::Path(DefinitionTypePath::new(
              to_span(extent),
              path,
              None,
              required,
            )),
            extent,
          ));
        };
        // `< Type+ >`, whose members are read one descent at a time.
        let mut inner = Cursor::new(generics);
        inner.token(K::LAngle, "the `<` an argument list opens with")?;
        let first = match inner.opt_one_of(&TYPE_KINDS) {
          Some(first) => first,
          None => return Err(inner.absent(Some(K::RAngle), "a type argument")),
        };
        frames.push(TypeFrame::Generics {
          cursor: inner,
          params: Vec::new(),
          outer: node,
          outer_extent: cursor.extent,
          outer_required: required,
          path,
        });
        node = first;
      }
      K::ListType | K::SetType => {
        // `[ Type ] !?` and `< Type > !?` — one element, so the whole sequence is read before the
        // descent and the frame carries only the fold.
        let (open, close) = match node.kind() {
          K::ListType => (K::LBracket, K::RBracket),
          _ => (K::LAngle, K::RAngle),
        };
        let mut cursor = Cursor::new(node);
        cursor.token(open, "the delimiter a type wrapper opens with")?;
        let wrapped = cursor.one_of(&TYPE_KINDS, "a type reference")?;
        // The closer is lenient; the `!` follows it in the production, so there is none without it.
        let required = cursor.opt_token(close).is_some() && cursor.opt_token(K::Bang).is_some();
        cursor.end()?;
        frames.push(TypeFrame::Wrapper {
          node,
          extent: cursor.extent,
          required,
        });
        node = wrapped;
      }
      K::MapType => {
        // `< Type => Type > !?` — two full types told apart only by their order around the `=>`.
        let mut cursor = Cursor::new(node);
        cursor.token(K::LAngle, "the `<` a map type opens with")?;
        let key = cursor.one_of(&TYPE_KINDS, "a key type")?;
        cursor.token(K::FatArrow, "the `=>` between a key type and a value type")?;
        let value = cursor.one_of(&TYPE_KINDS, "a value type")?;
        let required = cursor.opt_token(K::RAngle).is_some() && cursor.opt_token(K::Bang).is_some();
        cursor.end()?;
        frames.push(TypeFrame::Map {
          node,
          extent: cursor.extent,
          required,
          value,
          key: None,
        });
        node = key;
      }
      found => return Err(unexpected(node, found, node.text_range())),
    }
  }
}

/// Fold the type the level below finished into the frame that was waiting on it.
fn resume_type<'g, 'src>(
  frame: TypeFrame<'g, 'src>,
  value: Type<&'src str>,
  piece: TextRange,
) -> Out<ResumedType<'g, 'src>> {
  Ok(match frame {
    TypeFrame::Wrapper {
      node,
      mut extent,
      required,
    } => {
      extent.cover(piece);
      let extent = extent.range(node, "a token")?;
      let span = to_span(extent);
      let built = match node.kind() {
        K::SetType => Type::Set(Nest::new(SetType::new(span, value, required))),
        _ => Type::List(Nest::new(ListType::new(span, value, required))),
      };
      ResumedType::Done(built, extent)
    }
    TypeFrame::Map {
      node,
      mut extent,
      required,
      value: value_node,
      key,
    } => {
      extent.cover(piece);
      match key {
        // The key came back; the value half is what the walk goes to next, and the key rides on
        // the frame while it does.
        None => ResumedType::Descend(
          TypeFrame::Map {
            node,
            extent,
            required,
            value: value_node,
            key: Some(value),
          },
          value_node,
        ),
        Some(key) => {
          let extent = extent.range(node, "a token")?;
          ResumedType::Done(
            Type::Map(Nest::new(MapType::new(
              to_span(extent),
              key,
              value,
              required,
            ))),
            extent,
          )
        }
      }
    }
    TypeFrame::Generics {
      mut cursor,
      mut params,
      outer,
      mut outer_extent,
      outer_required,
      path,
    } => {
      cursor.extent.cover(piece);
      params.push(value);
      match cursor.opt_one_of(&TYPE_KINDS) {
        Some(next) => ResumedType::Descend(
          TypeFrame::Generics {
            cursor,
            params,
            outer,
            outer_extent,
            outer_required,
            path,
          },
          next,
        ),
        None => {
          cursor.opt_token(K::RAngle);
          let inner = cursor.finish("a token")?;
          outer_extent.cover(inner);
          let outer_range = outer_extent.range(outer, "a token")?;
          let generics: NestedTypeGenerics<'src> =
            crate::ty::TypeGenerics::new(to_span(inner), params.into());
          ResumedType::Done(
            Type::Path(DefinitionTypePath::new(
              to_span(outer_range),
              path,
              Some(generics),
              outer_required,
            )),
            outer_range,
          )
        }
      }
    }
  })
}

/// A type reference, with the nesting inside it read on a worklist rather than a stack.
fn ty<'src>(node: Node<'_>, source: &'src str) -> Out<(Type<&'src str>, TextRange)> {
  let mut frames: Vec<TypeFrame<'_, 'src>> = Vec::new();
  let mut built = open_type_chain(&mut frames, node, source)?;
  loop {
    let Some(frame) = frames.pop() else {
      return Ok(built);
    };
    let (value, piece) = built;
    built = match resume_type(frame, value, piece)? {
      ResumedType::Descend(frame, next) => {
        frames.push(frame);
        open_type_chain(&mut frames, next, source)?
      }
      ResumedType::Done(value, range) => (value, range),
    };
  }
}

// ---------------------------------------------------------------------------------------------
// directives and arguments
// ---------------------------------------------------------------------------------------------

/// A directive run, `Directive+` — **row one** of the container table: undelimited and at least
/// one, so a present node with no [`Directive`](SyntaxKind::Directive) child stands for no value
/// the parser produces and is refused as [`MissingChild`](ProjectErrorKind::MissingChild). Only an
/// *absent* run is `None`, and the two then agree without a zero-width placeholder.
///
/// `run` is the slot the caller's own dispatch filled, which is why this takes an `Option` rather
/// than looking the child up again.
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

/// One directive, `@ TypePath Arguments?`.
///
/// **The name is a [`TypePath`](SyntaxKind::TypePath), not a `Name` token** — divergence 10, and it
/// is in every tree: even `@deprecated` nests `Directive > TypePath > Path`.
fn directive<'src>(node: Node<'_>, source: &'src str) -> Out<(Directive<&'src str>, TextRange)> {
  // `@ TypePath Arguments?`
  let mut cursor = Cursor::new(node);
  cursor.token(K::At, "the `@` a directive opens with")?;
  let name_node = cursor.node(K::TypePath, "a directive name")?;
  let arguments_node = cursor.opt_node(K::Arguments);
  cursor.end()?;
  let name = cursor.keep(type_path(name_node, source)?);
  let arguments = cursor.keep_optional(optional_arguments(arguments_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((Directive::new(to_span(extent), name, arguments), extent))
}

/// An argument list, `( Argument* )`.
///
/// Delimited, so `()` is a real, written-down empty list and gets its node — and the syntactic
/// parser answers `None` for it while still covering the parentheses. That is the whole reason this
/// hands the value and the extent back separately; see [`Optional`].
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
  // `Name : Value` — the key is a plain `Name`, exactly as an object field's is: GraphQLx widened
  // the enum value and the directive's name and left both keys alone.
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
  // `@ TypePath Arguments[Const]?`
  let mut cursor = Cursor::new(node);
  cursor.token(K::At, "the `@` a directive opens with")?;
  let name_node = cursor.node(K::TypePath, "a directive name")?;
  let arguments_node = cursor.opt_node(K::Arguments);
  cursor.end()?;
  let name = cursor.keep(type_path(name_node, source)?);
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

/// The eleven value node kinds — GraphQL's nine plus [`SetValue`](SyntaxKind::SetValue) and
/// [`MapValue`](SyntaxKind::MapValue).
const VALUE_KINDS: [SyntaxKind; 11] = [
  K::VariableValue,
  K::IntValue,
  K::FloatValue,
  K::StringValue,
  K::BooleanValue,
  K::NullValue,
  K::EnumValue,
  K::ListValue,
  K::SetValue,
  K::MapValue,
  K::ObjectValue,
];

/// A leaf value's slice, its **token's** span and the node's extent.
///
/// The span is the token's rather than the node's, which is what the syntactic parser builds: every
/// one of these carriers is constructed from the token the lexer handed over. On a well-formed tree
/// the two coincide, and where they do not the tree is carrying something the leaf's production
/// never put there.
fn leaf<'src>(
  node: Node<'_>,
  source: &'src str,
  kind: SyntaxKind,
  wanted: &'static str,
) -> Out<(&'src str, SimpleSpan, TextRange)> {
  // One literal token and nothing else. A leaf that read the first and folded the rest was the byte
  // rule's own shape one level down: `IntValue` over `Int("1")` and `Int("2")` would have answered
  // `1` with a span across both.
  let mut cursor = Cursor::new(node);
  let token = cursor.token(kind, wanted)?;
  let extent = cursor.finish("a token")?;
  Ok((slice(source, token)?, to_span(token.text_range()), extent))
}

fn string_literal<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(StringValue<&'src str>, TextRange)> {
  // One string token, either spelling.
  let mut cursor = Cursor::new(node);
  let token = cursor.token_of(&[K::InlineString, K::BlockString], "a string literal")?;
  let extent = cursor.finish("a token")?;
  Ok((string_value(token, source)?, extent))
}

/// A leaf holding one `Name` read for its **spelling** — `true`, `false`, `null` — and the
/// spelling's classification through the lexer's own table. The caller refuses what it does not
/// classify.
fn spelled_leaf<'src>(
  node: Node<'_>,
  source: &'src str,
  wanted: &'static str,
) -> Out<(Option<ContextualKeyword>, &'src str, SimpleSpan, TextRange)> {
  let mut cursor = Cursor::new(node);
  let token = cursor.spelling(wanted)?;
  let extent = cursor.finish("a token")?;
  Ok((
    keyword_of(token),
    slice(source, token)?,
    to_span(token.text_range()),
    extent,
  ))
}

fn boolean_literal<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(BooleanValue<&'src str>, TextRange)> {
  // One `Name`, read for its spelling.
  let (keyword, _, span, extent) = spelled_leaf(node, source, "a `true` or `false` keyword")?;
  match keyword {
    Some(ContextualKeyword::True) => Ok((BooleanValue::new(span, true), extent)),
    Some(ContextualKeyword::False) => Ok((BooleanValue::new(span, false), extent)),
    _ => Err(ProjectError::new(
      ProjectErrorKind::MalformedToken { kind: K::Name },
      to_range(node.text_range()),
    )),
  }
}

/// An enum value in a **value** position — a whole [`Path`](SyntaxKind::Path), not a name.
///
/// Divergence 9's value half. The AST's span is the **path's**, which is what the syntactic parser
/// builds; on any tree its production made the two are the same bytes, `set { … }`'s retro-wrap
/// included, because both marks are minted in front of the same token.
fn enum_value<'src>(node: Node<'_>, source: &'src str) -> Out<(EnumValue<&'src str>, TextRange)> {
  // `Path`
  let mut cursor = Cursor::new(node);
  let path_node = cursor.node(K::Path, "a path")?;
  cursor.end()?;
  // `true`, `false` and `null` as an unqualified first segment are a boolean and a null in both
  // value grammars' dispatch, and the enum production refuses them (`syntactic/value.rs`), so an
  // enum value spelled that way is outside the parser's image.
  let (path, path_extent) = path_except(
    path_node,
    source,
    &[
      ContextualKeyword::True,
      ContextualKeyword::False,
      ContextualKeyword::Null,
    ],
    "an enum value may not be `true`, `false` or `null`",
  )?;
  cursor.extent.cover(path_extent);
  let extent = cursor.range("a token")?;
  Ok((EnumValue::new(to_span(path_extent), path), extent))
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
/// `value` and `const_value` are each other with `Const` spelled in: eleven arms, ten of them
/// character-for-character identical, differing in the constructors they name and in one refusal.
/// Duplicating a `match` is cheap; **duplicating a worklist is not** — the arms are the easy half
/// and the frame discipline is the half a second copy gets subtly wrong — so the machine below is
/// written once and the difference is this trait.
trait ValueGrammar<'src> {
  /// The value this grammar builds.
  type Value;
  /// One field of an object value in this grammar.
  type Field;
  /// One entry of a map value in this grammar — GraphQLx only.
  type Entry;

  /// A `$name` in this position.
  ///
  /// The one arm the two grammars genuinely disagree about: [`ConstInputValue`] has no `Variable`
  /// variant, so a constant position has nothing to construct and answers a refusal attributed to
  /// `parent` — the position, not the variable, is what is wrong.
  fn variable(parent: Node<'_>, node: Node<'_>, source: &'src str)
  -> Out<(Self::Value, TextRange)>;

  fn int(span: SimpleSpan, value: LitInt<&'src str>) -> Self::Value;
  fn float(span: SimpleSpan, value: LitFloat<&'src str>) -> Self::Value;
  fn string(value: StringValue<&'src str>) -> Self::Value;
  fn boolean(value: BooleanValue<&'src str>) -> Self::Value;
  fn null(span: SimpleSpan, text: &'src str) -> Self::Value;
  fn enumeration(value: EnumValue<&'src str>) -> Self::Value;
  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value;
  fn set(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value;
  fn map(span: SimpleSpan, entries: Vec<Self::Entry>) -> Self::Value;
  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value;
  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field;
  fn entry(span: SimpleSpan, key: Self::Value, value: Self::Value) -> Self::Entry;
}

/// A value position that admits a variable — an argument, a collection member, or an object field
/// of a non-constant argument.
struct Executable;

impl<'src> ValueGrammar<'src> for Executable {
  type Value = InputValue<&'src str>;
  type Field = ObjectField<&'src str>;
  type Entry = MapEntry<&'src str>;

  fn variable(_: Node<'_>, node: Node<'_>, source: &'src str) -> Out<(Self::Value, TextRange)> {
    let (variable, extent) = variable_value(node, source)?;
    Ok((InputValue::Variable(variable), extent))
  }

  fn int(span: SimpleSpan, value: LitInt<&'src str>) -> Self::Value {
    InputValue::Int(IntValue::new(span, value))
  }

  fn float(span: SimpleSpan, value: LitFloat<&'src str>) -> Self::Value {
    InputValue::Float(FloatValue::new(span, value))
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

  fn enumeration(value: EnumValue<&'src str>) -> Self::Value {
    InputValue::Enum(value)
  }

  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    InputValue::List(List::new(span, values.into()))
  }

  fn set(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    InputValue::Set(Set::new(span, values.into()))
  }

  fn map(span: SimpleSpan, entries: Vec<Self::Entry>) -> Self::Value {
    InputValue::Map(Map::new(span, entries.into()))
  }

  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value {
    InputValue::Object(Object::new(span, fields.into()))
  }

  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field {
    ObjectField::new(span, name, value)
  }

  fn entry(span: SimpleSpan, key: Self::Value, value: Self::Value) -> Self::Entry {
    MapEntry::new(span, key, value)
  }
}

/// A constant value position, where the AST's own type system forbids a variable.
struct Constant;

impl<'src> ValueGrammar<'src> for Constant {
  type Value = ConstInputValue<&'src str>;
  type Field = ConstObjectField<&'src str>;
  type Entry = ConstMapEntry<&'src str>;

  fn variable(parent: Node<'_>, node: Node<'_>, _: &'src str) -> Out<(Self::Value, TextRange)> {
    // The refusal is attributed to the position, not to the variable: a `VariableValue` node is
    // perfectly legal, and what is wrong is the const context that is holding one. This dialect
    // reaches it through two containers GraphQL does not have — a set and a map — and a map
    // through either half.
    Err(unexpected(parent, K::VariableValue, node.text_range()))
  }

  fn int(span: SimpleSpan, value: LitInt<&'src str>) -> Self::Value {
    ConstInputValue::Int(IntValue::new(span, value))
  }

  fn float(span: SimpleSpan, value: LitFloat<&'src str>) -> Self::Value {
    ConstInputValue::Float(FloatValue::new(span, value))
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

  fn enumeration(value: EnumValue<&'src str>) -> Self::Value {
    ConstInputValue::Enum(value)
  }

  fn list(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    ConstInputValue::List(ConstList::new(span, values.into()))
  }

  fn set(span: SimpleSpan, values: Vec<Self::Value>) -> Self::Value {
    ConstInputValue::Set(ConstSet::new(span, values.into()))
  }

  fn map(span: SimpleSpan, entries: Vec<Self::Entry>) -> Self::Value {
    ConstInputValue::Map(ConstMap::new(span, entries.into()))
  }

  fn object(span: SimpleSpan, fields: Vec<Self::Field>) -> Self::Value {
    ConstInputValue::Object(ConstObject::new(span, fields.into()))
  }

  fn field(span: SimpleSpan, name: Name<&'src str>, value: Self::Value) -> Self::Field {
    ConstObjectField::new(span, name, value)
  }

  fn entry(span: SimpleSpan, key: Self::Value, value: Self::Value) -> Self::Entry {
    ConstMapEntry::new(span, key, value)
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

/// A `MapEntry` whose halves are being built one after the other.
///
/// **The one place in either dialect where a single child slot yields two subtrees**, and the
/// reason it is an enum rather than a struct with an optional key: the two states are genuinely
/// different — in the first there is no key yet and in the second there is nothing left to descend
/// into — so neither carries a slot whose empty case no input produces.
enum OpenEntry<'g, 'src, G: ValueGrammar<'src>> {
  /// The key is being built below; the value half is where the walk goes when it comes back.
  Key {
    node: Node<'g>,
    extent: Extent,
    key: Node<'g>,
    value: Node<'g>,
  },
  /// The key is built and travels here while the value is built below.
  Value {
    node: Node<'g>,
    extent: Extent,
    key: G::Value,
    value: Node<'g>,
  },
}

impl<'g, 'src, G: ValueGrammar<'src>> OpenEntry<'g, 'src, G> {
  /// The `MapEntry` node, whichever half is open.
  const fn node(&self) -> Node<'g> {
    match self {
      Self::Key { node, .. } | Self::Value { node, .. } => *node,
    }
  }

  /// The half the walk descends into next.
  const fn pending(&self) -> Node<'g> {
    match self {
      Self::Key { key, .. } => *key,
      Self::Value { value, .. } => *value,
    }
  }
}

/// A container value suspended while the value below it is built.
///
/// `{a: {a: … }}`, `[[…]]`, `set { set { … } }` and `map { … => map { … } }` all nest without
/// bound at the lexer's own ceiling, so the walk that reads them is a loop over these rather than a
/// native frame per level.
struct ValueFrame<'g, 'src, G: ValueGrammar<'src>> {
  /// The container's cursor: its node, the fold over its own tokens and the members already
  /// finished, and the members not yet read.
  cursor: Cursor<'g>,
  /// What has been folded so far, and which container this is.
  built: Built<'g, 'src, G>,
}

/// A container's accumulator.
enum Built<'g, 'src, G: ValueGrammar<'src>> {
  /// A `ListValue`'s or a `SetValue`'s members, in document order. The node's kind says which, so
  /// the two share a shape rather than a name.
  Values(Vec<G::Value>),
  /// An `ObjectValue`'s finished fields, and the field whose value is being built below.
  Object {
    fields: Vec<G::Field>,
    open: OpenField<'g, 'src>,
  },
  /// A `MapValue`'s finished entries, and the entry one of whose halves is being built below.
  Map {
    entries: Vec<G::Entry>,
    open: OpenEntry<'g, 'src, G>,
  },
}

/// What a [`ValueFrame`] did with the value the level below finished.
///
/// The frame travels by value for the reason [`ResumedType`] records: two of the three
/// accumulators here hold an owned AST value that has to leave the frame when it closes. A frame
/// that descends again is pushed back onto the worklist by the resume itself, so this answer
/// carries only the two nodes — a frame holds a whole [`Cursor`], and handing it back through the
/// answer made every `Done` as large as the frame.
enum ResumedValue<'g, 'src, G: ValueGrammar<'src>> {
  /// The node whose dispatch reaches the next value, and that value's node; the frame is back on
  /// the worklist.
  Descend(Node<'g>, Node<'g>),
  /// This frame is finished, and what it finished to.
  Done(G::Value, TextRange),
}

/// The next field of an object value, with the tokens passed on the way folded in.
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

/// The next entry of a map value, with the tokens passed on the way folded in.
fn next_entry<'g, 'src, G: ValueGrammar<'src>>(
  cursor: &mut Cursor<'g>,
) -> Out<Option<OpenEntry<'g, 'src, G>>> {
  match cursor.opt_node(K::MapEntry) {
    Some(child) => open_map_entry(child).map(Some),
    None => Ok(None),
  }
}

/// A `MapEntry` read as far as its key, with the value half recorded for afterwards.
///
/// **Positional, and forced to be**: both halves are full values with nothing between them but the
/// `=>`, so an entry's key is its first node child and its value is the one after.
fn open_map_entry<'g, 'src, G: ValueGrammar<'src>>(node: Node<'g>) -> Out<OpenEntry<'g, 'src, G>> {
  // `Value => Value`
  let mut cursor = Cursor::new(node);
  let key = cursor.one_of(&VALUE_KINDS, "a key")?;
  cursor.token(K::FatArrow, "the `=>` between a key and a value")?;
  let value = cursor.one_of(&VALUE_KINDS, "a value")?;
  cursor.end()?;
  Ok(OpenEntry::Key {
    node,
    extent: cursor.extent,
    key,
    value,
  })
}

/// Open containers from `node` down to the first value that finishes without one, and answer that
/// one.
///
/// Every container on the way is suspended on `frames` with the child it must build first already
/// chosen, so a frame is never on the stack without a live descent below it. A frame is **pushed**
/// rather than returned, because returning one means moving a partly-built value through the
/// caller's frame on a walk whose whole subject is frame size.
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
      K::VariableValue => return G::variable(parent, node, source),
      K::IntValue => {
        let (text, span, extent) = leaf(node, source, K::Int, "an integer literal")?;
        return Ok((G::int(span, int_lit(text, span)?), extent));
      }
      K::FloatValue => {
        let (text, span, extent) = leaf(node, source, K::Float, "a float literal")?;
        return Ok((G::float(span, float_lit(text, span)?), extent));
      }
      K::StringValue => {
        let (string, extent) = string_literal(node, source)?;
        return Ok((G::string(string), extent));
      }
      K::BooleanValue => {
        let (boolean, extent) = boolean_literal(node, source)?;
        return Ok((G::boolean(boolean), extent));
      }
      K::NullValue => {
        let (keyword, text, span, extent) = spelled_leaf(node, source, "a `null` keyword")?;
        // The spelling, not just the kind: `NullValue` carries its own text into the AST, so a
        // node of this kind over any other identifier would project to a `null` the parser has no
        // way to produce. Its sibling `BooleanValue` has always checked; this one did not.
        if keyword != Some(ContextualKeyword::Null) {
          return Err(ProjectError::new(
            ProjectErrorKind::MalformedToken { kind: K::Name },
            to_range(node.text_range()),
          ));
        }
        return Ok((G::null(span, text), extent));
      }
      K::EnumValue => {
        let (value, extent) = enum_value(node, source)?;
        return Ok((G::enumeration(value), extent));
      }
      K::ListValue | K::SetValue | K::ObjectValue | K::MapValue => {
        // `[ Value* ]`, `set { Value* }`, `{ ObjectField* }`, `map { MapEntry* }` — the opener
        // here, each member one descent, and the closer when the members run out.
        let mut cursor = Cursor::new(node);
        match node.kind() {
          K::ListValue => {
            cursor.token(K::LBracket, "the `[` a list opens with")?;
          }
          K::SetValue => {
            cursor.keyword(ContextualKeyword::Set, "the `set` keyword")?;
            cursor.token(K::LBrace, "the `{` a set opens with")?;
          }
          K::ObjectValue => {
            cursor.token(K::LBrace, "the `{` an object opens with")?;
          }
          _ => {
            cursor.keyword(ContextualKeyword::Map, "the `map` keyword")?;
            cursor.token(K::LBrace, "the `{` a map opens with")?;
          }
        }
        let opened = match node.kind() {
          K::ListValue | K::SetValue => cursor
            .opt_one_of(&VALUE_KINDS)
            .map(|first| (node, first, Built::Values(Vec::new()))),
          K::ObjectValue => next_field(&mut cursor, source)?.map(|open| {
            (
              open.node,
              open.pending,
              Built::Object {
                fields: Vec::new(),
                open,
              },
            )
          }),
          _ => next_entry::<G>(&mut cursor)?.map(|open| {
            (
              open.node(),
              open.pending(),
              Built::Map {
                entries: Vec::new(),
                open,
              },
            )
          }),
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
              K::SetValue => G::set(span, Vec::new()),
              K::ObjectValue => G::object(span, Vec::new()),
              _ => G::map(span, Vec::new()),
            };
            return Ok((empty, range));
          }
        }
      }
      found => return Err(unexpected(node, found, node.text_range())),
    }
  }
}

/// The closer of a container value whose members have run out: `]` for a list, `}` for the
/// other three — **lenient**, because `unclosed_list` and `unclosed_object` build each of the four
/// hole-free without it and the closer has no AST image — and then nothing left over.
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
    Built::Values(mut values) => {
      cursor.extent.cover(piece);
      values.push(value);
      match cursor.opt_one_of(&VALUE_KINDS) {
        Some(next) => {
          frames.push(ValueFrame {
            cursor,
            built: Built::Values(values),
          });
          ResumedValue::Descend(node, next)
        }
        None => {
          let range = close_container(&mut cursor)?;
          let span = to_span(range);
          let built = match node.kind() {
            K::SetValue => G::set(span, values),
            _ => G::list(span, values),
          };
          ResumedValue::Done(built, range)
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
    Built::Map { mut entries, open } => match open {
      // The key came back. It rides on the frame while the value half is built.
      OpenEntry::Key {
        node: entry,
        extent: mut inner,
        value: value_node,
        ..
      } => {
        inner.cover(piece);
        {
          frames.push(ValueFrame {
            cursor,
            built: Built::Map {
              entries,
              open: OpenEntry::Value {
                node: entry,
                extent: inner,
                key: value,
                value: value_node,
              },
            },
          });
          ResumedValue::Descend(entry, value_node)
        }
      }
      OpenEntry::Value {
        node: entry,
        extent: mut inner,
        key,
        ..
      } => {
        inner.cover(piece);
        let range = inner.range(entry, "a token")?;
        entries.push(G::entry(to_span(range), key, value));
        cursor.extent.cover(range);
        match next_entry::<G>(&mut cursor)? {
          Some(next) => {
            let (parent, pending) = (next.node(), next.pending());
            frames.push(ValueFrame {
              cursor,
              built: Built::Map {
                entries,
                open: next,
              },
            });
            ResumedValue::Descend(parent, pending)
          }
          None => {
            let range = close_container(&mut cursor)?;
            ResumedValue::Done(G::map(to_span(range), entries), range)
          }
        }
      }
    },
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
/// The frame travels by value for the reason [`ResumedType`] records, and here for one more: an
/// open selection holds a finished `Alias`, `Arguments` and `Directives` that have to leave the
/// frame when it closes.
///
/// A frame that descends again is pushed back by the resume itself, as [`ResumedValue`]'s is.
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
/// `None` means the set is complete.
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
/// through [`Cursor::absent`], and the `}` is **lenient** — see the module header's missing-token
/// table.
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
        {
          frames.push(SelectionFrame {
            cursor,
            selections,
            open: next,
          });
          ResumedSet::Descend(nested)
        }
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
  // `Alias? Name Arguments? Directives? SelectionSet?` — the alias is a node holding its own
  // `Name` and `:`, so the field's name is the one `Name` this node holds directly.
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

/// A type condition, `on T` — **a node in this dialect**, where GraphQL's kind space has none and
/// the condition surfaces as the type after the keyword.
fn type_condition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(TypeCondition<&'src str>, TextRange)> {
  // `on TypePath`, with the `on` **lenient**: it has no AST image — the condition stores only the
  // path — and the lossless production reports a missing one and still builds this node, hole-free,
  // around the path it did find. The path is required either way.
  let mut cursor = Cursor::new(node);
  cursor.opt_keyword(ContextualKeyword::On);
  let child = cursor.node(K::TypePath, "a type path")?;
  cursor.end()?;
  let path = cursor.keep(type_path(child, source)?);
  let extent = cursor.range("a token")?;
  Ok((TypeCondition::new(to_span(extent), path), extent))
}

fn fragment_spread<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(FragmentSpread<&'src str>, TextRange)> {
  // `... TypePath Directives?` — the target is a whole path with optional generics, divergence
  // 11, so `... ns::F<Int>` is one spread rather than a spread followed by junk.
  let mut cursor = Cursor::new(node);
  cursor.token(K::Spread, "the `...` a spread opens with")?;
  let path_node = cursor.node(K::TypePath, "a fragment name")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;
  // `... on` is an inline fragment's head in both parsers (`selection/mod.rs`'s spread dispatch,
  // and `fragment_type_path`'s refusal), so an unqualified target spelled `on` is no spread any
  // source produces.
  let path = cursor.keep(type_path_except(
    path_node,
    source,
    &[ContextualKeyword::On],
    "a fragment spread may not target `on`",
  )?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((
    FragmentSpread::new(to_span(extent), path, directives),
    extent,
  ))
}

/// An inline fragment read as far as its selection set, which the grammar makes mandatory — so
/// unlike a field this always nests.
fn open_inline_fragment<'g, 'src>(
  node: Node<'g>,
  source: &'src str,
) -> Out<OpenSelection<'g, 'src>> {
  // `... TypeCondition? Directives? SelectionSet`
  let mut cursor = Cursor::new(node);
  cursor.token(K::Spread, "the `...` an inline fragment opens with")?;
  let condition_node = cursor.opt_node(K::TypeCondition);
  let directives_node = cursor.opt_node(K::Directives);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let type_condition = cursor.keep_opt(
    condition_node
      .map(|child| type_condition(child, source))
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
// executable definitions
// ---------------------------------------------------------------------------------------------

/// The operation keyword a `Name` token spells, classified through the lexer's own table.
///
/// A bare token here, where GraphQL wraps it in an `OperationType` node: this kind space gives one
/// keyword token no kind of its own. It is read for its **spelling** rather than for its position,
/// which is what makes an operation definition's only direct `Name` token unambiguous.
fn operation_type(token: Token<'_>, node: Node<'_>) -> Out<OperationType> {
  let span = to_span(token.text_range());
  match keyword_of(token) {
    Some(ContextualKeyword::Query) => Ok(OperationType::Query(span)),
    Some(ContextualKeyword::Mutation) => Ok(OperationType::Mutation(span)),
    Some(ContextualKeyword::Subscription) => Ok(OperationType::Subscription(span)),
    _ => {
      let _ = node;
      Err(ProjectError::new(
        ProjectErrorKind::MalformedToken { kind: token.kind() },
        to_range(token.text_range()),
      ))
    }
  }
}

fn operation_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, OperationDefinition<&'src str>>> {
  // `Description? OperationType DefinitionName? VariablesDefinition? Directives? WhereClause?
  //  SelectionSet` | `SelectionSet`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  let Some(keyword) = cursor.opt_spelling() else {
    // Query shorthand: the definition *is* its selection set, and the AST's span for it is the
    // selection set's own. The grammar gives a shorthand no other constituent, so anything else
    // here is not in the sequence and `end` refuses it.
    //
    // **The description is the one the parser itself produces.** `"docs" { id }` is reported by
    // `document.rs`'s `definition_after_description` and the operation is still built *around* the
    // string, because the description was committed before the mark the node opens at — so a
    // parser-built pair reaches here with a description, and the fail-fast projection used to
    // answer a described shorthand while the recovering one counted the entry complete with
    // `skipped == 0`. `UnexpectedChild` and not `SemanticRule`: the syntactic side refuses it in
    // `refuse_described_shorthand` with an `Expectation` at the `{` rather than by naming a rule,
    // so this is a token the production has no slot for and not a spelling a rule excludes.
    // al8n/smear#58.
    if let Some(token) = description_token {
      return Err(unexpected_token(node, token));
    }
    let set = cursor.node(K::SelectionSet, "a selection set")?;
    cursor.end()?;
    let selections = cursor.keep(selection_set(set, source)?);
    let (outer, _) = described_extents(node, cursor.extent, None)?;
    return Ok((None, OperationDefinition::Shorthand(selections), outer));
  };
  let operation_type = operation_type(keyword, node)?;
  let name_node = cursor.opt_node(K::DefinitionName);
  let variables_node = cursor.opt_node(K::VariablesDefinition);
  let directives_node = cursor.opt_node(K::Directives);
  let where_node = cursor.opt_node(K::WhereClause);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep_opt(
    name_node
      .map(|child| definition_name(child, source))
      .transpose()?,
  );
  let variables = cursor.keep_opt(variables_definition(variables_node, source)?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let clause = optional_where_clause(where_node, source)?;
  let selections = cursor.keep(constrained_before(clause, selection_set(set, source)?));

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

/// An operation's variable definitions, `( VariableDefinition+ )`.
///
/// **A `+` container, not an `()` one**, and the pair is worth stating side by side: an argument
/// list has no `at_least(1)`, so `f()` is a written-down empty list the syntactic parser maps to
/// `None`; a variables definition has one, so `query Q() { f }` is a document the syntactic parser
/// **rejects**. Collapsing the second to `None` the way the first is collapsed produced an AST for
/// a document that has none. See the module header's three-way rule.
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
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
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
  // `Description? VariableValue : Type DefaultValue? Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  let variable_node = cursor.node(K::VariableValue, "a variable")?;
  cursor.token(K::Colon, "the `:` before a variable's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let default_node = cursor.opt_node(K::DefaultValue);
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
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
) -> Out<Definition<'src, crate::graphqlx::ast::FragmentDefinition<&'src str>>> {
  // `Description? fragment ExecutableDefinitionTypeGenerics? ExecutableDefinitionName
  //  TypeCondition Directives? WhereClause? SelectionSet`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  let keyword = cursor.keyword(ContextualKeyword::Fragment, "the `fragment` keyword")?;
  // The **implementation** generics: the list written before the name, and a sibling of it. The
  // name's own list is a child of `ExecutableDefinitionName`, so the sequence answers the first
  // and never the second.
  let generics_node = cursor.opt_node(K::ExecutableDefinitionTypeGenerics);
  let name_node = cursor.node(K::ExecutableDefinitionName, "a fragment name")?;
  let condition_node = cursor.node(K::TypeCondition, "a type condition")?;
  let directives_node = cursor.opt_node(K::Directives);
  let where_node = cursor.opt_node(K::WhereClause);
  let set = cursor.node(K::SelectionSet, "a selection set")?;
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let implementation = cursor.keep_opt(
    generics_node
      .map(|child| executable_definition_type_generics(child, source))
      .transpose()?,
  );
  let (name, name_range) = executable_definition_name(name_node, source)?;
  cursor.extent.cover(name_range);
  // The header opens on the `fragment` keyword and closes at the end of the name, the
  // implementation generics between them inside it. The cursor read the keyword before the name,
  // so the two are in order by construction — the check a slot dispatch needed here (a
  // caller-built tree with the name first made `SimpleSpan::new` panic) is the sequence itself
  // now, and that tree is refused by the `keyword` atom at the name node.
  let header = ExecutableDefinitionHeader::new(
    SimpleSpan::new(
      usize::from(keyword.text_range().start()),
      usize::from(name_range.end()),
    ),
    implementation,
    name,
  );

  let condition = cursor.keep(type_condition(condition_node, source)?);
  let directives = cursor.keep_optional(optional_directives(directives_node, source)?);
  let clause = optional_where_clause(where_node, source)?;
  let selections = cursor.keep(constrained_before(clause, selection_set(set, source)?));

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    crate::graphqlx::ast::FragmentDefinition::new(
      to_span(inner),
      header,
      condition,
      directives,
      selections,
    ),
    outer,
  ))
}

// ---------------------------------------------------------------------------------------------
// SDL definitions
// ---------------------------------------------------------------------------------------------

/// An `implements A & B` clause, whose members are **type paths** and not bare names.
///
/// GraphQL holds `Name`s here because an implemented interface there can carry no `!` and no
/// brackets; GraphQLx's can carry a namespace and generic arguments, so the type-path level is
/// content rather than a wrapper over nothing.
fn optional_implements<'src>(
  clause: Option<Node<'_>>,
  source: &'src str,
) -> Out<Option<(ImplementInterfaces<&'src str>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  // `implements &? TypePath (& TypePath)*`
  let mut cursor = Cursor::new(clause);
  cursor.keyword(ContextualKeyword::Implements, "the `implements` keyword")?;
  let members = cursor.separated_nodes(
    &[K::TypePath],
    K::Ampersand,
    Leading::Allowed,
    "an interface",
  )?;
  let mut interfaces = Vec::with_capacity(members.len());
  for child in members {
    interfaces.push(cursor.keep(type_path(child, source)?));
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
) -> Out<Option<(UnionMemberTypes<&'src str>, TextRange)>> {
  let Some(clause) = clause else {
    return Ok(None);
  };
  // `= |? TypePath (| TypePath)*`
  let mut cursor = Cursor::new(clause);
  cursor.token(K::Equal, "the `=` before a union's members")?;
  let listed =
    cursor.separated_nodes(&[K::TypePath], K::Pipe, Leading::Allowed, "a member type")?;
  let mut members = Vec::with_capacity(listed.len());
  for child in listed {
    members.push(cursor.keep(type_path(child, source)?));
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
) -> Out<(crate::graphqlx::ast::FieldDefinition<&'src str>, TextRange)> {
  // `Description? Name ArgumentsDefinition? : Type Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  // One span for both halves, description included — trunk's rule for this node, see the module
  // header. The description is therefore folded straight in rather than held back.
  if let Some(token) = description_token {
    cursor.extent.cover(token.text_range());
  }
  let name = cursor.name_token(source, "a field name")?;
  let arguments_node = cursor.opt_node(K::ArgumentsDefinition);
  cursor.token(K::Colon, "the `:` before a field's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, _) = hoisted_description(description_token, source)?;
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
) -> Out<Option<(ArgumentsDefinition<&'src str>, TextRange)>> {
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
  // Lenient: no AST image, and `unclosed_*` builds the node hole-free without it — see the
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
  crate::graphqlx::ast::InputValueDefinition<&'src str>,
  TextRange,
)> {
  // `Description? Name : Type DefaultValue? Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  // The second of the three node types whose wrapper and inner span agree.
  if let Some(token) = description_token {
    cursor.extent.cover(token.text_range());
  }
  let name = cursor.name_token(source, "an input value name")?;
  cursor.token(K::Colon, "the `:` before an input value's type")?;
  let type_node = cursor.one_of(&TYPE_KINDS, "a type reference")?;
  let default_node = cursor.opt_node(K::DefaultValue);
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, _) = hoisted_description(description_token, source)?;
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

/// One enum value definition — a **bare `Name`**, where a value position's enum value is a whole
/// path.
///
/// Divergence 9's SDL half, and the site of this dialect's one semantic rule: `true`, `false` and
/// `null` are excluded here, the lossless production records the violation on the diagnostic
/// channel and still builds the node, and the shape alone therefore cannot tell a legal declaring
/// name from an illegal one.
fn enum_value_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<(
  crate::graphqlx::ast::EnumValueDefinition<&'src str>,
  TextRange,
)> {
  // `Description? Name Directives[Const]?`, and the name is not `true`, `false` or `null`.
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  // The third of the three node types whose wrapper and inner span agree.
  if let Some(token) = description_token {
    cursor.extent.cover(token.text_range());
  }
  let value = cursor.name_except(
    source,
    &[
      ContextualKeyword::True,
      ContextualKeyword::False,
      ContextualKeyword::Null,
    ],
    "an enum value may not be named `true`, `false` or `null`",
    "an enum value",
  )?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, _) = hoisted_description(description_token, source)?;
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
  // `Description? scalar DefinitionName Directives[Const]?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Scalar, "the `scalar` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let directives_node = cursor.opt_node(K::Directives);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
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
  // `Description? type DefinitionName ImplementInterfaces? Directives[Const]? WhereClause?
  //  FieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Type, "the `type` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let implements_node = cursor.opt_node(K::ImplementInterfaces);
  let directives_node = cursor.opt_node(K::Directives);
  let where_node = cursor.opt_node(K::WhereClause);
  let fields_node = cursor.opt_node(K::FieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields = optional_constrained_before(
    node,
    optional_where_clause(where_node, source)?,
    optional_fields_definition(fields_node, source)?,
    "the fields a where clause constrains",
  )?;
  let fields_definition = cursor.keep_opt(fields);

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
  // `Description? interface DefinitionName ImplementInterfaces? Directives[Const]? WhereClause?
  //  FieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Interface, "the `interface` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let implements_node = cursor.opt_node(K::ImplementInterfaces);
  let directives_node = cursor.opt_node(K::Directives);
  let where_node = cursor.opt_node(K::WhereClause);
  let fields_node = cursor.opt_node(K::FieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields = optional_constrained_before(
    node,
    optional_where_clause(where_node, source)?,
    optional_fields_definition(fields_node, source)?,
    "the fields a where clause constrains",
  )?;
  let fields_definition = cursor.keep_opt(fields);

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
  // `Description? union DefinitionName Directives[Const]? UnionMemberTypes? WhereClause?` —
  // divergence 17 puts the clause *after* the members it constrains.
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Union, "the `union` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let directives_node = cursor.opt_node(K::Directives);
  let members_node = cursor.opt_node(K::UnionMemberTypes);
  let where_node = cursor.opt_node(K::WhereClause);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let members = optional_constrained_after(
    node,
    optional_union_members(members_node, source)?,
    optional_where_clause(where_node, source)?,
    "the members a where clause constrains",
  )?;
  let member_types = cursor.keep_opt(members);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    UnionTypeDefinition::new(to_span(inner), name, directives, member_types),
    outer,
  ))
}

fn enum_type_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, EnumTypeDefinition<&'src str>>> {
  // `Description? enum DefinitionName Directives[Const]? EnumValuesDefinition?` — the one type
  // definition with no `where` clause in its grammar at all.
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Enum, "the `enum` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let directives_node = cursor.opt_node(K::Directives);
  let values_node = cursor.opt_node(K::EnumValuesDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
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
  // `Description? input DefinitionName Directives[Const]? WhereClause? InputFieldsDefinition?`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Input, "the `input` keyword")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let directives_node = cursor.opt_node(K::Directives);
  let where_node = cursor.opt_node(K::WhereClause);
  let input_fields_node = cursor.opt_node(K::InputFieldsDefinition);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let fields = optional_constrained_before(
    node,
    optional_where_clause(where_node, source)?,
    optional_input_fields_definition(input_fields_node, source)?,
    "the input fields a where clause constrains",
  )?;
  let fields_definition = cursor.keep_opt(fields);

  let (outer, inner) = described_extents(node, cursor.extent, described)?;
  Ok((
    description,
    InputObjectTypeDefinition::new(to_span(inner), name, directives, fields_definition),
    outer,
  ))
}

fn directive_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, crate::graphqlx::ast::DirectiveDefinition<&'src str>>> {
  // `Description? directive @ DefinitionName ArgumentsDefinition? repeatable? on
  //  DirectiveLocations WhereClause?` — three keyword positions, each read by its spelling in its
  // own place, so `directive @d foo FIELD` is refused at `foo` rather than answering what
  // `directive @d on FIELD` answers.
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Directive, "the `directive` keyword")?;
  cursor.token(K::At, "the `@` before a directive's name")?;
  let name_node = cursor.node(K::DefinitionName, "a definition name")?;
  let arguments_node = cursor.opt_node(K::ArgumentsDefinition);
  let repeatable = cursor.opt_keyword(ContextualKeyword::Repeatable).is_some();
  // Lenient, as a type condition's `on` is: no AST image, and the production reports a missing
  // one and still builds the definition, hole-free, around the locations. The locations stay
  // required.
  cursor.opt_keyword(ContextualKeyword::On);
  let locations_node = cursor.node(K::DirectiveLocations, "a location list")?;
  let where_node = cursor.opt_node(K::WhereClause);
  cursor.end()?;

  let (description, described) = hoisted_description(description_token, source)?;
  let name = cursor.keep(definition_name(name_node, source)?);
  let arguments_definition =
    cursor.keep_opt(optional_arguments_definition(arguments_node, source)?);
  // The other site this grammar puts the clause after the thing it constrains.
  let locations = cursor.keep(constrained_after(
    directive_locations(locations_node)?,
    optional_where_clause(where_node, source)?,
  ));

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
fn directive_locations(node: Node<'_>) -> Out<(DirectiveLocations, TextRange)> {
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
      .ok_or_else(|| {
        ProjectError::new(
          ProjectErrorKind::MalformedToken { kind: token.kind() },
          to_range(token.text_range()),
        )
      })?;
    locations.push(location);
  }
  let extent = cursor.finish("a token")?;
  Ok((DirectiveLocations::new(to_span(extent), locations), extent))
}

fn schema_definition<'src>(
  node: Node<'_>,
  source: &'src str,
) -> Out<Definition<'src, SchemaDefinition<&'src str>>> {
  // `Description? schema Directives[Const]? { RootOperationTypeDefinition+ }`
  let mut cursor = Cursor::new(node);
  let description_token = cursor.opt_description();
  cursor.keyword(ContextualKeyword::Schema, "the `schema` keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let roots_node = cursor.node(
    K::RootOperationTypesDefinition,
    "a root operation types block",
  )?;

  let (description, described) = hoisted_description(description_token, source)?;
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);
  let roots = cursor.keep(root_operation_types(roots_node, source)?);

  cursor.end()?;
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
  // `OperationType : TypePath` — the keyword a bare `Name` read for its spelling, exactly as on an
  // operation definition.
  let mut cursor = Cursor::new(node);
  let keyword = cursor.spelling("an operation keyword")?;
  let operation_type = operation_type(keyword, node)?;
  cursor.token(K::Colon, "the `:` before a root type")?;
  let named_node = cursor.node(K::TypePath, "a type path")?;
  cursor.end()?;
  let named = cursor.keep(type_path(named_node, source)?);
  let extent = cursor.range("a token")?;
  Ok((
    RootOperationTypeDefinition::new(to_span(extent), operation_type, named),
    extent,
  ))
}

// ---------------------------------------------------------------------------------------------
// SDL extensions
// ---------------------------------------------------------------------------------------------

/// The constituents every extension's tail is assembled from, and its extent.
///
/// The six named extension productions differ only in which of these the grammar lets them carry
/// and in how the combination is encoded, so the walk is written once. **No extension is
/// described**: a string written in front of an `extend` is reported and stays outside the node, so
/// there is no description to hold back here and a stray one is rubble the document level refuses.
struct ExtensionParts<'src> {
  name: ExtensionName<&'src str>,
  implements: Option<ImplementInterfaces<&'src str>>,
  directives: Option<ConstDirectives<&'src str>>,
  fields: Option<Constrained<FieldsDefinition<&'src str>, WhereClause<&'src str>>>,
  input_fields: Option<Constrained<InputFieldsDefinition<&'src str>, WhereClause<&'src str>>>,
  members: Option<Constrained<UnionMemberTypes<&'src str>, WhereClause<&'src str>>>,
  values: Option<EnumValuesDefinition<&'src str>>,
  extent: TextRange,
}

fn extension_parts<'src>(node: Node<'_>, source: &'src str) -> Out<ExtensionParts<'src>> {
  // `extend <keyword> ExtensionName <this kind's tail>` — one transcription per kind, sharing the
  // sequence *prefix* rather than a slot struct filled from the union of six vocabularies. The
  // union is what let a `ScalarTypeExtension` carry a `FieldsDefinition` into a constructor that
  // reads the directives and answered `Ok` with the block dropped; with the tail written per kind
  // the foreign child is simply not in the sequence, and `end` refuses it.
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
  let name_node = cursor.node(K::ExtensionName, "an extended type's name")?;

  let mut implements_node = None;
  let mut where_node = None;
  let mut fields_node = None;
  let mut input_fields_node = None;
  let mut members_node = None;
  let mut values_node = None;
  let directives_node;
  match node.kind() {
    // `extend scalar ExtensionName Directives[Const]`
    K::ScalarTypeExtension => directives_node = cursor.opt_node(K::Directives),
    // `extend (type|interface) ExtensionName ImplementInterfaces? Directives[Const]? WhereClause?
    //  FieldsDefinition?`
    K::ObjectTypeExtension | K::InterfaceTypeExtension => {
      implements_node = cursor.opt_node(K::ImplementInterfaces);
      directives_node = cursor.opt_node(K::Directives);
      where_node = cursor.opt_node(K::WhereClause);
      fields_node = cursor.opt_node(K::FieldsDefinition);
    }
    // `extend union ExtensionName Directives[Const]? UnionMemberTypes? WhereClause?` — the clause
    // follows what it constrains here, as it does at the definition site.
    K::UnionTypeExtension => {
      directives_node = cursor.opt_node(K::Directives);
      members_node = cursor.opt_node(K::UnionMemberTypes);
      where_node = cursor.opt_node(K::WhereClause);
    }
    // `extend enum ExtensionName Directives[Const]? EnumValuesDefinition?`
    K::EnumTypeExtension => {
      directives_node = cursor.opt_node(K::Directives);
      values_node = cursor.opt_node(K::EnumValuesDefinition);
    }
    // `extend input ExtensionName Directives[Const]? WhereClause? InputFieldsDefinition?`
    _ => {
      directives_node = cursor.opt_node(K::Directives);
      where_node = cursor.opt_node(K::WhereClause);
      input_fields_node = cursor.opt_node(K::InputFieldsDefinition);
    }
  }
  cursor.end()?;

  let name = cursor.keep(extension_name(name_node, source)?);
  let implements = cursor.keep_opt(optional_implements(implements_node, source)?);
  let directives = cursor.keep_optional(optional_const_directives(directives_node, source)?);

  // At most one tail is in any kind's sequence, so the three below cannot both be `Some` and the
  // clause belongs to whichever one is here.
  let clause = optional_where_clause(where_node, source)?;
  let fields = optional_fields_definition(fields_node, source)?;
  let input_fields = optional_input_fields_definition(input_fields_node, source)?;
  let members = optional_union_members(members_node, source)?;
  let (fields, input_fields, members) = match (fields, input_fields, members) {
    (Some(fields), input_fields, members) => (
      Some(constrained_before(clause, fields)),
      input_fields.map(|target| constrained_before(None, target)),
      members.map(|target| constrained_after(target, None)),
    ),
    (None, Some(input_fields), members) => (
      None,
      Some(constrained_before(clause, input_fields)),
      members.map(|target| constrained_after(target, None)),
    ),
    (None, None, Some(members)) => (None, None, Some(constrained_after(members, clause))),
    (None, None, None) => {
      if clause.is_some() {
        return Err(missing(node, "the tail a where clause constrains"));
      }
      (None, None, None)
    }
  };
  let fields = cursor.keep_opt(fields);
  let input_fields = cursor.keep_opt(input_fields);
  let members = cursor.keep_opt(members);
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
  // `extend schema Directives[Const]? RootOperationTypesDefinition?`
  let mut cursor = Cursor::new(node);
  cursor.keyword(ContextualKeyword::Extend, "the `extend` keyword")?;
  cursor.keyword(ContextualKeyword::Schema, "the `schema` keyword")?;
  let directives_node = cursor.opt_node(K::Directives);
  let roots_node = cursor.opt_node(K::RootOperationTypesDefinition);

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
  let extent = cursor.finish("a token")?;
  Ok((SchemaExtension::new(to_span(extent), data), extent))
}
