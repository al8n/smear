//! `Schema::build`: what it accepts, what it refuses, and the two reductions that would rot
//! silently if nothing pinned them.
//!
//! # The refusal floor is structural
//!
//! [`refusal_floor`] iterates `SchemaErrorKind::ALL` and asserts every kind has a schema in
//! [`FIXTURES`] that makes it fire, with the **exact** set of kinds that schema produces. A kind
//! added without a fixture fails; a fixture that starts tripping a second rule fails; a rule that
//! stops firing fails. The alternative — a builder exercised only on good input — is the defect
//! class this suite exists to close, so the floor is checked rather than reviewed.
//!
//! The kinds with no fixture are excused, each with a written reason, in [`UNFIREABLE`].

// Every fixture and assertion below calls into `smear::validator`, which does not exist in the
// crate's API surface with the feature off. Every other file in this directory gates itself to
// match its real dependency (`rowan`, a dialect, or both); this one did not, so it was a hard
// `E0433` compile error — not a cfg'd-out no-op — under any feature selection that excludes
// `validator`, including plain `--features graphql,parser`.
#![cfg(all(feature = "alloc", feature = "validator"))]
#![allow(missing_docs)]

use smear::{
  diagnostic::{Diagnose as _, Label, Location},
  lexer::{
    SyntaxErrorKind,
    tokora::{Parse as _, Parser, SimpleSpan},
  },
  parser::graphql::{
    GraphQL,
    ast::{
      ConstArgument, ConstArguments, ConstDirective, ConstDirectives, ConstInputValue, ConstList,
      ConstObject, ConstObjectField, DefaultInputValue, Described, InputFieldsDefinition,
      InputObjectTypeDefinition, InputValueDefinition, Name, NamedType, Nested,
      ScalarTypeDefinition, Type, TypeDefinition, TypeSystemDefinition,
      TypeSystemDefinitionOrExtension, TypeSystemDocument,
    },
    error::{ErrorData, GraphqlErrors},
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::schema::{
    DefaultKind, Deprecation, FieldDef, FieldMeta, InputValueDef, InputValueMeta,
    MAX_CONST_VALUE_DEPTH, Range32, RootOperation, Schema, SchemaBuilder, SchemaErrorKind,
    SchemaErrors, Sym, TypeId, TypeKind, builtin,
  },
};

// ---------------------------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------------------------

fn parse(sdl: &str) -> TypeSystemDocument<&str> {
  Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("fixture SDL does not parse: {errors:?}\n---\n{sdl}"))
}

fn build(sdl: &str) -> Result<Schema, SchemaErrors> {
  Schema::build(&parse(sdl))
}

fn built(sdl: &str) -> Schema {
  build(sdl).unwrap_or_else(|errors| panic!("expected a schema, got:\n{errors}\n---\n{sdl}"))
}

fn refused(sdl: &str) -> SchemaErrors {
  match build(sdl) {
    Err(errors) => errors,
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{sdl}"),
  }
}

/// A minimal query root, so a fixture's defect is the only thing wrong with it.
const ROOT: &str = "type Query { ok: Int }\n";

// ---------------------------------------------------------------------------------------------
// the refusal floor
// ---------------------------------------------------------------------------------------------

/// Kinds with no fixture, each excused in writing.
///
/// An excuse list is how a census stops being one, so both entries name a limit of the
/// implementation that no document can reach rather than a rule nobody got round to.
const UNFIREABLE: &[(SchemaErrorKind, &str)] = &[
  (
    SchemaErrorKind::InvalidName,
    "the lexer's identifier rule is the same grammar the arena admits, so no parsed document can \
     carry a name that fails it. `invalid_name_from_a_hand_built_ast` fires it through the other \
     door — a hand-assembled AST — because the AST types are public.",
  ),
  (
    SchemaErrorKind::UnreadableText,
    "no document, parsed or assembled, can carry a string that is not UTF-8. A `&str` source is \
     UTF-8 throughout, the byte lexer refuses a string literal that is not (smear#246) — \
     `ill_formed_text_is_refused_by_the_byte_lexer` pins that at the parse, and \
     `readable_text_from_a_byte_source_is_kept` is its valid twin — and `StringValue` has no \
     public constructor. The kind stays as the text arena's admission rule. What an assembled AST \
     can still carry is a default naming an object field with bytes that are not UTF-8, since \
     `Name::new` is public: the build refuses that name as `InvalidName`, and printing the \
     default trips this kind beside it. `unreadable_text_from_a_hand_built_ast` fires it that \
     way, and it stays excused here because this census is over a table of `&str` documents, \
     which a hand-built AST cannot be a row of.",
  ),
  (
    SchemaErrorKind::TooManyNames,
    "the name index addresses 2^30 symbols; a document that interns more would need gigabytes of \
     distinct identifiers, so the limit is unreachable in a test.",
  ),
  (
    SchemaErrorKind::TooManyInternedBytes,
    "the arena addresses its bytes with 32-bit offsets, so the ceiling is four gigabytes of \
     interned spelling — reachable from a 92 KB hand-assembled AST, whose overlapping suffixes \
     are quadratic, and not from anything a test should allocate. `smear-schema`'s \
     `builder::tests` fires it through `Interner::intern_within`, which is the same mechanism \
     with the two ceilings as parameters, and pins the accepting side beside it.",
  ),
  (
    SchemaErrorKind::PossibleTypeTableTooLarge,
    "the table addresses its words with a 32-bit offset, so the ceiling is four billion `u64` \
     words — 34 GB — and below it the refusal is the host's allocator saying no. `smear-schema`'s \
     `builder::tests` fires it through `possible_table`, which is the same mechanism with the \
     ceiling as a parameter, and pins the accepting side beside it.",
  ),
  (
    SchemaErrorKind::ConstantValueTooDeep,
    "`MAX_CONST_VALUE_DEPTH` is 1024 open containers and it is derived to sit above what every \
     smear door produces — the lossless door at `HARD_MAX` reaches 255 — so no SDL this harness \
     can parse reaches it. `a_literal_at_the_ceiling_is_a_price_and_one_past_it_is_a_refusal` \
     fires it through the other door, a hand-assembled AST, and pins the accepting side beside \
     it.",
  ),
];

// The draft §3 refusal corpus, shared with `validator_lossless_schema.rs`, which compares the two
// schema doors over exactly these documents. This file owns the census that keeps it complete —
// `refusal_floor`, below — and reads the table under its old local name so every assertion here is
// the one that was written against it.
#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::SCHEMA_FIXTURES as FIXTURES;

/// The sub-clause floor: a second SDL for a rule whose census row pins only one of its branches.
///
/// # Why the census above cannot do this
///
/// [`refusal_floor`] iterates an enumeration of **kinds**, and a kind is not a branch. One row per
/// kind is the most a mechanical census can demand, and it is genuinely a floor — but a rule whose
/// guard ranges over more than one input class is pinned on whichever class the fixture's author
/// happened to write.
///
/// That is not a hypothetical. A completeness audit planted nineteen single-branch breakages
/// across this validator; sixteen reddened and three did not, and the clearest of the three was
/// narrowing [`SchemaErrorKind::RootOperationTypeNotObject`]'s guard to the **query** root. Every
/// gate in the repository stayed green — including a differential oracle over six hundred
/// documents — because the census row above happens to be written with a `query` root. The sibling
/// kind [`SchemaErrorKind::UndefinedRootOperationType`] happens to be written with a `mutation`
/// root, which is the only reason the identical planting *there* was caught. Coverage by accident,
/// in both directions.
///
/// # It is systemic, and here is the number
///
/// The audit's nineteen plantings were spread across every rule shape and came back 3 invisible.
/// A second sample, drawn **only** at the shape above — disable exactly one value of an enumerated
/// domain a guard ranges over, leaving every other value working — came back **10 of 10
/// invisible**, every one verified by an SDL the unplanted builder refuses and the planted one
/// accepts, against the full suite *and* the differential oracle with apollo-rs's 682-case corpus.
/// So the audit's 3-in-19 is not the rate for this defect class; it is the rate for a sample that
/// mostly measured something else. Where a rule ranges over `TypeKind`, `DirectiveLocation` or
/// `RootOperation`, a one-fixture census pins one value and nothing pins the rest.
///
/// Those ten are closed below, in the second block. That is not a bound on the population — the
/// sample was not exhaustive and no census here can be, since the branchiness lives in guard
/// predicates rather than in anything an enumeration can iterate. It is a floor that now moves in
/// the right direction: a hole measured is a row added.
///
/// # What a row here has to be
///
/// A branch that was **measured** unreachable, not one that looks thin. Each row below either
/// reddens a planting that the census row did not, or is the other end of a family whose census row
/// leans on one side of an enumerated domain. They run through exactly the assertions the census
/// rows do — [`branch_floor`] and [`every_refusal_names_its_subject`] — and they stay out of
/// [`FIXTURES`] so that the one-row-per-kind census stays a census and this stays a list of holes
/// somebody closed.
#[allow(clippy::type_complexity)]
const BRANCHES: &[(SchemaErrorKind, &str, &[SchemaErrorKind])] = &[
  // -- §3.3, the three root operations ---------------------------------------------------------
  //
  // `RootOperationTypeNotObject` over each root and each non-object kind. The audit's planting was
  // `&& operation == RootOperation::Query`; these are what make it red.
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "type Query { ok: Int } interface M { m: Int } schema { query: Query mutation: M }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "type Query { ok: Int } interface S { s: Int } schema { query: Query subscription: S }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  // The guard is `!= Object`, not `== Interface`: a scalar, a union, an enum and an input object
  // are all equally not object types, and a guard written against the wrong side of that reads the
  // same in the two rows above.
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "type Query { ok: Int } scalar M schema { query: Query mutation: M }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "type Query { ok: Int } union S = Query schema { query: Query subscription: S }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "type Query { ok: Int } input M { a: Int } schema { query: Query mutation: M }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  // The sibling the audit found covered *by accident* — its census row uses a non-query root, so
  // the query branch is the one nothing pins. Same family, other end.
  (
    SchemaErrorKind::UndefinedRootOperationType,
    "type Real { ok: Int } schema { query: Nope }",
    &[SchemaErrorKind::UndefinedRootOperationType],
  ),
  (
    SchemaErrorKind::UndefinedRootOperationType,
    "type Query { ok: Int } schema { query: Query subscription: Nope }",
    &[SchemaErrorKind::UndefinedRootOperationType],
  ),
  // Distinctness holds between *any* two roots, not only against the query one.
  (
    SchemaErrorKind::SharedRootOperationType,
    "type Query { ok: Int } type Both { go: Int }
     schema { query: Query mutation: Both subscription: Both }",
    &[SchemaErrorKind::SharedRootOperationType],
  ),
  // -- §3.6.1 IsSubType(2), the union-membership branch of covariance --------------------------
  //
  // The census row for `InvalidInterfaceFieldType` is `Int` against `String`, which never reaches
  // `is_sub_type` past its `_ => false` arm. The audit's planting was `TypeKind::Union => true`;
  // this row is what makes it red. `valid_interface_implementations_are_accepted` holds the other
  // direction, where the field's type *is* a member.
  (
    SchemaErrorKind::InvalidInterfaceFieldType,
    "type Query { ok: Int }
     type Member { a: Int }
     type Outsider { a: Int }
     union U = Member
     interface I { f: U }
     type T implements I { f: Outsider }",
    &[SchemaErrorKind::InvalidInterfaceFieldType],
  ),
  // -- §3.13's value check, over the coercion arms one scalar row cannot reach -------------------
  //
  // The census row for `InvalidDirectiveArgumentValue` is a string offered to an `Int`. `ID`'s
  // integer arm is a *range* check the audit forced to `true` with nothing noticing — apollo does
  // not range-check `ID`, so no differential can see it. `validator_rules.rs` holds the same branch
  // at the executable door; this is the SDL one, and the two share the coercion table.
  (
    SchemaErrorKind::InvalidDirectiveArgumentValue,
    "directive @d(id: ID) on OBJECT
     type Query @d(id: 99999999999999) { ok: Int }",
    &[SchemaErrorKind::InvalidDirectiveArgumentValue],
  ),
  // -- §3.6.1(2.4.4.1) at the other `InputValueDefinition` position ----------------------------
  (
    SchemaErrorKind::DeprecatedRequiredArgument,
    "type Query { ok: Int } directive @d(a: Int! @deprecated) on OBJECT",
    &[SchemaErrorKind::DeprecatedRequiredArgument],
  ),
  // -- §3.6.1(2.4.5), the coercion arms a single scalar row leaves unpinned ---------------------
  (
    SchemaErrorKind::InvalidDefaultValue,
    "type Query { ok(a: E = NOPE): Int } enum E { A B }",
    &[SchemaErrorKind::InvalidDefaultValue],
  ),
  (
    SchemaErrorKind::InvalidDefaultValue,
    "type Query { ok(a: [Int] = [1, \"two\"]): Int }",
    &[SchemaErrorKind::InvalidDefaultValue],
  ),
  (
    SchemaErrorKind::InvalidDefaultValue,
    "type Query { ok(a: Int! = null): Int }",
    &[SchemaErrorKind::InvalidDefaultValue],
  ),
  (
    SchemaErrorKind::InvalidDefaultValue,
    "type Query { ok: Int } input In { a: Int = \"nope\" }",
    &[SchemaErrorKind::InvalidDefaultValue],
  ),
  (
    SchemaErrorKind::InvalidDefaultValue,
    "type Query { ok: Int } directive @d(a: Int = \"nope\") on OBJECT",
    &[SchemaErrorKind::InvalidDefaultValue],
  ),
  // -- §3.10.1(4), the two shapes a single self-referential row leaves unpinned -----------------
  (
    SchemaErrorKind::InputObjectDefaultValueCycle,
    "type Query { ok: Int } input A { b: B = {} } input B { a: A = {} }",
    &[SchemaErrorKind::InputObjectDefaultValueCycle],
  ),
  (
    SchemaErrorKind::InputObjectDefaultValueCycle,
    "type Query { ok: Int } input In { a: [In] = [{}] }",
    &[SchemaErrorKind::InputObjectDefaultValueCycle],
  ),
  // -- the ten from the second sample ------------------------------------------------------------
  //
  // Every row here closes a planting that came back GREEN against the whole gate set and the wide
  // oracle. They are grouped because they share one shape and were found by one sweep, not because
  // they share a rule: each is a value of an enumerated domain that its kind's census row does not
  // happen to use. See this table's header for the measurement.
  //
  // B1 — `UnsupportedDirectiveLocation` at a location other than `OBJECT`. Twelve locations, one
  // census row. `legal_directive_usages_are_accepted` covers all twelve in the *accepting*
  // direction, which is what made the hole easy to miss: the location word is exercised, the
  // refusal at eleven of the twelve is not.
  (
    SchemaErrorKind::UnsupportedDirectiveLocation,
    "directive @onObject on OBJECT
     type Query { ok: Int }
     enum E { A @onObject }",
    &[SchemaErrorKind::UnsupportedDirectiveLocation],
  ),
  (
    SchemaErrorKind::UnsupportedDirectiveLocation,
    "directive @onObject on OBJECT
     type Query { ok: Int }
     input In { a: Int @onObject }",
    &[SchemaErrorKind::UnsupportedDirectiveLocation],
  ),
  // B2 — `ExtensionKindMismatch` for an extension keyword other than `type`.
  (
    SchemaErrorKind::ExtensionKindMismatch,
    "type Query { ok: Int } type T { a: Int } extend enum T { B }",
    &[SchemaErrorKind::ExtensionKindMismatch],
  ),
  (
    SchemaErrorKind::ExtensionKindMismatch,
    "type Query { ok: Int } enum E { A } extend input E { a: Int }",
    &[SchemaErrorKind::ExtensionKindMismatch],
  ),
  // B3 — `UndefinedExtensionTarget` likewise: the census row is `extend type`.
  (
    SchemaErrorKind::UndefinedExtensionTarget,
    "type Query { ok: Int } extend input Nope { a: Int }",
    &[SchemaErrorKind::UndefinedExtensionTarget],
  ),
  (
    SchemaErrorKind::UndefinedExtensionTarget,
    "type Query { ok: Int } extend union Nope = Query",
    &[SchemaErrorKind::UndefinedExtensionTarget],
  ),
  // B4, B5 — `IsValidImplementationFieldType`'s two structural arms. The census row is a bare
  // `Int` against a bare `String`, which reaches neither: the list arm and the non-null arm both
  // returned `true` unconditionally with nothing noticing.
  (
    SchemaErrorKind::InvalidInterfaceFieldType,
    "type Query { ok: Int } interface I { f: [Int] } type T implements I { f: [String] }",
    &[SchemaErrorKind::InvalidInterfaceFieldType],
  ),
  (
    SchemaErrorKind::InvalidInterfaceFieldType,
    "type Query { ok: Int } interface I { f: Int } type T implements I { f: String! }",
    &[SchemaErrorKind::InvalidInterfaceFieldType],
  ),
  // B6 — `IsSubType`(3), the interface-closure arm. Its union sibling is the P10 row above; this
  // is the same hole one arm over, and `valid_interface_implementations_are_accepted` again covers
  // only the accepting direction.
  (
    SchemaErrorKind::InvalidInterfaceFieldType,
    "type Query { ok: Int }
     interface N { i: Int }
     type A { i: Int }
     interface I { f: N }
     type T implements I { f: A }",
    &[SchemaErrorKind::InvalidInterfaceFieldType],
  ),
  // B7 — `UnionMemberNotObject` for a member that is neither an object nor an interface.
  (
    SchemaErrorKind::UnionMemberNotObject,
    "type Query { ok: Int } enum E { A } union U = E",
    &[SchemaErrorKind::UnionMemberNotObject],
  ),
  // B8 — `ImplementsNonInterface` where the implemented type is not an object either.
  (
    SchemaErrorKind::ImplementsNonInterface,
    "type Query { ok: Int } type A { a: Int } union U = A type T implements U { a: Int }",
    &[SchemaErrorKind::ImplementsNonInterface],
  ),
  // B9 — `ArgumentTypeNotInputType` for an output type other than an object.
  (
    SchemaErrorKind::ArgumentTypeNotInputType,
    "type Query { ok(a: U): Int } type A { a: Int } union U = A",
    &[SchemaErrorKind::ArgumentTypeNotInputType],
  ),
  (
    SchemaErrorKind::ArgumentTypeNotInputType,
    "type Query { ok(a: I): Int } interface I { a: Int } type A implements I { a: Int }",
    &[SchemaErrorKind::ArgumentTypeNotInputType],
  ),
  // -- §3.13 and §3.13.6, where the list is a directive definition's own -----------------------
  //
  // The census rows of these four kinds write their directive, or their extension, against a
  // type. These are the same rules at `DIRECTIVE_DEFINITION`, where the list is the one a
  // directive definition carries and an `extend directive` adds to.
  (
    SchemaErrorKind::UndefinedExtensionTarget,
    "type Query { ok: Int } directive @k on DIRECTIVE_DEFINITION extend directive @nope @k",
    &[SchemaErrorKind::UndefinedExtensionTarget],
  ),
  (
    SchemaErrorKind::UnsupportedDirectiveLocation,
    "type Query { ok: Int } directive @k on FIELD directive @d @k on FIELD",
    &[SchemaErrorKind::UnsupportedDirectiveLocation],
  ),
  (
    SchemaErrorKind::DuplicateDirectiveUse,
    "type Query { ok: Int }
     directive @k on DIRECTIVE_DEFINITION
     directive @d @k on FIELD
     extend directive @d @k",
    &[SchemaErrorKind::DuplicateDirectiveUse],
  ),
  (
    SchemaErrorKind::SelfReferentialDirective,
    "type Query { ok: Int } directive @d @d on DIRECTIVE_DEFINITION",
    &[SchemaErrorKind::SelfReferentialDirective],
  ),
  (
    SchemaErrorKind::SelfReferentialDirective,
    "type Query { ok: Int } directive @d on DIRECTIVE_DEFINITION extend directive @d @d",
    &[SchemaErrorKind::SelfReferentialDirective],
  ),
];

/// Every branch row fires exactly what it says, and every one names a kind the census already
/// covers.
///
/// The second half is what keeps this table from becoming a second census: a row here may only
/// deepen a kind [`FIXTURES`] already has, never stand in for one.
#[test]
fn branch_floor() {
  assert!(
    !BRANCHES.is_empty(),
    "the sub-clause floor is empty; it was seeded from measured plantings and losing them all is \
     not an improvement"
  );
  for (kind, sdl, expected) in BRANCHES {
    assert!(
      FIXTURES.iter().any(|(census, ..)| census == kind),
      "{kind:?} has a branch row but no census row in FIXTURES — a branch is a second fixture, \
       not a first one"
    );
    assert!(
      FIXTURES.iter().all(|(_, census_sdl, _)| census_sdl != sdl),
      "{kind:?}'s branch row is the census row verbatim, so it pins nothing the census did not"
    );
    let errors = refused(sdl);
    let mut want = expected.to_vec();
    want.sort_unstable();
    want.dedup();
    assert_eq!(
      errors.kinds(),
      want,
      "branch row for {kind:?} produced {:?}\n---\n{sdl}",
      errors.kinds()
    );
  }
}

#[test]
fn refusal_floor() {
  let mut missing = Vec::new();
  for kind in SchemaErrorKind::ALL {
    let excused = UNFIREABLE.iter().any(|(excused, _)| excused == kind);
    let has_fixture = FIXTURES.iter().any(|(fixture, ..)| fixture == kind);
    if !has_fixture && !excused {
      missing.push(kind);
    }
    assert!(
      !(has_fixture && excused),
      "{kind:?} is both excused and fixtured; delete the excuse"
    );
  }
  assert!(
    missing.is_empty(),
    "these refusal kinds have no schema that makes them fire: {missing:?} — add one to FIXTURES, \
     or record a written reason in UNFIREABLE"
  );

  // The census must not pass vacuously.
  assert!(
    SchemaErrorKind::ALL.len() >= 66,
    "read only {} kinds; the enumeration is wrong, not the fixtures",
    SchemaErrorKind::ALL.len()
  );

  for (kind, sdl, expected) in FIXTURES {
    let errors = refused(sdl);
    let mut want = expected.to_vec();
    want.sort_unstable();
    want.dedup();
    assert_eq!(
      errors.kinds(),
      want,
      "fixture for {kind:?} produced {:?}\n---\n{sdl}",
      errors.kinds()
    );
    assert!(
      errors.contains_kind(*kind),
      "fixture for {kind:?} did not fire it"
    );
  }
}

/// Every refusal names the artifact it refused.
///
/// A rule that fires without saying what tripped it is a rule an SDL author cannot act on.
#[test]
fn every_refusal_names_its_subject() {
  for (kind, sdl, _) in FIXTURES.iter().chain(BRANCHES) {
    let errors = refused(sdl);
    for error in errors.errors() {
      assert!(
        !error.subject().is_empty(),
        "{kind:?} produced an error with an empty subject: {error}"
      );
      let rendered = error.to_string();
      assert!(
        rendered.contains(error.subject()),
        "{kind:?} renders as {rendered:?}, which does not name its subject {:?}",
        error.subject()
      );
    }
  }
}

/// Spot-check the qualified subjects, since the floor above only checks that one exists.
#[test]
fn subjects_are_qualified_by_their_owner() {
  let errors = refused("type Query { ok: Int } type T { f(a: Int, a: Int): Int }");
  let error = &errors.errors()[0];
  assert_eq!(error.kind(), SchemaErrorKind::DuplicateArgumentName);
  assert_eq!(error.subject(), "a");
  assert_eq!(error.owner(), Some("T.f"));
  assert_eq!(error.to_string(), "duplicate argument: `T.f.a`");
  assert!(
    error.related().is_some(),
    "the first `a` should be pointed at"
  );
}

/// One SDL per owner-path shape the builder renders, with the **exact** text it renders.
///
/// # Why the text and not the parts
///
/// An owner path is assembled from names the builder holds as interned symbols, at a point in a
/// pass chosen for the borrow checker rather than for the diagnostic. Move where a path is built —
/// which any rework of these passes does — and the segments can change without the *kind*, the
/// *subject* or the *span* changing with them: `Query.ok.a` becomes `Query.a`, a directive
/// coordinate loses its `@`, an argument path picks up its field twice. Every other assertion in
/// this file would still pass.
///
/// So the rows below pin the rendered string, and they cover each shape separately rather than
/// each rule: `Owner.field`, `Owner.field.arg`, `Input.field`, `directive.arg`, and the four
/// directive-usage coordinates that a `@` distinguishes from a field path.
const RENDERED: &[(&str, &[&str])] = &[
  // -- unresolved and over-nested bases, whose paths are built in `resolve_type_refs` ----------
  (
    "type Query { ok: Nope }",
    &["undefined type: `Query.ok.Nope`"],
  ),
  (
    "type Query { ok(a: Nope): Int }",
    &["undefined type: `Query.ok.a.Nope`"],
  ),
  (
    "type Query { ok: Int } input In { x: Nope }",
    &["undefined type: `In.x.Nope`"],
  ),
  (
    "type Query { ok: Int } directive @d(a: Nope) on OBJECT",
    &["undefined type: `d.a.Nope`"],
  ),
  (
    "type Query { ok: [[[[[[[[[[[[[[[[Int]]]]]]]]]]]]]]]] }",
    &["type reference nests too deeply: `Query.ok.Int`"],
  ),
  (
    "type Query { ok(a: [[[[[[[[[[[[[[[[Int]]]]]]]]]]]]]]]]): Int }",
    &["type reference nests too deeply: `Query.ok.a.Int`"],
  ),
  // -- fields and arguments --------------------------------------------------------------------
  (
    "type Query { ok: In } input In { x: Int }",
    &["field type is not an output type: `Query.ok.In`"],
  ),
  (
    "type Query { ok(a: Query): Int }",
    &["argument type is not an input type: `Query.ok.a.Query`"],
  ),
  (
    "type Query { ok: Int } type T { f: Int f: Int }",
    &["duplicate field: `T.f`"],
  ),
  (
    "type Query { __ok: Int }",
    &["field name is reserved for introspection: `Query.__ok`"],
  ),
  (
    "type Query { ok(__a: Int): Int }",
    &["argument name is reserved for introspection: `Query.ok.__a`"],
  ),
  (
    "type Query { ok(a: Int, a: Int): Int }",
    &["duplicate argument: `Query.ok.a`"],
  ),
  (
    "type Query { ok(a: Int! @deprecated): Int }",
    &["required argument is deprecated: `Query.ok.a`"],
  ),
  (
    "type Query { ok(a: Int = \"x\"): Int }",
    &["default value does not fit its declared type: `Query.ok.a`"],
  ),
  // -- input objects -----------------------------------------------------------------------------
  (
    "type Query { ok(a: In): Int } input In { x: Query }",
    &["input field type is not an input type: `In.x.Query`"],
  ),
  (
    "type Query { ok(a: In): Int } input In { x: Int = \"s\" }",
    &["default value does not fit its declared type: `In.x`"],
  ),
  (
    "type Query { ok(a: In): Int } input In { x: Int! @deprecated }",
    &["required input field is deprecated: `In.x`"],
  ),
  (
    "type Query { ok(a: In): Int } input In { x: Int x: Int }",
    &["duplicate input field: `In.x`"],
  ),
  (
    "type Query { ok(a: In): Int } input In { __x: Int }",
    &["input field name is reserved for introspection: `In.__x`"],
  ),
  (
    "type Query { ok(a: In): Int } input In @oneOf { x: Int! y: Int = 1 }",
    &[
      "field of a @oneOf input object is non-null: `In.x`",
      "field of a @oneOf input object has a default value: `In.y`",
    ],
  ),
  // -- interfaces, unions, enums -----------------------------------------------------------------
  (
    "type Query implements Nope { ok: Int }",
    &["implemented interface is not defined: `Query.Nope`"],
  ),
  (
    "type Query implements I & I { ok: Int } interface I { ok: Int }",
    &["duplicate implemented interface: `Query.I`"],
  ),
  (
    "type Query { ok: U } union U = Nope",
    &["union member is not defined: `U.Nope`"],
  ),
  (
    "type Query { ok: U } union U = Query | Query",
    &["duplicate union member: `U.Query`"],
  ),
  (
    "type Query { ok: E } enum E { A A }",
    &["duplicate enum value: `E.A`"],
  ),
  (
    "type Query { ok: E } enum E { __A }",
    &["enum value name is reserved for introspection: `E.__A`"],
  ),
  // -- directive definitions, whose owner is the bare directive name -----------------------------
  (
    "type Query { ok: Int } directive @d(a: Query) on OBJECT",
    &["directive argument type is not an input type: `d.a.Query`"],
  ),
  (
    "type Query { ok: Int } directive @d(a: Int, a: Int) on OBJECT",
    &["duplicate directive argument: `d.a`"],
  ),
  (
    "type Query { ok: Int } directive @d(__a: Int) on OBJECT",
    &["directive argument name is reserved for introspection: `d.__a`"],
  ),
  // -- directive usages, once per element `check_directive_uses` is called for --------------------
  (
    "directive @d on OBJECT\nschema @d { query: Query }\ntype Query { ok: Int }",
    &["directive is not allowed here: `schema.d`"],
  ),
  (
    "type Query @nope { ok: Int }",
    &["undefined directive: `Query.nope`"],
  ),
  (
    "type Query { ok: Int @nope }",
    &["undefined directive: `Query.ok.nope`"],
  ),
  (
    "type Query { ok(a: Int @nope): Int }",
    &["undefined directive: `Query.ok.a.nope`"],
  ),
  (
    "type Query { ok(a: In): Int } input In { x: Int @nope }",
    &["undefined directive: `In.x.nope`"],
  ),
  (
    "type Query { ok: E } enum E { A @nope }",
    &["undefined directive: `E.A.nope`"],
  ),
  (
    "type Query { ok: Int } directive @d(a: Int @nope) on OBJECT",
    &["undefined directive: `d.a.nope`"],
  ),
  (
    "type Query { ok: Int } directive @d @nope on FIELD",
    &["undefined directive: `d.nope`"],
  ),
  (
    "directive @d on SCALAR\ntype Query { ok: Int @d }",
    &["directive is not allowed here: `Query.ok.d`"],
  ),
  (
    "directive @d on FIELD_DEFINITION\ntype Query { ok: Int @d @d }",
    &["directive is not repeatable: `Query.ok.d`"],
  ),
  // -- directive-usage arguments: the `@` coordinate, at the deepest owner there is ---------------
  (
    "directive @d(a: Int) on ARGUMENT_DEFINITION\ntype Query { ok(b: Int @d(c: 1)): Int }",
    &["undefined directive argument: `Query.ok.b.@d.c`"],
  ),
  (
    "directive @d(a: Int!) on ARGUMENT_DEFINITION\ntype Query { ok(b: Int @d): Int }",
    &["required directive argument is missing: `Query.ok.b.@d.a`"],
  ),
  (
    "directive @d(a: Int!) on ARGUMENT_DEFINITION\ntype Query { ok(b: Int @d(a: null)): Int }",
    &["required directive argument is missing: `Query.ok.b.@d.a`"],
  ),
  (
    "directive @d(a: Int) on ARGUMENT_DEFINITION\ntype Query { ok(b: Int @d(a: \"x\")): Int }",
    &["directive argument value does not fit its declared type: `Query.ok.b.@d.a`"],
  ),
  (
    "directive @d(a: Int) on ARGUMENT_DEFINITION\ntype Query { ok(b: Int @d(a: 1, a: 2)): Int }",
    &["directive argument is passed twice: `Query.ok.b.@d.a`"],
  ),
  (
    "directive @k(a: Int) on DIRECTIVE_DEFINITION
     directive @d @k(a: \"x\") on FIELD
     type Query { ok: Int }",
    &["directive argument value does not fit its declared type: `d.@k.a`"],
  ),
];

/// Every row of [`RENDERED`] renders exactly what it says, in exactly that order.
#[test]
fn refusals_render_exactly() {
  for (sdl, expected) in RENDERED {
    let errors = refused(sdl);
    let rendered: Vec<String> = errors.errors().iter().map(ToString::to_string).collect();
    assert_eq!(rendered, *expected, "---\n{sdl}");
  }
}

// ---------------------------------------------------------------------------------------------
// directive usages: the accepted direction
// ---------------------------------------------------------------------------------------------

/// Every legal shape of directive usage, at every type-system location.
///
/// The floor above proves the six use-site refusals fire. This is the direction a refusal-only
/// suite cannot see, and the one that matters more: a false positive in `Schema::build` refuses a
/// schema no other implementation refuses, and there is no later stage to overrule it.
#[test]
fn legal_directive_usages_are_accepted() {
  // Each of the twelve type-system locations, with a directive that lists exactly it.
  built(
    "directive @onSchema on SCHEMA
     directive @onScalar on SCALAR
     directive @onObject on OBJECT
     directive @onFieldDef on FIELD_DEFINITION
     directive @onArgDef on ARGUMENT_DEFINITION
     directive @onInterface on INTERFACE
     directive @onUnion on UNION
     directive @onEnum on ENUM
     directive @onEnumValue on ENUM_VALUE
     directive @onInputObject on INPUT_OBJECT
     directive @onInputField on INPUT_FIELD_DEFINITION
     directive @onDirectiveDef on DIRECTIVE_DEFINITION
     schema @onSchema { query: Query }
     type Query @onObject { ok(a: Int @onArgDef): Int @onFieldDef }
     scalar Custom @onScalar
     interface I @onInterface { ok: Int }
     type Impl implements I @onObject { ok: Int }
     union U @onUnion = Query | Impl
     enum E @onEnum { A @onEnumValue }
     input In @onInputObject { a: Int @onInputField }
     directive @annotated @onDirectiveDef on FIELD",
  );

  // The same location reached through an extension, including `extend schema` and
  // `extend directive`.
  built(
    "directive @onSchema repeatable on SCHEMA
     directive @onObject on OBJECT
     directive @onEnumValue on ENUM_VALUE
     directive @onDirectiveDef on DIRECTIVE_DEFINITION
     schema @onSchema { query: Query }
     extend schema @onSchema
     type Query { ok: Int }
     type T @onObject { a: Int }
     enum E { A }
     extend enum E { B @onEnumValue }
     directive @annotated on FIELD
     extend directive @annotated @onDirectiveDef",
  );

  // `repeatable` means what it says.
  built("directive @r repeatable on OBJECT type Query @r @r @r { ok: Int }");

  // The five specified directives, each at a location its own definition lists.
  built(
    "type Query { legacy: Int @deprecated old(a: Int @deprecated): Int }
     scalar UUID @specifiedBy(url: \"https://example.invalid\")
     input Filter @oneOf { a: Int b: Int }
     input Legacy { a: Int @deprecated }
     enum E { A @deprecated(reason: \"gone\") B }",
  );

  // Every coercion the specification grants a constant argument value, and none it does not.
  built(
    "directive @v(
       i: Int
       f: Float
       fromInt: Float
       s: String
       b: Boolean
       idText: ID
       idNumber: ID
       list: [Int]
       nested: [[Int]]
       deepSingleton: [[Int]]
       obj: Point
       e: Unit
       custom: Custom
       nullable: Int
       defaulted: Int = 1
     ) on OBJECT
     scalar Custom
     input Point { x: Int y: Int }
     enum Unit { METER FOOT }
     type Query @v(
       i: -2147483648
       f: 1.5
       fromInt: 2
       s: \"x\"
       b: true
       idText: \"abc\"
       idNumber: 4
       list: 1
       nested: [[1], []]
       deepSingleton: 7
       obj: { x: 1 }
       e: METER
       custom: { anything: [1, true, NOT_AN_ENUM] }
       nullable: null
     ) { ok: Int }",
  );
}

/// A type and its extensions are one location for the repeatability rule.
#[test]
fn a_type_and_its_extensions_are_one_directive_location() {
  let errors = refused(
    "directive @d on OBJECT
     type Query @d { ok: Int }
     extend type Query @d",
  );
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::DuplicateDirectiveUse]);
  assert_eq!(errors.errors()[0].subject(), "d");
  assert!(
    errors.errors()[0].related().is_some(),
    "the first application should be pointed at"
  );
}

/// Draft §3.13 gives a directive definition `Directives[Const]?`, applied at
/// `DIRECTIVE_DEFINITION`, and §3.13.6's `extend directive` adds to them: the accepting direction
/// of every rule the tests after this one refuse with.
#[test]
fn directives_on_a_directive_definition_are_accepted() {
  // On the definition, between its arguments and `repeatable`, at the location the applied
  // directive's own definition lists.
  built(
    "type Query { ok: Int }
     directive @meta(level: Int) on DIRECTIVE_DEFINITION
     directive @d(a: Int) @meta(level: 1) repeatable on FIELD",
  );
  // Through an extension: of a directive the document defines, written before that definition,
  // and of one the specification provides.
  built(
    "type Query { ok: Int }
     extend directive @d @meta
     directive @meta on DIRECTIVE_DEFINITION
     directive @d on FIELD
     extend directive @deprecated @meta",
  );
  // A `repeatable` directive, by the definition and again by every extension.
  built(
    "type Query { ok: Int }
     directive @tag(name: String!) repeatable on DIRECTIVE_DEFINITION
     directive @d @tag(name: \"a\") on FIELD
     extend directive @d @tag(name: \"b\") @tag(name: \"c\")
     extend directive @d @tag(name: \"d\")",
  );
  // A non-repeatable one once on each of two definitions, which are two locations.
  built(
    "type Query { ok: Int }
     directive @k on DIRECTIVE_DEFINITION
     directive @d @k on FIELD
     directive @e on FIELD
     extend directive @e @k",
  );
  // A chain that never comes back is not a cycle, whether an extension or a type an argument
  // accepts is one of its links.
  built(
    "type Query { ok: Int }
     directive @a on DIRECTIVE_DEFINITION
     directive @b @a on DIRECTIVE_DEFINITION
     directive @c on DIRECTIVE_DEFINITION
     extend directive @c @b",
  );
  built(
    "type Query { ok: Int }
     directive @a(x: In) on DIRECTIVE_DEFINITION
     directive @b on INPUT_FIELD_DEFINITION
     input In { f: Int @b }
     directive @c @a(x: { f: 1 }) on FIELD",
  );
}

/// A directive written on a directive definition, or provided by an `extend directive`, is a usage
/// at `DIRECTIVE_DEFINITION`, held to every rule a usage anywhere else is held to.
#[test]
fn a_directive_on_a_directive_definition_is_checked_at_its_location() {
  // Not listed there: refused at the usage, on the definition and through an extension alike, and
  // owned by the directive it is written on.
  for sdl in [
    "type Query { ok: Int } directive @k on FIELD directive @d @k on FIELD",
    "type Query { ok: Int } directive @k on FIELD directive @d on FIELD extend directive @d @k",
  ] {
    let errors = refused(sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::UnsupportedDirectiveLocation],
      "{sdl}"
    );
    assert_eq!(errors.len(), 1, "{errors}");
    let error = &errors.errors()[0];
    assert_eq!(error.to_string(), "directive is not allowed here: `d.k`");
    assert_eq!(
      error.span().start(),
      sdl.rfind("@k").expect("the usage") + 1,
      "{sdl}"
    );
  }

  // No other location stands in for it: a directive listing all nineteen of the others is still
  // refused there.
  let errors = refused(
    "type Query { ok: Int }
     directive @k on QUERY | MUTATION | SUBSCRIPTION | FIELD | FRAGMENT_DEFINITION
       | FRAGMENT_SPREAD | INLINE_FRAGMENT | VARIABLE_DEFINITION | SCHEMA | SCALAR | OBJECT
       | FIELD_DEFINITION | ARGUMENT_DEFINITION | INTERFACE | UNION | ENUM | ENUM_VALUE
       | INPUT_OBJECT | INPUT_FIELD_DEFINITION
     directive @d @k on FIELD",
  );
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::UnsupportedDirectiveLocation]
  );

  // Undefined, and missing a required argument, exactly as on any other element.
  let errors = refused("type Query { ok: Int } directive @d @nope on FIELD");
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::UndefinedDirective]);
  let errors = refused(
    "type Query { ok: Int }
     directive @k(level: Int!) on DIRECTIVE_DEFINITION
     directive @d on FIELD
     extend directive @d @k",
  );
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingRequiredDirectiveArgument]
  );
  assert_eq!(errors.errors()[0].owner(), Some("d.@k"));
}

/// Draft §3.13.6: "Any non-repeatable directives provided must not already apply to the previous
/// directive." A directive definition and its extensions are one location, as a type and its
/// extensions are.
#[test]
fn a_directive_definition_and_its_extensions_are_one_directive_location() {
  let sdl = "type Query { ok: Int }
     directive @k on DIRECTIVE_DEFINITION
     directive @d @k on FIELD
     extend directive @d @k";
  let errors = refused(sdl);
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::DuplicateDirectiveUse]);
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.to_string(), "directive is not repeatable: `d.k`");
  // At the extension's `@k`, pointing back at the definition's.
  assert_eq!(
    error.span().start(),
    sdl.rfind("@k").expect("the extension's") + 1
  );
  let first = sdl.find("@d @k").expect("the definition's") + 4;
  assert_eq!(error.related(), Some(SimpleSpan::new(first, first + 1)));

  // Two extensions repeat it between them, and one extension within itself.
  for sdl in [
    "type Query { ok: Int } directive @k on DIRECTIVE_DEFINITION directive @d on FIELD
     extend directive @d @k extend directive @d @k",
    "type Query { ok: Int } directive @k on DIRECTIVE_DEFINITION directive @d on FIELD
     extend directive @d @k @k",
  ] {
    let errors = refused(sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::DuplicateDirectiveUse],
      "{sdl}"
    );
    assert_eq!(errors.len(), 1, "{errors}");
  }

  // The positive control: declared `repeatable`, the same three shapes build.
  for sdl in [
    "type Query { ok: Int } directive @k repeatable on DIRECTIVE_DEFINITION
     directive @d @k on FIELD extend directive @d @k",
    "type Query { ok: Int } directive @k repeatable on DIRECTIVE_DEFINITION directive @d on FIELD
     extend directive @d @k extend directive @d @k",
    "type Query { ok: Int } directive @k repeatable on DIRECTIVE_DEFINITION directive @d on FIELD
     extend directive @d @k @k",
  ] {
    built(sdl);
  }
}

/// Draft §3.13.6: "The previous directive must already be defined."
#[test]
fn an_extension_of_an_undefined_directive_is_refused() {
  let sdl = "type Query { ok: Int } directive @k on DIRECTIVE_DEFINITION extend directive @nope @k";
  let errors = refused(sdl);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::UndefinedExtensionTarget]
  );
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.subject(), "nope");
  assert_eq!(error.owner(), None);
  assert_eq!(&sdl[error.span().start()..error.span().end()], "nope");
  assert_eq!(error.to_string(), "extension target is not defined: `nope`");

  // A type is not a directive, nor a directive a type: the two names live apart.
  for sdl in [
    "type Query { ok: Int } directive @k on DIRECTIVE_DEFINITION extend directive @Query @k",
    "type Query { ok: Int } directive @d on FIELD extend type d { more: Int }",
  ] {
    let errors = refused(sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::UndefinedExtensionTarget],
      "{sdl}"
    );
  }

  // The directives of an extension with nothing to extend are not checked against a definition it
  // does not have: one mistake, one diagnostic, as for a type extension.
  let errors = refused("type Query { ok: Int } extend directive @nope @alsoNope");
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::UndefinedExtensionTarget]
  );
  assert_eq!(errors.len(), 1, "{errors}");

  // The positive control: defined, the same extension builds.
  built(
    "type Query { ok: Int }
     directive @k on DIRECTIVE_DEFINITION
     directive @nope on FIELD
     extend directive @nope @k",
  );
}

/// Draft §3.13's "must not contain the use of a Directive which references itself directly" and
/// "indirectly", through the directives on a definition and on its extensions. §3.13.6's rules 3
/// and 4 ask the same of an extension's directives, and get the same answer: the extension's
/// directives are the definition's, so one check is both.
#[test]
fn a_directive_reaching_itself_through_its_definitions_directives_is_refused() {
  let cases: &[(&str, &[&str])] = &[
    // Directly, on the definition and through an extension.
    ("directive @d @d on DIRECTIVE_DEFINITION", &["d"]),
    (
      "directive @d on DIRECTIVE_DEFINITION extend directive @d @d",
      &["d"],
    ),
    // Indirectly, through another directive's definition, and with an extension closing the loop.
    (
      "directive @a @b on DIRECTIVE_DEFINITION directive @b @a on DIRECTIVE_DEFINITION",
      &["a", "b"],
    ),
    (
      "directive @a on DIRECTIVE_DEFINITION
       directive @b @a on DIRECTIVE_DEFINITION
       extend directive @a @b",
      &["a", "b"],
    ),
    // Indirectly, through a type an argument accepts, closed on a definition and by an extension.
    (
      "directive @a(x: In) on DIRECTIVE_DEFINITION
       directive @b @a on INPUT_FIELD_DEFINITION
       input In { f: Int @b }",
      &["a", "b"],
    ),
    (
      "directive @a(x: In) on DIRECTIVE_DEFINITION
       directive @b on INPUT_FIELD_DEFINITION
       input In { f: Int @b }
       extend directive @b @a",
      &["a", "b"],
    ),
  ];
  for (sdl, cyclic) in cases {
    let sdl = format!("{ROOT}{sdl}");
    let errors = refused(&sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::SelfReferentialDirective],
      "{sdl}"
    );
    let blamed: Vec<&str> = errors
      .errors()
      .iter()
      .map(|error| error.subject())
      .collect();
    assert_eq!(blamed, *cyclic, "{sdl}");
  }

  // Blamed on the definition, where the cycle is, even when an extension closed it.
  let sdl = "type Query { ok: Int } directive @d on DIRECTIVE_DEFINITION extend directive @d @d";
  let errors = refused(sdl);
  assert_eq!(
    errors.errors()[0].span().start(),
    sdl.find("@d").expect("the definition") + 1
  );

  // A directive that merely uses a self-referential one is not on the cycle, so it is not blamed.
  let errors = refused(
    "type Query { ok: Int }
     directive @a @a on DIRECTIVE_DEFINITION
     directive @b @a on DIRECTIVE_DEFINITION",
  );
  let blamed: Vec<&str> = errors
    .errors()
    .iter()
    .map(|error| error.subject())
    .collect();
  assert_eq!(blamed, ["a"]);
}

/// Use-site refusals name the element the directive sits on, and the `@name` its arguments belong
/// to.
#[test]
fn directive_usage_errors_name_their_coordinate() {
  let cases: &[(&str, SchemaErrorKind, &str, &str)] = &[
    (
      "type Query @nowhere { ok: Int }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "Query",
    ),
    (
      "type Query { ok: Int @nowhere }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "Query.ok",
    ),
    (
      "type Query { ok(a: Int @nowhere): Int }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "Query.ok.a",
    ),
    (
      "type Query { ok: Int } enum E { A @nowhere }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "E.A",
    ),
    (
      "type Query { ok: Int } input In { a: Int @nowhere }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "In.a",
    ),
    (
      "type Query { ok: Int } directive @d(a: Int @nowhere) on OBJECT",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "d.a",
    ),
    (
      "schema @nowhere { query: Query } type Query { ok: Int }",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "schema",
    ),
    (
      "type Query { ok: Int } directive @d on FIELD extend directive @d @nowhere",
      SchemaErrorKind::UndefinedDirective,
      "nowhere",
      "d",
    ),
    (
      "directive @meta(a: Int) on DIRECTIVE_DEFINITION
       directive @d @meta(b: 1) on FIELD
       type Query { ok: Int }",
      SchemaErrorKind::UndefinedDirectiveArgument,
      "b",
      "d.@meta",
    ),
    (
      "directive @onObject(a: Int) on OBJECT
       type Query @onObject(b: 1) { ok: Int }",
      SchemaErrorKind::UndefinedDirectiveArgument,
      "b",
      "Query.@onObject",
    ),
    (
      "directive @onObject(a: Int!) on OBJECT
       type Query @onObject(a: null) { ok: Int }",
      SchemaErrorKind::MissingRequiredDirectiveArgument,
      "a",
      "Query.@onObject",
    ),
    (
      "directive @onObject(a: Int) on OBJECT
       type Query @onObject(a: \"x\") { ok: Int }",
      SchemaErrorKind::InvalidDirectiveArgumentValue,
      "a",
      "Query.@onObject",
    ),
    (
      "directive @onObject(a: Int) on OBJECT
       type Query @onObject(a: 1, a: 2) { ok: Int }",
      SchemaErrorKind::DuplicateDirectiveArgumentUse,
      "a",
      "Query.@onObject",
    ),
    // The two input-object kinds are named by the input object, not by the argument that carries
    // the literal: `In.y` is the coordinate apollo prints too, and it is the only one a nested
    // literal can be given unambiguously.
    (
      "directive @onObject(a: In) on OBJECT
       input In { x: Int }
       type Query @onObject(a: { y: 1 }) { ok: Int }",
      SchemaErrorKind::UndefinedInputObjectField,
      "y",
      "In",
    ),
    (
      "directive @onObject(a: In) on OBJECT
       input In { x: Int! }
       type Query @onObject(a: {}) { ok: Int }",
      SchemaErrorKind::MissingRequiredInputObjectField,
      "x",
      "In",
    ),
  ];

  for (sdl, kind, subject, owner) in cases {
    let errors = refused(sdl);
    assert_eq!(errors.kinds(), vec![*kind], "{sdl}");
    let error = &errors.errors()[0];
    assert_eq!(error.subject(), *subject, "{sdl}");
    assert_eq!(error.owner(), Some(*owner), "{sdl}");
    assert_eq!(
      error.to_string(),
      format!("{}: `{owner}.{subject}`", kind.message()),
      "{sdl}"
    );
  }
}

/// An omitted required argument has no argument to point at, so the usage is what is blamed.
#[test]
fn an_omitted_required_argument_is_blamed_on_the_usage() {
  const SDL: &str = "directive @onObject(a: Int!) on OBJECT\ntype Query @onObject { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingRequiredDirectiveArgument]
  );
  let span = errors.errors()[0].span();
  let start = SDL.find("@onObject { ok").expect("the usage is in the SDL");
  assert_eq!(
    &SDL[span.start()..span.end()],
    "@onObject",
    "the span should cover the usage, at byte {start}"
  );
}

/// Draft 5.6.4's SDL twin claims an explicit `null` as well as an omission.
///
/// A required input field written `null` is one mistake, and the diagnostic that names it is the
/// obligation rather than the coercion — the same choice `check_directive_arguments` makes for a
/// required *argument* written `null`. `apollo-compiler` reports both `RequiredField` and
/// `UnsupportedValueType` for this SDL; the verdicts agree either way, and reporting one is what
/// keeps a single defect from printing twice.
#[test]
fn an_explicit_null_for_a_required_input_field_is_the_obligation() {
  const SDL: &str = "directive @onObject(a: In) on OBJECT
input In { x: Int! }
type Query @onObject(a: { x: null }) { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingRequiredInputObjectField]
  );
  let error = &errors.errors()[0];
  assert_eq!(error.owner(), Some("In"));
  assert_eq!(error.subject(), "x");
  let span = error.span();
  assert_eq!(&SDL[span.start()..span.end()], "null");

  // A field with a default is not required, so the same `null` is an ordinary coercion failure.
  let with_default = SDL.replace("x: Int!", "x: Int! = 3");
  assert_eq!(
    refused(&with_default).kinds(),
    vec![SchemaErrorKind::InvalidDirectiveArgumentValue],
    "a defaulted non-null field is not a required field"
  );
}

/// An omitted required input field has no field to point at, so the literal is what is blamed.
#[test]
fn an_omitted_required_input_field_is_blamed_on_the_literal() {
  const SDL: &str = "directive @onObject(a: In) on OBJECT
input In { x: Int! }
type Query @onObject(a: {}) { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingRequiredInputObjectField]
  );
  let span = errors.errors()[0].span();
  assert_eq!(&SDL[span.start()..span.end()], "{}");
}

/// A nested input object is named by *its* type, which is the whole reason the owner is not the
/// argument coordinate.
#[test]
fn a_nested_input_object_is_named_by_its_own_type() {
  const SDL: &str = "directive @onObject(a: In) on OBJECT
input Inner { z: Int }
input In { x: Inner }
type Query @onObject(a: { x: { nope: 1 } }) { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::UndefinedInputObjectField]
  );
  let error = &errors.errors()[0];
  assert_eq!(error.owner(), Some("Inner"));
  assert_eq!(error.subject(), "nope");
  let span = error.span();
  assert_eq!(&SDL[span.start()..span.end()], "nope");
}

/// A repeated argument is reported once per repeat, and points back at the first one.
///
/// The definition is not consulted: an argument written twice is a mistake whether or not the
/// directive declares it, so an *undefined* argument written twice produces both diagnostics.
#[test]
fn a_repeated_directive_argument_names_the_first() {
  const SDL: &str = "directive @onObject(a: Int) on OBJECT
type Query @onObject(a: 1, a: 2, a: 3) { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::DuplicateDirectiveArgumentUse]
  );
  assert_eq!(
    errors.len(),
    2,
    "one diagnostic per repeat, not one in total"
  );
  let first = SDL.find("a: 1").expect("the first argument is in the SDL");
  for error in errors.errors() {
    assert_eq!(
      error.related(),
      Some(SimpleSpan::const_new(first, first + 1))
    );
  }

  let undefined = "directive @onObject(a: Int) on OBJECT
type Query @onObject(b: 1, b: 2) { ok: Int }";
  assert_eq!(
    refused(undefined).kinds(),
    vec![
      SchemaErrorKind::UndefinedDirectiveArgument,
      SchemaErrorKind::DuplicateDirectiveArgumentUse,
    ],
    "an undefined argument written twice is both mistakes"
  );
}

/// A duplicate reads the same on both sides of the width that changes how it is found.
///
/// The duplicate scan has two representations and the list's width alone chooses between them: a
/// narrow list is scanned, a wide one resolves its repeats through a sorted index. That boundary
/// is invisible from out here and has to stay that way — same kind, same count, same span, and
/// the same *related* span, which is the FIRST occurrence and not the nearest earlier one.
/// Nothing else in this suite reaches the wide representation: every other fixture is a handful
/// of lines. al8n/smear#198.
#[test]
fn a_duplicate_reads_the_same_at_every_list_width() {
  fn fixture(filler: usize) -> String {
    let mut sdl = String::from("type Query {\n  dup: Int\n");
    for i in 0..filler {
      sdl.push_str(&format!("  f{i}: Int\n"));
    }
    sdl.push_str("  dup: Int\n}\n");
    sdl
  }

  // 62 fillers is a list of 64 and 63 is a list of 65, which is the boundary itself.
  for filler in [0, 1, 62, 63, 64, 500] {
    let sdl = fixture(filler);
    let errors = refused(&sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::DuplicateFieldName],
      "filler {filler}"
    );
    assert_eq!(
      errors.len(),
      1,
      "one diagnostic per repeat, filler {filler}"
    );

    let error = &errors.errors()[0];
    let first = sdl.find("dup").expect("the first occurrence is written");
    assert_eq!(
      error.related(),
      Some(SimpleSpan::const_new(first, first + 3)),
      "the related span is the first occurrence, filler {filler}"
    );
    let second = sdl.rfind("dup").expect("the second occurrence is written");
    let span = error.span();
    assert_eq!(
      (span.start(), span.end()),
      (second, second + 3),
      "the diagnostic is at the repeat, filler {filler}"
    );
  }
}

/// A wide list relates every repeat to the first occurrence and reports them in source order.
///
/// The index that finds them is built by sorting, and a sorted order is not the document's: an
/// index that answered out of the sort would relate a repeat to whichever member of its group
/// came first *there*, and would emit the groups by name. Both are checked here because both are
/// invisible in a fixture narrow enough to be scanned.
#[test]
fn a_wide_list_relates_each_repeat_to_the_first_in_source_order() {
  let mut sdl = String::from("type Query {\n  alpha: Int\n  beta: Int\n");
  for i in 0..200 {
    sdl.push_str(&format!("  f{i}: Int\n"));
  }
  sdl.push_str("  beta: Int\n  alpha: Int\n  alpha: Int\n}\n");

  let errors = refused(&sdl);
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::DuplicateFieldName]);
  assert_eq!(errors.len(), 3, "one diagnostic per repeat, not per group");

  let alpha = sdl.find("alpha").expect("`alpha` is written");
  let beta = sdl.find("beta").expect("`beta` is written");
  let related: Vec<_> = errors
    .errors()
    .iter()
    .map(|error| error.related().map(|span| span.start()))
    .collect();
  assert_eq!(
    related,
    vec![Some(beta), Some(alpha), Some(alpha)],
    "every repeat relates to the first occurrence, and the repeats arrive in source order"
  );
}

/// A nested literal is blamed where it is written, not on the argument that contains it.
#[test]
fn a_nested_bad_literal_is_blamed_in_place() {
  const SDL: &str = "directive @v(p: [Point]) on OBJECT
input Point { x: Int }
type Query @v(p: [{ x: 1 }, { x: \"no\" }]) { ok: Int }";
  let errors = refused(SDL);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::InvalidDirectiveArgumentValue]
  );
  let error = &errors.errors()[0];
  assert_eq!(error.owner(), Some("Query.@v"));
  assert_eq!(error.subject(), "p");
  let span = error.span();
  assert_eq!(&SDL[span.start()..span.end()], "\"no\"");
}

/// A literal deeper than any parse can carry is reduced, checked and released without recursing.
///
/// # What was wrong
///
/// `SchemaBuilder::const_value` descended a constant literal one native frame per level, on the
/// argument that the AST came from a recursive-descent parser and would be dropped recursively
/// anyway. Neither half holds. The route below is public — a scalar definition, a const directive,
/// a const argument, a const list — so the literal is built with a loop and no parser sees it; and
/// al8n/smear#199 gave the value tree an iterative release, so the drop that was supposed to give
/// out first no longer does. `type_ref`, `check_const_value`, `map_nodes` and `RawValue`'s own
/// release were the same shape, each one the next to give out as the one in front of it was
/// repaired.
///
/// Measured at `8b73965`, `aarch64-apple-darwin`, unoptimised, one child process per depth with the
/// document built on another thread: `Schema::build` over exactly this shape aborted at **1 545**
/// levels on a 2 MiB thread — libtest's own — 389 on 512 KiB and 196 on 256 KiB.
///
/// # There is no red side here
///
/// A stack overflow is `SIGABRT`, which no `#[should_panic]` sees and which takes the harness with
/// it, so what this pins is the green side: the build returns on a stack far too small to have held
/// the recursion. The fixture picks its own stack for the reason `ast_release.rs` does — on
/// libtest's 2 MiB the old boundary was 1 545 and a 20 000-level fixture would have proved the same
/// thing more expensively, while a 512 KiB thread puts it fifty times past the boundary and costs
/// nothing, since a walk that does not recurse needs no more stack at 20 000 levels than at one.
/// `Schema::build` has a fixed floor of about 160 KiB of its own — it parses the built-in SDL — so
/// the thread cannot be smaller than that whatever the literal does.
#[test]
fn a_hand_built_literal_deeper_than_any_parse_is_reduced_without_recursing() {
  /// Fifty times the depth at which this aborted on [`STACK`] before the repair.
  const DEPTH: usize = 20_000;
  /// Above `Schema::build`'s own fixed floor and far below what the old recursion needed.
  const STACK: usize = 512 * 1024;

  std::thread::Builder::new()
    .stack_size(STACK)
    .spawn(|| {
      let span = SimpleSpan::const_new(0, 0);
      let empty = || ConstInputValue::List(ConstList::new(span, Nested::empty()));
      let mut value = empty();
      for _ in 0..DEPTH {
        value = ConstInputValue::List(ConstList::new(span, Nested::new(vec![value])));
      }

      let directive = ConstDirective::new(
        span,
        Name::new(span, "x"),
        Some(ConstArguments::new(
          span,
          vec![ConstArgument::new(span, Name::new(span, "a"), value)],
        )),
      );
      let scalar = ScalarTypeDefinition::new(
        span,
        Name::new(span, "Foo"),
        Some(ConstDirectives::new(span, vec![directive])),
      );
      let document = TypeSystemDocument::<&str>::new(
        span,
        vec![TypeSystemDefinitionOrExtension::Definition(Described::new(
          span,
          None,
          TypeSystemDefinition::Type(TypeDefinition::Scalar(scalar)),
        ))],
      );

      // The verdict is beside the point — `@x` is undefined, so this refuses — and the point is
      // that it refuses at all instead of aborting, and that the `RawValue` it built is released on
      // the way out without a frame per level either.
      let refused = Schema::build(&document).expect_err("`@x` is not a defined directive");
      assert!(
        refused
          .iter()
          .any(|error| matches!(error.kind(), SchemaErrorKind::UndefinedDirective)),
        "{refused}"
      );
      // And past `MAX_CONST_VALUE_DEPTH` the depth itself is reported, which is what makes the
      // build's answer a refusal rather than a truncation nobody was told about.
      assert!(
        refused
          .iter()
          .any(|error| matches!(error.kind(), SchemaErrorKind::ConstantValueTooDeep)),
        "{refused}"
      );
    })
    .expect("a fixture thread")
    .join()
    .expect("the fixture thread returned");
}

/// Exactly at the ceiling a literal is built; one container past it the build refuses.
///
/// # Why a ceiling at all, once the walk is a loop
///
/// Making the reduction iterative moved its growth from the native stack to the heap, and that was
/// the point — but `frames.push` and `Vec::with_capacity` are **infallible**, so a caller-built
/// literal deep enough still ended the process instead of the build. Measured out of suite at
/// `96881f2`, `aarch64-apple-darwin`, unoptimised, as peak live bytes across `Schema::build` with
/// the document built before the instrument was armed: 138 to 224 bytes for every container of a
/// list chain and 162 to 248 for an object one, with no ceiling anywhere in it.
///
/// `Schema::build` returns a `Result`, so the answer here is a refusal rather than a documented
/// bound — the thing al8n/smear#199 could not do inside a `Drop`. With the ceiling in, the same
/// measurement reads **140 007 bytes for a list chain of 1 025 containers, of 2 001, of 20 001 and
/// of 200 001** — one number, not a slope — against a 67 940-byte build with no literal in it.
///
/// # Why both halves are here
///
/// A bound that only ever refuses is indistinguishable from a prohibition. The accepting half is
/// what says the ceiling is a price: 1 024 containers — four times the 255 the lossless door at
/// `HARD_MAX` produces — is reduced, coerced against a custom scalar, and built into a schema.
///
/// The schema half is parsed and the literal half is hand-assembled, in two documents through one
/// [`SchemaBuilder`], because no SDL this harness can parse reaches the ceiling and hand-writing a
/// query root and a directive definition would put four hundred lines of constructor between the
/// reader and the property.
#[test]
fn a_literal_at_the_ceiling_is_a_price_and_one_past_it_is_a_refusal() {
  const DECLARATIONS: &str = "type Query { ok: Int }
     scalar Custom
     directive @x(a: Custom) on SCALAR";

  /// `scalar Foo @x(a: [[…[]…]])`, with exactly `containers` nested list literals.
  fn document(containers: usize) -> TypeSystemDocument<&'static str> {
    let span = SimpleSpan::const_new(0, 0);
    let mut value = ConstInputValue::List(ConstList::new(span, Nested::empty()));
    for _ in 1..containers {
      value = ConstInputValue::List(ConstList::new(span, Nested::new(vec![value])));
    }
    let directive = ConstDirective::new(
      span,
      Name::new(span, "x"),
      Some(ConstArguments::new(
        span,
        vec![ConstArgument::new(span, Name::new(span, "a"), value)],
      )),
    );
    TypeSystemDocument::new(
      span,
      vec![TypeSystemDefinitionOrExtension::Definition(Described::new(
        span,
        None,
        TypeSystemDefinition::Type(TypeDefinition::Scalar(ScalarTypeDefinition::new(
          span,
          Name::new(span, "Foo"),
          Some(ConstDirectives::new(span, vec![directive])),
        ))),
      ))],
    )
  }

  fn build(containers: usize) -> Result<Schema, SchemaErrors> {
    let declarations = parse(DECLARATIONS);
    let literal = document(containers);
    let mut builder = SchemaBuilder::new();
    builder.document(&declarations);
    builder.document(&literal);
    builder.finish()
  }

  // A custom scalar accepts every literal, so nothing but the depth is in question here.
  build(MAX_CONST_VALUE_DEPTH).unwrap_or_else(|errors| {
    panic!("a literal of exactly {MAX_CONST_VALUE_DEPTH} containers was refused:\n{errors}")
  });

  let refused = build(MAX_CONST_VALUE_DEPTH + 1).expect_err("one container past the ceiling");
  assert_eq!(
    refused.kinds(),
    std::vec![SchemaErrorKind::ConstantValueTooDeep],
    "one container past the ceiling produced {:?}, and the refusal is supposed to be the only \
     thing wrong with this document",
    refused.kinds()
  );
  assert_eq!(
    refused.iter().next().expect("one error").subject(),
    "a",
    "the refusal names the argument the literal was written for"
  );
}

/// The one kind no parsed document can reach, reached the only other way it can be.
///
/// The AST types are public and `const`-constructible, so a caller *can* hand the builder a name
/// the grammar would never produce. The arena's ASCII invariant is what makes `Schema::name`
/// infallible, so the builder checks rather than assumes.
#[test]
fn invalid_name_from_a_hand_built_ast() {
  let span = SimpleSpan::const_new(0, 9);
  let scalar = ScalarTypeDefinition::new(span, Name::new(span, "not a name"), None);
  let definition = TypeSystemDefinition::Type(TypeDefinition::Scalar(scalar));
  let document = TypeSystemDocument::<&str>::new(
    span,
    vec![TypeSystemDefinitionOrExtension::Definition(Described::new(
      span, None, definition,
    ))],
  );

  let errors = Schema::build(&document).expect_err("a name outside the grammar is not a schema");
  assert!(
    errors.contains_kind(SchemaErrorKind::InvalidName),
    "{errors}"
  );
  let error = errors
    .errors()
    .iter()
    .find(|error| error.kind() == SchemaErrorKind::InvalidName)
    .expect("the kind is present");
  assert_eq!(error.subject(), "not a name");
}

/// The node a [`Described`] holds.
///
/// `graphql::ast` spells an input value definition only with its description around it, so this
/// is how a test names the bare carrier it builds one from.
trait Wraps {
  type Node;
}

impl<T, S> Wraps for Described<T, S> {
  type Node = T;
}

/// The bare input value definition, over bytes.
type InputValueNode = <InputValueDefinition<&'static [u8]> as Wraps>::Node;

/// [`SchemaErrorKind::UnreadableText`], reached the one way a document still can.
///
/// No document carries a string that is not UTF-8, but `Name::new` is public, so an assembled AST
/// can carry a default naming an object field with bytes that are not UTF-8. Reducing the default
/// refuses that name as [`SchemaErrorKind::InvalidName`], and printing the default's canonical
/// text — the text a schema keeps for it — trips `UnreadableText` beside it, for the input field
/// the default belongs to. Both point at the name.
///
/// The query root and the scalar are parsed, and only the input object is assembled, through one
/// [`SchemaBuilder`]: the default is the one thing here no parse can produce.
#[test]
fn unreadable_text_from_a_hand_built_ast() {
  const DECLARATIONS: &str = "type Query { ok: Int }\nscalar Custom\n";

  // `input In { f: Custom = { <FF>: [] } }`, with the name at the byte it would sit at there.
  let span = SimpleSpan::const_new(0, 0);
  let at = SimpleSpan::const_new(25, 26);
  let literal = ConstInputValue::Object(ConstObject::new(
    span,
    Nested::new(vec![ConstObjectField::new(
      span,
      Name::new(at, b"\xff".as_slice()),
      ConstInputValue::List(ConstList::new(span, Nested::empty())),
    )]),
  ));
  let field: InputValueDefinition<&'static [u8]> = Described::new(
    span,
    None,
    InputValueNode::new(
      span,
      Name::new(span, b"f".as_slice()),
      Type::Name(NamedType::new(
        span,
        Name::new(span, b"Custom".as_slice()),
        false,
      )),
      Some(DefaultInputValue::new(span, literal)),
      None,
    ),
  );
  let assembled = TypeSystemDocument::<&'static [u8]>::new(
    span,
    vec![TypeSystemDefinitionOrExtension::Definition(Described::new(
      span,
      None,
      TypeSystemDefinition::Type(TypeDefinition::InputObject(InputObjectTypeDefinition::new(
        span,
        Name::new(span, b"In".as_slice()),
        None,
        Some(InputFieldsDefinition::new(span, vec![field])),
      ))),
    ))],
  );

  let declarations = parse(DECLARATIONS);
  let mut builder = SchemaBuilder::new();
  builder.document(&declarations);
  builder.document(&assembled);
  let errors = builder
    .finish()
    .expect_err("a default that cannot be printed is not a schema");

  // A custom scalar accepts every literal, so nothing but the name is in question here.
  assert_eq!(
    errors.kinds(),
    std::vec![
      SchemaErrorKind::InvalidName,
      SchemaErrorKind::UnreadableText
    ],
    "{errors}"
  );
  let reported = |kind: SchemaErrorKind| {
    let error = errors
      .errors()
      .iter()
      .find(|error| error.kind() == kind)
      .expect("the kind is present");
    (error.subject(), error.span())
  };
  // The name itself, rendered lossily, because it is not text.
  assert_eq!(reported(SchemaErrorKind::InvalidName), ("\u{fffd}", at));
  // The input field whose default could not be kept.
  assert_eq!(reported(SchemaErrorKind::UnreadableText), ("f", at));
}

/// A document parsed from bytes, for SDL that parses: it panics on a refusal.
///
/// The byte lexer refuses a string literal that is not UTF-8 (smear#246), so a document this
/// returns holds text in every string, as one parsed from a `&str` does.
fn parse_bytes(sdl: &[u8]) -> TypeSystemDocument<&[u8]> {
  Parser::with_parser::<
    GraphqlLexer<'_, [u8]>,
    TypeSystemDocument<&[u8]>,
    GraphqlErrors<&[u8]>,
    _,
    GraphQL,
  >(type_system_document)
  .parse(sdl)
  .unwrap_or_else(|errors| panic!("the byte SDL parses: {errors:?}"))
}

/// One string that is not UTF-8 at each kind of string a schema keeps: a type's description, an
/// argument's default, a field's deprecation reason and a scalar's `specifiedByURL`. The byte
/// lexer refuses all four, so this does not parse.
const UNREADABLE: &[u8] =
  b"\"\xff\" type Query { ok(a: String = \"\xfe\"): Int @deprecated(reason: \"\xfd\") }\n\
  scalar Url @specifiedBy(url: \"\xfc\")\n";

/// [`UNREADABLE`] with readable text in the same four places.
const READABLE: &[u8] =
  "\"Root.\" type Query { ok(a: String = \"\u{e9}\"): Int @deprecated(reason: \"gone\") }\n\
  scalar Url @specifiedBy(url: \"https://example.com\")\n"
    .as_bytes();

/// Text that is not UTF-8 never reaches the builder from a parse.
///
/// The byte lexer refuses a string literal that is not well-formed UTF-8 (smear#246), so
/// [`UNREADABLE`] fails to parse, and the refusals are the lexer's, at the string literals. This is
/// the door that used to reach [`SchemaErrorKind::UnreadableText`]: the byte lexer passed a string's
/// content through unread, and the builder refused it once per string it would have kept.
#[test]
fn ill_formed_text_is_refused_by_the_byte_lexer() {
  let Err(errors) = Parser::with_parser::<
    GraphqlLexer<'_, [u8]>,
    TypeSystemDocument<&[u8]>,
    GraphqlErrors<&[u8]>,
    _,
    GraphQL,
  >(type_system_document)
  .parse(UNREADABLE) else {
    panic!("a string literal that is not UTF-8 does not parse");
  };

  // The parser keeps a lexer error's span and its kind.
  let literals = [
    SimpleSpan::new(0, 3),
    SimpleSpan::new(32, 35),
    SimpleSpan::new(62, 65),
    SimpleSpan::new(98, 101),
  ];
  for literal in &literals {
    assert_eq!(UNREADABLE[literal.start()], b'"');
    assert_eq!(UNREADABLE[literal.end() - 1], b'"');
  }
  let lexer_errors: Vec<SimpleSpan> = errors
    .iter()
    .filter(|error| {
      matches!(
        error.data(),
        ErrorData::Lexical(SyntaxErrorKind::InvalidUtf8)
      )
    })
    .map(|error| error.span())
    .collect();
  assert!(!lexer_errors.is_empty(), "{errors:?}");
  assert!(
    lexer_errors.iter().all(|span| literals.contains(span)),
    "every lexer refusal is at one of the four literals: {errors:?}"
  );
}

/// The valid twin: the same four places holding text, kept.
#[test]
fn readable_text_from_a_byte_source_is_kept() {
  let schema = Schema::build(&parse_bytes(READABLE)).expect("readable text builds");
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  assert_eq!(
    schema.text(schema.type_meta(query).description()),
    Some("Root.")
  );

  let (ok, meta) = member(&schema, "Query", "ok");
  assert_eq!(deprecated(&schema, meta.deprecation()), Some(Some("gone")));
  let (_, a) = argument(&schema, ok.args(), "a");
  assert_eq!(schema.text(a.default_value()), Some("\"\u{e9}\""));

  let (url, _) = schema.type_by_name(b"Url").expect("Url");
  assert_eq!(schema.specified_by_url(url), Some("https://example.com"));
}

// ---------------------------------------------------------------------------------------------
// acceptance
// ---------------------------------------------------------------------------------------------

/// The realistic schema the rest of the acceptance tests read.
const STAR_WARS: &str = r#"
schema { query: Query mutation: Mutation }

directive @auth(role: Role! = USER) repeatable on FIELD_DEFINITION | OBJECT

interface Node { id: ID! }
interface Character implements Node { id: ID! name: String! friends: [Character] }

type Human implements Character & Node {
  id: ID!
  name: String!
  friends: [Character]
  homePlanet: String
}

type Droid implements Character & Node {
  id: ID!
  name: String!
  friends: [Character]
  primaryFunction: String
}

type Starship implements Node { id: ID! length(unit: LengthUnit = METER): Float }

union SearchResult = Human | Droid | Starship

enum LengthUnit { METER FOOT }
enum Role { USER ADMIN }

input ReviewInput { stars: Int! commentary: String tags: [String!] = [] }
input Filter @oneOf { byName: String byId: ID }

type Query {
  hero(episode: Int): Character
  search(text: String!, filter: Filter): [SearchResult]
  node(id: ID!): Node
}

type Mutation { createReview(review: ReviewInput!): Int }
"#;

#[test]
fn a_realistic_schema_builds() {
  let schema = built(STAR_WARS);

  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  assert_eq!(schema.root(RootOperation::Query), Some(query));
  let (mutation, _) = schema.type_by_name(b"Mutation").expect("Mutation");
  assert_eq!(schema.root(RootOperation::Mutation), Some(mutation));
  assert_eq!(schema.root(RootOperation::Subscription), None);

  // Field lookup goes through the interned symbol and a binary search over the sorted group.
  let hero = schema
    .sym(b"hero")
    .and_then(|sym| schema.field(query, sym))
    .expect("Query.hero");
  assert_eq!(schema.name(hero.ty().base()), "Character");
  assert!(!hero.ty().is_non_null());

  // Every composite carries `__typename`, unions included.
  for name in [
    &b"Query"[..],
    b"Character",
    b"Human",
    b"SearchResult",
    b"Node",
  ] {
    let (id, _) = schema.type_by_name(name).expect("the type exists");
    let typename = schema.sym(builtin::TYPENAME_FIELD.as_bytes()).expect("sym");
    let field = schema.field(id, typename).expect("__typename");
    assert_eq!(schema.name(field.ty().base()), "String");
    assert!(field.ty().is_non_null());
  }

  // A union's field group is exactly `__typename` — which is the whole of draft 5.3.1's
  // union clause, with no rule of its own.
  let (search_result, _) = schema.type_by_name(b"SearchResult").expect("SearchResult");
  assert_eq!(schema.fields_of(search_result).len(), 1);

  // The query root, and only the query root, carries `__schema` and `__type`.
  let schema_field = schema.sym(b"__schema").expect("sym");
  assert!(schema.field(query, schema_field).is_some());
  assert!(schema.field(mutation, schema_field).is_none());
  let type_field = schema.sym(b"__type").expect("sym");
  let type_field = schema.field(query, type_field).expect("Query.__type");
  let args = schema.inputs(type_field.args());
  assert_eq!(args.len(), 1);
  assert_eq!(schema.name(args[0].name()), "name");
  assert!(args[0].is_required());

  // Enum values are a sorted symbol group.
  let (unit, _) = schema.type_by_name(b"LengthUnit").expect("LengthUnit");
  assert_eq!(schema.enum_values_of(unit).len(), 2);
  assert!(schema.has_enum_value(unit, schema.sym(b"METER").expect("sym")));
  assert!(!schema.has_enum_value(unit, schema.sym(b"Human").expect("sym")));

  // `@oneOf` is a flag, read off the applied directive at build.
  let (filter, filter_def) = schema.type_by_name(b"Filter").expect("Filter");
  assert!(filter_def.is_one_of());
  assert_eq!(schema.input_fields_of(filter).len(), 2);

  // Directive locations are a mask, so 5.7.2 is one `AND`.
  let auth = schema.directive_by_name(b"auth").expect("@auth");
  assert!(auth.is_repeatable());
  assert!(
    auth
      .locations()
      .contains(smear::validator::schema::DirectiveLocation::FieldDefinition)
  );
  assert!(
    !auth
      .locations()
      .contains(smear::validator::schema::DirectiveLocation::Query)
  );
  assert!(!auth.is_built_in());

  // The five specified directives are injected.
  for name in builtin::BUILT_IN_DIRECTIVES {
    let directive = schema
      .directive_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("@{name} is injected"));
    assert!(directive.is_built_in(), "@{name} should be marked built-in");
  }
  // As are the five built-in scalars and the eight introspection types.
  for name in builtin::BUILT_IN_SCALARS {
    let (_, def) = schema
      .type_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("{name} is injected"));
    assert_eq!(def.kind(), TypeKind::Scalar);
    assert!(def.is_built_in());
  }
  for name in builtin::INTROSPECTION_TYPES {
    let (_, def) = schema
      .type_by_name(name.as_bytes())
      .unwrap_or_else(|| panic!("{name} is injected"));
    assert!(def.is_built_in());
  }
}

/// The meta-schema is not behind the `introspection` feature, and never was going to be: an
/// introspection *query* is an ordinary executable document that draft 5.3.1 checks field by
/// field, so it needs `__Schema` and friends present in every schema.
#[test]
fn introspection_is_part_of_every_schema() {
  let schema = built(ROOT);
  let (type_id, def) = schema.type_by_name(b"__Type").expect("__Type");
  assert_eq!(def.kind(), TypeKind::Object);

  let fields = schema
    .sym(b"fields")
    .and_then(|sym| schema.field(type_id, sym));
  let fields = fields.expect("__Type.fields");
  let args = schema.inputs(fields.args());
  assert_eq!(args.len(), 1);
  assert_eq!(schema.name(args[0].name()), "includeDeprecated");
  assert_eq!(args[0].default_kind(), DefaultKind::NonNull);

  let (kind_enum, def) = schema.type_by_name(b"__TypeKind").expect("__TypeKind");
  assert_eq!(def.kind(), TypeKind::Enum);
  assert_eq!(schema.enum_values_of(kind_enum).len(), 8);
}

#[test]
fn builtin_sdl_parses() {
  // The builder `expect()`s these; this is the standing guard on that expectation.
  for sdl in [
    builtin::BUILT_IN_SCALARS_SDL,
    builtin::BUILT_IN_DIRECTIVES_SDL,
    builtin::INTROSPECTION_SDL,
  ] {
    let document = parse(sdl);
    assert!(!document.definitions().is_empty());
  }
}

/// A document that spells out a built-in scalar or a specified directive replaces it rather than
/// colliding with it — which is what makes a printed schema re-readable.
#[test]
fn built_in_scalars_and_directives_are_replaceable() {
  let schema = built(
    "type Query { ok: Int }
     scalar String
     directive @skip(if: Boolean!) on FIELD",
  );
  let (_, string) = schema.type_by_name(b"String").expect("String");
  assert!(
    !string.is_built_in(),
    "the document's own String should win over the injected one"
  );
  let skip = schema.directive_by_name(b"skip").expect("@skip");
  assert!(!skip.is_built_in());
  assert!(
    skip
      .locations()
      .contains(smear::validator::schema::DirectiveLocation::Field)
  );
  assert!(
    !skip
      .locations()
      .contains(smear::validator::schema::DirectiveLocation::FragmentSpread),
    "the document's own @skip should replace the injected one, not merge with it"
  );
}

/// Extensions are applied once every document has been read, so order does not matter — within a
/// document or across them.
#[test]
fn extensions_apply_regardless_of_order() {
  let schema = built("extend type Query { extra: Int } type Query { ok: Int }");
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  // `ok`, `extra`, and the three meta-fields the query root carries.
  assert_eq!(schema.fields_of(query).len(), 5);

  let base = parse("type Query { ok: Int }");
  let extension =
    parse("extend type Query { extra: Int } extend schema { mutation: M } type M { go: Int }");
  let mut builder = SchemaBuilder::new();
  builder.document(&base).document(&extension);
  let schema = builder.finish().expect("two documents make one schema");
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  assert!(
    schema
      .sym(b"extra")
      .and_then(|s| schema.field(query, s))
      .is_some()
  );
  assert!(schema.root(RootOperation::Mutation).is_some());
}

/// A definition the specification provided is still an extension target.
///
/// `extend scalar Int @tag(...)` names something the document never wrote, and refusing it as an
/// undefined target would be a false positive — the worse defect for a validator to have.
#[test]
fn built_ins_can_be_extended() {
  let schema = built(
    "type Query { ok: Int }
     directive @tag(name: String!) on SCALAR | OBJECT
     extend scalar Int @tag(name: \"money\")",
  );
  let (int, def) = schema.type_by_name(b"Int").expect("Int");
  assert!(def.is_built_in());
  assert_eq!(def.kind(), TypeKind::Scalar);
  let _ = int;
}

/// Draft §3.13.4: `@specifiedBy` "must not appear on built-in scalar types". "Built-in scalar types
/// must not provide a scalar specification URL as they are specified by this document" (§3.5).
#[test]
fn specified_by_on_a_built_in_scalar_is_refused() {
  // On a definition that spells the built-in out, for the four other than `String`, the type of
  // `@specifiedBy`'s own `url` argument.
  for scalar in ["Int", "Float", "Boolean", "ID"] {
    let sdl = format!(
      "type Query {{ ok: Int }} scalar {scalar} @specifiedBy(url: \"https://example.invalid\")"
    );
    let errors = refused(&sdl);
    let rendered: Vec<String> = errors.errors().iter().map(ToString::to_string).collect();
    assert_eq!(
      rendered,
      [format!(
        "@specifiedBy is applied to a built-in scalar: `{scalar}.specifiedBy`"
      )],
      "---\n{sdl}"
    );
    let span = errors.errors()[0].span();
    assert_eq!(&sdl[span.start()..span.end()], "specifiedBy", "---\n{sdl}");
  }

  // On an extension, of the injected definition and of one the document spelled out.
  for sdl in [
    "type Query { ok: Int } extend scalar Float @specifiedBy(url: \"https://example.invalid\")",
    "type Query { ok: Int } scalar Float extend scalar Float @specifiedBy(url: \"https://example.invalid\")",
  ] {
    let errors = refused(sdl);
    let rendered: Vec<String> = errors.errors().iter().map(ToString::to_string).collect();
    assert_eq!(
      rendered,
      ["@specifiedBy is applied to a built-in scalar: `Float.specifiedBy`"],
      "---\n{sdl}"
    );
    let span = errors.errors()[0].span();
    assert_eq!(span.start(), sdl.rfind("specifiedBy").expect("the usage"));
  }

  // On `String`, whose directive also refers to itself through its own `url: String!`.
  assert_eq!(
    refused(
      "type Query { ok: String } scalar String @specifiedBy(url: \"https://example.invalid\")"
    )
    .kinds(),
    vec![
      SchemaErrorKind::SelfReferentialDirective,
      SchemaErrorKind::SpecifiedByOnBuiltInScalar,
    ]
  );
}

/// What stays legal beside that refusal: `@specifiedBy` on a custom scalar, from its definition or
/// from an extension, and a built-in scalar spelled out or extended without it.
#[test]
fn specified_by_on_a_custom_scalar_is_accepted() {
  let schema = built(
    "type Query { ok: Int }
     directive @tag on SCALAR
     scalar UUID @specifiedBy(url: \"https://example.invalid/uuid\")
     scalar Url
     extend scalar Url @specifiedBy(url: \"https://example.invalid/url\")
     scalar Int
     extend scalar Float @tag",
  );
  assert_eq!(
    schema.specified_by_url(ty(&schema, "UUID")),
    Some("https://example.invalid/uuid")
  );
  assert_eq!(
    schema.specified_by_url(ty(&schema, "Url")),
    Some("https://example.invalid/url")
  );
  assert_eq!(schema.specified_by_url(ty(&schema, "Int")), None);
  assert_eq!(schema.specified_by_url(ty(&schema, "Float")), None);
}

/// One builder, two documents whose source slice types differ — the property that makes `Schema`
/// non-generic worth having.
#[test]
fn one_builder_accepts_documents_of_different_source_types() {
  let text = parse("type Query { ok: Int }");
  let bytes_sdl: &[u8] = b"type Extra { more: Int }";
  let bytes = Parser::with_parser::<
    GraphqlLexer<'_, [u8]>,
    TypeSystemDocument<&[u8]>,
    GraphqlErrors<&[u8]>,
    _,
    GraphQL,
  >(type_system_document)
  .parse(bytes_sdl)
  .expect("the byte SDL parses");

  let mut builder = SchemaBuilder::new();
  builder.document(&text);
  builder.document(&bytes);
  let schema = builder.finish().expect("both documents build one schema");
  assert!(schema.type_by_name(b"Extra").is_some());
}

// ---------------------------------------------------------------------------------------------
// several documents: every position in the document it was read from
// ---------------------------------------------------------------------------------------------

/// Builds `documents` with one builder, in order, and returns what it refused them with.
fn refused_documents(documents: &[&str]) -> SchemaErrors {
  let parsed: Vec<TypeSystemDocument<&str>> = documents.iter().map(|&sdl| parse(sdl)).collect();
  let mut builder = SchemaBuilder::new();
  for document in &parsed {
    builder.document(document);
  }
  match builder.finish() {
    Err(errors) => errors,
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{documents:#?}"),
  }
}

/// The `nth` occurrence of `text` in `documents[document]`, as a diagnostic location.
fn occurrence(documents: &[&str], document: u32, text: &str, nth: usize) -> Location {
  let (start, _) = documents[document as usize]
    .match_indices(text)
    .nth(nth)
    .unwrap_or_else(|| {
      panic!(
        "{text:?} occurs fewer than {} times in document {document}",
        nth + 1
      )
    });
  Location::new(document, SimpleSpan::new(start, start + text.len()))
}

/// Builds `documents` together and requires one refusal, of `kind`: its primary position is the
/// occurrence `at` names, and its one label says `phrase` at the occurrence `related` names, each
/// as `(document, text, nth)`.
fn assert_points_across(
  documents: &[&str],
  kind: SchemaErrorKind,
  at: (u32, &str, usize),
  related: (u32, &str, usize),
  phrase: &'static str,
) {
  let errors = refused_documents(documents);
  assert_eq!(errors.kinds(), vec![kind], "{documents:#?}");
  assert_eq!(errors.len(), 1, "{errors}\n{documents:#?}");
  let error = &errors.errors()[0];
  assert_eq!(
    error.primary(),
    occurrence(documents, at.0, at.1, at.2),
    "{documents:#?}"
  );
  assert_eq!(error.document(), at.0, "{documents:#?}");
  let label = occurrence(documents, related.0, related.1, related.2);
  assert_eq!(error.labels(), 1, "{documents:#?}");
  assert_eq!(
    error.label(0),
    Some(Label::new(label, phrase)),
    "{documents:#?}"
  );
  assert_eq!(error.related(), label.span(), "{documents:#?}");
  assert_eq!(error.related_document(), Some(related.0), "{documents:#?}");
}

/// A missing query root is reported at the `schema` definition, in the document that definition
/// was read from: here the second, while a `Query` type sits in the first, which a `schema`
/// definition keeps from being presumed the root.
#[test]
fn a_missing_query_root_is_reported_in_the_schema_definitions_document() {
  let documents = [
    "type Query { ok: Int }",
    "schema { mutation: Mutation } type Mutation { go: Int }",
  ];
  let errors = refused_documents(&documents);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingQueryRootOperationType]
  );
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(
    error.primary(),
    occurrence(&documents, 1, "schema { mutation: Mutation }", 0)
  );
  assert_eq!(error.document(), 1);
  assert_eq!(error.labels(), 0);
  assert_eq!(error.related_document(), None);
}

/// A type defined in the first document and again in the second is refused in the second, and
/// its label points back into the first, at the definition it repeats.
#[test]
fn a_duplicate_type_across_documents_points_back_into_the_first() {
  assert_points_across(
    &[
      "type Query { ok: Int } type Thing { a: Int }",
      "type Thing { b: Int }",
    ],
    SchemaErrorKind::DuplicateTypeName,
    (1, "Thing", 0),
    (0, "Thing", 0),
    "first defined here",
  );
}

/// A `schema` definition in each of two documents is refused in the second, and its label points
/// back into the first.
#[test]
fn a_duplicate_schema_definition_across_documents_points_back_into_the_first() {
  assert_points_across(
    &[
      "type Query { ok: Int } schema { query: Query }",
      "schema { query: Query }",
    ],
    SchemaErrorKind::DuplicateSchemaDefinition,
    (1, "schema { query: Query }", 0),
    (0, "schema { query: Query }", 0),
    "first defined here",
  );
}

/// A duplicate whose two halves are in one document keeps both there: the related item's own
/// document is the primary's when it really is in it. Written in the second document, so a
/// related position pinned to the first would show.
#[test]
fn a_duplicate_within_one_document_points_within_it() {
  assert_points_across(
    &[
      "type Query { ok: Int }",
      "type Thing { a: Int } type Thing { b: Int }",
    ],
    SchemaErrorKind::DuplicateTypeName,
    (1, "Thing", 1),
    (1, "Thing", 0),
    "first defined here",
  );
}

/// The same across the related rules an extension in another document reaches: a field added
/// again, a directive applied again, to a type or to a directive definition, and a root operation
/// provided again.
#[test]
fn an_extension_in_another_document_points_back_at_what_it_repeats() {
  assert_points_across(
    &["type Query { ok: Int }", "extend type Query { ok: Int }"],
    SchemaErrorKind::DuplicateFieldName,
    (1, "ok", 0),
    (0, "ok", 0),
    "first defined here",
  );
  assert_points_across(
    &[
      "directive @tag on OBJECT type Query @tag { ok: Int }",
      "extend type Query @tag",
    ],
    SchemaErrorKind::DuplicateDirectiveUse,
    (1, "tag", 0),
    (0, "tag", 1),
    "first applied here",
  );
  assert_points_across(
    &[
      "type Query { ok: Int } directive @tag on DIRECTIVE_DEFINITION directive @d @tag on FIELD",
      "extend directive @d @tag",
    ],
    SchemaErrorKind::DuplicateDirectiveUse,
    (1, "tag", 0),
    (0, "tag", 1),
    "first applied here",
  );
  assert_points_across(
    &[
      "type Query { ok: Int } schema { query: Query }",
      "type Other { ok: Int } extend schema { query: Other }",
    ],
    SchemaErrorKind::DuplicateRootOperationType,
    (1, "Other", 1),
    (0, "Query", 1),
    "first named here",
  );
}

/// A related position inside an injected built-in definition is not reported at all: no source
/// the caller supplied holds it, so a label there would point past the end of the caller's
/// documents. The refusal keeps its primary position, in the caller's document, and has no label.
#[test]
fn no_related_label_points_into_a_built_in() {
  // An object extension of the injected `Int` scalar.
  let sdl = "type Query { ok: Int } extend type Int { f: Int }";
  let errors = refused(sdl);
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::ExtensionKindMismatch]);
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.subject(), "Int");
  assert_eq!(error.primary(), occurrence(&[sdl], 0, "Int", 1));
  assert_eq!(error.labels(), 0);
  assert_eq!(error.label(0), None);
  assert_eq!(error.related(), None);
  assert_eq!(error.related_document(), None);

  // A field that an extension of the injected `__Type` repeats.
  let sdl = "type Query { ok: Int } extend type __Type { name: String }";
  let errors = refused(sdl);
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::DuplicateFieldName]);
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.to_string(), "duplicate field: `__Type.name`");
  assert_eq!(error.primary(), occurrence(&[sdl], 0, "name", 0));
  assert_eq!(error.labels(), 0);
  assert_eq!(error.label(0), None);
  assert_eq!(error.related(), None);
  assert_eq!(error.related_document(), None);
}

/// The whole point of the arena: the schema outlives every byte it was built from.
#[test]
fn a_schema_outlives_its_document() {
  let schema = {
    let sdl = String::from("type Query { ok: Int }");
    let document = parse(&sdl);
    Schema::build(&document).expect("builds")
  };
  assert_eq!(schema.name(schema.sym(b"Query").expect("sym")), "Query");

  fn assert_send_sync<T: Send + Sync + 'static>(_: &T) {}
  assert_send_sync(&schema);
}

// ---------------------------------------------------------------------------------------------
// the two pins
// ---------------------------------------------------------------------------------------------

/// The possible-object bitsets, against a hand-computed expectation.
///
/// Draft 5.5.2.3 — all four subsections — is `possible(scope) ∩ possible(condition) ≠ ∅`, so if
/// these words are wrong every fragment-spread verdict is wrong and nothing else would say so.
/// The hierarchy below is small enough to work out by hand and covers all four shapes: an object,
/// an interface with implementors, an interface with none, and a union.
#[test]
fn possible_object_bitsets_match_a_hand_computation() {
  let schema = built(
    "type Query { ok: Int }
     interface Node { id: ID! }
     interface Named { name: String! }
     interface Orphan { nothing: Int }
     type Human implements Node & Named { id: ID! name: String! }
     type Droid implements Node & Named { id: ID! name: String! }
     type Ship implements Node { id: ID! }
     union Living = Human | Droid",
  );

  // Object ordinals are assigned in type-id order, which is document order for user types, so
  // the four objects are Query=0, Human=1, Droid=2, Ship=3. Everything below is derived from
  // that by hand.
  let names = |ids: Vec<smear::validator::schema::TypeId>| -> Vec<&str> {
    ids
      .into_iter()
      .map(|id| schema.name(schema.type_def(id).name()))
      .collect()
  };
  let ordinal = |name: &[u8]| {
    let (id, _) = schema.type_by_name(name).expect("the type exists");
    schema.type_def(id).object_ordinal()
  };
  assert_eq!(ordinal(b"Query"), 0);
  assert_eq!(ordinal(b"Human"), 1);
  assert_eq!(ordinal(b"Droid"), 2);
  assert_eq!(ordinal(b"Ship"), 3);

  // Four user objects plus the four introspection objects (__Schema, __Type, __Field,
  // __InputValue, __EnumValue, __Directive) still fit one 64-bit word.
  assert_eq!(schema.possible_words(), 1);

  let bits = |name: &[u8]| -> u64 {
    let (id, _) = schema.type_by_name(name).expect("the type exists");
    schema.possible_objects(id).expect("a composite")[0]
  };

  // An object's set is a singleton.
  assert_eq!(bits(b"Human"), 1 << 1);
  assert_eq!(bits(b"Ship"), 1 << 3);
  // An interface's set is its implementors.
  assert_eq!(bits(b"Node"), (1 << 1) | (1 << 2) | (1 << 3));
  assert_eq!(bits(b"Named"), (1 << 1) | (1 << 2));
  // An interface nobody implements has an empty set — which is exactly why draft 5.5.2.3 has an
  // ecosystem-wide `condition == scope` exception, applied by the rule and not by the schema.
  assert_eq!(bits(b"Orphan"), 0);
  // A union's set is its members.
  assert_eq!(bits(b"Living"), (1 << 1) | (1 << 2));

  // A scalar, an enum and an input object have no set at all.
  for name in [&b"Int"[..], b"ID", b"String"] {
    let (id, _) = schema.type_by_name(name).expect("the type exists");
    assert!(schema.possible_objects(id).is_none(), "{name:?}");
  }

  // The word-AND, which is what a rule actually calls.
  let id = |name: &[u8]| schema.type_by_name(name).expect("the type exists").0;
  assert!(schema.possible_objects_intersect(id(b"Node"), id(b"Living")));
  assert!(schema.possible_objects_intersect(id(b"Named"), id(b"Human")));
  assert!(!schema.possible_objects_intersect(id(b"Living"), id(b"Ship")));
  assert!(!schema.possible_objects_intersect(id(b"Orphan"), id(b"Orphan")));
  assert!(!schema.possible_objects_intersect(id(b"Node"), id(b"Query")));

  // And the enumeration the bitset stands for.
  let mut living = names(schema.possible_object_ids(id(b"Living")).collect());
  living.sort_unstable();
  assert_eq!(living, ["Droid", "Human"]);
  let mut node = names(schema.possible_object_ids(id(b"Node")).collect());
  node.sort_unstable();
  assert_eq!(node, ["Droid", "Human", "Ship"]);
  assert_eq!(
    names(schema.possible_object_ids(id(b"Orphan")).collect()),
    Vec::<&str>::new()
  );

  assert!(schema.is_possible_object(id(b"Node"), id(b"Ship")));
  assert!(!schema.is_possible_object(id(b"Living"), id(b"Ship")));
  // An abstract type is never a possible *object* of anything, including itself.
  assert!(!schema.is_possible_object(id(b"Node"), id(b"Named")));
}

/// The `DefaultKind` reduction, against a schema carrying all of its shapes.
///
/// Executable validation needs only whether a default exists and whether it is `null` — 5.4.3,
/// 5.6.4 and 5.8.5 never read the value — and dropping the content is what lets the schema stop
/// borrowing the SDL. If the reduction slipped, "required" would be wrong everywhere.
#[test]
fn default_kind_reduces_every_shape() {
  let schema = built(
    "type Query {
       f(
         absent:        Int,
         nullDefault:   Int  = null,
         intDefault:    Int  = 0,
         zeroFloat:     Float = 0.0,
         falseDefault:  Boolean = false,
         emptyString:   String = \"\",
         emptyList:     [Int] = [],
         emptyObject:   Point = {},
         enumDefault:   Unit = METER,
         requiredNoDef: Int!,
         requiredWithDefault: Int! = 1
       ): Int
     }
     input Point { x: Int y: Int }
     enum Unit { METER }",
  );

  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  let field = schema
    .sym(b"f")
    .and_then(|sym| schema.field(query, sym))
    .expect("Query.f");
  let args = schema.inputs(field.args());

  let of = |name: &str| {
    let sym = schema.sym(name.as_bytes()).expect("the argument name");
    let arg = args
      .iter()
      .find(|arg| arg.name() == sym)
      .unwrap_or_else(|| panic!("argument {name}"));
    (arg.default_kind(), arg.is_required())
  };

  // Absent means absent — not "null".
  assert_eq!(of("absent"), (DefaultKind::Absent, false));
  // An explicit `null` is its own class: it is present, so a non-null argument carrying it is
  // not required, and 5.6.4 must be able to tell it from a missing default.
  assert_eq!(of("nullDefault"), (DefaultKind::Null, false));
  // Everything else collapses to `NonNull`, whatever its literal shape — including the falsy
  // ones, which is the reduction most likely to be got wrong.
  for name in [
    "intDefault",
    "zeroFloat",
    "falseDefault",
    "emptyString",
    "emptyList",
    "emptyObject",
    "enumDefault",
  ] {
    assert_eq!(of(name), (DefaultKind::NonNull, false), "{name}");
  }
  // Required is exactly "non-null type and no default".
  assert_eq!(of("requiredNoDef"), (DefaultKind::Absent, true));
  assert_eq!(of("requiredWithDefault"), (DefaultKind::NonNull, false));

  // Input fields take the same reduction, through the other table.
  let schema = built(
    "type Query { ok: Int }
     input In { a: Int b: Int = null c: Int = 3 d: Int! e: Int! = 4 }",
  );
  let (input, _) = schema.type_by_name(b"In").expect("In");
  let of = |name: &str| {
    let sym = schema.sym(name.as_bytes()).expect("the field name");
    let field = schema.input_field(input, sym).expect("the field");
    (field.default_kind(), field.is_required())
  };
  assert_eq!(of("a"), (DefaultKind::Absent, false));
  assert_eq!(of("b"), (DefaultKind::Null, false));
  assert_eq!(of("c"), (DefaultKind::NonNull, false));
  assert_eq!(of("d"), (DefaultKind::Absent, true));
  assert_eq!(of("e"), (DefaultKind::NonNull, false));
}

// ---------------------------------------------------------------------------------------------
// representation invariants
// ---------------------------------------------------------------------------------------------

/// Packed type references: the wrapper word, and the walks the §5 rules will do over it.
#[test]
fn packed_types_round_trip_their_spelling() {
  let schema = built(
    "type Query {
       plain: Int
       required: Int!
       list: [Int]
       listOfRequired: [Int!]
       requiredList: [Int]!
       nested: [[Int!]!]!
     }",
  );
  let (query, _) = schema.type_by_name(b"Query").expect("Query");
  let ty = |name: &str| {
    let sym = schema.sym(name.as_bytes()).expect("the field name");
    schema.field(query, sym).expect("the field").ty()
  };

  assert_eq!(ty("plain").depth(), 0);
  assert!(!ty("plain").is_non_null());
  assert!(ty("required").is_non_null());
  assert_eq!(ty("required").nullable(), ty("plain"));
  assert!(ty("list").is_list());
  assert_eq!(ty("list").list_item(), Some(ty("plain")));
  assert_eq!(ty("listOfRequired").list_item(), Some(ty("required")));
  assert!(
    !ty("requiredList").is_list(),
    "`[Int]!` is a non-null, not a list"
  );
  assert_eq!(ty("requiredList").nullable(), ty("list"));
  assert_eq!(ty("nested").depth(), 5);

  // The base is stored twice over — resolved id and interned name — so a diagnostic never walks
  // a table to spell a type.
  let (int, _) = schema.type_by_name(b"Int").expect("Int");
  assert_eq!(ty("nested").base_id(), int);
  assert_eq!(schema.name(ty("nested").base()), "Int");
}

/// Field and argument groups are sorted by symbol, which is what makes lookup a binary search.
#[test]
fn groups_are_sorted_for_binary_search() {
  let schema = built(STAR_WARS);
  for (index, def) in schema.types().iter().enumerate() {
    let id = smear::validator::schema::TypeId::new(index as u32);
    let fields = schema.fields_of(id);
    assert!(
      fields
        .windows(2)
        .all(|w| w[0].name().get() < w[1].name().get()),
      "field group of {} is not sorted",
      schema.name(def.name())
    );
    for field in fields {
      let args = schema.inputs(field.args());
      assert!(
        args
          .windows(2)
          .all(|w| w[0].name().get() < w[1].name().get()),
        "argument group of {}.{} is not sorted",
        schema.name(def.name()),
        schema.name(field.name())
      );
    }
    let inputs = schema.input_fields_of(id);
    assert!(
      inputs
        .windows(2)
        .all(|w| w[0].name().get() < w[1].name().get()),
      "input field group of {} is not sorted",
      schema.name(def.name())
    );
    let values = schema.enum_values_of(id);
    assert!(values.windows(2).all(|w| w[0] < w[1]));
  }
  assert!(
    schema
      .directives()
      .windows(2)
      .all(|w| w[0].name().get() < w[1].name().get()),
    "the directive table is not sorted"
  );
}

/// Interface closures are transitive, and are what the §3 covariance check reads.
#[test]
fn interface_closures_are_transitive() {
  let schema = built(
    "type Query { ok: Int }
     interface A { a: Int }
     interface B implements A { a: Int b: Int }
     type T implements A & B { a: Int b: Int }",
  );
  let id = |name: &[u8]| schema.type_by_name(name).expect("the type").0;
  let mut closure: Vec<&str> = schema
    .interfaces_of(id(b"T"))
    .iter()
    .map(|i| schema.name(schema.type_def(*i).name()))
    .collect();
  closure.sort_unstable();
  assert_eq!(closure, ["A", "B"]);
  assert_eq!(
    schema
      .interfaces_of(id(b"B"))
      .iter()
      .map(|i| schema.name(schema.type_def(*i).name()))
      .collect::<Vec<_>>(),
    ["A"]
  );
  assert!(schema.interfaces_of(id(b"A")).is_empty());
}

/// Covariance is accepted where the draft accepts it — the direction a refusal-only suite would
/// never catch.
#[test]
fn valid_interface_implementations_are_accepted() {
  built(
    "type Query { ok: Int }
     interface Node { id: ID }
     interface Named { name: String }
     type Impl implements Node & Named {
       # `T!` implements `T`.
       id: ID!
       name: String
       # A field the interfaces do not declare, so its arguments are nobody's business.
       extra(x: Int, y: Int = 1): Int
     }
     interface Container { item: Node }
     # An object is a valid implementation of an interface it implements.
     type Box implements Container { item: Impl }
     interface ListContainer { items: [Node] }
     type ListBox implements ListContainer { items: [Impl!]! }
     # `IsValidImplementation` 2.4: an argument the interface field does not declare \"must not be
     # required\", and required is non-null *with no default*. So `defaulted` is legal and
     # `optional` is legal, and only a bare `Int!` is not (which is the census row for
     # `UnexpectedRequiredArgument`). Dropping the default half of that predicate was invisible to
     # every gate until this line existed — the obvious place to put it, `Impl.extra`, proves
     # nothing, because the rule only ever looks at arguments of a field that *implements* an
     # interface field.
     interface Extras { e: Int }
     type WithExtras implements Extras { e(defaulted: Int! = 1, optional: Int): Int }",
  );

  // `IsSubType`(2): an object that *is* a member of the interface field's union. The other end of
  // the branch row in `BRANCHES`, and the reason that row cannot be satisfied by refusing the
  // whole union arm.
  built(
    "type Query { ok: Int }
     type Member { a: Int }
     type Other { a: Int }
     union U = Member | Other
     interface I { f: U }
     type T implements I { f: Member }
     # And through the wrappers, which is what makes it the *sub-type* relation and not equality.
     interface L { g: [U!] }
     type M implements L { g: [Member!]! }",
  );
}

/// The deprecation obligations, in the direction that accepts.
///
/// Three rules landed together here — `@deprecated` on a required argument, on a required input
/// field, and on a field whose interface field is not deprecated — and every one of them is a
/// refusal that a nearby legal SDL would trip if the predicate were written slightly wrong.
/// "Required" in particular is *non-null with no default*, so a defaulted non-null is legal and a
/// nullable one is legal, and a check written as `is_non_null()` alone would refuse both.
#[test]
fn legal_deprecations_are_accepted() {
  built(
    "type Query {
       # Optional two ways: nullable, and non-null with a default.
       nullable(a: Int @deprecated): Int
       defaulted(a: Int! = 1 @deprecated): Int
       # A deprecated *field* is unconditional — the obligation is about arguments and input
       # fields, and about interfaces.
       gone: Int @deprecated
     }
     input In { nullable: Int @deprecated defaulted: Int! = 1 @deprecated }
     enum E { LIVE GONE @deprecated }
     directive @d(nullable: Int @deprecated, defaulted: Int! = 1 @deprecated) on OBJECT",
  );

  // `IsValidImplementation` 2.6 runs one way. An interface may deprecate ahead of its
  // implementors; an implementor may not deprecate ahead of its interface.
  built(
    "type Query { ok: Int }
     interface I { f: Int @deprecated g: Int }
     # Both deprecated, and the interface deprecated alone: the two legal shapes.
     type Both implements I { f: Int @deprecated g: Int }
     type InterfaceOnly implements I { f: Int g: Int }",
  );
}

/// Default values that *do* coerce, at all three positions an `InputValueDefinition` appears.
///
/// The refusal is new (draft §3.6.1 2.4.5) and the direction that matters more is this one: a
/// default the specification's coercion rules accept and `Schema::build` does not is a schema
/// nothing else refuses, with no later stage to overrule it.
#[test]
fn legal_default_values_are_accepted() {
  built(
    "type Query {
       f(
         i: Int = 2147483647
         negative: Int = -2147483648
         fl: Float = 1.5
         floatFromInt: Float = 2
         s: String = \"x\"
         b: Boolean = false
         idText: ID = \"abc\"
         idNumber: ID = 4
         # The singleton-to-list coercion, at one level and at two.
         list: [Int] = 1
         nested: [[Int]] = 7
         listed: [Int] = [1, 2]
         obj: Point = { x: 1 }
         e: Unit = METER
         custom: Custom = { anything: [1, true, NOT_AN_ENUM] }
         nullable: Int = null
         emptyList: [Int] = []
         emptyObject: Point = {}
       ): Int
     }
     scalar Custom
     input Point { x: Int y: Int }
     enum Unit { METER FOOT }
     input In { a: Point = { x: 1 } b: Unit = FOOT }
     directive @d(a: Point = { y: 2 }, b: [String!] = []) on OBJECT",
  );

  // A default that reaches another input object's default is not a *cycle* — it terminates, which
  // is the whole difference §3.10.1(4) turns on.
  built(
    "type Query { ok: Int }
     input A { b: B = {} }
     input B { n: Int = 1 }
     # A self-referential *type* whose defaults bottom out: the literal names `c` explicitly, so
     # `C.c`'s own default is never consulted a second time and the walk stops on `null`.
     input C { c: C = { c: null } n: Int }
     # And a self-reference with no default at all.
     input D { d: D n: Int }",
  );

  // The near miss, kept next to it: leaving `c` out of the literal is what makes it a cycle,
  // because coercing that default has to consult `C.c`'s default again to fill `c` in.
  assert_eq!(
    refused("type Query { ok: Int } input C { c: C = { n: 1 } n: Int }").kinds(),
    vec![SchemaErrorKind::InputObjectDefaultValueCycle],
    "a default that does not name the self-referential field falls back to itself"
  );
}

/// `@oneOf` on the definition stays legal; only an extension providing it is refused.
#[test]
fn one_of_on_the_definition_is_accepted() {
  built("type Query { ok: Int } input In @oneOf { a: Int b: String }");
  // An extension of a OneOf input object is fine as long as it does not *provide* the directive,
  // and its own fields are held to the OneOf constraints (draft §3.10.3(6)).
  let schema = built(
    "type Query { ok: Int }
     input In @oneOf { a: Int }
     extend input In { b: String }",
  );
  let (input, def) = schema.type_by_name(b"In").expect("In");
  assert!(def.is_one_of());
  assert_eq!(schema.input_fields_of(input).len(), 2);

  // The extension's own fields are still checked against the constraint.
  assert_eq!(
    refused(
      "type Query { ok: Int }
       input In @oneOf { a: Int }
       extend input In { b: String! }"
    )
    .kinds(),
    vec![SchemaErrorKind::OneOfFieldNotNullable]
  );
}

/// Draft §3.10.1's OneOf input coercion, at every SDL constant position and at every depth of the
/// literal: "the value to be coerced must contain exactly one entry and that entry must not be null
/// or the null literal".
#[test]
fn a_one_of_constant_value_holds_exactly_one_entry() {
  // (document, the bytes blamed, what it renders as)
  for (sdl, blamed, rendered) in [
    (
      "type Query { ok(f: O = {}): Int } input O @oneOf { a: Int b: Int }",
      "{}",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    (
      "type Query { ok(f: O = {a: 1, b: 2}): Int } input O @oneOf { a: Int b: Int }",
      "{a: 1, b: 2}",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    (
      "type Query { ok(f: O = {a: null}): Int } input O @oneOf { a: Int b: Int }",
      "null",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    // Inside a list, after an entry that fits too, and inside an ordinary input object.
    (
      "type Query { ok(f: [O] = [{}]): Int } input O @oneOf { a: Int b: Int }",
      "{}",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    (
      "type Query { ok(f: [O] = [{a: 1}, {}]): Int } input O @oneOf { a: Int b: Int }",
      "{}",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    (
      "type Query { ok(f: Outer = {o: {}}): Int } input Outer { o: O } input O @oneOf { a: Int b: Int }",
      "{}",
      "default value does not fit its declared type: `Query.ok.f`",
    ),
    // An input field's default.
    (
      "type Query { ok: Int } input Outer { o: O = {a: 1, b: 2} } input O @oneOf { a: Int b: Int }",
      "{a: 1, b: 2}",
      "default value does not fit its declared type: `Outer.o`",
    ),
    // A directive argument, bare and inside a list.
    (
      "directive @d(f: O) on FIELD_DEFINITION type Query { ok: Int @d(f: {}) } input O @oneOf { a: Int b: Int }",
      "{}",
      "directive argument value does not fit its declared type: `Query.ok.@d.f`",
    ),
    (
      "directive @d(f: [O]) on FIELD_DEFINITION type Query { ok: Int @d(f: [{b: null}]) } input O @oneOf { a: Int b: Int }",
      "null",
      "directive argument value does not fit its declared type: `Query.ok.@d.f`",
    ),
  ] {
    let errors = refused(sdl);
    let all: Vec<String> = errors.errors().iter().map(ToString::to_string).collect();
    assert_eq!(all, [rendered], "---\n{sdl}");
    let span = errors.errors()[0].span();
    assert_eq!(&sdl[span.start()..span.end()], blamed, "---\n{sdl}");
  }

  // The fields are still coerced as any input object's are, so a field that does not fit is a
  // second diagnostic, at its own value.
  let sdl = "type Query { ok(f: O = {a: 1, b: \"x\"}): Int } input O @oneOf { a: Int b: Int }";
  let errors = refused(sdl);
  let blamed: Vec<&str> = errors
    .errors()
    .iter()
    .map(|error| &sdl[error.span().start()..error.span().end()])
    .collect();
  assert_eq!(blamed, ["{a: 1, b: \"x\"}", "\"x\""]);
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::InvalidDefaultValue]);

  // A `null` for a field the type does not declare is the undefined field, once.
  let errors = refused("type Query { ok(f: O = {z: null}): Int } input O @oneOf { a: Int b: Int }");
  let all: Vec<String> = errors.errors().iter().map(ToString::to_string).collect();
  assert_eq!(all, ["undefined input object field: `O.z`"]);
}

/// The literals the OneOf coercion accepts, at the same positions — and the ones an ordinary input
/// object accepts that a OneOf does not.
#[test]
fn a_one_of_constant_value_with_one_entry_is_accepted() {
  built(
    "type Query {
       ok(
         a: O = {a: 1}
         b: O = {b: 2}
         none: O = null
         list: [O] = [{a: 1}, {b: 2}]
         single: [O] = {a: 1}
         nested: Outer = {o: {b: 2}}
       ): Int @d(f: {a: 1})
     }
     input Outer { o: O = {a: 1} }
     input O @oneOf { a: Int b: Int }
     directive @d(f: O) on FIELD_DEFINITION",
  );
  // An ordinary input object may be given no field, or every one.
  built("type Query { ok(f: P = {}, g: P = {a: 1, b: 2}): Int } input P { a: Int b: Int }");
}

/// Three roots, three distinct types — and the defaults, which cannot collide.
#[test]
fn distinct_roots_are_accepted() {
  built("type Query { ok: Int } type Mutation { go: Int } type Subscription { s: Int }");
  built(
    "type A { ok: Int } type B { go: Int } type C { s: Int }
     schema { query: A mutation: B subscription: C }",
  );
  // A root type may still be reachable from another root; distinctness is about the three roots
  // themselves, not about the graph under them.
  built(
    "type Query { m: Mutation } type Mutation { go: Int } schema { query: Query mutation: Mutation }",
  );
}

/// The cycle rules accept the shapes that are legal, not just refuse the ones that are not.
#[test]
fn breakable_cycles_are_accepted() {
  // A nullable link breaks it.
  built("type Query { ok: Int } input A { b: B } input B { a: A! }");
  // So does a list, which may be empty.
  built("type Query { ok: Int } input A { b: [B!]! } input B { a: A! }");
  // A directive that names an input object which does not name it back is not self-referential.
  built(
    "type Query { ok: Int }
     directive @d(a: In) on FIELD
     directive @e on INPUT_FIELD_DEFINITION
     input In { x: Int @e }",
  );
  // `@specifiedBy` on a user scalar is the specification's own idiom and must stay legal: the
  // walk reaches `String`, which carries no directive, and stops.
  built("type Query { ok: Int } scalar UUID @specifiedBy(url: \"https://example.invalid\")");
  // Reporting only lands on the directive that closes the cycle, not on a bystander that merely
  // uses it — the same refinement apollo-compiler makes.
  let errors = refused(
    "type Query { ok: Int }
     directive @a(x: In) on INPUT_FIELD_DEFINITION
     directive @b(y: In) on INPUT_FIELD_DEFINITION
     input In { z: Int @a }",
  );
  assert_eq!(errors.len(), 1, "{errors}");
  assert_eq!(errors.errors()[0].subject(), "a");
}

/// Draft §3.10's Counter Example 86, verbatim, and the other cycles a `@oneOf` input object holds
/// closed.
///
/// "OneOf Input Objects require exactly one field be provided, and that value cannot be null. This
/// example is invalid because providing a value for First requires a non-null Second, and
/// constructing a Second requires a non-null First".
#[test]
fn a_one_of_input_object_can_hold_a_cycle_closed() {
  const COUNTER_EXAMPLE_86: &str = "type Query { ok: Int }
input First @oneOf {
  second: Second
}
input Second {
  first: First!
}";
  let errors = refused(COUNTER_EXAMPLE_86);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::CircularOneOfInputField]
  );
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(
    error.to_string(),
    "input object cycle through a @oneOf input object has no finite value: `Second.first`"
  );
  assert_eq!(
    error.span().start(),
    COUNTER_EXAMPLE_86
      .find("first:")
      .expect("the closing field")
  );
  assert_eq!(error.span().end() - error.span().start(), "first".len());

  for (sdl, expected) in [
    // Its one field leads straight back.
    (
      "type Query { ok: Int } input A @oneOf { a: A }",
      &["input object cycle through a @oneOf input object has no finite value: `A.a`"][..],
    ),
    // Two OneOf input objects, each leading only into the other: one cycle, one diagnostic.
    (
      "type Query { ok: Int } input A @oneOf { b: B } input B @oneOf { a: A }",
      &["input object cycle through a @oneOf input object has no finite value: `B.a`"][..],
    ),
    // Every field leads back, by two cycles, and each is reported where it closes.
    (
      "type Query { ok: Int } input A @oneOf { a: A b: B } input B { a: A! }",
      &[
        "input object cycle through a @oneOf input object has no finite value: `A.a`",
        "input object cycle through a @oneOf input object has no finite value: `B.a`",
      ][..],
    ),
  ] {
    let rendered: Vec<String> = refused(sdl)
      .errors()
      .iter()
      .map(ToString::to_string)
      .collect();
    assert_eq!(rendered, expected, "---\n{sdl}");
  }
}

/// One field that can be given a finite value breaks a OneOf out of any cycle, which is the whole
/// of the draft's OneOf arm: "If not InputFieldTypeHasUnbreakableCycle(fieldType, nextVisited):
/// Return false."
#[test]
fn a_one_of_input_object_with_a_way_out_breaks_its_cycle() {
  for sdl in [
    // The scalar alternative beside a self-reference.
    "type Query { ok: Int } input Filter @oneOf { not: Filter, eq: String }",
    // An enum alternative, and a list one, which may be empty.
    "type Query { ok: Int } input Filter @oneOf { not: Filter, by: Order } enum Order { ASC DESC }",
    "type Query { ok: Int } input Filter @oneOf { not: Filter, all: [Filter!] }",
    // Counter Example 86 with a third input object to pick, which is finite.
    "type Query { ok: Int }
     input First @oneOf { second: Second, third: Third }
     input Second { first: First! }
     input Third { x: Int }",
    // Counter Example 86 with the ordinary input object's link made nullable.
    "type Query { ok: Int } input First @oneOf { second: Second } input Second { first: First }",
  ] {
    built(sdl);
  }

  // A way out breaks a non-null cycle through the OneOf too. The non-null field is refused, as a
  // field of a OneOf; the cycle it is on is not.
  let errors =
    refused("type Query { ok: Int } input O @oneOf { a: A!, s: String } input A { o: O! }");
  assert_eq!(errors.kinds(), vec![SchemaErrorKind::OneOfFieldNotNullable]);
}

/// The ordinary arm is unchanged by the OneOf one: draft §3.10's Examples 82 and 83 are valid and
/// its Counter Examples 84 and 85 are not, each with one diagnostic at the link that closes it.
#[test]
fn the_draft_input_object_cycle_examples_hold() {
  built("type Query { ok: Int } input Example { self: Example value: String }");
  built("type Query { ok: Int } input Example { self: [Example!]! value: String }");

  for (sdl, expected) in [
    (
      "type Query { ok: Int } input Example { self: Example! value: String }",
      "input object cycle has no nullable or list link: `Example.self`",
    ),
    (
      "type Query { ok: Int }
       input First { second: Second! value: String }
       input Second { first: First! value: String }",
      "input object cycle has no nullable or list link: `Second.first`",
    ),
  ] {
    let rendered: Vec<String> = refused(sdl)
      .errors()
      .iter()
      .map(ToString::to_string)
      .collect();
    assert_eq!(rendered, [expected], "---\n{sdl}");
  }
}

/// The OneOf cycle rule is linear and iterative over a chain as long as the document makes it.
///
/// The draft states `InputObjectHasUnbreakableCycle` recursively, with the path in `visited`, and
/// twenty thousand links is a stack overflow for a literal transcription of it.
#[test]
fn a_deep_one_of_cycle_does_not_recurse() {
  const DEPTH: usize = 20_000;

  // A chain of OneOf input objects, each naming the next, that the last one's scalar field lets
  // out of: every one of them is satisfied.
  let mut sdl = String::from("type Query { ok(a: In0): Int }\n");
  for level in 0..DEPTH {
    sdl.push_str(&format!(
      "input In{level} @oneOf {{ next: In{} }}\n",
      level + 1
    ));
  }
  sdl.push_str(&format!("input In{DEPTH} @oneOf {{ end: Int }}\n"));
  built(&sdl);

  // The same chain closed into a cycle: one diagnostic, at the link that closes it.
  let mut sdl = String::from("type Query { ok(a: In0): Int }\n");
  for level in 0..DEPTH {
    sdl.push_str(&format!(
      "input In{level} @oneOf {{ next: In{} }}\n",
      level + 1
    ));
  }
  sdl.push_str(&format!("input In{DEPTH} @oneOf {{ next: In0 }}\n"));
  let errors = refused(&sdl);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::CircularOneOfInputField]
  );
  assert_eq!(
    errors.len(),
    1,
    "a cycle through twenty thousand objects is one diagnostic, not twenty thousand"
  );
  let last = format!("In{DEPTH}");
  assert_eq!(errors.errors()[0].owner(), Some(last.as_str()));
  assert_eq!(errors.errors()[0].subject(), "next");
}

/// The default roots are the conventionally named object types, when no `schema` block names any.
#[test]
fn default_root_names_are_used_without_a_schema_definition() {
  let schema =
    built("type Query { ok: Int } type Mutation { go: Int } type Subscription { s: Int }");
  for operation in RootOperation::ALL {
    let root = schema.root(operation).expect("the default root resolves");
    assert_eq!(
      schema.name(schema.type_def(root).name()),
      operation.default_type_name()
    );
  }
  // A conventionally named type that is not an object is not silently adopted.
  let errors = refused("interface Query { ok: Int }");
  assert!(errors.contains_kind(SchemaErrorKind::MissingQueryRootOperationType));
}

/// Draft §3.3.1's Example 42, verbatim: a `schema` definition names every root the schema has.
///
/// "This example describes a valid GraphQL schema without a mutation root operation type, even
/// though it contains a type named "Mutation". The schema definition must be included, otherwise
/// the "Mutation" type would be incorrectly presumed to be the mutation root operation type of the
/// schema."
#[test]
fn a_schema_definition_turns_the_default_root_names_off() {
  const EXAMPLE_42: &str = r#"
schema {
  query: Query
}

type Query {
  latestVirus: Virus
}

type Virus {
  name: String
  mutations: [Mutation]
}

type Mutation {
  name: String
}
"#;
  let schema = built(EXAMPLE_42);
  assert_eq!(
    schema.root(RootOperation::Query),
    Some(ty(&schema, "Query"))
  );
  assert_eq!(
    schema.root(RootOperation::Mutation),
    None,
    "`Mutation` was presumed to be the mutation root beside a `schema` definition"
  );
  assert_eq!(schema.root(RootOperation::Subscription), None);

  // The third default name, the same way.
  let schema = built("schema { query: Query } type Query { ok: Int } type Subscription { s: Int }");
  assert_eq!(
    schema.root(RootOperation::Query),
    Some(ty(&schema, "Query"))
  );
  assert_eq!(schema.root(RootOperation::Subscription), None);
}

/// An `extend schema` is not a `schema` definition, so the default root names still apply beside
/// one.
#[test]
fn an_extend_schema_alone_keeps_the_default_root_names() {
  let schema = built(
    "directive @d on SCHEMA
     type Query { ok: Int }
     type Mutation { go: Int }
     extend schema @d",
  );
  assert_eq!(
    schema.root(RootOperation::Query),
    Some(ty(&schema, "Query"))
  );
  assert_eq!(
    schema.root(RootOperation::Mutation),
    Some(ty(&schema, "Mutation"))
  );
  assert_eq!(schema.root(RootOperation::Subscription), None);
}

/// Draft §3.3.2: "The Schema must already be defined." With no `schema` definition and no `Query`
/// type for an omitted one to presume, an `extend schema` has nothing to extend.
#[test]
fn an_extend_schema_with_no_schema_to_extend_is_refused() {
  let sdl = "type Q { ok: Int } extend schema { query: Q }";
  let errors = refused(sdl);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::SchemaExtensionWithoutSchema]
  );
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.subject(), "schema");
  assert_eq!(error.owner(), None);
  assert_eq!(
    &sdl[error.span().start()..error.span().end()],
    "extend schema { query: Q }"
  );
  assert_eq!(
    error.to_string(),
    "schema extension has no schema to extend: `schema`"
  );

  // Each extension is refused, at its own span.
  let sdl = "directive @d on SCHEMA type Q { ok: Int } extend schema { query: Q } extend schema @d";
  let errors = refused(sdl);
  let blamed: Vec<&str> = errors
    .errors()
    .iter()
    .map(|error| &sdl[error.span().start()..error.span().end()])
    .collect();
  assert_eq!(blamed, ["extend schema { query: Q }", "extend schema @d"]);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::SchemaExtensionWithoutSchema]
  );

  // A type called `Query` that is not an object type is never presumed to be the query root, so
  // it defines no schema either.
  let errors = refused("interface Query { ok: Int } type Q { ok: Int } extend schema { query: Q }");
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::SchemaExtensionWithoutSchema]
  );

  // Nothing else moves: a document with no query root anywhere is still told so as well.
  let errors = refused("directive @d on SCHEMA extend schema @d");
  assert_eq!(
    errors.kinds(),
    vec![
      SchemaErrorKind::MissingQueryRootOperationType,
      SchemaErrorKind::SchemaExtensionWithoutSchema,
    ]
  );
}

/// What an `extend schema` may extend: a `schema` definition wherever it is written, or the schema
/// that omitting one defines around an object type named `Query` (Example 41).
#[test]
fn an_extend_schema_with_a_schema_to_extend_is_accepted() {
  for sdl in [
    "type Query { ok: Int } type M { go: Int } schema { query: Query } extend schema { mutation: M }",
    "type Query { ok: Int } type M { go: Int } extend schema { mutation: M } schema { query: Query }",
    "type Query { ok: Int } type M { go: Int } extend schema { mutation: M }",
  ] {
    let schema = built(sdl);
    assert_eq!(
      schema.root(RootOperation::Query),
      Some(ty(&schema, "Query")),
      "{sdl}"
    );
    assert_eq!(
      schema.root(RootOperation::Mutation),
      Some(ty(&schema, "M")),
      "{sdl}"
    );
  }

  // The definition may be in a later document than the extension.
  let extension = parse("extend schema { mutation: M } type M { go: Int }");
  let definition = parse("schema { query: Query } type Query { ok: Int }");
  let mut builder = SchemaBuilder::new();
  builder.document(&extension).document(&definition);
  let schema = builder
    .finish()
    .expect("an extension may precede the definition it extends");
  assert_eq!(schema.root(RootOperation::Mutation), Some(ty(&schema, "M")));
}

/// An `extend schema` that provides a root an omitted `schema` definition presumed is the same
/// duplicate a written definition's root would be.
///
/// The presumed roots are in place before any extension is applied: "A schema defines the initial
/// root operation type for each kind of operation it supports", and Example 41's schema is defined
/// with its query root presumed. graphql-js lets the extension's root replace the presumed one in
/// silence; this follows the draft instead.
#[test]
fn an_extend_schema_cannot_provide_a_presumed_root_again() {
  let sdl = "type Query { ok: Int } type Other { ok: Int } extend schema { query: Other }";
  let errors = refused(sdl);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::DuplicateRootOperationType]
  );
  assert_eq!(errors.len(), 1, "{errors}");
  let error = &errors.errors()[0];
  assert_eq!(error.subject(), "query");
  assert_eq!(&sdl[error.span().start()..error.span().end()], "Other");
  assert_eq!(
    error.span().start(),
    sdl.rfind("Other").expect("the extension's root")
  );
  let related = error.related().expect("the presumed root is pointed at");
  assert_eq!(&sdl[related.start()..related.end()], "Query");
  assert_eq!(related.start(), "type ".len());

  // Naming the presumed root itself again is the same duplicate.
  let errors = refused(
    "type Query { ok: Int } type Mutation { go: Int } extend schema { mutation: Mutation }",
  );
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::DuplicateRootOperationType]
  );
  assert_eq!(errors.errors()[0].subject(), "mutation");

  // A root nothing presumed is the extension's to provide.
  let schema = built("type Query { ok: Int } type S { s: Int } extend schema { subscription: S }");
  assert_eq!(
    schema.root(RootOperation::Subscription),
    Some(ty(&schema, "S"))
  );
}

/// The name index is a real index: every interned name resolves, nothing else does.
#[test]
fn the_name_index_resolves_exactly_what_was_interned() {
  let schema = built(STAR_WARS);
  for index in 0..schema.symbol_count() {
    let sym = smear::validator::schema::Sym::new(index);
    let bytes = schema.name_bytes(sym);
    assert_eq!(schema.sym(bytes), Some(sym), "{:?}", schema.name(sym));
    assert!(
      smear::validator::schema::is_name(bytes),
      "the arena admitted a non-name: {bytes:?}"
    );
  }
  assert_eq!(schema.sym(b"NoSuchName"), None);
  assert_eq!(schema.sym(b""), None);
}

/// A directive graph deep enough that a recursive walk would be a stack question.
///
/// The §3.13.1 check reaches every input type an argument can name, and an SDL's input-object
/// chain is bounded by nothing. This is the shape that would have found a recursive walk.
#[test]
fn a_deep_input_chain_does_not_recurse() {
  let mut sdl =
    String::from("type Query { ok: Int }\ndirective @d(a: In0) on INPUT_FIELD_DEFINITION\n");
  const DEPTH: usize = 20_000;
  for level in 0..DEPTH {
    sdl.push_str(&format!("input In{level} {{ next: In{} }}\n", level + 1));
  }
  sdl.push_str(&format!("input In{DEPTH} {{ end: Int }}\n"));
  built(&sdl);

  // The same chain, with the directive applied at the far end, is a cycle and is found.
  let mut sdl =
    String::from("type Query { ok: Int }\ndirective @d(a: In0) on INPUT_FIELD_DEFINITION\n");
  for level in 0..DEPTH {
    sdl.push_str(&format!("input In{level} {{ next: In{} }}\n", level + 1));
  }
  sdl.push_str(&format!("input In{DEPTH} {{ end: Int @d }}\n"));
  let errors = refused(&sdl);
  assert!(
    errors.contains_kind(SchemaErrorKind::SelfReferentialDirective),
    "{errors}"
  );
}

/// The same shape for draft §3.10.1(4), where the chain is made of *default values*.
///
/// Two properties in one document, and neither is provable by a small one:
///
/// * **iterative.** `InputObjectDefaultValueHasCycle` is written in the draft as two mutually
///   recursive functions, and the recursion that matters descends into a field's own default —
///   bounded by the number of defaulted fields, which is bounded by nothing. Twenty thousand links
///   is a stack overflow for a literal transcription.
/// * **linear in the number of starts.** The walk begins at every input object, so without the
///   settling described on `validate_input_object_default_cycles` this is twenty thousand walks of
///   average length ten thousand. Measured with the skip removed: **41.5 s** for this test alone,
///   against **0.9 s** for the whole file with it.
#[test]
fn a_deep_defaulted_input_chain_does_not_recurse() {
  const DEPTH: usize = 20_000;

  let mut sdl = String::from("type Query { ok: Int }\n");
  for level in 0..DEPTH {
    sdl.push_str(&format!(
      "input In{level} {{ next: In{} = {{}} }}\n",
      level + 1
    ));
  }
  sdl.push_str(&format!("input In{DEPTH} {{ end: Int }}\n"));
  built(&sdl);

  // The same chain closed into a cycle: the last link's default points back at the first, so no
  // coercion of `In0`'s default terminates.
  let mut sdl = String::from("type Query { ok: Int }\n");
  for level in 0..DEPTH {
    sdl.push_str(&format!(
      "input In{level} {{ next: In{} = {{}} }}\n",
      level + 1
    ));
  }
  sdl.push_str(&format!("input In{DEPTH} {{ next: In0 = {{}} }}\n"));
  let errors = refused(&sdl);
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::InputObjectDefaultValueCycle],
    "{errors}"
  );
  assert_eq!(
    errors.len(),
    1,
    "a cycle through twenty thousand objects is one diagnostic, not twenty thousand"
  );
}

/// Draft §3.10.1(4) is a property of the definitions, so reordering them cannot change the answer.
///
/// The settling that keeps `a_deep_defaulted_input_chain_does_not_recurse` linear used to be taken
/// from *any* retired frame, including one reached through a caller's supplied literal — which
/// explores a different question from the empty-map call and establishes nothing about it. With
/// `Outer` read first, its `{ loop: null }` marked `Bad` clean, `Bad`'s own start was then skipped,
/// and the build **accepted** a schema whose default cycle it refused when the same two definitions
/// were read the other way round.
#[test]
fn input_object_default_cycle_verdict_is_declaration_order_independent() {
  const QUERY: &str = "type Query { q(a: Outer, b: Bad): String }\n";
  const BAD: &str = "input Bad { loop: Bad = {} }\n";
  const OUTER: &str = "input Outer { b: Bad = { loop: null } }\n";

  for sdl in [
    format!("{QUERY}{OUTER}{BAD}"),
    format!("{QUERY}{BAD}{OUTER}"),
  ] {
    let errors = refused(&sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::InputObjectDefaultValueCycle],
      "{errors}"
    );
  }
}

// ---------------------------------------------------------------------------------------------
// what draft §4 returns beyond the shape
// ---------------------------------------------------------------------------------------------

/// A schema carrying every datum draft §4 returns that a `Schema` keeps: the schema's own
/// description, descriptions at every element that takes one — inline, block, and empty — both
/// deprecation answers on a field, an argument, an input field and an enum value, a directive
/// deprecated by its definition and another by an `extend directive`, a `@specifiedBy` on a
/// definition and another arriving by `extend scalar`, and defaults of every shape the canonical
/// printer writes.
const METADATA: &str = r#"
"""
The schema of a small library.
"""
schema { query: Query mutation: Mutation }

"A book's identifier, as printed on its spine."
scalar ISBN @specifiedBy(url: "https://example.com/isbn")

scalar Url
extend scalar Url @specifiedBy(url: "https://url.spec.whatwg.org/")

"Something with an id."
interface Node {
  "The id."
  id: ID!
}

interface Titled { title: String }

"""
A book.

  Indented continuation.
"""
type Book implements Titled & Node {
  "The book's id."
  id: ID!
  title: String
  "Who wrote it."
  author(
    "Return the pen name instead."
    penName: Boolean = false @deprecated(reason: "Always the pen name now.")
    "Which format."
    format: Format = HARDCOVER
  ): String @deprecated
  isbn: ISBN
  shelf: [Int] @deprecated(reason: "Use `location`.\nIt moved.")
}

enum Format {
  "Paper covers."
  PAPERBACK
  HARDCOVER @deprecated(reason: "Out of print")
  EBOOK @deprecated
  AUDIO
}

input BookFilter {
  "Exact title."
  title: String = "The \"Great\" Gatsby"
  formats: [Format!] = [PAPERBACK,EBOOK]
  near: GeoPoint = {lat: 1.5 lng: -0.25}
  note: String = """
    multi
      line
  """
  legacy: Int @deprecated(reason: "unused")
}

input GeoPoint { lat: Float lng: Float }

type Query {
  books(filter: BookFilter = {}, first: Int = 10, after: String): [Book!]!
  node(id: ID!): Node
}

type Mutation {
  ""
  addBook(title: String!): Book
}

"Marks a field as cached."
directive @cached(
  "Seconds."
  ttl: Int = 60
  scope: String
) repeatable on FIELD_DEFINITION

directive @audit on OBJECT

"Replaced by `@cached`."
directive @memo(seconds: Int) @deprecated(reason: "Use `@cached`.") on FIELD_DEFINITION

extend directive @audit @deprecated
"#;

fn ty(schema: &Schema, name: &str) -> TypeId {
  schema
    .type_by_name(name.as_bytes())
    .unwrap_or_else(|| panic!("`{name}` is defined"))
    .0
}

/// A field of a named type, found in declaration order, with its metadata.
fn member<'s>(schema: &'s Schema, owner: &str, name: &str) -> (&'s FieldDef, &'s FieldMeta) {
  schema
    .fields_in_order(ty(schema, owner))
    .find(|(field, _)| schema.name(field.name()) == name)
    .unwrap_or_else(|| panic!("`{owner}.{name}` is a field"))
}

/// An argument of a group, found in declaration order, with its metadata.
fn argument<'s>(
  schema: &'s Schema,
  args: Range32,
  name: &str,
) -> (&'s InputValueDef, &'s InputValueMeta) {
  schema
    .inputs_in_order(args)
    .find(|(arg, _)| schema.name(arg.name()) == name)
    .unwrap_or_else(|| panic!("`{name}` is an argument"))
}

/// Draft §4's `isDeprecated` and `deprecationReason`, as one value: `None` when not deprecated.
fn deprecated(schema: &Schema, deprecation: Deprecation) -> Option<Option<&str>> {
  deprecation
    .is_deprecated()
    .then(|| schema.text(deprecation.reason()))
}

/// The names of a group, in the order an iterator over it yields them.
fn names<T>(schema: &Schema, rows: impl Iterator<Item = T>, name: impl Fn(T) -> Sym) -> Vec<&str> {
  rows.map(|row| schema.name(name(row))).collect()
}

#[test]
fn every_datum_section_4_returns_is_kept() {
  let schema = built(METADATA);

  assert_eq!(schema.description(), Some("The schema of a small library."));

  // Types: an inline description, a block one whose dedent keeps a deeper line's indent, and none.
  let isbn = ty(&schema, "ISBN");
  assert_eq!(
    schema.text(schema.type_meta(isbn).description()),
    Some("A book's identifier, as printed on its spine.")
  );
  assert_eq!(
    schema.text(schema.type_meta(ty(&schema, "Book")).description()),
    Some("A book.\n\n  Indented continuation.")
  );
  assert_eq!(
    schema.text(schema.type_meta(ty(&schema, "Titled")).description()),
    None
  );

  // `specifiedByURL`: from the definition, from an `extend scalar`, and nothing for another kind.
  assert_eq!(
    schema.specified_by_url(isbn),
    Some("https://example.com/isbn")
  );
  assert_eq!(
    schema.specified_by_url(ty(&schema, "Url")),
    Some("https://url.spec.whatwg.org/")
  );
  assert_eq!(schema.specified_by_url(ty(&schema, "Book")), None);

  // Fields: a description, and each deprecation answer.
  let (_, id) = member(&schema, "Node", "id");
  assert_eq!(schema.text(id.description()), Some("The id."));
  let (author, meta) = member(&schema, "Book", "author");
  assert_eq!(schema.text(meta.description()), Some("Who wrote it."));
  assert_eq!(
    deprecated(&schema, meta.deprecation()),
    Some(Some("No longer supported")),
    "a bare `@deprecated` carries the specified default"
  );
  let (_, isbn_field) = member(&schema, "Book", "isbn");
  assert_eq!(deprecated(&schema, isbn_field.deprecation()), None);
  let (_, shelf) = member(&schema, "Book", "shelf");
  assert_eq!(
    deprecated(&schema, shelf.deprecation()),
    Some(Some("Use `location`.\nIt moved."))
  );
  let (_, title) = member(&schema, "Book", "title");
  assert_eq!(deprecated(&schema, title.deprecation()), None);
  assert_eq!(schema.text(title.description()), None);

  // Arguments: descriptions, canonical defaults, and deprecation.
  let (_, pen_name) = argument(&schema, author.args(), "penName");
  assert_eq!(
    schema.text(pen_name.description()),
    Some("Return the pen name instead.")
  );
  assert_eq!(schema.text(pen_name.default_value()), Some("false"));
  assert_eq!(
    deprecated(&schema, pen_name.deprecation()),
    Some(Some("Always the pen name now."))
  );
  let (_, format) = argument(&schema, author.args(), "format");
  assert_eq!(schema.text(format.default_value()), Some("HARDCOVER"));
  assert_eq!(deprecated(&schema, format.deprecation()), None);

  // Input fields: every default shape, printed canonically, and a deprecated field with none.
  let filter = ty(&schema, "BookFilter");
  let defaults: Vec<(&str, Option<&str>)> = schema
    .input_fields_in_order(filter)
    .map(|(field, meta)| (schema.name(field.name()), schema.text(meta.default_value())))
    .collect();
  assert_eq!(
    defaults,
    [
      ("title", Some("\"The \\\"Great\\\" Gatsby\"")),
      ("formats", Some("[PAPERBACK, EBOOK]")),
      ("near", Some("{lat: 1.5, lng: -0.25}")),
      ("note", Some("\"multi\\n  line\"")),
      ("legacy", None),
    ]
  );
  let (_, legacy) = schema
    .input_fields_in_order(filter)
    .find(|(field, _)| schema.name(field.name()) == "legacy")
    .expect("legacy");
  assert_eq!(
    deprecated(&schema, legacy.deprecation()),
    Some(Some("unused"))
  );

  // Enum values: a description, and each deprecation answer.
  let values: Vec<_> = schema
    .enum_values_in_order(ty(&schema, "Format"))
    .map(|(value, meta)| {
      (
        schema.name(value),
        schema.text(meta.description()),
        deprecated(&schema, meta.deprecation()),
      )
    })
    .collect();
  assert_eq!(
    values,
    [
      ("PAPERBACK", Some("Paper covers."), None),
      ("HARDCOVER", None, Some(Some("Out of print"))),
      ("EBOOK", None, Some(Some("No longer supported"))),
      ("AUDIO", None, None),
    ]
  );

  // Directives: a description on the definition and on an argument, and a default.
  let (cached, cached_meta) = schema
    .directives_in_order()
    .find(|(directive, _)| schema.name(directive.name()) == "cached")
    .expect("@cached");
  assert_eq!(
    schema.text(cached_meta.description()),
    Some("Marks a field as cached.")
  );
  let (_, ttl) = argument(&schema, cached.args(), "ttl");
  assert_eq!(schema.text(ttl.description()), Some("Seconds."));
  assert_eq!(schema.text(ttl.default_value()), Some("60"));
  let (_, scope) = argument(&schema, cached.args(), "scope");
  assert_eq!(schema.text(scope.description()), None);
  assert_eq!(schema.text(scope.default_value()), None);

  // And a directive's own deprecation: by its definition, by an `extend directive`, and none.
  let directives: Vec<_> = schema
    .directives_in_order()
    .map(|(directive, meta)| {
      (
        schema.name(directive.name()),
        schema.text(meta.description()),
        deprecated(&schema, meta.deprecation()),
      )
    })
    .collect();
  assert_eq!(
    directives,
    [
      ("cached", Some("Marks a field as cached."), None),
      ("audit", None, Some(Some("No longer supported"))),
      (
        "memo",
        Some("Replaced by `@cached`."),
        Some(Some("Use `@cached`."))
      ),
      (
        "skip",
        Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
        None
      ),
      (
        "include",
        Some(
          "Directs the executor to include this field or fragment only when the `if` argument is \
           true."
        ),
        None
      ),
      (
        "deprecated",
        Some("Marks an element of a GraphQL schema as no longer supported."),
        None
      ),
      (
        "specifiedBy",
        Some("Exposes a URL that specifies the behavior of this scalar."),
        None
      ),
      (
        "oneOf",
        Some("Indicates exactly one field must be supplied and this field must not be `null`."),
        None
      ),
    ]
  );
}

/// Draft §3.13.3 declares the specified `@deprecated`'s `reason` as `String!`, so
/// `@deprecated(reason: null)` offers its argument a value it does not accept, at every location
/// the directive may stand — and is refused, where it was once read as deprecated with no reason.
/// A document that redefines `@deprecated` with a nullable `reason` is the one place a `null`
/// reason still builds, and it is kept as written: deprecated, with no reason.
#[test]
fn a_null_deprecation_reason_is_refused() {
  for sdl in [
    "type Query { ok: Int @deprecated(reason: null) }",
    "type Query { ok(a: Int @deprecated(reason: null)): Int }",
    "type Query { ok: Int } input In { a: Int @deprecated(reason: null) }",
    "type Query { ok: Int } enum E { A B @deprecated(reason: null) }",
    "type Query { ok: Int } directive @d @deprecated(reason: null) on FIELD",
    "type Query { ok: Int } directive @d on FIELD extend directive @d @deprecated(reason: null)",
  ] {
    let errors = refused(sdl);
    assert_eq!(
      errors.kinds(),
      vec![SchemaErrorKind::InvalidDirectiveArgumentValue],
      "{sdl}"
    );
  }

  let redefined = built(
    "directive @deprecated(reason: String) on FIELD_DEFINITION
     type Query { ok: Int @deprecated(reason: null) }",
  );
  let (_, ok) = member(&redefined, "Query", "ok");
  assert_eq!(deprecated(&redefined, ok.deprecation()), Some(None));
}

/// A bare `@deprecated` at every location the directive may stand, for the tests below to set
/// beside whichever definition of `@deprecated` they mean: a field, a field's argument, a field an
/// `extend type` adds, an input field, an enum value, a directive definition, a directive's
/// argument and an `extend directive`.
const BARE_USAGES: &str = "
type Query { old(by: Int @deprecated): Int @deprecated }
extend type Query { added: Int @deprecated }
input Filter { old: Int @deprecated }
enum Verdict { YES NO @deprecated }
directive @gone(by: Int @deprecated) @deprecated on FIELD
directive @faded on FIELD
extend directive @faded @deprecated
";

/// The five locations the specified `@deprecated` declares, for a replacement to declare as well.
const DEPRECATED_LOCATIONS: &str = "FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE | DIRECTIVE_DEFINITION";

/// What each bare `@deprecated` in [`BARE_USAGES`] came back as, in that document's order, read by
/// [`deprecated`].
fn bare_deprecations(schema: &Schema) -> [(&'static str, Option<Option<&str>>); 8] {
  let (old, old_meta) = member(schema, "Query", "old");
  let (_, by) = argument(schema, old.args(), "by");
  let (_, added) = member(schema, "Query", "added");
  let (_, input_field) = schema
    .input_fields_in_order(ty(schema, "Filter"))
    .find(|(field, _)| schema.name(field.name()) == "old")
    .expect("Filter.old");
  let (_, value) = schema
    .enum_values_in_order(ty(schema, "Verdict"))
    .find(|(value, _)| schema.name(*value) == "NO")
    .expect("Verdict.NO");
  let (gone, gone_meta) = schema
    .directives_in_order()
    .find(|(directive, _)| schema.name(directive.name()) == "gone")
    .expect("@gone");
  let (_, gone_by) = argument(schema, gone.args(), "by");
  let (_, faded) = schema
    .directives_in_order()
    .find(|(directive, _)| schema.name(directive.name()) == "faded")
    .expect("@faded");
  [
    ("a field", deprecated(schema, old_meta.deprecation())),
    ("a field's argument", deprecated(schema, by.deprecation())),
    (
      "a field an `extend type` adds",
      deprecated(schema, added.deprecation()),
    ),
    (
      "an input field",
      deprecated(schema, input_field.deprecation()),
    ),
    ("an enum value", deprecated(schema, value.deprecation())),
    (
      "a directive definition",
      deprecated(schema, gone_meta.deprecation()),
    ),
    (
      "a directive's argument",
      deprecated(schema, gone_by.deprecation()),
    ),
    (
      "an `extend directive`",
      deprecated(schema, faded.deprecation()),
    ),
  ]
}

/// Asserts that every bare `@deprecated` in a schema built over [`BARE_USAGES`] came back
/// deprecated with `reason`.
fn assert_every_bare_reason(schema: &Schema, reason: Option<&str>, sdl: &str) {
  for (location, found) in bare_deprecations(schema) {
    assert_eq!(found, Some(reason), "{location}\n---\n{sdl}");
  }
}

/// A bare `@deprecated` takes the default of its definition's `reason`, as draft §6.4.1 gives any
/// argument a usage leaves out, and the specified definition's is `No longer supported` — at every
/// location the directive may stand.
#[test]
fn a_bare_deprecated_takes_the_specified_default() {
  assert_every_bare_reason(
    &built(BARE_USAGES),
    Some("No longer supported"),
    BARE_USAGES,
  );
}

/// A document that replaces `@deprecated` with a default of its own gives every bare usage that
/// default, not the specified one's.
#[test]
fn a_bare_deprecated_takes_a_replacements_string_default() {
  let sdl = format!(
    "directive @deprecated(reason: String = \"Custom\") on {DEPRECATED_LOCATIONS}\n{BARE_USAGES}"
  );
  assert_every_bare_reason(&built(&sdl), Some("Custom"), &sdl);
}

/// A replacement whose `reason` defaults to `null` gives every bare usage a `null` reason —
/// deprecated, with no reason — and not the specified default.
#[test]
fn a_bare_deprecated_takes_a_replacements_null_default() {
  let sdl = format!(
    "directive @deprecated(reason: String = null) on {DEPRECATED_LOCATIONS}\n{BARE_USAGES}"
  );
  assert_every_bare_reason(&built(&sdl), None, &sdl);
}

/// A replacement that gives a bare usage no string to take gives it a `null` reason: a nullable
/// `reason` with no default, which leaves the usage no value at all; no `reason` argument; and a
/// default of another shape, which reads as a written `reason` of that shape does. A non-null
/// `reason` with no default is the one exception, because a bare usage of it omits a required
/// argument, and the build refuses that.
#[test]
fn a_bare_deprecated_with_no_string_default_to_take_has_a_null_reason() {
  for definition in [
    "directive @deprecated(reason: String)",
    "directive @deprecated",
    "directive @deprecated(reason: Int = 3)",
  ] {
    let sdl = format!("{definition} on {DEPRECATED_LOCATIONS}\n{BARE_USAGES}");
    assert_every_bare_reason(&built(&sdl), None, &sdl);
  }

  let errors = refused(
    "directive @deprecated(reason: String!) on FIELD_DEFINITION
     type Query { ok: Int @deprecated }",
  );
  assert_eq!(
    errors.kinds(),
    vec![SchemaErrorKind::MissingRequiredDirectiveArgument]
  );
}

/// The definition a bare `@deprecated` takes its reason from is the one the build ends with,
/// wherever it is written: a replacement after its bare usages in the same document, or in a later
/// document of the same build, still decides every one of them.
#[test]
fn a_replacement_written_after_its_bare_usages_still_decides_their_reason() {
  let replacement =
    format!("directive @deprecated(reason: String = \"Custom\") on {DEPRECATED_LOCATIONS}");

  let sdl = format!("{BARE_USAGES}\n{replacement}");
  assert_every_bare_reason(&built(&sdl), Some("Custom"), &sdl);

  let usages = parse(BARE_USAGES);
  let later = parse(&replacement);
  let mut builder = SchemaBuilder::new();
  builder.document(&usages);
  builder.document(&later);
  let schema = builder
    .finish()
    .unwrap_or_else(|errors| panic!("expected a schema, got:\n{errors}"));
  assert_every_bare_reason(
    &schema,
    Some("Custom"),
    "the replacement in a later document",
  );
}

/// Absent is not empty: `""` is a description of no bytes, and returned as one.
#[test]
fn an_empty_description_is_not_an_absent_one() {
  let schema = built(METADATA);
  let (_, add_book) = member(&schema, "Mutation", "addBook");
  assert_eq!(schema.text(add_book.description()), Some(""));
  assert!(add_book.description().is_some());
  let (_, node) = member(&schema, "Query", "node");
  assert_eq!(schema.text(node.description()), None);
  assert!(node.description().is_none());
}

/// Every group comes back in the order the document declared it, while lookups still binary
/// search the symbol-sorted tables.
#[test]
fn declaration_order_is_kept_beside_the_sorted_tables() {
  let schema = built(METADATA);

  let book = ty(&schema, "Book");
  assert_eq!(
    names(&schema, schema.fields_in_order(book), |(field, _)| field
      .name()),
    ["id", "title", "author", "isbn", "shelf", "__typename"],
    "declared fields first, the meta-field after them"
  );
  let query = ty(&schema, "Query");
  assert_eq!(
    names(&schema, schema.fields_in_order(query), |(field, _)| field
      .name()),
    ["books", "node", "__typename", "__schema", "__type"]
  );

  let (author, _) = member(&schema, "Book", "author");
  assert_eq!(
    names(
      &schema,
      schema.inputs_in_order(author.args()),
      |(arg, _)| arg.name()
    ),
    ["penName", "format"]
  );
  let (books, _) = member(&schema, "Query", "books");
  assert_eq!(
    names(&schema, schema.inputs_in_order(books.args()), |(arg, _)| {
      arg.name()
    }),
    ["filter", "first", "after"]
  );

  assert_eq!(
    names(
      &schema,
      schema.input_fields_in_order(ty(&schema, "BookFilter")),
      |(field, _)| field.name()
    ),
    ["title", "formats", "near", "note", "legacy"]
  );
  assert_eq!(
    names(
      &schema,
      schema.enum_values_in_order(ty(&schema, "Format")),
      |(value, _)| value
    ),
    ["PAPERBACK", "HARDCOVER", "EBOOK", "AUDIO"]
  );

  // `Node` is defined before `Titled`, so the closure — sorted by type id — lists it first, and the
  // declaration order is the other way round.
  let interface = |id: TypeId| schema.type_def(id).name();
  assert_eq!(
    names(
      &schema,
      schema.interfaces_of(book).iter().copied(),
      interface
    ),
    ["Node", "Titled"]
  );
  assert_eq!(
    names(&schema, schema.interfaces_in_order(book), interface),
    ["Titled", "Node"]
  );

  // The document's own directives in definition order, then each specified one it did not spell
  // out, in the order the built-in SDL declares them.
  assert_eq!(
    names(&schema, schema.directives_in_order(), |(directive, _)| {
      directive.name()
    }),
    [
      "cached",
      "audit",
      "memo",
      "skip",
      "include",
      "deprecated",
      "specifiedBy",
      "oneOf"
    ]
  );

  // The tables the lookups read are still sorted, so the orders above are permutations beside them
  // rather than the order they are stored in.
  for (index, _) in schema.types().iter().enumerate() {
    let id = TypeId::new(index as u32);
    for order in [
      schema.field_order_of(id),
      schema.input_field_order_of(id),
      schema.enum_value_order_of(id),
      schema.interface_order_of(id),
    ] {
      let mut sorted = order.to_vec();
      sorted.sort_unstable();
      assert!(
        sorted
          .iter()
          .enumerate()
          .all(|(at, &position)| position as usize == at),
        "an order table is not a permutation of its group: {order:?}"
      );
    }
  }
}

/// [`builtin::BUILT_IN_SCALAR_DESCRIPTIONS`] is what the build injects, scalar by scalar: the text
/// the introspection door compares a response's built-in scalar with before leaving it to the
/// build.
#[test]
fn the_built_in_scalar_descriptions_are_the_injected_ones() {
  let schema = built(ROOT);
  for (name, text) in builtin::BUILT_IN_SCALARS
    .iter()
    .zip(builtin::BUILT_IN_SCALAR_DESCRIPTIONS)
  {
    assert_eq!(
      schema.text(schema.type_meta(ty(&schema, name)).description()),
      Some(text),
      "{name}"
    );
  }
}

/// The built-ins carry graphql-js's descriptions, so introspecting them answers what the reference
/// implementation answers — and their defaults print in the same canonical form.
#[test]
fn the_built_ins_carry_graphql_js_descriptions() {
  let schema = built(ROOT);

  assert_eq!(
    schema.text(schema.type_meta(ty(&schema, "Int")).description()),
    Some(
      "The `Int` scalar type represents non-fractional signed whole numeric values. Int can \
       represent values between -(2^31) and 2^31 - 1."
    )
  );
  let id = schema
    .text(schema.type_meta(ty(&schema, "ID")).description())
    .expect("ID is described");
  assert!(
    id.contains("(such as `\"4\"`)"),
    "a quote inside a block string is content: {id}"
  );
  let type_ = schema
    .text(schema.type_meta(ty(&schema, "__Type")).description())
    .expect("__Type is described");
  assert!(
    type_.contains("`__TypeKind` enum.\n\nDepending on the kind"),
    "{type_}"
  );
  let (_, default_value) = member(&schema, "__InputValue", "defaultValue");
  assert_eq!(
    schema.text(default_value.description()),
    Some("A GraphQL-formatted string representing the default value for this input value.")
  );

  let (deprecated_directive, meta) = schema
    .directives_in_order()
    .find(|(directive, _)| schema.name(directive.name()) == "deprecated")
    .expect("@deprecated");
  assert_eq!(
    schema.text(meta.description()),
    Some("Marks an element of a GraphQL schema as no longer supported.")
  );
  let (_, reason) = argument(&schema, deprecated_directive.args(), "reason");
  assert_eq!(
    schema.text(reason.default_value()),
    Some("\"No longer supported\"")
  );
  assert!(
    schema
      .text(reason.description())
      .is_some_and(|text| text.starts_with("Explains why this element was deprecated")),
    "the block string's indentation came off"
  );

  let (fields, _) = member(&schema, "__Type", "fields");
  let (_, include_deprecated) = argument(&schema, fields.args(), "includeDeprecated");
  assert_eq!(
    schema.text(include_deprecated.default_value()),
    Some("false")
  );

  // The three meta-fields have no SDL of their own, and describe nothing.
  let (_, typename) = member(&schema, "Query", "__typename");
  assert_eq!(*typename, FieldMeta::EMPTY);
}

/// The two largest public schemas in the fixture tree build, and keep every description they
/// write.
///
/// Both were printed by GraphQL servers, so every description in them is a block string and no
/// block string in them is anything else: the `"""` delimiters, halved, count the described
/// elements. The build adds the built-ins' descriptions on top, so only what the document defined
/// is counted on the schema's side.
#[test]
fn the_large_public_schemas_keep_every_description() {
  const GITHUB: &str = include_str!("fixtures/schemas/github_schema.graphql");
  const GITLAB: &str = include_str!("fixtures/schemas/gitlab_schema.graphql");

  fn described(schema: &Schema) -> usize {
    let mut count = usize::from(schema.description().is_some());
    for (index, def) in schema.types().iter().enumerate() {
      if def.is_built_in() {
        continue;
      }
      let id = TypeId::new(index as u32);
      count += usize::from(schema.type_meta(id).description().is_some());
      for (field, meta) in schema.fields_in_order(id) {
        if schema.name(field.name()).starts_with("__") {
          continue;
        }
        count += usize::from(meta.description().is_some());
        count += schema
          .inputs_in_order(field.args())
          .filter(|(_, meta)| meta.description().is_some())
          .count();
      }
      count += schema
        .input_fields_in_order(id)
        .filter(|(_, meta)| meta.description().is_some())
        .count();
      count += schema
        .enum_values_in_order(id)
        .filter(|(_, meta)| meta.description().is_some())
        .count();
    }
    for (directive, meta) in schema.directives_in_order() {
      if directive.is_built_in() {
        continue;
      }
      count += usize::from(meta.description().is_some());
      count += schema
        .inputs_in_order(directive.args())
        .filter(|(_, meta)| meta.description().is_some())
        .count();
    }
    count
  }

  for (name, sdl) in [("github", GITHUB), ("gitlab", GITLAB)] {
    let delimiters = sdl.matches("\"\"\"").count();
    assert_eq!(delimiters % 2, 0, "{name}: the block delimiters pair up");
    let schema = built(sdl);
    assert_eq!(
      described(&schema),
      delimiters / 2,
      "{name}: a description the document wrote is not one the schema keeps"
    );
  }
}
