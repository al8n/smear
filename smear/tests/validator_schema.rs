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
//! Two kinds are excused, each with a written reason, in [`UNFIREABLE`].

// Every fixture and assertion below calls into `smear::validator`, which does not exist in the
// crate's API surface with the feature off. Every other file in this directory gates itself to
// match its real dependency (`rowan`, a dialect, or both); this one did not, so it was a hard
// `E0433` compile error — not a cfg'd-out no-op — under any feature selection that excludes
// `validator`, including plain `--features graphql,parser`.
#![cfg(feature = "validator")]
#![allow(missing_docs)]

use smear::{
  lexer::tokora::{Parse as _, Parser, SimpleSpan},
  parser::graphql::{
    GraphQL,
    ast::{
      Described, Name, ScalarTypeDefinition, TypeDefinition, TypeSystemDefinition,
      TypeSystemDefinitionOrExtension, TypeSystemDocument,
    },
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  validator::schema::{
    DefaultKind, RootOperation, Schema, SchemaBuilder, SchemaErrorKind, SchemaErrors, TypeKind,
    builtin,
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
    SchemaErrorKind::TooManyNames,
    "the name index addresses 2^30 symbols; a document that interns more would need gigabytes of \
     distinct identifiers, so the limit is unreachable in a test.",
  ),
];

/// Every kind, with a schema that makes it fire and the complete set of kinds that schema
/// produces.
///
/// Pinning the whole set, not just the kind under test, is what keeps a fixture from quietly
/// becoming a test of two rules at once.
#[allow(clippy::type_complexity)]
const FIXTURES: &[(SchemaErrorKind, &str, &[SchemaErrorKind])] = &[
  // -- §3.3 -----------------------------------------------------------------------------------
  (
    SchemaErrorKind::DuplicateTypeName,
    "type Query { ok: Int } type Dup { a: Int } type Dup { b: Int }",
    &[SchemaErrorKind::DuplicateTypeName],
  ),
  (
    SchemaErrorKind::DuplicateDirectiveDefinition,
    "type Query { ok: Int } directive @d on FIELD directive @d on QUERY",
    &[SchemaErrorKind::DuplicateDirectiveDefinition],
  ),
  (
    SchemaErrorKind::DuplicateSchemaDefinition,
    "type Query { ok: Int } schema { query: Query } schema { query: Query }",
    &[SchemaErrorKind::DuplicateSchemaDefinition],
  ),
  (
    SchemaErrorKind::DuplicateRootOperationType,
    "type Query { ok: Int } type Other { ok: Int } schema { query: Query query: Other }",
    &[SchemaErrorKind::DuplicateRootOperationType],
  ),
  (
    SchemaErrorKind::UndefinedRootOperationType,
    "type Query { ok: Int } schema { query: Query mutation: Nope }",
    &[SchemaErrorKind::UndefinedRootOperationType],
  ),
  (
    SchemaErrorKind::RootOperationTypeNotObject,
    "interface Root { ok: Int } schema { query: Root }",
    &[SchemaErrorKind::RootOperationTypeNotObject],
  ),
  (
    SchemaErrorKind::MissingQueryRootOperationType,
    "type NotTheRoot { ok: Int }",
    &[SchemaErrorKind::MissingQueryRootOperationType],
  ),
  (
    SchemaErrorKind::UndefinedExtensionTarget,
    "type Query { ok: Int } extend type Nope { more: Int }",
    &[SchemaErrorKind::UndefinedExtensionTarget],
  ),
  (
    SchemaErrorKind::ExtensionKindMismatch,
    "type Query { ok: Int } interface Iface { a: Int } extend type Iface { b: Int }",
    &[SchemaErrorKind::ExtensionKindMismatch],
  ),
  (
    SchemaErrorKind::RedefinedBuiltInType,
    "type Query { ok: Int } type __Type { name: String }",
    &[SchemaErrorKind::RedefinedBuiltInType],
  ),
  (
    SchemaErrorKind::ReservedTypeName,
    "type Query { ok: Int } type __Mine { a: Int }",
    &[SchemaErrorKind::ReservedTypeName],
  ),
  (
    SchemaErrorKind::UndefinedType,
    "type Query { ok: Nope }",
    &[SchemaErrorKind::UndefinedType],
  ),
  (
    SchemaErrorKind::TypeReferenceTooDeep,
    "type Query { ok: [[[[[[[[[[[[[[[[Int]]]]]]]]]]]]]]]] }",
    &[SchemaErrorKind::TypeReferenceTooDeep],
  ),
  // -- §3.6 / §3.7 ------------------------------------------------------------------------------
  (
    SchemaErrorKind::EmptyFieldsDefinition,
    "type Query { ok: Int } type Empty",
    &[SchemaErrorKind::EmptyFieldsDefinition],
  ),
  (
    SchemaErrorKind::DuplicateFieldName,
    "type Query { ok: Int dup: Int dup: String }",
    &[SchemaErrorKind::DuplicateFieldName],
  ),
  (
    SchemaErrorKind::ReservedFieldName,
    "type Query { ok: Int __mine: Int }",
    &[SchemaErrorKind::ReservedFieldName],
  ),
  (
    SchemaErrorKind::FieldTypeNotOutputType,
    "type Query { ok: In } input In { a: Int }",
    &[SchemaErrorKind::FieldTypeNotOutputType],
  ),
  (
    SchemaErrorKind::DuplicateArgumentName,
    "type Query { ok(a: Int, a: String): Int }",
    &[SchemaErrorKind::DuplicateArgumentName],
  ),
  (
    SchemaErrorKind::ReservedArgumentName,
    "type Query { ok(__a: Int): Int }",
    &[SchemaErrorKind::ReservedArgumentName],
  ),
  (
    SchemaErrorKind::ArgumentTypeNotInputType,
    "type Query { ok(a: Query): Int }",
    &[SchemaErrorKind::ArgumentTypeNotInputType],
  ),
  (
    SchemaErrorKind::ImplementsNonInterface,
    "type Query { ok: Int } type Thing { a: Int } type Other implements Thing { a: Int }",
    &[SchemaErrorKind::ImplementsNonInterface],
  ),
  (
    SchemaErrorKind::UndefinedImplementsInterface,
    "type Query { ok: Int } type Other implements Nope { a: Int }",
    &[SchemaErrorKind::UndefinedImplementsInterface],
  ),
  (
    SchemaErrorKind::DuplicateImplementsInterface,
    "type Query { ok: Int } interface I { a: Int } type T implements I & I { a: Int }",
    &[SchemaErrorKind::DuplicateImplementsInterface],
  ),
  (
    SchemaErrorKind::SelfImplementingInterface,
    "type Query { ok: Int } interface I implements I { a: Int }",
    &[SchemaErrorKind::SelfImplementingInterface],
  ),
  (
    SchemaErrorKind::MissingTransitiveInterface,
    "type Query { ok: Int }
     interface A { a: Int }
     interface B implements A { a: Int }
     type T implements B { a: Int }",
    &[SchemaErrorKind::MissingTransitiveInterface],
  ),
  (
    SchemaErrorKind::MissingInterfaceField,
    "type Query { ok: Int } interface I { a: Int b: Int } type T implements I { a: Int }",
    &[SchemaErrorKind::MissingInterfaceField],
  ),
  (
    SchemaErrorKind::InvalidInterfaceFieldType,
    "type Query { ok: Int } interface I { a: Int } type T implements I { a: String }",
    &[SchemaErrorKind::InvalidInterfaceFieldType],
  ),
  (
    SchemaErrorKind::MissingInterfaceFieldArgument,
    "type Query { ok: Int } interface I { a(x: Int): Int } type T implements I { a: Int }",
    &[SchemaErrorKind::MissingInterfaceFieldArgument],
  ),
  (
    SchemaErrorKind::InvalidInterfaceFieldArgumentType,
    "type Query { ok: Int }
     interface I { a(x: Int): Int }
     type T implements I { a(x: String): Int }",
    &[SchemaErrorKind::InvalidInterfaceFieldArgumentType],
  ),
  (
    SchemaErrorKind::UnexpectedRequiredArgument,
    "type Query { ok: Int } interface I { a: Int } type T implements I { a(x: Int!): Int }",
    &[SchemaErrorKind::UnexpectedRequiredArgument],
  ),
  // -- §3.8 -------------------------------------------------------------------------------------
  (
    SchemaErrorKind::EmptyUnionMembers,
    "type Query { ok: Int } union U",
    &[SchemaErrorKind::EmptyUnionMembers],
  ),
  (
    SchemaErrorKind::UnionMemberNotObject,
    "type Query { ok: Int } interface I { a: Int } union U = I",
    &[SchemaErrorKind::UnionMemberNotObject],
  ),
  (
    SchemaErrorKind::UndefinedUnionMember,
    "type Query { ok: Int } union U = Nope",
    &[SchemaErrorKind::UndefinedUnionMember],
  ),
  (
    SchemaErrorKind::DuplicateUnionMember,
    "type Query { ok: Int } type A { a: Int } union U = A | A",
    &[SchemaErrorKind::DuplicateUnionMember],
  ),
  // -- §3.9 -------------------------------------------------------------------------------------
  (
    SchemaErrorKind::EmptyEnumValues,
    "type Query { ok: Int } enum E",
    &[SchemaErrorKind::EmptyEnumValues],
  ),
  (
    SchemaErrorKind::DuplicateEnumValue,
    "type Query { ok: Int } enum E { A A }",
    &[SchemaErrorKind::DuplicateEnumValue],
  ),
  (
    SchemaErrorKind::ReservedEnumValueName,
    "type Query { ok: Int } enum E { __A }",
    &[SchemaErrorKind::ReservedEnumValueName],
  ),
  // -- §3.10 ------------------------------------------------------------------------------------
  (
    SchemaErrorKind::EmptyInputFields,
    "type Query { ok: Int } input In",
    &[SchemaErrorKind::EmptyInputFields],
  ),
  (
    SchemaErrorKind::DuplicateInputFieldName,
    "type Query { ok: Int } input In { a: Int a: String }",
    &[SchemaErrorKind::DuplicateInputFieldName],
  ),
  (
    SchemaErrorKind::ReservedInputFieldName,
    "type Query { ok: Int } input In { __a: Int }",
    &[SchemaErrorKind::ReservedInputFieldName],
  ),
  (
    SchemaErrorKind::InputFieldTypeNotInputType,
    "type Query { ok: Int } input In { a: Query }",
    &[SchemaErrorKind::InputFieldTypeNotInputType],
  ),
  (
    SchemaErrorKind::OneOfFieldNotNullable,
    "type Query { ok: Int } input In @oneOf { a: Int! }",
    &[SchemaErrorKind::OneOfFieldNotNullable],
  ),
  (
    SchemaErrorKind::OneOfFieldHasDefault,
    "type Query { ok: Int } input In @oneOf { a: Int = 1 }",
    &[SchemaErrorKind::OneOfFieldHasDefault],
  ),
  (
    SchemaErrorKind::CircularNonNullInputField,
    "type Query { ok: Int } input A { b: B! } input B { a: A! }",
    &[SchemaErrorKind::CircularNonNullInputField],
  ),
  // -- §3.13 ------------------------------------------------------------------------------------
  (
    SchemaErrorKind::ReservedDirectiveName,
    "type Query { ok: Int } directive @__mine on FIELD",
    &[SchemaErrorKind::ReservedDirectiveName],
  ),
  (
    SchemaErrorKind::DuplicateDirectiveArgumentName,
    "type Query { ok: Int } directive @d(a: Int, a: String) on FIELD",
    &[SchemaErrorKind::DuplicateDirectiveArgumentName],
  ),
  (
    SchemaErrorKind::ReservedDirectiveArgumentName,
    "type Query { ok: Int } directive @d(__a: Int) on FIELD",
    &[SchemaErrorKind::ReservedDirectiveArgumentName],
  ),
  (
    SchemaErrorKind::DirectiveArgumentTypeNotInputType,
    "type Query { ok: Int } directive @d(a: Query) on FIELD",
    &[SchemaErrorKind::DirectiveArgumentTypeNotInputType],
  ),
  (
    SchemaErrorKind::SelfReferentialDirective,
    "type Query { ok: Int }
     directive @d(a: In) on INPUT_FIELD_DEFINITION
     input In { x: Int @d }",
    &[SchemaErrorKind::SelfReferentialDirective],
  ),
  // -- §3.13, at a use site -----------------------------------------------------------------
  //
  // Every SDL below is the probe `benchmarks`'s differential oracle used to find the gap these
  // six kinds close (smear issue #91), verbatim. Each is refused by `apollo-compiler`, and each
  // was built without complaint before the use-site pass existed.
  (
    SchemaErrorKind::UndefinedDirective,
    "type Query @nowhere { ok: Int }",
    &[SchemaErrorKind::UndefinedDirective],
  ),
  (
    SchemaErrorKind::UnsupportedDirectiveLocation,
    "directive @onEnum on ENUM
     type Query @onEnum { ok: Int }",
    &[SchemaErrorKind::UnsupportedDirectiveLocation],
  ),
  (
    SchemaErrorKind::DuplicateDirectiveUse,
    "directive @onObject on OBJECT
     type Query @onObject @onObject { ok: Int }",
    &[SchemaErrorKind::DuplicateDirectiveUse],
  ),
  (
    SchemaErrorKind::UndefinedDirectiveArgument,
    "directive @onObject(a: Int) on OBJECT
     type Query @onObject(b: 1) { ok: Int }",
    &[SchemaErrorKind::UndefinedDirectiveArgument],
  ),
  (
    SchemaErrorKind::MissingRequiredDirectiveArgument,
    "directive @onObject(a: Int!) on OBJECT
     type Query @onObject { ok: Int }",
    &[SchemaErrorKind::MissingRequiredDirectiveArgument],
  ),
  (
    SchemaErrorKind::InvalidDirectiveArgumentValue,
    "directive @onObject(a: Int) on OBJECT
     type Query @onObject(a: \"x\") { ok: Int }",
    &[SchemaErrorKind::InvalidDirectiveArgumentValue],
  ),
];

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
    SchemaErrorKind::ALL.len() >= 56,
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
  for (kind, sdl, _) in FIXTURES {
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
     schema @onSchema { query: Query }
     type Query @onObject { ok(a: Int @onArgDef): Int @onFieldDef }
     scalar Custom @onScalar
     interface I @onInterface { ok: Int }
     type Impl implements I @onObject { ok: Int }
     union U @onUnion = Query | Impl
     enum E @onEnum { A @onEnumValue }
     input In @onInputObject { a: Int @onInputField }",
  );

  // The same location reached through an extension, including `extend schema`.
  built(
    "directive @onSchema repeatable on SCHEMA
     directive @onObject on OBJECT
     directive @onEnumValue on ENUM_VALUE
     schema @onSchema { query: Query }
     extend schema @onSchema
     type Query { ok: Int }
     type T @onObject { a: Int }
     enum E { A }
     extend enum E { B @onEnumValue }",
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
       # An extra optional argument is fine; an extra required one is not (fixtured above).
       extra(x: Int, y: Int = 1): Int
     }
     interface Container { item: Node }
     # An object is a valid implementation of an interface it implements.
     type Box implements Container { item: Impl }
     interface ListContainer { items: [Node] }
     type ListBox implements ListContainer { items: [Impl!]! }",
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
