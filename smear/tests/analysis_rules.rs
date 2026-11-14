//! The GraphQLx analyzer's own rules, which `analyze` runs over every supplied package once its
//! phases have written the model: a program defines its schema at most once; only the root
//! package defines or extends the schema; and a package extends its own definitions alone, but
//! for the directives the root package adds to a built-in, none of which may give the built-in a
//! built-in meaning or a host type. Each rule says nothing about what a phase already refused,
//! marks what it reports, and quotes a name only as an excerpt. The fourth rule, that an enum takes
//! no type parameter, is tested in `analysis_types`, beside the phase that types each use of the
//! enum, and the rules after it, on what is judged of types, in `analysis_type_rules`; the rule on
//! `@extern`'s use in `analysis_host`, beside the binding it reads. GraphQL's own rules on what a
//! definition holds are tested in `analysis_definition_rules`, and those on directives and their
//! applications in `analysis_directive_rules`; those on the schema, that a root operation type is
//! an object type, that the roots have distinct types, that a schema extension extends a schema
//! that is defined and that a schema has a query root, are tested here.
//!
//! Then the rule set itself: every code a rule lists fires on a fixture; and the walk the rules
//! ride, over an analyzed program of two packages, written out hook by hook.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Collection, Contribution, Cx, DefId, Diagnostic, DiagnosticKind,
    Layout, Limits, MemberId, Model, ModuleId, Namespace, Package, PackageId, Packages, Primitive,
    Root, RootOperation, SourceFile, SourceId, Tier, TypeExprId, ValueId, Visitor, analyze,
    test_support::RULES, visit,
  },
  diagnostic::{Code, Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a second schema definition, which the analyzer shares.
const DUPLICATE: &str = "smear::schema::duplicate-schema-definition";

/// The code for a schema item a package other than the root package writes.
const OUTSIDE: &str = "smear::graphqlx::schema-outside-root";

/// The code for an extension of a definition its package may not extend.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// GraphQL's code for the root package's extension that adds more than directives to an
/// introspection type, which the analyzer shares.
const INTROSPECTION: &str = "smear::schema::introspection-type-extension";

/// The code for an enum that declares a type parameter.
const GENERIC_ENUM: &str = "smear::graphqlx::generic-enum";

/// The code for a marker used as a marker may not be.
const MARKER_MISUSE: &str = "smear::graphqlx::marker-misuse";

/// The code for a collection whose count of type parameters it has not.
const COLLECTION_ARITY: &str = "smear::graphqlx::collection-arity";

/// The code for `TypeRef` where it may not stand.
const TYPEREF_MISUSE: &str = "smear::graphqlx::typeref-misuse";

/// The code for `NonZero` of what is no integer primitive.
const NONZERO_ARGUMENT: &str = "smear::graphqlx::nonzero-argument";

/// The code for a type parameter with the name of an earlier one.
const DUPLICATE_TYPE_PARAMETER: &str = "smear::graphqlx::duplicate-type-parameter";

/// GraphQL's codes for a member whose name begins with `__`, by what the member is.
const RESERVED_FIELD_NAME: &str = "smear::schema::reserved-field-name";
const RESERVED_ARGUMENT_NAME: &str = "smear::schema::reserved-argument-name";
const RESERVED_ENUM_VALUE_NAME: &str = "smear::schema::reserved-enum-value-name";
const RESERVED_INPUT_FIELD_NAME: &str = "smear::schema::reserved-input-field-name";
const RESERVED_DIRECTIVE_ARGUMENT_NAME: &str = "smear::schema::reserved-directive-argument-name";

/// GraphQL's codes for a member whose name an earlier member of its list has, by the list.
const DUPLICATE_FIELD_NAME: &str = "smear::schema::duplicate-field-name";
const DUPLICATE_ARGUMENT_NAME: &str = "smear::schema::duplicate-argument-name";
const DUPLICATE_ENUM_VALUE: &str = "smear::schema::duplicate-enum-value";
const DUPLICATE_INPUT_FIELD_NAME: &str = "smear::schema::duplicate-input-field-name";
const DUPLICATE_DIRECTIVE_ARGUMENT_NAME: &str = "smear::schema::duplicate-directive-argument-name";

/// GraphQL's codes for a definition that holds members and holds none, by its kind.
const EMPTY_FIELDS_DEFINITION: &str = "smear::schema::empty-fields-definition";
const EMPTY_UNION_MEMBERS: &str = "smear::schema::empty-union-members";
const EMPTY_ENUM_VALUES: &str = "smear::schema::empty-enum-values";
const EMPTY_INPUT_FIELDS: &str = "smear::schema::empty-input-fields";

/// GraphQL's codes for a member typed by a definition of a kind its position does not take.
const FIELD_TYPE_NOT_OUTPUT_TYPE: &str = "smear::schema::field-type-not-output-type";
const ARGUMENT_TYPE_NOT_INPUT_TYPE: &str = "smear::schema::argument-type-not-input-type";
const INPUT_FIELD_TYPE_NOT_INPUT_TYPE: &str = "smear::schema::input-field-type-not-input-type";
const DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE: &str =
  "smear::schema::directive-argument-type-not-input-type";

/// GraphQL's code for an `implements` entry that names what is no interface.
const IMPLEMENTS_NON_INTERFACE: &str = "smear::schema::implements-non-interface";

/// GraphQL's code for an interface that implements itself.
const SELF_IMPLEMENTING_INTERFACE: &str = "smear::schema::self-implementing-interface";

/// GraphQL's code for a union member that is no object type.
const UNION_MEMBER_NOT_OBJECT: &str = "smear::schema::union-member-not-object";

/// GraphQL's codes for a required argument and a required input field that are deprecated.
const DEPRECATED_REQUIRED_ARGUMENT: &str = "smear::schema::deprecated-required-argument";
const DEPRECATED_REQUIRED_INPUT_FIELD: &str = "smear::schema::deprecated-required-input-field";

/// GraphQL's codes for the three mistakes of a OneOf input object.
const ONE_OF_FIELD_NOT_NULLABLE: &str = "smear::schema::one-of-field-not-nullable";
const ONE_OF_FIELD_HAS_DEFAULT: &str = "smear::schema::one-of-field-has-default";
const ONE_OF_ON_INPUT_OBJECT_EXTENSION: &str = "smear::schema::one-of-on-input-object-extension";

/// GraphQL's code for a directive applied where its definition does not list.
const UNSUPPORTED_DIRECTIVE_LOCATION: &str = "smear::schema::unsupported-directive-location";

/// GraphQL's code for a directive that is not repeatable applied twice to one location.
const DUPLICATE_DIRECTIVE_USE: &str = "smear::schema::duplicate-directive-use";

/// GraphQL's code for an application that passes an argument twice.
const DUPLICATE_DIRECTIVE_ARGUMENT_USE: &str = "smear::schema::duplicate-directive-argument-use";

/// GraphQL's code for a directive that refers to itself.
const SELF_REFERENTIAL_DIRECTIVE: &str = "smear::schema::self-referential-directive";

/// GraphQL's code for `@specifiedBy` on a built-in scalar.
const SPECIFIED_BY_ON_BUILT_IN_SCALAR: &str = "smear::schema::specified-by-on-built-in-scalar";

/// GraphQL's code for a root operation type that is no object type.
const ROOT_OPERATION_TYPE_NOT_OBJECT: &str = "smear::schema::root-operation-type-not-object";

/// GraphQL's code for two root operations of one type.
const SHARED_ROOT_OPERATION_TYPE: &str = "smear::schema::shared-root-operation-type";

/// GraphQL's code for a schema extension with no schema to extend.
const SCHEMA_EXTENSION_WITHOUT_SCHEMA: &str = "smear::schema::schema-extension-without-schema";

/// The code of a type name that names nothing, a phase's.
const UNDEFINED_TYPE: &str = "smear::schema::undefined-type";

/// The code of a root operation that names nothing, a phase's.
const UNDEFINED_ROOT: &str = "smear::schema::undefined-root-operation-type";

/// The code for `@extern` misused.
const EXTERN_MISUSE: &str = "smear::graphqlx::extern-misuse";

/// The code for `@repr` misused.
const REPR_MISUSE: &str = "smear::graphqlx::repr-misuse";

/// The code for a use above the tier its package declares.
const TIER: &str = "smear::graphqlx::tier";

/// What the orphan rule tells the author of an extension of another supplied package's definition.
const THIRD_PARTY: &str = "a package extends its own definitions alone, so that a definition is \
                           the same in every program that holds it: define a type or a directive \
                           of this package's own, or make the change in the package that defines \
                           it";

/// What the orphan rule tells a dependency that adds directives alone to a built-in, which the root
/// package may add.
const NOT_THE_ROOT: &str = "only the root package extends a definition of `core`, `alloc` or \
                            `std`, and only to add directives: remove this extension, or write it \
                            in the root package";

/// What the orphan rule tells a dependency that adds more than directives to a built-in, which the
/// root package may not add either.
const NOT_EVEN_THE_ROOT: &str = "only the root package extends a definition of `core`, `alloc` \
                                 or `std`, and only to add directives: remove this extension";

/// What the orphan rule tells the root package when it adds more than directives to a built-in.
const MORE_THAN_DIRECTIVES: &str = "an extension of a definition of `core`, `alloc` or `std` adds \
                                    directives and nothing else: remove what this one adds beside \
                                    them";

/// What GraphQL's own refusal tells the root package when it adds more than directives to an
/// introspection type: `smear-schema`'s help, word for word.
const INTROSPECTION_HELP: &str = "an extension may add directives to an introspection type and \
                                  nothing else; the `__`-prefixed types are the introspection \
                                  system the specification defines.";

/// The primary label of GraphQL's own refusal of an extension that adds more than directives to an
/// introspection type: `smear-schema`'s phrase, word for word.
const INTROSPECTION_LABEL: &str = "introspection type extension adds more than directives";

/// What the orphan rule tells the root package when its extension of a built-in applies `@marker`,
/// `@set` or `@map`.
const MEANING: &str = "`@marker`, `@set` and `@map` say what a definition is, and what a built-in \
                       is is the language's to say: remove this directive";

/// What the orphan rule tells the root package when its extension of a built-in applies `@extern`.
const HOST: &str = "`@extern` binds a scalar its own package defines: give a built-in its host \
                    type with `extend schema @repr(for: …, lang: …, type: …)`";

/// What GraphQL tells the author of a directive on a cycle: `smear-schema`'s help, word for word.
const SELF_REFERENTIAL_HELP: &str = "a directive may not appear on its own definition or an \
                                     extension of it, nor on any type or directive they name.";

/// What GraphQL tells the author of a directive that is not repeatable applied twice to one
/// location: `smear-schema`'s help, word for word.
const DUPLICATE_DIRECTIVE_USE_HELP: &str = "declare the directive `repeatable`, or apply it once; \
                                            a definition and its extensions are one location.";

/// What GraphQL tells the author of a directive applied where its definition does not list:
/// `smear-schema`'s help, word for word.
const LOCATION_HELP: &str = "add the location to the directive's definition, or move the usage.";

/// What the orphan rule tells the root package when it adds more than directives to a built-in its
/// own definition replaces, which may write what the extension adds.
const MORE_THAN_DIRECTIVES_TO_ITS_OWN: &str = "an extension of a definition of `core`, `alloc` \
                                               or `std` adds directives and nothing else, and \
                                               this package's own definition replaces the \
                                               built-in: write what this one adds in the \
                                               definition itself";

/// A package named `name` of `layout` at tier `Std`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  layout: Layout,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, layout, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_one(text: &'static str) -> Analysis {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  analyze(&root, &[], &Limits::new())
}

/// The id of the file at `path` in the package `package`.
fn file(analysis: &Analysis, package: u32, path: &str) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == path)
    .unwrap_or_else(|| panic!("no file `{path}` in package {package}"))
}

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  file(analysis, 3, "schema.graphqlx")
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// GraphQL's code for a schema with no query root. Most programs of this file are parts of a
/// program and say nothing of the schema, so each reading below but `unwaived` leaves this code
/// out, as a consumer that analyzes a library waives it; the rule's own tests read the whole list.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// The diagnostics of `analysis`, in the list's order, but for the missing query root.
fn reported(analysis: &Analysis) -> Vec<&Diagnostic> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .collect()
}

/// Whether `analysis` reports nothing but that its program has no query root.
fn clean(analysis: &Analysis) -> bool {
  let diagnostics = analysis.diagnostics();
  let waived = diagnostics.len() - reported(analysis).len();
  diagnostics.len() == waived && diagnostics.error_count() == waived
}

/// The analysis's error count, but for the missing query root.
fn errors(analysis: &Analysis) -> usize {
  let diagnostics = analysis.diagnostics();
  diagnostics.error_count() - (diagnostics.len() - reported(analysis).len())
}

/// Each diagnostic's code, primary location, and first label's location, in the list's order, but
/// for the missing query root.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// Each diagnostic's code, primary location, and first label's location, in the list's order: the
/// whole list, the missing query root among them.
fn unwaived(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// Where the `nth`, counting from 0, of the names `name` written right after `before` is in `text`,
/// the file `source`: a name, so one followed by no letter, digit or `_`.
fn named(source: SourceId, text: &str, before: &str, name: &str, nth: usize) -> Location {
  let written = format!("{before}{name}");
  let start = text
    .match_indices(written.as_str())
    .map(|(start, _): (usize, &str)| start + before.len())
    .filter(|start: &usize| {
      text[start + name.len()..]
        .chars()
        .next()
        .is_none_or(|next: char| !(next.is_ascii_alphanumeric() || next == '_'))
    })
    .nth(nth)
    .unwrap_or_else(|| panic!("`{written}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// Each diagnostic's code, in the list's order, but for the missing query root.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect::<Vec<&'static str>>()
}

/// Each diagnostic's message, in the list's order, but for the missing query root.
fn messages(analysis: &Analysis) -> Vec<String> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect::<Vec<String>>()
}

/// Each diagnostic's help, in the list's order, but for the missing query root.
fn helps(analysis: &Analysis) -> Vec<Option<&str>> {
  let mut helps: Vec<Option<&str>> = Vec::new();
  for diagnostic in reported(analysis) {
    helps.push(diagnostic.help());
  }
  helps
}

/// Whether each of the schema's contributions has errors, in order.
fn schema_errors(model: &Model) -> Vec<bool> {
  model
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| part.has_errors())
    .collect::<Vec<bool>>()
}

/// The type the canonical name `name` names.
fn def(model: &Model, name: &str) -> DefId {
  model
    .lookup(Namespace::Type, name)
    .unwrap_or_else(|| panic!("no type `{name}`"))
}

/// Whether each contribution of `def` is an extension, and whether it has errors, in order.
fn contributions(model: &Model, def: DefId) -> Vec<(bool, bool)> {
  model
    .def(def)
    .contributions()
    .map(|part: Contribution<'_>| (part.is_extension(), part.has_errors()))
    .collect::<Vec<(bool, bool)>>()
}

/// How many definitions the built-in library alone gives a model: its own, and the primitives it
/// writes.
fn library_definitions() -> u32 {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  analyze(&root, &[], &Limits::new()).model().defs().len()
}

#[test]
fn a_second_schema_definition_is_reported_at_the_later_with_a_label_at_the_first() {
  // In one file: each definition after the first is reported, each with a label at the first.
  let text = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n\
              schema {\n  query: Nowhere\n}\nschema {\n  query: Query\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let first = at(schema, text, "schema", 0);
  assert_eq!(
    found(&analysis),
    [
      (DUPLICATE, at(schema, text, "schema", 1), Some(first)),
      (DUPLICATE, at(schema, text, "schema", 2), Some(first)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  // GraphQL's own phrase, label and help for its own rule.
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.to_string(),
      "the root package defines the schema twice"
    );
    assert_eq!(
      diagnostic.primary_label(),
      Some("duplicate schema definition")
    );
    assert_eq!(diagnostic.labels(), 1);
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("first defined here")
    );
    assert_eq!(
      diagnostic.help(),
      Some("a document holds at most one `schema` block; put the extra roots in `extend schema`.")
    );
  }
  // The first definition is the schema's, and the type a later one names is not looked for.
  assert!(matches!(
    analysis.model().schema().root(RootOperation::Query),
    Some(Root::Written(_))
  ));

  // In two files, whichever is passed first: the later by source is the one reported.
  let a = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let b = "schema {\n  query: Query\n}\n";
  for files in [
    [("a.graphqlx", a), ("b.graphqlx", b)],
    [("b.graphqlx", b), ("a.graphqlx", a)],
  ] {
    let root = package("app", Layout::Flat, &files);
    let analysis = analyze(&root, &[], &Limits::new());
    let a_file = file(&analysis, 3, "a.graphqlx");
    let b_file = file(&analysis, 3, "b.graphqlx");
    assert_eq!(
      found(&analysis),
      [(
        DUPLICATE,
        at(b_file, b, "schema", 0),
        Some(at(a_file, a, "schema", 0))
      )],
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn extensions_beside_one_definition_are_no_duplicate() {
  // One definition, with an extension written before it and one after it.
  let text = "extend schema {\n  mutation: Change\n}\nschema {\n  query: Query\n}\n\
              extend schema {\n  subscription: Tick\n}\ntype Query {\n  ok: Int\n}\n\
              type Change {\n  done: Int\n}\ntype Tick {\n  at: Int\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let extensions: Vec<bool> = analysis
    .model()
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| part.is_extension())
    .collect::<Vec<bool>>();
  assert_eq!(extensions, [false, true, true]);
}

#[test]
fn no_schema_definition_is_no_diagnostic() {
  // No schema item at all.
  let analysis = analyze_one("type Query {\n  ok: Int\n}\n");
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(analysis.model().schema().contributions().count(), 0);

  // A schema extension beside a presumed `Query` extends a schema that is defined: omitting the
  // schema definition beside an object type `Query` defines one. The extension gives the schema
  // its root, beside the one presumed.
  let analysis = analyze_one(
    "type Query {\n  ok: Int\n}\ntype Change {\n  done: Int\n}\n\
     extend schema {\n  mutation: Change\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let schema = analysis.model().schema();
  assert!(matches!(
    schema.root(RootOperation::Query),
    Some(Root::Presumed(_))
  ));
  assert!(matches!(
    schema.root(RootOperation::Mutation),
    Some(Root::Written(_))
  ));
}

#[test]
fn a_dependency_that_defines_the_schema_is_reported_once_and_not_as_a_duplicate() {
  let ours = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let theirs = "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  // Beside the root package's own definition it is no second definition of the schema: it is
  // reported once, as the dependency's to remove, with no label.
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(!analysis.is_ok());
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.code(), DiagnosticKind::SchemaOutsideRoot.code());
  assert_eq!(
    diagnostic.to_string(),
    "package `geo` defines the schema, which only the root package defines and extends"
  );
  assert_eq!(
    diagnostic.primary_label(),
    Some("schema outside the root package")
  );
  assert_eq!(diagnostic.labels(), 0);
  assert_eq!(
    diagnostic.help(),
    Some(
      "a dependency is a library, and the schema is the root package's: remove this item, or \
       write it in the root package"
    )
  );

  // With no schema definition of the root package's, two dependencies' definitions are each
  // reported once, and neither as a duplicate of the other.
  let stars = "schema {\n  query: Star\n}\ntype Star {\n  name: String\n}\n";
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["astro", "geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let astro = package("astro", Layout::Flat, &[("astro.graphqlx", stars)]);
  let analysis = analyze(&root, &[geo, astro], &Limits::new());
  let astro_file = file(&analysis, 4, "astro.graphqlx");
  let geo_file = file(&analysis, 5, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (OUTSIDE, at(astro_file, stars, "schema", 0), None),
      (OUTSIDE, at(geo_file, theirs, "schema", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_dependency_that_extends_the_schema_is_reported() {
  let ours = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let theirs = "type Point {\n  x: Int\n}\nextend schema {\n  mutation: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "extend", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    ["package `geo` extends the schema, which only the root package defines and extends"]
  );
  // It gives the schema nothing: the root it writes is not the schema's.
  assert_eq!(
    analysis.model().schema().root(RootOperation::Mutation),
    None
  );
}

#[test]
fn the_root_package_defines_and_extends_the_schema_from_any_of_its_modules() {
  // The definition in one directory module, an extension in another, and one in an inline `mod`.
  let root = package(
    "app",
    Layout::Directory,
    &[
      (
        "types.graphqlx",
        "type Query {\n  ok: Int\n}\ntype Change {\n  done: Int\n}\ntype Tick {\n  at: Int\n}\n",
      ),
      ("defined/s.graphqlx", "schema {\n  query: ~::Query\n}\n"),
      (
        "extended/e.graphqlx",
        "extend schema {\n  mutation: ~::Change\n}\n\
         mod inner {\n  extend schema {\n    subscription: ~::Tick\n  }\n}\n",
      ),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let schema = analysis.model().schema();
  for operation in [
    RootOperation::Query,
    RootOperation::Mutation,
    RootOperation::Subscription,
  ] {
    assert!(
      matches!(schema.root(operation), Some(Root::Written(_))),
      "{operation:?}"
    );
  }
}

/// What GraphQL tells the author of a root operation type that is no object type.
const ROOT_TYPE_HELP: &str = "a root operation type must be an object type: an interface or union \
                              has no concrete field set to resolve.";

#[test]
fn a_root_operation_type_is_an_object_type() {
  // Each of the three roots, written as an interface: each at its type.
  let text = "interface Q {\n  q: Int\n}\ninterface M {\n  m: Int\n}\ninterface S {\n  s: Int\n}\n\
              schema {\n  query: Q\n  mutation: M\n  subscription: S\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        ROOT_OPERATION_TYPE_NOT_OBJECT,
        named(schema, text, "query: ", "Q", 0),
        None
      ),
      (
        ROOT_OPERATION_TYPE_NOT_OBJECT,
        named(schema, text, "mutation: ", "M", 0),
        None
      ),
      (
        ROOT_OPERATION_TYPE_NOT_OBJECT,
        named(schema, text, "subscription: ", "S", 0),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let not_object = |operation: &str, name: &str, what: &str| -> String {
    format!(
      "the {operation} root operation type `{name}` is {what}, and a root operation type is an \
       object type"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      not_object("query", "Q", "an interface"),
      not_object("mutation", "M", "an interface"),
      not_object("subscription", "S", "an interface"),
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.primary_label(),
      Some("root operation type is not an object type")
    );
    assert_eq!(diagnostic.help(), Some(ROOT_TYPE_HELP));
  }

  // The query root as each other kind that is no object type, `core`'s `NonZero`, which an alias
  // names, among them: each once, at the type, named by its definition.
  for (text, written, name, what) in [
    (
      "type O {\n  o: Int\n}\nunion U = O\nschema {\n  query: U\n}\n",
      "U",
      "U",
      "a union",
    ),
    ("scalar S\nschema {\n  query: S\n}\n", "S", "S", "a scalar"),
    (
      "enum E {\n  A\n}\nschema {\n  query: E\n}\n",
      "E",
      "E",
      "an enum",
    ),
    (
      "input In {\n  i: Int\n}\nschema {\n  query: In\n}\n",
      "In",
      "In",
      "an input object",
    ),
    (
      "schema {\n  query: NonZeroU32\n}\n",
      "NonZeroU32",
      "NonZero",
      "a scalar",
    ),
  ] {
    let analysis = analyze_one(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(
        ROOT_OPERATION_TYPE_NOT_OBJECT,
        named(schema, text, "query: ", written, 0),
        None
      )],
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert_eq!(messages(&analysis), [not_object("query", name, what)]);
  }

  // A marker is an interface too, which the marker rule refuses at the same token.
  let analysis = analyze_one("interface M @marker\nschema {\n  query: M\n}\n");
  assert_eq!(
    codes(&analysis),
    [MARKER_MISUSE, ROOT_OPERATION_TYPE_NOT_OBJECT]
  );
  assert_eq!(messages(&analysis)[1], not_object("query", "M", "a marker"));

  // An instantiation of a generic object type is an object type.
  let analysis = analyze_one("type Root<T> {\n  r: T\n}\nschema {\n  query: Root<Int>\n}\n");
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn the_root_operations_have_distinct_types() {
  // Two of one type, at the later, labelled at the first; and all three, each later one labelled
  // at the query's.
  let text = "type Q {\n  q: Int\n}\nschema {\n  query: Q\n  mutation: Q\n  subscription: Q\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let query = Some(named(schema, text, "query: ", "Q", 0));
  assert_eq!(
    found(&analysis),
    [
      (
        SHARED_ROOT_OPERATION_TYPE,
        named(schema, text, "mutation: ", "Q", 0),
        query
      ),
      (
        SHARED_ROOT_OPERATION_TYPE,
        named(schema, text, "subscription: ", "Q", 0),
        query
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the query and mutation root operations have one type, `Q`",
      "the query and subscription root operations have one type, `Q`",
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.primary_label(),
      Some("two root operations name the same type")
    );
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("already a root operation type here")
    );
    assert_eq!(
      diagnostic.help(),
      Some(
        "the three roots are the entry points of three different operation kinds, so each needs \
         its own object type."
      )
    );
  }

  // One instantiation twice is one type; two instantiations of one definition are two.
  let text = "type Root<T> {\n  r: T\n}\nschema {\n  query: Root<Int>\n  mutation: Root<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      SHARED_ROOT_OPERATION_TYPE,
      at(schema, text, "Root<Int>", 1),
      Some(at(schema, text, "Root<Int>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let analysis = analyze_one(
    "type Root<T> {\n  r: T\n}\nschema {\n  query: Root<Int>\n  mutation: Root<String>\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // A presumed root and an extension's written root of its definition: at the written one,
  // labelled at the definition.
  let text = "type Query {\n  ok: Int\n}\nextend schema {\n  mutation: Query\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      SHARED_ROOT_OPERATION_TYPE,
      named(schema, text, "mutation: ", "Query", 0),
      Some(named(schema, text, "type ", "Query", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  // And when the presumed one is the later: at the written one still, labelled at the presumed
  // one's definition. With no `Query` presumed, the extension that writes the query root has no
  // schema to extend, which is another rule's to say.
  let text = "type Mutation {\n  m: Int\n}\nextend schema {\n  query: Mutation\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        SCHEMA_EXTENSION_WITHOUT_SCHEMA,
        at(schema, text, "extend", 0),
        None
      ),
      (
        SHARED_ROOT_OPERATION_TYPE,
        named(schema, text, "query: ", "Mutation", 0),
        Some(named(schema, text, "type ", "Mutation", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // Two roots that are one interface are each no object type, and that is all.
  let analysis =
    analyze_one("interface I {\n  i: Int\n}\nschema {\n  query: I\n  mutation: I\n}\n");
  assert_eq!(
    codes(&analysis),
    [
      ROOT_OPERATION_TYPE_NOT_OBJECT,
      ROOT_OPERATION_TYPE_NOT_OBJECT
    ]
  );
}

#[test]
fn a_schema_extension_extends_a_schema_that_is_defined() {
  // `smear-schema`'s fixture: no schema definition, and no `Query` presumed, so the extension
  // extends nothing, although it gives the schema its query root.
  let text = "type Q {\n  ok: Int\n}\nextend schema {\n  query: Q\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      SCHEMA_EXTENSION_WITHOUT_SCHEMA,
      at(schema, text, "extend", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the root package extends the schema, and has none to extend: it writes no schema \
       definition, and no object type `Query` is presumed its query root"
    ]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    diagnostic.primary_label(),
    Some("schema extension has no schema to extend")
  );
  assert_eq!(
    diagnostic.help(),
    Some(
      "`extend schema` adds to a schema the document defines: write a `schema` definition, or \
       define the `Query` type that omitting one presumes."
    )
  );
  assert!(matches!(
    analysis.model().schema().root(RootOperation::Query),
    Some(Root::Written(_))
  ));

  // Two such extensions are two.
  let text = "type Q {\n  ok: Int\n}\ntype M {\n  m: Int\n}\nextend schema {\n  query: Q\n}\n\
              extend schema {\n  mutation: M\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        SCHEMA_EXTENSION_WITHOUT_SCHEMA,
        at(schema, text, "extend", 0),
        None
      ),
      (
        SCHEMA_EXTENSION_WITHOUT_SCHEMA,
        at(schema, text, "extend", 1),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // Beside a schema definition the file writes after it, and beside a presumed `Query`, the
  // schema is defined.
  for text in [
    "extend schema {\n  mutation: M\n}\nschema {\n  query: Q\n}\ntype Q {\n  ok: Int\n}\n\
     type M {\n  m: Int\n}\n",
    "type Query {\n  ok: Int\n}\ntype M {\n  m: Int\n}\nextend schema {\n  mutation: M\n}\n",
  ] {
    let analysis = analyze_one(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }

  // An interface named `Query` is not presumed, so it defines no schema.
  let text = "interface Query {\n  ok: Int\n}\ntype Q {\n  q: Int\n}\n\
              extend schema {\n  query: Q\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      SCHEMA_EXTENSION_WITHOUT_SCHEMA,
      at(schema, text, "extend", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // A dependency's schema extension is the rule on schema items outside the root package's
  // alone, beside the root package's own, which this rule refuses.
  let ours = "type Q {\n  ok: Int\n}\nextend schema {\n  query: Q\n}\n";
  let theirs = "type Point {\n  x: Int\n}\nextend schema {\n  mutation: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        SCHEMA_EXTENSION_WITHOUT_SCHEMA,
        at(schema, ours, "extend", 0),
        None
      ),
      (OUTSIDE, at(geo_file, theirs, "extend", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_rule_of_this_part_says_nothing_where_a_phase_spoke() {
  // A rule on the roots, which no contribution holds, says nothing beside a second root that names
  // nothing, in the same schema definition or in a schema extension; beside an undefined type in a
  // definition, which is none of the schema's contributions, it speaks.
  for (text, expected) in [
    (
      "interface I {\n  i: Int\n}\nschema {\n  query: I\n  mutation: Missing\n}\n",
      &[UNDEFINED_ROOT][..],
    ),
    (
      "interface I {\n  i: Int\n}\nschema {\n  query: I\n}\nextend schema {\n  mutation: Missing\n}\n",
      &[UNDEFINED_ROOT][..],
    ),
    (
      "interface I {\n  i: Int\n}\nschema {\n  query: I\n}\ntype G {\n  x: Missing\n}\n",
      &[ROOT_OPERATION_TYPE_NOT_OBJECT, UNDEFINED_TYPE][..],
    ),
    (
      "type Q {\n  q: Int\n}\nschema {\n  query: Q\n  mutation: Q\n  subscription: Missing\n}\n",
      &[UNDEFINED_ROOT][..],
    ),
    (
      "type Q {\n  q: Int\n}\nschema {\n  query: Q\n  mutation: Q\n}\n\
       extend schema {\n  subscription: Missing\n}\n",
      &[UNDEFINED_ROOT][..],
    ),
    (
      "type Q {\n  q: Int\n}\nschema {\n  query: Q\n  mutation: Q\n}\ntype G {\n  x: Missing\n}\n",
      &[SHARED_ROOT_OPERATION_TYPE, UNDEFINED_TYPE][..],
    ),
    // A schema extension that holds the phase's error says nothing; one beside it, and one beside
    // an undefined type in a definition, speak.
    (
      "type Q {\n  q: Int\n}\nextend schema {\n  query: Q\n  mutation: Missing\n}\n",
      &[UNDEFINED_ROOT][..],
    ),
    (
      "type Q {\n  q: Int\n}\nextend schema {\n  query: Q\n}\nextend schema {\n  mutation: Missing\n}\n",
      &[SCHEMA_EXTENSION_WITHOUT_SCHEMA, UNDEFINED_ROOT][..],
    ),
    (
      "type Q {\n  q: Int\n}\nextend schema {\n  query: Q\n}\ntype G {\n  x: Missing\n}\n",
      &[SCHEMA_EXTENSION_WITHOUT_SCHEMA, UNDEFINED_TYPE][..],
    ),
  ] {
    let analysis = analyze_one(text);
    assert_eq!(codes(&analysis), expected, "{text}");
  }
}

#[test]
fn long_names_in_these_rules_are_quoted_as_excerpts() {
  let name = "R".repeat(4096);
  let texts = [
    format!("interface {name} {{\n  i: Int\n}}\nschema {{\n  query: {name}\n}}\n"),
    format!("type {name} {{\n  q: Int\n}}\nschema {{\n  query: {name}\n  mutation: {name}\n}}\n"),
  ];
  for text in &texts {
    let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
    let analysis = analyze(&root, &[], &Limits::new());
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}

/// What GraphQL tells the author of a schema with no query root: `smear-schema`'s help, word for
/// word.
const NO_QUERY_ROOT_HELP: &str =
  "define `type Query`, or name an existing object type with `schema { query: … }`.";

/// The whole of the root package's package source, the first of its sources.
fn root_package_source(analysis: &Analysis) -> Location {
  let model = analysis.model();
  let id = model
    .package(model.root_package())
    .sources()
    .into_iter()
    .next()
    .expect("every package has a package source");
  Location::entire(id.get())
}

#[test]
fn a_schema_has_a_query_root() {
  // `smear-schema`'s fixture: a type that is no query root, and nothing else. The schema is the
  // root package's as a whole, so the diagnostic is at the whole of its package source, with no
  // label, and not in its file, where it would lie within the type.
  let analysis = analyze_one("type NotTheRoot {\n  ok: Int\n}");
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, root_package_source(&analysis), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "the schema has no query root operation type"
  );
  assert_eq!(
    diagnostic.primary_label(),
    Some("no query root operation type")
  );
  assert_eq!(diagnostic.labels(), 0);
  assert_eq!(diagnostic.help(), Some(NO_QUERY_ROOT_HELP));
  assert!(!analysis.is_ok());
  assert_eq!(analysis.model().schema().root(RootOperation::Query), None);

  // A root package with no file has none either.
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, root_package_source(&analysis), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // A presumed `Query`, with no schema definition, and a written root with no `Query` at all.
  for text in [
    "type Query {\n  ok: Int\n}",
    "schema {\n  query: Q\n}\ntype Q {\n  ok: Int\n}\n",
  ] {
    let analysis = analyze_one(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn a_schema_definition_with_no_query_root_is_reported_at_its_keyword() {
  // A schema definition that writes no query root: at its `schema`, with no label.
  let text = "schema {\n  mutation: M\n}\ntype M {\n  m: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, at(schema, text, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // Beside an object named `Query`, which a written schema definition does not let be presumed:
  // still one, at the definition.
  let text = "schema {\n  mutation: M\n}\ntype M {\n  m: Int\n}\ntype Query {\n  ok: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, at(schema, text, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(analysis.model().schema().root(RootOperation::Query), None);

  // A second schema definition that writes the query root gives the schema nothing: still one, at
  // the first, beside the second's own refusal.
  let text = "schema {\n  mutation: M\n}\nschema {\n  query: Q\n}\ntype M {\n  m: Int\n}\n\
              type Q {\n  q: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let first = at(schema, text, "schema", 0);
  assert_eq!(
    unwaived(&analysis),
    [
      (NO_QUERY_ROOT, first, None),
      (DUPLICATE, at(schema, text, "schema", 1), Some(first)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn only_an_object_the_root_module_declares_is_presumed() {
  // An interface named `Query` is no object type.
  let analysis = analyze_one("interface Query {\n  ok: Int\n}\n");
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, root_package_source(&analysis), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // In a `Directory` root, a module other than the root module declares it.
  let root = package(
    "app",
    Layout::Directory,
    &[("m/a.graphqlx", "type Query {\n  ok: Int\n}\n")],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, root_package_source(&analysis), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // The root package declares none, and its dependency declares one.
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Other {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["dep"]);
  let dep = package(
    "dep",
    Layout::Flat,
    &[("dep.graphqlx", "type Query {\n  ok: Int\n}\n")],
  );
  let analysis = analyze(&root, &[dep], &Limits::new());
  assert_eq!(
    unwaived(&analysis),
    [(NO_QUERY_ROOT, root_package_source(&analysis), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // Every file of a `Flat` root is its root module: a `Query` its second file declares is
  // presumed.
  let root = package(
    "app",
    Layout::Flat,
    &[
      ("a.graphqlx", "type Other {\n  ok: Int\n}\n"),
      ("b.graphqlx", "type Query {\n  ok: Int\n}\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(matches!(
    analysis.model().schema().root(RootOperation::Query),
    Some(Root::Presumed(_))
  ));
}

#[test]
fn a_written_root_is_a_root_whatever_it_names() {
  // A query root that names nothing is name resolution's alone.
  let text = "schema {\n  query: Missing\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(
      UNDEFINED_ROOT,
      named(schema, text, "query: ", "Missing", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // One that names an interface is the rule on the roots' kinds' alone.
  let text = "interface I {\n  i: Int\n}\nschema {\n  query: I\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(
      ROOT_OPERATION_TYPE_NOT_OBJECT,
      named(schema, text, "query: ", "I", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // With no schema definition and no `Query`, the query root an extension writes is a root: the
  // extension has no schema to extend, and that is all.
  let text = "type Q {\n  q: Int\n}\nextend schema {\n  query: Q\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(
      SCHEMA_EXTENSION_WITHOUT_SCHEMA,
      at(schema, text, "extend", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn the_missing_root_marks_nothing_when_no_schema_is_written() {
  // At the package source, which no extent holds: the verdict is no, and nothing is marked.
  let analysis = analyze_one("type NotTheRoot {\n  ok: Int\n}\n");
  assert!(!analysis.is_ok());
  let model = analysis.model();
  for id in model.defs() {
    let definition = model.def(id);
    assert!(
      !definition.has_errors(),
      "`{}` is marked",
      definition.name()
    );
    for part in definition.contributions() {
      assert!(
        !part.has_errors(),
        "a contribution of `{}` is marked",
        definition.name()
      );
    }
  }
  assert_eq!(model.schema().contributions().count(), 0);

  // At a schema definition's `schema`: its contribution holds the error, and no definition does.
  let analysis = analyze_one("schema {\n  mutation: M\n}\ntype M {\n  m: Int\n}\n");
  assert!(!analysis.is_ok());
  let model = analysis.model();
  assert_eq!(schema_errors(model), [true]);
  assert!(
    model
      .defs()
      .into_iter()
      .all(|def: DefId| !model.def(def).has_errors())
  );
}

#[test]
fn a_phase_in_the_schema_definition_silences_it() {
  // A mutation root that names nothing, in the schema definition that writes no query root: the
  // phase's diagnostic alone, where GraphQL says both.
  let text = "schema {\n  mutation: Missing\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [(
      UNDEFINED_ROOT,
      named(schema, text, "mutation: ", "Missing", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // With no schema definition, the rule's diagnostic lies in no extent, and a phase's error in a
  // definition silences nothing: both are said.
  let text = "type T {\n  x: Missing\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    unwaived(&analysis),
    [
      (NO_QUERY_ROOT, root_package_source(&analysis), None),
      (UNDEFINED_TYPE, at(schema, text, "Missing", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A library: an interface, and an object type that implements it.
const LIBRARY: &str = "interface Shape {\n  area: Float\n}\n\
                       type Circle implements Shape {\n  area: Float\n  radius: Float\n}\n";

/// Every definition of `model` as its view prints it and whether it has errors, each followed by
/// its contributions and its fields, each field with its type occurrence and whether it has errors.
fn answers(model: &Model) -> Vec<String> {
  let mut lines: Vec<String> = Vec::new();
  for id in model.defs() {
    let definition = model.def(id);
    lines.push(format!("{definition:?} {}", definition.has_errors()));
    for part in definition.contributions() {
      lines.push(format!("  {part:?}"));
    }
    for field in definition.fields() {
      let held = model.member(field);
      lines.push(format!("  {held:?} {:?} {}", held.ty(), held.has_errors()));
    }
  }
  lines
}

#[test]
fn a_consumer_waives_the_missing_root_by_its_code() {
  // A library analyzed as the root package is not ok, and that it has no query root is all it is
  // told: a consumer that filters the code reads no other diagnostic.
  let analysis = analyze_one(LIBRARY);
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), 1);
  let kept: Vec<&Diagnostic> = analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .collect();
  assert!(kept.is_empty(), "{kept:#?}");

  // And its model is whole: beside a query root written after it, which adds no definition, every
  // definition, contribution and field answers as it does here.
  let text = format!("{LIBRARY}schema {{\n  query: Circle\n}}\n");
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
  let rooted = analyze(&root, &[], &Limits::new());
  assert!(
    rooted.diagnostics().is_empty(),
    "{:#?}",
    rooted.diagnostics()
  );
  assert_eq!(answers(analysis.model()), answers(rooted.model()));
}

#[test]
fn the_root_extending_a_dependencys_type_is_an_orphan() {
  // A dependency's type, and a dependency's directive.
  let ours = "extend type ^::geo::Point {\n  label: String\n}\n\
              extend directive @^::geo::tag @deprecated\n";
  let theirs = "type Point {\n  x: Int\n}\ndirective @tag on OBJECT\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  // Each at the extension's target path, with a label at the definition's name.
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, ours, "^::geo::Point", 0),
        Some(at(geo_file, theirs, "Point", 0))
      ),
      (
        ORPHAN,
        at(schema, ours, "^::geo::tag", 0),
        Some(at(geo_file, theirs, "tag", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each is an error: the analysis is not ok by them, whatever the missing root adds.
  assert_eq!(errors(&analysis), 2);
  assert_eq!(
    messages(&analysis),
    [
      "package `app` extends the type `Point`, which package `geo` defines",
      "package `app` extends the directive `@tag`, which package `geo` defines",
    ]
  );
  for diagnostic in reported(&analysis) {
    assert_eq!(diagnostic.code(), DiagnosticKind::OrphanExtension.code());
    assert_eq!(diagnostic.primary_label(), Some("orphan extension"));
    assert_eq!(diagnostic.labels(), 1);
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("the definition it extends")
    );
    assert_eq!(diagnostic.help(), Some(THIRD_PARTY));
  }
  // The extension is the definition's contribution all the same: the model holds what is written.
  let point = def(analysis.model(), "^::geo::Point");
  assert_eq!(
    contributions(analysis.model(), point),
    [(false, false), (true, true)]
  );
}

#[test]
fn a_dependency_extending_another_packages_type_is_an_orphan() {
  // Another dependency's type, and the root package's, which a dependency reaches when it
  // declares the root package.
  let ours = "type Query {\n  ok: Int\n}\n";
  let stars = "type Star {\n  name: String\n}\n";
  let theirs = "extend type ^::astro::Star {\n  near: Int\n}\n\
                extend type ^::app::Query {\n  fromGeo: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]);
  let astro = package("astro", Layout::Flat, &[("astro.graphqlx", stars)]);
  let geo =
    package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]).with_dependencies(["astro", "app"]);
  let analysis = analyze(&root, &[geo, astro], &Limits::new());
  let schema = schema_file(&analysis);
  let astro_file = file(&analysis, 4, "astro.graphqlx");
  let geo_file = file(&analysis, 5, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(geo_file, theirs, "^::astro::Star", 0),
        Some(at(astro_file, stars, "Star", 0))
      ),
      (
        ORPHAN,
        at(geo_file, theirs, "^::app::Query", 0),
        Some(at(schema, ours, "Query", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "package `geo` extends the type `Star`, which package `astro` defines",
      "package `geo` extends the type `Query`, which package `app` defines",
    ]
  );
}

#[test]
fn a_package_extends_its_own_types_from_any_module() {
  // The root package and a dependency each extend a type of their own: from the module that
  // defines it, from an inline `mod` inside that one, and from a directory module.
  let root = package(
    "app",
    Layout::Directory,
    &[
      (
        "shape.graphqlx",
        "type Shape {\n  own: Int\n}\nextend type Shape {\n  beside: Int\n}\n\
         mod inner {\n  extend type ..::Shape {\n    inMod: Int\n  }\n}\n",
      ),
      ("m/x.graphqlx", "extend type ~::Shape {\n  fromM: Int\n}\n"),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Directory,
    &[
      ("lib.graphqlx", "type Point {\n  x: Int\n}\n"),
      ("ext/e.graphqlx", "extend type ~::Point {\n  y: Int\n}\n"),
    ],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(
    contributions(model, def(model, "^::app::Shape")),
    [(false, false), (true, false), (true, false), (true, false)]
  );
  assert_eq!(
    contributions(model, def(model, "^::geo::Point")),
    [(false, false), (true, false)]
  );
}

#[test]
fn the_root_adds_directives_to_a_built_in() {
  // To `alloc`'s `string`, by its GraphQL name; to `i32`, a primitive `core` materialises; to
  // `f64`, which the root package's own `Float` replaces; to `core`'s `NonZero`; and to a
  // directive of `core`'s.
  let text = "directive @x on SCALAR | DIRECTIVE_DEFINITION\n\
              extend scalar String @x\nextend scalar Int @x\n\
              scalar Float\nextend scalar Float @x\n\
              extend scalar NonZero @x\nextend directive @deprecated @x\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  // Each is a contribution of a built-in definition, which the rule read and let be.
  let model = analysis.model();
  let builtins = model.builtins();
  let extended = [
    builtins.string(),
    builtins
      .primitive(Primitive::signed(32))
      .expect("`Int` writes `i32`"),
    builtins
      .primitive(Primitive::F64)
      .expect("the root package replaces `f64`"),
    builtins.non_zero(),
    builtins.deprecated(),
  ];
  for built_in in extended {
    assert_eq!(
      contributions(model, built_in),
      [(false, false), (true, false)],
      "{}",
      model.def(built_in).name()
    );
  }
}

#[test]
fn the_root_adding_more_than_directives_to_a_built_in_is_an_orphan() {
  // An enum value to `Tier`, a field to `__Type`, and an `implements` entry to a scalar: to
  // `alloc`'s `string`; to `i32`, which has no position for a label to point at; and to `f64`,
  // which the root package's own `Float` replaces. GraphQL refuses the field on `__Type` itself,
  // and that refusal keeps GraphQL's code, message, label and help, at the same place.
  let text = "interface Marked @marker\n\
              extend enum Tier {\n  EXTRA\n}\n\
              extend type __Type {\n  extra: Int\n}\n\
              extend scalar String implements Marked\n\
              extend scalar Int implements Marked\n\
              scalar Float\nextend scalar Float implements Marked\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let named = |name: &str| -> Option<Location> { Some(model.def(def(model, name)).location()) };
  assert_eq!(
    found(&analysis),
    [
      (ORPHAN, at(schema, text, "Tier", 0), named("^::core::Tier")),
      (
        INTROSPECTION,
        at(schema, text, "__Type", 0),
        named("^::core::__Type")
      ),
      (
        ORPHAN,
        at(schema, text, "String", 0),
        named("^::alloc::string")
      ),
      (ORPHAN, at(schema, text, "Int", 1), None),
      // `f64`'s definition is the root package's `scalar Float`, and its name is that `Float`.
      (
        ORPHAN,
        at(schema, text, "Float", 1),
        Some(at(schema, text, "Float", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the definition by its own name, `string` for `String`, `i32` for `Int` and `f64`
  // for `Float`, and the two packages.
  let more = |name: &str, built_in: &str| -> String {
    format!(
      "package `app` adds more than directives to the type `{name}` of the built-in package \
       `{built_in}`"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      more("Tier", "core"),
      "this extension adds more than directives to the introspection type `__Type`".to_owned(),
      more("string", "alloc"),
      more("i32", "core"),
      more("f64", "core"),
    ]
  );
  // Only the built-in the root package replaces has a definition of the root package's own to
  // write what the extension adds in.
  assert_eq!(
    helps(&analysis),
    [
      Some(MORE_THAN_DIRECTIVES),
      Some(INTROSPECTION_HELP),
      Some(MORE_THAN_DIRECTIVES),
      Some(MORE_THAN_DIRECTIVES),
      Some(MORE_THAN_DIRECTIVES_TO_ITS_OWN),
    ]
  );
  for diagnostic in reported(&analysis) {
    // GraphQL's own refusal has GraphQL's label.
    if diagnostic.code() == Code::new(INTROSPECTION) {
      assert_eq!(diagnostic.primary_label(), Some(INTROSPECTION_LABEL));
      continue;
    }
    assert_eq!(diagnostic.primary_label(), Some("orphan extension"));
  }
}

#[test]
fn the_roots_extension_of_a_meta_type_keeps_graphqls_code() {
  // A field on `__Type`, an enum value on `__TypeKind` and an `implements` entry on `__Schema`:
  // each is GraphQL's own refusal, at the extension's target, labelled at the meta-type's name.
  // `__Schema` has the one field `Face` declares, so the entry would be no other mistake.
  let text = "type Query {\n  ok: Int\n}\ninterface Face {\n  description: String\n}\n\
              extend type __Type {\n  extra: Int\n}\n\
              extend enum __TypeKind {\n  EXTRA\n}\n\
              extend type __Schema implements Face\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let named = |name: &str| -> Option<Location> { Some(model.def(def(model, name)).location()) };
  assert_eq!(
    found(&analysis),
    [
      (
        INTROSPECTION,
        at(schema, text, "__Type", 0),
        named("^::core::__Type")
      ),
      (
        INTROSPECTION,
        at(schema, text, "__TypeKind", 0),
        named("^::core::__TypeKind")
      ),
      (
        INTROSPECTION,
        at(schema, text, "__Schema", 0),
        named("^::core::__Schema")
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "this extension adds more than directives to the introspection type `__Type`",
      "this extension adds more than directives to the introspection type `__TypeKind`",
      "this extension adds more than directives to the introspection type `__Schema`",
    ]
  );
  assert_eq!(helps(&analysis), [Some(INTROSPECTION_HELP); 3]);
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.primary_label(), Some(INTROSPECTION_LABEL));
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("the definition it extends")
    );
  }

  // A directive alone is no refusal of either kind.
  let analysis =
    analyze_one("type Query {\n  ok: Int\n}\ndirective @x on OBJECT\nextend type __Type @x\n");
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // The same three written by a dependency are the orphan rule's, whatever they add.
  let ours = "type Query {\n  ok: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let theirs = "interface Face {\n  description: String\n}\n\
                extend type __Type {\n  extra: Int\n}\n\
                extend enum __TypeKind {\n  EXTRA\n}\n\
                extend type __Schema implements Face\n";
  let dep = package("dep", Layout::Flat, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  let model = analysis.model();
  let named = |name: &str| -> Option<Location> { Some(model.def(def(model, name)).location()) };
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(dep_file, theirs, "__Type", 0),
        named("^::core::__Type")
      ),
      (
        ORPHAN,
        at(dep_file, theirs, "__TypeKind", 0),
        named("^::core::__TypeKind")
      ),
      (
        ORPHAN,
        at(dep_file, theirs, "__Schema", 0),
        named("^::core::__Schema")
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // A built-in that is no meta-type is the orphan rule's, from the root package too.
  let text = "type Query {\n  ok: Int\n}\nextend enum Tier {\n  EXTRA\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(schema, text, "Tier", 0),
      Some(model.def(def(model, "^::core::Tier")).location())
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // A dependency's own type named as GraphQL names its introspection types is no meta-type: the
  // name is reserved where it is defined, and the root package's extension of it is an orphan.
  let ours = "type Query {\n  ok: Int\n}\nextend type ^::dep::__Mine {\n  b: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let theirs = "type __Mine {\n  a: Int\n}\n";
  let dep = package("dep", Layout::Flat, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = schema_file(&analysis);
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, ours, "^::dep::__Mine", 0),
        Some(at(dep_file, theirs, "__Mine", 0))
      ),
      (
        "smear::schema::reserved-type-name",
        at(dep_file, theirs, "__Mine", 0),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_dependency_extending_a_built_in_is_an_orphan() {
  // With a directive alone, which the root package may add and a dependency may not; and with an
  // enum value, which neither may add.
  let ours = "type Query {\n  ok: Int\n}\n";
  let theirs =
    "directive @once on SCALAR\nextend scalar String @once\nextend enum Tier {\n  X\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let model = analysis.model();
  let string = model.builtins().string();
  let tier = def(model, "^::core::Tier");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(geo_file, theirs, "String", 0),
        Some(model.def(string).location())
      ),
      (
        ORPHAN,
        at(geo_file, theirs, "Tier", 0),
        Some(model.def(tier).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "package `geo` extends the type `string` of the built-in package `alloc`, which only the \
       root package extends",
      "package `geo` extends the type `Tier` of the built-in package `core`, which only the root \
       package extends",
    ]
  );
  // The root package may write the first in the dependency's place, and not the second.
  assert_eq!(
    helps(&analysis),
    [Some(NOT_THE_ROOT), Some(NOT_EVEN_THE_ROOT)]
  );
}

/// What the orphan rule says of the root package's `@directive`, `core`'s `@marker`, `@set` or
/// `@map`, applied to `what`, a definition of the built-in package `built_in` written as the
/// message writes it: "type `NonZero`", "directive `@deprecated`".
fn meaning(directive: &str, what: &str, built_in: &str) -> String {
  format!(
    "package `app` applies `@{directive}` to the {what} of the built-in package `{built_in}`, and \
     what a built-in is cannot be changed by an extension"
  )
}

#[test]
fn the_root_giving_a_built_in_a_built_in_meaning_is_an_orphan() {
  // `@set` on `core`'s `NonZero`, `@marker` on `core`'s `MapKey` and `@set` on `std`'s `HashMap`:
  // each refused where the directive is written, with a label at the definition, and none
  // changes what the definition is. `core`'s own `MapKey` applies `@marker` already, and a
  // definition and its extensions are one location, so GraphQL's own rule refuses the second
  // `@marker` beside the orphan rule, labelled at `core`'s.
  let text =
    "extend scalar NonZero @set\nextend interface MapKey @marker\nextend scalar HashMap @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let builtins = model.builtins();
  let hash_map = def(model, "^::std::HashMap");
  let named = |id: DefId| -> Option<Location> { Some(model.def(id).location()) };
  // Where the first application a definition holds, its own definition's, names its directive.
  let first_applied = |id: DefId| -> Option<Location> {
    model
      .def(id)
      .applications()
      .first()
      .map(|application: &ApplicationId| model.application(*application).path_location())
  };
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, text, "set", 0),
        named(builtins.non_zero())
      ),
      (
        ORPHAN,
        at(schema, text, "marker", 0),
        named(builtins.map_key())
      ),
      (
        DUPLICATE_DIRECTIVE_USE,
        at(schema, text, "marker", 0),
        first_applied(builtins.map_key())
      ),
      (ORPHAN, at(schema, text, "set", 1), named(hash_map)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      meaning("set", "type `NonZero`", "core"),
      meaning("marker", "type `MapKey`", "core"),
      String::from(
        "the directive `@marker` is applied to the type `MapKey` again, and it is not repeatable"
      ),
      meaning("set", "type `HashMap`", "std"),
    ]
  );
  assert_eq!(
    helps(&analysis),
    [
      Some(MEANING),
      Some(MEANING),
      Some(DUPLICATE_DIRECTIVE_USE_HELP),
      Some(MEANING)
    ]
  );
  for diagnostic in reported(&analysis) {
    // GraphQL's own refusal has GraphQL's labels.
    if diagnostic.code() == Code::new(DUPLICATE_DIRECTIVE_USE) {
      assert_eq!(
        diagnostic.primary_label(),
        Some("directive is not repeatable")
      );
      assert_eq!(
        diagnostic.label(0).map(|label| label.text()),
        Some("first applied here")
      );
      continue;
    }
    assert_eq!(diagnostic.primary_label(), Some("orphan extension"));
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("the definition it extends")
    );
  }
  assert_eq!(model.def(builtins.non_zero()).collection(), None);
  assert!(model.def(builtins.map_key()).is_marker());
  assert_eq!(model.def(hash_map).collection(), Some(Collection::Map));
  // The error marks the extension that holds it, and the definition's own contribution not.
  assert_eq!(
    contributions(model, builtins.non_zero()),
    [(false, false), (true, true)]
  );

  // An extension that also adds an `implements` entry makes two mistakes, and is told both.
  let text = "interface Marked @marker\nextend scalar NonZero implements Marked @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let non_zero = Some(model.def(model.builtins().non_zero()).location());
  assert_eq!(
    found(&analysis),
    [
      (ORPHAN, at(schema, text, "NonZero", 0), non_zero),
      (ORPHAN, at(schema, text, "set", 0), non_zero),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      String::from(
        "package `app` adds more than directives to the type `NonZero` of the built-in package \
         `core`"
      ),
      meaning("set", "type `NonZero`", "core"),
    ]
  );
  assert_eq!(
    helps(&analysis),
    [Some(MORE_THAN_DIRECTIVES), Some(MEANING)]
  );

  // `@set` on a directive could give it no meaning, and the orphan rule refuses it all the same,
  // once: what a built-in is cannot be changed by an extension, whatever the extension applies.
  // `@set` stands on a scalar alone, so the rule on directive locations refuses it too, at the same
  // path.
  let text = "extend directive @deprecated @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, text, "set", 0),
        Some(model.def(model.builtins().deprecated()).location())
      ),
      (
        UNSUPPORTED_DIRECTIVE_LOCATION,
        at(schema, text, "set", 0),
        Some(model.def(model.builtins().set()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      meaning("set", "directive `@deprecated`", "core"),
      String::from(
        "the directive `@set` is applied to a directive definition, and its definition does not \
         list `DIRECTIVE_DEFINITION`"
      ),
    ]
  );
  assert_eq!(helps(&analysis), [Some(MEANING), Some(LOCATION_HELP)]);

  // Nor could `@marker` give a scalar one: the orphan rule refuses it once too, and the rule on
  // directive locations beside it, as `@marker` stands on an interface alone.
  let text = "extend scalar NonZero @marker\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, text, "marker", 0),
        Some(model.def(model.builtins().non_zero()).location())
      ),
      (
        UNSUPPORTED_DIRECTIVE_LOCATION,
        at(schema, text, "marker", 0),
        Some(model.def(model.builtins().marker()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      meaning("marker", "type `NonZero`", "core"),
      String::from(
        "the directive `@marker` is applied to a scalar, and its definition does not list `SCALAR`"
      ),
    ]
  );
  assert_eq!(helps(&analysis), [Some(MEANING), Some(LOCATION_HELP)]);
  assert!(!model.def(model.builtins().non_zero()).is_marker());

  // `core`'s `@set` under a name a `use` item gives it is still `core`'s `@set`: the directive is
  // told by what the application resolved to.
  let text = "use ^::core::set as bag\nextend scalar NonZero @bag\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(schema, text, "bag", 1),
      Some(model.def(model.builtins().non_zero()).location())
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // `@map`; `@set` on a primitive `core` materialises, which has no position for a label to point
  // at; and two of the three in one extension, each reported where it is written. `BTreeMap`
  // applies `@map`, and `HashSet` `@set`, in their own definitions, so each extension's is a
  // second application to one location too, labelled at the library's.
  let text =
    "extend scalar BTreeMap @map\nextend scalar Int @set\nextend scalar HashSet @set @map\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let btree_map_def = def(model, "^::alloc::BTreeMap");
  let hash_set_def = def(model, "^::std::HashSet");
  let btree_map = Some(model.def(btree_map_def).location());
  let hash_set = Some(model.def(hash_set_def).location());
  let first_applied = |id: DefId| -> Option<Location> {
    model
      .def(id)
      .applications()
      .first()
      .map(|application: &ApplicationId| model.application(*application).path_location())
  };
  assert_eq!(
    found(&analysis),
    [
      (ORPHAN, at(schema, text, "map", 0), btree_map),
      (
        DUPLICATE_DIRECTIVE_USE,
        at(schema, text, "map", 0),
        first_applied(btree_map_def)
      ),
      (ORPHAN, at(schema, text, "set", 0), None),
      (ORPHAN, at(schema, text, "set", 1), hash_set),
      (
        DUPLICATE_DIRECTIVE_USE,
        at(schema, text, "set", 1),
        first_applied(hash_set_def)
      ),
      (ORPHAN, at(schema, text, "map", 1), hash_set),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      meaning("map", "type `BTreeMap`", "alloc"),
      String::from(
        "the directive `@map` is applied to the type `BTreeMap` again, and it is not repeatable"
      ),
      meaning("set", "type `i32`", "core"),
      meaning("set", "type `HashSet`", "std"),
      String::from(
        "the directive `@set` is applied to the type `HashSet` again, and it is not repeatable"
      ),
      meaning("map", "type `HashSet`", "std"),
    ]
  );
  // None changes what the definition is.
  assert_eq!(
    model.def(def(model, "^::alloc::BTreeMap")).collection(),
    Some(Collection::Map)
  );
  assert_eq!(
    model.def(def(model, "^::std::HashSet")).collection(),
    Some(Collection::Set)
  );
}

#[test]
fn what_gives_a_built_in_no_meaning_is_no_orphan_and_a_dependencys_extension_is_one() {
  // The root package's own directive named `set`, and another of its own, on built-ins.
  let text = "directive @set on SCALAR\nextend scalar NonZero @set\ndirective @x on SCALAR\n\
              extend scalar String @x\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(model.def(model.builtins().non_zero()).collection(), None);

  // `@set` on a GraphQL built-in the root package replaces, which is the root package's own: no
  // orphan. The replaced built-in declares no type parameter, and a set has one: that is the rule
  // on a collection's type parameters' to say.
  let analysis = analyze_one("scalar String\nextend scalar String @set\n");
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
    .collect();
  assert_eq!(codes, [COLLECTION_ARITY], "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(model.def(model.builtins().non_zero()).collection(), None);

  // An extension a phase already refused something in is left alone.
  let text = "extend scalar NonZero @set @missing\n";
  let analysis = analyze_one(text);
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
    .collect();
  assert_eq!(codes, ["smear::schema::undefined-directive"]);

  // A dependency's extension of a built-in is refused whole, once, by the rule's second message,
  // whatever it applies.
  let ours = "type Query {\n  ok: Int\n}\n";
  let theirs = "extend scalar NonZero @set\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let model = analysis.model();
  let non_zero = model.builtins().non_zero();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(geo_file, theirs, "NonZero", 0),
      Some(model.def(non_zero).location())
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "package `geo` extends the type `NonZero` of the built-in package `core`, which only the root \
      package extends"
    ]
  );
  assert_eq!(helps(&analysis), [Some(NOT_THE_ROOT)]);
  assert_eq!(model.def(non_zero).collection(), None);
}

/// What the orphan rule says of the root package's `@extern` applied to `what`, a definition of
/// the built-in package `built_in` written as the message writes it: "type `string`".
fn host(what: &str, built_in: &str) -> String {
  format!(
    "package `app` applies `@extern` to the {what} of the built-in package `{built_in}`, and a \
     built-in's host type is given by `@repr(for: …)` on the schema alone"
  )
}

/// The root package's `@extern` on a built-in it did not write is an orphan: on `alloc`'s
/// `string`, on `i32`, a primitive `core` materialises with no position for a label, and on `std`'s
/// `HashSet`, which an extension names with no parameter list. Each is refused where it is written,
/// and none binds anything. Refuses the root's `@extern` on a built-in accepted, inert or bound.
///
/// `@extern` takes `string!` arguments, so its application to `string` also puts `core`'s
/// `@extern` on a cycle, which the self-reference rule reports at its name, in `core`'s source,
/// first: the orphan rule's refusal leaves the application in the model.
#[test]
fn the_root_giving_a_built_in_a_host_type_is_an_orphan() {
  let text = "type Query {\n  x: Int\n}\n\
              extend scalar string @extern(lang: \"rust\", type: \"my::Str\")\n\
              extend scalar Int @extern(lang: \"rust\", type: \"i32\")\n\
              extend scalar HashSet @extern(lang: \"rust\", type: \"std::collections::HashSet\")\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let string = model.builtins().string();
  let hash_set = def(model, "^::std::HashSet");
  assert_eq!(
    found(&analysis),
    [
      (
        SELF_REFERENTIAL_DIRECTIVE,
        model.def(model.builtins().r#extern()).location(),
        None
      ),
      (
        ORPHAN,
        at(schema, text, "extern", 0),
        Some(model.def(string).location())
      ),
      (ORPHAN, at(schema, text, "extern", 1), None),
      (
        ORPHAN,
        at(schema, text, "extern", 2),
        Some(model.def(hash_set).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      String::from(
        "the directive `@extern` refers to itself, through the directives and types it names"
      ),
      host("type `string`", "alloc"),
      host("type `i32`", "core"),
      host("type `HashSet`", "std"),
    ]
  );
  assert_eq!(
    helps(&analysis),
    [
      Some(SELF_REFERENTIAL_HELP),
      Some(HOST),
      Some(HOST),
      Some(HOST)
    ]
  );
  let int = def(model, "^::core::i32");
  for built_in in [string, int, hash_set] {
    assert_eq!(model.def(built_in).externs().len(), 0);
  }
}

/// A dependency's extension of a built-in that applies `@extern` is refused whole, once, by the
/// orphan rule's second message, at its target. Refuses a second diagnostic at the `@extern`.
///
/// The application stays in the model all the same, so it puts `core`'s `@extern`, which takes
/// `string!` arguments, on a cycle through `string`: the self-reference rule reports it at its
/// name, in `core`'s source, first.
#[test]
fn a_dependencys_extern_on_a_built_in_is_one_orphan() {
  let ours = "type Query {\n  x: Int\n}\n";
  let theirs = "extend scalar string @extern(lang: \"rust\", type: \"geo::Str\")\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  let model = analysis.model();
  let string = model.builtins().string();
  assert_eq!(
    found(&analysis),
    [
      (
        SELF_REFERENTIAL_DIRECTIVE,
        model.def(model.builtins().r#extern()).location(),
        None
      ),
      (
        ORPHAN,
        at(geo_file, theirs, "string", 0),
        Some(model.def(string).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the directive `@extern` refers to itself, through the directives and types it names",
      "package `geo` extends the type `string` of the built-in package `alloc`, which only the root \
       package extends"
    ]
  );
  assert_eq!(
    helps(&analysis),
    [Some(SELF_REFERENTIAL_HELP), Some(NOT_THE_ROOT)]
  );
  assert_eq!(model.def(string).externs().len(), 0);
}

/// Every file of a `Flat` package is one module of one package, as the documents of a GraphQL
/// schema are one schema: an extension of a definition of the package's own, whichever file holds
/// it, is no orphan, and nor is a directive it adds to a built-in. A `Flat` root package that adds
/// more than directives to a built-in does meet the rule, as
/// `the_root_adding_more_than_directives_to_a_built_in_is_an_orphan` shows.
///
/// `String` takes a directive of its own, and not `@d`: `@deprecated` takes a `String`, so `@d`
/// applying `@deprecated` while `String` applied `@d` would put both on a cycle, which is the
/// self-reference rule's.
#[test]
fn a_flat_package_extending_its_own_definitions_never_meets_the_orphan_rule() {
  let a = "type Query {\n  ok: Int\n}\ninterface Node {\n  id: ID\n}\nenum Color {\n  RED\n}\n\
           input Filter {\n  f: Int\n}\nunion Either = Query\nscalar Date\n\
           directive @d on SCALAR | DIRECTIVE_DEFINITION\ndirective @s on SCALAR\n";
  let b = "extend type Query implements Node {\n  id: ID\n  name: String\n}\n\
           extend interface Node {\n  name: String\n}\nextend enum Color {\n  GREEN\n}\n\
           extend input Filter {\n  g: Int\n}\nextend union Either = Other\n\
           extend scalar Date @d\nextend directive @d @deprecated\nextend scalar String @s\n\
           type Other {\n  ok: Int\n}\n";
  for files in [
    [("a.graphqlx", a), ("b.graphqlx", b)],
    [("b.graphqlx", b), ("a.graphqlx", a)],
  ] {
    let root = package("app", Layout::Flat, &files);
    let analysis = analyze(&root, &[], &Limits::new());
    assert!(
      analysis.diagnostics().is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn a_rule_says_nothing_about_a_contribution_a_phase_refused() {
  let resource = DiagnosticKind::ResourceLimit.code().as_str();
  // The definition limit leaves room for `Query` and for `i32`, which `Int` writes, and for no
  // definition after them: the primitive a later schema item writes first is refused where it is
  // written, inside that item.
  let limits = Limits::new().with_max_definitions(library_definitions() + 2);

  // A second schema definition of the root package's that holds the refusal is not also reported
  // as a duplicate.
  let text = "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\nschema {\n  query: u8\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &limits);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(resource, at(schema, text, "u8", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(schema_errors(analysis.model()), [false, true]);
  // With no limit in the way nothing refuses it, and the rule does.
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      at(schema, text, "schema", 1),
      Some(at(schema, text, "schema", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // A dependency's schema definition that holds the refusal is not also reported as a schema item
  // outside the root package.
  let ours = "type Query {\n  ok: Int\n}\n";
  let theirs = "schema {\n  query: u8\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let dependencies = [package("geo", Layout::Flat, &[("geo.graphqlx", theirs)])];
  let analysis = analyze(&root, &dependencies, &limits);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(resource, at(geo_file, theirs, "u8", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(schema_errors(analysis.model()), [true]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(OUTSIDE, at(geo_file, theirs, "schema", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  // An extension of a dependency's type that names a type that is none: name resolution's
  // diagnostic lies within the extension, so the extension is not also reported as an orphan.
  let theirs = "type Point {\n  x: Int\n}\n";
  let dependencies = [package("geo", Layout::Flat, &[("geo.graphqlx", theirs)])];
  let ours = "extend type ^::geo::Point {\n  extra: Missing\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::undefined-type",
      at(schema, ours, "Missing", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let point = def(analysis.model(), "^::geo::Point");
  assert_eq!(
    contributions(analysis.model(), point),
    [(false, false), (true, true)]
  );
  // With a type that is one, nothing refuses the extension, and the rule does.
  let ours = "extend type ^::geo::Point {\n  extra: Int\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &dependencies, &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(schema, ours, "^::geo::Point", 0),
      Some(at(geo_file, theirs, "Point", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_rules_error_marks_its_contribution_and_its_definition_and_fails_the_verdict() {
  // The schema's contributions: the root package's first definition and its second, then a
  // dependency's definition, then that dependency's extension.
  let ours =
    "schema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\nschema {\n  query: Query\n}\n";
  let theirs = "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n\
                extend schema {\n  mutation: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), 3);
  let model = analysis.model();
  // Each item a rule refused holds its error. The first definition, which a label alone points
  // at, holds none.
  assert_eq!(schema_errors(model), [false, true, true, true]);
  // And no definition does: a schema item is no definition's contribution.
  assert!(
    model
      .defs()
      .into_iter()
      .all(|def: DefId| !model.def(def).has_errors())
  );

  // An orphan extension holds its error, and through it the definition it extends does. The
  // definition's own contribution holds none, nor does the member the extension writes, which the
  // extension's target path lies outside of, nor any other definition.
  let ours = "type Query {\n  ok: Int\n}\nextend type ^::geo::Point {\n  label: String\n}\n";
  let theirs = "type Point {\n  x: Int\n}\ntype Line {\n  head: Point\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), 1);
  let model = analysis.model();
  let point = def(model, "^::geo::Point");
  assert_eq!(contributions(model, point), [(false, false), (true, true)]);
  assert!(model.def(point).has_errors());
  let marked_members: Vec<MemberId> = model
    .def(point)
    .fields()
    .into_iter()
    .filter(|member: &MemberId| model.member(*member).has_errors())
    .collect::<Vec<MemberId>>();
  assert!(marked_members.is_empty(), "{marked_members:?}");
  let marked: Vec<&str> = model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).has_errors())
    .map(|id: DefId| model.def(id).name())
    .collect::<Vec<&str>>();
  assert_eq!(marked, ["Point"]);
}

#[test]
fn long_names_are_quoted_as_excerpts() {
  // A dependency whose name is long defines the schema: the message quotes the ends of the name.
  let name = "p".repeat(4096);
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  );
  let dependency: Package<&'static str> = Package::new(name.as_str(), Layout::Flat, Tier::Std)
    .with_files([SourceFile::new(
      "s.graphqlx",
      "schema {\n  query: Query\n}\n",
    )]);
  let analysis = analyze(&root, &[dependency], &Limits::new());
  assert_eq!(
    messages(&analysis),
    [format!(
      "package `{}…{}` defines the schema, which only the root package defines and extends",
      "p".repeat(64),
      "p".repeat(64)
    )]
  );

  // A root package whose name is long extends a type whose name is long, of a dependency whose
  // name is long: the message quotes the ends of each of the three, and no path.
  let root_name = "r".repeat(4096);
  let geo_name = "g".repeat(4096);
  let type_name = "E".repeat(4096);
  let ours = format!("extend type ^::{geo_name}::{type_name} {{\n  extra: Int\n}}\n");
  let theirs = format!("type {type_name} {{\n  x: Int\n}}\n");
  let root: Package<&str> = Package::new(root_name.as_str(), Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", ours.as_str())])
    .with_dependencies([geo_name.as_str()]);
  let geo: Package<&str> = Package::new(geo_name.as_str(), Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("geo.graphqlx", theirs.as_str())]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert_eq!(
    messages(&analysis),
    [format!(
      "package `{}…{}` extends the type `{}…{}`, which package `{}…{}` defines",
      "r".repeat(64),
      "r".repeat(64),
      "E".repeat(64),
      "E".repeat(64),
      "g".repeat(64),
      "g".repeat(64)
    )]
  );
}

/// A program on which a code a rule lists must fire.
type Fixture = fn() -> Analysis;

/// Each code a rule of smear's lists, with a program on which it must fire. The liveness test
/// looks each code of the rule table up here, so the key is the code the table holds.
const FIXTURES: &[(Code, Fixture)] = &[
  (Code::new(DUPLICATE), two_schema_definitions),
  (Code::new(OUTSIDE), a_dependencys_schema_definition),
  (Code::new(ORPHAN), an_extension_of_a_dependencys_type),
  (Code::new(INTROSPECTION), a_field_added_to_a_meta_type),
  (Code::new(GENERIC_ENUM), a_generic_enum),
  (Code::new(MARKER_MISUSE), a_marker_as_a_fields_type),
  (Code::new(COLLECTION_ARITY), a_set_of_two_type_parameters),
  (Code::new(TYPEREF_MISUSE), a_type_ref_as_a_fields_type),
  (Code::new(NONZERO_ARGUMENT), a_non_zero_float),
  (
    Code::new(DUPLICATE_TYPE_PARAMETER),
    two_type_parameters_of_one_name,
  ),
  (Code::new(RESERVED_FIELD_NAME), a_reserved_field_name),
  (Code::new(RESERVED_ARGUMENT_NAME), a_reserved_argument_name),
  (
    Code::new(RESERVED_ENUM_VALUE_NAME),
    a_reserved_enum_value_name,
  ),
  (
    Code::new(RESERVED_INPUT_FIELD_NAME),
    a_reserved_input_field_name,
  ),
  (
    Code::new(RESERVED_DIRECTIVE_ARGUMENT_NAME),
    a_reserved_directive_argument_name,
  ),
  (Code::new(DUPLICATE_FIELD_NAME), a_duplicate_field),
  (Code::new(DUPLICATE_ARGUMENT_NAME), a_duplicate_argument),
  (Code::new(DUPLICATE_ENUM_VALUE), a_duplicate_enum_value),
  (
    Code::new(DUPLICATE_INPUT_FIELD_NAME),
    a_duplicate_input_field,
  ),
  (
    Code::new(DUPLICATE_DIRECTIVE_ARGUMENT_NAME),
    a_duplicate_directive_argument,
  ),
  (Code::new(EMPTY_FIELDS_DEFINITION), an_empty_type),
  (Code::new(EMPTY_UNION_MEMBERS), an_empty_union),
  (Code::new(EMPTY_ENUM_VALUES), an_empty_enum),
  (Code::new(EMPTY_INPUT_FIELDS), an_empty_input_object),
  (
    Code::new(FIELD_TYPE_NOT_OUTPUT_TYPE),
    a_field_typed_by_an_input,
  ),
  (
    Code::new(ARGUMENT_TYPE_NOT_INPUT_TYPE),
    an_argument_typed_by_an_object,
  ),
  (
    Code::new(INPUT_FIELD_TYPE_NOT_INPUT_TYPE),
    an_input_field_typed_by_an_object,
  ),
  (
    Code::new(DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE),
    a_directives_argument_typed_by_an_object,
  ),
  (
    Code::new(IMPLEMENTS_NON_INTERFACE),
    an_object_implementing_an_object,
  ),
  (
    Code::new(SELF_IMPLEMENTING_INTERFACE),
    an_interface_implementing_itself,
  ),
  (Code::new(UNION_MEMBER_NOT_OBJECT), a_union_of_an_interface),
  (
    Code::new(DEPRECATED_REQUIRED_ARGUMENT),
    a_deprecated_required_argument,
  ),
  (
    Code::new(DEPRECATED_REQUIRED_INPUT_FIELD),
    a_deprecated_required_input_field,
  ),
  (
    Code::new(ONE_OF_FIELD_NOT_NULLABLE),
    a_one_of_field_that_is_non_null,
  ),
  (
    Code::new(ONE_OF_FIELD_HAS_DEFAULT),
    a_one_of_field_with_a_default,
  ),
  (
    Code::new(ONE_OF_ON_INPUT_OBJECT_EXTENSION),
    a_one_of_that_an_extension_provides,
  ),
  (
    Code::new(UNSUPPORTED_DIRECTIVE_LOCATION),
    a_directive_where_its_definition_does_not_list,
  ),
  (
    Code::new(DUPLICATE_DIRECTIVE_USE),
    a_directive_that_is_not_repeatable_applied_twice,
  ),
  (
    Code::new(DUPLICATE_DIRECTIVE_ARGUMENT_USE),
    an_application_that_passes_an_argument_twice,
  ),
  (
    Code::new(SELF_REFERENTIAL_DIRECTIVE),
    a_directive_that_refers_to_itself,
  ),
  (
    Code::new(SPECIFIED_BY_ON_BUILT_IN_SCALAR),
    a_built_in_scalar_specified_by_a_url,
  ),
  (
    Code::new(ROOT_OPERATION_TYPE_NOT_OBJECT),
    a_root_that_is_no_object,
  ),
  (Code::new(SHARED_ROOT_OPERATION_TYPE), two_roots_of_one_type),
  (
    Code::new(SCHEMA_EXTENSION_WITHOUT_SCHEMA),
    a_schema_extension_with_no_schema,
  ),
  (Code::new(NO_QUERY_ROOT), a_schema_with_no_query_root),
  (Code::new(EXTERN_MISUSE), one_language_bound_twice),
  (Code::new(REPR_MISUSE), a_for_on_a_field),
  (Code::new(TIER), a_string_in_a_core_package),
];

/// A root package that defines its schema twice.
fn two_schema_definitions() -> Analysis {
  analyze_one(
    "schema {\n  query: Query\n}\nschema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n",
  )
}

/// A dependency that defines the schema.
fn a_dependencys_schema_definition() -> Analysis {
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[(
      "geo.graphqlx",
      "schema {\n  query: Point\n}\ntype Point {\n  x: Int\n}\n",
    )],
  );
  analyze(&root, &[geo], &Limits::new())
}

/// A root package that extends a dependency's type.
fn an_extension_of_a_dependencys_type() -> Analysis {
  let root = package(
    "app",
    Layout::Flat,
    &[(
      "schema.graphqlx",
      "extend type ^::geo::Point {\n  label: String\n}\n",
    )],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("geo.graphqlx", "type Point {\n  x: Int\n}\n")],
  );
  analyze(&root, &[geo], &Limits::new())
}

/// A root package that adds a field to `__Type`: `smear-schema`'s fixture for the kind.
fn a_field_added_to_a_meta_type() -> Analysis {
  analyze_one("type Query { ok: Int } extend type __Type { extra: Int }")
}

/// A root package that declares an enum with a type parameter.
fn a_generic_enum() -> Analysis {
  analyze_one("enum Choice<T> {\n  YES\n  NO\n}\ntype Query {\n  choice: Choice<Int>\n}\n")
}

/// A root package that types a field with a marker.
fn a_marker_as_a_fields_type() -> Analysis {
  analyze_one("interface Marked @marker\ntype Query {\n  marked: Marked\n}\n")
}

/// A root package that makes a scalar of two type parameters a set.
fn a_set_of_two_type_parameters() -> Analysis {
  analyze_one("scalar Pair<A, B> @set\n")
}

/// A root package that types a field with `TypeRef`.
fn a_type_ref_as_a_fields_type() -> Analysis {
  analyze_one("type Query {\n  named: TypeRef\n}\n")
}

/// A root package that types a field with `NonZero` of a float.
fn a_non_zero_float() -> Analysis {
  analyze_one("type Query {\n  count: NonZero<Float>\n}\n")
}

/// A root package that declares two type parameters of one name.
fn two_type_parameters_of_one_name() -> Analysis {
  analyze_one("scalar Pair<T, T>\n")
}

// GraphQL's own refusals, each `smear-schema`'s fixture for its kind, copied as text.

/// A field whose name begins with `__`.
fn a_reserved_field_name() -> Analysis {
  analyze_one("type Query { ok: Int __mine: Int }")
}

/// A field's argument whose name begins with `__`.
fn a_reserved_argument_name() -> Analysis {
  analyze_one("type Query { ok(__a: Int): Int }")
}

/// An enum value whose name begins with `__`.
fn a_reserved_enum_value_name() -> Analysis {
  analyze_one("type Query { ok: Int } enum E { __A }")
}

/// An input field whose name begins with `__`.
fn a_reserved_input_field_name() -> Analysis {
  analyze_one("type Query { ok: Int } input In { __a: Int }")
}

/// A directive's argument whose name begins with `__`.
fn a_reserved_directive_argument_name() -> Analysis {
  analyze_one("type Query { ok: Int } directive @d(__a: Int) on FIELD")
}

/// A field declared twice.
fn a_duplicate_field() -> Analysis {
  analyze_one("type Query { ok: Int dup: Int dup: String }")
}

/// A field's argument declared twice.
fn a_duplicate_argument() -> Analysis {
  analyze_one("type Query { ok(a: Int, a: String): Int }")
}

/// An enum value declared twice.
fn a_duplicate_enum_value() -> Analysis {
  analyze_one("type Query { ok: Int } enum E { A A }")
}

/// An input field declared twice.
fn a_duplicate_input_field() -> Analysis {
  analyze_one("type Query { ok: Int } input In { a: Int a: String }")
}

/// A directive's argument declared twice.
fn a_duplicate_directive_argument() -> Analysis {
  analyze_one("type Query { ok: Int } directive @d(a: Int, a: String) on FIELD")
}

/// An object type with no field.
fn an_empty_type() -> Analysis {
  analyze_one("type Query { ok: Int } type Empty")
}

/// A union with no member.
fn an_empty_union() -> Analysis {
  analyze_one("type Query { ok: Int } union U")
}

/// An enum with no value.
fn an_empty_enum() -> Analysis {
  analyze_one("type Query { ok: Int } enum E")
}

/// An input object with no field.
fn an_empty_input_object() -> Analysis {
  analyze_one("type Query { ok: Int } input In")
}

/// A field typed by an input object.
fn a_field_typed_by_an_input() -> Analysis {
  analyze_one("type Query { ok: In } input In { a: Int }")
}

/// A field's argument typed by an object.
fn an_argument_typed_by_an_object() -> Analysis {
  analyze_one("type Query { ok(a: Query): Int }")
}

/// An input field typed by an object.
fn an_input_field_typed_by_an_object() -> Analysis {
  analyze_one("type Query { ok: Int } input In { a: Query }")
}

/// A directive's argument typed by an object.
fn a_directives_argument_typed_by_an_object() -> Analysis {
  analyze_one("type Query { ok: Int } directive @d(a: Query) on FIELD")
}

/// An object that implements an object.
fn an_object_implementing_an_object() -> Analysis {
  analyze_one("type Query { ok: Int } type Thing { a: Int } type Other implements Thing { a: Int }")
}

/// An interface that implements itself.
fn an_interface_implementing_itself() -> Analysis {
  analyze_one("type Query { ok: Int } interface I implements I { a: Int }")
}

/// A union whose member is an interface.
fn a_union_of_an_interface() -> Analysis {
  analyze_one("type Query { ok: Int } interface I { a: Int } union U = I")
}

/// A required argument that is deprecated.
fn a_deprecated_required_argument() -> Analysis {
  analyze_one("type Query { ok(a: Int! @deprecated): Int }")
}

/// A required input field that is deprecated.
fn a_deprecated_required_input_field() -> Analysis {
  analyze_one("type Query { ok: Int } input In { a: Int! @deprecated }")
}

/// A OneOf input object's field that is non-null.
fn a_one_of_field_that_is_non_null() -> Analysis {
  analyze_one("type Query { ok: Int } input In @oneOf { a: Int! }")
}

/// A OneOf input object's field that declares a default.
fn a_one_of_field_with_a_default() -> Analysis {
  analyze_one("type Query { ok: Int } input In @oneOf { a: Int = 1 }")
}

/// An extension that applies `@oneOf` to an input object.
fn a_one_of_that_an_extension_provides() -> Analysis {
  analyze_one("type Query { ok: Int } input In { a: Int } extend input In @oneOf")
}

/// A directive applied where its definition does not list.
fn a_directive_where_its_definition_does_not_list() -> Analysis {
  analyze_one("directive @onEnum on ENUM\n     type Query @onEnum { ok: Int }")
}

/// A directive that is not repeatable applied twice to one type.
fn a_directive_that_is_not_repeatable_applied_twice() -> Analysis {
  analyze_one("directive @onObject on OBJECT\n     type Query @onObject @onObject { ok: Int }")
}

/// A root operation type that is an interface.
fn a_root_that_is_no_object() -> Analysis {
  analyze_one("interface Root { ok: Int } schema { query: Root }")
}

/// A schema extension with no schema to extend.
fn a_schema_extension_with_no_schema() -> Analysis {
  analyze_one("type Q { ok: Int } extend schema { query: Q }")
}

/// Two root operations of one type.
fn two_roots_of_one_type() -> Analysis {
  analyze_one("type Query { ok: Int } schema { query: Query mutation: Query }")
}

/// A schema with no query root.
fn a_schema_with_no_query_root() -> Analysis {
  analyze_one("type NotTheRoot { ok: Int }")
}

/// A directive that refers to itself, through the input object its argument takes.
fn a_directive_that_refers_to_itself() -> Analysis {
  analyze_one(
    "type Query { ok: Int }\n     directive @d(a: In) on INPUT_FIELD_DEFINITION\n     input In { x: Int @d }",
  )
}

/// `@specifiedBy` on `Int`, which the root package spells out.
fn a_built_in_scalar_specified_by_a_url() -> Analysis {
  analyze_one("type Query { ok: Int } scalar Int @specifiedBy(url: \"https://example.invalid\")")
}

/// An application that passes an argument twice.
fn an_application_that_passes_an_argument_twice() -> Analysis {
  analyze_one(
    "directive @onObject(a: Int) on OBJECT\n     type Query @onObject(a: 1, a: 2) { ok: Int }",
  )
}

/// A root package that binds one scalar to two host types of one language.
fn one_language_bound_twice() -> Analysis {
  analyze_one(
    "type Query {\n  x: Int\n}\n\
     scalar M @extern(lang: \"rust\", type: \"a\") @extern(lang: \"rust\", type: \"b\")\n",
  )
}

/// A root package whose field's `@repr` writes `for`, which is the schema's alone.
fn a_for_on_a_field() -> Analysis {
  analyze_one("type Query {\n  x: Int @repr(lang: \"rust\", type: \"i64\", for: Int)\n}\n")
}

/// A root package declared at `core` that types a field with `String`, which needs `alloc`.
fn a_string_in_a_core_package() -> Analysis {
  let root = Package::new("app", Layout::Flat, Tier::Core).with_files([SourceFile::new(
    "schema.graphqlx",
    "type Query {\n  x: String\n}\n",
  )]);
  analyze(&root, &[], &Limits::new())
}

/// Liveness: every code of every rule of the set `analyze` runs has a fixture and fires on it, and
/// no fixture is for a code no rule lists. A rule added with no fixture fails here, by its name.
#[test]
fn every_code_of_every_rule_fires_on_its_fixture() {
  let mut listed: Vec<Code> = Vec::new();
  for (rule, codes) in RULES {
    assert!(!codes.is_empty(), "the rule `{rule}` lists no code");
    for code in *codes {
      let fixture = FIXTURES
        .iter()
        .find(|(key, _): &&(Code, Fixture)| key == code);
      let Some((_, program)) = fixture else {
        panic!("the rule `{rule}` lists `{code}`, which no fixture makes fire");
      };
      let analysis = program();
      assert!(
        analysis
          .diagnostics()
          .iter()
          .any(|diagnostic: &Diagnostic| diagnostic.code() == *code),
        "`{code}`, which the rule `{rule}` lists, does not fire on its fixture: {:#?}",
        analysis.diagnostics()
      );
      listed.push(*code);
    }
  }
  for (code, _) in FIXTURES {
    assert!(
      listed.contains(code),
      "a fixture is for `{code}`, which no rule lists"
    );
  }
}

/// The root package of the walk written out below: a directive with a defaulted argument; an
/// interface; an object type that implements it, applies the directive, and holds a field with a
/// defaulted argument and a field that is a list; an extension of that type; and a schema
/// definition with an application.
const ORDER_APP: &str = r#"directive @tag(name: String = "none") repeatable on OBJECT | SCHEMA
interface Node {
  id: ID
}
type Query implements Node @tag(name: "q") {
  id: ID
  node(id: ID = "1"): Node
  nodes: [Node!]
}
extend type Query @tag {
  origin: ^::geo::Point
}
schema @tag(name: "s") {
  query: Query
}
"#;

/// Its dependency, `geo`.
const ORDER_GEO: &str = "type Point {\n  x: Float\n}\n";

/// The walk of that program over the supplied packages, hook by hook. A package is written by its
/// id, and every other id as its offset from the first of its kind the walk reaches.
///
/// The definitions are `@tag` (+0), `Node` (+1), `Query` (+2) and `geo`'s `Point` (+3). The
/// members are `@tag`'s `name` (+0), `Node`'s `id` (+1), then `Query`'s direct members, its own
/// `id` (+2), `node` (+3) and `nodes` (+4) and then its extension's `origin` (+5), then `node`'s
/// argument `id` (+6), which every direct member comes before, and `Point`'s `x` (+7).
const ORDER: &[&str] = &[
  "begin",
  "enter_package 3",
  "enter_module +0",
  // `@tag`: its argument's type and default.
  "enter_def +0",
  "enter_member +0",
  "type_expr +0 MemberType",
  "value +0 Default",
  "exit_member +0",
  "exit_def +0",
  // `Node`.
  "enter_def +1",
  "enter_member +1",
  "type_expr +1 MemberType",
  "exit_member +1",
  "exit_def +1",
  // `Query`: its `implements` entry; its own application and its extension's; its own members,
  // an argument nested in its field and a list before its element; then its extension's member.
  "enter_def +2",
  "type_expr +2 Implements",
  "application +0",
  "value +1 ApplicationArgument in +0",
  // The extension's bare `@tag` has its `name`, a copy of its default.
  "application +1",
  "value +2 ApplicationArgument in +1",
  "enter_member +2",
  "type_expr +3 MemberType",
  "exit_member +2",
  "enter_member +3",
  "type_expr +4 MemberType",
  "enter_member +6",
  "type_expr +5 MemberType",
  "value +3 Default",
  "exit_member +6",
  "exit_member +3",
  "enter_member +4",
  "type_expr +6 MemberType",
  "type_expr +7 MemberType",
  "exit_member +4",
  "enter_member +5",
  "type_expr +8 MemberType",
  "exit_member +5",
  "exit_def +2",
  "exit_module +0",
  "exit_package 3",
  // `geo`: `Point`.
  "enter_package 4",
  "enter_module +1",
  "enter_def +3",
  "enter_member +7",
  "type_expr +9 MemberType",
  "exit_member +7",
  "exit_def +3",
  "exit_module +1",
  "exit_package 4",
  // The schema, after every package: its written root, then its application.
  "schema",
  "type_expr +10 RootOperation",
  "application +2",
  "value +4 ApplicationArgument in +2",
  "end",
];

/// Writes down every hook a walk calls: its name and the id it is handed, and for an occurrence
/// where its outermost occurrence sits and the application it is in.
///
/// A package is written by its id. Every other id is written as its offset from the first id of
/// its kind the walk hands a hook, which is the least the walk reaches: the built-in library's
/// rows come first in every table, and how many it has is not what a walk's order is about.
#[derive(Default)]
struct Recorder {
  lines: Vec<String>,
  /// The first id of each kind the walk handed a hook.
  modules: Option<u32>,
  defs: Option<u32>,
  members: Option<u32>,
  type_exprs: Option<u32>,
  applications: Option<u32>,
  values: Option<u32>,
}

/// `id` as its offset from the first id of its kind, which `first` holds once one is reached.
fn offset(first: &mut Option<u32>, id: u32) -> u32 {
  let least = *first.get_or_insert(id);
  id.checked_sub(least)
    .expect("the first id of a kind a walk reaches is the least it reaches")
}

impl Recorder {
  /// An occurrence's line: the hook, the occurrence's offset, its site, and the application it is
  /// in, if any.
  fn occurrence(&mut self, hook: &str, at: u32, cx: &Cx<'_>) -> String {
    let site: String = match cx.site() {
      Some(site) => format!("{site:?}"),
      None => String::from("nowhere"),
    };
    match cx.application() {
      Some(application) => {
        let within = offset(&mut self.applications, application.get());
        format!("{hook} +{at} {site} in +{within}")
      }
      None => format!("{hook} +{at} {site}"),
    }
  }
}

impl Visitor for Recorder {
  fn begin(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("begin"));
  }

  fn enter_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.lines.push(format!("enter_package {}", package.get()));
  }

  fn exit_package(&mut self, _cx: &mut Cx<'_>, package: PackageId) {
    self.lines.push(format!("exit_package {}", package.get()));
  }

  fn enter_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    let at = offset(&mut self.modules, module.get());
    self.lines.push(format!("enter_module +{at}"));
  }

  fn exit_module(&mut self, _cx: &mut Cx<'_>, module: ModuleId) {
    let at = offset(&mut self.modules, module.get());
    self.lines.push(format!("exit_module +{at}"));
  }

  fn enter_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    let at = offset(&mut self.defs, def.get());
    self.lines.push(format!("enter_def +{at}"));
  }

  fn exit_def(&mut self, _cx: &mut Cx<'_>, def: DefId) {
    let at = offset(&mut self.defs, def.get());
    self.lines.push(format!("exit_def +{at}"));
  }

  fn enter_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    let at = offset(&mut self.members, member.get());
    self.lines.push(format!("enter_member +{at}"));
  }

  fn exit_member(&mut self, _cx: &mut Cx<'_>, member: MemberId) {
    let at = offset(&mut self.members, member.get());
    self.lines.push(format!("exit_member +{at}"));
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    let at = offset(&mut self.type_exprs, ty.get());
    let line = self.occurrence("type_expr", at, cx);
    self.lines.push(line);
  }

  fn application(&mut self, _cx: &mut Cx<'_>, application: ApplicationId) {
    let at = offset(&mut self.applications, application.get());
    self.lines.push(format!("application +{at}"));
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    let at = offset(&mut self.values, value.get());
    let line = self.occurrence("value", at, cx);
    self.lines.push(line);
  }

  fn schema(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("schema"));
  }

  fn end(&mut self, _cx: &mut Cx<'_>) {
    self.lines.push(String::from("end"));
  }
}

/// The order: the walk `analyze` runs its rules through, over an analyzed program of two packages,
/// is the sequence of hooks written out above.
#[test]
fn the_walk_of_an_analyzed_program_is_the_written_sequence_of_hooks() {
  let root =
    package("app", Layout::Flat, &[("schema.graphqlx", ORDER_APP)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", ORDER_GEO)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  let mut recorder = Recorder::default();
  let reported = visit(&analysis, Packages::Supplied, &mut recorder);
  assert!(reported.is_empty(), "the recorder reports nothing");
  for (index, (line, want)) in recorder.lines.iter().zip(ORDER).enumerate() {
    assert_eq!(line.as_str(), *want, "line {index} of the walk");
  }
  assert_eq!(recorder.lines.len(), ORDER.len(), "the walk's length");

  // The offsets are from the first rows the walk reaches: the root package's root module; the
  // first definition that module holds, `@tag`; its one member, its argument, with that argument's
  // type and default; and `Query`'s own application.
  let model = analysis.model();
  let root_module = model.package(model.root_package()).root_module();
  let tag = model
    .module(root_module)
    .defs()
    .into_iter()
    .next()
    .expect("the root module holds `@tag`");
  assert_eq!(model.def(tag).name(), "tag");
  let argument = model
    .def(tag)
    .arguments()
    .into_iter()
    .next()
    .expect("`@tag` has an argument");
  assert_eq!(recorder.modules, Some(root_module.get()));
  assert_eq!(recorder.defs, Some(tag.get()));
  assert_eq!(recorder.members, Some(argument.get()));
  assert_eq!(
    recorder.type_exprs,
    model.member(argument).ty().map(|ty: TypeExprId| ty.get())
  );
  assert_eq!(
    recorder.values,
    model
      .member(argument)
      .default()
      .map(|value: ValueId| value.get())
  );
  assert_eq!(
    recorder.applications,
    model
      .def(def(model, "^::app::Query"))
      .applications()
      .first()
      .map(|application: &ApplicationId| application.get())
  );
}
