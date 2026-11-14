//! The GraphQLx analyzer's rules on what a definition holds, each GraphQL's own with GraphQL's
//! code: a member's name does not begin with two underscores; a definition's members have distinct
//! names, and so have a field's arguments; a definition that holds members holds one at least; a
//! member's type is of the kind its position takes; an object or an interface implements
//! interfaces; no interface implements itself; a union's members are object types; a required
//! argument or input field is not deprecated; a OneOf input object's fields are nullable and have
//! no default; and an input object takes `@oneOf` in its definition alone. Each rule judges what a
//! supplied
//! package's definition holds, says nothing within what a phase refused, and quotes a name only as
//! an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Layout, Limits, Namespace, Package, PackageId, SourceFile, SourceId,
    Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a type name that names nothing, a phase's.
const UNDEFINED: &str = "smear::schema::undefined-type";

/// GraphQL's code for the root package's member on an introspection type.
const INTROSPECTION: &str = "smear::schema::introspection-type-extension";

/// GraphQL's codes for a member whose name begins with `__`, by what the member is.
const RESERVED_FIELD: &str = "smear::schema::reserved-field-name";
const RESERVED_ARGUMENT: &str = "smear::schema::reserved-argument-name";
const RESERVED_ENUM_VALUE: &str = "smear::schema::reserved-enum-value-name";
const RESERVED_INPUT_FIELD: &str = "smear::schema::reserved-input-field-name";
const RESERVED_DIRECTIVE_ARGUMENT: &str = "smear::schema::reserved-directive-argument-name";

/// GraphQL's codes for a member whose name an earlier member of its list has, by the list.
const DUPLICATE_FIELD: &str = "smear::schema::duplicate-field-name";
const DUPLICATE_ARGUMENT: &str = "smear::schema::duplicate-argument-name";
const DUPLICATE_ENUM_VALUE: &str = "smear::schema::duplicate-enum-value";
const DUPLICATE_INPUT_FIELD: &str = "smear::schema::duplicate-input-field-name";
const DUPLICATE_DIRECTIVE_ARGUMENT: &str = "smear::schema::duplicate-directive-argument-name";

/// GraphQL's codes for a definition that holds members and holds none, by its kind.
const EMPTY_FIELDS: &str = "smear::schema::empty-fields-definition";
const EMPTY_UNION: &str = "smear::schema::empty-union-members";
const EMPTY_ENUM: &str = "smear::schema::empty-enum-values";
const EMPTY_INPUT: &str = "smear::schema::empty-input-fields";

/// GraphQL's codes for a member typed by a definition of a kind its position does not take.
const FIELD_TYPE: &str = "smear::schema::field-type-not-output-type";
const ARGUMENT_TYPE: &str = "smear::schema::argument-type-not-input-type";
const INPUT_FIELD_TYPE: &str = "smear::schema::input-field-type-not-input-type";
const DIRECTIVE_ARGUMENT_TYPE: &str = "smear::schema::directive-argument-type-not-input-type";

/// GraphQL's code for an `implements` entry that names what is no interface.
const IMPLEMENTS: &str = "smear::schema::implements-non-interface";

/// GraphQL's code for an interface that implements itself.
const SELF_IMPLEMENTING: &str = "smear::schema::self-implementing-interface";

/// GraphQL's code for an interface a definition's interfaces implement and it does not, the
/// implementation phase's.
const TRANSITIVE: &str = "smear::schema::missing-transitive-interface";

/// GraphQL's code for a union member that is no object type.
const UNION_MEMBER: &str = "smear::schema::union-member-not-object";

/// The code of a union member that names nothing, a phase's.
const UNDEFINED_MEMBER: &str = "smear::schema::undefined-union-member";

/// GraphQL's codes for a required argument and a required input field that are deprecated.
const DEPRECATED_ARGUMENT: &str = "smear::schema::deprecated-required-argument";
const DEPRECATED_INPUT_FIELD: &str = "smear::schema::deprecated-required-input-field";

/// GraphQL's codes for the three mistakes of a OneOf input object.
const ONE_OF_NON_NULL: &str = "smear::schema::one-of-field-not-nullable";
const ONE_OF_DEFAULT: &str = "smear::schema::one-of-field-has-default";
const ONE_OF_EXTENSION: &str = "smear::schema::one-of-on-input-object-extension";

/// The code of a marker used as a marker may not be, which L3's rule reports.
const MARKER: &str = "smear::graphqlx::marker-misuse";

/// The code of an `implements` entry that names nothing, a phase's.
const UNDEFINED_INTERFACE: &str = "smear::schema::undefined-implements-interface";

/// The code of an extension its package may not write, the orphan rule's.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// What GraphQL tells the author of an object or an interface with no field.
const FIELDS_HELP: &str = "an object or interface type must define at least one field.";

/// What the analyzer tells the author of an interface with no field that is no marker.
const INTERFACE_HELP: &str = "an object or interface type must define at least one field. An \
                              interface that says what a type is and holds nothing is a marker, \
                              and takes `@marker`.";

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

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`, whose
/// text is `text`.
fn analyze_text(text: &str) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
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

/// Each diagnostic's code, primary location, and first label's location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
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

/// Each diagnostic's code, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect()
}

/// Each diagnostic's message, in the list's order.
fn messages(analysis: &Analysis) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
}

/// The location of the name of the definition the canonical name `name` names.
fn definition(analysis: &Analysis, name: &str) -> Location {
  let model = analysis.model();
  let def = model
    .lookup(Namespace::Type, name)
    .unwrap_or_else(|| panic!("no type `{name}`"));
  model.def(def).location()
}

/// A field, a field's argument, an enum value, an input field and a directive's argument, each
/// named with `__`, beside names that only hold an underscore; a field an extension of the
/// package's own adds; and a field of a generic object.
const RESERVED: &str = "type Query {\n  ok: Int\n}\n\
                        type T {\n  __f: Int\n  _ok: Int\n  ok__: Int\n  g(__a: Int, _b: Int): Int\n}\n\
                        enum E {\n  __V\n  _W\n}\ninput In {\n  __i: Int\n  ok__: Int\n}\n\
                        directive @d(__d: Int, _e: Int) on FIELD\n\
                        extend type T {\n  __x: Int\n}\ntype Box<B> {\n  __b: B\n}\n";

#[test]
fn a_members_name_does_not_begin_with_two_underscores() {
  let analysis = analyze_one(RESERVED);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (RESERVED_FIELD, at(schema, RESERVED, "__f", 0), None),
      (RESERVED_ARGUMENT, at(schema, RESERVED, "__a", 0), None),
      (RESERVED_ENUM_VALUE, at(schema, RESERVED, "__V", 0), None),
      (RESERVED_INPUT_FIELD, at(schema, RESERVED, "__i", 0), None),
      (
        RESERVED_DIRECTIVE_ARGUMENT,
        at(schema, RESERVED, "__d", 0),
        None
      ),
      (RESERVED_FIELD, at(schema, RESERVED, "__x", 0), None),
      (RESERVED_FIELD, at(schema, RESERVED, "__b", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the member and what holds it, a directive with its `@`.
  let reserved =
    |what: &str| -> String { format!("{what} begins with `__`, which introspection reserves") };
  assert_eq!(
    messages(&analysis),
    [
      reserved("the field `__f` of `T`"),
      reserved("the argument `__a` of the field `g` of `T`"),
      reserved("the value `__V` of the enum `E`"),
      reserved("the input field `__i` of `In`"),
      reserved("the argument `__d` of the directive `@d`"),
      reserved("the field `__x` of `T`"),
      reserved("the field `__b` of `Box`"),
    ]
  );
  // GraphQL's own phrases and help.
  let labels: Vec<Option<&str>> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.primary_label())
    .collect();
  assert_eq!(
    labels[..5],
    [
      Some("field name is reserved for introspection"),
      Some("argument name is reserved for introspection"),
      Some("enum value name is reserved for introspection"),
      Some("input field name is reserved for introspection"),
      Some("directive argument name is reserved for introspection"),
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.help(),
      Some("a name beginning with `__` is reserved for introspection; rename it.")
    );
  }

  // A dependency's member is refused in the dependency's own source.
  let theirs = "type Point {\n  __g: Int\n  x: Int\n}\n";
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(RESERVED_FIELD, at(geo_file, theirs, "__g", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_replaced_built_in_is_judged_and_a_built_ins_extension_is_not() {
  // The root package's own `@deprecated` replaces `core`'s, and is the root package's to answer
  // for; what the root package adds to `__Type` is GraphQL's own refusal, and that alone.
  let text = "type Query {\n  ok: Int\n}\n\
              directive @deprecated(__why: String) on FIELD_DEFINITION\n\
              extend type __Type {\n  __x: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        RESERVED_DIRECTIVE_ARGUMENT,
        at(schema, text, "__why", 0),
        None
      ),
      (
        INTROSPECTION,
        at(schema, text, "__Type", 0),
        Some(definition(&analysis, "^::core::__Type"))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // A built-in's extension is judged by none of these rules, whatever it adds: a field `__Type`
  // already has, a field typed by an input object, a required argument that is deprecated, an
  // argument typed by an object, and an entry that names an object. Each extension is GraphQL's
  // one refusal of it, and nothing more.
  let text = "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Obj {\n  o: Int\n}\n\
              extend type __Type {\n  name: String\n  bad: In\n  \
              f(a: Int! @deprecated, o: Obj): Int\n}\nextend type __Schema implements Obj\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        INTROSPECTION,
        at(schema, text, "__Type", 0),
        Some(definition(&analysis, "^::core::__Type"))
      ),
      (
        INTROSPECTION,
        at(schema, text, "__Schema", 0),
        Some(definition(&analysis, "^::core::__Schema"))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Each of the five lists with a name written twice, and a name written three times; beside two
/// fields that each take an argument of one name, and a field that takes an argument of its own
/// name.
const DUPLICATES: &str = "type Query {\n  ok: Int\n}\n\
                          type T {\n  dupf: Int\n  dupf: String\n  \
                          g(dupa: Int, dupa: String): Int\n  h(a: Int): Int\n  k(a: Int): Int\n  \
                          a(a: Int): Int\n}\n\
                          enum E {\n  DUPV\n  DUPV\n}\ninput In {\n  dupi: Int\n  dupi: String\n}\n\
                          directive @d(dupd: Int, dupd: String) on FIELD\n\
                          type Thrice {\n  three: Int\n  three: Int\n  three: Int\n}\n";

#[test]
fn a_member_is_declared_once() {
  let analysis = analyze_one(DUPLICATES);
  let schema = schema_file(&analysis);
  let twice = |code: &'static str, name: &str| -> (&'static str, Location, Option<Location>) {
    (
      code,
      at(schema, DUPLICATES, name, 1),
      Some(at(schema, DUPLICATES, name, 0)),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      twice(DUPLICATE_FIELD, "dupf"),
      twice(DUPLICATE_ARGUMENT, "dupa"),
      twice(DUPLICATE_ENUM_VALUE, "DUPV"),
      twice(DUPLICATE_INPUT_FIELD, "dupi"),
      twice(DUPLICATE_DIRECTIVE_ARGUMENT, "dupd"),
      twice(DUPLICATE_FIELD, "three"),
      // The third is labelled at the first, not at the second.
      (
        DUPLICATE_FIELD,
        at(schema, DUPLICATES, "three", 2),
        Some(at(schema, DUPLICATES, "three", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[..5],
    [
      "`T` declares the field `dupf` again",
      "the field `g` of `T` declares the argument `dupa` again",
      "the enum `E` declares the value `DUPV` again",
      "`In` declares the input field `dupi` again",
      "the directive `@d` declares the argument `dupd` again",
    ]
  );
  // GraphQL's own phrases, for the member and for the first of its name, and no help.
  let phrases: Vec<(Option<&str>, Option<&str>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.primary_label(),
        diagnostic.label(0).map(|label| label.text()),
      )
    })
    .collect();
  assert_eq!(
    phrases[..5],
    [
      (Some("duplicate field"), Some("first defined here")),
      (Some("duplicate argument"), Some("first declared here")),
      (Some("duplicate enum value"), Some("first defined here")),
      (Some("duplicate input field"), Some("first defined here")),
      (
        Some("duplicate directive argument"),
        Some("first declared here")
      ),
    ]
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.help(), None);
  }
}

#[test]
fn an_extension_repeats_no_member_of_its_definition() {
  // In one file: each repetition is reported in the extension, labelled in the definition.
  let text = "type Query {\n  ok: Int\n}\ntype T {\n  fa: Int\n}\nextend type T {\n  fa: String\n}\n\
              enum E {\n  VA\n}\nextend enum E {\n  VA\n}\n\
              input In {\n  ia: Int\n}\nextend input In {\n  ia: String\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let again = |code: &'static str, name: &str| -> (&'static str, Location, Option<Location>) {
    (
      code,
      at(schema, text, name, 1),
      Some(at(schema, text, name, 0)),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      again(DUPLICATE_FIELD, "fa"),
      again(DUPLICATE_ENUM_VALUE, "VA"),
      again(DUPLICATE_INPUT_FIELD, "ia"),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // In a second file of a `Flat` package, the label is in the first file.
  let first = "type Query {\n  ok: Int\n}\ntype T {\n  fa: Int\n}\n";
  let second = "extend type T {\n  fa: String\n}\n";
  let root = package(
    "app",
    Layout::Flat,
    &[("a.graphqlx", first), ("b.graphqlx", second)],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "a.graphqlx");
  let b = file(&analysis, 3, "b.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE_FIELD,
      at(b, second, "fa", 0),
      Some(at(a, first, "fa", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_wide_list_reports_as_a_narrow_one() {
  // Sixty-six fields, past the width a list is scanned at, the last named as the first.
  let mut text = String::from("type Query {\n  ok: Int\n}\ntype Wide {\n");
  for index in 0..65 {
    text.push_str(&format!("  f{index}: Int\n"));
  }
  text.push_str("  f0: String\n}\n");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE_FIELD,
      at(schema, &text, "f0", 1),
      Some(at(schema, &text, "f0", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn two_definitions_of_one_name_are_two_definitions() {
  let analysis =
    analyze_one("type Query {\n  ok: Int\n}\ntype T {\n  a: Int\n}\ntype T {\n  a: Int\n}\n");
  assert_eq!(
    codes(&analysis),
    ["smear::schema::duplicate-type-name"],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// One definition of each kind that holds members, each holding none, and a generic one; beside a
/// scalar and a directive with no argument, which hold no members.
const EMPTY: &str = "type Query {\n  ok: Int\n}\ntype T\ninterface I\nunion U\nenum E\ninput In\n\
                     type Box<B>\nscalar S\ndirective @d on OBJECT\n";

#[test]
fn a_definition_that_holds_members_holds_one() {
  let analysis = analyze_one(EMPTY);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (EMPTY_FIELDS, named(schema, EMPTY, "type ", "T", 0), None),
      (
        EMPTY_FIELDS,
        named(schema, EMPTY, "interface ", "I", 0),
        None
      ),
      (EMPTY_UNION, named(schema, EMPTY, "union ", "U", 0), None),
      (EMPTY_ENUM, named(schema, EMPTY, "enum ", "E", 0), None),
      (EMPTY_INPUT, named(schema, EMPTY, "input ", "In", 0), None),
      (EMPTY_FIELDS, named(schema, EMPTY, "type ", "Box", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the type `T` holds no field",
      "the interface `I` holds no field",
      "the union `U` holds no member type",
      "the enum `E` holds no value",
      "the input object `In` holds no field",
      "the type `Box` holds no field",
    ]
  );
  let words: Vec<(Option<&str>, Option<&str>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.primary_label(), diagnostic.help()))
    .collect();
  assert_eq!(
    words,
    [
      (Some("type defines no fields"), Some(FIELDS_HELP)),
      (Some("type defines no fields"), Some(INTERFACE_HELP)),
      (Some("union declares no member types"), None),
      (Some("enum defines no values"), None),
      (Some("input object defines no fields"), None),
      (Some("type defines no fields"), Some(FIELDS_HELP)),
    ]
  );
}

#[test]
fn a_marker_and_an_extended_definition_are_not_empty() {
  // A marker; an interface its own package's extension makes one; a type and a union whose every
  // member an extension of their own package adds.
  let analysis = analyze_one(
    "type Query {\n  ok: Int\n}\ninterface M @marker\ninterface P\nextend interface P @marker\n\
     type T\nextend type T {\n  a: Int\n}\nunion U\nextend union U = Query\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // What another package's extension adds counts, as GraphQL judges the merged type; and its
  // `@marker` makes no marker, so an interface it is applied to is still empty.
  let ours = "type Query {\n  ok: Int\n}\nextend type ^::geo::Point {\n  x: Int\n}\n\
              extend interface ^::geo::Face @marker\n";
  let theirs = "type Point\ninterface Face\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, ours, "^::geo::Point", 0),
        Some(named(geo_file, theirs, "type ", "Point", 0))
      ),
      (
        ORPHAN,
        at(schema, ours, "^::geo::Face", 0),
        Some(named(geo_file, theirs, "interface ", "Face", 0))
      ),
      (
        EMPTY_FIELDS,
        named(geo_file, theirs, "interface ", "Face", 0),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// An input object, an object, an interface, a union, an enum and a scalar; then each of them as
/// the type of a field, of a field's argument, of an input field and of a directive's argument,
/// the input object written under lists too, and the three output types as a list's element.
const KINDS: &str = "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Obj {\n  o: Int\n}\n\
                     interface Face {\n  f: Int\n}\nunion Either = Obj\nenum Color {\n  RED\n}\n\
                     scalar Date\n\
                     type Fields {\n  bad: In\n  worse: [In!]!\n  date: Date\n  color: Color\n  \
                     obj: Obj\n  face: Face\n  either: Either\n}\n\
                     type Arguments {\n  \
                     f(a1: Obj, a2: Face, a3: Either, a4: [Obj], d: Date, c: Color, i: In): Int\n}\n\
                     input Inputs {\n  i1: Obj\n  i2: Face\n  i3: Either\n  i4: [Obj]\n  d: Date\n  \
                     c: Color\n  i: In\n}\n\
                     directive @d(d1: Obj, d2: Face, d3: Either, d4: [Obj], d: Date, c: Color, \
                     i: In) on FIELD\n";

#[test]
fn a_members_type_is_of_the_kind_its_position_takes() {
  let analysis = analyze_one(KINDS);
  let schema = schema_file(&analysis);
  let refused =
    |code: &'static str, before: &str, name: &str| -> (&'static str, Location, Option<Location>) {
      (code, named(schema, KINDS, before, name, 0), None)
    };
  assert_eq!(
    found(&analysis),
    [
      refused(FIELD_TYPE, "bad: ", "In"),
      // At the named type under the list, its `!` included, and not at the list.
      (FIELD_TYPE, at(schema, KINDS, "In!", 0), None),
      refused(ARGUMENT_TYPE, "a1: ", "Obj"),
      refused(ARGUMENT_TYPE, "a2: ", "Face"),
      refused(ARGUMENT_TYPE, "a3: ", "Either"),
      refused(ARGUMENT_TYPE, "a4: [", "Obj"),
      refused(INPUT_FIELD_TYPE, "i1: ", "Obj"),
      refused(INPUT_FIELD_TYPE, "i2: ", "Face"),
      refused(INPUT_FIELD_TYPE, "i3: ", "Either"),
      refused(INPUT_FIELD_TYPE, "i4: [", "Obj"),
      refused(DIRECTIVE_ARGUMENT_TYPE, "d1: ", "Obj"),
      refused(DIRECTIVE_ARGUMENT_TYPE, "d2: ", "Face"),
      refused(DIRECTIVE_ARGUMENT_TYPE, "d3: ", "Either"),
      refused(DIRECTIVE_ARGUMENT_TYPE, "d4: [", "Obj"),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the member, its owner and what the base names, and prints no type.
  let messages = messages(&analysis);
  assert_eq!(
    messages[0],
    "the field `bad` of `Fields` is typed by the input object `In`, and a field's type is an \
     output type"
  );
  assert_eq!(
    messages[3],
    "the argument `a2` of the field `f` of `Arguments` is typed by the interface `Face`, and an \
     argument's type is an input type"
  );
  assert_eq!(
    messages[8],
    "the input field `i3` of `Inputs` is typed by the union `Either`, and an input field's type \
     is an input type"
  );
  assert_eq!(
    messages[10],
    "the argument `d1` of the directive `@d` is typed by the object type `Obj`, and a \
     directive's argument's type is an input type"
  );
  // GraphQL's own phrases and helps, one of each code.
  let words: Vec<(Option<&str>, Option<&str>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.primary_label(), diagnostic.help()))
    .collect();
  assert_eq!(
    [words[0], words[2], words[6], words[10]],
    [
      (
        Some("field type is not an output type"),
        Some(
          "a field's type must be a scalar, object, interface, union or enum; an input object \
           appears only in argument and input-field positions."
        )
      ),
      (
        Some("argument type is not an input type"),
        Some("an argument's type must be a scalar, enum or input object.")
      ),
      (
        Some("input field type is not an input type"),
        Some("an input field's type must be a scalar, enum or input object.")
      ),
      (
        Some("directive argument type is not an input type"),
        Some("a directive argument's type must be a scalar, enum or input object.")
      ),
    ]
  );
}

#[test]
fn a_scalars_arguments_are_opaque_and_a_collections_are_not() {
  // A scalar that is no collection hides its arguments, and a type parameter is a type of every
  // position; a collection holds values of its arguments, so an object in a map's value is no
  // input type, at the object; a generic input object as a field's type and a generic object as
  // an argument's are judged by their kind, at the whole occurrence.
  let text = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\nscalar Ref<R>\n\
              input Filter<F> {\n  eq: F\n}\ntype Page<P> {\n  items: [P]\n  at(cursor: P): Int\n}\n\
              type Uses {\n  r(a: Ref<Obj>): Int\n  m(a: HashMap<string, Obj>): Int\n  \
              filtered: Filter<Int>\n  paged(a: Page<Int>): Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        ARGUMENT_TYPE,
        named(schema, text, "HashMap<string, ", "Obj", 0),
        None
      ),
      (FIELD_TYPE, at(schema, text, "Filter<Int>", 0), None),
      (ARGUMENT_TYPE, at(schema, text, "Page<Int>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// An object, an input object, a user's set and a user's map whose value a default fills, and a
/// scalar that is no collection; then collections of them in every position, nested, under lists,
/// and with a parameter, the refused ones first in each definition.
const COLLECTIONS: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ninput In {\n  i: Int\n}\n\
                           scalar Bag<T> @set\nscalar Pair<A, B = Obj> @map\nscalar Ref<R>\n\
                           type Uses {\n  f1(m: HashMap<string, Obj>): Int\n  \
                           f2(s: [Bag<Obj>!]): Int\n  f3(n: HashMap<string, Bag<Obj>>): Int\n  \
                           g: HashMap<string, In>\n  f4(p: Pair<string>): Int\n  \
                           f5(k: HashMap<string, In>): Int\n  h: HashMap<string, Obj>\n  \
                           f6(r: Ref<Obj>): Int\n}\n\
                           input Inputs {\n  b: Bag<Obj>\n  c: Bag<In>\n}\n\
                           directive @d(dm: HashMap<string, Obj>) on FIELD\n\
                           type G<T> {\n  f7(a: HashMap<string, T>): Int\n}\n";

#[test]
fn a_collections_arguments_take_the_kind_of_its_place() {
  let analysis = analyze_one(COLLECTIONS);
  let schema = schema_file(&analysis);
  let leaf =
    |code: &'static str, before: &str, name: &str| -> (&'static str, Location, Option<Location>) {
      (code, named(schema, COLLECTIONS, before, name, 0), None)
    };
  assert_eq!(
    found(&analysis),
    [
      leaf(ARGUMENT_TYPE, "m: HashMap<string, ", "Obj"),
      // Under the list and the `!`, at the set's element.
      leaf(ARGUMENT_TYPE, "s: [Bag<", "Obj"),
      // Through two collections, at the deepest.
      leaf(ARGUMENT_TYPE, "n: HashMap<string, Bag<", "Obj"),
      leaf(FIELD_TYPE, "g: HashMap<string, ", "In"),
      // The value is the default's: at the occurrence, labelled at the default.
      (
        ARGUMENT_TYPE,
        at(schema, COLLECTIONS, "Pair<string>", 0),
        Some(named(schema, COLLECTIONS, "B = ", "Obj", 0))
      ),
      leaf(INPUT_FIELD_TYPE, "b: Bag<", "Obj"),
      leaf(DIRECTIVE_ARGUMENT_TYPE, "dm: HashMap<string, ", "Obj"),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the member, its owner, the collection and what does not fit, and prints no type.
  let messages = messages(&analysis);
  assert_eq!(
    messages[0],
    "the argument `m` of the field `f1` of `Uses` is typed by the collection `HashMap`, whose \
     argument is the object type `Obj`, and an argument's type is an input type"
  );
  assert_eq!(
    messages[2],
    "the argument `n` of the field `f3` of `Uses` is typed by the collection `HashMap`, whose \
     argument is the object type `Obj`, and an argument's type is an input type"
  );
  assert_eq!(
    messages[3],
    "the field `g` of `Uses` is typed by the collection `HashMap`, whose argument is the input \
     object `In`, and a field's type is an output type"
  );
  assert_eq!(
    messages[4],
    "the argument `p` of the field `f4` of `Uses` is typed by the collection `Pair`, whose \
     argument is the object type `Obj`, and an argument's type is an input type"
  );
  assert_eq!(
    messages[6],
    "the argument `dm` of the directive `@d` is typed by the collection `HashMap`, whose \
     argument is the object type `Obj`, and a directive's argument's type is an input type"
  );
  // GraphQL's phrases and helps, as the base check gives them, and the default's label.
  let diagnostics = analysis.diagnostics();
  let first = diagnostics.iter().next().expect("a diagnostic");
  assert_eq!(
    (first.primary_label(), first.help()),
    (
      Some("argument type is not an input type"),
      Some("an argument's type must be a scalar, enum or input object.")
    )
  );
  let filled = diagnostics.iter().nth(4).expect("five diagnostics");
  assert_eq!(
    filled.label(0).map(|label| label.text()),
    Some("filled from this default")
  );
}

/// An object, a scalar, an enum, a union, an input object, interfaces and a marker; then objects
/// and an interface that implement each of what is no interface, a type parameter and an alias;
/// an extension's entry; and entries that are interfaces, one with an argument, and a marker.
const IMPLEMENTS_SITES: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\nscalar Sca\n\
                                enum Enu {\n  V\n}\nunion Uni = Obj\ninput Inp {\n  i: Int\n}\n\
                                interface Node {\n  id: ID\n}\ninterface Gen<X> {\n  id: ID\n}\n\
                                interface Mark @marker\n\
                                type A1 implements Obj {\n  id: ID\n}\n\
                                type A2 implements Sca {\n  id: ID\n}\n\
                                type A3 implements Enu {\n  id: ID\n}\n\
                                type A4 implements Uni {\n  id: ID\n}\n\
                                type A5 implements Inp {\n  id: ID\n}\n\
                                interface I1 implements Obj {\n  id: ID\n}\n\
                                type O<T> implements T {\n  id: ID\n}\n\
                                type N implements NonZeroU32 {\n  id: ID\n}\n\
                                type E {\n  id: ID\n}\nextend type E implements Obj\n\
                                type Good implements Node & Gen<ID> & Mark {\n  id: ID\n}\n\
                                scalar S implements Obj\n";

#[test]
fn an_object_or_an_interface_implements_interfaces() {
  let text = IMPLEMENTS_SITES;
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let entry = |before: &str, name: &str| -> (&'static str, Location, Option<Location>) {
    (IMPLEMENTS, named(schema, text, before, name, 0), None)
  };
  assert_eq!(
    found(&analysis),
    [
      entry("A1 implements ", "Obj"),
      entry("A2 implements ", "Sca"),
      entry("A3 implements ", "Enu"),
      entry("A4 implements ", "Uni"),
      entry("A5 implements ", "Inp"),
      entry("I1 implements ", "Obj"),
      entry("O<T> implements ", "T"),
      entry("N implements ", "NonZeroU32"),
      entry("E implements ", "Obj"),
      // A scalar implements markers alone, which is the marker rule's, and not this rule's.
      (MARKER, named(schema, text, "S implements ", "Obj", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[..9],
    [
      "the type `A1` implements `Obj`, which is an object type and no interface",
      "the type `A2` implements `Sca`, which is a scalar and no interface",
      "the type `A3` implements `Enu`, which is an enum and no interface",
      "the type `A4` implements `Uni`, which is a union and no interface",
      "the type `A5` implements `Inp`, which is an input object and no interface",
      "the interface `I1` implements `Obj`, which is an object type and no interface",
      "the type `O` implements its type parameter `T`, and a type parameter is no interface",
      "the type `N` implements `NonZero`, which is a scalar and no interface",
      "the type `E` implements `Obj`, which is an object type and no interface",
    ]
  );
  let first = analysis.diagnostics().iter().next().expect("a diagnostic");
  assert_eq!(
    first.primary_label(),
    Some("implemented type is not an interface")
  );
  assert_eq!(first.help(), None);
}

/// An interface that implements itself; two, and then three, in a ring; one whose entry names
/// itself with other arguments; a ring an extension closes; and one that implements another that
/// leads nowhere back.
const RINGS: &str = "type Query {\n  ok: Int\n}\ninterface Selfish implements Selfish {\n  id: ID\n}\n\
                     interface P1 implements P2 {\n  id: ID\n}\ninterface P2 implements P1 {\n  id: ID\n}\n\
                     interface R1 implements R2 {\n  id: ID\n}\ninterface R2 implements R3 {\n  id: ID\n}\n\
                     interface R3 implements R1 {\n  id: ID\n}\n\
                     scalar Box<B>\ninterface G<X> implements G<Box<X>> {\n  id: ID\n}\n\
                     interface E1 implements E2 {\n  id: ID\n}\ninterface E2 {\n  id: ID\n}\n\
                     extend interface E2 implements E1\n\
                     interface Down implements Leaf {\n  id: ID\n}\ninterface Leaf {\n  id: ID\n}\n";

/// Each interface on a ring is refused at its name, but on the ring of three. There each interface
/// misses the interface its own interface implements, which the implementation phase reports at
/// its entry, labelled at the entry that implements it; that error, in the interface's own
/// contribution, silences this rule there. On every other ring the interface missed is the
/// definition itself, which the phase passes by.
#[test]
fn no_interface_implements_itself() {
  let analysis = analyze_one(RINGS);
  let schema = schema_file(&analysis);
  let ring = |name: &str| -> (&'static str, Location, Option<Location>) {
    (
      SELF_IMPLEMENTING,
      named(schema, RINGS, "interface ", name, 0),
      None,
    )
  };
  let entry = |of: &str, names: &str| -> Location {
    named(schema, RINGS, &format!("{of} implements "), names, 0)
  };
  assert_eq!(
    found(&analysis),
    [
      ring("Selfish"),
      ring("P1"),
      ring("P2"),
      (TRANSITIVE, entry("R1", "R2"), Some(entry("R2", "R3"))),
      (TRANSITIVE, entry("R2", "R3"), Some(entry("R3", "R1"))),
      (TRANSITIVE, entry("R3", "R1"), Some(entry("R1", "R2"))),
      ring("G"),
      ring("E1"),
      ring("E2"),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[0],
    "the interface `Selfish` implements itself, through the interfaces it implements"
  );
  let first = analysis.diagnostics().iter().next().expect("a diagnostic");
  assert_eq!(first.primary_label(), Some("interface implements itself"));
  assert_eq!(first.help(), None);
}

/// Unions of an interface, a scalar, an enum, an input object, a union and a generic scalar; then
/// of an object, a generic object, and a generic union's own parameters.
const UNIONS: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ntype Page<P> {\n  items: [P]\n}\n\
                      interface Face {\n  f: Int\n}\nscalar Sca\nenum Enu {\n  V\n}\n\
                      input Inp {\n  i: Int\n}\nunion Inner = Obj\nscalar Box<B>\n\
                      union U1 = Face\nunion U2 = Sca\nunion U3 = Enu\nunion U4 = Inp\n\
                      union U5 = Inner\nunion U6 = Box<Int>\n\
                      union Good = Obj | Page<Obj>\nunion Either<A, B> = A | B\n";

#[test]
fn a_unions_members_are_object_types() {
  let analysis = analyze_one(UNIONS);
  let schema = schema_file(&analysis);
  let member = |before: &str, name: &str| -> (&'static str, Location, Option<Location>) {
    (UNION_MEMBER, named(schema, UNIONS, before, name, 0), None)
  };
  assert_eq!(
    found(&analysis),
    [
      member("U1 = ", "Face"),
      member("U2 = ", "Sca"),
      member("U3 = ", "Enu"),
      member("U4 = ", "Inp"),
      member("U5 = ", "Inner"),
      (UNION_MEMBER, at(schema, UNIONS, "Box<Int>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the union `U1` holds `Face`, which is an interface and no object type",
      "the union `U2` holds `Sca`, which is a scalar and no object type",
      "the union `U3` holds `Enu`, which is an enum and no object type",
      "the union `U4` holds `Inp`, which is an input object and no object type",
      "the union `U5` holds `Inner`, which is a union and no object type",
      "the union `U6` holds `Box`, which is a scalar and no object type",
    ]
  );
  let first = analysis.diagnostics().iter().next().expect("a diagnostic");
  assert_eq!(
    (first.primary_label(), first.help()),
    (
      Some("union member is not an object type"),
      Some(
        "a union's members must be object types: a union of interfaces or of unions has no \
         concrete member set."
      )
    )
  );
}

/// A required argument of a field, of a directive and of a generic object's field, and a required
/// input field, each deprecated; beside a nullable one, one with a default, a field, and a required
/// argument that applies another directive.
const DEPRECATED: &str = "type Query {\n  ok: Int\n}\n\
                          type T {\n  f(req: Int! @deprecated, opt: Int @deprecated, \
                          dflt: Int! = 1 @deprecated, other: Int! @obsolete): Int\n  \
                          g: Int! @deprecated\n}\n\
                          directive @dd(dreq: [Int]! @deprecated) on FIELD\n\
                          input In {\n  ireq: Int! @deprecated\n  iopt: Int @deprecated\n}\n\
                          type G<P> {\n  h(preq: P! @deprecated): Int\n}\n\
                          directive @obsolete on ARGUMENT_DEFINITION\n";

#[test]
fn a_required_argument_or_input_field_is_not_deprecated() {
  let analysis = analyze_one(DEPRECATED);
  let schema = schema_file(&analysis);
  // Each at its name, labelled at its `@deprecated`, the first, fourth, fifth and seventh of the
  // text's.
  let deprecated =
    |nth: usize| -> Option<Location> { Some(at(schema, DEPRECATED, "deprecated", nth)) };
  assert_eq!(
    found(&analysis),
    [
      (
        DEPRECATED_ARGUMENT,
        named(schema, DEPRECATED, "f(", "req", 0),
        deprecated(0)
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, DEPRECATED, "@dd(", "dreq", 0),
        deprecated(4)
      ),
      (
        DEPRECATED_INPUT_FIELD,
        named(schema, DEPRECATED, "  ", "ireq", 0),
        deprecated(5)
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, DEPRECATED, "h(", "preq", 0),
        deprecated(7)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the argument `req` of the field `f` of `T` is required and deprecated",
      "the argument `dreq` of the directive `@dd` is required and deprecated",
      "the input field `ireq` of `In` is required and deprecated",
      "the argument `preq` of the field `h` of `G` is required and deprecated",
    ]
  );
  let words: Vec<(Option<&str>, Option<&str>, Option<&str>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.primary_label(),
        diagnostic.label(0).map(|label| label.text()),
        diagnostic.help(),
      )
    })
    .collect();
  assert_eq!(
    words[1..3],
    [
      (
        Some("required argument is deprecated"),
        Some("deprecated here"),
        Some(
          "to deprecate a required argument, first make it optional — give it a default, or drop \
           the `!`."
        )
      ),
      (
        Some("required input field is deprecated"),
        Some("deprecated here"),
        Some(
          "to deprecate a required input field, first make it optional — give it a default, or \
           drop the `!`."
        )
      ),
    ]
  );

  // A directive merely named `deprecated`, in another module, is another directive.
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", "type Query {\n  ok: Int\n}\n"),
      (
        "m/a.graphqlx",
        "directive @deprecated on ARGUMENT_DEFINITION\ntype T {\n  f(a: Int! @deprecated): Int\n}\n",
      ),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A OneOf input object with a field of each shape, and a generic one whose field is a type
/// parameter.
const ONE_OF: &str = "type Query {\n  ok: Int\n}\n\
                      input A @oneOf {\n  a: Int!\n  b: Int = 1\n  c: Int! = 2\n  d: [Int!]\n}\n\
                      input Choice<T> @oneOf {\n  x: T\n}\n";

#[test]
fn a_one_of_input_objects_fields_are_nullable_and_have_no_default() {
  let analysis = analyze_one(ONE_OF);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (ONE_OF_NON_NULL, named(schema, ONE_OF, "  ", "a", 0), None),
      (ONE_OF_DEFAULT, named(schema, ONE_OF, "  ", "b", 0), None),
      // A field that is both is two diagnostics, one of each rule.
      (ONE_OF_DEFAULT, named(schema, ONE_OF, "  ", "c", 0), None),
      (ONE_OF_NON_NULL, named(schema, ONE_OF, "  ", "c", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[..2],
    [
      "the field `a` of the OneOf input object `A` is non-null",
      "the field `b` of the OneOf input object `A` has a default value",
    ]
  );
  let words: Vec<(Option<&str>, Option<&str>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.primary_label(), diagnostic.help()))
    .collect();
  assert_eq!(
    words[..2],
    [
      (
        Some("field of a @oneOf input object is non-null"),
        Some(
          "every field of a `@oneOf` input object is one of the alternatives, so each one has to \
           be nullable."
        )
      ),
      (
        Some("field of a @oneOf input object has a default value"),
        Some("a defaulted field would always be present, which is the one thing `@oneOf` forbids.")
      ),
    ]
  );
}

#[test]
fn no_extension_makes_an_input_object_one_of() {
  // An extension of the package's own input object that applies `@oneOf` is refused at it, and
  // the input object is OneOf all the same: the definition's field is judged, and so is the
  // extension's own.
  let text = "type Query {\n  ok: Int\n}\ninput In {\n  a: Int!\n}\nextend input In @oneOf {\n  \
              b: Int!\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (ONE_OF_NON_NULL, named(schema, text, "  ", "a", 0), None),
      (ONE_OF_EXTENSION, at(schema, text, "oneOf", 0), None),
      (ONE_OF_NON_NULL, named(schema, text, "  ", "b", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let refused = analysis.diagnostics().iter().nth(1).expect("a diagnostic");
  assert_eq!(
    refused.to_string(),
    "this extension of the input object `In` applies `@oneOf`, which belongs on its definition"
  );
  assert_eq!(
    (refused.primary_label(), refused.help()),
    (
      Some("@oneOf is provided by an input object type extension"),
      Some(
        "`@oneOf` changes what every existing field of the type means, so it belongs on the \
         definition."
      )
    )
  );

  // Another package's extension is an orphan, and its `@oneOf` is refused where it is written,
  // and makes nothing OneOf: the dependency's field is not judged.
  let ours = "type Query {\n  ok: Int\n}\nextend input ^::dep::In @oneOf\n";
  let theirs = "input In {\n  a: Int!\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["dep"]);
  let dep = package("dep", Layout::Flat, &[("dep.graphqlx", theirs)]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  let schema = schema_file(&analysis);
  let dep_file = file(&analysis, 4, "dep.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        at(schema, ours, "^::dep::In", 0),
        Some(named(dep_file, theirs, "input ", "In", 0))
      ),
      (ONE_OF_EXTENSION, at(schema, ours, "oneOf", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A refusal of `code` is silent in a contribution that also holds a phase's error, `together`,
/// and is reported beside it when the error is in a sibling definition, `apart`. The phase's error
/// is an undefined type.
fn silent_beside_a_phase(code: &str, together: &'static str, apart: &'static str) {
  silent_beside(UNDEFINED, code, together, apart);
}

/// A refusal of `code` is silent in a contribution that also holds the error `phase`, `together`,
/// and is reported beside it when the error is in a sibling definition, `apart`.
fn silent_beside(phase: &str, code: &str, together: &'static str, apart: &'static str) {
  let analysis = analyze_one(together);
  assert_eq!(
    codes(&analysis),
    [phase],
    "`{code}` beside a phase's error: {:#?}",
    analysis.diagnostics()
  );
  let analysis = analyze_one(apart);
  let mut reported = codes(&analysis);
  reported.sort_unstable();
  let mut expected = [code, phase];
  expected.sort_unstable();
  assert_eq!(
    reported,
    expected,
    "`{code}` apart from a phase's error: {:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_rule_of_this_part_says_nothing_where_a_phase_spoke() {
  silent_beside_a_phase(
    RESERVED_FIELD,
    "type Query {\n  ok: Int\n}\ntype T {\n  __f: Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype T {\n  __f: Int\n}\ntype U {\n  x: Missing\n}\n",
  );
  // A field's arguments are judged as the walk enters the field, by its contribution.
  silent_beside_a_phase(
    DUPLICATE_ARGUMENT,
    "type Query {\n  ok: Int\n}\ntype T {\n  f(a: Int, a: Int): Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype T {\n  f(a: Int, a: Int): Int\n}\ntype U {\n  x: Missing\n}\n",
  );
  // The later member's contribution silences it; the first's does not.
  silent_beside_a_phase(
    DUPLICATE_FIELD,
    "type Query {\n  ok: Int\n}\ntype T {\n  a: Int\n}\nextend type T {\n  a: Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype T {\n  a: Int\n  x: Missing\n}\nextend type T {\n  a: Int\n}\n",
  );
  // An empty type whose `implements` entry names nothing.
  silent_beside(
    UNDEFINED_INTERFACE,
    EMPTY_FIELDS,
    "type Query {\n  ok: Int\n}\ntype T implements Missing\n",
    "type Query {\n  ok: Int\n}\ntype T\ntype U implements Missing {\n  a: Int\n}\n",
  );
  silent_beside_a_phase(
    FIELD_TYPE,
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype T {\n  f: In\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype T {\n  f: In\n}\n\
     type U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    IMPLEMENTS,
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
     type T implements Obj {\n  id: ID\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ntype T implements Obj {\n  id: ID\n}\n\
     type U {\n  x: Missing\n}\n",
  );
  silent_beside(
    UNDEFINED_MEMBER,
    UNION_MEMBER,
    "type Query {\n  ok: Int\n}\ninterface Face {\n  f: Int\n}\nunion U = Face | Missing\n",
    "type Query {\n  ok: Int\n}\ninterface Face {\n  f: Int\n}\nunion U = Face\nunion V = Missing\n",
  );
  silent_beside_a_phase(
    DEPRECATED_ARGUMENT,
    "type Query {\n  ok: Int\n}\ntype T {\n  f(a: Int! @deprecated, x: Missing): Int\n}\n",
    "type Query {\n  ok: Int\n}\ntype T {\n  f(a: Int! @deprecated): Int\n}\ntype U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    ONE_OF_NON_NULL,
    "type Query {\n  ok: Int\n}\ninput In @oneOf {\n  a: Int!\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In @oneOf {\n  a: Int!\n}\ninput U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    ONE_OF_DEFAULT,
    "type Query {\n  ok: Int\n}\ninput In @oneOf {\n  a: Int = 1\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In @oneOf {\n  a: Int = 1\n}\ninput U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    ONE_OF_EXTENSION,
    "type Query {\n  ok: Int\n}\ninput In {\n  a: Int\n}\nextend input In @oneOf {\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In {\n  a: Int\n}\nextend input In @oneOf\n\
     input U {\n  x: Missing\n}\n",
  );
  // An error in an interface's own contribution silences it, and not the other interface of its
  // ring. `B` writes `A`'s field `x` too, so that the implementation phase finds nothing missing in
  // `B`'s own contribution: a type that names nothing is a subtype of any type, and has every type
  // as a subtype.
  let text = "type Query {\n  ok: Int\n}\ninterface A implements B {\n  id: ID\n  x: Missing\n}\n\
              interface B implements A {\n  id: ID\n  x: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, at(schema, text, "Missing", 0), None),
      (
        SELF_IMPLEMENTING,
        named(schema, text, "interface ", "B", 0),
        None
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn long_names_in_these_rules_are_quoted_as_excerpts() {
  let owner = "T".repeat(4096);
  let member = format!("__{}", "f".repeat(4096));
  let plain = "m".repeat(4096);
  let texts = [
    format!("type Query {{\n  ok: Int\n}}\ntype {owner} {{\n  {member}: Int\n}}\n"),
    format!("type Query {{\n  ok: Int\n}}\ntype {owner} {{\n  {plain}: Int\n  {plain}: Int\n}}\n"),
    format!("type Query {{\n  ok: Int\n}}\ntype {owner}\n"),
    format!(
      "type Query {{\n  ok: Int\n}}\ninput In {{\n  i: Int\n}}\ntype {owner} {{\n  {plain}: In\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ntype {plain} {{\n  o: Int\n}}\n\
       type {owner} implements {plain} {{\n  id: ID\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ninterface {owner} implements {owner} {{\n  id: ID\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ninterface {plain} {{\n  f: Int\n}}\nunion {owner} = {plain}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ntype {owner} {{\n  f({plain}: Int! @deprecated): Int\n}}\n"
    ),
    format!("type Query {{\n  ok: Int\n}}\ninput {owner} @oneOf {{\n  {plain}: Int!\n}}\n"),
    format!("type Query {{\n  ok: Int\n}}\ninput {owner} @oneOf {{\n  {plain}: Int = 1\n}}\n"),
    format!(
      "type Query {{\n  ok: Int\n}}\ninput {owner} {{\n  i: Int\n}}\nextend input {owner} @oneOf\n"
    ),
  ];
  for text in &texts {
    let analysis = analyze_text(text);
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}
