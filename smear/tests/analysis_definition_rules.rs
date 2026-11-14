//! The GraphQLx analyzer's rules on what a definition holds, each GraphQL's own with GraphQL's
//! code: a member's name does not begin with two underscores; a definition's members have distinct
//! names, and so have a field's arguments; a definition that holds members holds one at least; a
//! member's type is of the kind its position takes, and so are a collection's arguments there and
//! what each instantiation gives a type parameter where it stands; an object or an interface
//! implements
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
    Tier, analyze, analyze_with,
  },
  diagnostic::{Diagnose, Location},
  lexer::{limits::SyntacticLimits, tokora::SimpleSpan},
  parser::graphqlx::Syntactic,
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

/// Generic definitions whose type parameters stand in each place: directly, through a list,
/// through another definition, in an extension's field, from a default, and in a directive's
/// argument; then instantiations of each that give a parameter what its place does not take.
const INSTANTIATIONS: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ninput In {\n  i: Int\n}\n\
                              input Filter<T> {\n  eq: T\n  any: [T!]\n}\n\
                              type Page<T> {\n  items: [T]\n  at(cursor: T): Int\n}\n\
                              type Outer<U> {\n  p: Page<U>\n}\ntype Def<T = In> {\n  d: T\n}\n\
                              directive @tagged<V>(value: V) on OBJECT\n\
                              interface Node<X> {\n  id: X\n}\n\
                              type Holds implements Node<In> {\n  id: In\n}\n\
                              scalar Pair<A, B = Obj> @map\ninput Box<T> {\n  v: T\n}\n\
                              type Uses @tagged<Obj> {\n  f(where: Filter<Obj>): Int\n  \
                              p: Page<In>\n  q: Page<Obj>\n  o: Outer<In>\n  x: Def\n  \
                              g(b: Box<Pair<string>>): Int\n}\n";

/// Instantiations of the same definitions that give each parameter what its places take, or that
/// leave a parameter that stands nowhere to its default; and a parameter given to another
/// definition, inside a generic definition nothing instantiates.
const FITTING: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ninput In {\n  i: Int\n}\n\
                       input Filter<T> {\n  eq: T\n}\ntype Page<T> {\n  items: [T]\n  at(cursor: T): Int\n}\n\
                       type Edge<N> {\n  node: N\n}\ntype Conn<N, E = Edge<N>> {\n  edges: [E]\n}\n\
                       type G<T> {\n  f(w: Filter<T>): Int\n}\nscalar Ref<R>\n\
                       directive @tagged<V>(value: V) on OBJECT\ntype R<T = In> {\n  r: Int\n}\n\
                       type Uses @tagged<Int> {\n  a(f: Filter<Int>): Int\n  b(f: Filter<In>): Int\n  \
                       c: Page<Int>\n  d: Conn<In, Obj>\n  e(r: Ref<Obj>): Int\n  r: Ref<In>\n  s: R\n}\n";

#[test]
fn an_instantiations_arguments_fit_where_its_parameters_stand() {
  let analysis = analyze_one(INSTANTIATIONS);
  let schema = schema_file(&analysis);
  let text = INSTANTIATIONS;
  assert_eq!(
    found(&analysis),
    [
      // An `implements` entry is an instantiation; the field that implements it is the base
      // check's, each true.
      (
        FIELD_TYPE,
        named(schema, text, "implements Node<", "In", 0),
        Some(named(schema, text, "id: ", "X", 0))
      ),
      (FIELD_TYPE, named(schema, text, "id: ", "In", 0), None),
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "@tagged<", "Obj", 0),
        Some(named(schema, text, "value: ", "V", 0))
      ),
      // Labelled where the parameter first stands, the field `eq`.
      (
        INPUT_FIELD_TYPE,
        named(schema, text, "Filter<", "Obj", 0),
        Some(named(schema, text, "eq: ", "T", 0))
      ),
      // A field's type comes before a field argument's.
      (
        FIELD_TYPE,
        named(schema, text, "p: Page<", "In", 0),
        Some(at(schema, text, "[T]", 0))
      ),
      (
        ARGUMENT_TYPE,
        named(schema, text, "q: Page<", "Obj", 0),
        Some(named(schema, text, "cursor: ", "T", 0))
      ),
      // `Outer`'s parameter stands where `Page`'s does, passed on at `Page<U>`.
      (
        FIELD_TYPE,
        named(schema, text, "o: Outer<", "In", 0),
        Some(at(schema, text, "Page<U>", 0))
      ),
      // A default fills it: at the occurrence.
      (
        FIELD_TYPE,
        named(schema, text, "x: ", "Def", 0),
        Some(named(schema, text, "d: ", "T", 0))
      ),
      // A collection a default completes: at the deepest written occurrence.
      (
        INPUT_FIELD_TYPE,
        at(schema, text, "Pair<string>", 0),
        Some(named(schema, text, "v: ", "T", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert_eq!(
    messages[2],
    "`@tagged` takes the object type `Obj` for its type parameter `V`, which stands as a \
     directive argument's type, and a directive's argument's type is an input type"
  );
  assert_eq!(
    messages[3],
    "`Filter` takes the object type `Obj` for its type parameter `T`, which stands as an input \
     field's type, and an input field's type is an input type"
  );
  assert_eq!(
    messages[6],
    "`Outer` takes the input object `In` for its type parameter `U`, which stands as a field's \
     type, and a field's type is an output type"
  );
  assert_eq!(
    messages[7],
    "`Def` takes, from the default of `T`, the input object `In` for its type parameter `T`, \
     which stands as a field's type, and a field's type is an output type"
  );
  assert_eq!(
    messages[8],
    "`Box` takes, for its type parameter `T`, which stands as an input field's type, a \
     collection whose argument is the object type `Obj`, and an input field's type is an input \
     type"
  );
  // The labels after the first: where the parameter stands, then the default that fills it.
  let labels: Vec<Vec<(Location, &str)>> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (0..diagnostic.labels())
        .filter_map(|index: usize| diagnostic.label(index))
        .map(|label| (label.location(), label.text()))
        .collect()
    })
    .collect();
  assert_eq!(
    labels[7],
    [
      (named(schema, text, "d: ", "T", 0), "`T` stands here"),
      (
        named(schema, text, "T = ", "In", 0),
        "filled from this default"
      ),
    ]
  );
  assert_eq!(
    labels[8],
    [
      (named(schema, text, "v: ", "T", 0), "`T` stands here"),
      (
        named(schema, text, "B = ", "Obj", 0),
        "filled from this default"
      ),
    ]
  );
  // GraphQL's phrase and help, as the base check gives them for the place's code.
  let outer = analysis
    .diagnostics()
    .iter()
    .nth(6)
    .expect("seven diagnostics");
  assert_eq!(
    (outer.primary_label(), outer.help()),
    (
      Some("field type is not an output type"),
      Some(
        "a field's type must be a scalar, object, interface, union or enum; an input object \
         appears only in argument and input-field positions."
      )
    )
  );

  let analysis = analyze_one(FITTING);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn what_a_default_derives_is_judged_where_it_is_derived() {
  // `Conn<In>` is `Conn<In, Edge<In>>`, whose `Edge<In>` no occurrence writes: it is judged at
  // each occurrence that derives it, a type's fill judged by the type and its count of written
  // arguments alone, so `Other<In>`, which derives the same type, is refused too.
  let text = "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
              type Conn<N, E = Edge<N>> {\n  edges: [E]\n}\ntype Other<N, F = Edge<N>> {\n  f: F\n}\n\
              type Uses {\n  a: Conn<In>\n  b: Conn<In>\n  c: Other<In>\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  let node = Some(named(schema, text, "node: ", "N", 0));
  assert_eq!(
    found(&analysis),
    [
      (FIELD_TYPE, at(schema, text, "Conn<In>", 0), node),
      (FIELD_TYPE, at(schema, text, "Conn<In>", 1), node),
      (FIELD_TYPE, at(schema, text, "Other<In>", 0), node),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert_eq!(
    messages[0],
    "the default of `E` of `Conn` makes `Edge` take the input object `In` for its type parameter \
     `N`, which stands as a field's type, and a field's type is an output type"
  );
  assert_eq!(
    messages[2],
    "the default of `F` of `Other` makes `Edge` take the input object `In` for its type \
     parameter `N`, which stands as a field's type, and a field's type is an output type"
  );
  let defaults: Vec<Option<Location>> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.label(1).map(|label| label.location()))
    .collect();
  assert_eq!(
    defaults,
    [
      Some(named(schema, text, "E = ", "Edge<N>", 0)),
      Some(named(schema, text, "E = ", "Edge<N>", 0)),
      Some(named(schema, text, "F = ", "Edge<N>", 0)),
    ]
  );

  // A default that is written closed is an occurrence: judged where it is written, once, and not
  // at each use, whichever of the use and the default the walk reaches first.
  for text in [
    "type Query {\n  u: Uses\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
     type Uses {\n  a: Conn2<Int>\n  b: Conn2<Int>\n}\ntype Conn2<N, E = Edge<In>> {\n  e: [E]\n  \
     n: N\n}\n",
    "type Query {\n  u: Uses\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
     type Conn2<N, E = Edge<In>> {\n  e: [E]\n  n: N\n}\ntype Uses {\n  a: Conn2<Int>\n  \
     b: Conn2<Int>\n}\n",
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(
        FIELD_TYPE,
        named(schema, text, "Edge<", "In", 0),
        Some(named(schema, text, "node: ", "N", 0))
      )],
      "{:#?}",
      analysis.diagnostics()
    );
  }

  // A slot the occurrence writes is judged where it is written, and the default never derives
  // what the occurrence writes; nor is a type a field writes judged again where a fill derives it.
  for (text, at_in) in [
    (
      "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
       type Conn<N, E = Edge<N>> {\n  edges: [E]\n}\ntype Uses {\n  a: Conn<In, Edge<In>>\n}\n",
      "a: Conn<In, Edge<",
    ),
    (
      "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
       type Conn<N, E = Edge<N>> {\n  edges: [E]\n}\ntype Uses {\n  a: Conn<In>\n  e: Edge<In>\n}\n",
      "e: Edge<",
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(
        FIELD_TYPE,
        named(schema, text, at_in, "In", 0),
        Some(named(schema, text, "node: ", "N", 0))
      )],
      "{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn an_application_is_an_instantiation_of_its_directive() {
  let text = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
              directive @tagged<V>(value: V) on OBJECT\ndirective @plain<V> on OBJECT\n\
              directive @dd<V = Obj>(value: V) on OBJECT\n\
              type A @tagged<Obj> {\n  a: Int\n}\ntype B @tagged<Int> @plain<Obj> {\n  b: Int\n}\n\
              type C @dd {\n  c: Int\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "@tagged<", "Obj", 0),
        Some(named(schema, text, "value: ", "V", 0))
      ),
      // A default fills it: at the application's path.
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "C @", "dd", 0),
        Some(named(schema, text, "value: ", "V", 1))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[1],
    "`@dd` takes, from the default of `V`, the object type `Obj` for its type parameter `V`, \
     which stands as a directive argument's type, and a directive's argument's type is an input \
     type"
  );
  let filled = analysis
    .diagnostics()
    .iter()
    .nth(1)
    .expect("two diagnostics");
  assert_eq!(
    filled
      .label(1)
      .map(|label| (label.location(), label.text())),
    Some((
      named(schema, text, "V = ", "Obj", 0),
      "filled from this default"
    ))
  );
}

#[test]
fn a_collections_own_parameters_stand_in_what_it_is_given() {
  // `@d<V>` puts the map `M`'s value parameter in a directive's argument, so `M<Int, Obj>` is
  // refused at `Obj`, labelled at that `V`. `Box`'s `T` is given to that parameter in `v`, so it
  // stands there too, beside the field's type `v` makes it through the map's value: `Box<Obj>` is
  // refused under the same code, labelled at `M<Int, T>`. A collection that only held its
  // arguments, and passed none of its own parameters' places to them, accepted `Box<Obj>`.
  let text = "type Query {\n  b: Box<Obj>\n  m: M<Int, Obj>\n}\ntype Obj {\n  x: Int\n}\n\
              directive @d<X>(v: [X]) on SCALAR\nscalar M<K, V> @map @d<V>\n\
              type Box<T> {\n  v: M<Int, T>\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "Box<", "Obj", 0),
        Some(at(schema, text, "M<Int, T>", 0))
      ),
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "m: M<Int, ", "Obj", 0),
        Some(named(schema, text, "@d<", "V", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Box` takes the object type `Obj` for its type parameter `T`, which stands as a directive \
       argument's type, and a directive's argument's type is an input type",
      "`M` takes the object type `Obj` for its type parameter `V`, which stands as a directive \
       argument's type, and a directive's argument's type is an input type",
    ]
  );
  let labels: Vec<Vec<(Location, &str)>> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (0..diagnostic.labels())
        .filter_map(|index: usize| diagnostic.label(index))
        .map(|label| (label.location(), label.text()))
        .collect()
    })
    .collect();
  assert_eq!(
    labels,
    [
      vec![(at(schema, text, "M<Int, T>", 0), "`T` stands here")],
      vec![(named(schema, text, "@d<", "V", 0), "`V` stands here")],
    ]
  );

  // The twin, an input object where the object was: `Box` is an input object and both are taken
  // as field arguments, so every place each parameter stands in takes an input type, and the
  // input object passes both ways.
  let text = "type Query {\n  b(a: Box<In>): Int\n  m(a: M<Int, In>): Int\n}\n\
              input In {\n  x: Int\n}\ndirective @d<X>(v: [X]) on SCALAR\n\
              scalar M<K, V> @map @d<V>\ninput Box<T> {\n  v: M<Int, T>\n}\n";
  let analysis = analyze_text(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_scalars_own_parameters_stand_in_what_it_is_given() {
  // `@d<T>` puts the scalar `Ref`'s parameter in a directive's argument, so `Ref<Obj>` is refused
  // at `Obj`, labelled at that `T`. `Box`'s `T` is given to that parameter in `v`, so it stands
  // there too, and `Box<Obj>`, which holds a `Ref<Obj>`, is refused under the same code, labelled
  // at `Ref<T>`; and so is `Y<Obj>`, whose default derives a `Ref<Obj>`. A scalar that hid its
  // arguments accepted `Box<Obj>`.
  let text = "type Query {\n  r: Ref<Obj>\n  b: Box<Obj>\n}\ntype Obj {\n  x: Int\n}\n\
              directive @d<X>(v: [X]) on SCALAR\nscalar Ref<T> @d<T>\n\
              type Box<T> {\n  v: Ref<T>\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "Ref<", "Obj", 0),
        Some(named(schema, text, "@d<", "T", 0))
      ),
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "Box<", "Obj", 0),
        Some(named(schema, text, "v: ", "Ref<T>", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Ref` takes the object type `Obj` for its type parameter `T`, which stands as a directive \
       argument's type, and a directive's argument's type is an input type",
      "`Box` takes the object type `Obj` for its type parameter `T`, which stands as a directive \
       argument's type, and a directive's argument's type is an input type",
    ]
  );

  // Derived from a default, alone: a `Ref<Obj>` the program writes would be judged where it is.
  let text = "type Query {\n  y: Y<Obj>\n}\ntype Obj {\n  x: Int\n}\n\
              directive @d<X>(v: [X]) on SCALAR\nscalar Ref<T> @d<T>\n\
              type Y<A, R = Ref<A>> {\n  r: R\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DIRECTIVE_ARGUMENT_TYPE,
      at(schema, text, "Y<Obj>", 0),
      Some(named(schema, text, "@d<", "T", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the default of `R` of `Y` makes `Ref` take the object type `Obj` for its type parameter \
       `T`, which stands as a directive argument's type, and a directive's argument's type is an \
       input type"
    ]
  );

  // The twin, an input object where the object was, passes every form: a value of a scalar that
  // is no collection holds nothing, so `Box`'s `T` stands in no field's type.
  let text = "type Query {\n  r: Ref<In>\n  b: Box<In>\n  y: Y<In>\n}\ninput In {\n  i: Int\n}\n\
              directive @d<X>(v: [X]) on SCALAR\nscalar Ref<T> @d<T>\n\
              type Box<T> {\n  v: Ref<T>\n}\ntype Y<A, R = Ref<A>> {\n  r: R\n}\n";
  let analysis = analyze_text(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn what_a_scalars_argument_holds_passes_its_parameters_on() {
  // `Page`'s `T` stands as a field's type, so `Page<In>` is refused in each of its forms, each
  // alone: written under `Ref`'s argument, at `In`; substituted, as the `Ref<Page<In>>` of
  // `Box<In>`, at `In`, labelled at `Ref<Page<T>>`, which puts `Box`'s `T` where `Page`'s stands;
  // and derived from `X`'s default, at `X<In>`. A scalar that hid its arguments accepted
  // `Box<In>`, and refused the other two.
  let definitions = "input In {\n  i: Int\n}\ntype Obj {\n  x: Int\n}\nscalar Ref<T>\n\
                     type Page<T> {\n  items: [T]\n}\ntype Box<T> {\n  v: Ref<Page<T>>\n}\n\
                     type X<A, R = Ref<Page<A>>> {\n  r: R\n}\n";
  let text = format!("type Query {{\n  a: Ref<Page<In>>\n}}\n{definitions}");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      FIELD_TYPE,
      named(schema, &text, "Page<", "In", 0),
      Some(at(schema, &text, "[T]", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Page` takes the input object `In` for its type parameter `T`, which stands as a field's \
       type, and a field's type is an output type"
    ]
  );

  let text = format!("type Query {{\n  b: Box<In>\n}}\n{definitions}");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      FIELD_TYPE,
      named(schema, &text, "Box<", "In", 0),
      Some(named(schema, &text, "v: ", "Ref<Page<T>>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Box` takes the input object `In` for its type parameter `T`, which stands as a field's \
       type, and a field's type is an output type"
    ]
  );

  let text = format!("type Query {{\n  c: X<In>\n}}\n{definitions}");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      FIELD_TYPE,
      at(schema, &text, "X<In>", 0),
      Some(at(schema, &text, "[T]", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the default of `R` of `X` makes `Page` take the input object `In` for its type parameter \
       `T`, which stands as a field's type, and a field's type is an output type"
    ]
  );

  // The twin, an object where the input object was, passes every form.
  let text =
    format!("type Query {{\n  a: Ref<Page<Obj>>\n  b: Box<Obj>\n  c: X<Obj>\n}}\n{definitions}");
  let analysis = analyze_text(&text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_implements_entry_passes_its_interfaces_parameters_on() {
  // `@d<X>` puts `Tagged`'s parameter in a directive's argument, so `Direct`'s entry `Tagged<Obj>`
  // is refused at `Obj`, labelled at that `X`. `Holds`'s `T` is given to it in `Holds`'s entry, so
  // `Holds<Obj>`, which implements `Tagged<Obj>`, is refused under the same code, labelled at the
  // entry. An implementing type declares each field itself, but not its interface's applications:
  // an entry that passed nothing on accepted `Holds<Obj>`.
  let text = "type Query {\n  h: Holds<Obj>\n  d: Direct\n}\ntype Obj {\n  x: Int\n}\n\
              directive @d<V>(v: V) on INTERFACE\ninterface Tagged<X> @d<X> {\n  a: Int\n}\n\
              type Holds<T> implements Tagged<T> {\n  a: Int\n}\n\
              type Direct implements Tagged<Obj> {\n  a: Int\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "Holds<", "Obj", 0),
        Some(at(schema, text, "Tagged<T>", 0))
      ),
      (
        DIRECTIVE_ARGUMENT_TYPE,
        named(schema, text, "Tagged<", "Obj", 0),
        Some(named(schema, text, "@d<", "X", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Holds` takes the object type `Obj` for its type parameter `T`, which stands as a directive \
       argument's type, and a directive's argument's type is an input type",
      "`Tagged` takes the object type `Obj` for its type parameter `X`, which stands as a \
       directive argument's type, and a directive's argument's type is an input type",
    ]
  );

  // Nor the `@deprecated` of its fields' arguments: `I<Int!>` makes `a` required, written in
  // `H2`'s entry and substituted in `H<Int!>`, whose own `f` deprecates nothing.
  let text = "type Query {\n  h: H<Int!>\n  h2: H2\n}\n\
              interface I<X> {\n  f(a: X @deprecated): Int\n}\n\
              type H<T> implements I<T> {\n  f(a: T): Int\n}\n\
              type H2 implements I<Int!> {\n  f(a: Int!): Int\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DEPRECATED_ARGUMENT,
        named(schema, text, "H<", "Int!", 0),
        Some(at(schema, text, "I<T>", 0))
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, text, "I<", "Int!", 0),
        Some(named(schema, text, "(a: ", "X", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`H` takes a non-null type for its type parameter `T`, which makes a deprecated argument \
       required",
      "`I` takes a non-null type for its type parameter `X`, which makes a deprecated argument \
       required",
    ]
  );

  // The twins pass both forms: an input object where the object was, and a nullable `Int`.
  for text in [
    "type Query {\n  h: Holds<In>\n  d: Direct\n}\ninput In {\n  i: Int\n}\n\
     directive @d<V>(v: V) on INTERFACE\ninterface Tagged<X> @d<X> {\n  a: Int\n}\n\
     type Holds<T> implements Tagged<T> {\n  a: Int\n}\n\
     type Direct implements Tagged<In> {\n  a: Int\n}\n",
    "type Query {\n  h: H<Int>\n  h2: H2\n}\ninterface I<X> {\n  f(a: X @deprecated): Int\n}\n\
     type H<T> implements I<T> {\n  f(a: T): Int\n}\n\
     type H2 implements I<Int> {\n  f(a: Int): Int\n}\n",
  ] {
    let analysis = analyze_text(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}\n{:#?}",
      analysis.diagnostics()
    );
  }
}

#[test]
fn a_type_given_to_many_parameters_is_one_node() {
  // `D.d`'s complete type is `H<Tup<T>, G1<Tup<T>>, G2<Tup<T>>>`: `Tup<T>` is given to three
  // slots, and is one node of the table with an edge from each, walked once. A test cannot see
  // the work, which the table bounds by construction; it pins what the table answers through it.
  let text = "type Query {\n  d: D<In>\n}\ninput In {\n  i: Int\n}\ntype Tup<A> {\n  a: A\n}\n\
              type G1<X> {\n  g: X\n}\ntype G2<X> {\n  g: X\n}\n\
              type H<A, B1 = G1<A>, B2 = G2<A>> {\n  h: Int\n}\ntype D<T> {\n  d: H<Tup<T>>\n}\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      FIELD_TYPE,
      named(schema, text, "D<", "In", 0),
      Some(at(schema, text, "H<Tup<T>>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_application_its_defaults_complete_is_entered_once() {
  // `@d` has many parameters its defaults fill, each from the first, and is applied as `@d<T>` to
  // each of many fields of `G`: every application's type is the one type `d<T, T, …, T>`, whose
  // filled arguments the table enters once, at the first application, which the last parameter's
  // place reaches `G`'s `T` through. `@e`, applied bare to each field too, has a closed type, which
  // gives no edge and is passed over at once. A test sees the diagnostics, not the work, which the
  // table bounds by construction: each written type argument once, and the filled ones once for
  // each type and count of written arguments, which the fill that typed them paid for. Read once
  // for each application, this program's 4,000 applications would each read 2,000 slots.
  const PARAMS: usize = 2_000;
  const FIELDS: usize = 2_000;
  let mut text =
    String::from("type Query {\n  g: G<Obj>\n}\ntype Obj {\n  o: Int\n}\ndirective @d<A");
  for index in 1..PARAMS {
    text.push_str(&format!(", B{index} = A"));
  }
  text.push_str(&format!(
    ">(b: B{}) on FIELD_DEFINITION\ndirective @e<C0 = Int",
    PARAMS - 1
  ));
  for index in 1..PARAMS {
    text.push_str(&format!(", C{index} = Int"));
  }
  text.push_str("> on FIELD_DEFINITION\ntype G<T> {\n");
  for index in 0..FIELDS {
    text.push_str(&format!("  f{index}: Int @d<T> @e\n"));
  }
  text.push_str("}\n");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DIRECTIVE_ARGUMENT_TYPE,
      named(schema, &text, "G<", "Obj", 0),
      Some(named(schema, &text, "f0: Int @", "d", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_parameter_far_down_a_long_list_is_named_in_one_step() {
  // `Wide`'s parameters are all defaulted, the last to an input object that its field takes, and
  // each of many fields leaves them all out: each is refused, naming the last parameter and
  // labelling its default. A parameter is named by reading its row, skipping none, so each report
  // costs the same however far down the list its parameter is: a test sees the diagnostics, not
  // the work, which holds by construction, `TypeParams::nth` reading the row it is asked for. Read
  // from the first parameter on, each of this program's 3,000 reports would read 3,000 rows twice.
  const PARAMS: usize = 3_000;
  const FIELDS: usize = 3_000;
  let mut text = String::from("type Query {\n  u: Uses\n}\ninput In {\n  i: Int\n}\ntype Wide<");
  for index in 0..PARAMS {
    text.push_str(&format!("T{index} = Int, "));
  }
  let default = text.len() + format!("T{PARAMS} = ").len();
  text.push_str(&format!("T{PARAMS} = In> {{\n  f: "));
  let stands = text.len();
  text.push_str(&format!("T{PARAMS}\n}}\ntype Uses {{\n"));
  let mut uses: Vec<usize> = Vec::with_capacity(FIELDS);
  for index in 0..FIELDS {
    let field = format!("  u{index}: ");
    uses.push(text.len() + field.len());
    text.push_str(&field);
    text.push_str("Wide\n");
  }
  text.push_str("}\n");
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  let span = |start: usize, len: usize| -> Location {
    Location::new(schema.get(), SimpleSpan::new(start, start + len))
  };
  let last = format!("T{PARAMS}");
  let expected: Vec<(&'static str, Location, Option<Location>)> = uses
    .iter()
    .map(|start: &usize| {
      (
        FIELD_TYPE,
        span(*start, "Wide".len()),
        Some(span(stands, last.len())),
      )
    })
    .collect();
  assert_eq!(found(&analysis), expected);
  let first = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("a diagnostic for each field");
  assert_eq!(
    first.to_string(),
    format!(
      "`Wide` takes, from the default of `{last}`, the input object `In` for its type parameter \
       `{last}`, which stands as a field's type, and a field's type is an output type"
    )
  );
  assert_eq!(
    first.label(1).map(|label| (label.location(), label.text())),
    Some((span(default, "In".len()), "filled from this default"))
  );
}

#[test]
fn a_type_many_fills_derive_is_walked_once() {
  // Each `Outer<Si>` leaves `T`, `R`, `X` and `B` to their defaults. `R` derives `Maker<Int>`,
  // whose own default derives `Wide<Int, …, Int>`, and `B` derives `Page<In>`, which no occurrence
  // writes and which takes an input object where its parameter stands as a field's type: each use
  // is refused, labelled at `item: T` and at `B`'s default. Every use's fill derives the one
  // `Maker<Int>`, which the instantiation phase typed once, and the walk below a derived type is
  // made once, whichever use asks: each use reads one kept answer for each slot it leaves out. The
  // depth is constant; the width of `Wide` and the count of uses vary. A test sees the
  // diagnostics, not the work, which holds by construction: a derived type's answer is kept by its
  // id, the same whichever fill derives it, and each type is expanded once, its arguments read at
  // most twice. Walked again for each use, the largest program here would read `Wide`'s 3,000
  // arguments for each of its 3,000 uses, in each of the four rules that judge what a fill
  // derives: 36 million steps, where each rule now makes a few for each use and for each of
  // `Wide`'s arguments.
  const WIDTH: usize = 3_000;
  const USES: usize = 3_000;
  for (width, uses) in [(1, 1), (WIDTH, 1), (1, USES), (WIDTH, USES)] {
    let mut text = String::from(
      "type Query {\n  u: Uses\n}\ninput In {\n  i: Int\n}\ntype Page<T> {\n  item: T\n}\n\
       scalar Wide<P0",
    );
    for index in 1..width {
      text.push_str(&format!(", P{index}"));
    }
    text.push_str(">\nscalar Maker<T, R = Wide<T");
    for _ in 1..width {
      text.push_str(", T");
    }
    text.push_str(">>\nscalar Outer<U, T = Int, R = Maker<T>, X = In, B = ");
    let default = text.len();
    text.push_str("Page<X>>\n");
    for index in 0..uses {
      text.push_str(&format!("scalar S{index}\n"));
    }
    text.push_str("type Uses {\n");
    let mut written: Vec<(usize, usize)> = Vec::with_capacity(uses);
    for index in 0..uses {
      let field = format!("  u{index}: ");
      let ty = format!("Outer<S{index}>");
      written.push((text.len() + field.len(), ty.len()));
      text.push_str(&field);
      text.push_str(&ty);
      text.push('\n');
    }
    text.push_str("}\n");
    let analysis = analyze_text(&text);
    let schema = schema_file(&analysis);
    let span = |start: usize, len: usize| -> Location {
      Location::new(schema.get(), SimpleSpan::new(start, start + len))
    };
    let item = Some(named(schema, &text, "item: ", "T", 0));
    let expected: Vec<(&'static str, Location, Option<Location>)> = written
      .iter()
      .map(|&(start, len): &(usize, usize)| (FIELD_TYPE, span(start, len), item))
      .collect();
    assert_eq!(found(&analysis), expected, "width {width}, uses {uses}");
    let first = analysis
      .diagnostics()
      .iter()
      .next()
      .expect("a diagnostic for each use");
    assert_eq!(
      first.to_string(),
      "the default of `B` of `Outer` makes `Page` take the input object `In` for its type \
       parameter `T`, which stands as a field's type, and a field's type is an output type"
    );
    assert_eq!(
      first.label(1).map(|label| (label.location(), label.text())),
      Some((span(default, "Page<X>".len()), "filled from this default"))
    );
  }
}

#[test]
fn a_report_never_walks_a_path_defaults_fill() {
  // Each field of `Query` is typed by the last set of a chain, each set's element its default, the
  // set before it, down to `scalar C0<T = Bad> @set`: the field's complete type holds the input
  // object `Bad` at the chain's end, so each field is refused, at its occurrence, labelled at the
  // default of the last set's `T`. The occurrence writes no type argument, so the way down to `Bad`
  // leaves what it writes at once, and the rest of the way is what defaults fill: its end is kept
  // for each set when the chain is judged, once for the walk, and each report reads it in one
  // lookup. The depth and the count of fields vary, and each program runs again under a diagnostic
  // ceiling of zero, which keeps no report and still builds and counts each. A test sees the
  // diagnostics, not the work, which holds by construction: a report follows the levels its
  // occurrence writes alone, and reads the end kept for the type where the occurrence stops
  // writing the way. Walked again for each report, the largest program here would follow its 3,000
  // sets for each of its 3,000 fields, 9 million steps, where the rule now takes a few for each set
  // and for each field.
  const DEPTH: usize = 3_000;
  const FIELDS: usize = 3_000;
  // The last set is as deep as the chain, past the default bound.
  let limits = Limits::new()
    .with_max_generic_depth(u32::try_from(DEPTH + 1).expect("the chain's depth is a u32"));
  for (depth, fields) in [(1, 1), (DEPTH, 1), (1, FIELDS), (DEPTH, FIELDS)] {
    let last = format!("C{}", depth - 1);
    let mut text = String::from("type Query {\n");
    let mut written: Vec<usize> = Vec::with_capacity(fields);
    for index in 0..fields {
      let field = format!("  f{index}: ");
      written.push(text.len() + field.len());
      text.push_str(&field);
      text.push_str(&last);
      text.push('\n');
    }
    text.push_str("}\ninput Bad {\n  x: Int\n}\n");
    // Where the last set's default is written, and its length.
    let mut default = (0, 0);
    for index in 0..depth {
      let element = match index {
        0 => String::from("Bad"),
        _ => format!("C{}", index - 1),
      };
      let head = format!("scalar C{index}<T = ");
      default = (text.len() + head.len(), element.len());
      text.push_str(&head);
      text.push_str(&element);
      text.push_str("> @set\n");
    }
    let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
    let analysis = analyze(&root, &[], &limits);
    let schema = schema_file(&analysis);
    let span = |start: usize, len: usize| -> Location {
      Location::new(schema.get(), SimpleSpan::new(start, start + len))
    };
    let filled = Some(span(default.0, default.1));
    let expected: Vec<(&'static str, Location, Option<Location>)> = written
      .iter()
      .map(|start: &usize| (FIELD_TYPE, span(*start, last.len()), filled))
      .collect();
    assert_eq!(found(&analysis), expected, "depth {depth}, fields {fields}");
    let first = analysis
      .diagnostics()
      .iter()
      .next()
      .expect("a diagnostic for each field");
    assert_eq!(
      first.to_string(),
      format!(
        "the field `f0` of `Query` is typed by the collection `{last}`, whose argument is the \
         input object `Bad`, and a field's type is an output type"
      )
    );
    assert_eq!(
      (
        first.primary_label(),
        first.labels(),
        first.label(0).map(|label| label.text()),
        first.help()
      ),
      (
        Some("field type is not an output type"),
        1,
        Some("filled from this default"),
        Some(
          "a field's type must be a scalar, object, interface, union or enum; an input object \
           appears only in argument and input-field positions."
        )
      )
    );
    // Under a ceiling of zero, each report is built and counted, and none is kept.
    let silent = analyze(&root, &[], &limits.with_max_diagnostics(0));
    let diagnostics = silent.diagnostics();
    assert!(diagnostics.is_empty(), "depth {depth}, fields {fields}");
    assert!(diagnostics.truncated());
    assert_eq!(diagnostics.error_count(), fields);
  }
}

/// The stack [`on_a_deep_stack`] runs on: 512 MiB, past four times what the parser's recursion
/// takes in a debug build for a type nested 3,000 deep, at about 40 KiB for each level.
const DEEP_STACK: usize = 512 << 20;

/// Runs `body` on a thread with room for the parser's own recursion, which descends a level for
/// each bracket a text opens and drops its tree the same way: neither is what these tests are
/// about.
fn on_a_deep_stack(body: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(DEEP_STACK)
    .spawn(body)
    .expect("the thread spawns")
    .join()
    .expect("the body does not panic");
}

#[test]
fn a_written_tree_many_reports_nest_in_is_read_once() {
  // `@d<V>` puts the map `M`'s value parameter in a directive's argument, and `Query.f` is typed
  // by `M<Int, …>` nested `depth` deep around the object `Obj`: each occurrence of `M` gives `V`
  // `Obj`, or a map that holds it, so each is refused, at `Obj`, the deepest occurrence that
  // writes the way to it, labelled where `V` stands. The depth varies, and each program runs again
  // under a diagnostic ceiling of zero, which keeps no report and still builds and counts each. A
  // test sees the diagnostics, not the work, which holds by construction: each written occurrence
  // is answered once for the walk, in one step from the answer of the child it writes on the way,
  // and each report reads the one answer kept for its argument. Walked again for each report, the
  // deepest program here would follow what each occurrence writes below it for each of its 3,000
  // reports, 4.5 million steps, where the rule now takes a few for each occurrence. The parser
  // descends its own recursion a level for each `<`, so the text is read under a raised nesting
  // ceiling, on a thread with room for that.
  const DEPTH: usize = 3_000;
  on_a_deep_stack(|| {
    // The outermost map is as deep as the nesting, past the default bound.
    let limits = Limits::new()
      .with_max_generic_depth(u32::try_from(DEPTH + 1).expect("the nesting's depth is a u32"));
    // `type Query {` opens a bracket, and so does each `<`.
    let syntactic = Syntactic::with_limits(SyntacticLimits::with_max_nesting_depth(DEPTH + 2));
    let flat = "`M` takes the object type `Obj` for its type parameter `V`, which stands as a \
                directive argument's type, and a directive's argument's type is an input type";
    let held = "`M` takes, for its type parameter `V`, which stands as a directive argument's \
                type, a collection whose argument is the object type `Obj`, and a directive's \
                argument's type is an input type";
    for depth in [1, 2, DEPTH] {
      let text = format!(
        "type Query {{\n  f: {}Obj{}\n}}\ntype Obj {{\n  x: Int\n}}\n\
         directive @d<X>(v: [X]) on SCALAR\nscalar M<K, V> @map @d<V>\n",
        "M<Int, ".repeat(depth),
        ">".repeat(depth)
      );
      let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
        .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
      let (analysis, _) = analyze_with(&syntactic, &root, &[], &limits);
      let schema = schema_file(&analysis);
      let leaf = named(schema, &text, "M<Int, ", "Obj", 0);
      let stands = named(schema, &text, "@d<", "V", 0);
      assert_eq!(
        found(&analysis),
        vec![(DIRECTIVE_ARGUMENT_TYPE, leaf, Some(stands)); depth],
        "depth {depth}"
      );
      // The innermost map takes `Obj` itself, and each other one a map that holds it; equal but
      // for their messages, they are in the order of those.
      let mut expected = vec![String::from(flat)];
      expected.resize(depth, String::from(held));
      assert_eq!(messages(&analysis), expected, "depth {depth}");
      let first = analysis
        .diagnostics()
        .iter()
        .next()
        .expect("a diagnostic for each map");
      assert_eq!(
        (
          first.primary_label(),
          first.label(0).map(|label| label.text()),
          first.help()
        ),
        (
          Some("directive argument type is not an input type"),
          Some("`V` stands here"),
          Some("a directive argument's type must be a scalar, enum or input object.")
        )
      );
      assert!(
        analysis
          .diagnostics()
          .iter()
          .all(|diagnostic: &Diagnostic| diagnostic.labels() == 1),
        "depth {depth}"
      );
      // Under a ceiling of zero, each report is built and counted, and none is kept.
      let (silent, _) = analyze_with(&syntactic, &root, &[], &limits.with_max_diagnostics(0));
      let diagnostics = silent.diagnostics();
      assert!(diagnostics.is_empty(), "depth {depth}");
      assert!(diagnostics.truncated());
      assert_eq!(diagnostics.error_count(), depth);
    }
  });
}

#[test]
fn instantiations_are_judged_across_files_modules_and_packages() {
  // An extension in another file uses a generic type of a module; an extension in a third file
  // adds a field argument to it; a dependency's generic input object is used by the root package.
  const B: &str = "extend type Uses {\n  p: ~::m::Page<In>\n}\n";
  const MORE: &str = "extend type Page<U> {\n  at(cursor: U): Int\n}\n";
  const PAGE: &str = "type Page<T> {\n  items: [T]\n}\n";
  const SCHEMA: &str = "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\ninput In {\n  i: Int\n}\n\
                        type Uses {\n  q: ~::m::Page<Obj>\n  f(w: ^::geo::Filter<Obj>): Int\n}\n";
  const GEO: &str = "input Filter<T> {\n  eq: T\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("b.graphqlx", B),
      ("m/more.graphqlx", MORE),
      ("m/page.graphqlx", PAGE),
      ("schema.graphqlx", SCHEMA),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", GEO)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let b = file(&analysis, 3, "b.graphqlx");
  let more = file(&analysis, 3, "m/more.graphqlx");
  let page = file(&analysis, 3, "m/page.graphqlx");
  let schema = file(&analysis, 3, "schema.graphqlx");
  let geo = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      // In the extension's file; where `Page`'s parameter stands, in its own file.
      (
        FIELD_TYPE,
        named(b, B, "Page<", "In", 0),
        Some(at(page, PAGE, "[T]", 0))
      ),
      // The argument another file's extension adds.
      (
        ARGUMENT_TYPE,
        named(schema, SCHEMA, "m::Page<", "Obj", 0),
        Some(named(more, MORE, "cursor: ", "U", 0))
      ),
      // In the root package's source, labelled in the dependency's.
      (
        INPUT_FIELD_TYPE,
        named(schema, SCHEMA, "Filter<", "Obj", 0),
        Some(named(geo, GEO, "eq: ", "T", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
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

/// Deprecated members with no default typed by their definition's type parameter, written
/// nullable: an input field, a field's argument and a directive's argument; one with a default;
/// and instantiations that give each a non-null type, or a nullable one.
const DEPRECATED_INSTANTIATIONS: &str = "type Query {\n  ok: Int\n}\n\
                                         input I<T> {\n  a: T @deprecated\n  b: T = 1 @deprecated\n}\n\
                                         type O<T> {\n  f(a: T @deprecated): Int\n}\n\
                                         directive @d<T>(a: T @deprecated) on OBJECT\n\
                                         type Uses @d<Int!>(a: 1) {\n  g(i: I<Int!>): Int\n  \
                                         h(j: I<Int>): Int\n  o: O<Int!>\n}\n";

#[test]
fn what_an_instantiation_makes_required_is_not_deprecated() {
  let analysis = analyze_one(DEPRECATED_INSTANTIATIONS);
  let schema = schema_file(&analysis);
  let text = DEPRECATED_INSTANTIATIONS;
  assert_eq!(
    found(&analysis),
    [
      (
        DEPRECATED_ARGUMENT,
        named(schema, text, "@d<", "Int!", 0),
        Some(named(schema, text, "(a: ", "T", 1))
      ),
      // `b` has a default: the input field `a` is the one `I<Int!>` makes required.
      (
        DEPRECATED_INPUT_FIELD,
        named(schema, text, "I<", "Int!", 0),
        Some(named(schema, text, "  a: ", "T", 0))
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, text, "O<", "Int!", 0),
        Some(named(schema, text, "(a: ", "T", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`@d` takes a non-null type for its type parameter `T`, which makes a deprecated argument \
       required",
      "`I` takes a non-null type for its type parameter `T`, which makes a deprecated input field \
       required",
      "`O` takes a non-null type for its type parameter `T`, which makes a deprecated argument \
       required",
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
        Some("required argument is deprecated"),
        Some(
          "to deprecate a required argument, first make it optional — give it a default, or drop \
           the `!`."
        )
      ),
      (
        Some("required input field is deprecated"),
        Some(
          "to deprecate a required input field, first make it optional — give it a default, or \
           drop the `!`."
        )
      ),
    ]
  );
}

/// A set whose element, and a map whose key, `@d` makes a deprecated argument's whole type,
/// beside a map whose value it does; and uses of each, one through a type parameter.
const FORCED: &str = "type Query {\n  s: S<Int>\n  t: S<Int!>\n  u: Box<Int!>\n  \
                      m: M<Int, Int>\n  n: N<Int, Int>\n  o: N<Int, Int!>\n}\n\
                      directive @d<X>(v: X @deprecated) on SCALAR\nscalar S<T> @set @d<T>\n\
                      scalar M<K, V> @map @d<K>\nscalar N<K, V> @map @d<V>\n\
                      type Box<T> {\n  v: S<T>\n}\n";

#[test]
fn a_collections_key_a_deprecated_member_takes_is_refused_once() {
  // A set's element and a map's key are non-null in every instantiation, however they are
  // written, so `S`'s `@d<T>` and `M`'s `@d<K>` make `v` required whatever `S` and `M` are given:
  // each definition's mistake, reported once, where it gives the place, labelled at the
  // `@deprecated`. No use repeats it, `S<Int>`, `S<Int!>`, `Box<Int!>`, which gives `S` its `T`,
  // and `M<Int, Int>` among them; `N`'s value is no key, and `N<Int, Int!>` alone is refused.
  let analysis = analyze_one(FORCED);
  let schema = schema_file(&analysis);
  let deprecated = Some(at(schema, FORCED, "deprecated", 0));
  assert_eq!(
    found(&analysis),
    [
      (
        DEPRECATED_ARGUMENT,
        named(schema, FORCED, "N<Int, ", "Int!", 0),
        Some(named(schema, FORCED, "@d<", "V", 0))
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, FORCED, "@d<", "T", 0),
        deprecated
      ),
      (
        DEPRECATED_ARGUMENT,
        named(schema, FORCED, "@d<", "K", 0),
        deprecated
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`N` takes a non-null type for its type parameter `V`, which makes a deprecated argument \
       required",
      "`T`, the element of the set `S`, is non-null in every instantiation, and here it makes the \
       deprecated argument `v` of the directive `@d` required",
      "`K`, the key of the map `M`, is non-null in every instantiation, and here it makes the \
       deprecated argument `v` of the directive `@d` required",
    ]
  );
  let forced = analysis
    .diagnostics()
    .iter()
    .nth(1)
    .expect("three diagnostics");
  assert_eq!(
    (forced.primary_label(), forced.labels(), forced.help()),
    (
      Some("required argument is deprecated"),
      1,
      Some(
        "a set's element and a map's key are non-null however they are written: give the \
         deprecated member a default, or do not pass the key or element to it."
      )
    )
  );

  // An input field, through the input object `W` the directive's argument names: the element is
  // reported where `@e<T>` gives it, under the input field's code.
  let text = "type Query {\n  s: S<Int>\n}\ninput W<Y> {\n  w: Y @deprecated\n}\n\
              directive @e<X>(v: W<X>) on SCALAR\nscalar S<T> @set @e<T>\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEPRECATED_INPUT_FIELD,
      named(schema, text, "@e<", "T", 0),
      Some(at(schema, text, "deprecated", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`T`, the element of the set `S`, is non-null in every instantiation, and here it makes the \
       deprecated input field `w` of `W` required"
    ]
  );

  // Where a default fills the slot, at the application's path; and where an `implements` entry
  // passes the element on, at the entry. Each once, and `S<Int>` adds nothing.
  let deprecated_message = "`T`, the element of the set `S`, is non-null in every instantiation, \
                            and here it makes the deprecated argument `v` of the directive `@d` \
                            required";
  let text = "type Query {\n  s: S<Int>\n}\n\
              directive @d<X, Y = X>(v: Y @deprecated) on SCALAR\nscalar S<T> @set @d<T>\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEPRECATED_ARGUMENT,
      named(schema, text, "@", "d", 1),
      Some(at(schema, text, "deprecated", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(messages(&analysis), [deprecated_message]);
  let text = "type Query {\n  s: S<Int>\n}\ndirective @d<X>(v: X @deprecated) on INTERFACE\n\
              interface Mk<X> @marker @d<X>\nscalar S<T> implements Mk<T> @set\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEPRECATED_ARGUMENT,
      at(schema, text, "Mk<T>", 0),
      Some(at(schema, text, "deprecated", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(messages(&analysis), [deprecated_message]);
}

#[test]
fn a_member_many_collections_make_required_is_read_once() {
  // `@d`'s argument `v` applies `@tag` `tags` times and then `@deprecated`, and each of `sets`
  // sets gives `v` its element through `@d<T>`: each is its definition's mistake, refused once, at
  // the `T` of its `@d<T>`, labelled at the one `@deprecated`. The counts vary, and each program
  // runs again under a diagnostic ceiling of zero, which keeps no report and still builds and
  // counts each. A test sees the diagnostics, not the work, which holds by construction: where `v`
  // applies `@deprecated` is found in the one look the table of where each type parameter stands
  // takes at `v`'s applications, and kept with each set's mistake, so each report reads it in one
  // lookup. Read from `v`'s applications again for each report, the largest program here would
  // pass its 3,000 `@tag`s for each of its 3,000 sets, 9 million steps, where the rule now takes a
  // few for each set.
  const SETS: usize = 3_000;
  const TAGS: usize = 3_000;
  let limits = Limits::new();
  for (sets, tags) in [(1, 1), (SETS, 1), (1, TAGS), (SETS, TAGS)] {
    let mut text = format!(
      "type Query {{\n  ok: Int\n}}\ndirective @tag repeatable on ARGUMENT_DEFINITION\n\
       directive @d<X>(v: X{} @deprecated) on SCALAR\n",
      " @tag".repeat(tags)
    );
    // Where each set's `T` in `@d<T>` is written.
    let mut written: Vec<usize> = Vec::with_capacity(sets);
    for index in 0..sets {
      let head = format!("scalar S{index}<T> @set @d<");
      written.push(text.len() + head.len());
      text.push_str(&head);
      text.push_str("T>\n");
    }
    let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
    let analysis = analyze(&root, &[], &limits);
    let schema = schema_file(&analysis);
    let deprecated = Some(at(schema, &text, "deprecated", 0));
    let expected: Vec<(&'static str, Location, Option<Location>)> = written
      .iter()
      .map(|start: &usize| {
        (
          DEPRECATED_ARGUMENT,
          Location::new(schema.get(), SimpleSpan::new(*start, *start + 1)),
          deprecated,
        )
      })
      .collect();
    assert_eq!(found(&analysis), expected, "sets {sets}, tags {tags}");
    let expected: Vec<String> = (0..sets)
      .map(|index: usize| {
        format!(
          "`T`, the element of the set `S{index}`, is non-null in every instantiation, and here \
           it makes the deprecated argument `v` of the directive `@d` required"
        )
      })
      .collect();
    assert_eq!(messages(&analysis), expected, "sets {sets}, tags {tags}");
    assert!(
      analysis
        .diagnostics()
        .iter()
        .all(|diagnostic: &Diagnostic| diagnostic.labels() == 1),
      "sets {sets}, tags {tags}"
    );
    // Under a ceiling of zero, each report is built and counted, and none is kept.
    let silent = analyze(&root, &[], &limits.with_max_diagnostics(0));
    let diagnostics = silent.diagnostics();
    assert!(diagnostics.is_empty(), "sets {sets}, tags {tags}");
    assert!(diagnostics.truncated());
    assert_eq!(diagnostics.error_count(), sets);
  }
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

/// OneOf input objects whose field is a type parameter, written nullable, and one whose field is a
/// list of one; an input object that holds one; and one whose default fills it non-null.
const ONE_OF_INSTANTIATIONS: &str = "type Query {\n  ok: Int\n}\n\
                                     input Choice<T> @oneOf {\n  x: T\n}\n\
                                     input L<T> @oneOf {\n  x: [T]\n}\n\
                                     input Holds<U> {\n  c: Choice<U>\n}\n\
                                     input Filled<T = Int!> @oneOf {\n  y: T\n}\n\
                                     type Uses {\n  a(c: Choice<Int!>): Int\n  \
                                     b(c: Choice<Int>): Int\n  d(l: L<Int!>): Int\n  \
                                     e(h: Holds<Int!>): Int\n  f(c: Filled): Int\n}\n";

#[test]
fn a_one_of_field_a_parameter_types_stays_nullable() {
  let analysis = analyze_one(ONE_OF_INSTANTIATIONS);
  let schema = schema_file(&analysis);
  let text = ONE_OF_INSTANTIATIONS;
  let choice = Some(named(schema, text, "x: ", "T", 0));
  assert_eq!(
    found(&analysis),
    [
      (
        ONE_OF_NON_NULL,
        named(schema, text, "a(c: Choice<", "Int!", 0),
        choice
      ),
      // `Holds`'s `U` is `Choice`'s `T`, passed on at `Choice<U>`.
      (
        ONE_OF_NON_NULL,
        named(schema, text, "Holds<", "Int!", 0),
        Some(at(schema, text, "Choice<U>", 0))
      ),
      // A default fills it: at the occurrence.
      (
        ONE_OF_NON_NULL,
        named(schema, text, "f(c: ", "Filled", 0),
        Some(named(schema, text, "y: ", "T", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert_eq!(
    messages,
    [
      "`Choice` takes a non-null type for its type parameter `T`, which is the type of the field \
       `x` of the OneOf input object `Choice`",
      "`Holds` takes a non-null type for its type parameter `U`, which is the type of the field \
       `x` of the OneOf input object `Choice`",
      "`Filled` takes, from the default of `T`, a non-null type for its type parameter `T`, which \
       is the type of the field `y` of the OneOf input object `Filled`",
    ]
  );
  let filled = analysis
    .diagnostics()
    .iter()
    .nth(2)
    .expect("three diagnostics");
  assert_eq!(
    (
      filled.primary_label(),
      filled
        .label(1)
        .map(|label| (label.location(), label.text())),
      filled.help()
    ),
    (
      Some("field of a @oneOf input object is non-null"),
      Some((
        named(schema, text, "T = ", "Int!", 0),
        "filled from this default"
      )),
      Some(
        "every field of a `@oneOf` input object is one of the alternatives, so each one has to be \
         nullable."
      )
    )
  );
}

#[test]
fn a_collections_key_a_one_of_field_takes_is_refused_once() {
  // `@d<T>` makes the element of the set `S`, non-null in every instantiation, the type of
  // `Choice`'s field `x`: the definition's mistake, reported once, where it gives the place,
  // labelled at the field. `S<Int>` and `S<Int!>` add nothing.
  let text = "type Query {\n  s: S<Int>\n  t: S<Int!>\n}\ninput Choice<C> @oneOf {\n  x: C\n}\n\
              directive @d<X>(v: Choice<X>) on SCALAR\nscalar S<T> @set @d<T>\n";
  let analysis = analyze_text(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      ONE_OF_NON_NULL,
      named(schema, text, "@d<", "T", 0),
      Some(named(schema, text, "  ", "x", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "`T`, the element of the set `S`, is non-null in every instantiation, and here it is the \
       type of the field `x` of the OneOf input object `Choice`"
    ]
  );
  let forced = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    (
      forced.primary_label(),
      forced.label(0).map(|label| label.text()),
      forced.help()
    ),
    (
      Some("field of a @oneOf input object is non-null"),
      Some("the field it is the type of"),
      Some(
        "a set's element and a map's key are non-null however they are written, and every field \
         of a `@oneOf` input object has to be nullable: do not pass the key or element to one."
      )
    )
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
  // A collection's argument, an instantiation's in a member's type, an application's, and what a
  // fill derives.
  silent_beside_a_phase(
    ARGUMENT_TYPE,
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
     type T {\n  f(m: HashMap<string, Obj>): Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
     type T {\n  f(m: HashMap<string, Obj>): Int\n}\ntype U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    FIELD_TYPE,
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Page<P> {\n  items: [P]\n}\n\
     type T {\n  p: Page<In>\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Page<P> {\n  items: [P]\n}\n\
     type T {\n  p: Page<In>\n}\ntype U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    DIRECTIVE_ARGUMENT_TYPE,
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
     directive @tagged<V>(value: V) on OBJECT\ntype T @tagged<Obj> {\n  t: Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ntype Obj {\n  o: Int\n}\n\
     directive @tagged<V>(value: V) on OBJECT\ntype T @tagged<Obj> {\n  t: Int\n}\n\
     type U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    FIELD_TYPE,
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
     type Conn<N, E = Edge<N>> {\n  edges: [E]\n}\ntype T {\n  c: Conn<In>\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput In {\n  i: Int\n}\ntype Edge<N> {\n  node: N\n}\n\
     type Conn<N, E = Edge<N>> {\n  edges: [E]\n}\ntype T {\n  c: Conn<In>\n}\n\
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
    DEPRECATED_INPUT_FIELD,
    "type Query {\n  ok: Int\n}\ninput I<T> {\n  a: T @deprecated\n}\n\
     type T {\n  f(i: I<Int!>): Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput I<T> {\n  a: T @deprecated\n}\n\
     type T {\n  f(i: I<Int!>): Int\n}\ntype U {\n  x: Missing\n}\n",
  );
  silent_beside_a_phase(
    ONE_OF_NON_NULL,
    "type Query {\n  ok: Int\n}\ninput Choice<T> @oneOf {\n  x: T\n}\n\
     type T {\n  f(c: Choice<Int!>): Int\n  x: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ninput Choice<T> @oneOf {\n  x: T\n}\n\
     type T {\n  f(c: Choice<Int!>): Int\n}\ntype U {\n  x: Missing\n}\n",
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
  let param = "P".repeat(4096);
  let texts = [
    format!(
      "type Query {{\n  ok: Int\n}}\ntype {owner} {{\n  f({plain}: HashMap<string, {owner}>): Int\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ninput In {{\n  i: Int\n}}\n\
       type {owner}<{param}> {{\n  {plain}: {param}\n}}\ntype U {{\n  u: {owner}<In>\n}}\n"
    ),
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
