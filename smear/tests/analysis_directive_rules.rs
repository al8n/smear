//! The GraphQLx analyzer's rules on directive applications, each GraphQL's own with GraphQL's code:
//! a directive stands where its definition lists. Each rule judges every application a supplied
//! package writes, says nothing within what a phase refused, and quotes a name only as an excerpt.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, Layout, Limits, Package, PackageId, SourceFile, SourceId, Tier,
    analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a type name that names nothing, a phase's.
const UNDEFINED: &str = "smear::schema::undefined-type";

/// The code of an extension its package may not write, the orphan rule's.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// GraphQL's code for a directive applied where its definition does not list.
const LOCATION: &str = "smear::schema::unsupported-directive-location";

/// What GraphQL tells the author of a directive applied where its definition does not list.
const LOCATION_HELP: &str = "add the location to the directive's definition, or move the usage.";

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

/// The twelve locations of a type-system document, as a definition writes them.
const LOCATIONS: [&str; 12] = [
  "SCHEMA",
  "SCALAR",
  "OBJECT",
  "FIELD_DEFINITION",
  "ARGUMENT_DEFINITION",
  "INTERFACE",
  "UNION",
  "ENUM",
  "ENUM_VALUE",
  "INPUT_OBJECT",
  "INPUT_FIELD_DEFINITION",
  "DIRECTIVE_DEFINITION",
];

/// For each of the twelve locations, a directive that lists every location but that one, applied
/// there; and `@all`, which lists all twelve, applied beside each. An argument is applied to twice,
/// a field's and a directive definition's.
fn every_location() -> String {
  let mut text = String::from("type Query {\n  ok: Int\n}\n");
  let names = [
    "noSchema",
    "noScalar",
    "noObject",
    "noField",
    "noArgument",
    "noInterface",
    "noUnion",
    "noEnum",
    "noValue",
    "noInput",
    "noInputField",
    "noDirective",
  ];
  for (name, skipped) in names.iter().zip(LOCATIONS) {
    let listed: Vec<&str> = LOCATIONS
      .iter()
      .copied()
      .filter(|location: &&str| *location != skipped)
      .collect();
    text.push_str(&format!("directive @{name} on {}\n", listed.join(" | ")));
  }
  text.push_str(&format!("directive @all on {}\n", LOCATIONS.join(" | ")));
  text.push_str(
    "schema @noSchema @all {\n  query: Query\n}\nscalar S @noScalar @all\n\
     type O @noObject @all {\n  f(a: Int @noArgument @all): Int @noField @all\n}\n\
     interface I @noInterface @all {\n  i: Int\n}\nunion U @noUnion @all = O\n\
     enum E @noEnum @all {\n  V @noValue @all\n}\n\
     input In @noInput @all {\n  x: Int @noInputField @all\n}\n\
     directive @d(b: Int @noArgument @all) @noDirective @all on FIELD\n",
  );
  text
}

#[test]
fn a_directive_stands_where_its_definition_lists() {
  let text = every_location();
  let analysis = analyze_text(&text);
  let schema = schema_file(&analysis);
  // Each at the application's path, which the text writes after the directive's definition, and
  // labelled at the definition's name; `@all` nowhere.
  let refused = |name: &str, nth: usize| -> (&'static str, Location, Option<Location>) {
    (
      LOCATION,
      named(schema, &text, "@", name, nth),
      Some(named(schema, &text, "directive @", name, 0)),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      refused("noSchema", 1),
      refused("noScalar", 1),
      refused("noObject", 1),
      refused("noArgument", 1),
      refused("noField", 1),
      refused("noInterface", 1),
      refused("noUnion", 1),
      refused("noEnum", 1),
      refused("noValue", 1),
      refused("noInput", 1),
      refused("noInputField", 1),
      refused("noArgument", 2),
      refused("noDirective", 1),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the directive with its `@`, what it is applied to and the location it lacks.
  let applied = |name: &str, what: &str, location: &str| -> String {
    format!(
      "the directive `@{name}` is applied to {what}, and its definition does not list `{location}`"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      applied("noSchema", "the schema", "SCHEMA"),
      applied("noScalar", "a scalar", "SCALAR"),
      applied("noObject", "an object type", "OBJECT"),
      applied("noArgument", "an argument", "ARGUMENT_DEFINITION"),
      applied("noField", "a field", "FIELD_DEFINITION"),
      applied("noInterface", "an interface", "INTERFACE"),
      applied("noUnion", "a union", "UNION"),
      applied("noEnum", "an enum", "ENUM"),
      applied("noValue", "an enum value", "ENUM_VALUE"),
      applied("noInput", "an input object", "INPUT_OBJECT"),
      applied("noInputField", "an input field", "INPUT_FIELD_DEFINITION"),
      applied("noArgument", "an argument", "ARGUMENT_DEFINITION"),
      applied(
        "noDirective",
        "a directive definition",
        "DIRECTIVE_DEFINITION"
      ),
    ]
  );
  // GraphQL's own phrase and help, and the analyzer's label at the definition.
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.primary_label(),
      Some("directive is not allowed here")
    );
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("the directive's definition")
    );
    assert_eq!(diagnostic.help(), Some(LOCATION_HELP));
  }
}

/// `core`'s directives, each where `core` does not let it stand, and each where it does.
const CORE: &str = "type Query {\n  ok: Int\n}\ntype Marked @marker {\n  a: Int\n}\n\
                    type Bagged @set {\n  b: Int\n}\ntype Chosen @oneOf {\n  c: Int\n}\n\
                    scalar Old @deprecated\nenum Specified @specifiedBy(url: \"u\") {\n  V\n}\n\
                    interface M @marker\nscalar Bag<T> @set\ninput In @oneOf {\n  i: Int\n}\n";

#[test]
fn cores_directives_stand_where_core_lists_them() {
  let analysis = analyze_one(CORE);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let builtins = model.builtins();
  // Each labelled at `core`'s definition of the directive, by what the application resolved to.
  let refused = |name: &str, definition: DefId| -> (&'static str, Location, Option<Location>) {
    (
      LOCATION,
      named(schema, CORE, "@", name, 0),
      Some(model.def(definition).location()),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      refused("marker", builtins.marker()),
      refused("set", builtins.set()),
      refused("oneOf", builtins.one_of()),
      refused("deprecated", builtins.deprecated()),
      refused("specifiedBy", builtins.specified_by()),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[3],
    "the directive `@deprecated` is applied to a scalar, and its definition does not list `SCALAR`"
  );
}

#[test]
fn an_extensions_application_stands_where_its_definition_stands() {
  // On an object, a directive definition and the schema, each by an extension, with a directive
  // that lists none of the three.
  let text = "type Query {\n  ok: Int\n}\ndirective @x on FIELD_DEFINITION\ntype T {\n  t: Int\n}\n\
              extend type T @x\ndirective @d on FIELD\nextend directive @d @x\nextend schema @x\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let x = Some(named(schema, text, "directive @", "x", 0));
  assert_eq!(
    found(&analysis),
    [
      (LOCATION, named(schema, text, "@", "x", 1), x),
      (LOCATION, named(schema, text, "@", "x", 2), x),
      (LOCATION, named(schema, text, "@", "x", 3), x),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    [
      "the directive `@x` is applied to an object type, and its definition does not list `OBJECT`",
      "the directive `@x` is applied to a directive definition, and its definition does not list \
       `DIRECTIVE_DEFINITION`",
      "the directive `@x` is applied to the schema, and its definition does not list `SCHEMA`",
    ]
  );

  // The root package's `@marker` on `core`'s `NonZero` is the orphan rule's, since what a built-in
  // is is the language's, and this rule's, since `@marker` stands on an interface alone: two
  // diagnostics at one path, each true of it.
  let text = "type Query {\n  ok: Int\n}\nextend scalar NonZero @marker\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let builtins = model.builtins();
  let marker = named(schema, text, "@", "marker", 0);
  assert_eq!(
    found(&analysis),
    [
      (
        ORPHAN,
        marker,
        Some(model.def(builtins.non_zero()).location())
      ),
      (
        LOCATION,
        marker,
        Some(model.def(builtins.marker()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_location_written_twice_is_listed() {
  let text = "type Query {\n  ok: Int\n}\ndirective @d on OBJECT | OBJECT | FIELD_DEFINITION\n\
              type T @d {\n  f: Int @d\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
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
  silent_beside(
    UNDEFINED,
    LOCATION,
    "type Query {\n  ok: Int\n}\ndirective @x on FIELD_DEFINITION\n\
     type T @x {\n  t: Int\n  u: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ndirective @x on FIELD_DEFINITION\ntype T @x {\n  t: Int\n}\n\
     type U {\n  u: Missing\n}\n",
  );
}

#[test]
fn long_names_in_these_rules_are_quoted_as_excerpts() {
  let name = "d".repeat(4096);
  let texts = [format!(
    "type Query {{\n  ok: Int\n}}\ndirective @{name} on FIELD_DEFINITION\ntype T @{name} {{\n  \
     t: Int\n}}\n"
  )];
  for text in &texts {
    let analysis = analyze_text(text);
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}
