//! The GraphQLx analyzer's rules on directive applications, each GraphQL's own with GraphQL's code:
//! a directive stands where its definition lists; a directive that is not repeatable is applied
//! once to a location; and an application passes each argument once. Each rule judges every
//! application a supplied package writes, says nothing within what a phase refused, and quotes a
//! name only as an excerpt.
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

/// GraphQL's code for a directive that is not repeatable applied twice to one location.
const DUPLICATE: &str = "smear::schema::duplicate-directive-use";

/// GraphQL's code for an application that passes an argument twice.
const ARGUMENT: &str = "smear::schema::duplicate-directive-argument-use";

/// The code of an argument its directive does not declare, lowering's.
const UNDECLARED: &str = "smear::schema::undefined-directive-argument";

/// What GraphQL tells the author of a directive that is not repeatable applied twice to one
/// location.
const DUPLICATE_HELP: &str = "declare the directive `repeatable`, or apply it once; a definition \
                              and its extensions are one location.";

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

/// A directive that is not repeatable twice at each kind of location, a directive definition's
/// argument among them, and three times on one type; a repeatable directive twice; and a directive
/// once on a type and once on its field.
const TWICE: &str = "type Query {\n  ok: Int\n}\n\
                     directive @d on OBJECT | FIELD_DEFINITION | ARGUMENT_DEFINITION | \
                     INPUT_FIELD_DEFINITION | ENUM_VALUE | DIRECTIVE_DEFINITION | SCHEMA\n\
                     directive @r repeatable on OBJECT | FIELD_DEFINITION\n\
                     type T @d @d {\n  f(a: Int @d @d): Int @d @d\n}\n\
                     input In {\n  i: Int @d @d\n}\nenum E {\n  V @d @d\n}\n\
                     directive @e(b: Int @d @d) @d @d on FIELD\n\
                     schema @d @d {\n  query: Query\n}\n\
                     type Thrice @d @d @d {\n  t: Int\n}\ntype Again @r @r {\n  g: Int @r @r\n}\n\
                     type Both @d {\n  h: Int @d\n}\n";

#[test]
fn a_directive_that_is_not_repeatable_is_applied_once() {
  let analysis = analyze_one(TWICE);
  let schema = schema_file(&analysis);
  // Each at the later application, labelled at the first of its location: `@d`'s nth occurrence,
  // its definition's being the 0th.
  let d = |nth: usize| -> Location { named(schema, TWICE, "@", "d", nth) };
  let again = |later: usize, first: usize| -> (&'static str, Location, Option<Location>) {
    (DUPLICATE, d(later), Some(d(first)))
  };
  assert_eq!(
    found(&analysis),
    [
      again(2, 1),
      again(4, 3),
      again(6, 5),
      again(8, 7),
      again(10, 9),
      again(12, 11),
      again(14, 13),
      again(16, 15),
      again(18, 17),
      again(19, 17),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // Each names the directive with its `@`, and what it is applied to by its own name.
  let applied = |what: &str| -> String {
    format!("the directive `@d` is applied to {what} again, and it is not repeatable")
  };
  assert_eq!(
    messages(&analysis),
    [
      applied("the type `T`"),
      applied("the argument `a` of the field `f` of `T`"),
      applied("the field `f` of `T`"),
      applied("the input field `i` of `In`"),
      applied("the value `V` of the enum `E`"),
      applied("the argument `b` of the directive `@e`"),
      applied("the directive `@e`"),
      applied("the schema"),
      applied("the type `Thrice`"),
      applied("the type `Thrice`"),
    ]
  );
  // GraphQL's own phrases and help.
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.primary_label(),
      Some("directive is not repeatable")
    );
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("first applied here")
    );
    assert_eq!(diagnostic.help(), Some(DUPLICATE_HELP));
  }
}

#[test]
fn a_definition_and_its_extensions_are_one_location() {
  // A type, a directive definition and the schema, each applied to once by its definition and
  // once by an extension: each reported in the extension, labelled in the definition.
  let text = "type Query {\n  ok: Int\n}\ndirective @d on OBJECT | DIRECTIVE_DEFINITION | SCHEMA\n\
              type T @d {\n  t: Int\n}\nextend type T @d\ndirective @e @d on FIELD\n\
              extend directive @e @d\nschema @d {\n  query: Query\n}\nextend schema @d\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let d = |nth: usize| -> Location { named(schema, text, "@", "d", nth) };
  assert_eq!(
    found(&analysis),
    [
      (DUPLICATE, d(2), Some(d(1))),
      (DUPLICATE, d(4), Some(d(3))),
      (DUPLICATE, d(6), Some(d(5))),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // The root package's extension of a built-in is the root package's to answer for.
  let text = "type Query {\n  ok: Int\n}\ndirective @x on SCALAR\nextend scalar String @x @x\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      named(schema, text, "@", "x", 2),
      Some(named(schema, text, "@", "x", 1))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis),
    ["the directive `@x` is applied to the type `string` again, and it is not repeatable"]
  );
}

#[test]
fn a_generic_directive_is_one_directive() {
  let text = "type Query {\n  ok: Int\n}\ndirective @d<T> on OBJECT\n\
              type G @d<Int> @d<String> {\n  g: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DUPLICATE,
      named(schema, text, "@", "d", 2),
      Some(named(schema, text, "@", "d", 1))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// An argument written twice, twice beside another, and three times, each in an application of its
/// own.
const ARGUMENTS: &str = "type Query {\n  ok: Int\n}\ndirective @d(a: Int, b: Int) on OBJECT\n\
                         type One @d(a: 1, a: 2) {\n  o: Int\n}\n\
                         type Two @d(a: 1, b: 2, a: 3) {\n  t: Int\n}\n\
                         type Three @d(a: 1, a: 2, a: 3) {\n  h: Int\n}\n";

#[test]
fn an_application_passes_each_argument_once() {
  let analysis = analyze_one(ARGUMENTS);
  let schema = schema_file(&analysis);
  // A later `a` follows `, `, and an application's first `(`: the directive definition's own
  // argument is the 0th of those.
  let later = |nth: usize| -> Location { named(schema, ARGUMENTS, ", ", "a", nth) };
  let first = |nth: usize| -> Option<Location> { Some(named(schema, ARGUMENTS, "(", "a", nth)) };
  assert_eq!(
    found(&analysis),
    [
      (ARGUMENT, later(0), first(1)),
      (ARGUMENT, later(1), first(2)),
      (ARGUMENT, later(2), first(3)),
      (ARGUMENT, later(3), first(3)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    messages(&analysis)[0],
    "this application of `@d` passes the argument `a` again"
  );
  // GraphQL's own phrases, and no help, as GraphQL gives none.
  for diagnostic in analysis.diagnostics() {
    assert_eq!(
      diagnostic.primary_label(),
      Some("directive argument is passed twice")
    );
    assert_eq!(
      diagnostic.label(0).map(|label| label.text()),
      Some("first passed here")
    );
    assert_eq!(diagnostic.help(), None);
  }

  // An argument the directive does not declare is lowering's, each time it is written, and is not
  // kept: none of it is this rule's.
  let text = "type Query {\n  ok: Int\n}\ndirective @d(a: Int) on OBJECT\n\
              type Four @d(zz: 1, zz: 2) {\n  z: Int\n}\n";
  let analysis = analyze_one(text);
  assert_eq!(
    codes(&analysis),
    [UNDECLARED, UNDECLARED],
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
  // The later application's contribution silences it; the first's does not.
  silent_beside(
    UNDEFINED,
    DUPLICATE,
    "type Query {\n  ok: Int\n}\ndirective @d on OBJECT\ntype T @d {\n  t: Int\n}\n\
     extend type T @d {\n  u: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ndirective @d on OBJECT\ntype T @d {\n  t: Int\n  u: Missing\n}\n\
     extend type T @d\n",
  );
  silent_beside(
    UNDEFINED,
    ARGUMENT,
    "type Query {\n  ok: Int\n}\ndirective @d(a: Int) on OBJECT\n\
     type T @d(a: 1, a: 2) {\n  t: Int\n  u: Missing\n}\n",
    "type Query {\n  ok: Int\n}\ndirective @d(a: Int) on OBJECT\ntype T @d(a: 1, a: 2) {\n  t: Int\n}\n\
     type U {\n  u: Missing\n}\n",
  );
}

#[test]
fn long_names_in_these_rules_are_quoted_as_excerpts() {
  let name = "d".repeat(4096);
  let owner = "T".repeat(4096);
  let member = "f".repeat(4096);
  let texts = [
    format!(
      "type Query {{\n  ok: Int\n}}\ndirective @{name} on FIELD_DEFINITION\ntype T @{name} {{\n  \
       t: Int\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ndirective @{name} on FIELD_DEFINITION\n\
       type {owner} {{\n  {member}: Int @{name} @{name}\n}}\n"
    ),
    format!(
      "type Query {{\n  ok: Int\n}}\ndirective @{name}({member}: Int) on OBJECT\n\
       type T @{name}({member}: 1, {member}: 2) {{\n  t: Int\n}}\n"
    ),
  ];
  for text in &texts {
    let analysis = analyze_text(text);
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}
