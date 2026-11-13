//! The GraphQLx analyzer's rules on what a definition holds, each GraphQL's own with GraphQL's
//! code: a member's name does not begin with two underscores. Each rule judges what a supplied
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
}

/// A refusal of `code` is silent in a contribution that also holds a phase's error, `together`,
/// and is reported beside it when the error is in a sibling definition, `apart`.
fn silent_beside_a_phase(code: &str, together: &'static str, apart: &'static str) {
  let analysis = analyze_one(together);
  assert_eq!(
    codes(&analysis),
    [UNDEFINED],
    "`{code}` beside a phase's error: {:#?}",
    analysis.diagnostics()
  );
  let analysis = analyze_one(apart);
  let mut reported = codes(&analysis);
  reported.sort_unstable();
  let mut expected = [code, UNDEFINED];
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
}

#[test]
fn long_names_in_these_rules_are_quoted_as_excerpts() {
  let owner = "T".repeat(4096);
  let member = format!("__{}", "f".repeat(4096));
  let texts = [format!(
    "type Query {{\n  ok: Int\n}}\ntype {owner} {{\n  {member}: Int\n}}\n"
  )];
  for text in &texts {
    let analysis = analyze_text(text);
    let messages = messages(&analysis);
    assert_eq!(messages.len(), 1, "{:#?}", analysis.diagnostics());
    assert!(messages[0].len() < 1024, "{}", messages[0]);
  }
}
