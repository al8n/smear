//! The GraphQLx analyzer's composite values: an input object's value, its fields found by name and
//! kept in the input object's order; the codes GraphQL gives an object literal's unknown and
//! required fields; and an input object its own package makes OneOf.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, Cx, DefId, Diagnostic, Layout, Limits, MemberId, Model,
    Namespace, ObjectEntry, Package, PackageId, Packages, Path, PathRoot, SourceFile, SourceId,
    Sym, Tier, ValueId, ValueKind, Visitor, analyze, visit,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a default that does not fit.
const DEFAULT: &str = "smear::schema::invalid-default-value";
/// GraphQL's code for a directive argument's value that does not fit.
const ARGUMENT: &str = "smear::schema::invalid-directive-argument-value";
/// GraphQL's code for an object literal's field its input object does not declare.
const UNDEFINED: &str = "smear::schema::undefined-input-object-field";
/// GraphQL's code for a required field an object literal leaves out or gives `null`.
const REQUIRED: &str = "smear::schema::missing-required-input-object-field";
/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
}

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(3))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == "schema.graphqlx")
    .expect("the root package's file")
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

/// Where `literal` is written right after `before`, the first time, in `text`, the file `source`.
fn after(source: SourceId, text: &str, before: &str, literal: &str) -> Location {
  let written = format!("{before}{literal}");
  let start = text
    .find(written.as_str())
    .unwrap_or_else(|| panic!("`{written}` is not written"))
    + before.len();
  Location::new(source.get(), SimpleSpan::new(start, start + literal.len()))
}

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The rows of `found` whose code is one of `codes`, in the list's order.
fn found_of(analysis: &Analysis, codes: &[&str]) -> Vec<(&'static str, Location)> {
  found(analysis)
    .into_iter()
    .filter(|row: &(&'static str, Location)| codes.contains(&row.0))
    .collect()
}

/// `rows` in one order, by where each is and then its code, so that two lists of one content
/// compare equal whatever order they were written in.
fn sorted(mut rows: Vec<(&'static str, Location)>) -> Vec<(&'static str, Location)> {
  rows.sort_by_key(|&(code, at): &(&'static str, Location)| {
    (
      at.source(),
      at.span().map(|span: SimpleSpan| (span.start(), span.end())),
      code,
    )
  });
  rows
}

/// The definition the root package's `name` names in the type namespace.
fn type_def(model: &Model, name: &str) -> DefId {
  let canonical = format!("^::app::{name}");
  model
    .lookup(Namespace::Type, &canonical)
    .unwrap_or_else(|| panic!("no type `{canonical}`"))
}

/// The input field `name` of the root package's input object `owner`.
fn input_field(model: &Model, owner: &str, name: &str) -> MemberId {
  model
    .def(type_def(model, owner))
    .input_fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == name)
    .unwrap_or_else(|| panic!("no input field `{owner}.{name}`"))
}

/// The default of the input field `name` of `owner`.
fn default(model: &Model, owner: &str, name: &str) -> ValueId {
  model
    .member(input_field(model, owner, name))
    .default()
    .unwrap_or_else(|| panic!("`{owner}.{name}` has no default"))
}

/// The default of the argument `argument` of the field `field` of the root package's type `owner`.
fn argument_default(model: &Model, owner: &str, field: &str, argument: &str) -> ValueId {
  let field = model
    .def(type_def(model, owner))
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{owner}.{field}`"));
  let argument = model
    .member(field)
    .arguments()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == argument)
    .unwrap_or_else(|| panic!("no argument `{argument}`"));
  model
    .member(argument)
    .default()
    .unwrap_or_else(|| panic!("`{argument:?}` has no default"))
}

/// The arguments of the first application on the root package's type `owner`.
fn arguments_of(model: &Model, owner: &str) -> Vec<ApplicationArgument> {
  let application = model.def(type_def(model, owner)).applications()[0];
  model.application(application).arguments().to_vec()
}

/// `path` as written: its root and its segments.
fn spelled(model: &Model, path: Option<Path<'_>>) -> String {
  let Some(path) = path else {
    return String::new();
  };
  let root = match path.root() {
    PathRoot::Relative => String::new(),
    PathRoot::Package => String::from("~::"),
    PathRoot::Dependency => String::from("^::"),
    PathRoot::Parent(levels) => "..::".repeat(levels.get() as usize),
    _ => String::from("?::"),
  };
  let segments: Vec<&str> = path
    .segments()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect();
  format!("{root}{}", segments.join("::"))
}

/// Each of `items`, written out, joined by commas.
fn joined(model: &Model, items: &[ValueId]) -> String {
  let items: Vec<String> = items
    .iter()
    .map(|item: &ValueId| shown(model, *item))
    .collect();
  items.join(", ")
}

/// The value `id`, written out: a coerced value as its kind's name and its text, `Int 1`, `String
/// "q"` with its cooked text quoted, `Enum Color::RED` with its path as written and `List [Int 1]`;
/// an input object as `Object {a: Int 1, step= Int 1}`, each entry its field's name and its value,
/// a field a default filled marked `=` for its `:`; `Set {Int 1, Int 2}`; `Map {String "a" => Int
/// 1}`; a type value as `Type User`, the name of the definition it names; a literal kept as written
/// as `Literal(Int) 1`; and an unresolved value as `?`.
fn shown(model: &Model, id: ValueId) -> String {
  let value = model.value(id);
  match value.kind() {
    ValueKind::Null => String::from("Null null"),
    ValueKind::Bool(flag) => format!("Boolean {flag}"),
    ValueKind::Int(int) => format!("Int {}", int.text()),
    ValueKind::Float(float) => format!("Float {}", float.text()),
    ValueKind::String(text) => format!("String {text:?}"),
    ValueKind::Bytes(bytes) => format!("Bytes {bytes:?}"),
    ValueKind::Enum { .. } => format!("Enum {}", spelled(model, value.path())),
    ValueKind::Type { def, .. } => format!("Type {}", model.def(def).name()),
    ValueKind::List(items) => format!("List [{}]", joined(model, items)),
    ValueKind::Set(items) => format!("Set {{{}}}", joined(model, items)),
    ValueKind::Map(entries) => {
      let entries: Vec<String> = entries
        .iter()
        .map(|&(key, item): &(ValueId, ValueId)| {
          format!("{} => {}", shown(model, key), shown(model, item))
        })
        .collect();
      format!("Map {{{}}}", entries.join(", "))
    }
    ValueKind::Object(entries) => {
      let entries: Vec<String> = entries
        .iter()
        .map(|entry: &ObjectEntry| {
          let mark = if entry.name().is_some() { ":" } else { "=" };
          format!(
            "{}{mark} {}",
            model.member(entry.field()).name(),
            shown(model, entry.value())
          )
        })
        .collect();
      format!("Object {{{}}}", entries.join(", "))
    }
    ValueKind::Literal(literal) => format!("Literal({:?}) {}", literal.kind(), literal.text()),
    ValueKind::Unresolved => String::from("?"),
    other => format!("{other:?}"),
  }
}

/// The default of `owner.name`, written out.
fn shown_default(model: &Model, owner: &str, name: &str) -> String {
  shown(model, default(model, owner, name))
}

/// The entries of the input object `id`.
fn entries(model: &Model, id: ValueId) -> Vec<ObjectEntry> {
  match model.value(id).kind() {
    ValueKind::Object(entries) => entries.to_vec(),
    other => panic!("value {} is no object: {other:?}", id.get()),
  }
}

/// Every value a walk of some packages reaches.
#[derive(Default)]
struct Values {
  ids: Vec<ValueId>,
}

impl Visitor for Values {
  fn value(&mut self, _cx: &mut Cx<'_>, value: ValueId) {
    self.ids.push(value);
  }
}

/// Every value a walk of `packages` reaches, in its order.
fn values(analysis: &Analysis, packages: Packages) -> Vec<ValueId> {
  let mut reached = Values::default();
  let _ = visit(analysis, packages, &mut reached);
  reached.ids
}

/// The fields an object literal sets are its entries in the input object's order, whatever order
/// they are written in, and the field it leaves out is absent. Refuses entries, or ids, in written
/// order.
#[test]
fn an_object_holds_its_fields_in_the_input_objects_order() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int\n  b: Int\n  c: Int\n}\n\
              input J {\n  x: I = { c: 3, a: 1 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let object = default(model, "J", "x");
  assert_eq!(shown(model, object), "Object {a: Int 1, c: Int 3}");
  let entries = entries(model, object);
  assert_eq!(entries[0].field(), input_field(model, "I", "a"));
  assert_eq!(entries[1].field(), input_field(model, "I", "c"));
  assert_eq!(entries[0].name(), Some(after(schema, text, "3, ", "a")));
  assert_eq!(entries[1].name(), Some(after(schema, text, "= { ", "c")));
  // The object, then the `1`, then the `3`.
  assert_eq!(entries[0].value().get(), object.get() + 1);
  assert_eq!(entries[1].value().get(), object.get() + 2);
  assert_eq!(
    model.value(entries[0].value()).location(),
    after(schema, text, "a: ", "1")
  );
  assert!(model.value(object).written());
}

/// A field written twice is two entries, in written order, and nothing is said. Refuses the second
/// entry dropped, and a report.
#[test]
fn a_field_written_twice_is_kept_twice() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int\n}\n\
              input J {\n  x: I = { a: 1, a: 2 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let object = default(model, "J", "x");
  assert_eq!(shown(model, object), "Object {a: Int 1, a: Int 2}");
  let entries = entries(model, object);
  let a = input_field(model, "I", "a");
  assert_eq!((entries[0].field(), entries[1].field()), (a, a));
  assert_eq!(entries[0].name(), Some(after(schema, text, "{ ", "a")));
  assert_eq!(entries[1].name(), Some(after(schema, text, "1, ", "a")));
}

/// The corpus' own document: a field the input object does not declare is reported at its name,
/// with the nearest field in its help, and no value is made of what it is given. Refuses a row for
/// a dropped field's value.
#[test]
fn an_undeclared_field_is_reported_and_not_kept() {
  let text = "directive @onObject(a: In) on OBJECT\ninput In {\n  x: Int\n}\n\
              type Query @onObject(a: { y: 1 }) {\n  ok: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNDEFINED, after(schema, text, "{ ", "y"))]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.help(), Some("did you mean `x`?"));
  let model = analysis.model();
  assert_eq!(
    shown(model, arguments_of(model, "Query")[0].value()),
    "Object {}"
  );
  let one = after(schema, text, "y: ", "1");
  for id in values(&analysis, Packages::All) {
    assert_ne!(model.value(id).location(), one, "value {}", id.get());
  }

  let analysis = analyze_text(text, &Limits::new().with_max_near_miss_comparisons(0));
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.help(), None);
}

/// The corpus' own document: a required field left out is reported once, at the literal, with a
/// label at the field; one given `null` once, at the `null`, its entry kept with an unresolved
/// value; a field with a default is not required. Refuses both diagnostics for `null`, and a
/// defaulted field taken as required.
#[test]
fn a_required_field_is_reported_once() {
  let text = "directive @onObject(a: In) on OBJECT\ninput In {\n  x: Int!\n}\n\
              type Query @onObject(a: {}) {\n  ok: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(REQUIRED, at(schema, text, "{}", 0))]);
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(diagnostic.labels(), 1);
  assert_eq!(
    diagnostic.label(0).map(|label| label.location()),
    Some(after(schema, text, "{\n  ", "x"))
  );

  let text = "directive @onObject(a: In) on OBJECT\ninput In {\n  x: Int!\n}\n\
              type Query @onObject(a: { x: null }) {\n  ok: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(REQUIRED, at(schema, text, "null", 0))]);
  let model = analysis.model();
  assert_eq!(
    shown(model, arguments_of(model, "Query")[0].value()),
    "Object {x: ?}"
  );

  let text = "directive @onObject(a: In2, b: In) on OBJECT\ninput In2 {\n  x: Int! = 1\n}\n\
              input In {\n  x: Int!\n}\ntype Query @onObject(a: {}, b: { x: 1 }) {\n  ok: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
}

/// An unknown field and a required one left out have one code each, in an input field's default
/// and in a directive's argument alike, and neither site's value code is said. Refuses a code told
/// by the site.
#[test]
fn the_field_codes_are_the_same_in_a_default_and_in_an_argument() {
  let text = "type Query {\n  x: Int\n}\ndirective @d(a: In) repeatable on OBJECT\n\
              input In {\n  x: Int!\n}\ninput Holder {\n  unknown: In = { y: 1 }\n  missing: In = {}\n}\n\
              type T @d(a: { y: 2 }) @d(a: {}) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (UNDEFINED, after(schema, text, "unknown: In = { ", "y")),
      (REQUIRED, at(schema, text, "{ y: 1 }", 0)),
      (REQUIRED, at(schema, text, "{}", 0)),
      (UNDEFINED, after(schema, text, "@d(a: { ", "y")),
      (REQUIRED, at(schema, text, "{ y: 2 }", 0)),
      (REQUIRED, at(schema, text, "{}", 1)),
    ])
  );
}

/// A literal that is no object, where an input object is expected, does not fit, with the site's
/// code; an object where a list of input objects is expected is a list of it; `null` is `null`
/// where the type is nullable. Refuses a lone object left a literal.
#[test]
fn a_literal_that_is_no_object_is_no_input_object() {
  let text = "type Query {\n  x: Int\n}\nenum Color {\n  RED\n}\ninput I {\n  a: Int\n}\n\
              input J {\n  i: I = 1\n  s: I = \"x\"\n  e: I = RED\n  l: I = [{}]\n  \
              lone: [I] = {}\n  n: I = null\n  nn: I! = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (DEFAULT, after(schema, text, "i: I = ", "1")),
      (DEFAULT, after(schema, text, "s: I = ", "\"x\"")),
      (DEFAULT, after(schema, text, "e: I = ", "RED")),
      (DEFAULT, after(schema, text, "l: I = ", "[{}]")),
      (DEFAULT, after(schema, text, "nn: I! = ", "null")),
    ])
  );
  let model = analysis.model();
  for field in ["i", "s", "e", "l", "nn"] {
    assert_eq!(shown_default(model, "J", field), "?", "`{field}`");
  }
  assert_eq!(shown_default(model, "J", "lone"), "List [Object {}]");
  assert_eq!(shown_default(model, "J", "n"), "Null null");
}

/// The OneOf input object of the next tests.
const ONE_OF: &str = "input O @oneOf {\n  a: Int\n  b: Int\n}\n";

/// M4's documents: a OneOf literal of no field, of two, or of one given `null`, at any depth, in a
/// default or in a directive's argument, is refused once, with the site's code, at the literal or at
/// the `null`; it stays an object, and its fields are coerced, a field that does not fit being a
/// diagnostic of its own; a `null` for a field the input object does not declare is that field's
/// diagnostic alone. Refuses a OneOf literal left unresolved and not descended, and a second
/// diagnostic for the `null`.
#[test]
fn one_of_takes_exactly_one_field_that_is_not_null() {
  for (head, blamed, code) in [
    ("type Query {\n  ok(f: O = {}): Int\n}\n", "{}", DEFAULT),
    (
      "type Query {\n  ok(f: O = {a: 1, b: 2}): Int\n}\n",
      "{a: 1, b: 2}",
      DEFAULT,
    ),
    (
      "type Query {\n  ok(f: O = {a: null}): Int\n}\n",
      "null",
      DEFAULT,
    ),
    ("type Query {\n  ok(f: [O] = [{}]): Int\n}\n", "{}", DEFAULT),
    (
      "type Query {\n  ok(f: [O] = [{a: 1}, {}]): Int\n}\n",
      "{}",
      DEFAULT,
    ),
    (
      "type Query {\n  ok(f: Outer = {o: {}}): Int\n}\ninput Outer {\n  o: O\n}\n",
      "{}",
      DEFAULT,
    ),
    (
      "type Query {\n  ok: Int\n}\ninput Outer {\n  o: O = {a: 1, b: 2}\n}\n",
      "{a: 1, b: 2}",
      DEFAULT,
    ),
    (
      "directive @d(f: O) on FIELD_DEFINITION\ntype Query {\n  ok: Int @d(f: {})\n}\n",
      "{}",
      ARGUMENT,
    ),
    (
      "directive @d(f: [O]) on FIELD_DEFINITION\ntype Query {\n  ok: Int @d(f: [{b: null}])\n}\n",
      "null",
      ARGUMENT,
    ),
  ] {
    let text = [head, ONE_OF].concat();
    let analysis = analyze_text(&text, &Limits::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(code, at(schema, &text, blamed, 0))],
      "---\n{text}"
    );
  }

  // What the first three hold.
  for (head, held) in [
    ("type Query {\n  ok(f: O = {}): Int\n}\n", "Object {}"),
    (
      "type Query {\n  ok(f: O = {a: 1, b: 2}): Int\n}\n",
      "Object {a: Int 1, b: Int 2}",
    ),
    (
      "type Query {\n  ok(f: O = {a: null}): Int\n}\n",
      "Object {a: ?}",
    ),
  ] {
    let text = [head, ONE_OF].concat();
    let analysis = analyze_text(&text, &Limits::new());
    let model = analysis.model();
    assert_eq!(
      shown(model, argument_default(model, "Query", "ok", "f")),
      held,
      "---\n{text}"
    );
  }

  let text = [
    "type Query {\n  ok(f: O = {a: 1, b: \"x\"}): Int\n}\n",
    ONE_OF,
  ]
  .concat();
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (DEFAULT, at(schema, &text, "{a: 1, b: \"x\"}", 0)),
      (DEFAULT, at(schema, &text, "\"x\"", 0)),
    ])
  );

  let text = ["type Query {\n  ok(f: O = {z: null}): Int\n}\n", ONE_OF].concat();
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNDEFINED, after(schema, &text, "{", "z"))]
  );
}

/// The literals a OneOf input object accepts: one field each, at every position, and a lone one
/// where a list is expected. Refuses a list's elements counted as one literal's fields, and a lone
/// object refused.
#[test]
fn a_one_of_literal_of_one_field_is_accepted() {
  let text = [
    "type Query {\n  ok(\n    a: O = {a: 1}\n    b: O = {b: 2}\n    none: O = null\n    \
     list: [O] = [{a: 1}, {b: 2}]\n    single: [O] = {a: 1}\n    nested: Outer = {o: {b: 2}}\n  \
     ): Int @d(f: {a: 1})\n}\ninput Outer {\n  o: O = {a: 1}\n}\n\
     directive @d(f: O) on FIELD_DEFINITION\n",
    ONE_OF,
  ]
  .concat();
  let analysis = analyze_text(&text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(
    shown(model, argument_default(model, "Query", "ok", "single")),
    "List [Object {a: Int 1}]"
  );
  assert_eq!(
    shown(model, argument_default(model, "Query", "ok", "list")),
    "List [Object {a: Int 1}, Object {b: Int 2}]"
  );
}

/// A copy of a default that is an object literal is an object, not written, located at the
/// default's literal; its entry keeps where its name is written in the default, and its value is
/// not written either. Refuses a copy located at the application, and a copied entry that lost its
/// name.
#[test]
fn an_object_in_a_default_is_copied_whole() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  x: Int\n}\n\
              directive @d(a: I = { x: 1 }) on OBJECT\ntype T @d {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let argument = arguments_of(model, "T")[0];
  assert!(!argument.written());
  let copy = model.value(argument.value());
  assert_eq!(shown(model, copy.id()), "Object {x: Int 1}");
  assert!(!copy.written());
  assert_eq!(copy.location(), at(schema, text, "{ x: 1 }", 0));
  let entries = entries(model, copy.id());
  assert_eq!(entries[0].name(), Some(after(schema, text, "{ ", "x")));
  let value = model.value(entries[0].value());
  assert!(!value.written());
  assert_eq!(value.location(), after(schema, text, "x: ", "1"));
}

/// A default whose object literal holds an unknown field and a field that does not fit is
/// reported where it is written, once each, and its fifty copies say nothing. Refuses a report at
/// each copy.
#[test]
fn a_copy_of_an_object_says_nothing() {
  let applied = "@bad ".repeat(50);
  let text = format!(
    "type Query {{\n  x: Int\n}}\ninput I {{\n  x: Int\n}}\n\
     directive @bad(a: I = {{ y: 1, x: \"s\" }}) repeatable on OBJECT\n\
     type T {applied}{{\n  f: Int\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (UNDEFINED, after(schema, &text, "{ ", "y")),
      (DEFAULT, at(schema, &text, "\"s\"", 0)),
    ])
  );
  let model = analysis.model();
  let applications = model.def(type_def(model, "T")).applications();
  assert_eq!(applications.len(), 50);
  for &application in applications {
    let arguments = model.application(application).arguments();
    assert_eq!(shown(model, arguments[0].value()), "Object {x: ?}");
  }
}

/// An input object is OneOf when the package that wrote it applies `@oneOf`, in its definition or
/// in an extension of its own, and not when another package's extension does. The codes are read
/// by name: the orphan rule speaks in the third program, and the other rules may refuse the
/// second's extension. Refuses OneOf read off the definition's contribution alone, and another
/// package's `@oneOf` making a type OneOf.
#[test]
fn a_package_makes_its_own_input_object_one_of() {
  let values_codes = [DEFAULT, ARGUMENT];

  let text = "type Query {\n  x: Int\n}\ninput O @oneOf {\n  a: Int\n  b: Int\n}\n\
              input H {\n  o: O = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &values_codes),
    [(DEFAULT, at(schema, text, "{}", 0))]
  );

  let text = "type Query {\n  x: Int\n}\ninput P {\n  a: Int\n  b: Int\n}\nextend input P @oneOf\n\
              input H {\n  p: P = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &values_codes),
    [(DEFAULT, at(schema, text, "{}", 0))]
  );

  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new(
      "schema.graphqlx",
      "type Query {\n  x: Int\n}\nextend input ^::dep::Q @oneOf\ninput H {\n  q: ^::dep::Q = {}\n}\n",
    )])
    .with_dependencies(["dep"]);
  let dep: Package<&str> =
    Package::new("dep", Layout::Flat, Tier::Std).with_files([SourceFile::new(
      "dep.graphqlx",
      "input Q {\n  a: Int\n  b: Int\n}\n",
    )]);
  let analysis = analyze(&root, &[dep], &Limits::new());
  assert!(
    found_of(&analysis, &values_codes).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "H", "q"), "Object {}");
}

/// Each required field an object literal leaves out is reported, a step each: three at three
/// steps, and at one step one report and the literal past the budget. Refuses a report nobody pays
/// for.
#[test]
fn a_required_field_left_out_costs_a_step() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int!\n  b: Int!\n  c: Int!\n}\n\
              input J {\n  x: I = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(3));
  let schema = schema_file(&analysis);
  let object = at(schema, text, "{}", 0);
  assert_eq!(
    found(&analysis),
    [(REQUIRED, object), (REQUIRED, object), (REQUIRED, object)]
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(1));
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![(REQUIRED, object), (LIMIT, object)])
  );
}

/// A message quotes what the input wrote only by a bounded excerpt.
#[test]
fn long_spellings_are_quoted_as_excerpts() {
  // A field named with four kilobytes, which the input object does not declare.
  let long = "y".repeat(4096);
  let text = format!(
    "type Query {{\n  x: Int\n}}\ninput I {{\n  x: Int\n}}\n\
     input J {{\n  i: I = {{ {long}: 1 }}\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  assert_eq!(analysis.diagnostics().len(), 1);
  for diagnostic in analysis.diagnostics() {
    assert!(diagnostic.to_string().len() < 1024);
  }
}
