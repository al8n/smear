//! The GraphQLx analyzer's composite values: an input object's value, its fields found by name and
//! kept in the input object's order, with copies of the defaults of the fields it leaves out; the
//! codes GraphQL gives an object literal's unknown and required fields; an input object its own
//! package makes OneOf; the count of a copy, which holds copies of defaults that reach other
//! defaults, made before the copy and charged read by read; and a set and a map, each where its
//! collection is expected and nowhere else.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, Cx, DefId, Diagnostic, Layout, Limits, MemberId, Model,
    Namespace, ObjectEntry, Package, PackageId, Packages, Path, PathRoot, SourceFile, SourceId,
    Sym, Tier, TyKind, ValueId, ValueKind, Visitor, analyze, test_support, visit,
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
/// A collection literal out of its place.
const COLLECTION: &str = "smear::graphqlx::collection-literal";

/// The codes this phase reports values with. A program the other rules refuse, a cycle of
/// defaults or a OneOf field with a default among them, is asserted on these alone.
const PHASE: [&str; 6] = [DEFAULT, ARGUMENT, UNDEFINED, REQUIRED, LIMIT, COLLECTION];

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

/// A field the literal leaves out takes a copy of its default, with no name, not written, located
/// at the default, where the field stands in the input object's order. Refuses a copy located at
/// the object, and the filled entries after the written ones.
#[test]
fn a_field_left_out_takes_its_default() {
  let text = "type Query {\n  range(range: Range = {max: 9, min: 1}): Int\n}\n\
              input Range {\n  min: Int = 0\n  max: Int\n  step: Int = 1\n}\n\
              input R2 {\n  a: Int = 0\n  b: Int\n}\ninput H {\n  r: R2 = { b: 1 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let range = argument_default(model, "Query", "range", "range");
  assert_eq!(
    shown(model, range),
    "Object {min: Int 1, max: Int 9, step= Int 1}"
  );
  let step = entries(model, range)[2];
  assert_eq!(step.name(), None);
  let filled = model.value(step.value());
  assert!(!filled.written());
  assert_eq!(filled.location(), after(schema, text, "step: Int = ", "1"));

  let r = default(model, "H", "r");
  assert_eq!(shown(model, r), "Object {a= Int 0, b: Int 1}");
  let entries = entries(model, r);
  assert!(entries[0].value() < entries[1].value());
}

/// Two objects that leave one field out each have a copy of its default of their own. Refuses a
/// row shared.
#[test]
fn each_object_has_its_own_copy() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int = 5\n}\n\
              input J {\n  x: I = {}\n  y: I = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let own = default(model, "I", "a");
  let x = entries(model, default(model, "J", "x"))[0].value();
  let y = entries(model, default(model, "J", "y"))[0].value();
  assert!(own != x && own != y && x != y);
  for copy in [x, y] {
    assert_eq!(shown(model, copy), shown(model, own));
    assert_eq!(model.value(copy).location(), model.value(own).location());
    assert_eq!(model.value(copy).ty(), model.value(own).ty());
    assert!(!model.value(copy).written());
  }
}

/// A default that leaves a field out is filled from that field's default, and so is its copy:
/// every row of the copy not written, each at its own default. Refuses a copy that fills one level.
#[test]
fn a_default_inside_a_default_is_filled() {
  let text = "type Query {\n  f(a: A = {}): Int\n}\ninput A {\n  b: B = {}\n}\n\
              input B {\n  n: Int = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  assert_eq!(shown_default(model, "A", "b"), "Object {n= Int 1}");
  let f = argument_default(model, "Query", "f", "a");
  assert_eq!(shown(model, f), "Object {b= Object {n= Int 1}}");
  let b = entries(model, f)[0].value();
  let n = entries(model, b)[0].value();
  assert!(model.value(f).written());
  assert!(!model.value(b).written() && !model.value(n).written());
  assert_eq!(
    model.value(b).location(),
    after(schema, text, "b: B = ", "{}")
  );
  assert_eq!(
    model.value(n).location(),
    after(schema, text, "n: Int = ", "1")
  );
}

/// A default that sets its own field reads no default of that field: it is no cycle, and is
/// filled. Refuses a count that reads the default of a field the literal sets.
#[test]
fn a_default_that_sets_its_own_field_is_no_cycle() {
  let text =
    "type Query {\n  f(v: C = {}): Int\n}\ninput C {\n  c: C = { c: null }\n  n: Int = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    shown(model, argument_default(model, "Query", "f", "v")),
    "Object {c= Object {c: Null null, n= Int 1}, n= Int 1}"
  );
}

/// Filling stops by field: a field is filled again under another field of the chain, and the
/// chain ends where a literal sets the field. Refuses the stop by object, which leaves a `z` out.
#[test]
fn the_stop_is_by_field() {
  let text = "type Query {\n  x: Int\n}\ninput A {\n  p: B = {}\n  q: B = { a: null }\n}\n\
              input B {\n  a: A = { p: null }\n  z: Int = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    found_of(&analysis, &PHASE).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    shown_default(model, "A", "p"),
    "Object {a= Object {p: Null null, q= Object {a: Null null, z= Int 1}}, z= Int 1}"
  );
}

/// A default whose copy would hold a copy of itself is not copied, and this phase says nothing:
/// the cycle is the other rules' to report. So is one that only reads such a default. Refuses
/// filling until a limit, a diagnostic of this phase, and a poison that does not reach what reads
/// it.
#[test]
fn a_default_that_needs_itself_is_not_filled() {
  let text = "type Query {\n  x: Int\n}\ninput A {\n  b: B = {}\n}\ninput B {\n  a: A = {}\n}\n\
              directive @d(a: A = {}) on OBJECT\ntype T @d {\n  f: Int\n}\n\
              input C2 {\n  c: C2 = { n: 1 }\n  n: Int\n}\n\
              input A2 {\n  b: B2 = {}\n}\ninput B2 {\n  b: B2 = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    found_of(&analysis, &PHASE).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "A", "b"), "Object {}");
  assert!(arguments_of(model, "T").is_empty());
  assert_eq!(shown_default(model, "C2", "c"), "Object {n: Int 1}");
  assert_eq!(shown_default(model, "A2", "b"), "Object {}");
}

/// A OneOf input object's fields left out are never filled. The codes are read by name: the other
/// rules refuse the definition. Refuses a OneOf's default filled.
#[test]
fn a_one_of_objects_fields_are_never_filled() {
  let text = "type Query {\n  x: Int\n}\ninput O @oneOf {\n  a: Int = 1\n  b: Int\n}\n\
              input H {\n  f: O = { b: 2 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    found_of(&analysis, &PHASE).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    shown_default(analysis.model(), "H", "f"),
    "Object {b: Int 2}"
  );
}

/// A generic input object's field is coerced against the type it is written with, which names a
/// parameter: it keeps its literal, typed with the parameter, and nothing is said. Refuses a field
/// read under its object's arguments.
#[test]
fn a_generic_input_objects_field_keeps_its_literal() {
  let text = "type Query {\n  x: Int\n}\ninput In<T> {\n  v: T\n  n: Int = 1\n}\n\
              input H {\n  x: In<i32> = { v: 1 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let x = default(model, "H", "x");
  assert_eq!(shown(model, x), "Object {v: Literal(Int) 1, n= Int 1}");
  let v = entries(model, x)[0].value();
  let written = model
    .member(input_field(model, "In", "v"))
    .ty()
    .expect("`v` has a type");
  assert_eq!(model.value(v).ty(), model.type_expr(written).ty());
}

/// A later field of a name an earlier field has is never asked for, though it is required; the
/// control beside it is. Refuses a report for the later field.
#[test]
fn a_later_field_of_a_name_already_taken_is_not_asked_for() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int\n  a: Int!\n}\ninput J {\n  b: Int!\n}\n\
              input H {\n  i: I = {}\n  j: J = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &[REQUIRED]),
    [(REQUIRED, after(schema, text, "j: J = ", "{}"))]
  );
  assert_eq!(shown_default(analysis.model(), "H", "i"), "Object {}");
}

/// A count is made once and kept: the default `{}` where it is written fills two fields, 2 steps;
/// `T`'s copy reads two counts, 2 steps, and makes three rows, 3; `U`'s copy, its count kept, is 3
/// more. At 10 all is paid; at 9 `U` alone is past the budget. At 6 `T`'s two reads stay spent
/// though its copy is refused, so `U` cannot be paid either. Refuses a count made again for each
/// copy, which fails at 10; one whose reads nobody pays for, which passes at 9; and reads paid
/// together with the copy as one charge, which fills `U` at 6.
#[test]
fn a_default_is_counted_once() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  x: Int = 1\n  y: Int = 2\n}\n\
              directive @d(a: I = {}) on OBJECT\ntype T @d {\n  f: Int\n}\ntype U @d {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(10));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(
    shown(model, arguments_of(model, "U")[0].value()),
    "Object {x= Int 1, y= Int 2}"
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(9));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, after(schema, text, "type U @", "d"))]
  );
  let model = analysis.model();
  assert_eq!(arguments_of(model, "T").len(), 1);
  assert!(arguments_of(model, "U").is_empty());

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(6));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, after(schema, text, "type T @", "d")),
      (LIMIT, after(schema, text, "type U @", "d")),
    ]
  );
}

/// Sixty-four input objects that each default two fields to `{}` of the next stand for about 2^64
/// rows: each count is kept, and is never added up past the budget, so the analysis ends, every
/// diagnostic is the budget's, and the application that asks for the whole has no argument.
/// Refuses a copy made before its count, which leaves part of one behind, and a count that neither
/// keeps what it counted nor stops at the budget, which does not end.
#[test]
fn nested_defaults_are_counted_before_they_are_copied() {
  let mut text = String::from("type Query {\n  x: Int\n}\ninput B63 {\n  z: Int = 1\n}\n");
  for level in (0..63).rev() {
    let next = level + 1;
    text.push_str(&format!(
      "input B{level} {{\n  x: B{next} = {{}}\n  y: B{next} = {{}}\n}}\n"
    ));
  }
  text.push_str("directive @d(a: B0 = {}) on OBJECT\ntype T @d {\n  f: Int\n}\n");
  let analysis = analyze_text(&text, &Limits::new().with_max_value_steps(1000));
  let found = found(&analysis);
  assert!(!found.is_empty());
  for (code, at) in found {
    assert_eq!(code, LIMIT, "{at:?}");
  }
  let model = analysis.model();
  assert!(arguments_of(model, "T").is_empty());
  let (_, _, values, _) = test_support::counts(model);
  assert!(values < 3000, "{values} values");
}

/// A literal starved while its defaults are filled keeps what it had: the field filled before the
/// budget ran out, and every field it writes. Refuses a second `resource-limit`, and what was
/// filled taken back.
#[test]
fn a_starved_object_keeps_what_it_had() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int = 1\n  b: Int = 2\n  c: Int\n}\n\
              input J {\n  x: I = { c: 3 }\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(1));
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(LIMIT, at(schema, text, "{ c: 3 }", 0))]);
  assert_eq!(
    shown_default(analysis.model(), "J", "x"),
    "Object {a= Int 1, c: Int 3}"
  );
}

/// The required fields a literal leaves out are asked for before its defaults are filled: at one
/// step, `b` is reported and `a` is not filled. Refuses a budget spent on completing a value that
/// is wrong.
#[test]
fn required_fields_are_asked_before_defaults_are_filled() {
  let text =
    "type Query {\n  x: Int\n}\ninput I {\n  a: Int = 1\n  b: Int!\n}\ninput J {\n  x: I = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(1));
  let schema = schema_file(&analysis);
  let object = at(schema, text, "{}", 0);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![(REQUIRED, object), (LIMIT, object)])
  );
  assert_eq!(shown_default(analysis.model(), "J", "x"), "Object {}");
}

/// A default that needs itself costs a step each time it is asked for, besides the reads that met
/// it: seven in all. `A.b`'s default reads `B.a`'s count, which reads `A.b`'s, which meets itself,
/// 2, and pays 1 for the poisoned default; `B.a`'s and `@d`'s defaults pay 1 each; `T`'s copy reads
/// 1 and pays 1. Refuses a default that needs itself passed by for nothing.
#[test]
fn a_default_that_needs_itself_costs_a_step() {
  let text = "type Query {\n  x: Int\n}\ninput A {\n  b: B = {}\n}\ninput B {\n  a: A = {}\n}\n\
              directive @d(a: A = {}) on OBJECT\ntype T @d {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(7));
  assert!(
    found_of(&analysis, &[LIMIT]).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(6));
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &[LIMIT]),
    [(LIMIT, after(schema, text, "type T @", "d"))]
  );
}

/// Object literals that set every field cost nothing to complete, in a default and in an
/// argument, and neither do sets and maps. Refuses a charge for a written row.
#[test]
fn written_objects_cost_no_step() {
  let text = "type Query {\n  x: Int\n}\ninput I {\n  a: Int = 1\n  b: Int!\n}\n\
              input J {\n  x: I = { a: 2, b: 3 }\n  y: I = { b: 4, a: 5 }\n  \
              s: HashSet<Int> = set!{1, 2}\n  m: HashMap<string, Int> = map!{\"a\" => 1}\n}\n\
              directive @d(i: I) on OBJECT\ntype T @d(i: { a: 6, b: 7 }) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(0));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A set literal where a set is expected is a set of its elements, each coerced against the set's
/// one argument, non-null there; a map literal where a map is expected a map of its entries, each
/// key against the first argument and each value against the second. Refuses a key coerced against
/// the value's type.
#[test]
fn a_set_literal_is_a_set_and_a_map_literal_a_map() {
  let text = "type Query {\n  x: Int\n}\nscalar Bag<T> @set\n\
              input H {\n  tags: HashSet<string> = set!{\"a\", \"b\"}\n  \
              m: HashMap<string, Int> = map!{\"a\" => 1, \"b\" => null}\n  \
              bt: BTreeSet<Int> = set!{1}\n  bm: BTreeMap<Int, string> = map!{1 => \"x\"}\n  \
              bag: Bag<Int> = set!{2}\n  empty: HashSet<Int> = set!{}\n  \
              none: HashMap<Int, Int> = map!{}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let tags = default(model, "H", "tags");
  assert_eq!(shown(model, tags), "Set {String \"a\", String \"b\"}");
  let TyKind::Named { args, .. } = model.ty(model.value(tags).ty()).kind() else {
    panic!("`tags` has no named type");
  };
  let ValueKind::Set(elements) = model.value(tags).kind() else {
    panic!("`tags` is no set");
  };
  for element in elements {
    assert_eq!(model.value(*element).ty(), args[0]);
  }
  assert!(model.ty(args[0]).non_null());

  let m = default(model, "H", "m");
  assert_eq!(
    shown(model, m),
    "Map {String \"a\" => Int 1, String \"b\" => Null null}"
  );
  let ValueKind::Map(entries) = model.value(m).kind() else {
    panic!("`m` is no map");
  };
  let ids: Vec<(u32, u32)> = entries
    .iter()
    .map(|&(key, item): &(ValueId, ValueId)| (key.get(), item.get()))
    .collect();
  let first = m.get();
  assert_eq!(
    ids,
    [(first + 1, first + 2), (first + 3, first + 4)],
    "the map, then each key and then its value"
  );

  for (field, held) in [
    ("bt", "Set {Int 1}"),
    ("bm", "Map {Int 1 => String \"x\"}"),
    ("bag", "Set {Int 2}"),
    ("empty", "Set {}"),
    ("none", "Map {}"),
  ] {
    assert_eq!(shown_default(model, "H", field), held, "`{field}`");
  }
}

/// A set's element and a map's key are never `null`, however the type writes them: `null` is
/// refused with the site's code; a map's value is nullable as it is written. Refuses an element's
/// nullness read off the argument as it is written, and a value's read off its key.
#[test]
fn a_collections_element_is_never_null() {
  let text = "type Query {\n  x: Int\n}\ninput H {\n  s: HashSet<Int> = set!{null}\n  \
              k: HashMap<Int, Int> = map!{null => 1}\n  \
              v: HashMap<string, Int> = map!{\"a\" => null}\n  \
              w: HashMap<string, Int!> = map!{\"a\" => null}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (DEFAULT, after(schema, text, "set!{", "null")),
      (DEFAULT, after(schema, text, "map!{", "null")),
      (
        DEFAULT,
        after(schema, text, "Int!> = map!{\"a\" => ", "null")
      ),
    ])
  );
  let model = analysis.model();
  for (field, held) in [
    ("s", "Set {?}"),
    ("k", "Map {? => Int 1}"),
    ("v", "Map {String \"a\" => Null null}"),
    ("w", "Map {String \"a\" => ?}"),
  ] {
    assert_eq!(shown_default(model, "H", field), held, "`{field}`");
  }
}

/// A collection takes its own literal and `null` alone, and a collection literal is a value of its
/// collection alone: either way `collection-literal`, at the literal, and the value unresolved. A
/// set literal where a list of `Int` is expected is a lone value that does not fit. Refuses the
/// site's code for either direction.
#[test]
fn a_collection_takes_its_own_literal_and_no_other() {
  let text = "type Query {\n  x: Int\n}\nenum Color {\n  RED\n}\ninput I {\n  a: Int\n}\n\
              input H {\n  l: HashSet<Int> = [1]\n  i: HashSet<Int> = 1\n  s: HashSet<Int> = \"x\"\n  \
              o: HashSet<Int> = {a: 1}\n  m: HashSet<Int> = map!{1 => 1}\n  \
              hm: HashMap<Int, Int> = set!{1}\n  n: Int = set!{2}\n  st: String = set!{3}\n  \
              e: Color = set!{4}\n  io: I = set!{5}\n  b: bytes = set!{6}\n  lone: [Int] = set!{7}\n  \
              nul: HashSet<Int> = null\n  nn: HashSet<Int>! = null\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (COLLECTION, at(schema, text, "[1]", 0)),
      (COLLECTION, after(schema, text, "i: HashSet<Int> = ", "1")),
      (COLLECTION, at(schema, text, "\"x\"", 0)),
      (COLLECTION, at(schema, text, "{a: 1}", 0)),
      (COLLECTION, at(schema, text, "map!{1 => 1}", 0)),
      (COLLECTION, at(schema, text, "set!{1}", 0)),
      (COLLECTION, at(schema, text, "set!{2}", 0)),
      (COLLECTION, at(schema, text, "set!{3}", 0)),
      (COLLECTION, at(schema, text, "set!{4}", 0)),
      (COLLECTION, at(schema, text, "set!{5}", 0)),
      (COLLECTION, at(schema, text, "set!{6}", 0)),
      (COLLECTION, at(schema, text, "set!{7}", 0)),
      (DEFAULT, after(schema, text, "nn: HashSet<Int>! = ", "null")),
    ])
  );
  let model = analysis.model();
  for field in [
    "l", "i", "s", "o", "m", "hm", "n", "st", "e", "io", "b", "nn",
  ] {
    assert_eq!(shown_default(model, "H", field), "?", "`{field}`");
  }
  assert_eq!(shown_default(model, "H", "lone"), "List [?]");
  assert_eq!(shown_default(model, "H", "nul"), "Null null");
  let list = analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| diagnostic.primary() == at(schema, text, "[1]", 0))
    .expect("the list's diagnostic");
  assert_eq!(list.help(), Some("a set is written `set!{ … }`"));
}

/// A custom scalar that is no collection keeps a set or a map literal, as it keeps any literal,
/// and nothing is said. Refuses a collection literal refused where no collection is expected, read
/// onto a scalar that is no collection.
#[test]
fn a_custom_scalar_keeps_a_collection_literal() {
  let text = "type Query {\n  x: Int\n}\nscalar Any\n\
              input H {\n  s: Any = set!{1}\n  m: Any = map!{a => 1}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  assert_eq!(shown_default(model, "H", "s"), "Literal(Set) set!{1}");
  assert_eq!(shown_default(model, "H", "m"), "Literal(Map) map!{a => 1}");
}

/// An extern collection is a collection: it takes its own literal, and no other. Refuses an extern
/// collection kept as a custom scalar's literal.
#[test]
fn an_extern_collection_takes_its_literal_too() {
  let text = "type Query {\n  x: Int\n}\n\
              scalar IndexSet<T> @set @extern(lang: \"rust\", type: \"indexmap::IndexSet\")\n\
              input H {\n  s: IndexSet<Int> = set!{1}\n  l: IndexSet<Int> = [1]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &PHASE),
    [(COLLECTION, at(schema, text, "[1]", 0))]
  );
  assert_eq!(shown_default(analysis.model(), "H", "s"), "Set {Int 1}");
}

/// A collection whose type has another count of arguments than its collection, and one its own
/// package makes both a set and a map, take no value: each literal is kept, and the collection's
/// arity is the one diagnostic. Refuses a second diagnostic for the definition's mistake.
#[test]
fn a_collection_of_the_wrong_count_takes_no_value() {
  let text = "type Query {\n  x: Int\n}\nscalar Bad<A, B> @set\nscalar Both<T> @set @map\n\
              input H {\n  x: Bad<Int, Int> = set!{1}\n  y: Both<Int> = set!{1}\n  \
              z: Both<Int> = map!{1 => 1}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let codes: Vec<&str> = found(&analysis)
    .iter()
    .map(|&(code, _): &(&'static str, Location)| code)
    .collect();
  assert_eq!(
    codes,
    [
      "smear::graphqlx::collection-arity",
      "smear::graphqlx::collection-arity",
    ]
  );
  let model = analysis.model();
  assert_eq!(shown_default(model, "H", "x"), "Literal(Set) set!{1}");
  assert_eq!(shown_default(model, "H", "y"), "Literal(Set) set!{1}");
  assert_eq!(shown_default(model, "H", "z"), "Literal(Map) map!{1 => 1}");
}

/// A copy of a set and a copy of a map are whole: three rows and four, the `1` under a list no
/// literal writes, every row not written. The default's own `1` stands for a list, one step where
/// it is written; the two copies are three and four: at eight all is filled, and at seven the
/// map's copy is refused, the application past the budget. Refuses a count that misses the list a
/// lone value stands for under a map.
#[test]
fn a_copy_of_a_collection_is_whole() {
  let text = "type Query {\n  x: Int\n}\n\
              directive @d(s: HashSet<Int> = set!{1, 2}, m: HashMap<string, [Int]> = map!{\"a\" => 1}) \
              on OBJECT\ntype T @d {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(8));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let arguments = arguments_of(model, "T");
  let (s, m) = (arguments[0].value(), arguments[1].value());
  assert_eq!(shown(model, s), "Set {Int 1, Int 2}");
  assert_eq!(shown(model, m), "Map {String \"a\" => List [Int 1]}");
  assert_eq!(m.get(), s.get() + 3);
  for id in s.get()..m.get() + 4 {
    assert!(!model.value(ValueId::new(id)).written(), "value {id}");
  }
  let list = ValueId::new(m.get() + 2);
  assert_eq!(shown(model, list), "List [Int 1]");
  assert_eq!(
    model.value(list).location(),
    after(schema, text, "\"a\" => ", "1")
  );

  let analysis = analyze_text(text, &Limits::new().with_max_value_steps(7));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, after(schema, text, "type T @", "d"))]
  );
  let model = analysis.model();
  let arguments = arguments_of(model, "T");
  assert_eq!(arguments.len(), 1);
  assert_eq!(model.member(arguments[0].argument()).name(), "s");
}

/// Composite values are numbered in the walk's order: an object before its entries' values, in
/// the input object's order, a default's copy where its field stands; a set before its elements;
/// a map before each key and then its value. `k` is the first id after `R`'s own two defaults.
#[test]
fn composite_values_are_numbered_in_the_walks_order() {
  let text = "type Query {\n  x: Int\n}\ninput R {\n  min: Int = 0\n  max: Int\n  step: Int = 1\n}\n\
              directive @d(r: R = { max: 9 }, s: HashSet<Int> = set!{1, 2}, \
              m: HashMap<string, [Int]> = map!{\"a\" => 1}) on OBJECT\n\
              type T @d(r: { step: 5, min: 2 }) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let k = default(model, "R", "step").get() + 1;
  let id = |offset: u32| -> ValueId { ValueId::new(k + offset) };
  let values_of = |object: ValueId| -> Vec<u32> {
    entries(model, object)
      .iter()
      .map(|entry: &ObjectEntry| entry.value().get())
      .collect()
  };

  let d = model
    .lookup(Namespace::Directive, "@^::app::d")
    .expect("`@d`");
  let defaults: Vec<u32> = model
    .def(d)
    .arguments()
    .into_iter()
    .map(|argument: MemberId| model.member(argument).default().expect("a default").get())
    .collect();
  assert_eq!(defaults, [k, k + 4, k + 7]);
  assert_eq!(
    shown(model, id(0)),
    "Object {min= Int 0, max: Int 9, step= Int 1}"
  );
  assert_eq!(values_of(id(0)), [k + 1, k + 2, k + 3]);
  assert_eq!(shown(model, id(4)), "Set {Int 1, Int 2}");
  let ValueKind::Map(pairs) = model.value(id(7)).kind() else {
    panic!("`m` is no map");
  };
  assert_eq!(pairs.to_vec(), [(id(8), id(9))]);
  assert_eq!(shown(model, id(9)), "List [Int 1]");

  let applied: Vec<u32> = arguments_of(model, "T")
    .iter()
    .map(|argument: &ApplicationArgument| argument.value().get())
    .collect();
  assert_eq!(applied, [k + 11, k + 14, k + 17]);
  assert_eq!(values_of(id(11)), [k + 12, k + 13]);
  assert_eq!(shown(model, id(14)), "Set {Int 1, Int 2}");
  assert_eq!(shown(model, id(17)), "Map {String \"a\" => List [Int 1]}");

  let reached = values(&analysis, Packages::All);
  for (index, reached) in reached.iter().enumerate() {
    assert_eq!(reached.get() as usize, index);
  }
}
