//! The GraphQLx analyzer's bounds on the shapes a program writes, how many arguments a field and a
//! directive declare and how many list and non-null wrappers a written type has, and on the values
//! an analysis reads and makes, each at its default, tightened and relaxed.
//!
//! The three shape bounds refuse a shape and stop no work: a refused field, directive or type keeps
//! everything it writes, so nothing downstream sees less than the program says. At 64, 64 and 15
//! they refuse each document GraphQL's schema builder refuses for a capacity ceiling, which
//! `analysis_differential.rs` holds over the corpus.
//!
//! `Limits::max_values` charges each literal a supplied package writes where lowering reads it, and
//! each list a lone value stands for and each value of a copy where coercion makes it. A written
//! value it cannot pay for is one unresolved value, read no further, so the model holds at most the
//! bound's values beyond the built-in library's own and one for each value refused, which
//! `the_model_holds_what_it_says` measures.
//!
//! Each test builds the shapes and the values it needs from counts, as a fixture of
//! `support/validator_corpus.rs` writes a field of sixty-five arguments out, so the number a test
//! uses is readable beside the number it tests.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, DefId, Layout, Limits, MemberId, Model, Namespace, ObjectEntry,
    Options, Package, PackageId, SourceFile, SourceId, Tier, TyId, TyKind, ValueId, ValueKind,
    analyze, analyze_with, test_support,
  },
  diagnostic::{Diagnose, Location},
  lexer::{limits::SyntacticLimits, tokora::SimpleSpan},
  parser::graphqlx::Syntactic,
};

/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

/// What every report of `Limits::max_values` quotes.
const VALUES: &str = "`Limits::max_values`";

/// What a report of a written value `Limits::max_values` refused says, and no other report.
const UNREAD: &str = "so none of it is read";

/// GraphQL's code for a default that does not fit.
const DEFAULT: &str = "smear::schema::invalid-default-value";

/// GraphQL's code for a schema with no query root, which a library analyzed as the root package
/// has.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// The three shape bounds at the ceilings of GraphQL's schema builder: sixty-four arguments for a
/// field, sixty-four for a directive and fifteen wrappers for a type.
const PARITY: Limits = Limits::new()
  .with_max_field_arguments(64)
  .with_max_directive_arguments(64)
  .with_max_type_wrappers(15);

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_text(text: &str, limits: &Limits, options: &Options) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits, options)
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

/// Where the name of an argument is, in the `nth` occurrence of the text `argument` that writes it,
/// as `c: Int`: the name alone, which is what a label at an argument is.
fn at_name(source: SourceId, text: &str, argument: &str, nth: usize) -> Location {
  let name = argument
    .find(':')
    .expect("an argument is written `name: Type`");
  let whole = at(source, text, argument, nth);
  let start = whole.span().expect("a span").start();
  Location::new(source.get(), SimpleSpan::new(start, start + name))
}

/// Each diagnostic whose code is `code`: its primary location and its first label's location, in
/// the list's order.
fn found(analysis: &Analysis, code: &str) -> Vec<(Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic| {
      (
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// The message of each diagnostic whose code is `code`, in the list's order.
fn messages(analysis: &Analysis, code: &str) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic| diagnostic.to_string())
    .collect()
}

/// The one message of `messages`.
fn only(mut messages: Vec<String>) -> String {
  assert_eq!(messages.len(), 1, "{messages:#?}");
  messages.remove(0)
}

/// Whether `analysis` says nothing but that the root package has no query root: the one thing a
/// package with no file or no query says, which a consumer analyzing a library waives.
fn clean(analysis: &Analysis) -> bool {
  analysis
    .diagnostics()
    .iter()
    .all(|diagnostic| diagnostic.code().as_str() == NO_QUERY_ROOT)
}

/// `count` arguments, `a0: Int, a1: Int, …`.
fn arguments(count: usize) -> String {
  (0..count)
    .map(|index: usize| format!("a{index}: Int"))
    .collect::<Vec<String>>()
    .join(", ")
}

/// A program whose one field, `wide`, declares `count` arguments.
fn wide_field(count: usize) -> String {
  format!("type Query {{\n  wide({}): Int\n}}\n", arguments(count))
}

/// A program whose one directive, `@wide`, declares `count` arguments.
fn wide_directive(count: usize) -> String {
  format!(
    "type Query {{\n  x: Int\n}}\ndirective @wide({}) on FIELD_DEFINITION\n",
    arguments(count)
  )
}

/// A program whose one field, `deep`, has the type of `lists` lists around `Int`, each nullable.
fn deep_field(lists: usize) -> String {
  format!(
    "type Query {{\n  deep: {}Int{}\n}}\n",
    "[".repeat(lists),
    "]".repeat(lists)
  )
}

/// The definition the root package's `name` names in the type namespace.
fn type_def(model: &Model, name: &str) -> DefId {
  let canonical = format!("^::app::{name}");
  model
    .lookup(Namespace::Type, &canonical)
    .unwrap_or_else(|| panic!("no type `{canonical}`"))
}

/// The field `field` of the root package's type `owner`.
fn field_of(model: &Model, owner: &str, field: &str) -> MemberId {
  model
    .def(type_def(model, owner))
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{owner}.{field}`"))
}

/// How many arguments the field `field` of the root package's type `owner` holds.
fn field_args(model: &Model, owner: &str, field: &str) -> u32 {
  model
    .member(field_of(model, owner, field))
    .arguments()
    .len()
}

/// The type `id`, written out: a definition's name, a list in `[]`, and `!` after a type that is
/// non-null.
fn shown(model: &Model, id: TyId) -> String {
  let ty = model.ty(id);
  let mut text = match ty.kind() {
    TyKind::Named(named_ty) => String::from(model.def(named_ty.def()).name()),
    TyKind::List(element) => format!("[{}]", shown(model, element)),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  text
}

/// The type of the field `field` of the root package's type `owner`, written out.
fn field_shown(model: &Model, owner: &str, field: &str) -> String {
  let written = model
    .member(field_of(model, owner, field))
    .ty()
    .unwrap_or_else(|| panic!("`{owner}.{field}` has no type"));
  shown(model, model.type_expr(written).ty())
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

/// The value `id`, written out: a coerced value as its kind's name and its text, `Int 1` and
/// `List [Int 1]`; a byte string as `Bytes` and its bytes; an input object's entries, each its
/// field's name, `:` after one its literal writes and `=` after a copy of a default, and its
/// value; a literal kept as written as `Literal(List) [1]`; and an unresolved value as `?`.
fn valued(model: &Model, id: ValueId) -> String {
  let value = model.value(id);
  match value.kind() {
    ValueKind::Null => String::from("Null null"),
    ValueKind::Int(int) => format!("Int {}", int.text()),
    ValueKind::String(text) => format!("String {text:?}"),
    ValueKind::Bytes(bytes) => format!("Bytes {bytes:?}"),
    ValueKind::List(items) => {
      let items: Vec<String> = items
        .iter()
        .map(|item: &ValueId| valued(model, *item))
        .collect();
      format!("List [{}]", items.join(", "))
    }
    ValueKind::Object(entries) => {
      let entries: Vec<String> = entries
        .iter()
        .map(|entry: &ObjectEntry| {
          let mark = if entry.name().is_some() { ":" } else { "=" };
          format!(
            "{}{mark} {}",
            model.member(entry.field()).name(),
            valued(model, entry.value())
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

/// The default of the input field `name` of the root package's input object `owner`.
fn input_default(model: &Model, owner: &str, name: &str) -> ValueId {
  let field = model
    .def(type_def(model, owner))
    .input_fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == name)
    .unwrap_or_else(|| panic!("no input field `{owner}.{name}`"));
  model
    .member(field)
    .default()
    .unwrap_or_else(|| panic!("`{owner}.{name}` has no default"))
}

/// The default of the argument `name` of the field `field` of the root package's type `owner`,
/// written out.
fn argument_default(model: &Model, owner: &str, field: &str, name: &str) -> String {
  let argument = model
    .member(field_of(model, owner, field))
    .arguments()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == name)
    .unwrap_or_else(|| panic!("no argument `{name}` of `{owner}.{field}`"));
  let default = model
    .member(argument)
    .default()
    .unwrap_or_else(|| panic!("`{name}` of `{owner}.{field}` has no default"));
  valued(model, default)
}

/// Each argument of the first application on the field `field` of the root package's type
/// `owner`: its name, `:` after one written and `=` after a copy of a default, and its value.
fn applied(model: &Model, owner: &str, field: &str) -> Vec<String> {
  let application = model.member(field_of(model, owner, field)).applications()[0];
  model
    .application(application)
    .arguments()
    .iter()
    .map(|argument: &ApplicationArgument| {
      let mark = if argument.written() { ":" } else { "=" };
      format!(
        "{}{mark} {}",
        model.member(argument.argument()).name(),
        valued(model, argument.value())
      )
    })
    .collect()
}

/// How many values `model` holds.
fn values_held(model: &Model) -> u32 {
  let test_support::Counts { values, .. } = test_support::counts(model);
  values
}

/// How many values the built-in library's own are: what an empty root package's model holds.
fn library_values() -> u32 {
  let empty: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  values_held(analyze(&empty, &[], &Limits::new(), &Options::new()).model())
}

/// How many values `analysis`'s model holds beyond the built-in library's own, `library`.
fn made(analysis: &Analysis, library: u32) -> u32 {
  values_held(analysis.model())
    .checked_sub(library)
    .expect("every model holds the built-in library's values")
}

/// How many of `analysis`'s reports are of a written value `Limits::max_values` refused.
fn unread(analysis: &Analysis) -> u32 {
  let refused = messages(analysis, LIMIT)
    .iter()
    .filter(|message: &&String| message.contains(UNREAD))
    .count();
  u32::try_from(refused).expect("a count of reports fits a `u32`")
}

/// The help of each diagnostic whose code is `code`, in the list's order.
fn helps(analysis: &Analysis, code: &str) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic| diagnostic.code().as_str() == code)
    .map(|diagnostic| diagnostic.help().map(String::from).unwrap_or_default())
    .collect()
}

/// Three defaults: `a`'s three literals, `b`'s one, and `c`'s one, which stands for two lists that
/// coercion makes. Seven values in all.
const DEFAULTS: &str = "input In {\n  a: [Int] = [1, 2]\n  b: Int = 7\n  c: [[Int]] = 5\n}\n";

/// A default of three values that three applications leave out: the default's own three, and
/// three for each copy. Twelve in all.
const COPIED: &str = "directive @d(a: [Int] = [1, 2]) on FIELD_DEFINITION\n\
                      type Query {\n  x: Int @d\n  y: Int @d\n  z: Int @d\n}\n";

/// An object literal that leaves out both fields of its input object, each defaulted to one value:
/// three written values, and two copied. Five in all.
const FILLED: &str =
  "input In {\n  a: Int = 1\n  b: Int = 2\n}\ntype Query {\n  f(x: In = {}): Int\n}\n";

/// The three shapes GraphQL's schema builder refuses for a capacity ceiling, which GraphQLx
/// accepts by default: a field and a directive of sixty-five arguments, and a type of sixteen
/// lists. The same shapes `support/validator_corpus.rs` writes out. Refuses a ceiling GraphQLx
/// does not have.
#[test]
fn graphqlx_accepts_what_the_ceilings_refuse_by_default() {
  for text in [wide_field(65), wide_directive(65), deep_field(16)] {
    let analysis = analyze_text(&text, &Limits::new(), &Options::new());
    assert!(analysis.is_ok(), "{text}: {:#?}", analysis.diagnostics());
  }
}

/// One past the parity bounds is refused, once, and at the bound it is not. A field of 64
/// arguments is ok and one of 65 is `resource-limit` at its name, labelled at the first argument
/// past the bound, `a64`; the same for a directive; and `[[[[[[[Int!]!]!]!]!]!]!]!`, seven lists
/// and eight `!`, is ok where `[[[[[[[[Int!]!]!]!]!]!]!]!]`, eight and eight, is `resource-limit`
/// at the whole type. Refuses a bound that counts one too many or one too few.
#[test]
fn one_past_the_parity_bounds_is_refused() {
  let fits = analyze_text(&wide_field(64), &PARITY, &Options::new());
  assert!(fits.is_ok(), "{:#?}", fits.diagnostics());
  let text = wide_field(65);
  let analysis = analyze_text(&text, &PARITY, &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(
      at(schema, &text, "wide", 0),
      Some(at(schema, &text, "a64", 0))
    )]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains("65 arguments"), "{message}");
  assert!(
    message.contains("`Limits::max_field_arguments`, 64"),
    "{message}"
  );

  let fits = analyze_text(&wide_directive(64), &PARITY, &Options::new());
  assert!(fits.is_ok(), "{:#?}", fits.diagnostics());
  let text = wide_directive(65);
  let analysis = analyze_text(&text, &PARITY, &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(
      at(schema, &text, "wide", 0),
      Some(at(schema, &text, "a64", 0))
    )]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains("65 arguments"), "{message}");
  assert!(
    message.contains("`Limits::max_directive_arguments`, 64"),
    "{message}"
  );

  let fits = "type Query {\n  ok: [[[[[[[Int!]!]!]!]!]!]!]!\n}\n";
  let analysis = analyze_text(fits, &PARITY, &Options::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let refused = "type Query {\n  ok: [[[[[[[[Int!]!]!]!]!]!]!]!]\n}\n";
  let analysis = analyze_text(refused, &PARITY, &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, refused, "[[[[[[[[Int!]!]!]!]!]!]!]!]", 0), None)]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("16 list and non-null wrappers"),
    "{message}"
  );
  assert!(
    message.contains("`Limits::max_type_wrappers`, 15"),
    "{message}"
  );
}

/// A refused shape keeps what it writes, so the bounds stop no work: the field of 65 arguments
/// still has 65, the directive's application that writes the 65th still has its value, and no
/// `undefined-directive-argument` follows; and the deep type is the type its text writes. Refuses
/// a refused argument dropped.
#[test]
fn a_refused_shape_keeps_what_it_writes() {
  let text = wide_field(65);
  let analysis = analyze_text(&text, &PARITY, &Options::new());
  assert_eq!(field_args(analysis.model(), "Query", "wide"), 65);

  let applied = (0..65)
    .map(|index: usize| format!("a{index}: 1"))
    .collect::<Vec<String>>()
    .join(", ");
  let text = format!(
    "type Query {{\n  x: Int @wide({applied})\n}}\ndirective @wide({}) on FIELD_DEFINITION\n",
    arguments(65)
  );
  let analysis = analyze_text(&text, &PARITY, &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(
      at(schema, &text, "wide", 1),
      Some(at(schema, &text, "a64", 1))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let application = model.member(field_of(model, "Query", "x")).applications()[0];
  let arguments = model.application(application).arguments();
  assert_eq!(arguments.len(), 65);
  let ValueKind::Int(last) = model.value(arguments[64].value()).kind() else {
    panic!("the 65th argument's value is no integer");
  };
  assert_eq!(last.text(), "1");

  let refused = "type Query {\n  deep: [[[[[[[[Int!]!]!]!]!]!]!]!]\n}\n";
  let analysis = analyze_text(refused, &PARITY, &Options::new());
  assert_eq!(found(&analysis, LIMIT).len(), 1);
  let unbounded = analyze_text(refused, &Limits::new(), &Options::new());
  assert!(unbounded.is_ok(), "{:#?}", unbounded.diagnostics());
  let shown_refused = field_shown(analysis.model(), "Query", "deep");
  assert!(shown_refused.starts_with("[[[[[[[["), "{shown_refused}");
  assert_eq!(
    shown_refused,
    field_shown(unbounded.model(), "Query", "deep")
  );
}

/// At the defaults, 2^10 arguments are ok on a field and on a directive and one more is refused,
/// at the name, naming `Limits::max_field_arguments` or `Limits::max_directive_arguments`, 1024.
/// Refuses a default that is another number.
#[test]
fn one_past_the_defaults_is_refused() {
  for (fits, past, bound) in [
    (
      wide_field(1024),
      wide_field(1025),
      "`Limits::max_field_arguments`, 1024",
    ),
    (
      wide_directive(1024),
      wide_directive(1025),
      "`Limits::max_directive_arguments`, 1024",
    ),
  ] {
    let analysis = analyze_text(&fits, &Limits::new(), &Options::new());
    assert!(analysis.is_ok(), "{bound}: {:#?}", analysis.diagnostics());
    let analysis = analyze_text(&past, &Limits::new(), &Options::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis, LIMIT),
      [(
        at(schema, &past, "wide", 0),
        Some(at(schema, &past, "a1024", 0))
      )]
    );
    let message = only(messages(&analysis, LIMIT));
    assert!(message.contains("1025 arguments"), "{message}");
    assert!(message.contains(bound), "{message}");
  }
}

/// Relaxed bounds admit past the defaults: 1,025 arguments on a field and on a directive under
/// the bounds set to 1,025, and a type of 67 wrappers, 33 non-null lists around `Int!`, under a
/// parser given the ceiling to read it: refused at the default of 64, ok at 67, refused at 66.
/// Refuses a bound that cannot be relaxed.
#[test]
fn relaxed_bounds_admit_past_the_defaults() {
  let relaxed = Limits::new()
    .with_max_field_arguments(1025)
    .with_max_directive_arguments(1025);
  for text in [wide_field(1025), wide_directive(1025)] {
    let analysis = analyze_text(&text, &relaxed, &Options::new());
    assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  }

  let lists = 33;
  let text = format!(
    "type Query {{\n  deep: {}Int!{}\n}}\n",
    "[".repeat(lists),
    "]!".repeat(lists)
  );
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
  let parser = Syntactic::with_limits(SyntacticLimits::with_max_nesting_depth(80));
  let analysis_under =
    |limits: &Limits| -> Analysis { analyze_with(&parser, &root, &[], limits, &Options::new()).0 };

  let analysis = analysis_under(&Limits::new());
  assert_eq!(
    found(&analysis, LIMIT).len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("67 list and non-null wrappers"),
    "{message}"
  );
  let analysis = analysis_under(&Limits::new().with_max_type_wrappers(67));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let analysis = analysis_under(&Limits::new().with_max_type_wrappers(66));
  assert_eq!(
    found(&analysis, LIMIT).len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Wrappers are counted where they are written, one chain at a time, under a bound of two:
/// `a`'s `[Int!]!` has three, and is refused; `b`'s `Holder<[Int!]!>!` has one at its root, which
/// is kept, and its argument three, which is refused at the argument; `c`'s `Twice<[[Int]]>`
/// writes two for the argument and `Twice` has two written inside it, so nothing is refused though
/// substituted it is four, which the bound does not count. Exactly two reports.
#[test]
fn wrappers_are_counted_where_written() {
  let text = "type Holder<T> {\n  v: T\n}\ntype Twice<T> {\n  v: [[T]]\n}\n\
              type Query {\n  a: [Int!]!\n  b: Holder<[Int!]!>!\n  c: Twice<[[Int]]>\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_type_wrappers(2),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (at(schema, text, "[Int!]!", 0), None),
      (at(schema, text, "[Int!]!", 1), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    analysis.diagnostics().len(),
    2,
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Every type a supplied package writes is counted, wherever it stands: under a bound of `0`, an
/// input field's `[Int]`, a field's argument `Int!`, a directive's argument `[Int]`, a type
/// parameter's default `[Int]`, an application's type argument `[Int]`, a bound's argument
/// `[Int]` and a field's own type `[Int]` are each one `resource-limit`, at the type. Refuses a
/// position passed over.
#[test]
fn every_written_type_is_counted() {
  let text = "type Query {\n  x: Int\n  field: [Int]\n  arg(a: Int!): Int\n}\n\
              input In {\n  f: [Int]\n}\n\
              directive @d(v: [Int]) on FIELD_DEFINITION\n\
              type P<T = [Int]> {\n  v: T\n}\n\
              directive @tag<T> on FIELD_DEFINITION\n\
              type A {\n  y: Int @tag<[Int]>\n}\n\
              interface Cmp<U> {\n  z: Int\n}\n\
              type B<T> where T: Cmp<[Int]> {\n  v: T\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_type_wrappers(0),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (at(schema, text, "[Int]", 0), None),
      (at(schema, text, "Int!", 0), None),
      (at(schema, text, "[Int]", 1), None),
      (at(schema, text, "[Int]", 2), None),
      (at(schema, text, "[Int]", 3), None),
      (at(schema, text, "[Int]", 4), None),
      (at(schema, text, "[Int]", 5), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// An extension's field, an interface's field and the object field that implements it are each
/// counted, under a bound of two: three reports, one at each name. Refuses a field counted in its
/// definition alone.
#[test]
fn extensions_and_interfaces_are_counted() {
  let text = "type Query {\n  x: Int\n}\n\
              extend type Query {\n  extra(a: Int, b: Int, c: Int): Int\n}\n\
              interface I {\n  field(a: Int, b: Int, c: Int): Int\n}\n\
              type T implements I {\n  field(a: Int, b: Int, c: Int): Int\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_field_arguments(2),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (
        at(schema, text, "extra", 0),
        Some(at_name(schema, text, "c: Int", 0))
      ),
      (
        at(schema, text, "field", 0),
        Some(at_name(schema, text, "c: Int", 1))
      ),
      (
        at(schema, text, "field", 1),
        Some(at_name(schema, text, "c: Int", 2))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The root package's replacement of a GraphQL built-in directive is a supplied definition, and
/// is counted: a `@deprecated` of three arguments is `resource-limit` under a bound of two, at
/// its name. Refuses a replaced built-in counted as the library's.
#[test]
fn a_replaced_directive_is_counted() {
  let text = "type Query {\n  x: Int\n}\n\
              directive @deprecated(a: Int, b: Int, c: Int) on FIELD_DEFINITION\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_directive_arguments(2),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(
      at(schema, text, "deprecated", 0),
      Some(at_name(schema, text, "c: Int", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The built-in library is counted by none of the four: an empty root package under the four
/// bounds at `0` is clean, though `core`'s `@extern` has three arguments, `__Type.fields` one and
/// `__Schema.types` is `[__Type!]!`, and `@deprecated`'s `reason` keeps its default, a value the
/// library writes and reads for nothing; and beside it a root package that writes a field with an
/// argument, a directive with an argument and a type `Int!` is refused once for each. Refuses a
/// library counted as a supplied package is.
#[test]
fn the_built_in_library_is_counted_by_none_of_the_four() {
  let none = Limits::new()
    .with_max_field_arguments(0)
    .with_max_directive_arguments(0)
    .with_max_type_wrappers(0)
    .with_max_values(0);
  let empty: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&empty, &[], &none, &Options::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert!(found(&analysis, LIMIT).is_empty());
  let model = analysis.model();
  let reason = model
    .def(model.builtins().deprecated())
    .arguments()
    .into_iter()
    .next()
    .expect("`reason`");
  let default = model.member(reason).default().expect("a default");
  assert_eq!(
    model.value(default).kind(),
    ValueKind::String("No longer supported")
  );

  let text = "type Query {\n  wide(a: Int): Int\n  g: Int!\n}\n\
              directive @mark(b: Int) on FIELD_DEFINITION\n";
  let analysis = analyze_text(text, &none, &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (
        at(schema, text, "wide", 0),
        Some(at_name(schema, text, "a: Int", 0))
      ),
      (at(schema, text, "Int!", 0), None),
      (
        at(schema, text, "mark", 0),
        Some(at_name(schema, text, "b: Int", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A written value is charged its literals where lowering reads it, and the lists a lone value
/// stands for where coercion makes them: `In`'s defaults write five literals, and `c`'s `5` stands
/// for two lists. At 7 all is made. At 6 `c`'s inner list cannot be paid for: `c` is `List [?]`,
/// reported at `5`. At 4 lowering reads `a` and `b`, which spend all four, and refuses `c`, which
/// is `?`. At 2 lowering refuses `a`, charging nothing, and reads `b` and `c`, which spend both,
/// so coercion cannot pay for `c`'s first list. Refuses a lone value's lists left uncharged, and a
/// refused value that spends what is left.
#[test]
fn a_written_value_is_charged_its_literals() {
  let text = DEFAULTS;
  let defaults = |analysis: &Analysis| -> [String; 3] {
    let model = analysis.model();
    ["a", "b", "c"].map(|name: &str| valued(model, input_default(model, "In", name)))
  };

  let analysis = analyze_text(text, &Limits::new().with_max_values(7), &Options::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(
    defaults(&analysis),
    ["List [Int 1, Int 2]", "Int 7", "List [List [Int 5]]"]
  );

  let analysis = analyze_text(text, &Limits::new().with_max_values(6), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "5", 0), None)]);
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("the lists this value stands for"),
    "{message}"
  );
  assert!(message.contains(&format!("{VALUES}, 6")), "{message}");
  assert_eq!(
    defaults(&analysis),
    ["List [Int 1, Int 2]", "Int 7", "List [?]"]
  );

  let analysis = analyze_text(text, &Limits::new().with_max_values(4), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "5", 0), None)]);
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains(UNREAD), "{message}");
  assert!(
    message.contains(&format!("the 0 left of {VALUES}, 4")),
    "{message}"
  );
  assert_eq!(defaults(&analysis), ["List [Int 1, Int 2]", "Int 7", "?"]);

  let analysis = analyze_text(text, &Limits::new().with_max_values(2), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (at(schema, text, "[1, 2]", 0), None),
      (at(schema, text, "5", 0), None),
    ]
  );
  let reports = messages(&analysis, LIMIT);
  assert!(
    reports[0].contains(&format!("the 2 left of {VALUES}, 2")),
    "{reports:#?}"
  );
  assert!(
    reports[1].contains("the lists this value stands for"),
    "{reports:#?}"
  );
  assert_eq!(defaults(&analysis), ["?", "Int 7", "?"]);
}

/// A value kept as written costs every literal lowering reads of it, though the model keeps it as
/// one value: `[0, 0, 0]` of a custom scalar is four literals, refused at 3 and one `Literal` at 4,
/// and so is `[1, 2, 3]` of `bytes`, one `Bytes` value at 4. Refuses a value charged by the rows it
/// makes rather than by what is read of it.
#[test]
fn a_value_kept_as_written_costs_its_literals() {
  for (text, written, kept) in [
    (
      "scalar Any\ninput H {\n  a: Any = [0, 0, 0]\n}\n",
      "[0, 0, 0]",
      "Literal(List) [0, 0, 0]",
    ),
    (
      "input H {\n  a: bytes = [1, 2, 3]\n}\n",
      "[1, 2, 3]",
      "Bytes [1, 2, 3]",
    ),
  ] {
    let analysis = analyze_text(text, &Limits::new().with_max_values(3), &Options::new());
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis, LIMIT),
      [(at(schema, text, written, 0), None)]
    );
    let model = analysis.model();
    assert_eq!(valued(model, input_default(model, "H", "a")), "?");

    let analysis = analyze_text(text, &Limits::new().with_max_values(4), &Options::new());
    assert!(clean(&analysis), "{text}: {:#?}", analysis.diagnostics());
    let model = analysis.model();
    assert_eq!(valued(model, input_default(model, "H", "a")), kept);
  }
}

/// A value of more literals than are left is refused alone, and charges nothing: under a bound of
/// four, `[1, 2, 3, 4]`, five literals, is refused, and the next default's one literal is still
/// read. Refuses a refusal that spends the bound, or that stops the reading of what follows.
#[test]
fn a_later_value_that_fits_is_read() {
  let text = "input In {\n  a: [Int] = [1, 2, 3, 4]\n  b: Int = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_values(4), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, text, "[1, 2, 3, 4]", 0), None)]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains(&format!("the 4 left of {VALUES}, 4")),
    "{message}"
  );
  let model = analysis.model();
  assert_eq!(valued(model, input_default(model, "In", "a")), "?");
  assert_eq!(valued(model, input_default(model, "In", "b")), "Int 1");
}

/// A lone value's lists cost a value a level, as they cost a step: `a: [[Int]] = 1` is one literal
/// and two lists. At 3 it is made; at 2 its inner list is not, so it is `List [?]`, reported at `1`
/// with `Limits::with_max_values` in its help; at 1 neither list is, and it is `?`; and at 0
/// lowering refuses its literal. Refuses a level charged to the steps alone.
#[test]
fn a_lone_value_costs_a_value_a_level() {
  let text = "input I {\n  a: [[Int]] = 1\n}\n";
  let default = |analysis: &Analysis| -> String {
    let model = analysis.model();
    valued(model, input_default(model, "I", "a"))
  };

  let analysis = analyze_text(text, &Limits::new().with_max_values(3), &Options::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(default(&analysis), "List [List [Int 1]]");

  let analysis = analyze_text(text, &Limits::new().with_max_values(2), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "1", 0), None)]);
  assert_eq!(default(&analysis), "List [?]");
  let help = only(helps(&analysis, LIMIT));
  assert!(help.contains("Limits::with_max_values"), "{help}");

  let analysis = analyze_text(text, &Limits::new().with_max_values(1), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "1", 0), None)]);
  assert_eq!(default(&analysis), "?");
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("the lists this value stands for"),
    "{message}"
  );

  let analysis = analyze_text(text, &Limits::new().with_max_values(0), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "1", 0), None)]);
  assert_eq!(default(&analysis), "?");
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains(UNREAD), "{message}");
}

/// A copy is charged its values whole, before its first is made: `@d`'s default is three values,
/// and each copy three more. At 12 the three applications are filled; at 9 the third, `z`'s, is
/// `resource-limit` at its path, naming the bound, and holds no argument. Refuses a copy charged
/// by the row, or made in part.
#[test]
fn copies_are_charged_whole() {
  let text = COPIED;
  let analysis = analyze_text(text, &Limits::new().with_max_values(12), &Options::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  for field in ["x", "y", "z"] {
    assert_eq!(
      applied(analysis.model(), "Query", field),
      ["a= List [Int 1, Int 2]"]
    );
  }

  let analysis = analyze_text(text, &Limits::new().with_max_values(9), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(after(schema, text, "z: Int @", "d"), None)]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("the arguments this application leaves out"),
    "{message}"
  );
  assert!(message.contains(&format!("{VALUES}, 9")), "{message}");
  let model = analysis.model();
  for field in ["x", "y"] {
    assert_eq!(applied(model, "Query", field), ["a= List [Int 1, Int 2]"]);
  }
  assert!(applied(model, "Query", "z").is_empty());
}

/// An object literal's copies are charged as an application's are: `{}` leaves out `a` and `b`,
/// each defaulted to one value, so the program's values are its three written ones and the two
/// copies. At 5 the literal holds both; at 4 its second copy cannot be paid for, so the literal is
/// `resource-limit`, naming the bound, and holds `a` and not `b`. Refuses an object literal's
/// copies left uncharged.
#[test]
fn an_object_literals_copies_are_charged_whole() {
  let text = FILLED;
  let analysis = analyze_text(text, &Limits::new().with_max_values(5), &Options::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(
    argument_default(analysis.model(), "Query", "f", "x"),
    "Object {a= Int 1, b= Int 2}"
  );

  let analysis = analyze_text(text, &Limits::new().with_max_values(4), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis, LIMIT), [(at(schema, text, "{}", 0), None)]);
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("the fields this literal leaves out"),
    "{message}"
  );
  assert!(message.contains(&format!("{VALUES}, 4")), "{message}");
  assert_eq!(
    argument_default(analysis.model(), "Query", "f", "x"),
    "Object {a= Int 1}"
  );
}

/// A default lowering refused is one unresolved value, and so is each copy of it, charged one
/// value: under a bound of two, `@d`'s `[1, 2]` is refused, charging nothing; `x`'s and `y`'s
/// copies are one value each, not written and located at the default; and `z`'s cannot be paid
/// for. The reports are at `[1, 2]` and at `z`'s `@d`. Refuses a refused default copied as more
/// than one value, or as none.
#[test]
fn a_refused_default_is_one_value_wherever_it_is_copied() {
  let text = COPIED;
  let analysis = analyze_text(text, &Limits::new().with_max_values(2), &Options::new());
  let schema = schema_file(&analysis);
  let default = at(schema, text, "[1, 2]", 0);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (default, None),
      (after(schema, text, "z: Int @", "d"), None)
    ]
  );
  let model = analysis.model();
  let d = model
    .lookup(Namespace::Directive, "@^::app::d")
    .expect("`@d`");
  let own = model
    .member(model.def(d).arguments().into_iter().next().expect("`a`"))
    .default()
    .expect("a default");
  assert_eq!(valued(model, own), "?");
  for field in ["x", "y"] {
    let application = model.member(field_of(model, "Query", field)).applications()[0];
    let [argument] = model.application(application).arguments() else {
      panic!("`{field}`'s `@d` has one argument");
    };
    assert!(!argument.written());
    let copy = model.value(argument.value());
    assert_eq!(copy.kind(), ValueKind::Unresolved);
    assert!(!copy.written());
    assert_eq!(copy.location(), default);
  }
  assert!(applied(model, "Query", "z").is_empty());
}

/// A value lowering refused fits wherever it is read: `Box`'s default `[1, 2, 3]`, four literals
/// under a bound of three, is refused, and the check of `Box<[Int]>` reads its one unresolved value
/// and says nothing. One report, at `[1, 2, 3]`. Refuses a refused value judged by its kind at an
/// instantiation.
#[test]
fn a_refused_value_says_nothing_at_an_instantiation() {
  let text = "type Box<T> {\n  f(a: T = [1, 2, 3]): Int\n}\ntype Query {\n  b: Box<[Int]>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_values(3), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, text, "[1, 2, 3]", 0), None)]
  );
  assert_eq!(
    analysis.diagnostics().len(),
    1,
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(argument_default(analysis.model(), "Box", "f", "a"), "?");
}

/// A value lowering refused is reported once, by lowering, and no type is rebuilt for it under its
/// application's type arguments: with no value and no step to spend, `@d<Int>(a: 1)`'s `1` is
/// refused, and the one `resource-limit` is lowering's, at `1`. The argument is one unresolved
/// value of the type it is written with. Refuses a second report at the same literal.
#[test]
fn a_refused_argument_is_reported_once_where_its_type_cannot_be_built() {
  let text = "directive @d<T>(a: T) on OBJECT\ntype Query @d<Int>(a: 1) {\n  f: Int\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_values(0).with_max_value_steps(0),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, text, "1", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains(UNREAD), "{message}");
  let model = analysis.model();
  let application = model.def(type_def(model, "Query")).applications()[0];
  let [argument] = model.application(application).arguments() else {
    panic!("`@d<Int>(a: 1)` has one argument");
  };
  let value = model.value(argument.value());
  assert_eq!(value.kind(), ValueKind::Unresolved);
  let d = model
    .lookup(Namespace::Directive, "@^::app::d")
    .expect("`@d`");
  let a = model.def(d).arguments().into_iter().next().expect("`a`");
  let written = model.member(a).ty().expect("`a` has a type");
  assert_eq!(value.ty(), model.type_expr(written).ty());
}

/// A value lowering refused spends no step: no type is rebuilt for it. Under no value and two
/// steps, `@d<Int>(a: 1)`'s `1` is refused, and `@d<String>`, which leaves `a` out, has the two
/// steps it needs, one to ask whether `a` is required there and one to rebuild `T` as `String`; one
/// step short, it is starved at its path. Refuses a refused argument's type rebuilt with a step
/// another application needs.
#[test]
fn a_refused_argument_spends_no_step() {
  let text = "directive @d<T>(a: T) repeatable on OBJECT\n\
              type Query @d<Int>(a: 1) @d<String> {\n  f: Int\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_values(0).with_max_value_steps(2),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, text, "1", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );

  let short = analyze_text(
    text,
    &Limits::new().with_max_values(0).with_max_value_steps(1),
    &Options::new(),
  );
  let schema = schema_file(&short);
  assert_eq!(
    found(&short, LIMIT),
    [
      (at(schema, text, "1", 0), None),
      (after(schema, text, "(a: 1) @", "d"), None),
    ],
    "{:#?}",
    short.diagnostics()
  );
  let reports = messages(&short, LIMIT);
  assert!(
    reports[1].contains("`Limits::max_value_steps`, 1"),
    "{reports:#?}"
  );
}

/// A value lowering refused is checked at no instantiation, since it fits every type, so its check
/// spends no step that a value read needs: under one value and three steps, `Box`'s `[1, 2]` is
/// refused and its `"x"` read, and the check of `Box<Int>` spends its three steps on `"x"` alone,
/// one for the row, one to rebuild `T` and one for the value, which does not fit `Int` and is
/// reported there, labelled at `"x"`. The one `resource-limit` is lowering's. Refuses a refused
/// value checked before the values that were read.
#[test]
fn a_refused_default_spends_no_step_at_an_instantiation() {
  let text = "input Box<T> {\n  dropped: T = [1, 2]\n  kept: T = \"x\"\n}\n\
              type Query {\n  f(b: Box<Int>): Int\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_values(1).with_max_value_steps(3),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(at(schema, text, "[1, 2]", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    found(&analysis, DEFAULT),
    [(
      at(schema, text, "Box<Int>", 0),
      Some(at(schema, text, "\"x\"", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A copy `Limits::max_values` kept from being made is checked at no instantiation, so the check
/// spends no step on it that a written value needs: under two values and five steps, `@d<T>` on
/// `Box` spends two steps rebuilding `U` and counting the copy of `true`, cannot pay for its value,
/// and is `resource-limit` at its path; `Box<Boolean>`'s check then spends its three steps on
/// `f`'s `0` alone, one for the row, one to rebuild `T` and one for the value, which does not fit
/// `Boolean` and is reported there, labelled at `0`. Refuses a copy never made checked before the
/// values that were.
#[test]
fn a_copy_never_made_is_checked_at_no_instantiation() {
  let text = "directive @d<U>(a: U = true) on OBJECT\n\
              type Box<T> @d<T> {\n  f(x: T = 0): Boolean\n}\ntype Query {\n  b: Box<Boolean>\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_values(2).with_max_value_steps(5),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(after(schema, text, "type Box<T> @", "d"), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains(&format!("{VALUES}, 2")), "{message}");
  assert_eq!(
    found(&analysis, DEFAULT),
    [(
      at(schema, text, "Box<Boolean>", 0),
      Some(after(schema, text, "x: T = ", "0"))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A copy the step budget kept from being made is checked at no instantiation either, so no use of
/// its definition is reported again for it: under one step, `@d<T>` on `Box` rebuilds `U` and
/// cannot pay to count the copy of `true`, and is `resource-limit` at its path; `Box` records
/// nothing else, so `Box<Boolean>` is checked for nothing and says nothing. Refuses a starved copy
/// that each use then reports again, `resource-limit` at it.
#[test]
fn a_starved_copy_is_checked_at_no_instantiation() {
  let text = "directive @d<U>(a: U = true) on OBJECT\n\
              type Box<T> @d<T> {\n  f: Boolean\n}\ntype Query {\n  b: Box<Boolean>\n}\n";
  let analysis = analyze_text(
    text,
    &Limits::new().with_max_value_steps(1),
    &Options::new(),
  );
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(after(schema, text, "type Box<T> @", "d"), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(
    message.contains("`Limits::max_value_steps`, 1"),
    "{message}"
  );
}

/// A copy's count reads its default under the asker's type arguments, and what that reading reports
/// stands whatever either budget then answers, once: at `core`, under the switch that judges
/// compile-time metadata, `@d<E>` counts `Missing::A` under `E`, and the resolver refuses `Missing`.
/// Under one value the copy cannot be paid for, and the application is `resource-limit`; under two
/// it is made, and reads the prefix the count resolved. Either way `Missing` is `unresolved-path`
/// once, and that error marks `@d`'s contribution, so `label: string` is not judged there. Where
/// the prefix resolves, `E::A`, nothing marks `@d`, and `string` is `tier`. Refuses a verdict that
/// changes as the budget is raised.
#[test]
fn what_a_count_reports_stands_whether_or_not_its_copy_is_made() {
  const UNRESOLVED: &str = "smear::graphqlx::unresolved-path";
  const TIER: &str = "smear::graphqlx::tier";
  let program = |default: &str| -> String {
    format!(
      "enum E {{\n  A\n}}\ndirective @d<T>(v: T = {default}, label: string) on OBJECT\n\
       type Query @d<E> {{\n  ok: Boolean\n}}\n"
    )
  };
  let analyze_core = |text: &str, limits: &Limits| -> Analysis {
    let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Core)
      .with_files([SourceFile::new("schema.graphqlx", text)]);
    analyze(
      &root,
      &[],
      limits,
      &Options::new().with_judged_metadata(true),
    )
  };

  let text = program("Missing::A");
  for max in [1, 2] {
    let analysis = analyze_core(&text, &Limits::new().with_max_values(max));
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis, UNRESOLVED),
      [(at(schema, &text, "Missing", 0), None)],
      "at {max}: {:#?}",
      analysis.diagnostics()
    );
    let string = at(schema, &text, "string", 0);
    assert!(
      found(&analysis, TIER)
        .iter()
        .all(|(primary, _): &(Location, Option<Location>)| *primary != string),
      "at {max}: {:#?}",
      analysis.diagnostics()
    );
    if max == 1 {
      assert_eq!(
        found(&analysis, LIMIT),
        [(after(schema, &text, "type Query @", "d"), None)],
        "{:#?}",
        analysis.diagnostics()
      );
      let message = only(messages(&analysis, LIMIT));
      assert!(message.contains(&format!("{VALUES}, 1")), "{message}");
    } else {
      assert!(
        found(&analysis, LIMIT).is_empty(),
        "{:#?}",
        analysis.diagnostics()
      );
    }
  }

  let resolved = program("E::A");
  let analysis = analyze_core(&resolved, &Limits::new());
  let schema = schema_file(&analysis);
  let string = at(schema, &resolved, "string", 0);
  assert!(
    found(&analysis, TIER)
      .iter()
      .any(|(primary, _): &(Location, Option<Location>)| *primary == string),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The model holds at most the bound's values beyond the built-in library's own and one for each
/// written value refused: measured for the programs above at each bound they run at, the values a
/// program's model holds beyond an empty root package's, against the bound and the reports that
/// say a value was not read. And the term the bound does not hold: `@d(a: 0, a: 0, …)` of
/// sixty-four arguments under a bound of four is sixty-four values, each argument keeping its own,
/// the first four read and the other sixty refused from the fifth `0` on. Refuses a refused value
/// that holds more than its one value, and a bound said to hold what it does not.
#[test]
fn the_model_holds_what_it_says() {
  let library = library_values();
  for (text, bounds) in [
    (DEFAULTS, &[7, 6, 4, 2][..]),
    (COPIED, &[12, 9, 2][..]),
    (FILLED, &[5, 4][..]),
  ] {
    for &bound in bounds {
      let analysis = analyze_text(text, &Limits::new().with_max_values(bound), &Options::new());
      let (held, refused) = (made(&analysis, library), unread(&analysis));
      assert!(
        held <= bound + refused,
        "{text}at {bound}: {held} values, {refused} refused"
      );
    }
  }

  let arguments = ["a: 0"; 64].join(", ");
  let text = format!(
    "directive @d(a: Int) on FIELD_DEFINITION\ntype Query {{\n  x: Int @d({arguments})\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new().with_max_values(4), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(made(&analysis, library), 64);
  assert_eq!(unread(&analysis), 60);
  assert_eq!(
    found(&analysis, LIMIT)[0],
    (at(schema, &text, "0", 4), None)
  );
  assert_eq!(applied(analysis.model(), "Query", "x").len(), 64);
}

/// At 0 nothing a supplied package writes is read and nothing it leaves out is filled: `f`'s
/// default `1` is refused, and is `?`; the bare `@deprecated` is `resource-limit` at its path, and
/// holds no `reason`; and `core`'s `@deprecated` keeps its own default, which the built-in library
/// reads for nothing. Refuses a bound the library's own values are charged to.
#[test]
fn at_zero_nothing_is_read_or_filled() {
  let text = "type Query {\n  f(a: Int = 1): Int @deprecated\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_values(0), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [
      (at(schema, text, "1", 0), None),
      (at(schema, text, "deprecated", 0), None),
    ]
  );
  let reports = messages(&analysis, LIMIT);
  assert!(reports[0].contains(UNREAD), "{reports:#?}");
  assert!(
    reports[1].contains("the arguments this application leaves out"),
    "{reports:#?}"
  );
  let model = analysis.model();
  assert_eq!(argument_default(model, "Query", "f", "a"), "?");
  assert!(applied(model, "Query", "f").is_empty());
  let reason = model
    .def(model.builtins().deprecated())
    .arguments()
    .into_iter()
    .next()
    .expect("`reason`");
  let default = model.member(reason).default().expect("a default");
  assert_eq!(
    model.value(default).kind(),
    ValueKind::String("No longer supported")
  );
}

/// The default bound is 2^20, and a value is read up to it: `[0 0 … 0]` of 2^20 - 1 zeros is 2^20
/// literals, read under `Limits::new()`; one zero more is refused there, whole, and read under a
/// bound one higher. Refuses a default that is another number.
#[test]
fn the_default_bound_and_one_past_it() {
  let zeros = |count: usize| -> String {
    format!("input In {{\n  a: [Int] = [{}]\n}}\n", "0 ".repeat(count))
  };
  let elements = |analysis: &Analysis| -> Option<usize> {
    let model = analysis.model();
    match model.value(input_default(model, "In", "a")).kind() {
      ValueKind::List(items) => Some(items.len()),
      _ => None,
    }
  };
  let all: usize = 1 << 20;

  let text = zeros(all - 1);
  let analysis = analyze_text(&text, &Limits::new(), &Options::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(elements(&analysis), Some(all - 1));

  let text = zeros(all);
  let analysis = analyze_text(&text, &Limits::new(), &Options::new());
  let schema = schema_file(&analysis);
  let start = text.find("= [").expect("the default") + 2;
  let end = text.rfind(']').expect("the default's end") + 1;
  assert_eq!(
    found(&analysis, LIMIT),
    [(
      Location::new(schema.get(), SimpleSpan::new(start, end)),
      None
    )]
  );
  let message = only(messages(&analysis, LIMIT));
  assert!(message.contains(UNREAD), "{message}");
  assert!(message.contains(&format!("{VALUES}, 1048576")), "{message}");
  assert_eq!(elements(&analysis), None);

  let analysis = analyze_text(
    &text,
    &Limits::new().with_max_values((1 << 20) + 1),
    &Options::new(),
  );
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert_eq!(elements(&analysis), Some(all));
}

/// The written values are charged first, so a copy early in the walk can be starved by a default
/// written after it: under a bound of six, `@d`'s `[1]` and `In.b`'s `[1, 2, 3]` spend all six
/// where lowering reads them, and `x`'s `@d`, which the walk reaches before `In.b`, cannot pay for
/// its copy: it is reported at its path, and `In.b`'s default is read. At eight the copy is made.
/// Refuses written values charged where the walk makes them, beside the copies.
#[test]
fn an_earlier_copy_is_starved_by_a_later_default() {
  let text = "directive @d(a: [Int] = [1]) on FIELD_DEFINITION\n\
              type Query {\n  x(i: In): Int @d\n}\ninput In {\n  b: [Int] = [1, 2, 3]\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_values(6), &Options::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis, LIMIT),
    [(after(schema, text, "x(i: In): Int @", "d"), None)]
  );
  let model = analysis.model();
  assert!(applied(model, "Query", "x").is_empty());
  assert_eq!(
    valued(model, input_default(model, "In", "b")),
    "List [Int 1, Int 2, Int 3]"
  );

  let analysis = analyze_text(text, &Limits::new().with_max_values(8), &Options::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(applied(analysis.model(), "Query", "x"), ["a= List [Int 1]"]);
}

/// GitLab's schema, the largest of smear's fixtures, makes the 1,547 values `Limits::max_values`'
/// documentation says, the values its model holds beyond the built-in library's own, far below the
/// default bound, and no report names the bound. Refuses a documented count that no longer holds.
#[test]
fn gitlabs_schema_makes_the_values_the_doc_says() {
  let gitlab = include_str!("fixtures/schemas/gitlab_schema.graphql");
  let analysis = analyze_text(
    gitlab,
    &Limits::new().with_max_diagnostics(1 << 20),
    &Options::new(),
  );
  let reports: Vec<String> = messages(&analysis, LIMIT)
    .into_iter()
    .filter(|message: &String| message.contains(VALUES))
    .collect();
  assert!(reports.is_empty(), "{reports:#?}");
  let count = made(&analysis, library_values());
  assert_eq!(count, 1547);
  assert!(count < Limits::new().max_values(), "{count}");
}
