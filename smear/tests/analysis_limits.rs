//! The GraphQLx analyzer's bounds on the shapes a program writes: how many arguments a field and a
//! directive declare, and how many list and non-null wrappers a written type has, each at its
//! default, tightened and relaxed.
//!
//! The three refuse a shape and stop no work: a refused field, directive or type keeps everything
//! it writes, so nothing downstream sees less than the program says. At 64, 64 and 15 they refuse
//! each document GraphQL's schema builder refuses for a capacity ceiling, which
//! `analysis_differential.rs` holds over the corpus.
//!
//! Each test builds the shapes it needs from counts, as a fixture of `support/validator_corpus.rs`
//! writes a field of sixty-five arguments out, so the number a test uses is readable beside the
//! number it tests.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Layout, Limits, MemberId, Model, Namespace, Options, Package, PackageId,
    SourceFile, SourceId, Tier, TyId, TyKind, ValueKind, analyze, analyze_with,
  },
  diagnostic::{Diagnose, Location},
  lexer::{limits::SyntacticLimits, tokora::SimpleSpan},
  parser::graphqlx::Syntactic,
};

/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

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

/// The built-in library is counted by none of the three: an empty root package under the three
/// bounds at `0` is clean, though `core`'s `@extern` has three arguments, `__Type.fields` one and
/// `__Schema.types` is `[__Type!]!`; and beside it a root package that writes a field with an
/// argument, a directive with an argument and a type `Int!` is refused once for each. Refuses a
/// library counted as a supplied package is.
#[test]
fn the_built_in_library_is_counted_by_none_of_the_four() {
  let none = Limits::new()
    .with_max_field_arguments(0)
    .with_max_directive_arguments(0)
    .with_max_type_wrappers(0);
  let empty: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&empty, &[], &none, &Options::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert!(found(&analysis, LIMIT).is_empty());

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
