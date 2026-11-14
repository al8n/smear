//! The GraphQLx analyzer's values under type arguments, and what they rest on: a directive
//! application's complete type arguments, typed as an occurrence of its directive would be, its
//! written type arguments first and its directive's defaults filling the rest; its directive's
//! bounds, judged at it by those arguments; its arguments, each read under them, written or a
//! copy of its default, the copy's count judging whether it fits and paying for each node; and a
//! generic input object's fields, read under its type's arguments, a copy that does not fit them
//! reported by the written literal whose type a rebuild made; and what a generic definition holds
//! whose type names its type parameters, a value, or an argument an application in it leaves out,
//! checked at each closed instantiation of it, a type's the program writes or a directive's an
//! application makes.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, Cx, DefId, Diagnostic, Layout, Limits, MemberId, Model, Namespace,
    ObjectEntry, Package, PackageId, Packages, SourceFile, SourceId, Tier, TyId, TyKind, ValueId,
    ValueKind, Visitor, analyze, test_support, visit,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// L3's code for a count of type arguments, and for one left to a default that cannot fill it.
const ARITY: &str = "smear::graphqlx::generic-arity";
/// L3's code for a type argument that does not satisfy a bound.
const BOUND: &str = "smear::graphqlx::unsatisfied-bound";
/// A bound of the limits passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";
/// The resolver's code for a type name that names nothing.
const UNDEFINED_TYPE: &str = "smear::schema::undefined-type";
/// The resolver's code for a directive name that names nothing.
const UNDEFINED_DIRECTIVE: &str = "smear::schema::undefined-directive";
/// GraphQL's code for a default that does not fit.
const DEFAULT: &str = "smear::schema::invalid-default-value";
/// GraphQL's code for a directive argument's value that does not fit.
const ARGUMENT: &str = "smear::schema::invalid-directive-argument-value";
/// GraphQL's code for a required directive argument left out or given `null`.
const MISSING: &str = "smear::schema::missing-required-directive-argument";

/// The codes coercion reports values with. A program another rule refuses, an input object as a
/// directive's type argument among them, is asserted on these alone.
const PHASE: [&str; 4] = [DEFAULT, ARGUMENT, MISSING, LIMIT];

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

/// Where the `nth` occurrence, counting from 0, of the name `name` written right after `before` is
/// in `text`, the file `source`: a name that a longer name begins with is not one.
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

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
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

/// The rows of `found` whose code is one of `codes`, in the list's order.
fn found_of(analysis: &Analysis, codes: &[&str]) -> Vec<(&'static str, Location)> {
  found(analysis)
    .into_iter()
    .filter(|row: &(&'static str, Location)| codes.contains(&row.0))
    .collect()
}

/// Asserts that `analysis` says nothing.
fn quiet(analysis: &Analysis) {
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The diagnostic with the code `code` at `at`.
fn the<'a>(analysis: &'a Analysis, code: &str, at: Location) -> &'a Diagnostic {
  analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == code && diagnostic.primary() == at
    })
    .unwrap_or_else(|| panic!("no `{code}` at {at:?}: {:#?}", analysis.diagnostics()))
}

/// Each label of `diagnostic`, where it is and what it says, in order.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, &str)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), label.text()))
    .collect()
}

/// The definition the root package's `name` names in the type namespace.
fn type_def(model: &Model, name: &str) -> DefId {
  let canonical = format!("^::app::{name}");
  model
    .lookup(Namespace::Type, &canonical)
    .unwrap_or_else(|| panic!("no type `{canonical}`"))
}

/// The type `id`, written out: a definition's name, with its arguments in `<>` when it has any; a
/// type parameter as its definition's name, `#` and its index; a list in `[]`; the unresolved type
/// as `?`; and `!` after a type that is non-null.
fn shown(model: &Model, id: TyId) -> String {
  let ty = model.ty(id);
  let mut text = match ty.kind() {
    TyKind::Named { def, args, .. } => {
      let name = model.def(def).name();
      if args.is_empty() {
        String::from(name)
      } else {
        let args: Vec<String> = args.iter().map(|arg: &TyId| shown(model, *arg)).collect();
        format!("{name}<{}>", args.join(", "))
      }
    }
    TyKind::Param { def, index, .. } => format!("{}#{index}", model.def(def).name()),
    TyKind::List { element, .. } => format!("[{}]", shown(model, element)),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  text
}

/// The complete type arguments of `application`, each written out.
fn arguments(model: &Model, application: ApplicationId) -> Vec<String> {
  model
    .application(application)
    .ty_args()
    .iter()
    .map(|ty: &TyId| shown(model, *ty))
    .collect()
}

/// The complete type arguments of each application on the root package's type `owner`, in order.
fn completed(model: &Model, owner: &str) -> Vec<Vec<String>> {
  model
    .def(type_def(model, owner))
    .applications()
    .iter()
    .map(|application: &ApplicationId| arguments(model, *application))
    .collect()
}

/// An application's complete type arguments are its written ones, then its directive's defaults
/// filling the rest, a default that names a parameter written after it filled after that one.
/// Refuses the written arguments alone, and a fill that ignores a parameter written after it.
#[test]
fn an_applications_type_arguments_are_completed_from_its_directives_defaults() {
  let text = "directive @d<A, B = [A], C = Int> repeatable on OBJECT\n\
              directive @e<A = B, B = Int> on OBJECT\n\
              type Query @d<String> @d<String, Int> @d<String, Int, Float> @e {\n  x: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  assert_eq!(
    completed(analysis.model(), "Query"),
    [
      vec!["string", "[string]", "i32"],
      vec!["string", "i32", "i32"],
      vec!["string", "i32", "f64"],
      vec!["i32", "i32"],
    ]
  );
}

/// A directive of no type parameter, one that does not resolve, a count the directive does not
/// take, and a written type argument that names nothing leave an application with no complete
/// type arguments, and nothing is said of it but the name's and the count's own diagnostics.
/// Refuses a type built for a refused count.
#[test]
fn an_application_with_no_complete_type_arguments_has_none() {
  let text = "directive @d<A, B = [A]> repeatable on OBJECT\n\
              type Query @missing @d @d<Missing> {\n  x: Int @deprecated\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (UNDEFINED_DIRECTIVE, named(schema, text, "@", "missing", 0)),
      (ARITY, named(schema, text, "@", "d", 1)),
      (UNDEFINED_TYPE, at(schema, text, "Missing", 0)),
    ])
  );
  let model = analysis.model();
  let (_, _, _, applications) = test_support::counts(model);
  for index in 0..applications {
    let application = ApplicationId::new(index);
    assert!(
      model.application(application).ty_args().is_empty(),
      "application {index} has complete type arguments: {:?}",
      arguments(model, application)
    );
    assert_eq!(
      model.application(application).ty(),
      None,
      "application {index}"
    );
  }
}

/// A slot an application leaves to a default that cannot fill it is `generic-arity` at the
/// application's path, the message naming the directive with its `@`, labelled at the default, as
/// the default's own occurrence is where it is written. Refuses silence at the application, and a
/// message without the `@`.
#[test]
fn an_applications_default_that_cannot_fill_is_reported_at_the_application() {
  let text = "type Query {\n  x: Int\n}\nscalar Pair<X, Y>\n\
              directive @d<A, B = Pair<A>> on OBJECT\n\
              type T @d<Int> {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let default = at(schema, text, "Pair<A>", 0);
  let application = named(schema, text, "@", "d", 1);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![(ARITY, default), (ARITY, application)])
  );
  let report = the(&analysis, ARITY, application);
  assert_eq!(
    report.to_string(),
    "`@d` is written without a type argument for `B`, and the default of `B` cannot fill it here"
  );
  assert_eq!(labels(report), [(default, "this default cannot fill it")]);
  assert!(completed(analysis.model(), "T")[0].is_empty());
}

/// Each distinct set of written type arguments is filled once, on `Limits::max_substitutions`:
/// `@d<String>` costs one step for the slot it leaves out and one for each of `[A]`'s two nodes,
/// three; the second `@d<String>` finds the fill made; `@d<Int>` costs three more, six in all. At
/// five the third application alone is `resource-limit`, at its path, and has no complete type
/// arguments. Refuses a fill per application, which needs nine, and a fill nobody pays for, which
/// passes at five.
#[test]
fn an_applications_fill_is_charged_once_per_set_of_written_types() {
  let text = "directive @d<A, B = [A]> repeatable on OBJECT\n\
              type Query @d<String> @d<String> @d<Int> {\n  x: Int\n}\n";
  let paid = analyze_text(text, &Limits::new().with_max_substitutions(6));
  quiet(&paid);
  assert_eq!(
    completed(paid.model(), "Query"),
    [
      vec!["string", "[string]"],
      vec!["string", "[string]"],
      vec!["i32", "[i32]"],
    ]
  );

  let short = analyze_text(text, &Limits::new().with_max_substitutions(5));
  let schema = schema_file(&short);
  let third = named(schema, text, "@", "d", 3);
  assert_eq!(found(&short), [(LIMIT, third)]);
  assert_eq!(
    the(&short, LIMIT, third).to_string(),
    "filling the type arguments this application of `@d` leaves out takes more steps of \
     substitution than `Limits::max_substitutions`, 5, admits"
  );
  assert_eq!(
    completed(short.model(), "Query"),
    [
      vec!["string", "[string]"],
      vec!["string", "[string]"],
      Vec::<&str>::new(),
    ]
  );
}

/// An application's type is an instantiation of its directive, counted toward
/// `Limits::max_instantiations` and nested a level above its type arguments: at no instantiation
/// it is `resource-limit`, and at a depth of one `@d<[Int]>` is two deep where its `[Int]` is one.
/// Refuses an application's instantiation interned uncounted, or not depth-checked.
#[test]
fn an_applications_instantiation_is_counted() {
  let text = "directive @d<T> on OBJECT\ntype Query @d<String> {\n  x: Int\n}\n";
  quiet(&analyze_text(text, &Limits::new()));
  let none = analyze_text(text, &Limits::new().with_max_instantiations(0));
  let schema = schema_file(&none);
  let application = named(schema, text, "@", "d", 1);
  assert_eq!(found(&none), [(LIMIT, application)]);
  assert_eq!(
    the(&none, LIMIT, application).to_string(),
    "this application of `@d` is one more instantiation of a generic definition than \
     `Limits::max_instantiations`, 0, admits"
  );
  assert!(completed(none.model(), "Query")[0].is_empty());

  let deep = "directive @d<T> on OBJECT\ntype Query @d<[Int]> {\n  x: Int\n}\n";
  let analysis = analyze_text(deep, &Limits::new().with_max_generic_depth(1));
  let schema = schema_file(&analysis);
  let application = named(schema, deep, "@", "d", 1);
  assert_eq!(found(&analysis), [(LIMIT, application)]);
  assert_eq!(
    the(&analysis, LIMIT, application).to_string(),
    "this application of `@d` nests deeper than `Limits::max_generic_depth`, 1, admits, an \
     application being typed a level above its type arguments"
  );
  let two = analyze_text(deep, &Limits::new().with_max_generic_depth(2));
  quiet(&two);
  assert_eq!(completed(two.model(), "Query"), [vec!["[i32]"]]);
}

/// The interfaces and types the bounds tests share, with a query root.
const NODES: &str = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
                     type User implements Node {\n  id: ID\n}\ntype Post {\n  id: ID\n}\n";

/// What a bound's report tells the author of a named type argument.
const NAMED_HELP: &str = concat!(
  "declare the interface on the type argument with exactly these type arguments, ",
  "or pass a type that declares it"
);

/// A generic directive's bounds are judged at each application, by its complete type arguments:
/// `@d<Post>` is `unsatisfied-bound` at its path, the message naming `@d`, labelled at the bound
/// and at `Post`, and `@d<User>` holds. Refuses an application left unjudged.
#[test]
fn a_generic_directives_bounds_hold_at_each_application() {
  let text = format!(
    "{NODES}directive @d<T> repeatable on OBJECT where T: Node\n\
     type A @d<User> {{\n  f: Int\n}}\ntype B @d<Post> {{\n  f: Int\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let application = named(schema, &text, "@", "d", 2);
  assert_eq!(found(&analysis), [(BOUND, application)]);
  let report = the(&analysis, BOUND, application);
  assert_eq!(
    report.to_string(),
    "`@d` bounds its type parameter `T` by `Node`, and the type argument for `T` here does not \
     satisfy that bound"
  );
  assert_eq!(
    labels(report),
    [
      (named(schema, &text, "T: ", "Node", 0), "the bound"),
      (named(schema, &text, "@d<", "Post", 0), "the type argument"),
    ]
  );
  assert_eq!(report.help(), Some(NAMED_HELP));
}

/// A type argument a directive's default fills is judged as a written one, the report labelled at
/// the default.
#[test]
fn a_filled_type_argument_is_judged_at_the_application() {
  let text =
    format!("{NODES}directive @e<T = Post> on OBJECT where T: Node\ntype C @e {{\n  f: Int\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let application = named(schema, &text, "@", "e", 1);
  assert_eq!(found(&analysis), [(BOUND, application)]);
  assert_eq!(
    labels(the(&analysis, BOUND, application)),
    [
      (named(schema, &text, "T: ", "Node", 0), "the bound"),
      (
        named(schema, &text, "T = ", "Post", 0),
        "filled from this default"
      ),
    ]
  );
}

/// Every application of one type shares its verdict, on `Limits::max_bound_steps`: one step for
/// the bound and one for `User`'s one `implements` entry, two for both applications. At one, the
/// verdict is stopped, and kept: both applications are `resource-limit`. Refuses a verdict made for
/// each application, which needs four.
#[test]
fn one_verdict_serves_every_application_of_one_instantiation() {
  let text = "type Query @d<User> @d<User> {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              type User implements Node {\n  id: ID\n}\n\
              directive @d<T> repeatable on OBJECT where T: Node\n";
  quiet(&analyze_text(text, &Limits::new().with_max_bound_steps(2)));
  let short = analyze_text(text, &Limits::new().with_max_bound_steps(1));
  let schema = schema_file(&short);
  let first = named(schema, text, "@", "d", 0);
  let second = named(schema, text, "@", "d", 1);
  assert_eq!(found(&short), [(LIMIT, first), (LIMIT, second)]);
  assert_eq!(
    the(&short, LIMIT, second).to_string(),
    "the bounds of this instantiation could not be judged within `Limits::max_bound_steps`, 1, \
     the steps judging bounds may take"
  );
}

/// An application inside a generic definition whose type argument is that definition's parameter
/// is judged through the parameter's bounds: `G`'s `T` is bounded by `Node`, and `H`'s is not,
/// whose report helps with the `where` clause. Refuses an open application left unjudged.
#[test]
fn an_application_inside_a_generic_definition_is_judged_through_its_parameters() {
  let text = "type Query {\n  x: Int\n}\ninterface Node {\n  id: ID\n}\n\
              directive @d<T> repeatable on OBJECT where T: Node\n\
              type G<T> @d<T> where T: Node {\n  f: Int\n}\ntype H<T> @d<T> {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let application = named(schema, text, "@", "d", 2);
  assert_eq!(found(&analysis), [(BOUND, application)]);
  let report = the(&analysis, BOUND, application);
  assert_eq!(
    labels(report),
    [
      (named(schema, text, "T: ", "Node", 0), "the bound"),
      (named(schema, text, "@d<", "T", 2), "the type argument"),
    ]
  );
  assert_eq!(
    report.help(),
    Some("bound this type parameter by the interface in this definition's `where` clause")
  );
}

/// The value `id`, written out: a coerced value as its kind's name and its text, `Int 1`, `String
/// "q"`; `List [Int 1]`; an input object as `Object {a: Int 1, b= Int 2}`, a field a default filled
/// marked `=` for its `:`; `Set {Int 1}`; a literal kept as written as `Literal(Int) 1`; and an
/// unresolved value as `?`.
fn valued(model: &Model, id: ValueId) -> String {
  let value = model.value(id);
  let joined = |items: &[ValueId]| -> String {
    let items: Vec<String> = items
      .iter()
      .map(|item: &ValueId| valued(model, *item))
      .collect();
    items.join(", ")
  };
  match value.kind() {
    ValueKind::Null => String::from("Null null"),
    ValueKind::Int(int) => format!("Int {}", int.text()),
    ValueKind::Float(float) => format!("Float {}", float.text()),
    ValueKind::String(text) => format!("String {text:?}"),
    ValueKind::List(items) => format!("List [{}]", joined(items)),
    ValueKind::Set(items) => format!("Set {{{}}}", joined(items)),
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

/// The application at `nth` of those on the root package's type `owner`.
fn application_on(model: &Model, owner: &str, nth: usize) -> ApplicationId {
  model.def(type_def(model, owner)).applications()[nth]
}

/// The value of `application`'s argument named `name`, written or a copy: `None` when it has none.
fn argument(model: &Model, application: ApplicationId, name: &str) -> Option<ValueId> {
  model
    .application(application)
    .arguments()
    .iter()
    .find(|argument| model.member(argument.argument()).name() == name)
    .map(|argument| argument.value())
}

/// The value of the argument named `name` of the application at `nth` on `owner`, written out.
fn argument_shown(model: &Model, owner: &str, nth: usize, name: &str) -> String {
  argument(model, application_on(model, owner, nth), name).map_or_else(
    || String::from("absent"),
    |value: ValueId| valued(model, value),
  )
}

/// The type of the argument named `name` of the application at `nth` on `owner`, written out.
fn argument_ty(model: &Model, owner: &str, nth: usize, name: &str) -> String {
  let value = argument(model, application_on(model, owner, nth), name).expect("an argument");
  shown(model, model.value(value).ty())
}

/// A generic directive's argument is coerced against its type under the application's type
/// arguments: `a: T` takes `Int` and `l: [T]` a lone value as `[Int]` under `@d<Int>`, and `1` is
/// no `String`. Refuses a parameter-typed argument kept as written at a closed application.
#[test]
fn a_generic_directives_argument_takes_the_type_its_application_gives_it() {
  let text = "type Query @d<Int>(a: 1, l: 2) @d<String>(a: 1) {\n  x: Int\n}\n\
              directive @d<T>(a: T, l: [T]) repeatable on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARGUMENT, after(schema, text, "@d<String>(a: ", "1"))]
  );
  let model = analysis.model();
  assert_eq!(argument_shown(model, "Query", 0, "a"), "Int 1");
  assert_eq!(argument_ty(model, "Query", 0, "a"), "i32");
  assert_eq!(argument_shown(model, "Query", 0, "l"), "List [Int 2]");
  assert_eq!(argument_ty(model, "Query", 0, "l"), "[i32]");
  assert_eq!(argument_shown(model, "Query", 1, "a"), "?");
  assert_eq!(argument_ty(model, "Query", 1, "a"), "string");
}

/// "Required" is read under the application's type arguments: `@d<Int!>` and `@e`, whose default
/// fills `Int!`, leave a required argument out, `@d<Int>` does not, and `@d<Int!>` gives it `null`.
/// Refuses "required" read off the written `!`.
#[test]
fn required_is_read_under_the_applications_type_arguments() {
  let text = "type Query @d<Int!> @d<Int> @d<Int!>(a: null) @e {\n  x: Int\n}\n\
              directive @d<T>(a: T) repeatable on OBJECT\n\
              directive @e<T = Int!>(a: T) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    sorted(found(&analysis)),
    sorted(vec![
      (MISSING, named(schema, text, "@", "d", 0)),
      (MISSING, named(schema, text, "(", "a", 0)),
      (MISSING, named(schema, text, "@", "e", 0)),
    ])
  );
}

/// An argument that may be required, a bare nullable type parameter, costs a step each time an
/// application leaves it out, whatever its type turns out to be: the first `@m<Int>` three and one
/// for rebuilding `T`, the second three, seven in all. At six the second application alone is
/// `resource-limit`. Refuses entries walked for nothing, which pass at six.
#[test]
fn an_argument_that_may_be_required_costs_a_step_when_left_out() {
  let text = "type Query @m<Int> @m<Int> {\n  x: Int\n}\n\
              directive @m<T>(a: T, b: T, c: T) repeatable on OBJECT\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(7)));
  let short = analyze_text(text, &Limits::new().with_max_value_steps(6));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, named(schema, text, "@", "m", 1))]);
}

/// With no complete type arguments, an argument that may be required is passed over with no step,
/// each run of them at once, and the arguments after it are read as ever. `@d<Int, Int>`'s count
/// is refused, so it has none: `a` and `c` are passed over; `b`, required, is reported left out,
/// a step; and `e` takes a copy of its default, its one row a step. At two steps nothing more is
/// said, and at one `e`'s copy is past the budget, at the path. Refuses a step for an argument
/// passed over, which needs four, and a pass that runs over `b` or `e`.
#[test]
fn an_application_with_no_complete_type_arguments_passes_over_what_may_be_required() {
  let text = "type Query @d<Int, Int> {\n  x: Int\n}\n\
              directive @d<T>(a: T, b: Int!, c: T, e: Int = 1) on OBJECT\n";
  let codes = [ARITY, DEFAULT, ARGUMENT, MISSING, LIMIT];
  let paid = analyze_text(text, &Limits::new().with_max_value_steps(2));
  let schema = schema_file(&paid);
  let path = named(schema, text, "@", "d", 0);
  assert_eq!(
    sorted(found_of(&paid, &codes)),
    sorted(vec![(ARITY, path), (MISSING, path)])
  );
  let model = paid.model();
  assert_eq!(argument_shown(model, "Query", 0, "e"), "Int 1");
  for name in ["a", "b", "c"] {
    assert_eq!(argument_shown(model, "Query", 0, name), "absent");
  }
  let short = analyze_text(text, &Limits::new().with_max_value_steps(1));
  assert_eq!(
    sorted(found_of(&short, &codes)),
    sorted(vec![(ARITY, path), (MISSING, path), (LIMIT, path)])
  );
  assert_eq!(argument_shown(short.model(), "Query", 0, "e"), "absent");
}

/// A default left out is copied under the application's type arguments: `[1]` against `[Int]`,
/// each row typed under them, not written, and located at the default. Refuses a copy against the
/// written type, which keeps its literal.
#[test]
fn a_default_is_copied_under_the_applications_type_arguments() {
  let text = "type Query @d<Int> {\n  x: Int\n}\n\
              directive @d<T>(a: [T] = [1]) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let copy = argument(model, application_on(model, "Query", 0), "a").expect("a copy");
  assert_eq!(valued(model, copy), "List [Int 1]");
  assert_eq!(shown(model, model.value(copy).ty()), "[i32]");
  assert!(!model.value(copy).written());
  assert_eq!(model.value(copy).location(), at(schema, text, "[1]", 0));
  let ValueKind::List(items) = model.value(copy).kind() else {
    panic!("no list");
  };
  assert_eq!(shown(model, model.value(items[0]).ty()), "i32");
}

/// A default that does not fit the type an application's type arguments give it is reported once,
/// at the application that leaves it out, labelled at the default, and its copy is made, silently:
/// `@d<String>`'s `1`. `@d<Int>`'s fits, and an application that writes the argument uses no
/// default. Refuses a silent copy, a report at the default, and one at an application that writes
/// the argument.
#[test]
fn a_default_that_does_not_fit_is_reported_at_the_application() {
  let text = "type Query @d<String> @d<Int> @d<String>(a: \"x\") {\n  x: Int\n}\n\
              directive @d<T>(a: T = 1) repeatable on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = named(schema, text, "@", "d", 0);
  assert_eq!(found(&analysis), [(DEFAULT, first)]);
  let report = the(&analysis, DEFAULT, first);
  assert_eq!(
    report.to_string(),
    "under this application's type arguments, the default `1` of `a` does not fit the type `a` \
     takes"
  );
  let default = after(schema, text, "T = ", "1");
  assert_eq!(labels(report), [(default, "this default")]);
  let model = analysis.model();
  assert_eq!(argument_shown(model, "Query", 0, "a"), "?");
  let copy = argument(model, application_on(model, "Query", 0), "a").expect("a copy");
  assert_eq!(model.value(copy).location(), default);
  assert_eq!(argument_shown(model, "Query", 1, "a"), "Int 1");
  assert_eq!(argument_shown(model, "Query", 2, "a"), "String \"x\"");
}

/// An application reports its first default that does not fit, and no other: two `@d<String>`,
/// each once, labelled at `a`'s default. Refuses a report for each default.
#[test]
fn one_diagnostic_per_application_for_its_first_failing_default() {
  let text = "type Query @d<String> @d<String> {\n  x: Int\n}\n\
              directive @d<T>(a: T = 1, b: T = 2) repeatable on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let (first, second) = (
    named(schema, text, "@", "d", 0),
    named(schema, text, "@", "d", 1),
  );
  assert_eq!(found(&analysis), [(DEFAULT, first), (DEFAULT, second)]);
  let default = after(schema, text, "a: T = ", "1");
  for at in [first, second] {
    assert_eq!(
      labels(the(&analysis, DEFAULT, at)),
      [(default, "this default")]
    );
  }
}

/// A copy is judged whole under the application's type arguments, as a written value is: a field
/// its input object does not declare, a required field left out, a OneOf literal of no field, and
/// a set that holds one element twice each make the copy unfit, though no row of it is unresolved.
/// Refuses a fit read off unresolved rows alone, which none of the four makes.
#[test]
fn a_copy_is_judged_whole_under_the_applications_type_arguments() {
  let text = "type Query @u<In> @n<Need> @o<O> @s<Int> {\n  x: Int\n}\n\
              input In {\n  x: Int\n}\ninput Need {\n  y: Int!\n}\n\
              input O @oneOf {\n  a: Int\n  b: Int\n}\nscalar Bag<T> @set\n\
              directive @u<T>(a: T = { z: 1 }) on OBJECT\n\
              directive @n<T>(a: T = {}) on OBJECT\n\
              directive @o<T>(a: T = {}) on OBJECT\n\
              directive @s<T>(a: Bag<T> = set!{1, 0x1}) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found_of(&analysis, &PHASE),
    [
      (DEFAULT, named(schema, text, "@", "u", 0)),
      (DEFAULT, named(schema, text, "@", "n", 0)),
      (DEFAULT, named(schema, text, "@", "o", 0)),
      (DEFAULT, named(schema, text, "@", "s", 0)),
    ]
  );
  let model = analysis.model();
  assert_eq!(argument_shown(model, "Query", 0, "a"), "Object {}");
  assert_eq!(argument_shown(model, "Query", 1, "a"), "Object {}");
  assert_eq!(argument_shown(model, "Query", 2, "a"), "Object {}");
  assert_eq!(
    argument_shown(model, "Query", 3, "a"),
    "Set {Int 1, Int 0x1}"
  );
}

/// A count under a type its default is not written against pays a step for each node: the first
/// `@d<Float>` rebuilds `[T]`, two, counts four nodes, four, and copies four rows, four; the
/// second reads the kept count, one, and copies four; `@d<Int>` rebuilds two, counts four and
/// copies four: twenty-five. At twenty-four the third alone is `resource-limit`, with no `a`.
/// Refuses a judged count nobody pays for, which passes at twenty-four, and a count made again for
/// a type already counted, which fails at twenty-five.
#[test]
fn the_count_under_type_arguments_is_charged_as_it_is_counted() {
  let text = "type Query @d<Float> @d<Float> @d<Int> {\n  x: Int\n}\n\
              directive @d<T>(a: [T] = [1, 2, 3]) repeatable on OBJECT\n";
  let paid = analyze_text(text, &Limits::new().with_max_value_steps(25));
  quiet(&paid);
  assert_eq!(
    argument_shown(paid.model(), "Query", 0, "a"),
    "List [Float 1, Float 2, Float 3]"
  );
  assert_eq!(argument_ty(paid.model(), "Query", 1, "a"), "[f64]");
  let short = analyze_text(text, &Limits::new().with_max_value_steps(24));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, named(schema, text, "@", "d", 2))]);
  assert_eq!(argument_shown(short.model(), "Query", 2, "a"), "absent");
}

/// A type that cannot be built under the application's type arguments is a limit: under a depth of
/// two, `[[T]]` under `@w<[Int]>` is three deep, and its written value is one unresolved row with
/// `resource-limit` at it; an application that leaves such an argument out is starved, at its
/// path. Refuses a rebuild past the depth.
#[test]
fn an_arguments_type_that_cannot_be_built_is_a_limit() {
  let text = "type Query @w<[Int]>(a: 1) @v<[Int]> {\n  x: Int\n}\n\
              directive @w<T>(a: [[T]]) on OBJECT\n\
              directive @v<T>(a: [[T]] = [[1]]) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(2));
  let schema = schema_file(&analysis);
  let written = after(schema, text, "@w<[Int]>(a: ", "1");
  let left = named(schema, text, "@", "v", 0);
  assert_eq!(found(&analysis), [(LIMIT, written), (LIMIT, left)]);
  assert_eq!(
    the(&analysis, LIMIT, written).to_string(),
    "this value takes a type that, under these type arguments, nests deeper than \
     `Limits::max_generic_depth`, 2, admits"
  );
  assert_eq!(
    the(&analysis, LIMIT, left).to_string(),
    "the arguments this application leaves out take a type that, under these type arguments, \
     nests deeper than `Limits::max_generic_depth`, 2, admits"
  );
  let model = analysis.model();
  assert_eq!(argument_shown(model, "Query", 0, "a"), "?");
  assert_eq!(argument_ty(model, "Query", 0, "a"), "[[w#0]]");
  assert_eq!(argument_shown(model, "Query", 1, "a"), "absent");
}

/// A value whose type cannot be built keeps the path it is written as, though its row is
/// unresolved, as every row of a written path does: under a depth of two, `RED` for `@w<[Int]>`'s
/// three-deep `a`, and for `c` of the literal `{ c: RED }` of `In<[Int]>`, two deep, whose `[[U]]`
/// is three deep there, each with `resource-limit` at it. Refuses the path of an unbuilt row
/// dropped.
#[test]
fn an_unbuilt_value_keeps_the_path_it_is_written_as() {
  let text = "type Query @w<[Int]>(a: RED, b: { c: RED }) {\n  x: Int\n}\n\
              directive @w<T>(a: [[T]], b: In<T>) on OBJECT\ninput In<U> {\n  c: [[U]]\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, after(schema, text, "(a: ", "RED")),
      (LIMIT, after(schema, text, "{ c: ", "RED")),
    ]
  );
  let model = analysis.model();
  let application = application_on(model, "Query", 0);
  let a = argument(model, application, "a").expect("`a`'s value");
  let b = argument(model, application, "b").expect("`b`'s value");
  let ValueKind::Object([c]) = model.value(b).kind() else {
    panic!("`b` is no object of one field");
  };
  for id in [a, c.value()] {
    let value = model.value(id);
    assert!(
      matches!(value.kind(), ValueKind::Unresolved),
      "value {}",
      id.get()
    );
    let path = value.path().expect("the path it is written as");
    assert_eq!(path.segments().len(), 1);
    assert_eq!(model.name(path.segments()[0]), "RED");
  }
}

/// An application inside a generic definition whose type arguments name its parameters keeps its
/// arguments' literals, written and copied, each typed with that parameter, and says nothing.
/// Refuses a value under an open type coerced.
#[test]
fn an_application_inside_a_generic_definition_keeps_its_literals() {
  let text = "type Query {\n  x: Int\n}\ndirective @d<X>(a: X, b: X = 2) on OBJECT\n\
              type G<T> @d<T>(a: \"x\") {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  assert_eq!(argument_shown(model, "G", 0, "a"), "Literal(String) \"x\"");
  assert_eq!(argument_shown(model, "G", 0, "b"), "Literal(Int) 2");
  assert_eq!(argument_ty(model, "G", 0, "a"), "G#0");
  assert_eq!(argument_ty(model, "G", 0, "b"), "G#0");
}

/// The member named `name` among `members`.
fn member_named(
  model: &Model,
  members: impl IntoIterator<Item = MemberId>,
  name: &str,
) -> MemberId {
  members
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == name)
    .unwrap_or_else(|| panic!("no member `{name}`"))
}

/// The default of the input field `name` of the root package's input object `owner`.
fn input_default(model: &Model, owner: &str, name: &str) -> ValueId {
  let field = member_named(
    model,
    model.def(type_def(model, owner)).input_fields(),
    name,
  );
  model.member(field).default().expect("a default")
}

/// The default of the argument `argument` of the field `field` of the root package's type `owner`.
fn argument_default(model: &Model, owner: &str, field: &str, argument: &str) -> ValueId {
  let field = member_named(model, model.def(type_def(model, owner)).fields(), field);
  let argument = member_named(model, model.member(field).arguments(), argument);
  model.member(argument).default().expect("a default")
}

/// A default a literal of a generic input object leaves out is copied under the object's type
/// arguments: `[1]` against `[Int]` under `In<Int>`. Refuses a copy against the written type.
#[test]
fn a_generic_fields_default_is_copied_under_the_objects_arguments() {
  let text = "type Query {\n  x: Int\n}\ninput In<T> {\n  v: [T] = [1]\n}\n\
              input H {\n  x: In<Int> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  let x = input_default(model, "H", "x");
  assert_eq!(valued(model, x), "Object {v= List [Int 1]}");
  let ValueKind::Object(entries) = model.value(x).kind() else {
    panic!("no object");
  };
  assert_eq!(shown(model, model.value(entries[0].value()).ty()), "[i32]");
  assert_eq!(shown(model, model.value(x).ty()), "In<i32>");
}

/// "Required" is read under the object's type arguments: `In<Int!>` requires `v`, given `null` or
/// left out, and `In<Int>` does not. Refuses "required" read off the written `!`.
#[test]
fn required_is_read_under_the_objects_arguments() {
  let text = "type Query {\n  x: Int\n}\ninput In<T> {\n  v: T\n}\n\
              input H {\n  a: In<Int!> = {}\n  b: In<Int> = {}\n  c: In<Int!> = { v: null }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let required = "smear::schema::missing-required-input-object-field";
  assert_eq!(
    found(&analysis),
    [
      (required, after(schema, text, "a: In<Int!> = ", "{}")),
      (required, at(schema, text, "null", 0)),
    ]
  );
}

/// A field that may be required, a bare nullable type parameter, costs a step each time a written
/// literal leaves it out, whatever its type turns out to be: three, and one for rebuilding `T`,
/// four. At three the literal is `resource-limit`. Refuses entries walked for nothing.
#[test]
fn a_field_that_may_be_required_costs_a_step_when_left_out() {
  let text = "type Query {\n  x: Int\n}\ninput In<T> {\n  a: T\n  b: T\n  c: T\n}\n\
              input H {\n  x: In<Int> = {}\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(4)));
  let short = analyze_text(text, &Limits::new().with_max_value_steps(3));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, at(schema, text, "{}", 0))]);
}

/// A literal whose type a rebuild made reports the copy of a default it leaves out that does not
/// fit: `@w<String>(a: {})` gives `{}` the type `Box<String>`, under which `Box`'s `1` does not
/// fit, reported once at `{}`, labelled at the `1`. Refuses a copy silent under a derived type.
#[test]
fn a_copy_inside_a_written_argument_is_judged_at_its_literal() {
  let text = "type Query @w<String>(a: {}) {\n  x: Int\n}\ninput Box<U> {\n  v: U = 1\n}\n\
              directive @w<T>(a: Box<T>) on OBJECT\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let literal = at(schema, text, "{}", 0);
  assert_eq!(found(&analysis), [(DEFAULT, literal)]);
  let report = the(&analysis, DEFAULT, literal);
  assert_eq!(
    report.to_string(),
    "this literal leaves out `v`, whose default `1` does not fit the type `v` takes under these \
     type arguments"
  );
  assert_eq!(
    labels(report),
    [(after(schema, text, "U = ", "1"), "this default")]
  );
  assert_eq!(
    argument_shown(analysis.model(), "Query", 0, "a"),
    "Object {v= ?}"
  );
}

/// A literal under derived arguments, a field's type rebuilt under the enclosing literal's, reports
/// its copy that does not fit, though no application is near and no occurrence writes its type:
/// `Box<String>` is derived from `Wrap<String>`. Refuses a silent `Unresolved` copy in an ok
/// analysis.
#[test]
fn a_literal_under_derived_arguments_reports_its_copy() {
  let text = "type Query {\n  x: Int\n}\ninput Box<U> {\n  v: U = 1\n}\n\
              input Wrap<W> {\n  w: Box<W>\n}\ninput H {\n  h: Wrap<String> = { w: {} }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let literal = after(schema, text, "{ w: ", "{}");
  assert_eq!(found(&analysis), [(DEFAULT, literal)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, literal)),
    [(after(schema, text, "U = ", "1"), "this default")]
  );
  let model = analysis.model();
  assert_eq!(
    valued(model, input_default(model, "H", "h")),
    "Object {w: Object {v= ?}}"
  );
}

/// A literal whose type a type parameter's default filled reports its copy that does not fit,
/// though no rebuild made its type: `Dict<String>`'s value type is `V`'s default `In<K>` filled
/// under `K = string!`, `In<string!>`, which no type occurrence writes, so no instantiation's check
/// reads it. `{}` leaves out `In`'s `1`, which `string!` does not take: one
/// `invalid-default-value`, at `{}`, labelled at the `1`, and the analysis is not ok. Refuses an ok
/// analysis that holds an unresolved copy.
#[test]
fn a_literal_under_a_filled_type_reports_its_copy() {
  let text = "type Query {\n  x: Int\n}\ninput In<T> {\n  a: T = 1\n}\n\
              scalar Dict<K, V = In<K>> @map\n\
              input H {\n  h: Dict<String> = map!{ \"k\" => {} }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let literal = at(schema, text, "{}", 0);
  assert_eq!(found(&analysis), [(DEFAULT, literal)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, literal)),
    [(after(schema, text, "T = ", "1"), "this default")]
  );
  assert!(!analysis.is_ok());
  let model = analysis.model();
  let ValueKind::Map(&[(key, value)]) = model.value(input_default(model, "H", "h")).kind() else {
    panic!("`h` is no map of one entry");
  };
  assert_eq!(valued(model, key), "String \"k\"");
  assert_eq!(valued(model, value), "Object {a= ?}");
}

/// A literal whose type a type occurrence writes leaves its copy that does not fit to that
/// occurrence's check, which reports it once, where the type is written, labelled at the value:
/// `W`'s `In<String!>` is the `In<string!>` a fill gives `Dict<String>`'s value, and `Y`'s
/// `Box<String>` the type a rebuild gives `@w<String>`'s literal. Refuses a second report, at the
/// literal.
#[test]
fn a_literal_whose_type_is_written_leaves_its_copy_to_that_occurrence() {
  let filled = "type Query {\n  x: Int\n}\ninput In<T> {\n  a: T = 1\n}\n\
                scalar Dict<K, V = In<K>> @map\n\
                input H {\n  h: Dict<String> = map!{ \"k\" => {} }\n}\n\
                input W {\n  w: In<String!>\n}\n";
  let rebuilt = "type Query @w<String>(a: {}) {\n  x: Int\n}\ninput Box<U> {\n  v: U = 1\n}\n\
                 directive @w<T>(a: Box<T>) on OBJECT\ninput Y {\n  y: Box<String>\n}\n";
  for (text, written, before) in [
    (filled, "In<String!>", "T = "),
    (rebuilt, "Box<String>", "U = "),
  ] {
    let analysis = analyze_text(text, &Limits::new());
    let schema = schema_file(&analysis);
    let occurrence = at(schema, text, written, 0);
    assert_eq!(found(&analysis), [(DEFAULT, occurrence)], "{text}");
    assert_eq!(
      labels(the(&analysis, DEFAULT, occurrence)),
      [(after(schema, text, before, "1"), "this value")],
      "{text}"
    );
  }
}

/// A field's default read again under other type arguments is a count of its own, not a cycle:
/// `P.f` under `Q` reads `Q.p`, which reads `P.f` under `R`, and the fill ends. Its answer does not
/// change with the order the definitions are written in. Refuses a stop by field, which poisons
/// `P.f` under `R` and leaves `f` absent.
#[test]
fn a_field_read_again_under_other_arguments_is_no_cycle() {
  let forward = "type Query {\n  x(y: P<Q> = {}): Int\n}\ninput P<T> {\n  f: T = {}\n}\n\
                 input Q {\n  p: P<R> = {}\n}\ninput R {\n  z: Int = 1\n}\n";
  let backward = "input R {\n  z: Int = 1\n}\ninput Q {\n  p: P<R> = {}\n}\n\
                  input P<T> {\n  f: T = {}\n}\ntype Query {\n  x(y: P<Q> = {}): Int\n}\n";
  for text in [forward, backward] {
    let analysis = analyze_text(text, &Limits::new());
    quiet(&analysis);
    let model = analysis.model();
    assert_eq!(
      valued(model, argument_default(model, "Query", "x", "y")),
      "Object {f= Object {p= Object {f= Object {z= Int 1}}}}"
    );
  }
}

/// A default that fills itself under growing type arguments ends where a type it would read passes
/// `Limits::max_generic_depth`: `resource-limit` at the literal that asks, and the field absent.
/// Refuses a count that never ends.
#[test]
fn growing_arguments_end_at_the_depth_limit() {
  let text = "type Query {\n  f(x: G<Int> = {}): Int\n}\ninput G<T> {\n  g: G<[T]> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let literal = at(schema, text, "{}", 0);
  // `G<Int>`'s check, whose own diagnostic is the next test's.
  assert_eq!(
    found_of(&analysis, &[DEFAULT, LIMIT]),
    [(LIMIT, at(schema, text, "G<Int>", 0)), (LIMIT, literal)]
  );
  assert_eq!(
    the(&analysis, LIMIT, literal).to_string(),
    "the fields this literal leaves out take a type that, under these type arguments, nests \
     deeper than `Limits::max_generic_depth`, 64, admits"
  );
  let model = analysis.model();
  assert_eq!(
    valued(model, argument_default(model, "Query", "f", "x")),
    "Object {}"
  );
}

/// A default whose type names its definition's type parameter is checked at each closed
/// instantiation the program writes: `In<String>` is `invalid-default-value`, at the occurrence,
/// labelled at the `1`, and `In<Int>` fits. The default stays its definition's literal. Refuses the
/// check never made, and a value row made by it.
#[test]
fn a_default_under_type_parameters_is_checked_at_each_written_instantiation() {
  let text = "type Query {\n  x(f: In<Int>): Int\n  y(f: In<String>): Int\n}\n\
              input In<T> {\n  a: T = 1\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "In<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  let report = the(&analysis, DEFAULT, occurrence);
  assert_eq!(
    report.to_string(),
    "`In` is instantiated here with type arguments under which `1`, a value it holds, does not fit \
     the type it takes"
  );
  assert_eq!(
    labels(report),
    [(after(schema, text, "a: T = ", "1"), "this value")]
  );
  let model = analysis.model();
  assert_eq!(
    valued(model, input_default(model, "In", "a")),
    "Literal(Int) 1"
  );
  // The check makes no row: the root package's one value is `In`'s own default.
  let mut reached = Values::default();
  let _ = visit(&analysis, Packages::Supplied, &mut reached);
  assert_eq!(reached.ids.len(), 1);
}

/// Each occurrence of an instantiation whose values do not fit is reported once, for its first
/// value in the definition's order: three occurrences, three reports, each labelled at `a1`'s.
/// Refuses a report for each value at each occurrence.
#[test]
fn one_diagnostic_per_occurrence_for_the_first_value_that_does_not_fit() {
  let text = "type Query {\n  x(f: In<String>): Int\n  y(f: In<String>): Int\n  \
              z(f: In<String>): Int\n}\ninput In<T> {\n  a1: T = 1\n  a2: T = 2\n  a3: T = 3\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrences: Vec<Location> = (0..3)
    .map(|nth: usize| at(schema, text, "In<String>", nth))
    .collect();
  assert_eq!(
    found(&analysis),
    occurrences
      .iter()
      .map(|at: &Location| (DEFAULT, *at))
      .collect::<Vec<(&str, Location)>>()
  );
  let first = after(schema, text, "a1: T = ", "1");
  for at in occurrences {
    assert_eq!(labels(the(&analysis, DEFAULT, at)), [(first, "this value")]);
  }
}

/// An instantiation's two nullnesses share one verdict, on `Limits::max_value_steps`: `In<Int>`
/// costs a step for its pair, two to rebuild `[T]` and three for the nodes of `[1, 2]`, six;
/// `In<Int>!` nothing; `In<Float>` six; twelve. At eleven `In<Float>` alone is `resource-limit`.
/// Refuses a verdict per nullness, which needs eighteen, and an uncharged count.
#[test]
fn both_nullnesses_share_one_verdict() {
  let text = "type Query {\n  f(x: In<Int>): Int\n  g(y: In<Int>!): Int\n  \
              h(z: In<Float>): Int\n}\ninput In<T> {\n  a: [T] = [1, 2]\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(12)));
  let short = analyze_text(text, &Limits::new().with_max_value_steps(11));
  let schema = schema_file(&short);
  let float = at(schema, text, "In<Float>", 0);
  assert_eq!(found(&short), [(LIMIT, float)]);
  assert_eq!(
    the(&short, LIMIT, float).to_string(),
    "the values `In` holds could not be checked under the type arguments it is instantiated with \
     here within `Limits::max_value_steps`, 11, the steps the analysis may take to complete values"
  );
}

/// Each pair of a distinct instantiation and a value its definition records is a step: fifty
/// values of `In<T, U>` under fifty `In<Int, Xj>` share every count after the first
/// instantiation's, and still cost a step each pair, 2,600 in all. The first instantiation pays
/// fifty pairs, one rebuild of `T` and fifty counts of one node, 101; each of the other forty-nine
/// fifty pairs and one rebuild, its counts kept, 51: 101 + 49 × 51 = 2,600. At 2,599 the last,
/// `In<Int, X50>`, alone is `resource-limit`. Refuses verdicts read off shared counts for nothing,
/// which cost 100.
#[test]
fn every_pair_of_an_instantiation_and_a_value_is_a_step() {
  let mut text = String::from("type Query {\n");
  for j in 1..=50 {
    text.push_str(&format!("  f{j}(x: In<Int, X{j}>): Int\n"));
  }
  text.push_str("}\ninput In<T, U> {\n");
  for k in 1..=50 {
    text.push_str(&format!("  a{k}: T = 1\n"));
  }
  text.push_str("}\n");
  for j in 1..=50 {
    text.push_str(&format!("input X{j} {{\n  v: Int\n}}\n"));
  }
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_value_steps(2600),
  ));
  let short = analyze_text(&text, &Limits::new().with_max_value_steps(2599));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, at(schema, &text, "In<Int, X50>", 0))]
  );
}

/// A field argument's default is a value its type records too: `Page<String>`'s `10`.
#[test]
fn a_field_arguments_default_is_checked_too() {
  let text = "type Query {\n  p: Page<String>\n  q: Page<Int>\n}\n\
              type Page<T> {\n  items(first: T = 10): [T]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Page<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, text, "T = ", "10"), "this value")]
  );
}

/// An argument of an application on a generic definition whose type names its parameters is one of
/// its values: `G<Int>` refuses `"x"`, with the argument's code. Refuses the check of defaults
/// alone.
#[test]
fn an_application_on_a_generic_definition_is_checked_at_its_instantiations() {
  let text = "type Query {\n  g: G<Int>\n  h: G<String>\n}\n\
              directive @d<X>(a: X) on OBJECT\ntype G<T> @d<T>(a: \"x\") {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "G<Int>", 0);
  assert_eq!(found(&analysis), [(ARGUMENT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, ARGUMENT, occurrence)),
    [(after(schema, text, "(a: ", "\"x\""), "this value")]
  );
}

/// A default under derived arguments is checked through the counts it reads: `In<String>`'s `{}`
/// leaves out `Box`'s `1`, under `Box<String>`, which no occurrence writes. Refuses a check that
/// stops at the first level.
#[test]
fn a_nested_default_under_derived_arguments_is_checked() {
  let text = "type Query {\n  f(x: In<String>): Int\n}\ninput Box<U> {\n  v: U = 1\n}\n\
              input In<T> {\n  a: Box<T> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "In<String>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, text, "a: Box<T> = ", "{}"), "this value")]
  );
}

/// A default whose type is closed was judged where it is written, and a check that reads it says
/// nothing more: `Pair`'s `n`. Refuses a judged count that judges a frame of a closed type.
#[test]
fn a_default_judged_where_it_is_written_is_not_reported_again() {
  let text = "type Query {\n  f(x: In<Int>): Int\n}\n\
              input Pair<T> {\n  t: T = 1\n  n: Int = \"s\"\n}\n\
              input In<T> {\n  a: Pair<T> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(found(&analysis), [(DEFAULT, at(schema, text, "\"s\"", 0))]);
}

/// A check a limit stops is `resource-limit` at the occurrence, naming the limit: `G<Int>`'s value
/// `{}` reads types that grow past `Limits::max_generic_depth`.
#[test]
fn a_check_a_limit_stops_is_reported_at_the_occurrence() {
  let text = "type Query {\n  f(x: G<Int> = {}): Int\n}\ninput G<T> {\n  g: G<[T]> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    the(&analysis, LIMIT, at(schema, text, "G<Int>", 0)).to_string(),
    "a value `G` holds takes a type that, under these type arguments, nests deeper than \
     `Limits::max_generic_depth`, 64, admits"
  );
}

/// A count that meets itself still judges the values it reached. `Rec<Int>`'s check counts
/// `next`'s default under `Rec<Int>`, judged: its object pushes `"bad"` against `Int`, `value`'s
/// type there, then the read of `next`'s default, its own. The stack takes the read first, which
/// meets the count being made: the count is poisoned, no copy is made, and the read is cut. Then
/// `"bad"`, which `Int` does not take: the count does not fit, so `Rec<Int>` is
/// `invalid-default-value`, the code of a default's site, at the occurrence, labelled at the
/// value. `next`'s default stays its literal. The cycle is another rule's, so this phase's codes
/// alone are read. Refuses a poison that clears the check pending beside it.
#[test]
fn a_count_that_meets_itself_still_judges_the_values_it_reached() {
  let text = "type Query {\n  f(arg: Rec<Int>): Int\n}\n\
              input Rec<T> {\n  value: T\n  next: Rec<T> = { value: \"bad\" }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Rec<Int>", 0);
  assert_eq!(found_of(&analysis, &PHASE), [(DEFAULT, occurrence)]);
  let report = the(&analysis, DEFAULT, occurrence);
  assert_eq!(
    report.to_string(),
    "`Rec` is instantiated here with type arguments under which `{ value: \"bad\" }`, a value it \
     holds, does not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [(
      after(schema, text, "next: Rec<T> = ", "{ value: \"bad\" }"),
      "this value"
    )]
  );
  let model = analysis.model();
  assert_eq!(
    valued(model, input_default(model, "Rec", "next")),
    "Literal(Object) { value: \"bad\" }"
  );
}

/// The same count, its `1` taking `Int`, fits: the count is poisoned as before, and nothing it
/// reached is refused, so this phase says nothing, and the cycle is another rule's to report.
/// Refuses a poison taken for a value that does not fit.
#[test]
fn a_count_that_meets_itself_with_values_that_fit_says_nothing() {
  let text = "type Query {\n  f(arg: Rec<Int>): Int\n}\n\
              input Rec<T> {\n  value: T\n  next: Rec<T> = { value: 1 }\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(
    found_of(&analysis, &PHASE).is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// What a count reaches only after it meets itself is judged too. In `Rec`, `["bad"]` against
/// `[Int]` is pushed under the read of `next`'s default, which the stack takes first and which
/// meets the count; the list is counted after that, and its `"bad"` only then. In `Ring`, `{}`
/// against `Box<Int>` is likewise counted after the read that meets the count, and leaves out
/// `Box`'s `v`, whose `"bad"` its read reaches under `Int`: a count of its own, opened after the
/// poison, exact, and not fitting, which `Ring<Int>`'s count holds. No occurrence writes
/// `Box<Int>`, so this count alone judges it. Each program is one `invalid-default-value` at its
/// written instantiation, labelled at the value it holds. Refuses a count that stops judging where
/// it meets itself.
#[test]
fn what_a_count_reaches_after_it_meets_itself_is_judged() {
  let nested = "type Query {\n  f(arg: Rec<Int>): Int\n}\n\
                input Rec<T> {\n  value: [T]\n  next: Rec<T> = { value: [\"bad\"] }\n}\n";
  let analysis = analyze_text(nested, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, nested, "Rec<Int>", 0);
  assert_eq!(found_of(&analysis, &PHASE), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(
      after(schema, nested, "next: Rec<T> = ", "{ value: [\"bad\"] }"),
      "this value"
    )]
  );

  let read = "type Query {\n  f(arg: Ring<Int>): Int\n}\ninput Box<U> {\n  v: U = \"bad\"\n}\n\
              input Ring<T> {\n  box: Box<T>\n  next: Ring<T> = { box: {} }\n}\n";
  let analysis = analyze_text(read, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, read, "Ring<Int>", 0);
  assert_eq!(found_of(&analysis, &PHASE), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(
      after(schema, read, "next: Ring<T> = ", "{ box: {} }"),
      "this value"
    )]
  );
}

/// A default whose copy would hold a copy of itself is judged where it is left out, by what its
/// count reached, and is absent. `@d<Int>` leaves out `a`, whose default is counted under
/// `Rec<Int>`: it reads `next`'s default, whose own read of itself poisons it, and so `a`'s count;
/// `a`'s `"bad"` is counted after, and `Int` does not take it. So the application reports `a`'s
/// default, at its path. `H.h`'s `{ r: {} }` gives its `{}` the type `Rec<Int>`, rebuilt under
/// `Wrap<Int>`; `{}` leaves out `next`, whose count reads itself, and whose `"bad"` is counted
/// after: the literal reports it, at the `{}`. Refuses an absent default taken to fit.
#[test]
fn a_default_that_meets_itself_is_judged_where_it_is_left_out() {
  let applied = "type Query @d<Int> {\n  x: Int\n}\n\
                 directive @d<T>(a: Rec<T> = { value: \"bad\" }) on OBJECT\n\
                 input Rec<T> {\n  value: T\n  next: Rec<T> = { value: 1 }\n}\n";
  let analysis = analyze_text(applied, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, applied, "@", "d", 0);
  assert_eq!(found_of(&analysis, &PHASE), [(DEFAULT, path)]);
  let report = the(&analysis, DEFAULT, path);
  assert_eq!(
    report.to_string(),
    "under this application's type arguments, the default `{ value: \"bad\" }` of `a` does not \
     fit the type `a` takes"
  );
  assert_eq!(
    labels(report),
    [(
      after(schema, applied, "Rec<T> = ", "{ value: \"bad\" }"),
      "this default"
    )]
  );
  assert_eq!(argument_shown(analysis.model(), "Query", 0, "a"), "absent");

  let literal = "type Query {\n  x: Int\n}\n\
                 input Rec<T> {\n  value: T\n  next: Rec<T> = { value: \"bad\" }\n}\n\
                 input Wrap<W> {\n  r: Rec<W>\n}\ninput H {\n  h: Wrap<Int> = { r: {} }\n}\n";
  let analysis = analyze_text(literal, &Limits::new());
  let schema = schema_file(&analysis);
  let object = after(schema, literal, "{ r: ", "{}");
  assert_eq!(found_of(&analysis, &PHASE), [(DEFAULT, object)]);
  let report = the(&analysis, DEFAULT, object);
  assert_eq!(
    report.to_string(),
    "this literal leaves out `next`, whose default `{ value: \"bad\" }` does not fit the type \
     `next` takes under these type arguments"
  );
  assert_eq!(
    labels(report),
    [(
      after(schema, literal, "next: Rec<T> = ", "{ value: \"bad\" }"),
      "this default"
    )]
  );
  let model = analysis.model();
  assert_eq!(
    valued(model, input_default(model, "H", "h")),
    "Object {r: Object {}}"
  );
}

/// Many defaulted type parameters and many defaulted values cost steps in their sum. `@d` has `N`
/// type parameters, each `Int` by default, and `M` arguments typed `T0`, each `1` by default, and
/// `Query` applies it bare twice. The first application leaves out `N` slots, a substitution step
/// each, and fills each from its one-node default, a step each: `2N`; the second finds its type in
/// the fill table, for nothing. The first rebuilds `T0` once, a value step, then counts each
/// argument's default under `Int`, its one node a step, and charges the copy's one row: `2M + 1`;
/// the second reads each kept count, a step, and charges the row: `2M`. So at `4M + 1` nothing is
/// said, and at `4M` the second application's last copy is past the budget, at its path; at
/// `2N - 1` substitutions each application's fill is, at its path. `In` has the same parameters
/// and `M` such fields, and `H.y`, a non-null `In!`, leaves every field out: `2M + 1`, as the
/// first application, its fields read under `In!`'s nullable form; then `In!`'s check, a step for
/// each of its `M` values, their counts kept: `3M + 1`, and at `3M` the check is
/// `resource-limit` at `In!`. A test sees steps, not work: the work is linear by construction, a
/// nullable environment used as it is, a non-null one's nullable form found once and remembered,
/// and a remembered rebuild reading none of the environment's arguments. Refuses a charge for
/// each lookup.
#[test]
fn many_defaulted_parameters_and_values_cost_their_sum() {
  const N: u32 = 1000;
  const M: u32 = 1000;
  let params: Vec<String> = (0..N).map(|i: u32| format!("T{i} = Int")).collect();
  let params = params.join(", ");
  let arguments: Vec<String> = (0..M).map(|k: u32| format!("a{k}: T0 = 1")).collect();
  let directive = format!(
    "type Query @d @d {{\n  x: Int\n}}\ndirective @d<{params}>({}) repeatable on OBJECT\n",
    arguments.join(", ")
  );
  quiet(&analyze_text(&directive, &Limits::new()));
  quiet(&analyze_text(
    &directive,
    &Limits::new().with_max_value_steps(4 * M + 1),
  ));
  let short = analyze_text(&directive, &Limits::new().with_max_value_steps(4 * M));
  let schema = schema_file(&short);
  assert_eq!(
    found(&short),
    [(LIMIT, named(schema, &directive, "@", "d", 1))]
  );
  let last = format!("a{}", M - 1);
  assert_eq!(argument_shown(short.model(), "Query", 0, &last), "Int 1");
  assert_eq!(argument_shown(short.model(), "Query", 1, &last), "absent");
  quiet(&analyze_text(
    &directive,
    &Limits::new().with_max_substitutions(2 * N),
  ));
  let unfilled = analyze_text(&directive, &Limits::new().with_max_substitutions(2 * N - 1));
  let schema = schema_file(&unfilled);
  assert_eq!(
    found(&unfilled),
    [
      (LIMIT, named(schema, &directive, "@", "d", 0)),
      (LIMIT, named(schema, &directive, "@", "d", 1)),
    ]
  );

  let fields: String = (0..M).map(|k: u32| format!("  a{k}: T0 = 1\n")).collect();
  let input = format!(
    "type Query {{\n  x: Int\n}}\ninput In<{params}> {{\n{fields}}}\n\
     input H {{\n  y: In! = {{}}\n}}\n"
  );
  quiet(&analyze_text(&input, &Limits::new()));
  quiet(&analyze_text(
    &input,
    &Limits::new().with_max_value_steps(3 * M + 1),
  ));
  let short = analyze_text(&input, &Limits::new().with_max_value_steps(3 * M));
  let schema = schema_file(&short);
  assert_eq!(found(&short), [(LIMIT, at(schema, &input, "In!", 0))]);
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

/// Values under type arguments are numbered where the walk reaches them: `@d<Int>`'s copy of `a`
/// at `k` and `k+1`, its `2` at `k+2`, `x`'s object at `k+3` and its copy of `v` at `k+4` and
/// `k+5`, then `In`'s own default at `k+6`; the walk reaches every value in id order.
#[test]
fn values_under_type_arguments_are_numbered_in_the_walks_order() {
  let text = "directive @d<T>(a: [T] = [1], b: T) on OBJECT\n\
              type Query @d<Int>(b: 2) {\n  x(i: In<Int> = {}): Int\n}\n\
              input In<T> {\n  v: [T] = [3]\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  quiet(&analysis);
  let model = analysis.model();
  let application = application_on(model, "Query", 0);
  let k = argument(model, application, "a").expect("a copy").get();
  assert_eq!(
    argument(model, application, "b").map(|id: ValueId| id.get()),
    Some(k + 2)
  );
  let object = argument_default(model, "Query", "x", "i");
  assert_eq!(object.get(), k + 3);
  let ValueKind::Object(entries) = model.value(object).kind() else {
    panic!("no object");
  };
  assert_eq!(entries[0].value().get(), k + 4);
  assert_eq!(input_default(model, "In", "v").get(), k + 6);
  let mut reached = Values::default();
  let _ = visit(&analysis, Packages::All, &mut reached);
  let ids: Vec<u32> = reached.ids.iter().map(|id: &ValueId| id.get()).collect();
  let expected: Vec<u32> = (0..u32::try_from(ids.len()).expect("few")).collect();
  assert_eq!(ids, expected);
}

/// Whether the type `id` names a type parameter, anywhere in it.
fn names_param(model: &Model, id: TyId) -> bool {
  match model.ty(id).kind() {
    TyKind::Param { .. } => true,
    TyKind::Named { args, .. } => args.iter().any(|arg: &TyId| names_param(model, *arg)),
    TyKind::List { element, .. } => names_param(model, element),
    _ => false,
  }
}

/// An ok analysis under type arguments holds no unresolved value, and keeps a literal only where
/// its type names a type parameter: an application's arguments, written and copied, an object
/// literal's fields and its copies, a nested literal's whose type a rebuild made, a map's value
/// whose type a type parameter's default filled, and the values each instantiation checks.
#[test]
fn an_ok_analysis_under_type_arguments_has_no_unresolved_value() {
  let text = "type Query @d<Int>(a: 1) @d<String>(a: \"s\") {\n  x(i: In<Int> = { v: 2 }): Int\n  \
              g: G<Int>\n}\ndirective @d<T>(a: T, l: [T] = []) repeatable on OBJECT\n\
              input In<T> {\n  v: T\n  w: [T] = [4]\n  b: Box<T> = {}\n}\n\
              input Box<U> {\n  u: U = 5\n}\ninput Wrap<W> {\n  w: Box<W>\n}\n\
              input H {\n  h: Wrap<Int> = { w: {} }\n  m: Dict<Int> = map!{ 1 => {} }\n}\n\
              scalar Dict<K, V = Box<K>> @map\n\
              type G<T> @d<T>(a: 6) {\n  f(y: T = 7): Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let mut reached = Values::default();
  let _ = visit(&analysis, Packages::Supplied, &mut reached);
  assert!(!reached.ids.is_empty());
  for id in reached.ids {
    let value = model.value(id);
    match value.kind() {
      ValueKind::Unresolved => panic!("value {} is unresolved", id.get()),
      ValueKind::Literal(_) => assert!(
        names_param(model, value.ty()),
        "value {} keeps its literal under a closed type",
        id.get()
      ),
      _ => {}
    }
  }
  assert_eq!(
    valued(model, input_default(model, "H", "h")),
    "Object {w: Object {u= Int 5}}"
  );
  let ValueKind::Map(&[(_, value)]) = model.value(input_default(model, "H", "m")).kind() else {
    panic!("`m` is no map of one entry");
  };
  assert_eq!(valued(model, value), "Object {u= Int 5}");
}

/// The analysis of a generic directive applied with a default that does not fit, and of a generic
/// input object instantiated in two files, written out: every value, every application's complete
/// type arguments and every diagnostic.
fn rendered(analysis: &Analysis) -> String {
  let model = analysis.model();
  let (_, _, values, applications) = test_support::counts(model);
  let mut text = String::new();
  for index in 0..values {
    text.push_str(&format!("{index} {}\n", valued(model, ValueId::new(index))));
  }
  for index in 0..applications {
    let arguments = arguments(model, ApplicationId::new(index));
    text.push_str(&format!("@{index} {arguments:?}\n"));
  }
  for diagnostic in analysis.diagnostics() {
    text.push_str(&format!("{diagnostic:?}\n"));
  }
  text
}

/// Permuting a package's files and the dependencies changes no value, no complete type argument
/// and no diagnostic under type arguments.
#[test]
fn reversing_files_and_dependencies_changes_nothing_under_type_arguments() {
  let files = [
    SourceFile::new(
      "a.graphqlx",
      "type Query @^::dep::g<String> {\n  x(i: In<Int>): Int\n}\ninput In<T> {\n  a: T = 1\n}\n",
    ),
    SourceFile::new(
      "b.graphqlx",
      "type Other {\n  y(i: In<String>): Int\n  z(i: In<Int> = {}): Int\n}\n",
    ),
  ];
  let dep = Package::new("dep", Layout::Flat, Tier::Std).with_files([SourceFile::new(
    "d.graphqlx",
    "directive @g<T>(a: T = 1) on OBJECT\n",
  )]);
  let other = Package::new("other", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("o.graphqlx", "type Far {\n  f: Int\n}\n")]);
  let [first, second] = files;
  let forward = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([first.clone(), second.clone()])
    .with_dependencies(["dep", "other"]);
  let backward = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([second, first])
    .with_dependencies(["other", "dep"]);
  let one = analyze(&forward, &[dep.clone(), other.clone()], &Limits::new());
  let two = analyze(&backward, &[other, dep], &Limits::new());
  // Not vacuous: `@g<String>`'s copy and `In<String>` are each reported.
  assert_eq!(
    found(&one)
      .iter()
      .map(|row: &(&str, Location)| row.0)
      .collect::<Vec<&str>>(),
    [DEFAULT, DEFAULT]
  );
  assert_eq!(rendered(&one), rendered(&two));
}

/// A name past what an excerpt quotes is quoted elided, in the copy's report at an application and
/// in an instantiation's check.
#[test]
fn long_spellings_are_quoted_as_excerpts() {
  let long = "a".repeat(4096);
  let text = format!(
    "type Query @d<String> {{\n  x(f: I{long}<String>): Int\n}}\n\
     directive @d<T>({long}: T = 1) on OBJECT\ninput I{long}<T> {{\n  v: T = 2\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let messages: Vec<String> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect();
  assert_eq!(messages.len(), 2, "{messages:#?}");
  for message in messages {
    assert!(message.len() < 1024, "{message}");
    assert!(message.contains('…'), "{message}");
  }
}

/// A count reads an object literal's fields once against each input object it is counted against,
/// each field a step the first time, one the input object does not declare included; a reading
/// kept costs nothing more.
///
/// `@d`'s default writes N = 64 fields, and none of the M = 32 input objects declares one. Each of
/// the first M applications rebuilds `T`, one step; counts the literal's node under its input
/// object, one; reads its 64 fields, 64; and copies the copy's one row, one: 67 each, 2,144 in
/// all, each copy an `invalid-default-value`. The last application writes `@d<I0>` again: it reads
/// the kept count, one, and copies its row, one, so 2,146 pays for everything. At 2,145 the last
/// copy cannot be paid for, and that application alone is `resource-limit`. The test pins the
/// charge: the work of reading the fields is bounded by it by construction, each field paid for
/// before it is looked at. Refuses the fields read for nothing, which passes at 2,145, since the
/// rest costs 3 × 32 + 2 = 98; and a reading made again for a literal and an input object read
/// already, which fails at 2,146.
#[test]
fn a_count_pays_for_each_field_it_reads_against_a_new_input_object() {
  const N: usize = 64;
  const M: usize = 32;
  let fields: Vec<String> = (0..N).map(|k: usize| format!("f{k}: 1")).collect();
  let applied: Vec<String> = (0..M).map(|i: usize| format!("@d<I{i}>")).collect();
  let inputs: String = (0..M)
    .map(|i: usize| format!("input I{i} {{\n  a: Int\n}}\n"))
    .collect();
  let text = format!(
    "type Query {} @d<I0> {{\n  x: Int\n}}\n\
     directive @d<T>(v: T = {{ {} }}) repeatable on OBJECT\n{inputs}",
    applied.join(" "),
    fields.join(", ")
  );
  let total = u32::try_from(M * (N + 3) + 2).expect("a small budget");
  let paid = analyze_text(&text, &Limits::new().with_max_value_steps(total));
  assert!(
    found_of(&paid, &[LIMIT]).is_empty(),
    "{:#?}",
    paid.diagnostics()
  );
  assert_eq!(found_of(&paid, &[DEFAULT]).len(), M + 1);
  let short = analyze_text(&text, &Limits::new().with_max_value_steps(total - 1));
  let schema = schema_file(&short);
  assert_eq!(
    found_of(&short, &[LIMIT]),
    [(LIMIT, named(schema, &text, "@", "d", M))]
  );
}

/// A count under type arguments renders the identities of a set's elements once for each literal
/// and element type it searches, each a step for each 64 bytes or part, paid before it is
/// rendered.
///
/// The default `set!{"…"}` holds one string of 6,400 bytes, and is copied under M = 20 sets of
/// distinct definitions, each of `String`, whose element is one type. The first application
/// rebuilds `T`, one step; counts the set and its element, two; renders the element's identity,
/// 6,400 bytes, 100; and copies two rows, two: 105. Each later one renders nothing, the literal
/// searched already against its element type: 1 + 2 + 2 = 5. In all 105 + 19 × 5 = 200; at 199 the
/// last application's copy cannot be paid for. Refuses a search made again under each collection
/// type, which needs 19 × 100 more.
///
/// Under sets of `String`, `ID` and `bytes`, three element types, each search is made and pays
/// for its rendering: 105 each, 315 in all; at 314 the third application is `resource-limit`.
/// Refuses a rendering paid for nothing, which passes at 314 with 15.
#[test]
fn a_count_pays_for_each_identity_it_renders_once_per_element_type() {
  const M: usize = 20;
  let long = "a".repeat(6400);
  let applied: Vec<String> = (0..M).map(|i: usize| format!("@d<S{i}<String>>")).collect();
  let sets: String = (0..M)
    .map(|i: usize| format!("scalar S{i}<E> @set\n"))
    .collect();
  let text = format!(
    "type Query {} {{\n  x: Int\n}}\n{sets}\
     directive @d<T>(v: T = set!{{\"{long}\"}}) repeatable on OBJECT\n",
    applied.join(" ")
  );
  let total = u32::try_from(105 + (M - 1) * 5).expect("a small budget");
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_value_steps(total),
  ));
  let short = analyze_text(&text, &Limits::new().with_max_value_steps(total - 1));
  let schema = schema_file(&short);
  assert_eq!(
    found_of(&short, &[LIMIT]),
    [(LIMIT, named(schema, &text, "@", "d", M - 1))]
  );

  let text = format!(
    "type Query @d<S<String>> @d<S<ID>> @d<S<bytes>> {{\n  x: Int\n}}\nscalar S<E> @set\n\
     directive @d<T>(v: T = set!{{\"{long}\"}}) repeatable on OBJECT\n"
  );
  quiet(&analyze_text(
    &text,
    &Limits::new().with_max_value_steps(315),
  ));
  let short = analyze_text(&text, &Limits::new().with_max_value_steps(314));
  let schema = schema_file(&short);
  assert_eq!(
    found_of(&short, &[LIMIT]),
    [(LIMIT, named(schema, &text, "@", "d", 2))]
  );
}

/// A count under type arguments pays for each node before it schedules it, so a type that cannot
/// be built, which ends the count, discards no work the count did not pay for.
///
/// `I<T>` has N = 128 nullable `Int` fields and then `tail: [[[T]]]`, and `@d`'s default, of type
/// `I<T>`, sets every one. `@d` is applied under K = 48 distinct `[[[Si]]]`, under a depth of
/// four. Each application rebuilds `I<T>` under its type argument, two steps, a type four deep;
/// counts the literal's node, one; pays for the 129 fields it sets, 129, before it looks at any;
/// and rebuilds `tail`'s `[[[T]]]`, a step for its root and one for each of its three elements,
/// four, stopping where the type would nest five deep: 136. The first also reads the literal's 129
/// fields against `I`, 129 more. So 48 × 136 + 129 = 6,657 pays for every count, and each
/// application is `resource-limit` for the depth; at 6,656 the last application's last step of
/// rebuilding cannot be paid for, and it alone is refused for the budget. The test pins the
/// charge: the work of scheduling the fields is bounded by it by construction, each paid for
/// before it is looked at. Refuses fields scheduled for nothing and dropped unpaid where `tail`
/// cannot be built, with which 48 × 7 + 129 = 465 would pay for every count.
#[test]
fn a_count_pays_for_what_it_schedules_before_a_type_stops_it() {
  const N: usize = 128;
  const K: usize = 48;
  let declared: String = (0..N).map(|k: usize| format!("  f{k}: Int\n")).collect();
  let set: Vec<String> = (0..N).map(|k: usize| format!("f{k}: 1")).collect();
  let applied: Vec<String> = (0..K).map(|i: usize| format!("@d<[[[S{i}]]]>")).collect();
  let scalars: String = (0..K).map(|i: usize| format!("scalar S{i}\n")).collect();
  let text = format!(
    "type Query {} {{\n  x: Int\n}}\ninput I<T> {{\n{declared}  tail: [[[T]]]\n}}\n\
     directive @d<T>(v: I<T> = {{ {}, tail: null }}) repeatable on OBJECT\n{scalars}",
    applied.join(" "),
    set.join(", ")
  );
  let total = u32::try_from(K * (N + 8) + N + 1).expect("a small budget");
  let limits = Limits::new().with_max_generic_depth(4);
  let depth = "nests deeper than `Limits::max_generic_depth`, 4, admits";

  let paid = analyze_text(&text, &limits.with_max_value_steps(total));
  let schema = schema_file(&paid);
  let applications: Vec<(&str, Location)> = (0..K)
    .map(|nth: usize| (LIMIT, named(schema, &text, "@", "d", nth)))
    .collect();
  assert_eq!(found(&paid), applications);
  for diagnostic in paid.diagnostics() {
    assert!(diagnostic.to_string().contains(depth), "{diagnostic}");
  }

  let short = analyze_text(&text, &limits.with_max_value_steps(total - 1));
  assert_eq!(found(&short), applications);
  let messages: Vec<String> = short
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect();
  let (last, before) = messages.split_last().expect("a diagnostic");
  for message in before {
    assert!(message.contains(depth), "{message}");
  }
  assert!(
    last.contains(&format!("`Limits::max_value_steps`, {}", total - 1)),
    "{last}"
  );
}

/// `null` for an argument that is required under an application's open type arguments is refused
/// once, at the argument, for every instantiation: `@d<T>`'s `a` takes `T!` on `G`, required
/// whatever `T` is, and `G<Int>` and `G<String>` report nothing more. Refuses the value recorded
/// and refused again at each instantiation, `1 + k` diagnostics for one mistake.
#[test]
fn null_for_an_argument_required_under_open_arguments_is_refused_once() {
  let text = "type Query {\n  g: G<Int>\n  h: G<String>\n}\ndirective @d<X>(a: X!) on OBJECT\n\
              type G<T> @d<T>(a: null) {\n  f: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let argument = after(schema, text, "@d<T>(", "a");
  assert_eq!(found(&analysis), [(MISSING, argument)]);
  assert_eq!(
    the(&analysis, MISSING, argument).to_string(),
    "the argument `a` of `@d` is required, and `null` is given"
  );
}

/// An argument an application on a generic definition leaves out, whose type there is the
/// definition's bare nullable type parameter, is required wherever an instantiation makes that
/// parameter non-null. `@need<T>` on `Box` leaves out `v`, whose type there is `Box`'s `T`:
/// `Box<Int>` gives it `Int`, and holds; `Box<Int!>` gives it `Int!`, and is
/// `missing-required-directive-argument` at the occurrence, labelled at the application and at
/// `v`. With a default, `v` is required under neither, and its copy fits both. Refuses an omission
/// accepted under the open type and never asked again.
#[test]
fn an_argument_left_out_is_required_where_an_instantiation_makes_it_non_null() {
  let text = "type Query {\n  a: Box<Int>\n  b: Box<Int!>\n}\n\
              directive @need<T>(v: T) on OBJECT\ntype Box<T> @need<T> {\n  value: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Box<Int!>", 0);
  assert_eq!(found(&analysis), [(MISSING, occurrence)]);
  let report = the(&analysis, MISSING, occurrence);
  assert_eq!(
    report.to_string(),
    "`Box` is instantiated here with type arguments under which `@need` requires its argument \
     `v`, and an application it holds leaves it out"
  );
  assert_eq!(
    labels(report),
    [
      (
        named(schema, text, "@", "need", 1),
        "this application leaves it out"
      ),
      (named(schema, text, "(", "v", 0), "required here"),
    ]
  );

  let defaulted = "type Query {\n  a: Box<Int>\n  b: Box<Int!>\n}\n\
                   directive @need<T>(v: T = 1) on OBJECT\n\
                   type Box<T> @need<T> {\n  value: Int\n}\n";
  quiet(&analyze_text(defaulted, &Limits::new()));
}

/// A field an object literal of a generic definition leaves out, whose type there is its input
/// object's bare nullable type parameter, is judged by the same rule: the literal is a value its
/// definition records, and its count at each instantiation reads the field under that
/// instantiation's arguments. `Box`'s `{}` leaves out `In`'s `v`: `Box<Int>` gives it `Int`, and
/// holds; `Box<Int!>` gives it `Int!`, which the literal must set, and is refused at the
/// occurrence with the default's code, labelled at the literal. With a default, `v` is required
/// under neither. Refuses a field left out read off the `!` it is written with.
#[test]
fn a_field_left_out_is_required_where_an_instantiation_makes_it_non_null() {
  let text = "type Query {\n  a(x: Box<Int>): Int\n  b(y: Box<Int!>): Int\n}\n\
              input In<U> {\n  v: U\n}\ninput Box<T> {\n  f: In<T> = {}\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, text, "Box<Int!>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, text, "In<T> = ", "{}"), "this value")]
  );

  let defaulted = "type Query {\n  a(x: Box<Int>): Int\n  b(y: Box<Int!>): Int\n}\n\
                   input In<U> {\n  v: U = 1\n}\ninput Box<T> {\n  f: In<T> = {}\n}\n";
  quiet(&analyze_text(defaulted, &Limits::new()));
}

/// Each pair of a distinct instantiation and an argument left out that its definition records is
/// a step, as each pair of one and a value is. `@need<T>` on `Box` leaves out `v` and `w`: a step
/// each, and one to rebuild `T`, which `w` finds made, three. `Box<Int>` pays a step for each pair
/// and one to rebuild `Box`'s `T`, which the second pair finds made, three; `Box<String>` three
/// more: nine in all. At eight, `Box<String>`'s second pair cannot be paid for, and it alone is
/// `resource-limit`. Refuses the arguments left out checked for nothing, which passes at eight
/// with five, and their check never made, with three.
#[test]
fn every_pair_of_an_instantiation_and_an_argument_left_out_is_a_step() {
  let text = "type Query {\n  a: Box<Int>\n  b: Box<String>\n}\n\
              directive @need<T>(v: T, w: T) on OBJECT\n\
              type Box<T> @need<T> {\n  value: Int\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(9)));
  let short = analyze_text(text, &Limits::new().with_max_value_steps(8));
  let schema = schema_file(&short);
  let string = at(schema, text, "Box<String>", 0);
  assert_eq!(found(&short), [(LIMIT, string)]);
  assert_eq!(
    the(&short, LIMIT, string).to_string(),
    "the values `Box` holds could not be checked under the type arguments it is instantiated with \
     here within `Limits::max_value_steps`, 8, the steps the analysis may take to complete values"
  );
}

/// An `implements` entry is an occurrence like any other, and what its interface records is
/// checked under its type arguments there: `I<String>` refuses `I`'s `1`, with the default's code,
/// and `J<Int!>` makes the `v` that `@need<X>` on `J` leaves out required. Each is reported at its
/// entry. Refuses an entry the check passes over, which only the implementation phase reads.
#[test]
fn what_an_implements_entry_instantiates_is_checked_there() {
  let text = "type Query {\n  a: A\n  b: B\n}\ninterface I<X> {\n  f(a: X = 1): Int\n}\n\
              type A implements I<String> {\n  f(a: String): Int\n}\n\
              directive @need<T>(v: T) on INTERFACE\ninterface J<X> @need<X> {\n  g: Int\n}\n\
              type B implements J<Int!> {\n  g: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let value = at(schema, text, "I<String>", 0);
  let omitted = at(schema, text, "J<Int!>", 0);
  assert_eq!(
    sorted(found_of(&analysis, &PHASE)),
    sorted(vec![(DEFAULT, value), (MISSING, omitted)])
  );
  assert_eq!(
    labels(the(&analysis, DEFAULT, value)),
    [(after(schema, text, "X = ", "1"), "this value")]
  );
  assert_eq!(
    labels(the(&analysis, MISSING, omitted)),
    [
      (
        named(schema, text, "@", "need", 1),
        "this application leaves it out"
      ),
      (named(schema, text, "(", "v", 0), "required here"),
    ]
  );
}

/// The values of the applications on a generic directive's definition, and on its arguments, whose
/// types name its type parameters are checked at each closed application of the directive, as a
/// generic type's are at each closed instantiation: `@outer<Int>` refuses the `"bad"` of the
/// `@inner<T>` it holds, with the argument's code, at its path, labelled at the value, and
/// `@outer<String>` holds. The value stays its definition's literal. Refuses an annotation on a
/// generic directive that no application checks.
#[test]
fn a_generic_directives_annotations_are_checked_at_each_closed_application() {
  let text = "directive @inner<T>(v: T) on DIRECTIVE_DEFINITION\n\
              directive @outer<T> @inner<T>(v: \"bad\") repeatable on OBJECT\n\
              type Query @outer<Int> @outer<String> {\n  value: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "outer", 1);
  assert_eq!(found(&analysis), [(ARGUMENT, path)]);
  let report = the(&analysis, ARGUMENT, path);
  assert_eq!(
    report.to_string(),
    "`@outer` is applied here with type arguments under which `\"bad\"`, a value it holds, does \
     not fit the type it takes"
  );
  assert_eq!(
    labels(report),
    [(at(schema, text, "\"bad\"", 0), "this value")]
  );
  let model = analysis.model();
  let outer = model
    .lookup(Namespace::Directive, "@^::app::outer")
    .expect("`@outer`");
  let inner = model.def(outer).applications()[0];
  assert_eq!(
    valued(model, argument(model, inner, "v").expect("`v`")),
    "Literal(String) \"bad\""
  );

  let argued = "directive @inner<T>(v: T) on ARGUMENT_DEFINITION\n\
                directive @outer<T>(a: Int @inner<T>(v: \"bad\")) repeatable on OBJECT\n\
                type Query @outer<Int> @outer<String> {\n  value: Int\n}\n";
  let analysis = analyze_text(argued, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, argued, "@", "outer", 1);
  assert_eq!(found(&analysis), [(ARGUMENT, path)]);
  assert_eq!(
    labels(the(&analysis, ARGUMENT, path)),
    [(at(schema, argued, "\"bad\"", 0), "this value")]
  );
}

/// An argument an application on a generic directive leaves out, whose type there is the
/// directive's bare nullable type parameter, is required wherever a closed application of the
/// directive makes that parameter non-null: `@outer<Int!>` is
/// `missing-required-directive-argument` at its path, labelled at the `@need<T>` it holds and at
/// `v`, and `@outer<Int>` holds. Refuses an omission in an annotation that no application asks.
#[test]
fn an_argument_an_annotation_leaves_out_is_required_where_an_application_needs_it() {
  let text = "directive @need<T>(v: T) on DIRECTIVE_DEFINITION\n\
              directive @outer<T> @need<T> repeatable on OBJECT\n\
              type Query @outer<Int> @outer<Int!> {\n  value: Int\n}\n";
  let analysis = analyze_text(text, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, text, "@", "outer", 2);
  assert_eq!(found(&analysis), [(MISSING, path)]);
  let report = the(&analysis, MISSING, path);
  assert_eq!(
    report.to_string(),
    "`@outer` is applied here with type arguments under which `@need` requires its argument `v`, \
     and an application it holds leaves it out"
  );
  assert_eq!(
    labels(report),
    [
      (
        named(schema, text, "@", "need", 1),
        "this application leaves it out"
      ),
      (named(schema, text, "(", "v", 0), "required here"),
    ]
  );
}

/// A default an application leaves out, copied under type arguments that still name the enclosing
/// definition's parameters, is a value that definition records, checked at each closed
/// instantiation of it as a written argument is: under `Box<Int>`, and under `@outer<Int>`,
/// `@inner`'s `"bad"` is `invalid-default-value`, labelled at the default, and under `String` it
/// fits. `@inner`'s own default is judged only where an application leaves it out. Refuses a copy
/// under an open application that no instantiation checks.
#[test]
fn a_default_an_annotation_leaves_out_is_checked_as_its_value() {
  let typed = "type Query {\n  a: Box<Int>\n  b: Box<String>\n}\n\
               directive @inner<T>(v: T = \"bad\") on OBJECT\n\
               type Box<T> @inner<T> {\n  value: Int\n}\n";
  let analysis = analyze_text(typed, &Limits::new());
  let schema = schema_file(&analysis);
  let occurrence = at(schema, typed, "Box<Int>", 0);
  assert_eq!(found(&analysis), [(DEFAULT, occurrence)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, occurrence)),
    [(after(schema, typed, "T = ", "\"bad\""), "this value")]
  );

  let applied = "directive @inner<T>(v: T = \"bad\") on DIRECTIVE_DEFINITION\n\
                 directive @outer<T> @inner<T> repeatable on OBJECT\n\
                 type Query @outer<Int> @outer<String> {\n  value: Int\n}\n";
  let analysis = analyze_text(applied, &Limits::new());
  let schema = schema_file(&analysis);
  let path = named(schema, applied, "@", "outer", 1);
  assert_eq!(found(&analysis), [(DEFAULT, path)]);
  assert_eq!(
    labels(the(&analysis, DEFAULT, path)),
    [(after(schema, applied, "T = ", "\"bad\""), "this value")]
  );
}

/// Every closed application of one instantiation of a generic directive shares its verdict, and
/// each pair of one and a value is a step: `@inner<T>`'s `1` costs a step to rebuild its `T` where
/// it is written; the first `@outer<Int>` a step for its pair, one to rebuild `@outer`'s `T` under
/// it and one for the count's one node, three; the second nothing; `@outer<Float>` three: seven in
/// all. At six, `@outer<Float>`'s count cannot be paid for, and it alone is `resource-limit`, at
/// its path. Refuses a verdict made for each application, which needs eight, and a pair paid for
/// nothing, which passes at six with five.
#[test]
fn one_verdict_serves_every_closed_application_of_a_generic_directive() {
  let text = "directive @inner<T>(v: T) on DIRECTIVE_DEFINITION\n\
              directive @outer<T> @inner<T>(v: 1) repeatable on OBJECT\n\
              type Query @outer<Int> @outer<Int> @outer<Float> {\n  value: Int\n}\n";
  quiet(&analyze_text(text, &Limits::new().with_max_value_steps(7)));
  let short = analyze_text(text, &Limits::new().with_max_value_steps(6));
  let schema = schema_file(&short);
  let float = named(schema, text, "@", "outer", 3);
  assert_eq!(found(&short), [(LIMIT, float)]);
  assert_eq!(
    the(&short, LIMIT, float).to_string(),
    "the values `@outer` holds could not be checked under the type arguments it is applied with \
     here within `Limits::max_value_steps`, 6, the steps the analysis may take to complete values"
  );
}

/// A tree's spelling is kept once, however many of its nodes are kept as written and under however
/// many types: each node's text is a run of that one copy. `@d`'s default nests a string in sixteen
/// lists, and `@d` is applied under `S`, `[S]`, and so on to sixteen levels of list, `S` a custom
/// scalar, so each application keeps as written the node at its depth, from the whole default down
/// to the string: seventeen nodes, each holding the string. Lengthening the string by `GROWN`
/// bytes lengthens the model's text arena by exactly `GROWN`, the one copy's growth, where a copy
/// for each node kept would lengthen it by seventeen times as much. Each kept literal still reads
/// back its spelling exactly. The test pins the arena's growth and the spellings; that the texts
/// kept are bounded by the source holds by construction, each tree copied once and each of its
/// nodes' texts a run of the copy.
#[test]
fn a_trees_spelling_is_kept_once_whatever_reads_its_nodes() {
  const LEVELS: usize = 16;
  const GROWN: usize = 4096;
  let program = |long: &str| -> String {
    let applied: Vec<String> = (0..=LEVELS)
      .map(|k: usize| format!("@d<{}S{}>", "[".repeat(k), "]".repeat(k)))
      .collect();
    format!(
      "type Query {} {{\n  x: Int\n}}\nscalar S\n\
       directive @d<T>(v: T = {}\"{long}\"{}) repeatable on OBJECT\n",
      applied.join(" "),
      "[".repeat(LEVELS),
      "]".repeat(LEVELS)
    )
  };
  let short = analyze_text(&program("a"), &Limits::new());
  quiet(&short);
  let long = "a".repeat(GROWN + 1);
  let analysis = analyze_text(&program(&long), &Limits::new());
  quiet(&analysis);
  assert_eq!(
    test_support::text_len(analysis.model()),
    test_support::text_len(short.model()) + GROWN
  );
  let model = analysis.model();
  for k in 0..=LEVELS {
    let mut value = argument(model, application_on(model, "Query", k), "v").expect("a copy");
    for _ in 0..k {
      let ValueKind::List(items) = model.value(value).kind() else {
        panic!("no list at depth {k}");
      };
      value = items[0];
    }
    let ValueKind::Literal(literal) = model.value(value).kind() else {
      panic!("no literal at depth {k}");
    };
    let lists = LEVELS - k;
    assert_eq!(
      literal.text(),
      format!("{}\"{long}\"{}", "[".repeat(lists), "]".repeat(lists)),
      "the node at depth {k}"
    );
  }
}
