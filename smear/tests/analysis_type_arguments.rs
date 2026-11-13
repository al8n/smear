//! The GraphQLx analyzer's values under type arguments, and what they rest on: a directive
//! application's complete type arguments, typed as an occurrence of its directive would be, its
//! written type arguments first and its directive's defaults filling the rest; its directive's
//! bounds, judged at it by those arguments; its arguments, each read under them, written or a
//! copy of its default, the copy's count judging whether it fits and paying for each node; and a
//! generic input object's fields, read under its type's arguments, a copy that does not fit them
//! reported by the written literal whose type a rebuild made.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, DefId, Diagnostic, Layout, Limits, MemberId, Model, Namespace,
    ObjectEntry, Package, PackageId, SourceFile, SourceId, Tier, TyId, TyKind, ValueId, ValueKind,
    analyze, test_support,
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

/// The complete type arguments of `application`, each written out, through the hidden door.
fn arguments(model: &Model, application: ApplicationId) -> Vec<String> {
  test_support::type_arguments(model, application)
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
      test_support::type_arguments(model, application).is_empty(),
      "application {index} has complete type arguments: {:?}",
      arguments(model, application)
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
const NAMED_HELP: &str = "declare the interface on the type argument with exactly these type arguments, or pass a type \
   that declares it";

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
  assert_eq!(found_of(&analysis, &[DEFAULT, LIMIT]), [(LIMIT, literal)]);
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
