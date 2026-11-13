//! The GraphQLx analyzer's values under type arguments, and what they rest on: a directive
//! application's complete type arguments, typed as an occurrence of its directive would be, its
//! written type arguments first and its directive's defaults filling the rest; and its directive's
//! bounds, judged at it by those arguments.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationId, DefId, Diagnostic, Layout, Limits, Model, Namespace, Package,
    PackageId, SourceFile, SourceId, Tier, TyId, TyKind, analyze, test_support,
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
