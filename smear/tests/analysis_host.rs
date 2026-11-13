//! The GraphQLx analyzer's host bindings: each scalar's externs, the `@extern`s its own package
//! writes on it, each with its effective tier, written from coerced values.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, ApplicationArgument, ApplicationId, Collection, DefId, Diagnostic, Extern, Layout,
    Limits, Model, Namespace, Package, PackageId, Resolution, SourceFile, SourceId, Tier, ValueId,
    ValueKind, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// GraphQL's code for a directive argument's value that does not fit.
const ARGUMENT: &str = "smear::schema::invalid-directive-argument-value";
/// GraphQL's code for a required directive argument left out or given `null`.
const MISSING: &str = "smear::schema::missing-required-directive-argument";
/// A collection literal out of its place.
const COLLECTION: &str = "smear::graphqlx::collection-literal";
/// The code for `@extern` misused.
const EXTERN_MISUSE: &str = "smear::graphqlx::extern-misuse";
/// The code for an extension of a definition its package may not extend.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";
/// The code for `@repr` misused.
const REPR_MISUSE: &str = "smear::graphqlx::repr-misuse";

/// A package named `name`, `Flat`, at `tier`, holding `files`, `(path, text)`.
fn package<'a>(name: &'a str, tier: Tier, files: &[(&'a str, &'a str)]) -> Package<&'a str> {
  Package::new(name, Layout::Flat, tier).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'a str, &'a str)| SourceFile::new(path, text)),
  )
}

/// An analysis of the root package `app`, `Flat`, at tier `Std`, holding the one file
/// `schema.graphqlx`.
fn analyze_text(text: &str) -> Analysis {
  analyze(
    &package("app", Tier::Std, &[("schema.graphqlx", text)]),
    &[],
    &Limits::new(),
  )
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

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

/// Each diagnostic whose code is one of `codes`: its code and primary location, in the list's
/// order.
fn found_of(analysis: &Analysis, codes: &[&str]) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .filter(|row: &(&'static str, Location)| codes.contains(&row.0))
    .collect()
}

/// Each diagnostic whose code is one of `codes`: its code, its primary location and its first
/// label's location, in the list's order.
fn labelled(
  analysis: &Analysis,
  codes: &[&str],
) -> Vec<(&'static str, Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| codes.contains(&diagnostic.code().as_str()))
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// The type the canonical name `name` names.
fn type_def(model: &Model, name: &str) -> DefId {
  model
    .lookup(Namespace::Type, name)
    .unwrap_or_else(|| panic!("no type `{name}`"))
}

/// The externs of the type the canonical name `name` names, each as its `lang`, its `type` and its
/// tier, in order.
fn externs<'m>(model: &'m Model, name: &str) -> Vec<(&'m str, &'m str, Tier)> {
  model
    .def(type_def(model, name))
    .externs()
    .map(|bound: Extern<'m>| (bound.lang(), bound.r#type(), bound.tier()))
    .collect()
}

/// The arguments of `application`, each as its name and its value: a string quoted, and a type
/// value as the name of the definition it names.
fn shown_arguments(model: &Model, application: ApplicationId) -> Vec<String> {
  model
    .application(application)
    .arguments()
    .iter()
    .map(|argument: &ApplicationArgument| {
      let name = model.member(argument.argument()).name();
      match model.value(argument.value()).kind() {
        ValueKind::String(text) => format!("{name}: {text:?}"),
        ValueKind::Type { def, .. } => format!("{name}: {}", model.def(def).name()),
        other => format!("{name}: {other:?}"),
      }
    })
    .collect()
}

/// The default of the input field `field` of the root package's input object `owner`.
fn input_default(model: &Model, owner: &str, field: &str) -> ValueId {
  let owner = type_def(model, &format!("^::app::{owner}"));
  let field = model
    .def(owner)
    .input_fields()
    .into_iter()
    .find(|id| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no input field `{field}`"));
  model
    .member(field)
    .default()
    .unwrap_or_else(|| panic!("`{field:?}` has no default"))
}

/// `@extern` binds a scalar its own package defines, on its definition or on an extension the
/// package writes in another file: `[Z5]`'s second program. Each binding is listed, in application
/// order, the definition's first. Refuses the definition's applications read alone, and an order
/// read off the files as given.
#[test]
fn a_scalars_externs_are_its_own_packages_applications() {
  let a = "type Query {\n  x: Int\n}\n\
           scalar MediaId @extern(lang: \"rust\", type: \"crate::MediaId\")\n";
  let b = "extend scalar MediaId @extern(lang: \"typescript\", type: \"string\")\n";
  for files in [
    [("a.graphqlx", a), ("b.graphqlx", b)],
    [("b.graphqlx", b), ("a.graphqlx", a)],
  ] {
    let analysis = analyze(&package("app", Tier::Std, &files), &[], &Limits::new());
    assert!(
      analysis.diagnostics().is_empty(),
      "{:#?}",
      analysis.diagnostics()
    );
    assert_eq!(
      externs(analysis.model(), "^::app::MediaId"),
      [
        ("rust", "crate::MediaId", Tier::Std),
        ("typescript", "string", Tier::Std),
      ]
    );
  }
}

/// An extern's `lang` and `type` are its strings cooked, as coercion made them. Refuses a literal's
/// text kept as written.
#[test]
fn an_externs_strings_are_cooked() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\nscalar X @extern(lang: \"r\\u0075st\", type: \"\"\"crate::X\"\"\")\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    externs(analysis.model(), "^::app::X"),
    [("rust", "crate::X", Tier::Std)]
  );
}

/// An extern's tier is the one its `tier` names, or, when it writes none or `null`, the tier its
/// scalar's package is declared at: `geo` is at `alloc`, and the root at `std`. Refuses an absent
/// tier read as `core` or as the root's, and `null` read as a tier.
#[test]
fn an_externs_tier_is_its_own_or_its_packages() {
  let theirs = "scalar A @extern(lang: \"rust\", type: \"a\")\n\
                scalar B @extern(lang: \"rust\", type: \"b\", tier: CORE)\n\
                scalar C @extern(lang: \"rust\", type: \"c\", tier: null)\n\
                scalar D @extern(lang: \"rust\", type: \"d\", tier: Tier::STD)\n";
  let root = package(
    "app",
    Tier::Std,
    &[("schema.graphqlx", "type Query {\n  x: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Tier::Alloc, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let model = analysis.model();
  assert_eq!(externs(model, "^::geo::A"), [("rust", "a", Tier::Alloc)]);
  assert_eq!(externs(model, "^::geo::B"), [("rust", "b", Tier::Core)]);
  assert_eq!(externs(model, "^::geo::C"), [("rust", "c", Tier::Alloc)]);
  assert_eq!(externs(model, "^::geo::D"), [("rust", "d", Tier::Std)]);
}

/// An `@extern` another package writes binds nothing, and neither does one on a built-in, a
/// GraphQL built-in the root package replaces among them, nor a package's own directive named
/// `extern`. Refuses a binding read off every application, and a replaced built-in bound.
#[test]
fn what_another_package_writes_binds_nothing() {
  // The root package's extension of a dependency's scalar, beside the dependency's own `@extern`.
  let theirs = "scalar Point @extern(lang: \"rust\", type: \"geo::Point\")\n";
  let ours = "type Query {\n  x: Int\n}\n\
              extend scalar ^::geo::Point @extern(lang: \"typescript\", type: \"P\")\n";
  let root = package("app", Tier::Std, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Tier::Std, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert_eq!(
    externs(analysis.model(), "^::geo::Point"),
    [("rust", "geo::Point", Tier::Std)]
  );

  // The root package's extension of a built-in, and its own `String`, which replaces one.
  for text in [
    "type Query {\n  x: Int\n}\nextend scalar string @extern(lang: \"rust\", type: \"my::Str\")\n",
    "type Query {\n  x: Int\n}\nscalar String @extern(lang: \"rust\", type: \"my::Str\")\n",
  ] {
    let analysis = analyze_text(text);
    assert!(
      externs(analysis.model(), "^::alloc::string").is_empty(),
      "{text}"
    );
  }

  // A dependency's extension of a built-in.
  let root = package(
    "app",
    Tier::Std,
    &[("schema.graphqlx", "type Query {\n  x: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Tier::Std,
    &[(
      "geo.graphqlx",
      "extend scalar string @extern(lang: \"rust\", type: \"geo::Str\")\n",
    )],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(externs(analysis.model(), "^::alloc::string").is_empty());

  // A directive of the package's own named `extern` is not `core`'s.
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\ndirective @extern(lang: String!, type: String!) repeatable on \
     SCALAR\nscalar M @extern(lang: \"rust\", type: \"m\")\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(externs(analysis.model(), "^::app::M").is_empty());
}

/// An `@extern` whose `lang` is no string, that leaves `lang` out, or whose `tier` names no value of
/// `Tier`, binds nothing, and coercion's diagnostic says why. Refuses an extern bound with an empty
/// text or a guessed tier.
#[test]
fn an_extern_whose_values_did_not_coerce_binds_nothing() {
  let text = "type Query {\n  x: Int\n}\nscalar A @extern(lang: 1, type: \"x\")\n\
              scalar B @extern(type: \"x\")\n\
              scalar C @extern(lang: \"x\", type: \"y\", tier: LOW)\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found_of(&analysis, &[ARGUMENT, MISSING]),
    [
      (ARGUMENT, at(schema, text, "1", 0)),
      (MISSING, at(schema, text, "extern", 1)),
      (ARGUMENT, at(schema, text, "LOW", 0)),
    ]
  );
  let model = analysis.model();
  for name in ["A", "B", "C"] {
    assert!(
      externs(model, &format!("^::app::{name}")).is_empty(),
      "{name}"
    );
  }
}

/// An extern collection binds as any scalar does, and its values are its collection's: its own
/// literal, and no other (`[Z19]`). Refuses an extern collection's value kept as a custom scalar's,
/// and its binding lost to its meaning.
#[test]
fn an_extern_collection_binds_and_takes_its_own_literal() {
  let text = "type Query {\n  x: Int\n}\n\
              scalar IndexSet<T> @set @extern(lang: \"rust\", type: \"indexmap::IndexSet\")\n\
              input H {\n  s: IndexSet<Int> = set!{1}\n  l: IndexSet<Int> = [1]\n}\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    found_of(&analysis, &[COLLECTION]),
    [(COLLECTION, at(schema, text, "[1]", 0))]
  );
  let model = analysis.model();
  assert_eq!(
    externs(model, "^::app::IndexSet"),
    [("rust", "indexmap::IndexSet", Tier::Std)]
  );
  assert_eq!(
    model.def(type_def(model, "^::app::IndexSet")).collection(),
    Some(Collection::Set)
  );
  let ValueKind::Set(elements) = model.value(input_default(model, "H", "s")).kind() else {
    panic!("`H.s` is no set");
  };
  let ValueKind::Int(element) = model.value(elements[0]).kind() else {
    panic!("`H.s` holds no integer");
  };
  assert_eq!((elements.len(), element.text()), (1, "1"));
}

/// A `lang` written twice is bound twice, in order: the model holds what is written, and the rule on
/// `@extern`'s use refuses the later. Refuses a later binding dropped.
#[test]
fn a_lang_written_twice_is_bound_twice() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\n\
     scalar M @extern(lang: \"rust\", type: \"a\") @extern(lang: \"rust\", type: \"b\")\n",
  );
  assert_eq!(
    externs(analysis.model(), "^::app::M"),
    [("rust", "a", Tier::Std), ("rust", "b", Tier::Std)]
  );
}

/// A language binds a scalar once: the later of two externs of one `lang` is `extern-misuse`, at
/// its `@extern`'s path, with a label at the earlier one's, in one definition and across the
/// definition and the package's own extension in another file. The `lang`s are compared as they
/// are cooked: `"rust"` is `"rust"`, and `"Rust"` is another. Refuses spellings compared, and
/// a duplicate across contributions missed.
#[test]
fn a_lang_bound_twice_is_refused_at_the_later() {
  let text = "type Query {\n  x: Int\n}\n\
              scalar M @extern(lang: \"rust\", type: \"a\") @extern(lang: \"rust\", type: \"b\")\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[EXTERN_MISUSE]),
    [(
      EXTERN_MISUSE,
      at(schema, text, "extern", 1),
      Some(at(schema, text, "extern", 0))
    )]
  );

  let a = "type Query {\n  x: Int\n}\nscalar M @extern(lang: \"rust\", type: \"a\")\n";
  let b = "extend scalar M @extern(lang: \"rust\", type: \"b\")\n";
  let analysis = analyze(
    &package("app", Tier::Std, &[("a.graphqlx", a), ("b.graphqlx", b)]),
    &[],
    &Limits::new(),
  );
  let (a_file, b_file) = (
    file(&analysis, 3, "a.graphqlx"),
    file(&analysis, 3, "b.graphqlx"),
  );
  assert_eq!(
    labelled(&analysis, &[EXTERN_MISUSE]),
    [(
      EXTERN_MISUSE,
      at(b_file, b, "extern", 0),
      Some(at(a_file, a, "extern", 0))
    )]
  );

  let text = "type Query {\n  x: Int\n}\n\
              scalar M @extern(lang: \"rust\", type: \"a\") @extern(lang: \"r\\u0075st\", type: \"b\")\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[EXTERN_MISUSE]),
    [(
      EXTERN_MISUSE,
      at(schema, text, "extern", 1),
      Some(at(schema, text, "extern", 0))
    )]
  );
  assert_eq!(
    analysis
      .diagnostics()
      .iter()
      .next()
      .map(Diagnostic::to_string),
    Some(String::from(
      "the scalar `M` is bound to a host type of `rust` twice, and a language binds a scalar once"
    ))
  );

  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\n\
     scalar M @extern(lang: \"rust\", type: \"a\") @extern(lang: \"Rust\", type: \"b\")\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Sixty-five externs, all of distinct `lang`s but the last, which is the third's: one
/// diagnostic, at the last, labelled at the third, found by the kernel's sort past its scan's
/// sixty-four. Refuses a search that stops at sixty-four.
#[test]
fn sixty_five_externs_find_their_duplicate() {
  let mut externs: Vec<String> = (0..64)
    .map(|k: usize| format!("@extern(lang: \"l{k}\", type: \"t\")"))
    .collect();
  externs.push(String::from("@extern(lang: \"l2\", type: \"t\")"));
  let text = format!(
    "type Query {{\n  x: Int\n}}\nscalar M {}\n",
    externs.join(" ")
  );
  let analysis = analyze_text(&text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[EXTERN_MISUSE]),
    [(
      EXTERN_MISUSE,
      at(schema, &text, "extern", 64),
      Some(at(schema, &text, "extern", 2))
    )]
  );
}

/// `@extern` on a GraphQL built-in the root package's own definition replaces is `extern-misuse`,
/// at its path, in the definition and in the root package's own extension of it, and no orphan:
/// `[Z5]`'s programs 3 and 4. Its help names `@repr(for: …)`. Refuses an `@extern` on a replaced
/// built-in bound, or left to the orphan rule.
#[test]
fn extern_on_a_replaced_built_in_is_refused() {
  for (text, nth) in [
    (
      "type Query {\n  x: Int\n}\nscalar String @extern(lang: \"rust\", type: \"my::Str\")\n",
      0,
    ),
    (
      "type Query {\n  x: Int\n}\nscalar Int @extern(lang: \"rust\", type: \"my::Int\")\n",
      0,
    ),
    (
      "type Query {\n  x: Int\n}\nscalar String\n\
       extend scalar String @extern(lang: \"rust\", type: \"my::Str\")\n",
      0,
    ),
  ] {
    let analysis = analyze_text(text);
    let schema = file(&analysis, 3, "schema.graphqlx");
    assert_eq!(
      labelled(&analysis, &[EXTERN_MISUSE, ORPHAN]),
      [(EXTERN_MISUSE, at(schema, text, "extern", nth), None)],
      "{text}"
    );
    let help = analysis
      .diagnostics()
      .iter()
      .find_map(|diagnostic: &Diagnostic| diagnostic.help());
    assert!(
      help.is_some_and(|help: &str| help.contains("@repr(for: …")),
      "{text}: {help:?}"
    );
  }
}

/// A package binding a scalar of its own, on its definition and on its own extension in another
/// file, says nothing: `[Z5]`'s second program.
#[test]
fn a_package_binding_its_own_scalar_says_nothing() {
  let a = "type Query {\n  x: Int\n}\n\
           scalar MediaId @extern(lang: \"rust\", type: \"crate::MediaId\")\n";
  let b = "extend scalar MediaId @extern(lang: \"typescript\", type: \"string\")\n";
  let analysis = analyze(
    &package("app", Tier::Std, &[("a.graphqlx", a), ("b.graphqlx", b)]),
    &[],
    &Limits::new(),
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Two `rust` externs that bind, beside an `@extern` whose `lang` did not coerce in the same
/// contribution: coercion's diagnostic alone, the rule silent within what a phase refused. Refuses
/// a rule that ignores the contribution's errors.
#[test]
fn extern_misuse_says_nothing_where_a_phase_spoke() {
  let text = "type Query {\n  x: Int\n}\nscalar M @extern(lang: \"rust\", type: \"a\") \
              @extern(lang: \"rust\", type: \"b\") @extern(lang: 1, type: \"c\")\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[EXTERN_MISUSE, ARGUMENT]),
    [(ARGUMENT, at(schema, text, "1", 0), None)]
  );
  assert_eq!(
    externs(analysis.model(), "^::app::M"),
    [("rust", "a", Tier::Std), ("rust", "b", Tier::Std)]
  );
}

/// On the schema, `@repr` names with `for` the type it chooses for: one whose `for` is absent or
/// `null` is `repr-misuse`, at its path; one that names a type says nothing, in a schema definition
/// and in the root package's schema extension. Refuses `for` left unchecked, and `null` taken for a
/// type.
#[test]
fn repr_on_the_schema_names_its_type_with_for() {
  let text = "schema @repr(lang: \"rust\", type: \"x\") @repr(for: null, lang: \"rust\", type: \"y\") \
              {\n  query: Query\n}\ntype Query {\n  x: Int\n}\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[REPR_MISUSE]),
    [
      (REPR_MISUSE, at(schema, text, "repr", 0), None),
      (REPR_MISUSE, at(schema, text, "repr", 1), None),
    ]
  );

  let analysis = analyze_text(
    "schema @repr(for: bytes, lang: \"rust\", type: \"smol_bytes::Bytes\") {\n  query: Query\n}\n\
     type Query {\n  x: Int\n}\nextend schema @repr(for: string, lang: \"rust\", type: \"String\")\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

/// `for` is written on the schema alone: an `@repr` that writes it on a field, a field's argument,
/// a directive definition's argument or an input field, `null` included, is `repr-misuse`, at
/// `for`'s name. Refuses a check of the schema alone.
#[test]
fn for_is_written_on_the_schema_alone() {
  let text = "type Query {\n  a: Int @repr(lang: \"rust\", type: \"i32\", for: Int)\n  \
              b(x: Int @repr(lang: \"rust\", type: \"i32\", for: Int)): Int\n  \
              c: Int @repr(lang: \"rust\", type: \"i32\", for: null)\n}\n\
              directive @d(y: Int @repr(lang: \"rust\", type: \"i32\", for: Int)) on OBJECT\n\
              input I {\n  z: Int @repr(lang: \"rust\", type: \"i32\", for: Int)\n}\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  let expected: Vec<(&str, Location, Option<Location>)> = (0..5)
    .map(|nth: usize| (REPR_MISUSE, at(schema, text, "for", nth), None))
    .collect();
  assert_eq!(labelled(&analysis, &[REPR_MISUSE]), expected);
}

/// A position chooses its host type once in each language: the later of two `@repr`s of one
/// `lang` on one field is `repr-misuse`, at its path, labelled at the earlier's. A field and its
/// argument are two positions, and `rust` and `typescript` two languages. Refuses a field and its
/// argument taken for one position.
#[test]
fn a_positions_lang_is_written_once() {
  let text = "type Query {\n  \
              a: Int @repr(lang: \"rust\", type: \"i32\") @repr(lang: \"rust\", type: \"i64\")\n  \
              b(x: Int @repr(lang: \"rust\", type: \"i32\")): Int @repr(lang: \"rust\", type: \"i32\")\n  \
              c: Int @repr(lang: \"rust\", type: \"i32\") @repr(lang: \"typescript\", type: \"number\")\n}\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[REPR_MISUSE]),
    [(
      REPR_MISUSE,
      at(schema, text, "repr", 1),
      Some(at(schema, text, "repr", 0))
    )]
  );
}

/// The schema chooses once for each type and language: the later of two `@repr`s of one `for` and
/// one `lang`, across the schema definition and the root package's extension, is `repr-misuse`,
/// at its path, labelled at the earlier's; `String` and `string` name one type. A pair that
/// differs in either says nothing. Refuses a pair told by its spelling, and `lang` alone made
/// unique on the schema.
#[test]
fn the_schemas_for_and_lang_are_written_once() {
  let text = "schema @repr(for: bytes, lang: \"rust\", type: \"a\") {\n  query: Query\n}\n\
              type Query {\n  x: Int\n}\nextend schema @repr(for: bytes, lang: \"rust\", type: \"b\")\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[REPR_MISUSE]),
    [(
      REPR_MISUSE,
      at(schema, text, "repr", 1),
      Some(at(schema, text, "repr", 0))
    )]
  );

  let analysis = analyze_text(
    "schema @repr(for: bytes, lang: \"rust\", type: \"a\") @repr(for: string, lang: \"rust\", \
     type: \"b\") @repr(for: bytes, lang: \"typescript\", type: \"c\") {\n  query: Query\n}\n\
     type Query {\n  x: Int\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  let text = "schema @repr(for: String, lang: \"rust\", type: \"a\") @repr(for: string, lang: \
              \"rust\", type: \"b\") {\n  query: Query\n}\ntype Query {\n  x: Int\n}\n";
  let analysis = analyze_text(text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[REPR_MISUSE]),
    [(
      REPR_MISUSE,
      at(schema, text, "repr", 1),
      Some(at(schema, text, "repr", 0))
    )]
  );
}

/// `@repr` is read off its applications, found by `Builtins::repr`: a field's answers its typed
/// `lang` and `type`, and the schema's its `for` as the type value of the definition it names.
/// Refuses a model that lost what the applications hold.
#[test]
fn repr_is_read_off_its_applications() {
  let analysis = analyze_text(
    "schema @repr(for: bytes, lang: \"rust\", type: \"smol_bytes::Bytes\") {\n  query: Query\n}\n\
     type Query {\n  x: Int @repr(lang: \"rust\", type: \"i64\")\n}\n",
  );
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let repr = model.builtins().repr();
  let field = model
    .def(type_def(model, "^::app::Query"))
    .fields()
    .into_iter()
    .next()
    .expect("the field `x`");
  let on_field = model.member(field).applications()[0];
  assert_eq!(
    model.application(on_field).directive(),
    Resolution::Resolved(repr)
  );
  assert_eq!(
    shown_arguments(model, on_field),
    ["lang: \"rust\"", "type: \"i64\""]
  );
  let on_schema = model.schema().applications()[0];
  assert_eq!(
    model.application(on_schema).directive(),
    Resolution::Resolved(repr)
  );
  assert_eq!(
    shown_arguments(model, on_schema),
    [
      "lang: \"rust\"",
      "type: \"smol_bytes::Bytes\"",
      "for: bytes"
    ]
  );
}

/// Where `@repr` may stand is not this rule's: on an object type and on an enum value it says
/// nothing.
#[test]
fn repr_where_it_may_not_stand_is_not_this_rules() {
  let analysis = analyze_text(
    "type Query {\n  x: Int\n}\ntype Q @repr(lang: \"rust\", type: \"x\") {\n  y: Int\n}\n\
     enum E {\n  A @repr(lang: \"rust\", type: \"x\")\n}\n",
  );
  assert!(labelled(&analysis, &[REPR_MISUSE]).is_empty());
}

/// Sixty-five `@repr`s on one field, of distinct `lang`s but the last, which is the third's: one
/// diagnostic, at the last, labelled at the third. Refuses a search that stops at sixty-four.
#[test]
fn sixty_five_reprs_find_their_duplicate() {
  let mut reprs: Vec<String> = (0..64)
    .map(|k: usize| format!("@repr(lang: \"l{k}\", type: \"t\")"))
    .collect();
  reprs.push(String::from("@repr(lang: \"l2\", type: \"t\")"));
  let text = format!("type Query {{\n  x: Int {}\n}}\n", reprs.join(" "));
  let analysis = analyze_text(&text);
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    labelled(&analysis, &[REPR_MISUSE]),
    [(
      REPR_MISUSE,
      at(schema, &text, "repr", 64),
      Some(at(schema, &text, "repr", 2))
    )]
  );
}
