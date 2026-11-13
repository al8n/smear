//! The GraphQLx analyzer's types: what lowering checks of a written parameter list, an extension's
//! and a replaced built-in's; the built-in meaning of a definition, a marker or a collection, read
//! off `core`'s directives; and a definition's defaults, checked where they are written.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Collection, DefId, Diagnostic, IdRange, Layout, Limits, MemberId, Model, Namespace,
    Package, PackageId, SourceFile, SourceId, Tier, TyKind, TypeExprKind, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a count of type arguments or type parameters that does not match.
const ARITY: &str = "smear::graphqlx::generic-arity";

/// The code of a type parameter's default that can never fill.
const DEFAULT: &str = "smear::graphqlx::generic-default";

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

/// An analysis of the root package `app`, `Flat`, holding the files `a.graphqlx`, `a`, and
/// `b.graphqlx`, `b`, passed in that order, or reversed when `reversed`.
fn analyze_two(a: &'static str, b: &'static str, reversed: bool) -> Analysis {
  let mut files = vec![("a.graphqlx", a), ("b.graphqlx", b)];
  if reversed {
    files.reverse();
  }
  let root = package("app", Layout::Flat, &files);
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

/// The definition the canonical name `name` names in `namespace`.
fn def(model: &Model, namespace: Namespace, name: &str) -> DefId {
  model
    .lookup(namespace, name)
    .unwrap_or_else(|| panic!("no definition `{name}`"))
}

/// The ids `range` holds, in order.
fn ids(range: IdRange<MemberId>) -> Vec<MemberId> {
  range.into_iter().collect()
}

/// The names of the members `range` holds, in order.
fn member_names(model: &Model, range: IdRange<MemberId>) -> Vec<&str> {
  range
    .into_iter()
    .map(|id: MemberId| model.member(id).name())
    .collect()
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

#[test]
fn an_extension_that_writes_a_list_writes_its_targets_count() {
  // One list names its target's parameters by other names, which is fine; one names one more than
  // `Pair` has, one one fewer, and one names a parameter of `Plain`, which has none.
  let text = "type Pair<A, B> {\n  first: A\n}\n\
              extend type Pair<X, Y> {\n  second: Y\n}\n\
              extend type Pair<P, Q, R> {\n  third: R\n}\n\
              extend type Pair<S> {\n  fourth: S\n}\n\
              type Plain {\n  x: Int\n}\n\
              extend type Plain<T> {\n  y: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let pair = named(schema, text, "type ", "Pair", 0);
  let plain = named(schema, text, "type ", "Plain", 0);
  assert_eq!(
    found(&analysis),
    [
      (ARITY, at(schema, text, "<P, Q, R>", 0), Some(pair)),
      (ARITY, at(schema, text, "<S>", 0), Some(pair)),
      (ARITY, at(schema, text, "<T>", 0), Some(plain)),
    ]
  );
  let message = analysis
    .diagnostics()
    .iter()
    .next()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string());
  assert_eq!(
    message.as_deref(),
    Some(
      "an extension's list names its target's type parameters by position, one name each: `Pair` \
       has 2, and this list names 3"
    )
  );

  // Each extension still contributes what it writes.
  let model = analysis.model();
  let pair = def(model, Namespace::Type, "^::app::Pair");
  assert_eq!(
    member_names(model, model.def(pair).fields()),
    ["first", "second", "third", "fourth"]
  );
  let plain = def(model, Namespace::Type, "^::app::Plain");
  assert_eq!(member_names(model, model.def(plain).fields()), ["x", "y"]);

  // The name past `Pair`'s parameters keeps its position, adds no diagnostic of its own, and its
  // type is the unresolved one.
  let third = model
    .member(ids(model.def(pair).fields())[2])
    .ty()
    .expect("typed");
  assert!(matches!(
    model.type_expr(third).kind(),
    TypeExprKind::Param { index: 2, .. }
  ));
  assert_eq!(
    model.ty(model.type_expr(third).ty()).kind(),
    TyKind::Unresolved
  );
}

#[test]
fn an_extension_with_no_list_names_none_of_its_targets_parameters() {
  // A directive, and a field beside a `T` the module declares: the extension's `T` is the
  // module's scalar, not `Page`'s parameter, whatever `Page`'s count.
  let text = "directive @k on OBJECT\nscalar T\ntype Page<T> {\n  a: T\n}\n\
              extend type Page @k\nextend type Page {\n  b: Int\n  c: T\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let page = def(model, Namespace::Type, "^::app::Page");
  let scalar = def(model, Namespace::Type, "^::app::T");
  let fields = ids(model.def(page).fields());
  assert_eq!(
    member_names(model, model.def(page).fields()),
    ["a", "b", "c"]
  );
  let a = model.member(fields[0]).ty().expect("typed");
  assert!(matches!(
    model.type_expr(a).kind(),
    TypeExprKind::Param { def, index: 0, .. } if def == page
  ));
  let c = model.member(fields[2]).ty().expect("typed");
  assert!(matches!(
    model.type_expr(c).kind(),
    TypeExprKind::Named { def, .. } if def == scalar
  ));

  // With no `T` in the module, the extension's `T` names nothing: name resolution says so, and
  // nothing else does.
  let text = "type Page<T> {\n  a: T\n}\nextend type Page {\n  c: T\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::undefined-type",
      named(schema, text, "c: ", "T", 0),
      None
    )]
  );
}

#[test]
fn a_replaced_built_in_declares_no_type_parameter() {
  // The root package's `String` replaces `string`, whose identity has no parameter: its list is
  // refused, once, and so is nothing of the library's, whose `String`s name the same definition.
  // Its `where` clause is not lowered.
  let text = "scalar String<T> where T: MapKey\ntype Query {\n  name: String\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "<T>", 0), None)]
  );
  let model = analysis.model();
  let string = model.builtins().string();
  assert_eq!(model.def(string).type_params().count(), 0);
  assert!(model.def(string).bounds().is_empty());

  // A replaced directive's argument that names the parameter it lists names a position no
  // parameter has, and its type is the unresolved one.
  let text = "directive @deprecated<R>(reason: R) on FIELD_DEFINITION\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "<R>", 0), None)]
  );
  let model = analysis.model();
  let deprecated = model.builtins().deprecated();
  assert_eq!(model.def(deprecated).type_params().count(), 0);
  let reason = model
    .member(ids(model.def(deprecated).arguments())[0])
    .ty()
    .expect("typed");
  assert!(matches!(
    model.type_expr(reason).kind(),
    TypeExprKind::Param { index: 0, .. }
  ));
  assert_eq!(
    model.ty(model.type_expr(reason).ty()).kind(),
    TyKind::Unresolved
  );
}

/// The names of the model's markers, in id order.
fn markers(model: &Model) -> Vec<&str> {
  model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).is_marker())
    .map(|id: DefId| model.def(id).name())
    .collect()
}

/// The names of the model's collections, each with what it stands for, in id order.
fn collections(model: &Model) -> Vec<(&str, Collection)> {
  model
    .defs()
    .into_iter()
    .filter_map(|id: DefId| {
      let definition = model.def(id);
      definition
        .collection()
        .map(|collection: Collection| (definition.name(), collection))
    })
    .collect()
}

#[test]
fn markers_and_collections_are_read_off_cores_directives() {
  // `core`'s directives, on the library's definitions and on the program's: a marker, and one an
  // extension makes; a set, a map, and a scalar that applies both, which is the first; an
  // interface that applies nothing, and an object that applies `@set`, which neither is.
  let text = "interface Ord @marker\ninterface Node {\n  id: ID\n}\ninterface Plain\n\
              extend interface Plain @marker\n\
              scalar Bag<T> @set\nscalar Dict<K, V> @map\nscalar Both<K, V> @map @set\n\
              type Obj @set {\n  x: Int\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(markers(model), ["MapKey", "Ord", "Plain"]);
  assert_eq!(
    collections(model),
    [
      ("BTreeMap", Collection::Map),
      ("BTreeSet", Collection::Set),
      ("HashMap", Collection::Map),
      ("HashSet", Collection::Set),
      ("Bag", Collection::Set),
      ("Dict", Collection::Map),
      ("Both", Collection::Map),
    ]
  );

  // The program's own directives of those names mean nothing: the meaning is `core`'s
  // definitions', never a spelling's.
  let text = "directive @marker on INTERFACE\ndirective @set on SCALAR\n\
              interface Mine @marker\nscalar Own<T> @set\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(markers(model), ["MapKey"]);
  assert_eq!(
    collections(model),
    [
      ("BTreeMap", Collection::Map),
      ("BTreeSet", Collection::Set),
      ("HashMap", Collection::Map),
      ("HashSet", Collection::Set),
    ]
  );
}

#[test]
fn a_default_after_which_a_parameter_has_none_is_refused() {
  // Arguments bind by position, so `T`'s default could never fill: refused at `U`, which follows it
  // with no default of its own, with a label at the default that cannot fill.
  let text = "scalar P<T = Int, U>\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEFAULT,
      named(schema, text, ", ", "U", 0),
      Some(named(schema, text, "T = ", "Int", 0))
    )]
  );
}

#[test]
fn the_trailing_rule_comes_first() {
  // `T`'s default names `T` itself, but `T` is below the least number of arguments `F` takes, so
  // it is always written: only the trailing rule speaks.
  let text = "scalar F<T = [T], U>\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEFAULT,
      named(schema, text, ", ", "U", 0),
      Some(at(schema, text, "[T]", 0))
    )]
  );
}

/// Definitions whose defaults need each other, none of them used: a default that names its own
/// parameter, and two defaults that name each other.
const GROUPS_A: &str = "scalar One<T = [T]>\nscalar Two<T = U, U = T>\n";

/// More of them: one group of three, which holds two cycles, `X` and `Y`, `Y` and `Z`; and two
/// groups in one definition.
const GROUPS_B: &str = "scalar Pair<X, Y>\nscalar Three<X = Y, Y = Pair<X, Z>, Z = Y>\n\
                        scalar Four<A = A2, A2 = A, B = B2, B2 = B>\n";

#[test]
fn a_group_of_defaults_that_need_each_other_is_refused_once_at_its_lowest_parameter() {
  for reversed in [false, true] {
    let analysis = analyze_two(GROUPS_A, GROUPS_B, reversed);
    let a = file(&analysis, 3, "a.graphqlx");
    let b = file(&analysis, 3, "b.graphqlx");
    // One report a group, at the default of its lowest parameter, labelled where the lowest other
    // member of the group whose default names that parameter first names it; none for a group of
    // one.
    assert_eq!(
      found(&analysis),
      [
        (DEFAULT, at(a, GROUPS_A, "[T]", 0), None),
        (
          DEFAULT,
          named(a, GROUPS_A, "T = ", "U", 0),
          Some(named(a, GROUPS_A, "U = ", "T", 0))
        ),
        (
          DEFAULT,
          named(b, GROUPS_B, "X = ", "Y", 0),
          Some(named(b, GROUPS_B, "Pair<", "X", 1))
        ),
        (
          DEFAULT,
          named(b, GROUPS_B, "A = ", "A2", 0),
          Some(named(b, GROUPS_B, "A2 = ", "A", 0))
        ),
        (
          DEFAULT,
          named(b, GROUPS_B, "B = ", "B2", 0),
          Some(named(b, GROUPS_B, "B2 = ", "B", 0))
        ),
      ],
      "files reversed: {reversed}"
    );
    let messages: Vec<String> = analysis
      .diagnostics()
      .iter()
      .map(|diagnostic: &Diagnostic| diagnostic.to_string())
      .collect();
    assert_eq!(
      messages[0],
      "the default of `T` names `T` itself, so it can never fill"
    );
    assert_eq!(
      messages[1],
      "the default of `T` can never fill: a use that leaves `T` out leaves out the whole group of \
       type parameters whose defaults need each other"
    );
  }
}

#[test]
fn a_default_that_leads_into_a_group_says_nothing() {
  // `A` and `B` need each other; `C`'s default only leads into them, and is reported nowhere.
  let text = "scalar E<A = B, B = A, C = A>\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      DEFAULT,
      named(schema, text, "A = ", "B", 0),
      Some(named(schema, text, "B = ", "A", 0))
    )]
  );
}
