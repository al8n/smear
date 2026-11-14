//! The GraphQLx analyzer's types: what lowering checks of a written parameter list, an extension's
//! and a replaced built-in's; the built-in meaning of a definition, a marker or a collection, read
//! off `core`'s directives; a definition's defaults, checked where they are written; and every
//! type occurrence's type: one identity a type, interned in the walk's order, every argument left
//! out filled from its default, aliases expanded, counts checked, collections' keys non-null, each
//! written closed instantiation listed, and the three limits that bound what filling derives; and
//! smear's rule that an enum takes no type parameter, beside the uses of such an enum.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Collection, Cx, DefId, Diagnostic, IdRange, Layout, Limits, MemberId, Model,
    Namespace, Package, PackageId, Packages, Root, RootOperation, SourceFile, SourceId, Sym, Tier,
    TyId, TyKind, TypeExprId, TypeExprKind, Visitor, analyze, visit,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a count of type arguments or type parameters that does not match.
const ARITY: &str = "smear::graphqlx::generic-arity";

/// The code of a type parameter's default that can never fill.
const DEFAULT: &str = "smear::graphqlx::generic-default";

/// The code of a bound of the analysis's limits that is passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

/// The code of a type name that names nothing.
const UNDEFINED: &str = "smear::schema::undefined-type";

/// The code of an extension its package may not write, which the orphan rule reports.
const ORPHAN: &str = "smear::graphqlx::orphan-extension";

/// The code of an enum that declares a type parameter, which a rule reports.
const GENERIC_ENUM: &str = "smear::graphqlx::generic-enum";

/// The code of a collection whose count of type parameters it has not, which a rule reports.
const COLLECTION: &str = "smear::graphqlx::collection-arity";

/// GraphQL's code for a directive applied where its definition does not list.
const LOCATION: &str = "smear::schema::unsupported-directive-location";

/// GraphQL's code for a schema with no query root. The programs of this file are parts of a
/// program and say nothing of the schema, so each reading below leaves this code out, as a
/// consumer that analyzes a library waives it.
const NO_QUERY_ROOT: &str = "smear::schema::missing-query-root-operation-type";

/// The diagnostics of `analysis`, in the list's order, but for the missing query root.
fn reported(analysis: &Analysis) -> Vec<&Diagnostic> {
  analysis
    .diagnostics()
    .iter()
    .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str() != NO_QUERY_ROOT)
    .collect()
}

/// Whether `analysis` reports nothing but that its program has no query root.
fn clean(analysis: &Analysis) -> bool {
  let diagnostics = analysis.diagnostics();
  let waived = diagnostics.len() - reported(analysis).len();
  diagnostics.len() == waived && diagnostics.error_count() == waived
}

/// What the rule says of an enum named `name` that declares a type parameter.
fn generic_enum(name: &str) -> String {
  format!(
    "the enum `{name}` declares a type parameter, and an enum takes none: its values are names, \
     which hold no type"
  )
}

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

/// Each diagnostic's code, primary location, and first label's location, in the list's order, but
/// for the missing query root.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// Each diagnostic's message, in the list's order, but for the missing query root.
fn messages(analysis: &Analysis) -> Vec<String> {
  reported(analysis)
    .into_iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`, whose
/// text is `text`, within `limits`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
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

/// The occurrence that is the type of the field `field` of the definition the canonical name
/// `owner` names.
fn field_occurrence(model: &Model, owner: &str, field: &str) -> TypeExprId {
  let owner_def = def(model, Namespace::Type, owner);
  let member = model
    .def(owner_def)
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{field}` in `{owner}`"));
  model.member(member).ty().expect("a field has a type")
}

/// The type of the field `field` of the definition the canonical name `owner` names.
fn field_type(model: &Model, owner: &str, field: &str) -> TyId {
  model.type_expr(field_occurrence(model, owner, field)).ty()
}

/// The type of that field, written out.
fn field_shown(model: &Model, owner: &str, field: &str) -> String {
  shown(model, field_type(model, owner, field))
}

/// Every type occurrence a walk reaches, in its order.
#[derive(Default)]
struct Occurrences {
  types: Vec<TypeExprId>,
}

impl Visitor for Occurrences {
  fn type_expr(&mut self, _cx: &mut Cx<'_>, ty: TypeExprId) {
    self.types.push(ty);
  }
}

/// Every type occurrence of `analysis`, of every package, in the walk's order.
fn every_occurrence(analysis: &Analysis) -> Vec<TypeExprId> {
  let mut walked = Occurrences::default();
  let _ = visit(analysis, Packages::All, &mut walked);
  walked.types
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
  let message = reported(&analysis)
    .into_iter()
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

  // A directive's extension that writes a list of another count, labelled at the directive's
  // name, which the message quotes with its `@`, as every message quotes one.
  let text = "directive @x on DIRECTIVE_DEFINITION\ndirective @tag<T> on OBJECT\n\
              extend directive @tag<A, B> @x\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let tag = model
    .def(def(model, Namespace::Directive, "@^::app::tag"))
    .location();
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "<A, B>", 0), Some(tag))]
  );
  assert_eq!(
    messages(&analysis),
    [
      "an extension's list names its target's type parameters by position, one name each: `@tag` \
       has 1, and this list names 2"
    ]
  );
}

#[test]
fn an_extension_with_no_list_names_none_of_its_targets_parameters() {
  // A directive, and a field beside a `T` the module declares: the extension's `T` is the
  // module's scalar, not `Page`'s parameter, whatever `Page`'s count.
  let text = "directive @k on OBJECT\nscalar T\ntype Page<T> {\n  a: T\n}\n\
              extend type Page @k\nextend type Page {\n  b: Int\n  c: T\n}\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
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
  assert_eq!(
    messages(&analysis),
    ["`String` replaces a GraphQL built-in and keeps its identity, which has no type parameter"]
  );
  let model = analysis.model();
  let string = model.builtins().string();
  assert_eq!(model.def(string).type_params().count(), 0);
  assert!(model.def(string).bounds().is_empty());

  // A replaced directive's argument that names the parameter it lists names a position no
  // parameter has, and its type is the unresolved one. The message writes the directive with its
  // `@`.
  let text = "directive @deprecated<R>(reason: R) on FIELD_DEFINITION\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "<R>", 0), None)]
  );
  assert_eq!(
    messages(&analysis),
    [
      "`@deprecated` replaces a GraphQL built-in and keeps its identity, which has no type \
       parameter"
    ]
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
  // extension of the definition's own package makes; a set, a map, and a scalar that applies both,
  // which is the first, and which the rule on a collection's type parameters refuses, at the later;
  // an interface that applies nothing, and an object that applies `@set`, which neither is, and
  // where `@set`, which stands on a scalar alone, is refused by the rule on directive locations.
  let text = "interface Ord @marker\ninterface Node {\n  id: ID\n}\ninterface Plain\n\
              extend interface Plain @marker\n\
              scalar Bag<T> @set\nscalar Dict<K, V> @map\nscalar Both<K, V> @map @set\n\
              type Obj @set {\n  x: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  assert_eq!(
    found(&analysis),
    [
      (
        COLLECTION,
        named(schema, text, "@", "set", 1),
        Some(named(schema, text, "scalar ", "Both", 0))
      ),
      (
        LOCATION,
        named(schema, text, "@", "set", 2),
        Some(model.def(model.builtins().set()).location())
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
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
  // definitions', never a spelling's. So `Mine` is no marker, and an interface that holds no field
  // is GraphQL's own refusal, which says so.
  let text = "directive @marker on INTERFACE\ndirective @set on SCALAR\n\
              interface Mine @marker\nscalar Own<T> @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::empty-fields-definition",
      named(schema, text, "interface ", "Mine", 0),
      None
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(messages(&analysis), ["the interface `Mine` holds no field"]);
  assert_eq!(
    reported(&analysis)
      .into_iter()
      .next()
      .and_then(|diagnostic: &Diagnostic| diagnostic.help()),
    Some(
      "an object or interface type must define at least one field. An interface that says what a \
       type is and holds nothing is a marker, and takes `@marker`."
    )
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

  // A definition's meaning is its own package's to say. The root package's `@set` on `core`'s
  // `NonZero` makes it no set, and the orphan rule refuses it, once, at the directive: the alias
  // and the form it spells are one type, whose argument keeps its nullness.
  let text = "extend scalar NonZero @set\ntype Query {\n  a: NonZeroU32\n  b: NonZero<u32>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let non_zero = model.builtins().non_zero();
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      named(schema, text, "@", "set", 0),
      Some(model.def(non_zero).location())
    )]
  );
  assert_eq!(model.def(non_zero).collection(), None);
  let a = field_type(model, "^::app::Query", "a");
  assert_eq!(field_type(model, "^::app::Query", "b"), a);
  assert_eq!(shown(model, a), "NonZero<u32>");

  // The root package's `@set` on a dependency's scalar: the orphan rule refuses the whole
  // extension, as it refuses any of another package's definition, and the scalar is no set, in the
  // dependency's own fields too.
  let ours = "extend scalar ^::geo::Bag @set\n";
  let theirs = "scalar Bag<T>\ntype G {\n  g: Bag<Int>\n}\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", ours)]).with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = schema_file(&analysis);
  let geo_file = file(&analysis, 4, "geo.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      ORPHAN,
      at(schema, ours, "^::geo::Bag", 0),
      Some(named(geo_file, theirs, "scalar ", "Bag", 0))
    )]
  );
  let model = analysis.model();
  let bag = def(model, Namespace::Type, "^::geo::Bag");
  assert_eq!(model.def(bag).collection(), None);
  assert_eq!(field_shown(model, "^::geo::G", "g"), "Bag<i32>");

  // A definition's own package gives its meaning from any of its files or modules: the test
  // compares packages, never sources or modules. Two files of a `Flat` root package, in each order.
  for reversed in [false, true] {
    let analysis = analyze_two(
      "scalar Bag<T>\ninterface Plain\n",
      "extend scalar Bag @set\nextend interface Plain @marker\n",
      reversed,
    );
    assert!(
      clean(&analysis),
      "files reversed: {reversed}: {:#?}",
      analysis.diagnostics()
    );
    let model = analysis.model();
    let bag = def(model, Namespace::Type, "^::app::Bag");
    assert_eq!(model.def(bag).collection(), Some(Collection::Set));
    assert!(
      model
        .def(def(model, Namespace::Type, "^::app::Plain"))
        .is_marker()
    );
  }
  // Two modules of a `Directory` root package.
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", "scalar Bag<T>\n"),
      ("inner/x.graphqlx", "extend scalar ~::Bag @set\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let bag = def(model, Namespace::Type, "^::app::Bag");
  assert_eq!(model.def(bag).collection(), Some(Collection::Set));

  // A GraphQL built-in the root package replaces is the root package's own: its extension there
  // gives it its meaning, and is no orphan. A set of no type parameter is the rule on a
  // collection's type parameters' to refuse, at the `@set`.
  let text = "scalar String\nextend scalar String @set\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      COLLECTION,
      named(schema, text, "@", "set", 0),
      Some(named(schema, text, "scalar ", "String", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    model.def(model.builtins().string()).collection(),
    Some(Collection::Set)
  );
}

#[test]
fn a_default_after_which_a_parameter_has_none_is_refused() {
  // Arguments bind by position, so `T`'s default could never fill: refused at `U`, which follows it
  // with no default of its own, with a label at the default that cannot fill. A use that writes
  // both is built, and one that writes one is too few.
  let text = "scalar P<T = Int, U>\ntype Query {\n  both: P<Int, Int>\n  one: P<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (
        DEFAULT,
        named(schema, text, ", ", "U", 0),
        Some(named(schema, text, "T = ", "Int", 0))
      ),
      (
        ARITY,
        at(schema, text, "P<Int>", 0),
        Some(named(schema, text, "scalar ", "P", 0))
      ),
    ]
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "both"), "P<i32, i32>");
  assert_eq!(field_shown(model, "^::app::Query", "one"), "?");
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
    let messages: Vec<String> = reported(&analysis)
      .into_iter()
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
  // `A` and `B` need each other; `C`'s default only leads into them, and is reported nowhere: a use
  // that writes `A` and `B` fills it.
  let text = "scalar E<A = B, B = A, C = A>\ntype Query {\n  e: E<Int, Int>\n}\n";
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
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "e"),
    "E<i32, i32, i32>"
  );
}

#[test]
fn one_type_has_one_id_however_it_is_spelled() {
  // `Int` and `i32` name one definition; `[Int!]!` written twice, and in another file, is one type.
  let a = "type A {\n  x: Int\n  y: [Int!]!\n}\n";
  let b = "type B {\n  x: i32\n  y: [Int!]!\n  z: [i32!]!\n}\n";
  let analysis = analyze_two(a, b, false);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let int = field_type(model, "^::app::A", "x");
  assert_eq!(field_type(model, "^::app::B", "x"), int);
  let list = field_type(model, "^::app::A", "y");
  assert_eq!(field_type(model, "^::app::B", "y"), list);
  assert_eq!(field_type(model, "^::app::B", "z"), list);
  assert_ne!(int, list);
  assert_eq!(shown(model, list), "[i32!]!");
}

#[test]
fn a_parameter_is_its_definitions() {
  // The `T` of `Box` and the `T` of `Other` have one name and one index, and are two types; `T`
  // and `T!` are two types too.
  let text = "type Box<T> {\n  a: T\n  b: T!\n  c: T\n}\ntype Other<T> {\n  a: T\n}\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let boxed = field_type(model, "^::app::Box", "a");
  assert_ne!(field_type(model, "^::app::Other", "a"), boxed);
  assert_ne!(field_type(model, "^::app::Box", "b"), boxed);
  assert_eq!(field_type(model, "^::app::Box", "c"), boxed);
  assert_eq!(shown(model, boxed), "Box#0");
  assert_eq!(field_shown(model, "^::app::Box", "b"), "Box#0!");
}

#[test]
fn types_are_interned_arguments_first_in_the_walks_order() {
  let text = "scalar A<X, Y>\nscalar B<Z>\nscalar C\nscalar D\n\
              type T {\n  f: A<B<C>, D>\n  g: Int\n}\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let f = field_type(model, "^::app::T", "f");
  let TyKind::Named { args, .. } = model.ty(f).kind() else {
    panic!("`A<B<C>, D>` is a named type");
  };
  let (b, d) = (args[0], args[1]);
  let TyKind::Named { args, .. } = model.ty(b).kind() else {
    panic!("`B<C>` is a named type");
  };
  let c = args[0];
  // `C`, then `B<C>`, then `D`, then `A<…>`: each argument before what holds it, left to right;
  // and every one of them before `i32`, first written after them.
  let int = field_type(model, "^::app::T", "g");
  assert!(
    c < b && b < d && d < f && f < int,
    "{c:?} {b:?} {d:?} {f:?} {int:?}"
  );
}

#[test]
fn a_default_is_interned_where_the_walk_reaches_it() {
  // The use, written before `Page`, needs `[User]` for `C`; `Page`'s own default, `[T]`, is
  // interned when the walk reaches it, after the use's type.
  let text = "type Query {\n  page: Page<User>\n}\ntype User {\n  id: ID\n}\n\
              type Page<T, C = [T]> {\n  items: C\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let page = field_type(model, "^::app::Query", "page");
  assert_eq!(shown(model, page), "Page<User, [User]>");
  let page_def = def(model, Namespace::Type, "^::app::Page");
  let cursor = model
    .def(page_def)
    .type_params()
    .nth(1)
    .expect("`Page` has a second type parameter");
  let default = cursor.default().expect("`C` has a default");
  let own = model.type_expr(default).ty();
  assert_eq!(shown(model, own), "[Page#0]");
  assert!(page < own, "{page:?} {own:?}");
}

#[test]
fn filled_arguments_are_interned_in_index_order() {
  // Written before `P`, the use finds none of these types interned. `A`'s default needs `C`'s,
  // which is filled there and then; then `B`'s.
  let text = "type Query {\n  a: P\n}\nscalar Box1<X>\nscalar Box2<X>\nscalar Box3<X>\n\
              scalar P<A = Box1<C>, B = Box2<Int>, C = Box3<Int>>\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let a = field_type(model, "^::app::Query", "a");
  assert_eq!(shown(model, a), "P<Box1<Box3<i32>>, Box2<i32>, Box3<i32>>");
  let TyKind::Named { args, .. } = model.ty(a).kind() else {
    panic!("`P` is a named type");
  };
  let (box1, box2, box3) = (args[0], args[1], args[2]);
  assert!(box3 < box1 && box1 < box2, "{box3:?} {box1:?} {box2:?}");
}

#[test]
fn a_default_fills_what_is_left_out_substituted() {
  let text = "type Query {\n  short: Page<User>\n  long: Page<User, Cursor<User>>\n}\n\
              type User {\n  id: ID\n}\nscalar Cursor<X>\n\
              type Page<T, C = Cursor<T>> {\n  items: [T]\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let short = field_type(model, "^::app::Query", "short");
  assert_eq!(field_type(model, "^::app::Query", "long"), short);
  assert_eq!(shown(model, short), "Page<User, Cursor<User>>");
  // The occurrence still writes the one argument written.
  let written = field_occurrence(model, "^::app::Query", "short");
  let TypeExprKind::Named { args, .. } = model.type_expr(written).kind() else {
    panic!("`Page<User>` names `Page`");
  };
  assert_eq!(args.len(), 1);
}

#[test]
fn a_default_may_name_an_earlier_filled_parameter() {
  let text = "scalar Pair<A, B>\ntype P<T, U = [T], V = Pair<T, U>> {\n  a: T\n}\n\
              type Query {\n  p: P<Int>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "p"),
    "P<i32, [i32], Pair<i32, [i32]>>"
  );
}

#[test]
fn a_forward_default_is_filled_before_the_parameter_that_names_it() {
  let text = "type Query {\n  a: P\n  b: P<String>\n}\ntype P<T = [U], U = Int> {\n  x: T\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "a"), "P<[i32], i32>");
  assert_eq!(field_shown(model, "^::app::Query", "b"), "P<string, i32>");
}

#[test]
fn a_forward_default_inside_another_definitions_generic() {
  // `U`, written inside `Q`'s argument, reads `P`'s slots; `Q`'s own default reads `Q`'s.
  let text = "scalar Q<X, Y = [X]>\ntype P<T = Q<U>, U = Int> {\n  x: T\n}\n\
              type Query {\n  a: P\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "a"),
    "P<Q<i32, [i32]>, i32>"
  );
}

/// `S`'s defaults: `A` needs `C`, which needs `B`, whose `S<Int, Int>` fills its own `C` from its
/// own written `B`: no cycle, with the use written before `S` or after it.
const FILL_BEFORE: &str = "type Query {\n  s: S\n}\n\
                           type S<A = C, B = S<Int, Int>, C = B> {\n  a: A\n}\n";

/// The same, `S` written first.
const FILL_AFTER: &str = "type S<A = C, B = S<Int, Int>, C = B> {\n  a: A\n}\n\
                          type Query {\n  s: S\n}\n";

#[test]
fn a_fill_in_progress_is_told_by_its_written_count() {
  for text in [FILL_BEFORE, FILL_AFTER] {
    let analysis = analyze_one(text);
    assert!(
      analysis.diagnostics().is_empty(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert_eq!(
      field_shown(analysis.model(), "^::app::Query", "s"),
      "S<S<i32, i32, i32>, S<i32, i32, i32>, S<i32, i32, i32>>",
      "{text}"
    );
  }
}

#[test]
fn one_default_filled_twice_in_one_fill() {
  // `Q`'s `Y` is filled twice while `P`'s `U` is: a fill that ends leaves the set.
  let text = "scalar Q<X, Y = [X]>\nscalar Pair<A, B>\ntype P<T, U = Pair<Q<T>, Q<[T]>>> {\n  \
              a: T\n}\ntype Query {\n  p: P<Int>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "p"),
    "P<i32, Pair<Q<i32, [i32]>, Q<[i32], [[i32]]>>>"
  );
}

#[test]
fn a_failed_fill_leaves_the_set() {
  // `a`'s fill of `Y` nests a fourth level and fails at the limit; `b`'s fill of the same slot is
  // no cycle.
  let text = "scalar Q<X, Y = [[X]]>\ntype Query {\n  a: Q<[[Int]]>\n  b: Q<Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "Q<[[Int]]>", 0), None)]
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "a"), "?");
  assert_eq!(field_shown(model, "^::app::Query", "b"), "Q<i32, [[i32]]>");
}

/// `Page`, and a type of a hundred fields of one instantiation of it that leaves `C` out.
fn hundred_pages() -> String {
  let mut text = String::from(
    "type User {\n  id: ID\n}\ntype Page<T, C = [T]> {\n  items: C\n}\ntype Query {\n",
  );
  for index in 0..100 {
    text.push_str(&format!("  f{index}: Page<User>\n"));
  }
  text.push_str("}\n");
  text
}

#[test]
fn a_hundred_uses_cost_one_fill() {
  // One fill of `C` costs three steps: one for the slot, and one for each of the default's two
  // nodes, `[T]` and `T`. The ninety-nine other uses are answered by the fill table.
  let text = hundred_pages();
  let analysis = analyze_text(&text, &Limits::new().with_max_substitutions(3));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let first = field_type(model, "^::app::Query", "f0");
  assert_eq!(shown(model, first), "Page<User, [User]>");
  for index in 1..100 {
    assert_eq!(
      field_type(model, "^::app::Query", &format!("f{index}")),
      first
    );
  }
}

#[test]
fn a_failed_fill_is_remembered() {
  // `A`'s and `B`'s defaults need each other through two definitions. One attempt to fill
  // `A<i32>` costs seven steps: three misses of one slot each, and two defaults of two nodes each;
  // each default's own tree costs seven more. Twenty-one in all, with the table; without it, each
  // of the hundred uses would cost seven again.
  let mut text = String::from("scalar A<T, U = B<T>>\nscalar B<T, V = A<T>>\ntype Query {\n");
  for index in 0..100 {
    text.push_str(&format!("  f{index}: A<Int>\n"));
  }
  text.push_str("}\n");
  let analysis = analyze_text(&text, &Limits::new().with_max_substitutions(64));
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
    .collect();
  assert_eq!(
    codes.iter().filter(|code: &&&str| **code == ARITY).count(),
    100
  );
  assert_eq!(
    codes
      .iter()
      .filter(|code: &&&str| **code == DEFAULT)
      .count(),
    2
  );
  assert_eq!(codes.len(), 102, "{:#?}", analysis.diagnostics());
  let schema = schema_file(&analysis);
  let defaults: Vec<Location> = found(&analysis)
    .into_iter()
    .filter(|(code, _, _): &(&str, Location, Option<Location>)| *code == DEFAULT)
    .map(|(_, at, _): (&str, Location, Option<Location>)| at)
    .collect();
  assert_eq!(
    defaults,
    [at(schema, &text, "B<T>", 0), at(schema, &text, "A<T>", 0)]
  );
}

/// Definitions whose own defaults need each other, beside a type that uses them: one use fills, one
/// cannot, for each.
const REFUSED: &str = "scalar Two<T = U, U = T>\nscalar Pair<X, Y>\n\
                       scalar Three<X = Y, Y = Pair<X, Z>, Z = Y>\n\
                       type Query {\n  a: Two<Int>\n  b: Two\n  c: Three<Int, Int>\n  \
                       d: Three<Int>\n}\n";

#[test]
fn a_use_of_a_refused_definition_is_filled_when_it_can_be() {
  let analysis = analyze_one(REFUSED);
  let schema = schema_file(&analysis);
  // The two definitions' groups, each once; then the two uses whose left-out suffix holds a
  // cycle, each labelled at the default its order was filling, which can never fill: `Two`'s
  // `T`, and `Three`'s `Y`, neither `X`'s nor `Z`'s.
  assert_eq!(
    found(&analysis),
    [
      (
        DEFAULT,
        named(schema, REFUSED, "T = ", "U", 0),
        Some(named(schema, REFUSED, "U = ", "T", 0))
      ),
      (
        DEFAULT,
        named(schema, REFUSED, "X = ", "Y", 0),
        Some(named(schema, REFUSED, "Pair<", "X", 1))
      ),
      (
        ARITY,
        named(schema, REFUSED, "b: ", "Two", 0),
        Some(named(schema, REFUSED, "T = ", "U", 0))
      ),
      (
        ARITY,
        at(schema, REFUSED, "Three<Int>", 0),
        Some(at(schema, REFUSED, "Pair<X, Z>", 0))
      ),
    ]
  );
  // The uses that leave out no cycle are filled, and say nothing.
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "a"), "Two<i32, i32>");
  assert_eq!(
    field_shown(model, "^::app::Query", "c"),
    "Three<i32, i32, i32>"
  );
}

/// A default that needs itself through another occurrence; two that need each other through two
/// definitions; one that leads into those two; and a type whose fields leave the arguments out.
const THROUGH: &str = "scalar A<T, U = A<[T]>>\nscalar B<T = C>\nscalar C<T = B>\nscalar D<T = B>\n\
                       type Query {\n  a: A<Int>\n  b: B\n}\n";

/// The same definitions, written in the other order.
const THROUGH_BACKWARD: &str = "scalar D<T = B>\nscalar C<T = B>\nscalar B<T = C>\n\
                                scalar A<T, U = A<[T]>>\ntype Query {\n  a: A<Int>\n  b: B\n}\n";

#[test]
fn a_default_that_needs_itself_through_another_occurrence_is_refused_and_never_followed() {
  for text in [THROUGH, THROUGH_BACKWARD] {
    let analysis = analyze_one(text);
    let schema = schema_file(&analysis);
    let found = found(&analysis);
    let expected = [
      (DEFAULT, at(schema, text, "A<[T]>", 0), None),
      (DEFAULT, named(schema, text, "B<T = ", "C", 0), None),
      (DEFAULT, named(schema, text, "C<T = ", "B", 0), None),
      (DEFAULT, named(schema, text, "D<T = ", "B", 0), None),
      (
        ARITY,
        at(schema, text, "A<Int>", 0),
        Some(at(schema, text, "A<[T]>", 0)),
      ),
      (
        ARITY,
        named(schema, text, "b: ", "B", 0),
        Some(named(schema, text, "B<T = ", "C", 0)),
      ),
    ];
    assert_eq!(found.len(), expected.len(), "{text}: {found:#?}");
    for one in expected {
      assert!(
        found.contains(&one),
        "{text}: {one:?} is missing from {found:#?}"
      );
    }
  }
}

/// `G` and `H`, written after the type that uses `H`, so that the `H<Int>` that `H`'s `T` writes is
/// not built yet when the use fills `T`.
const MEETING_USED_FIRST: &str = "type Query {\n  h: H\n}\n\
                           type G<T = [U], U = G<Int>> {\n  x: T\n}\n\
                           type H<T = H<Int>, U = T> {\n  x: T\n}\n";

/// The same, written before the type that uses `H`.
const MEETING_USED_LAST: &str = "type G<T = [U], U = G<Int>> {\n  x: T\n}\n\
                          type H<T = H<Int>, U = T> {\n  x: T\n}\n\
                          type Query {\n  h: H\n}\n";

#[test]
fn a_forward_default_meets_a_cycle_through_another_occurrence() {
  // `G`'s `U` leaves its own `U` out again: a cycle through another occurrence, at that
  // occurrence. `H`'s `T` writes `H<Int>`, whose `U` is filled from its own written `T`. With the
  // use written first, that `H<Int>` is filled while the use's own fill of `T` is in progress: a
  // set keyed by the definition alone would call it a cycle, and slots shared between the two
  // occurrences would read the use's empty `T`.
  for text in [MEETING_USED_FIRST, MEETING_USED_LAST] {
    let analysis = analyze_one(text);
    let schema = schema_file(&analysis);
    assert_eq!(
      found(&analysis),
      [(DEFAULT, at(schema, text, "G<Int>", 0), None)],
      "{text}"
    );
    assert_eq!(
      field_shown(analysis.model(), "^::app::Query", "h"),
      "H<H<i32, i32>, H<i32, i32>>",
      "{text}"
    );
  }
}

#[test]
fn an_alias_is_the_instantiation_it_spells() {
  let text = "type Query {\n  a: NonZeroU32\n  b: NonZero<u32>\n  c: NonZeroU32!\n  \
              d: NonZero<u32>!\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let a = field_type(model, "^::app::Query", "a");
  let c = field_type(model, "^::app::Query", "c");
  assert_eq!(field_type(model, "^::app::Query", "b"), a);
  assert_eq!(field_type(model, "^::app::Query", "d"), c);
  assert_ne!(a, c);
  assert_eq!(shown(model, a), "NonZero<u32>");
  // The alias keeps its spelling, and writes no argument.
  let alias = model.type_expr(field_occurrence(model, "^::app::Query", "a"));
  let spelled: Vec<&str> = alias
    .path()
    .expect("a path")
    .segments()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect();
  assert_eq!(spelled, ["NonZeroU32"]);
  let TypeExprKind::Named { args, .. } = alias.kind() else {
    panic!("`NonZeroU32` names `NonZero`");
  };
  assert!(args.is_empty());
}

#[test]
fn a_renamed_non_zero_is_no_alias() {
  // `NonZeroU32` is here a second name of `NonZero` itself, which takes one argument: written with
  // none it is too few, and written with one it is that instantiation.
  let text = "use ^::core::NonZero as NonZeroU32\ntype Query {\n  a: NonZeroU32\n  \
              b: NonZeroU32<u8>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let non_zero = model.def(model.builtins().non_zero()).location();
  assert_eq!(
    found(&analysis),
    [(
      ARITY,
      named(schema, text, "a: ", "NonZeroU32", 0),
      Some(non_zero)
    )]
  );
  assert_eq!(field_shown(model, "^::app::Query", "b"), "NonZero<u8>");
}

#[test]
fn an_alias_under_another_name_is_still_its_instantiation() {
  // A `use` item binds the alias itself under another name: what lowering notes is the alias's
  // primitive, whatever name the occurrence spells.
  let text = "use NonZeroU32 as Nz\ntype Query {\n  a: Nz\n  b: NonZero<u32>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    field_type(model, "^::app::Query", "a"),
    field_type(model, "^::app::Query", "b")
  );
}

#[test]
fn an_alias_takes_no_argument() {
  let text = "type Query {\n  a: NonZeroU32<u8>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "NonZeroU32<u8>", 0), None)]
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "a"), "?");
  // The argument is an occurrence of its own, with its own type.
  let written = field_occurrence(model, "^::app::Query", "a");
  let TypeExprKind::Named { args, .. } = model.type_expr(written).kind() else {
    panic!("`NonZeroU32` names `NonZero`");
  };
  assert_eq!(shown(model, model.type_expr(args[0]).ty()), "u8");
}

/// The external review's program: the root package extends `core`'s `NonZero` with `@set`, then
/// writes the alias and the form it spells, in each order.
const NON_ZERO_SET: [&str; 2] = [
  "extend scalar NonZero @set\ntype Query {\n  a: NonZeroU32\n  b: NonZero<u32>\n}\n",
  "extend scalar NonZero @set\ntype Query {\n  b: NonZero<u32>\n  a: NonZeroU32\n}\n",
];

#[test]
fn an_alias_and_the_form_it_spells_are_one_type_when_non_zero_is_extended() {
  // The root package's `@set` gives `core`'s `NonZero` no meaning, so the argument keeps its
  // nullness, and the orphan rule refuses the directive; the alias and the form it spells are one
  // type and one instantiation: with room for one instantiation, both are built. No program can
  // make `NonZero` a collection, so the alias's slot, made what a written argument is, is never
  // made non-null: that normalisation is a guard this test no longer reaches.
  for text in NON_ZERO_SET {
    for limits in [Limits::new(), Limits::new().with_max_instantiations(1)] {
      let analysis = analyze_text(text, &limits);
      let schema = schema_file(&analysis);
      let model = analysis.model();
      let non_zero = model.builtins().non_zero();
      assert_eq!(
        found(&analysis),
        [(
          ORPHAN,
          named(schema, text, "@", "set", 0),
          Some(model.def(non_zero).location())
        )],
        "{text}"
      );
      assert_eq!(model.def(non_zero).collection(), None, "{text}");
      let a = field_type(model, "^::app::Query", "a");
      assert_eq!(field_type(model, "^::app::Query", "b"), a, "{text}");
      assert_eq!(shown(model, a), "NonZero<u32>", "{text}");
    }
  }
}

#[test]
fn too_many_and_too_few_arguments_are_refused_where_they_are_written() {
  let text = "type Page<T, C = Int> {\n  a: T\n}\n\
              type Query {\n  many: Page<Int, Int, Int>\n  few: Page\n  fine: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let page = named(schema, text, "type ", "Page", 0);
  assert_eq!(
    found(&analysis),
    [
      (
        ARITY,
        at(schema, text, "Page<Int, Int, Int>", 0),
        Some(page)
      ),
      (ARITY, named(schema, text, "few: ", "Page", 0), Some(page)),
    ]
  );
  assert_eq!(
    messages(&analysis),
    [
      "`Page` takes at most 2 type arguments, and 3 are written",
      "`Page` takes at least 1 type argument, and 0 are written",
    ]
  );
  // Each refused occurrence is unresolved and marks its member and the contribution; its sibling
  // is typed and marked by nothing.
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "many"), "?");
  assert_eq!(field_shown(model, "^::app::Query", "few"), "?");
  assert_eq!(field_shown(model, "^::app::Query", "fine"), "i32");
  let query = def(model, Namespace::Type, "^::app::Query");
  let fields = ids(model.def(query).fields());
  assert!(model.member(fields[0]).has_errors());
  assert!(model.member(fields[1]).has_errors());
  assert!(!model.member(fields[2]).has_errors());
  let own = model
    .def(query)
    .contributions()
    .next()
    .expect("`Query` has its own contribution");
  assert!(own.has_errors());
}

#[test]
fn a_count_says_what_the_definition_takes() {
  // A count says the side of it that failed: a definition of no type parameter; one that takes
  // every type argument it has, a directive among them; and two that take fewer than they have,
  // written with too many and with too few. `R`'s `B` and `C` need each other, so `R<Int>` cannot
  // be filled either, and `R` takes at least one type argument, not one to three.
  let text = "scalar Plain\nscalar Pair<A, B>\nscalar Page<T, C = Int>\n\
              scalar R<A, B = C, C = B>\ndirective @tag<T> on OBJECT\n\
              type Query @tag<Int, Int> {\n  a: Plain<Int>\n  b: Pair<Int>\n  \
              c: Page<Int, Int, Int>\n  r: R\n}\n";
  let analysis = analyze_one(text);
  assert_eq!(
    messages(&analysis),
    [
      "the default of `B` can never fill: a use that leaves `B` out leaves out the whole group of \
       type parameters whose defaults need each other",
      "`@tag` takes 1 type argument, and 2 are written",
      "`Plain` takes no type argument, and 1 is written",
      "`Pair` takes 2 type arguments, and 1 is written",
      "`Page` takes at most 2 type arguments, and 3 are written",
      "`R` takes at least 1 type argument, and 0 are written",
    ]
  );
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
    .collect();
  assert_eq!(codes, [DEFAULT, ARITY, ARITY, ARITY, ARITY, ARITY]);
  // What takes no type argument is told to remove them; every other count, to write one for each
  // type parameter.
  let count = "write a type argument for each type parameter, leaving out only trailing defaulted \
               ones";
  let helps: Vec<Option<&str>> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.help())
    .collect();
  assert_eq!(
    helps,
    [
      Some(
        "give this type parameter no default, and none to the type parameters written before it"
      ),
      Some(count),
      Some("remove the type arguments"),
      Some(count),
      Some(count),
      Some(count),
    ]
  );
}

#[test]
fn a_parameter_takes_no_argument() {
  let text = "type Box<T> {\n  a: T<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(ARITY, at(schema, text, "T<Int>", 0), None)]
  );
  assert_eq!(
    messages(&analysis),
    ["a type parameter takes no type argument"]
  );
  assert_eq!(field_shown(analysis.model(), "^::app::Box", "a"), "?");
}

#[test]
fn a_directives_type_arguments_are_counted() {
  // Too many, too few, and one left to its default.
  let text = "directive @tag<T, U = Int>(value: Int) on OBJECT | FIELD_DEFINITION\n\
              type Query @tag<Int, Int, Int>(value: 1) {\n  a: Int @tag(value: 2)\n  \
              b: Int @tag<Int>(value: 3)\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let tag = model
    .def(def(model, Namespace::Directive, "@^::app::tag"))
    .location();
  assert_eq!(
    found(&analysis),
    [
      (ARITY, named(schema, text, "@", "tag", 1), Some(tag)),
      (ARITY, named(schema, text, "@", "tag", 2), Some(tag)),
    ]
  );
  assert_eq!(
    messages(&analysis),
    [
      "`@tag` takes at most 2 type arguments, and 3 are written",
      "`@tag` takes at least 1 type argument, and 0 are written",
    ]
  );
}

#[test]
fn an_occurrence_that_holds_an_unresolved_one_is_unresolved_and_says_nothing() {
  let text = "type Page<T> {\n  a: T\n}\n\
              type Query {\n  a: Page<Missing>\n  b: [Missing]\n  c: Page<Missing, Int, Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, at(schema, text, "Missing", 0), None),
      (UNDEFINED, at(schema, text, "Missing", 1), None),
      (
        ARITY,
        at(schema, text, "Page<Missing, Int, Int>", 0),
        Some(named(schema, text, "type ", "Page", 0))
      ),
      (UNDEFINED, at(schema, text, "Missing", 2), None),
    ]
  );
  let model = analysis.model();
  for field in ["a", "b", "c"] {
    assert_eq!(field_shown(model, "^::app::Query", field), "?", "{field}");
  }
}

#[test]
fn an_instantiation_is_listed_as_each_written_closed_type() {
  // Both nullnesses of `Page<User>` are written, the non-null one twice; `Page<T>` inside `Page`
  // names a parameter; `Box<i32>` is only ever a default's substituted; and an alias is
  // `NonZero`'s. A union member, a root and a default's own closed tree are written too.
  let text = "type User {\n  id: ID\n}\ntype Page<T, C = [T]> {\n  items: C\n  inner: Page<T>\n}\n\
              scalar Box<X>\nscalar Holder<T, B = Box<T>>\n\
              type Query {\n  a: Page<User>\n  b: Page<User>!\n  c: [Page<User>!]\n  \
              d: Holder<Int>\n  e: NonZeroU32\n}\n\
              type Wrap<T> {\n  v: T\n}\nunion Either = Wrap<User>\n\
              scalar Tag<X>\nscalar Labelled<T, L = Tag<Int>>\n\
              type Root<T> {\n  item: T\n}\nschema {\n  query: Root<Int>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let a = field_type(model, "^::app::Query", "a");
  let b = field_type(model, "^::app::Query", "b");
  let page = def(model, Namespace::Type, "^::app::Page");
  assert_eq!(model.instantiations(page), [a, b]);
  // Every written closed occurrence finds its own type in its definition's list.
  let c = field_type(model, "^::app::Query", "c");
  let TyKind::List { element, .. } = model.ty(c).kind() else {
    panic!("`[Page<User>!]` is a list");
  };
  assert!(model.instantiations(page).binary_search(&element).is_ok());
  let holder = def(model, Namespace::Type, "^::app::Holder");
  let d = field_type(model, "^::app::Query", "d");
  assert_eq!(model.instantiations(holder), [d]);
  assert_eq!(shown(model, d), "Holder<i32, Box<i32>>");
  let boxed = def(model, Namespace::Type, "^::app::Box");
  assert!(model.instantiations(boxed).is_empty());
  let e = field_type(model, "^::app::Query", "e");
  assert_eq!(model.instantiations(model.builtins().non_zero()), [e]);

  // A union member's type, a root's, and a default's own closed tree's are each listed.
  let wrap = def(model, Namespace::Type, "^::app::Wrap");
  let either = def(model, Namespace::Type, "^::app::Either");
  let member = model.def(either).union_members()[0];
  assert_eq!(model.instantiations(wrap), [model.type_expr(member).ty()]);
  assert_eq!(shown(model, model.type_expr(member).ty()), "Wrap<User>");
  let root_def = def(model, Namespace::Type, "^::app::Root");
  let Some(Root::Written(root)) = model.schema().root(RootOperation::Query) else {
    panic!("the query root is written");
  };
  assert_eq!(model.instantiations(root_def), [model.type_expr(root).ty()]);
  let tag = def(model, Namespace::Type, "^::app::Tag");
  let labelled = def(model, Namespace::Type, "^::app::Labelled");
  let label = model
    .def(labelled)
    .type_params()
    .nth(1)
    .expect("`Labelled` has a second type parameter");
  let default = label.default().expect("`L` has a default");
  assert_eq!(model.instantiations(tag), [model.type_expr(default).ty()]);
  assert_eq!(shown(model, model.type_expr(default).ty()), "Tag<i32>");
  assert!(model.instantiations(labelled).is_empty());
}

#[test]
fn polymorphic_recursion_is_accepted_and_adds_nothing() {
  // `Nest<i32>` would need `Nest<[i32]>` and on without end: nothing follows a type into its
  // members, and only what is written is listed.
  let text = "type Nest<T> {\n  value: T\n  deeper: Nest<[T]>\n}\n\
              type Query {\n  nest: Nest<i32>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let nest = def(model, Namespace::Type, "^::app::Nest");
  assert_eq!(
    model.instantiations(nest),
    [field_type(model, "^::app::Query", "nest")]
  );
}

#[test]
fn a_root_may_be_an_instantiation() {
  let text = "type Root<T> {\n  item: T\n}\nschema {\n  query: Root<i32>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let Some(Root::Written(root)) = model.schema().root(RootOperation::Query) else {
    panic!("the query root is written");
  };
  assert_eq!(shown(model, model.type_expr(root).ty()), "Root<i32>");

  let text = "type Root<T> {\n  item: T\n}\nschema {\n  query: Root<i32, i32>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      ARITY,
      at(schema, text, "Root<i32, i32>", 0),
      Some(named(schema, text, "type ", "Root", 0))
    )]
  );
}

#[test]
fn a_collections_key_is_non_null_however_written() {
  let text = "scalar Plain<X>\ntype Query {\n  a: HashSet<string>\n  b: HashSet<string!>\n  \
              c: HashMap<string, i32>\n  d: HashMap<string!, i32>\n  e: HashMap<string, i32!>\n  \
              f: Plain<string>\n  g: Plain<string!>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let typed = |field: &str| -> TyId { field_type(model, "^::app::Query", field) };
  // A set's element and a map's key are non-null however they are written; a map's value keeps
  // its own nullness, and so does a scalar's argument that is no collection's.
  assert_eq!(typed("a"), typed("b"));
  assert_eq!(typed("c"), typed("d"));
  assert_ne!(typed("c"), typed("e"));
  assert_ne!(typed("f"), typed("g"));
  assert_eq!(shown(model, typed("a")), "HashSet<string!>");
  assert_eq!(shown(model, typed("e")), "HashMap<string!, i32!>");
  // The written argument keeps its own type.
  let written = field_occurrence(model, "^::app::Query", "a");
  let TypeExprKind::Named { args, .. } = model.type_expr(written).kind() else {
    panic!("`HashSet<string>` names `HashSet`");
  };
  assert_eq!(shown(model, model.type_expr(args[0]).ty()), "string");
}

#[test]
fn a_collections_key_is_non_null_before_any_default_reads_it() {
  // `V`'s default reads the key: written or filled, it is non-null by then.
  let text = "scalar Box<X>\nscalar Dict<K, V = Box<K>> @map\n\
              scalar Dict2<K = String, V = Box<K>> @map\n\
              type Query {\n  a: Dict<String>\n  b: Dict<String!>\n  c: Dict2\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let a = field_type(model, "^::app::Query", "a");
  assert_eq!(field_type(model, "^::app::Query", "b"), a);
  assert_eq!(shown(model, a), "Dict<string!, Box<string!>>");
  assert_eq!(
    field_shown(model, "^::app::Query", "c"),
    "Dict2<string!, Box<string!>>"
  );
}

#[test]
fn a_type_deeper_than_the_limit_is_refused_where_it_is_written() {
  // Two levels are built; a third, written as a list or as an argument, or filled from a default,
  // is refused where the type that would nest it is written.
  let text = "scalar Box<X>\nscalar Deep<T, U = [[T]]>\n\
              type Query {\n  a: [[Int]]\n  b: [[[Int]]]\n  c: Box<[[Int]]>\n  d: Deep<[Int]>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, at(schema, text, "[[[Int]]]", 0), None),
      (LIMIT, at(schema, text, "Box<[[Int]]>", 0), None),
      (LIMIT, at(schema, text, "Deep<[Int]>", 0), None),
    ]
  );
  assert!(
    messages(&analysis)
      .iter()
      .all(|message: &String| message.contains("`Limits::max_generic_depth`, 2"))
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "a"), "[[i32]]");
  for field in ["b", "c", "d"] {
    assert_eq!(field_shown(model, "^::app::Query", field), "?", "{field}");
  }
}

#[test]
fn instantiations_past_the_limit_are_each_refused() {
  // The first instantiation is built, in both nullnesses, which count once; a second distinct one
  // is refused each time it is written; the first, written again, is still built.
  let text = "scalar Box<X>\ntype Query {\n  a: Box<Int>\n  b: Box<Int>!\n  c: Box<String>\n  \
              d: Box<String>\n  e: Box<Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_instantiations(1));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, at(schema, text, "Box<String>", 0), None),
      (LIMIT, at(schema, text, "Box<String>", 1), None),
    ]
  );
  let model = analysis.model();
  let a = field_type(model, "^::app::Query", "a");
  assert_eq!(shown(model, a), "Box<i32>");
  assert_eq!(field_shown(model, "^::app::Query", "b"), "Box<i32>!");
  assert_eq!(field_type(model, "^::app::Query", "e"), a);
}

#[test]
fn substitution_stops_at_its_budget() {
  // One fill of `Page<User>`'s `C` costs three steps.
  let text = "type User {\n  id: ID\n}\ntype Page<T, C = [T]> {\n  items: C\n}\n\
              type Query {\n  page: Page<User>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_substitutions(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "Page<User>", 0), None)]
  );
  assert!(messages(&analysis)[0].contains("`Limits::max_substitutions`, 2"));

  let analysis = analyze_text(text, &Limits::new().with_max_substitutions(3));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "page"),
    "Page<User, [User]>"
  );
}

#[test]
fn the_built_in_library_is_built_whatever_the_limits() {
  let limits = Limits::new()
    .with_max_generic_depth(0)
    .with_max_instantiations(0)
    .with_max_substitutions(0)
    .with_max_bound_steps(0)
    .with_max_value_steps(0)
    .with_max_implementation_steps(0);
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &limits);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  // `core`'s lists have their types.
  assert_eq!(
    field_shown(analysis.model(), "^::core::__Schema", "types"),
    "[__Type!]!"
  );

  // Beside it, the root package's own are refused, each where it is written: a list, an alias,
  // each a level deep, and a use that leaves an argument out, whose fill no step can pay for.
  let text = "scalar Box<T, U = Int>\ntype Query {\n  a: [Int]\n  b: NonZeroU8\n  c: Box<Int>\n}\n";
  let analysis = analyze_text(text, &limits);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, at(schema, text, "[Int]", 0), None),
      (LIMIT, at(schema, text, "NonZeroU8", 0), None),
      (LIMIT, at(schema, text, "Box<Int>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert!(messages[0].contains("`Limits::max_generic_depth`, 0"));
  assert!(messages[1].contains("`Limits::max_generic_depth`, 0"));
  assert!(messages[2].contains("`Limits::max_substitutions`, 0"));
  assert_eq!(
    field_shown(analysis.model(), "^::core::__Schema", "types"),
    "[__Type!]!"
  );
}

#[test]
fn derived_instantiations_are_counted() {
  // Written first, `W<Int>` fills `U` with `Box<i32>`, which takes the one instantiation the
  // limit admits: `W<i32, Box<i32>>` would be a second, and so would `W`'s default's own
  // `Box<T>`, which names a parameter and counts all the same. Each is refused where it is
  // written.
  let text = "type Query {\n  w: W<Int>\n}\nscalar Box<X>\nscalar W<T, U = Box<T>>\n";
  let analysis = analyze_text(text, &Limits::new().with_max_instantiations(1));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, at(schema, text, "W<Int>", 0), None),
      (LIMIT, at(schema, text, "Box<T>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(
    messages(&analysis)
      .iter()
      .all(|message: &String| message.contains("`Limits::max_instantiations`, 1"))
  );
  assert_eq!(field_shown(analysis.model(), "^::app::Query", "w"), "?");
}

#[test]
fn a_miss_pays_for_every_slot_it_leaves_out_before_any_is_filled() {
  // `P<Int>` leaves three arguments out, and its miss charges three steps before it fills any:
  // past a budget of two, the use is `resource-limit`, though `B`'s default would fail at once.
  let text = "scalar P<A, B = Missing, C = Int, D = Int>\ntype Query {\n  p: P<Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_substitutions(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, at(schema, text, "Missing", 0), None),
      (LIMIT, at(schema, text, "P<Int>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(messages(&analysis)[1].contains("`Limits::max_substitutions`, 2"));
}

#[test]
fn a_default_that_cannot_be_used_is_remembered_and_told_at_each_use() {
  // `B`'s default names nothing, which name resolution says once. Each occurrence that leaves `X`
  // out is `generic-arity`, labelled at that default: `A`'s default `B` too, which is no cycle;
  // and `A`, whose own default is that `B`, labelled at `A`'s default.
  let text = "scalar B<X = Missing>\nscalar A<T = B>\ntype Query {\n  a: B\n  b: B\n  c: A\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let missing = Some(at(schema, text, "Missing", 0));
  let b_default = named(schema, text, "T = ", "B", 0);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, at(schema, text, "Missing", 0), None),
      (ARITY, b_default, missing),
      (ARITY, named(schema, text, "a: ", "B", 0), missing),
      (ARITY, named(schema, text, "b: ", "B", 0), missing),
      (ARITY, named(schema, text, "c: ", "A", 0), Some(b_default)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let messages = messages(&analysis);
  assert_eq!(
    messages[1],
    "`B` is written without a type argument for `X`, and the default of `X` cannot fill it here"
  );
  assert_eq!(
    messages[4],
    "`A` is written without a type argument for `T`, and the default of `T` cannot fill it here"
  );

  // The failure is remembered. The program costs four steps of substitution: two for `A`'s default
  // `B`, its miss and the one node of `B`'s default, `Missing`, which fails as it is entered; none
  // for `a` and `b`, which find that failure in the fill table; and two for `c`, its miss and the
  // node of `A`'s default, `B`, which finds it too. Within a budget of four every diagnostic is as
  // without one. A table that kept no failure would fill `B` again at `a`, and `b` would pass it.
  let limited = analyze_text(text, &Limits::new().with_max_substitutions(4));
  assert_eq!(limited.diagnostics(), analysis.diagnostics());
}

/// Whether some occurrence of `analysis`, of any package, has the unresolved type.
fn holds_unresolved(analysis: &Analysis) -> bool {
  let model = analysis.model();
  every_occurrence(analysis)
    .into_iter()
    .any(|id: TypeExprId| model.ty(model.type_expr(id).ty()).kind() == TyKind::Unresolved)
}

#[test]
fn an_ok_analysis_has_no_unresolved_type_occurrence() {
  // Generics with defaults, lists, non-null, aliases, collections, parameters, a bound, an
  // `implements` entry, union members, an extension's list, an application's type argument, and a
  // written root.
  let text = "interface Node {\n  id: ID\n}\ninterface Comparable<X> {\n  id: ID\n}\n\
              scalar Box<X>\n\
              type Page<T, C = [T]> implements Node where T: Comparable<T> {\n  id: ID\n  \
              items: C\n  first: T!\n}\n\
              extend type Page<U, D> {\n  last: U\n}\n\
              union Either = Page<Item> | Item\n\
              directive @typed<V = Int>(value: Int) on OBJECT\n\
              type Item implements Node & Comparable<Item> @typed<[Int!]>(value: 1) {\n  \
              id: ID\n  small: NonZeroU32!\n  tags: HashSet<String>\n  \
              index: HashMap<String, [Item!]>\n}\n\
              type Query @typed(value: 2) {\n  page: Page<Item>\n  items: [Item!]!\n  \
              either: Either\n  boxed: Box<Box<Int>>\n}\n\
              schema {\n  query: Query\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(every_occurrence(&analysis).len() > 20);
  assert!(!holds_unresolved(&analysis));
}

#[test]
fn a_bounds_interface_is_typed_as_any_occurrence() {
  // `Comparable<T>` is an open instantiation: its one argument is `Page`'s parameter, it is not
  // listed, and nothing judges it here.
  let text = "interface Comparable<X> {\n  id: ID\n}\n\
              type Page<T> where T: Comparable<T> {\n  item: T\n}\n";
  let analysis = analyze_one(text);
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let page = def(model, Namespace::Type, "^::app::Page");
  let comparable = def(model, Namespace::Type, "^::app::Comparable");
  let bound = model.def(page).bounds()[0].interface();
  let ty = model.type_expr(bound).ty();
  let TyKind::Named {
    def: named, args, ..
  } = model.ty(ty).kind()
  else {
    panic!("the bound names `Comparable`");
  };
  assert_eq!(named, comparable);
  assert!(matches!(
    model.ty(args[0]).kind(),
    TyKind::Param { def: owner, index: 0, .. } if owner == page
  ));
  assert!(!model.ty(ty).is_closed());
  assert!(model.instantiations(comparable).is_empty());
}

#[test]
fn the_built_in_library_instantiates_with_no_diagnostic() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(clean(&analysis), "{:#?}", analysis.diagnostics());
  assert!(!holds_unresolved(&analysis));
  let model = analysis.model();
  assert_eq!(
    field_shown(model, "^::core::__Type", "fields"),
    "[__Field!]"
  );
  assert_eq!(
    field_shown(model, "^::core::__Schema", "description"),
    "string"
  );
}

/// The root package's files: defaults, a forward default, two aliases, a dependency's generic type,
/// an extension written in a file before its definition's, and three refusals: a count, a default
/// that leads back to itself, and a group.
const ORDERED_APP: [(&str, &str); 2] = [
  (
    "a.graphqlx",
    "type Query {\n  page: Page<Item>\n  forward: Fwd\n  small: NonZeroU16\n  tiny: NonZeroI8!\n  \
     far: ^::geo::Wrap<Int>\n  bad: Page<Item, Item, Item>\n}\n\
     extend type Page<U, V> {\n  extra: [U]\n  cursor: V\n}\n",
  ),
  (
    "b.graphqlx",
    "type Page<T, C = [T]> {\n  items: C\n}\nscalar Fwd<A = [B], B = Int>\ntype Item {\n  \
     id: ID\n}\nscalar Loop<T = Loop>\nscalar Dup<T = U, U = T>\n",
  ),
];

/// The dependency `geo`'s file.
const ORDERED_GEO: (&str, &str) = ("g.graphqlx", "type Wrap<T, U = [T]> {\n  value: U\n}\n");

/// The dependency `astro`'s file.
const ORDERED_ASTRO: (&str, &str) = ("s.graphqlx", "type Star {\n  near: Int\n}\n");

/// The type `id`, written out as `shown` writes it, each type in it, `id` itself and each argument
/// and element at any depth, after its own id and a `:`.
fn structure(model: &Model, id: TyId) -> String {
  let ty = model.ty(id);
  let mut text = match ty.kind() {
    TyKind::Named { def, args, .. } => {
      let name = model.def(def).name();
      if args.is_empty() {
        String::from(name)
      } else {
        let args: Vec<String> = args
          .iter()
          .map(|arg: &TyId| structure(model, *arg))
          .collect();
        format!("{name}<{}>", args.join(", "))
      }
    }
    TyKind::Param { def, index, .. } => format!("{}#{index}", model.def(def).name()),
    TyKind::List { element, .. } => format!("[{}]", structure(model, element)),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  format!("{}:{text}", id.get())
}

/// Every occurrence's type, written out with its id and its arguments' ids; every definition's
/// instantiations; and every diagnostic.
fn rendered(analysis: &Analysis) -> Vec<String> {
  let model = analysis.model();
  let mut lines: Vec<String> = Vec::new();
  for id in every_occurrence(analysis) {
    let ty = model.type_expr(id).ty();
    lines.push(format!("{} {}", id.get(), structure(model, ty)));
  }
  for id in model.defs() {
    let listed: Vec<u32> = model
      .instantiations(id)
      .iter()
      .map(|ty: &TyId| ty.get())
      .collect();
    lines.push(format!("{} {listed:?}", model.canonical_name(id)));
  }
  for diagnostic in analysis.diagnostics() {
    lines.push(format!("{diagnostic:?}"));
  }
  lines
}

#[test]
fn reversing_files_and_dependencies_changes_no_type_and_no_diagnostic() {
  let mut runs: Vec<Vec<String>> = Vec::new();
  for reversed in [false, true] {
    let mut files = ORDERED_APP.to_vec();
    if reversed {
      files.reverse();
    }
    let root = package("app", Layout::Flat, &files).with_dependencies(["geo", "astro"]);
    let mut dependencies = vec![
      package("geo", Layout::Flat, &[ORDERED_GEO]),
      package("astro", Layout::Flat, &[ORDERED_ASTRO]),
    ];
    if reversed {
      dependencies.reverse();
    }
    let analysis = analyze(&root, &dependencies, &Limits::new());
    let codes: Vec<&str> = found(&analysis)
      .into_iter()
      .map(|(code, _, _): (&'static str, Location, Option<Location>)| code)
      .collect();
    assert_eq!(
      codes,
      [ARITY, DEFAULT, DEFAULT],
      "{:#?}",
      analysis.diagnostics()
    );
    runs.push(rendered(&analysis));
  }
  assert_eq!(runs[0], runs[1]);
}

#[test]
fn long_names_are_quoted_as_excerpts() {
  // A definition named with four kilobytes, in a count.
  let name = "E".repeat(4096);
  let text = format!("type {name}<T> {{\n  a: T\n}}\ntype Query {{\n  f: {name}<Int, Int>\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  assert_eq!(
    messages(&analysis),
    [format!(
      "`{}…{}` takes 1 type argument, and 2 are written",
      "E".repeat(64),
      "E".repeat(64)
    )]
  );

  // A parameter named with four kilobytes, in a group's report.
  let param = "P".repeat(4096);
  let text = format!("scalar G<{param} = [{param}]>\n");
  let analysis = analyze_text(&text, &Limits::new());
  let quoted = format!("{}…{}", "P".repeat(64), "P".repeat(64));
  assert_eq!(
    messages(&analysis),
    [format!(
      "the default of `{quoted}` names `{quoted}` itself, so it can never fill"
    )]
  );
}

#[test]
fn a_generic_enum_is_refused_at_its_first_parameter() {
  // Once an enum, at its first type parameter's name, however many it declares; an enum with none
  // and a generic definition of another kind say nothing, and a use of the enum is typed as any
  // generic definition's is.
  let text = "enum Pick<A, B> {\n  ONE\n  TWO\n}\nenum Plain {\n  ONLY\n}\nscalar Box<X>\n\
              type Query {\n  pick: Pick<Int, String>\n  plain: Plain\n  boxed: Box<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(GENERIC_ENUM, named(schema, text, "Pick<", "A", 0), None)]
  );
  assert_eq!(messages(&analysis), [generic_enum("Pick")]);
  let model = analysis.model();
  assert!(
    model
      .def(def(model, Namespace::Type, "^::app::Pick"))
      .has_errors()
  );
  assert_eq!(
    field_shown(model, "^::app::Query", "pick"),
    "Pick<i32, string>"
  );

  // A name of four kilobytes is quoted as an excerpt.
  let name = "N".repeat(4096);
  let text = format!("enum {name}<T> {{\n  A\n}}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let quoted = format!("{}…{}", "N".repeat(64), "N".repeat(64));
  assert_eq!(messages(&analysis), [generic_enum(&quoted)]);
}

#[test]
fn a_generic_enum_a_phase_already_refused_is_left_alone() {
  // The default of its type parameter names nothing: name resolution says so within the enum,
  // and the rule says nothing more.
  let text = "enum E<T = Missing> {\n  A\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(UNDEFINED, at(schema, text, "Missing", 0), None)]
  );

  // The default names its own parameter: instantiation's definition pass says so within the enum,
  // and the rule says nothing more.
  let text = "enum E<T = [T]> {\n  A\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(DEFAULT, at(schema, text, "[T]", 0), None)]
  );
}

#[test]
fn each_use_of_a_generic_enum_that_leaves_its_argument_out_is_an_arity_error() {
  let text = "enum E<T> {\n  A\n}\ntype Query {\n  f: E\n  g: E<Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (GENERIC_ENUM, named(schema, text, "E<", "T", 0), None),
      (
        ARITY,
        named(schema, text, "f: ", "E", 0),
        Some(named(schema, text, "enum ", "E", 0))
      ),
    ]
  );
  let model = analysis.model();
  assert_eq!(field_shown(model, "^::app::Query", "f"), "?");
  assert_eq!(field_shown(model, "^::app::Query", "g"), "E<i32>");
}
